-- |
-- Module      : UPIMock.Application.Service
-- Description : Use cases. The only place where a decision, a commit and a
--               projection are sequenced.
--
-- This layer is not in the design document's module tree; it is the one
-- deliberate addition, and it exists to keep two things out of the HTTP
-- handlers. First, the write-path sequence — load, decide, commit, project — is
-- identical for every command and is easy to get subtly wrong (projecting before
-- the commit is durable, or reusing a stale version). Second, the handlers stay
-- free of any knowledge of event sourcing, so an alternative transport (a gRPC
-- façade, a scenario-file driver in Phase 2) reuses these functions verbatim.
--
-- Every function here is polymorphic in @m@ and constrained only by
-- "UPIMock.Application.Ports". The test suite instantiates them at a @StateT@
-- over an in-memory log; the binary instantiates them at
-- 'UPIMock.App.App'. Neither instantiation is visible from this module.
module UPIMock.Application.Service
  ( -- * Use cases
    InitiateRequest (..)
  , initiateTransaction
  , applyCommand
  , fetchEventLog
  , rebuildReadModel

    -- * Failure
  , ServiceError (..)
  , renderServiceError
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE)
import Data.Aeson (object, (.=))
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID

import UPIMock.Application.Ports
import UPIMock.Domain.Events
  ( AggregateType (..)
  , DomainEvent
  , PendingEvent (..)
  , StoredEvent (..)
  , eventTypeText
  )
import UPIMock.Domain.Transaction
  ( AnyTransaction
  , TxnCore (..)
  , TxnSeed (..)
  , TxnView
  , anyTxnCore
  , project
  )
import UPIMock.Domain.Types
  ( AuthRef (..)
  , Flow
  , Money
  , Note
  , Party
  , Rrn
  , StreamId
  , StreamVersion
  , TxnId (..)
  , ValidationError
  , noStreamVersion
  , renderValidationError
  , rrnOfWord
  , streamIdOfTxn
  )
import UPIMock.Engine.StateMachine
  ( Command
  , DomainError
  , Initiation (..)
  , ReplayError
  , applyDecided
  , checkSeed
  , decide
  , initiate
  , renderDomainError
  , renderReplayError
  , replay
  )

-- | Everything a client must supply to open a transaction. Value objects, not
-- text: parsing happened in "UPIMock.API.Wire", so nothing here can be
-- malformed. 'initRrn' is optional — a harness that wants to provoke NPCI @DF@
-- supplies its own, everyone else lets the simulator mint one.
data InitiateRequest = InitiateRequest
  { initFlow :: Flow
  , initPayer :: Party
  , initPayee :: Party
  , initMoney :: Money
  , initNote :: Maybe Note
  , initRrn :: Maybe Rrn
  }
  deriving stock (Eq, Show)

-- | The union of everything that can go wrong on a write path, kept as four
-- distinct wrappers rather than a flat sum because the HTTP layer maps each
-- constructor to a different status class: 'ServiceInvalid' is a 400,
-- 'ServiceDomain' a 409, 'ServiceStore' a 409 or 503 depending on the cause,
-- 'ServiceReplay' a 500 (the log is corrupt), 'ServiceNotFound' a 404.
data ServiceError
  = ServiceInvalid ValidationError
  | ServiceDomain DomainError
  | ServiceStore StoreError
  | ServiceReplay ReplayError
  | ServiceNotFound TxnId
  deriving stock (Eq, Show)

renderServiceError :: ServiceError -> Text
renderServiceError = \case
  ServiceInvalid e -> renderValidationError e
  ServiceDomain e -> renderDomainError e
  ServiceStore e -> renderStoreError e
  ServiceReplay e -> renderReplayError e
  ServiceNotFound txnId -> "no such transaction: " <> UUID.toText (unTxnId txnId)

-- | Open a transaction.
--
-- The order of effects is the contract: identity and timestamp first (so the
-- seed is fixed before anything is written), then the store commit, then the
-- projection. A crash between the commit and 'putView' loses nothing — the boot
-- rebuild ('rebuildReadModel') recomputes the view from the log.
--
-- The entropy draws are unconditional. Minting an 'AuthRef' that a Collect flow
-- discards costs one UUID and buys a property worth more than that: the number
-- of draws per request does not depend on the request, so a seeded Phase-2 run
-- produces the same identifiers regardless of flow.
initiateTransaction ::
  (MonadClock m, MonadEntropy m, MonadEventStore m, MonadReadModel m) =>
  InitiateRequest ->
  m (Either ServiceError TxnView)
initiateTransaction request = runExceptT $ do
  now <- lift currentTime
  txnId <- TxnId <$> lift freshUuid
  implicitRef <- AuthRef . ("auth-" <>) . UUID.toText <$> lift freshUuid
  minted <- rrnOfWord <$> lift freshWord64
  let rrn = fromMaybe minted (initRrn request)
  seed <- except (first ServiceInvalid (checkSeed (seedOf now txnId rrn request)))
  let initiation = initiate seed implicitRef
      events = initiationEvents initiation
  result <-
    ExceptT . fmap (first ServiceStore) . commit $
      CommitRequest
        { commitStreamId = streamIdOfTxn txnId
        , commitAggregateType = AggTransaction
        , commitExpectedVersion = noStreamVersion
        , commitEvents = fmap (\event -> PendingEvent event now) events
        , commitRrnClaims = [RrnClaim {claimRrn = rrn, claimTxnId = txnId}]
        , commitOutbox = map (outboxDraftOf txnId rrn now) (toList events)
        }
  publish (project (committedVersion result) (initiationState initiation))

seedOf :: UTCTime -> TxnId -> Rrn -> InitiateRequest -> TxnSeed
seedOf now txnId rrn request =
  TxnSeed
    { seedTxnId = txnId
    , seedRrn = rrn
    , seedFlow = initFlow request
    , seedPayer = initPayer request
    , seedPayee = initPayee request
    , seedMoney = initMoney request
    , seedNote = initNote request
    , seedCreatedAt = now
    }

-- | The outbox row for one event. Topic is the event type, so a Phase-2
-- dispatcher can route on it without decoding the payload.
outboxDraftOf :: TxnId -> Rrn -> UTCTime -> DomainEvent -> OutboxDraft
outboxDraftOf txnId rrn now event =
  OutboxDraft
    { outboxTopic = eventTypeText event
    , outboxPayload = object ["txnId" .= txnId, "rrn" .= rrn, "event" .= event]
    , outboxOccurredAt = now
    }

-- | Advance a transaction. Rejections are values: an illegal command leaves the
-- log untouched and returns 'ServiceDomain', and a concurrent writer that got
-- there first returns 'ServiceStore' carrying 'VersionConflict'. Neither is
-- retried here — the caller decides, because for a simulator a lost race is
-- often the behaviour under test.
applyCommand ::
  (MonadClock m, MonadEventStore m, MonadReadModel m) =>
  TxnId ->
  Command ->
  m (Either ServiceError TxnView)
applyCommand txnId command = runExceptT $ do
  (version, current) <- loadAggregate txnId
  decided <- except (first ServiceDomain (decide command current))
  now <- lift currentTime
  let (event, next) = applyDecided now decided
  result <-
    ExceptT . fmap (first ServiceStore) . commit $
      CommitRequest
        { commitStreamId = streamIdOfTxn txnId
        , commitAggregateType = AggTransaction
        , commitExpectedVersion = version
        , commitEvents = PendingEvent event now :| []
        , commitRrnClaims = []
        , commitOutbox = [outboxDraftOf txnId (txnRrn (anyTxnCore next)) now event]
        }
  publish (project (committedVersion result) next)

-- | The audit endpoint's payload: the raw stream, undecorated.
fetchEventLog :: MonadEventStore m => TxnId -> m (Either ServiceError [StoredEvent])
fetchEventLog = runExceptT . readStreamOrFail

-- | Rebuild the entire read model from the log. Called once during boot, before
-- the listener is bound, and never on a request path.
--
-- A failure here is not recoverable at runtime: it means the log contains a
-- stream this binary cannot fold, so the process must refuse to start rather
-- than serve a projection with a hole in it.
rebuildReadModel :: (MonadEventStore m, MonadReadModel m) => m (Either ServiceError Int)
rebuildReadModel = runExceptT $ do
  events <- ExceptT (fmap (first ServiceStore) readAllEvents)
  views <- traverse rebuildStream (Map.toList (groupByStream events))
  lift (replaceAllViews views)
  pure (length views)
  where
    rebuildStream (stream, streamEvents) = do
      (version, aggregate) <- except (first ServiceReplay (replay stream streamEvents))
      pure (project version aggregate)

-- | Partition the log by stream, preserving log order inside each bucket:
-- @fromListWith (flip (<>))@ combines as @older <> newer@, which is what
-- 'UPIMock.Engine.StateMachine.replay' requires.
groupByStream :: [StoredEvent] -> Map.Map StreamId [StoredEvent]
groupByStream = Map.fromListWith (flip (<>)) . map (\event -> (storedStreamId event, [event]))

loadAggregate ::
  MonadEventStore m =>
  TxnId ->
  ExceptT ServiceError m (StreamVersion, AnyTransaction)
loadAggregate txnId = do
  events <- readStreamOrFail txnId
  except (first ServiceReplay (replay (streamIdOfTxn txnId) events))

-- | An empty stream and a missing aggregate are the same thing in an
-- event-sourced store, and both are a 404 rather than a corrupt log.
readStreamOrFail :: MonadEventStore m => TxnId -> ExceptT ServiceError m [StoredEvent]
readStreamOrFail txnId = do
  events <- ExceptT (fmap (first ServiceStore) (readStream (streamIdOfTxn txnId)))
  case events of
    [] -> throwE (ServiceNotFound txnId)
    _ -> pure events

publish :: MonadReadModel m => TxnView -> ExceptT ServiceError m TxnView
publish view = do
  lift (putView view)
  pure view
