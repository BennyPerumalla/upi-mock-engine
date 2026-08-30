-- |
-- Module      : UPIMock.Application.Ports
-- Description : The hexagonal boundary. Every effect the application needs, and
--               nothing about how it is provided.
--
-- These classes are the reason "the domain must not know SQLite exists" is a
-- structural fact rather than a review comment: this module imports nothing from
-- @UPIMock.Infrastructure@, and the modules below it in the dependency graph
-- import nothing at all outside the domain.
--
-- Three deliberate choices shape the interface.
--
--   * __No method throws.__ Every failure that a caller can reasonably act on is
--     a value in 'Either'. Exceptions escaping an instance are bugs in the
--     adapter, not part of the contract.
--   * __Time and entropy are ports.__ 'MonadClock' and 'MonadEntropy' look like
--     over-engineering in Phase 1, where they are @getCurrentTime@ and a
--     'System.Random'-free @UUID@ draw. They are what makes Phase 2's seeded
--     runs and injected clock skew a new instance rather than a rewrite of every
--     call site.
--   * __'CommitRequest' is a record, not an argument list.__ Phase 2 adds
--     latency-injection metadata and mandate claims to the same atomic write.
--     Growing a record leaves existing construction sites compiling.
module UPIMock.Application.Ports
  ( -- * Ambient effects
    MonadClock (..)
  , MonadEntropy (..)

    -- * Write side
  , MonadEventStore (..)
  , CommitRequest (..)
  , CommitResult (..)
  , RrnClaim (..)
  , OutboxDraft (..)
  , StoreError (..)
  , renderStoreError

    -- * Read side
  , MonadReadModel (..)
  , ViewQuery (..)
  , defaultViewQuery
  ) where

import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Word (Word64)

import UPIMock.Domain.Events (AggregateType, PendingEvent, StoredEvent)
import UPIMock.Domain.Transaction (TxnState, TxnView)
import UPIMock.Domain.Types (Flow, Rrn, StreamId (..), StreamVersion (..), TxnId, rrnText)

-- | The system's only clock. An engine that calls 'Data.Time.getCurrentTime'
-- directly cannot be replayed, and a test for a timeout would have to sleep.
class Monad m => MonadClock m where
  currentTime :: m UTCTime

-- | The system's only source of nondeterminism. Two primitives, both cheap to
-- reimplement over a seeded splitmix state in Phase 2; every identifier the
-- engine mints is derived from one of them by a pure function
-- ('UPIMock.Domain.Types.rrnOfWord' and friends), so the derivation stays
-- testable without stubbing the generator.
class Monad m => MonadEntropy m where
  freshUuid :: m UUID
  freshWord64 :: m Word64

-- | The append-only event log.
--
-- Laws an adapter must satisfy. The SQLite adapter satisfies them with a single
-- serialised writer and a @UNIQUE (stream_id, stream_version)@ constraint; the
-- Phase-2 PostgreSQL adapter will satisfy them with @SERIALIZABLE@ and the same
-- constraint. The in-memory double in the test suite satisfies them with
-- 'Control.Monad.State.StateT', which is why the property tests are meaningful.
--
--   1. /Atomicity./ 'commit' persists the events, the RRN claims and the outbox
--      rows in one transaction, or none of them.
--   2. /Optimistic concurrency./ 'commit' succeeds only if
--      'commitExpectedVersion' equals the stream's current version; otherwise it
--      returns 'VersionConflict' and writes nothing.
--   3. /Ordering./ 'readStream' yields versions @1..n@ contiguously ascending;
--      'readAllEvents' yields the log ascending by 'UPIMock.Domain.Events.storedEventId'.
--   4. /Totality./ No method throws. A decode failure is 'CorruptPayload'.
--   5. /RRN uniqueness./ A claim in 'commitRrnClaims' that collides with an
--      existing claim fails the entire commit with 'DuplicateRrn'. This is where
--      design document §8.2.3 is enforced; no aggregate can enforce it about
--      itself, because it is a statement about the set of all aggregates.
class Monad m => MonadEventStore m where
  readStream :: StreamId -> m (Either StoreError [StoredEvent])

  -- | The entire log. Used once, at boot, to rebuild the read model; never on a
  -- request path.
  readAllEvents :: m (Either StoreError [StoredEvent])

  commit :: CommitRequest -> m (Either StoreError CommitResult)

-- | One atomic unit of write-side work.
data CommitRequest = CommitRequest
  { commitStreamId :: StreamId
  , commitAggregateType :: AggregateType
  , commitExpectedVersion :: StreamVersion
  -- ^ 'UPIMock.Domain.Types.noStreamVersion' for a creation.
  , commitEvents :: NonEmpty PendingEvent
  -- ^ Appended in order at @expected + 1 ..@.
  , commitRrnClaims :: [RrnClaim]
  -- ^ Global uniqueness assertions; a violation is 'DuplicateRrn'.
  , commitOutbox :: [OutboxDraft]
  -- ^ Written in the same transaction as the events. Phase 1 ships no
  -- dispatcher, so these rows accumulate unread; that is the transactional
  -- outbox seam, deliberately load-bearing before it is loaded.
  }

-- | A claim on an RRN by a transaction. Separate from the event payload because
-- uniqueness is a property of the /set/ of aggregates, which no aggregate can
-- enforce about itself.
data RrnClaim = RrnClaim
  { claimRrn :: Rrn
  , claimTxnId :: TxnId
  }
  deriving stock (Eq, Show)

-- | A notification to be published after the commit is durable.
data OutboxDraft = OutboxDraft
  { outboxTopic :: Text
  , outboxPayload :: Value
  , outboxOccurredAt :: UTCTime
  }
  deriving stock (Eq, Show)

data CommitResult = CommitResult
  { committedVersion :: StreamVersion
  -- ^ Version of the last appended event; the caller projects the read model at
  -- this version.
  , committedEvents :: NonEmpty StoredEvent
  }

-- | Failures of the write side. All four are conditions the caller must be able
-- to distinguish: the first is retryable by reloading, the second is an NPCI
-- @DF@, the third is data corruption, the fourth is an operational fault.
data StoreError
  = -- | Expected version, then the version the stream is actually at.
    VersionConflict StreamId StreamVersion StreamVersion
  | DuplicateRrn Rrn
  | CorruptPayload StreamId StreamVersion Text
  | StoreUnavailable Text
  deriving stock (Eq, Show)

renderStoreError :: StoreError -> Text
renderStoreError = \case
  VersionConflict sid expected actual ->
    "stream "
      <> unStreamId sid
      <> " moved: expected version "
      <> version expected
      <> ", found "
      <> version actual
  DuplicateRrn rrn -> "RRN already used: " <> rrnText rrn
  CorruptPayload sid at reason ->
    "undecodable payload in " <> unStreamId sid <> " version " <> version at <> ": " <> reason
  StoreUnavailable reason -> "event store unavailable: " <> reason
  where
    version = T.pack . show . unStreamVersion

-- | The CQRS read side. Every method is a total function over the projection;
-- none of them can fail, because the projection is derived state that is rebuilt
-- from the log at boot and can always be recomputed.
--
-- One law, and it is the one that concurrent commits make interesting:
-- 'putView' must not regress a row. If the stored view carries a
-- 'UPIMock.Domain.Transaction.viewVersion' greater than or equal to the incoming
-- one, the write is dropped. Without it, two commits that interleave between
-- their append and their projection can leave the read model showing the older
-- state indefinitely.
class Monad m => MonadReadModel m where
  putView :: TxnView -> m ()
  getView :: TxnId -> m (Maybe TxnView)
  getViewByRrn :: Rrn -> m (Maybe TxnView)

  -- | Most recently updated first. The query is applied in memory: the read
  -- model is a @TVar@ of maps, and Phase 1 caps the working set at what a
  -- developer's laptop holds.
  queryViews :: ViewQuery -> m [TxnView]

  -- | Replace the entire projection. Boot-time rebuild only; not exposed over
  -- HTTP.
  replaceAllViews :: [TxnView] -> m ()

-- | Filters for the collection endpoint. Absent filters match everything.
data ViewQuery = ViewQuery
  { queryState :: Maybe TxnState
  , queryFlow :: Maybe Flow
  , queryLimit :: Int
  , queryOffset :: Int
  }
  deriving stock (Eq, Show)

defaultViewQuery :: ViewQuery
defaultViewQuery =
  ViewQuery
    { queryState = Nothing
    , queryFlow = Nothing
    , queryLimit = 50
    , queryOffset = 0
    }
