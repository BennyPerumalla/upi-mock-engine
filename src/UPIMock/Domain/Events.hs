-- |
-- Module      : UPIMock.Domain.Events
-- Description : Domain events, their envelopes, and the transition-to-event map.
--
-- Events are the only durable representation of a transaction. The aggregate is
-- a fold over them, never a row that is updated in place.
--
-- Two rules keep the log readable by things that are not this binary:
--
--   1. @payload@ is self-describing (the constructor tag is inside the JSON), so
--      a decoder never depends on the @event_type@ column being correct.
--   2. @event_type@ is written anyway, as a denormalised index column, and
--      'eventTypeText' is the single definition of it. A property test asserts
--      that it agrees with the JSON tag, because the moment those two drift the
--      SQL projections start lying.
module UPIMock.Domain.Events
  ( DomainEvent (..)
  , eventTypeText
  , eventOfTransition

    -- * Envelopes
  , PendingEvent (..)
  , StoredEvent (..)
  , PayloadVersion (..)
  , currentPayloadVersion
  , AggregateType (..)
  , aggregateTypeText
  , parseAggregateType
  ) where

import Data.Aeson
  ( FromJSON (..)
  , Options (..)
  , SumEncoding (..)
  , ToJSON (..)
  , camelTo2
  , defaultOptions
  , genericParseJSON
  , genericToEncoding
  , genericToJSON
  )
import Data.Char (toUpper)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import UPIMock.Domain.ErrorCodes (ErrorCode, ReconCode)
import UPIMock.Domain.Transaction (Transition (..), TxnSeed)
import UPIMock.Domain.Types (AuthRef, EventId, RefundRef, StreamId, StreamVersion)

-- | Facts about a transaction, in the past tense. One constructor per
-- 'Transition', plus the creation event.
data DomainEvent
  = TxnInitiated TxnSeed
  | TxnAuthorized AuthRef
  | TxnDeclinedAtPsp ErrorCode
  | TxnSucceeded
  | TxnFailed ErrorCode
  | TxnTimedOut
  | TxnReconciled ReconCode
  | TxnReconciliationDropped ErrorCode
  | RefundOpened RefundRef
  | RefundSettled
  deriving stock (Eq, Show, Generic)

eventOptions :: Options
eventOptions =
  defaultOptions
    { sumEncoding = TaggedObject {tagFieldName = "type", contentsFieldName = "data"}
    , constructorTagModifier = map toUpper . camelTo2 '_'
    , omitNothingFields = True
    }

instance ToJSON DomainEvent where
  toJSON = genericToJSON eventOptions
  toEncoding = genericToEncoding eventOptions

instance FromJSON DomainEvent where
  parseJSON = genericParseJSON eventOptions

-- | The @event_type@ column. Kept explicit rather than derived from
-- 'eventOptions' so that a reader of this module can see the on-disk vocabulary
-- without simulating aeson's generics.
eventTypeText :: DomainEvent -> Text
eventTypeText = \case
  TxnInitiated{} -> "TXN_INITIATED"
  TxnAuthorized{} -> "TXN_AUTHORIZED"
  TxnDeclinedAtPsp{} -> "TXN_DECLINED_AT_PSP"
  TxnSucceeded -> "TXN_SUCCEEDED"
  TxnFailed{} -> "TXN_FAILED"
  TxnTimedOut -> "TXN_TIMED_OUT"
  TxnReconciled{} -> "TXN_RECONCILED"
  TxnReconciliationDropped{} -> "TXN_RECONCILIATION_DROPPED"
  RefundOpened{} -> "REFUND_OPENED"
  RefundSettled -> "REFUND_SETTLED"

-- | The event that records a transition. Total in both directions: every
-- constructor of 'Transition' has exactly one event, and
-- 'UPIMock.Engine.StateMachine.stepForEvent' inverts it under the state index.
-- Adding a transition without extending both maps does not compile.
eventOfTransition :: Transition from to -> DomainEvent
eventOfTransition = \case
  Authorize ref -> TxnAuthorized ref
  DeclineAtPsp code -> TxnDeclinedAtPsp code
  MarkSuccess -> TxnSucceeded
  MarkFailed code -> TxnFailed code
  MarkTimeout -> TxnTimedOut
  Reconcile code -> TxnReconciled code
  DropInReconciliation code -> TxnReconciliationDropped code
  OpenRefund ref -> RefundOpened ref
  SettleRefund -> RefundSettled

-- | An event decided by the engine but not yet committed. @occurredAt@ comes
-- from 'UPIMock.Application.Ports.MonadClock', never from
-- 'Data.Time.getCurrentTime' inline, so that a seeded run is reproducible.
data PendingEvent = PendingEvent
  { pendingEvent :: DomainEvent
  , pendingOccurredAt :: UTCTime
  }
  deriving stock (Eq, Show)

-- | A committed event, as read back from the store.
data StoredEvent = StoredEvent
  { storedEventId :: EventId
  , storedStreamId :: StreamId
  , storedAggregateType :: AggregateType
  , storedVersion :: StreamVersion
  , storedEvent :: DomainEvent
  , storedOccurredAt :: UTCTime
  , storedRecordedAt :: UTCTime
  , storedPayloadVersion :: PayloadVersion
  }
  deriving stock (Eq, Show)

-- | Schema version of the JSON payload. Bumping it obliges you to add an
-- upcaster; see CONTRIBUTING.md § Changing an event.
newtype PayloadVersion = PayloadVersion {unPayloadVersion :: Int}
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

currentPayloadVersion :: PayloadVersion
currentPayloadVersion = PayloadVersion 1

-- | Discriminates streams in the shared @events@ table. Phase 1 has one
-- aggregate; @MANDATE@ and @DISPUTE@ join it with the Dispute & Reconciliation
-- context, which is why the column exists now.
data AggregateType
  = AggTransaction
  deriving stock (Eq, Show, Enum, Bounded)

aggregateTypeText :: AggregateType -> Text
aggregateTypeText = \case
  AggTransaction -> "TRANSACTION"

parseAggregateType :: Text -> Maybe AggregateType
parseAggregateType t =
  lookup t [(aggregateTypeText a, a) | a <- [minBound .. maxBound]]
