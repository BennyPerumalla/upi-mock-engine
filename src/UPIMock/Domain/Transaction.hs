-- |
-- Module      : UPIMock.Domain.Transaction
-- Description : The Transaction aggregate root and its compile-time state machine.
--
-- The aggregate is indexed by a type-level 'TxnState'. There is exactly one way
-- to obtain a @'Transaction' s@ for @s@ other than @'Initiated'@: apply a
-- 'Transition' whose /from/ index matches the value you already hold. The data
-- constructor of 'Transaction' is not exported, so this is not a convention that
-- reviewers must police — it is a property of the module boundary.
--
-- Consequences, none of which require a runtime check:
--
--   * @FAILED@ is absorbing. No constructor of 'Transition' has @'Failed'@ in its
--     /from/ index, so the invariant \"a failed transaction is never reconciled
--     into success\" (design document §8.2.1) has no code path to violate.
--   * Reconciliation cannot lose its TTUM code: 'Reconcile' takes a 'ReconCode'
--     argument, so @TIMEOUT -> SUCCESS@ without a @102@\/@103@ record does not
--     typecheck (§8.2.2).
--   * A refund can only be opened against @SUCCESS@, and settled only from
--     @REFUND_PENDING@.
--
-- RRN uniqueness (§8.2.3) is the one stated invariant that types cannot carry,
-- because it is a statement about the /set/ of aggregates. It is enforced in the
-- store, transactionally, and is documented as such in ARCHITECTURE.md.
module UPIMock.Domain.Transaction
  ( -- * States
    TxnState (..)
  , renderTxnState
  , parseTxnState
  , isTerminalState

    -- * Reifying the state index
  , StateSing (..)
  , SomeStateSing (..)
  , KnownTxnState (..)
  , demoteState
  , singOfState

    -- * The aggregate
  , TxnSeed (..)
  , TxnCore (..)
  , Transaction
  , txnCore
  , newTransaction

    -- * Transitions
  , Transition (..)
  , transitionLabel
  , sourceSing
  , targetSing
  , applyTransition
  , step

    -- * Existential packaging
  , AnyTransaction (..)
  , anyTxnState
  , anyTxnCore

    -- * Read model
  , TxnView (..)
  , project
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import UPIMock.Domain.ErrorCodes (ErrorCode, ReconCode)
import UPIMock.Domain.Types
  ( AuthRef
  , Flow
  , Money
  , Note
  , Party
  , RefundRef
  , Rrn
  , StreamVersion
  , TxnId
  )

-- | The lifecycle of a UPI transaction (design document §8.1). Promoted to a
-- kind by @DataKinds@; the value-level type exists for the read model and the
-- wire encoding only.
data TxnState
  = Initiated
  | Pending
  | Success
  | Failed
  | Timeout
  | RefundPending
  | Refunded
  deriving stock (Eq, Ord, Show, Enum, Bounded)

renderTxnState :: TxnState -> Text
renderTxnState = \case
  Initiated -> "INITIATED"
  Pending -> "PENDING"
  Success -> "SUCCESS"
  Failed -> "FAILED"
  Timeout -> "TIMEOUT"
  RefundPending -> "REFUND_PENDING"
  Refunded -> "REFUNDED"

parseTxnState :: Text -> Maybe TxnState
parseTxnState t = lookup t [(renderTxnState s, s) | s <- [minBound .. maxBound]]

-- | A state with no outgoing transitions. @SUCCESS@ and @TIMEOUT@ are /not/
-- terminal: the first can be refunded, the second reconciled.
isTerminalState :: TxnState -> Bool
isTerminalState = \case
  Failed -> True
  Refunded -> True
  Initiated -> False
  Pending -> False
  Success -> False
  Timeout -> False
  RefundPending -> False

-- | Singleton witness for the state index. Hand-rolled rather than pulled from
-- @singletons@: seven constructors do not justify the dependency or the Template
-- Haskell.
data StateSing (s :: TxnState) where
  SInitiated :: StateSing 'Initiated
  SPending :: StateSing 'Pending
  SSuccess :: StateSing 'Success
  SFailed :: StateSing 'Failed
  STimeout :: StateSing 'Timeout
  SRefundPending :: StateSing 'RefundPending
  SRefunded :: StateSing 'Refunded

demoteState :: StateSing s -> TxnState
demoteState = \case
  SInitiated -> Initiated
  SPending -> Pending
  SSuccess -> Success
  SFailed -> Failed
  STimeout -> Timeout
  SRefundPending -> RefundPending
  SRefunded -> Refunded

-- | An existentially quantified singleton, produced when crossing back from
-- untyped data (a database row, an HTTP request) into the typed world.
data SomeStateSing where
  SomeStateSing :: StateSing s -> SomeStateSing

singOfState :: TxnState -> SomeStateSing
singOfState = \case
  Initiated -> SomeStateSing SInitiated
  Pending -> SomeStateSing SPending
  Success -> SomeStateSing SSuccess
  Failed -> SomeStateSing SFailed
  Timeout -> SomeStateSing STimeout
  RefundPending -> SomeStateSing SRefundPending
  Refunded -> SomeStateSing SRefunded

-- | Recover the singleton from the index when it is statically known.
class KnownTxnState (s :: TxnState) where
  stateSing :: StateSing s

instance KnownTxnState 'Initiated where stateSing = SInitiated
instance KnownTxnState 'Pending where stateSing = SPending
instance KnownTxnState 'Success where stateSing = SSuccess
instance KnownTxnState 'Failed where stateSing = SFailed
instance KnownTxnState 'Timeout where stateSing = STimeout
instance KnownTxnState 'RefundPending where stateSing = SRefundPending
instance KnownTxnState 'Refunded where stateSing = SRefunded

-- | Everything fixed at initiation. Doubles as the payload of the
-- @TXN_INITIATED@ event, so that replay and creation share one shape and cannot
-- drift apart.
data TxnSeed = TxnSeed
  { seedTxnId :: TxnId
  , seedRrn :: Rrn
  , seedFlow :: Flow
  , seedPayer :: Party
  , seedPayee :: Party
  , seedMoney :: Money
  , seedNote :: Maybe Note
  , seedCreatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | State-independent aggregate data. Split out from 'Transaction' so that
-- 'applyTransition' can change the type index by rebuilding the wrapper, with no
-- phantom-changing record update and no @unsafeCoerce@.
data TxnCore = TxnCore
  { txnId :: TxnId
  , txnRrn :: Rrn
  , txnFlow :: Flow
  , txnPayer :: Party
  , txnPayee :: Party
  , txnMoney :: Money
  , txnNote :: Maybe Note
  , txnCreatedAt :: UTCTime
  , txnUpdatedAt :: UTCTime
  , txnSettledAt :: Maybe UTCTime
  , txnAuthRef :: Maybe AuthRef
  , txnTerminalCode :: Maybe ErrorCode
  , txnReconCode :: Maybe ReconCode
  , txnRefundRef :: Maybe RefundRef
  }
  deriving stock (Eq, Show)

-- | The aggregate root. @newtype@, so the index costs nothing at runtime.
--
-- The constructor is intentionally not exported: 'newTransaction' and
-- 'applyTransition' are the only ways to produce a value of this type.
newtype Transaction (s :: TxnState) = Transaction {txnCore :: TxnCore}
  deriving stock (Eq, Show)

newTransaction :: TxnSeed -> Transaction 'Initiated
newTransaction TxnSeed {..} =
  Transaction
    TxnCore
      { txnId = seedTxnId
      , txnRrn = seedRrn
      , txnFlow = seedFlow
      , txnPayer = seedPayer
      , txnPayee = seedPayee
      , txnMoney = seedMoney
      , txnNote = seedNote
      , txnCreatedAt = seedCreatedAt
      , txnUpdatedAt = seedCreatedAt
      , txnSettledAt = Nothing
      , txnAuthRef = Nothing
      , txnTerminalCode = Nothing
      , txnReconCode = Nothing
      , txnRefundRef = Nothing
      }

-- | Legal state transitions, and the /only/ way to move an aggregate.
--
-- @
--            Authorize                MarkSuccess
--   INITIATED ---------> PENDING ---------------> SUCCESS
--      |                   |  \\ MarkFailed          | OpenRefund
--      | DeclineAtPsp      |   \\                    v
--      v                   |    -> FAILED      REFUND_PENDING
--   FAILED <---------------+                        | SettleRefund
--          DropInReconciliation                     v
--                          | MarkTimeout        REFUNDED
--                          v
--                       TIMEOUT --Reconcile--> SUCCESS
-- @
--
-- 'DeclineAtPsp' extends the diagram in §8.1 of the design document. It models a
-- pre-switch rejection: the payer declining or mistyping the MPIN on a Collect
-- request (@ZA@, @ZM@), or the collect window expiring. Without it, a declined
-- Collect would have to be laundered through @PENDING@, which would claim to the
-- client that the switch forwarded a debit that never left the PSP.
data Transition (from :: TxnState) (to :: TxnState) where
  -- | Payer authorisation captured; the switch forwards to the acquirer.
  Authorize :: AuthRef -> Transition 'Initiated 'Pending
  -- | Rejected before the switch forwarded anything.
  DeclineAtPsp :: ErrorCode -> Transition 'Initiated 'Failed
  -- | Acquirer confirmed the beneficiary credit.
  MarkSuccess :: Transition 'Pending 'Success
  -- | Acquirer or issuer declined.
  MarkFailed :: ErrorCode -> Transition 'Pending 'Failed
  -- | The switch window elapsed with no acquirer response.
  MarkTimeout :: Transition 'Pending 'Timeout
  -- | Offline reconciliation credited the beneficiary (@102@\/@103@).
  Reconcile :: ReconCode -> Transition 'Timeout 'Success
  -- | Reconciliation confirmed the credit never happened.
  DropInReconciliation :: ErrorCode -> Transition 'Timeout 'Failed
  -- | Refund leg opened against a settled transaction.
  OpenRefund :: RefundRef -> Transition 'Success 'RefundPending
  -- | Refund credited back to the payer.
  SettleRefund :: Transition 'RefundPending 'Refunded

transitionLabel :: Transition from to -> Text
transitionLabel = \case
  Authorize _ -> "AUTHORIZE"
  DeclineAtPsp _ -> "DECLINE_AT_PSP"
  MarkSuccess -> "MARK_SUCCESS"
  MarkFailed _ -> "MARK_FAILED"
  MarkTimeout -> "MARK_TIMEOUT"
  Reconcile _ -> "RECONCILE"
  DropInReconciliation _ -> "DROP_IN_RECONCILIATION"
  OpenRefund _ -> "OPEN_REFUND"
  SettleRefund -> "SETTLE_REFUND"

-- | Recover the source index from the proof. Lets callers report a rejected
-- command without threading the singleton separately.
sourceSing :: Transition from to -> StateSing from
sourceSing = \case
  Authorize _ -> SInitiated
  DeclineAtPsp _ -> SInitiated
  MarkSuccess -> SPending
  MarkFailed _ -> SPending
  MarkTimeout -> SPending
  Reconcile _ -> STimeout
  DropInReconciliation _ -> STimeout
  OpenRefund _ -> SSuccess
  SettleRefund -> SRefundPending

-- | Recover the target index from the proof. This is what allows 'step' to
-- repackage an existential without the caller supplying a witness, and it is
-- checked by the compiler against the GADT signature above.
targetSing :: Transition from to -> StateSing to
targetSing = \case
  Authorize _ -> SPending
  DeclineAtPsp _ -> SFailed
  MarkSuccess -> SSuccess
  MarkFailed _ -> SFailed
  MarkTimeout -> STimeout
  Reconcile _ -> SSuccess
  DropInReconciliation _ -> SFailed
  OpenRefund _ -> SRefundPending
  SettleRefund -> SRefunded

-- | Total, pure, and the only state mutation in the system.
--
-- @now@ is supplied by the caller rather than read from the clock, which is what
-- makes replay deterministic and lets Phase 2 inject clock skew without touching
-- this function.
applyTransition :: UTCTime -> Transition from to -> Transaction from -> Transaction to
applyTransition now transition (Transaction core) = Transaction $ case transition of
  Authorize ref -> touched {txnAuthRef = Just ref}
  DeclineAtPsp code -> touched {txnTerminalCode = Just code}
  MarkSuccess -> touched {txnSettledAt = Just now}
  MarkFailed code -> touched {txnTerminalCode = Just code}
  MarkTimeout -> touched
  -- Reconciliation clears the technical decline: the money did move.
  Reconcile code ->
    touched {txnSettledAt = Just now, txnReconCode = Just code, txnTerminalCode = Nothing}
  DropInReconciliation code -> touched {txnReconCode = Nothing, txnTerminalCode = Just code}
  OpenRefund ref -> touched {txnRefundRef = Just ref}
  SettleRefund -> touched
  where
    touched = core {txnUpdatedAt = now}

-- | 'applyTransition' with the result repackaged existentially, for callers that
-- fold a sequence of transitions whose intermediate indices are not statically
-- known (replay, and the command handler).
step :: UTCTime -> Transition from to -> Transaction from -> AnyTransaction
step now transition txn =
  AnyTransaction (targetSing transition) (applyTransition now transition txn)

-- | An aggregate whose state is known only at runtime.
data AnyTransaction where
  AnyTransaction :: StateSing s -> Transaction s -> AnyTransaction

anyTxnState :: AnyTransaction -> TxnState
anyTxnState (AnyTransaction s _) = demoteState s

anyTxnCore :: AnyTransaction -> TxnCore
anyTxnCore (AnyTransaction _ txn) = txnCore txn

-- | The CQRS read model row. Flat, pre-joined, and cheap to serve from STM;
-- derived from the aggregate and never written to directly.
data TxnView = TxnView
  { viewTxnId :: TxnId
  , viewRrn :: Rrn
  , viewState :: TxnState
  , viewFlow :: Flow
  , viewPayer :: Party
  , viewPayee :: Party
  , viewMoney :: Money
  , viewNote :: Maybe Note
  , viewErrorCode :: Maybe ErrorCode
  , viewReconCode :: Maybe ReconCode
  , viewRefundRef :: Maybe RefundRef
  , viewCreatedAt :: UTCTime
  , viewUpdatedAt :: UTCTime
  , viewSettledAt :: Maybe UTCTime
  , viewVersion :: StreamVersion
  }
  deriving stock (Eq, Show)

project :: StreamVersion -> AnyTransaction -> TxnView
project version anyTxn =
  TxnView
    { viewTxnId = txnId core
    , viewRrn = txnRrn core
    , viewState = anyTxnState anyTxn
    , viewFlow = txnFlow core
    , viewPayer = txnPayer core
    , viewPayee = txnPayee core
    , viewMoney = txnMoney core
    , viewNote = txnNote core
    , viewErrorCode = txnTerminalCode core
    , viewReconCode = txnReconCode core
    , viewRefundRef = txnRefundRef core
    , viewCreatedAt = txnCreatedAt core
    , viewUpdatedAt = txnUpdatedAt core
    , viewSettledAt = txnSettledAt core
    , viewVersion = version
    }
  where
    core = anyTxnCore anyTxn
