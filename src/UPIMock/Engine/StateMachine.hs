-- |
-- Module      : UPIMock.Engine.StateMachine
-- Description : The pure decision core: commands in, transition proofs out.
--
-- Nothing in this module performs IO, reads a clock, or knows that storage
-- exists. It has three jobs.
--
-- [Decide] 'decide' turns a 'Command' plus the aggregate the caller already
--   holds into a proof ('Decided') that the requested move is legal. A rejected
--   command is a 'DomainError' value — never an exception, never a silently
--   ignored no-op.
--
-- [Initiate] 'initiate' encodes the one flow-dependent rule of Phase 1: an
--   Intent or QR payment carries its authorisation in the initiation request,
--   whereas a Collect request must wait for the payer. See
--   'flowAuthorizesImplicitly'.
--
-- [Replay] 'stepForEvent' and 'replay' are the crossing from untyped log rows
--   back into the typed aggregate. 'stepForEvent' inverts
--   'UPIMock.Domain.Events.eventOfTransition' under the state index; both are
--   total case analyses over closed types, so adding a transition without
--   extending them is a compile error rather than a runtime surprise.
--
-- Determinism: every function that advances an aggregate takes the timestamp as
-- an argument. 'replay' supplies @storedOccurredAt@, so a read model rebuilt
-- from the log is identical to the one the write path produced.
module UPIMock.Engine.StateMachine
  ( -- * Commands
    Command (..)
  , commandLabel

    -- * Initiation
  , Initiation (..)
  , initiate
  , flowAuthorizesImplicitly
  , checkSeed

    -- * Deciding
  , Decided (..)
  , decide
  , applyDecided

    -- * Replay
  , Step (..)
  , stepForEvent
  , replay
  , initialVersion

    -- * Failures
  , DomainError (..)
  , renderDomainError
  , ReplayError (..)
  , renderReplayError
  ) where

import Control.Monad (foldM, when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)

import UPIMock.Domain.ErrorCodes (ErrorCode, ReconCode)
import UPIMock.Domain.Events (DomainEvent (..), StoredEvent (..), eventOfTransition, eventTypeText)
import UPIMock.Domain.Transaction
  ( AnyTransaction (..)
  , StateSing (..)
  , Transaction
  , TxnCore (..)
  , TxnSeed (..)
  , TxnState
  , Transition (..)
  , demoteState
  , isTerminalState
  , newTransaction
  , renderTxnState
  , step
  , txnCore
  )
import UPIMock.Domain.Types
  ( AuthRef
  , Flow (..)
  , Party (..)
  , RefundRef
  , StreamId (..)
  , StreamVersion (..)
  , ValidationError (..)
  , flowIsMerchant
  , nextVersion
  , noStreamVersion
  , renderFlow
  , vpaText
  )

-- | An intent to move a transaction. One constructor per 'Transition' that a
-- client or the simulated switch may request; the mapping is deliberately not
-- collapsed into 'Transition' itself, because a command is untyped (it arrives
-- over HTTP) whereas a 'Transition' is a proof.
data Command
  = CmdAuthorize AuthRef
  | CmdDeclineAtPsp ErrorCode
  | CmdMarkSuccess
  | CmdMarkFailed ErrorCode
  | CmdMarkTimeout
  | CmdReconcile ReconCode
  | CmdDropInReconciliation ErrorCode
  | CmdOpenRefund RefundRef
  | CmdSettleRefund
  deriving stock (Eq, Show)

commandLabel :: Command -> Text
commandLabel = \case
  CmdAuthorize{} -> "AUTHORIZE"
  CmdDeclineAtPsp{} -> "DECLINE_AT_PSP"
  CmdMarkSuccess -> "MARK_SUCCESS"
  CmdMarkFailed{} -> "MARK_FAILED"
  CmdMarkTimeout -> "MARK_TIMEOUT"
  CmdReconcile{} -> "RECONCILE"
  CmdDropInReconciliation{} -> "DROP_IN_RECONCILIATION"
  CmdOpenRefund{} -> "OPEN_REFUND"
  CmdSettleRefund -> "SETTLE_REFUND"

-- | Whether the flow's authorisation is part of the initiation request.
--
-- Intent and QR payments are initiated /by the payer/, who has already entered
-- the MPIN in their PSP app; the simulator therefore records @TXN_INITIATED@ and
-- @TXN_AUTHORIZED@ in the same commit and answers @PENDING@. A Collect request
-- is initiated by the /payee/, so the aggregate rests in @INITIATED@ until the
-- payer approves ('CmdAuthorize') or declines ('CmdDeclineAtPsp'), which is the
-- state where a Collect can expire.
flowAuthorizesImplicitly :: Flow -> Bool
flowAuthorizesImplicitly = not . flowRequiresPayerDecision

flowRequiresPayerDecision :: Flow -> Bool
flowRequiresPayerDecision = \case
  FlowP2PCollect -> True
  FlowP2PIntent -> False
  FlowP2MQr -> False

-- | Cross-field checks on a seed that no single value object can make. Called
-- at the HTTP boundary; the resulting 'ValidationError' is a 400, not an NPCI
-- decline.
checkSeed :: TxnSeed -> Either ValidationError TxnSeed
checkSeed seed
  | payer == payee = Left (PayerIsPayee (vpaText payer))
  | merchantFlow, Nothing <- merchantId = Left MerchantIdRequired
  | not merchantFlow, Just _ <- merchantId = Left MerchantIdForbidden
  | otherwise = Right seed
  where
    payer = partyVpa (seedPayer seed)
    payee = partyVpa (seedPayee seed)
    merchantId = partyMerchantId (seedPayee seed)
    merchantFlow = flowIsMerchant (seedFlow seed)

-- | The events and the resulting aggregate produced by a creation command.
-- 'initiationEvents' is ordered and non-empty; its head is always
-- 'TxnInitiated', which is what 'replay' relies on.
data Initiation = Initiation
  { initiationEvents :: NonEmpty DomainEvent
  , initiationState :: AnyTransaction
  }

-- | Total: an initiation cannot be rejected here, because everything that could
-- reject it (field validity, cross-field consistency, RRN uniqueness) has been
-- decided before this point by 'checkSeed' and by the store's RRN index.
--
-- @implicitRef@ is minted by the caller from 'UPIMock.Application.Ports.MonadEntropy'
-- and discarded for flows that require an explicit payer decision. Minting it
-- unconditionally keeps this function pure and its result independent of the
-- order in which the caller draws from the entropy source.
initiate :: TxnSeed -> AuthRef -> Initiation
initiate seed implicitRef
  | flowAuthorizesImplicitly (seedFlow seed) =
      Initiation
        { initiationEvents = TxnInitiated seed :| [TxnAuthorized implicitRef]
        , initiationState = step (seedCreatedAt seed) (Authorize implicitRef) created
        }
  | otherwise =
      Initiation
        { initiationEvents = TxnInitiated seed :| []
        , initiationState = AnyTransaction SInitiated created
        }
  where
    created = newTransaction seed

-- | A legal move together with the aggregate it applies to. The indices are
-- hidden, but they were checked when this value was built, so 'applyDecided'
-- needs no further validation and cannot fail.
data Decided where
  Decided :: Transition from to -> Transaction from -> Decided

-- | The whole of Phase-1 command authorisation. The @case@ scrutinises the
-- command and the /runtime witness/ of the current state together; every
-- right-hand side type-checks only because the witness refines the aggregate's
-- index to the transition's source. There is no @otherwise@ branch that could
-- accidentally admit an illegal move — the fallback can only produce 'Left'.
decide :: Command -> AnyTransaction -> Either DomainError Decided
decide cmd (AnyTransaction sing txn) = case (cmd, sing) of
  (CmdAuthorize ref, SInitiated) -> Right (Decided (Authorize ref) txn)
  (CmdDeclineAtPsp code, SInitiated) -> Right (Decided (DeclineAtPsp code) txn)
  (CmdMarkSuccess, SPending) -> Right (Decided MarkSuccess txn)
  (CmdMarkFailed code, SPending) -> Right (Decided (MarkFailed code) txn)
  (CmdMarkTimeout, SPending) -> Right (Decided MarkTimeout txn)
  (CmdReconcile code, STimeout) -> Right (Decided (Reconcile code) txn)
  (CmdDropInReconciliation code, STimeout) -> Right (Decided (DropInReconciliation code) txn)
  (CmdOpenRefund ref, SSuccess) -> Right (Decided (OpenRefund ref) txn)
  (CmdSettleRefund, SRefundPending) -> Right (Decided SettleRefund txn)
  _ -> Left rejection
  where
    flow = txnFlow (txnCore txn)
    current = demoteState sing
    rejection
      | isPayerDecision cmd, flowAuthorizesImplicitly flow = AuthorizationNotApplicable flow
      | isTerminalState current = AlreadyTerminal (commandLabel cmd) current
      | otherwise = IllegalTransition (commandLabel cmd) current
    isPayerDecision = \case
      CmdAuthorize{} -> True
      CmdDeclineAtPsp{} -> True
      _ -> False

-- | Stamp the decision with the instant it was taken. Returns the event to
-- append and the aggregate to project; the caller must persist the first before
-- publishing the second.
applyDecided :: UTCTime -> Decided -> (DomainEvent, AnyTransaction)
applyDecided now (Decided transition txn) =
  (eventOfTransition transition, step now transition txn)

-- | A transition out of a known state to a state the caller does not need to
-- name. Existentially hiding only the /target/ is what lets a fold over the log
-- keep its type-checking obligation while the state changes at every element.
data Step (from :: TxnState) where
  Step :: Transition from to -> Step from

-- | Which transition, if any, an event denotes when the aggregate is in state
-- @from@. 'Nothing' means the log is inconsistent with itself — an event was
-- appended that the state machine could not have produced — which 'replay'
-- reports rather than skips.
--
-- @TxnInitiated@ has no case here on purpose: it creates a stream instead of
-- advancing one, and 'replay' handles it separately.
stepForEvent :: StateSing from -> DomainEvent -> Maybe (Step from)
stepForEvent sing event = case (sing, event) of
  (SInitiated, TxnAuthorized ref) -> Just (Step (Authorize ref))
  (SInitiated, TxnDeclinedAtPsp code) -> Just (Step (DeclineAtPsp code))
  (SPending, TxnSucceeded) -> Just (Step MarkSuccess)
  (SPending, TxnFailed code) -> Just (Step (MarkFailed code))
  (SPending, TxnTimedOut) -> Just (Step MarkTimeout)
  (STimeout, TxnReconciled code) -> Just (Step (Reconcile code))
  (STimeout, TxnReconciliationDropped code) -> Just (Step (DropInReconciliation code))
  (SSuccess, RefundOpened ref) -> Just (Step (OpenRefund ref))
  (SRefundPending, RefundSettled) -> Just (Step SettleRefund)
  _ -> Nothing

-- | The version of the first event in a stream. Streams are 1-based;
-- 'UPIMock.Domain.Types.noStreamVersion' (@0@) is the expected version of a
-- creation command.
initialVersion :: StreamVersion
initialVersion = nextVersion noStreamVersion

-- | Fold a stream into its aggregate. The store returns events ordered by
-- @stream_version@; this function re-checks that ordering rather than trusting
-- it, because a gap means the log lost a write and the resulting aggregate would
-- be a plausible-looking lie.
replay :: StreamId -> [StoredEvent] -> Either ReplayError (StreamVersion, AnyTransaction)
replay sid = \case
  [] -> Left (ReplayEmptyStream sid)
  creation : rest -> do
    when (storedVersion creation /= initialVersion) $
      Left (ReplayVersionGap sid initialVersion (storedVersion creation))
    seed <- case storedEvent creation of
      TxnInitiated s -> Right s
      other -> Left (ReplayMissingSeed sid (eventTypeText other))
    foldM advance (initialVersion, AnyTransaction SInitiated (newTransaction seed)) rest
  where
    advance ::
      (StreamVersion, AnyTransaction) ->
      StoredEvent ->
      Either ReplayError (StreamVersion, AnyTransaction)
    advance (previous, anyTxn) stored = do
      let version = storedVersion stored
      when (version /= nextVersion previous) $
        Left (ReplayVersionGap sid (nextVersion previous) version)
      case storedEvent stored of
        TxnInitiated{} -> Left (ReplayDuplicateSeed sid version)
        event -> case anyTxn of
          AnyTransaction sing txn -> case stepForEvent sing event of
            Nothing ->
              Left (ReplayIllegalEvent sid version (eventTypeText event) (demoteState sing))
            Just (Step transition) ->
              Right (version, step (storedOccurredAt stored) transition txn)

-- | A command the state machine refused. Distinct from
-- 'UPIMock.Domain.Types.ValidationError' (a malformed request) and from
-- 'UPIMock.Domain.ErrorCodes.ErrorCode' (a simulated NPCI decline): this is a
-- well-formed request for a move that the lifecycle does not permit.
data DomainError
  = IllegalTransition Text TxnState
  | AlreadyTerminal Text TxnState
  | AuthorizationNotApplicable Flow
  deriving stock (Eq, Show)

renderDomainError :: DomainError -> Text
renderDomainError = \case
  IllegalTransition cmd state ->
    cmd <> " is not applicable while the transaction is " <> renderTxnState state
  AlreadyTerminal cmd state ->
    cmd <> " was rejected: " <> renderTxnState state <> " is terminal"
  AuthorizationNotApplicable flow ->
    renderFlow flow <> " authorises at initiation; it has no separate authorisation step"

-- | A corrupt or truncated event stream. Every constructor names a condition
-- that cannot arise from this binary's write path, so encountering one means
-- either the database was edited by hand or an upcaster is missing.
data ReplayError
  = ReplayEmptyStream StreamId
  | ReplayMissingSeed StreamId Text
  | ReplayDuplicateSeed StreamId StreamVersion
  | -- | Expected version, then the version actually found.
    ReplayVersionGap StreamId StreamVersion StreamVersion
  | ReplayIllegalEvent StreamId StreamVersion Text TxnState
  deriving stock (Eq, Show)

renderReplayError :: ReplayError -> Text
renderReplayError = \case
  ReplayEmptyStream sid ->
    "stream " <> unStreamId sid <> " has no events"
  ReplayMissingSeed sid found ->
    "stream " <> unStreamId sid <> " begins with " <> found <> " instead of TXN_INITIATED"
  ReplayDuplicateSeed sid at ->
    "stream " <> unStreamId sid <> " has a second TXN_INITIATED at version " <> version at
  ReplayVersionGap sid expected found ->
    "stream "
      <> unStreamId sid
      <> " expected version "
      <> version expected
      <> " but found "
      <> version found
  ReplayIllegalEvent sid at found state ->
    "stream "
      <> unStreamId sid
      <> " version "
      <> version at
      <> ": "
      <> found
      <> " is not reachable from "
      <> renderTxnState state
  where
    version = T.pack . show . unStreamVersion
