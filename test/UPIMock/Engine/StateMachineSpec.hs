-- |
-- Module      : UPIMock.Engine.StateMachineSpec
-- Description : The transition matrix, the rejection taxonomy, and replay.
--
-- Most of the state machine's guarantees are carried by the types: a
-- 'Transition' whose /from/ index does not match the aggregate does not compile,
-- so there is no runtime path for @FAILED -> SUCCESS@ to be tested against. What
-- remains testable — and worth testing — is the part where untyped input meets
-- the typed core:
--
--   * 'decide' maps a @(command, runtime state)@ pair to a proof or a refusal.
--     The matrix below is the diagram in "UPIMock.Domain.Transaction" written as
--     data, and the property asserts agreement in /both/ directions.
--   * The refusal is classified. \"Illegal\" and \"already terminal\" and \"this
--     flow has no authorisation step\" are three different things to a client,
--     and 'decide' picks between them by a guard order that a reader cannot check
--     by inspection.
--   * 'replay' is the inverse of the write path. Every constructor of
--     'ReplayError' names a corruption that this binary cannot produce, so each
--     one is provoked here deliberately.
--
-- The aggregates the matrix runs against are built by walking the /legal/ path
-- with 'applyTransition' — there is no other way to obtain a @Transaction 'Success@,
-- which is the point of not exporting the constructor.
module UPIMock.Engine.StateMachineSpec (spec) where

import Data.Either (isRight)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Hedgehog (forAll, (===))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import UPIMock.Domain.ErrorCodes (ErrorCode (UB, Z9), ReconCode (ReconCreditedOnline))
import UPIMock.Domain.Events
  ( AggregateType (AggTransaction)
  , DomainEvent (..)
  , StoredEvent (..)
  , currentPayloadVersion
  , eventOfTransition
  , eventTypeText
  )
import UPIMock.Domain.Transaction
  ( AnyTransaction (..)
  , StateSing (..)
  , Transaction
  , TxnCore (txnSettledAt, txnUpdatedAt)
  , TxnSeed (..)
  , TxnState (..)
  , Transition (..)
  , anyTxnCore
  , anyTxnState
  , applyTransition
  , demoteState
  , newTransaction
  , sourceSing
  , targetSing
  , transitionLabel
  )
import UPIMock.Domain.Types
  ( AuthRef (..)
  , EventId (..)
  , Flow (..)
  , RefundRef (..)
  , StreamId
  , StreamVersion (..)
  , ValidationError (..)
  , streamIdOfTxn
  )
import UPIMock.Engine.StateMachine
  ( Command (..)
  , Decided (..)
  , DomainError (..)
  , Initiation (..)
  , ReplayError (..)
  , Step (..)
  , applyDecided
  , checkSeed
  , commandLabel
  , decide
  , flowAuthorizesImplicitly
  , initiate
  , replay
  , stepForEvent
  )
import UPIMock.Support.Gen (genAuthRef, genCommand, genSeed, genTxnState)
import UPIMock.Support.Sim (alice, bob, epoch, merchant, seedFor)

spec :: Spec
spec = do
  describe "decide" $ do
    it "admits exactly the moves the diagram draws" $
      [ (commandLabel cmd, state)
      | state <- [minBound .. maxBound]
      , cmd <- commandWitnesses
      , isRight (decide cmd (aggregateIn state))
      ]
        `shouldMatchList` legalMatrix

    it "admits nothing outside the matrix, for any drawn command" $ hedgehog $ do
      -- The list above is hand-maintained; this property is what stops it from
      -- going stale silently. A command constructor added without a matrix entry
      -- fails here the moment it is legal anywhere.
      cmd <- forAll genCommand
      state <- forAll genTxnState
      isRight (decide cmd (aggregateIn state)) === ((commandLabel cmd, state) `elem` legalMatrix)

    it "mints an event that stepForEvent inverts" $
      for_ legalPairs $ \(cmd, state) ->
        withDecision cmd state $ \case
          Decided transition _ ->
            case stepForEvent (sourceSing transition) (eventOfTransition transition) of
              Nothing ->
                expectationFailure
                  ("no inverse for " <> T.unpack (transitionLabel transition))
              Just (Step inverted) ->
                transitionLabel inverted `shouldBe` transitionLabel transition

    it "lands in the state the transition's target names" $
      for_ legalPairs $ \(cmd, state) ->
        withDecision cmd state $ \case
          Decided transition txn -> do
            let (_, next) = applyDecided (at 9) (Decided transition txn)
            anyTxnState next `shouldBe` demoteState (targetSing transition)
            -- The clock is an argument, not a call: this is the assertion that
            -- keeps replay reproducible.
            txnUpdatedAt (anyTxnCore next) `shouldBe` at 9

  describe "decide/rejections" $ do
    it "reports a terminal state as terminal, not merely illegal" $ do
      decision CmdMarkSuccess Failed `shouldBe` Left (AlreadyTerminal "MARK_SUCCESS" Failed)
      decision CmdSettleRefund Refunded `shouldBe` Left (AlreadyTerminal "SETTLE_REFUND" Refunded)

    it "reports an out-of-order move as illegal" $ do
      decision CmdSettleRefund Pending `shouldBe` Left (IllegalTransition "SETTLE_REFUND" Pending)
      decision CmdMarkSuccess Timeout `shouldBe` Left (IllegalTransition "MARK_SUCCESS" Timeout)

    it "tells an Intent client that authorisation is not a separate step" $
      -- Guard order matters here: PENDING is not terminal, so without the
      -- flow check this would be reported as a plain illegal transition and the
      -- client would retry a step that can never apply to this flow.
      outcome (decide (CmdAuthorize authRef) (AnyTransaction SPending intentPending))
        `shouldBe` Left (AuthorizationNotApplicable FlowP2PIntent)

    it "reports the same move on a Collect as merely out of order" $
      outcome (decide (CmdAuthorize authRef) (aggregateIn Pending))
        `shouldBe` Left (IllegalTransition "AUTHORIZE" Pending)

  describe "initiate" $ do
    it "authorises an Intent payment inside the creation commit" $ do
      let result = initiate (seedFor FlowP2PIntent 11) authRef
      map eventTypeText (NE.toList (initiationEvents result))
        `shouldBe` ["TXN_INITIATED", "TXN_AUTHORIZED"]
      anyTxnState (initiationState result) `shouldBe` Pending

    it "leaves a Collect request waiting for the payer" $ do
      let result = initiate (seedFor FlowP2PCollect 12) authRef
      map eventTypeText (NE.toList (initiationEvents result)) `shouldBe` ["TXN_INITIATED"]
      anyTxnState (initiationState result) `shouldBe` Initiated

    it "agrees with flowAuthorizesImplicitly, and always seeds first" $ hedgehog $ do
      seed <- forAll genSeed
      ref <- forAll genAuthRef
      let result = initiate seed ref
      -- 'replay' assumes the head is the seed. Asserting it here is what makes
      -- that assumption a property of 'initiate' rather than a comment.
      NE.head (initiationEvents result) === TxnInitiated seed
      anyTxnState (initiationState result)
        === (if flowAuthorizesImplicitly (seedFlow seed) then Pending else Initiated)

  describe "checkSeed" $ do
    it "accepts every generated seed" $ hedgehog $ do
      seed <- forAll genSeed
      checkSeed seed === Right seed

    it "rejects a payer paying itself" $
      checkSeed (collectSeed {seedPayee = alice}) `shouldBe` Left (PayerIsPayee "alice@psp")

    it "requires a merchant id on the QR flow" $
      checkSeed ((seedFor FlowP2MQr 13) {seedPayee = bob}) `shouldBe` Left MerchantIdRequired

    it "forbids a merchant id on the person-to-person flows" $
      checkSeed ((seedFor FlowP2PIntent 14) {seedPayee = merchant})
        `shouldBe` Left MerchantIdForbidden

  describe "replay" $ do
    it "rebuilds the aggregate the write path produced" $
      summary (replay stream settledLog) `shouldBe` Right (StreamVersion 3, Success)

    it "takes its timestamps from the log rather than from a clock" $
      fmap (txnSettledAt . anyTxnCore . snd) (replay stream settledLog)
        `shouldBe` Right (Just (at 3))

    it "refuses an empty stream" $
      summary (replay stream []) `shouldBe` Left (ReplayEmptyStream stream)

    it "refuses a stream that does not begin with the seed" $
      summary (replay stream (logOf [TxnSucceeded]))
        `shouldBe` Left (ReplayMissingSeed stream "TXN_SUCCEEDED")

    it "refuses a stream whose first event is not version one" $
      summary (replay stream [storedAt (StreamVersion 2) (TxnInitiated collectSeed)])
        `shouldBe` Left (ReplayVersionGap stream (StreamVersion 1) (StreamVersion 2))

    it "refuses a gap" $
      summary
        ( replay
            stream
            [ storedAt (StreamVersion 1) (TxnInitiated collectSeed)
            , storedAt (StreamVersion 3) (TxnAuthorized authRef)
            ]
        )
        `shouldBe` Left (ReplayVersionGap stream (StreamVersion 2) (StreamVersion 3))

    it "refuses a second seed" $
      summary (replay stream (logOf [TxnInitiated collectSeed, TxnInitiated collectSeed]))
        `shouldBe` Left (ReplayDuplicateSeed stream (StreamVersion 2))

    it "refuses an event the state machine could not have produced" $
      summary (replay stream (logOf [TxnInitiated collectSeed, TxnSucceeded]))
        `shouldBe` Left (ReplayIllegalEvent stream (StreamVersion 2) "TXN_SUCCEEDED" Initiated)

    it "cannot reconcile a failed transaction into success" $
      -- FAILED is absorbing by construction: no 'Transition' names it as a
      -- source, so 'stepForEvent' has no case to offer and a log that claims
      -- otherwise is reported as corrupt instead of folded into a plausible lie.
      summary
        ( replay
            stream
            ( logOf
                [ TxnInitiated collectSeed
                , TxnAuthorized authRef
                , TxnFailed Z9
                , TxnReconciled ReconCreditedOnline
                ]
            )
        )
        `shouldBe` Left (ReplayIllegalEvent stream (StreamVersion 4) "TXN_RECONCILED" Failed)

-- | The Collect fixture. Chosen as the default because it is the one flow whose
-- authorisation is an explicit payer decision, which is what makes @INITIATED@
-- observable and every state in the matrix reachable by a legal path.
collectSeed :: TxnSeed
collectSeed = seedFor FlowP2PCollect 1

stream :: StreamId
stream = streamIdOfTxn (seedTxnId collectSeed)

authRef :: AuthRef
authRef = AuthRef "auth-fixture"

refundRef :: RefundRef
refundRef = RefundRef "rfnd-fixture"

-- | @n@ seconds after 'epoch'. Distinct per version, so an assertion on a
-- timestamp can only pass if the value came from the event it claims to.
at :: Int64 -> UTCTime
at n = addUTCTime (fromIntegral n) epoch

-- The legal ladder. Each rung can only be built from the one above it: the
-- 'Transaction' constructor is not exported, so there is no way to fabricate a
-- @Transaction 'Success@ and no way for this fixture set to drift from the
-- diagram it illustrates.
initiatedTxn :: Transaction 'Initiated
initiatedTxn = newTransaction collectSeed

pendingTxn :: Transaction 'Pending
pendingTxn = applyTransition (at 1) (Authorize authRef) initiatedTxn

succeededTxn :: Transaction 'Success
succeededTxn = applyTransition (at 2) MarkSuccess pendingTxn

failedTxn :: Transaction 'Failed
failedTxn = applyTransition (at 2) (MarkFailed Z9) pendingTxn

timedOutTxn :: Transaction 'Timeout
timedOutTxn = applyTransition (at 2) MarkTimeout pendingTxn

refundPendingTxn :: Transaction 'RefundPending
refundPendingTxn = applyTransition (at 3) (OpenRefund refundRef) succeededTxn

refundedTxn :: Transaction 'Refunded
refundedTxn = applyTransition (at 4) SettleRefund refundPendingTxn

-- | The same rung for a flow that authorises implicitly. Its only purpose is to
-- provoke 'AuthorizationNotApplicable', which is a statement about the flow and
-- not about the state.
intentPending :: Transaction 'Pending
intentPending =
  applyTransition (at 1) (Authorize authRef) (newTransaction (seedFor FlowP2PIntent 2))

-- | Exhaustive by compiler check: a new state must be given a representative
-- here, and the only way to give it one is to find a legal path to it.
aggregateIn :: TxnState -> AnyTransaction
aggregateIn = \case
  Initiated -> AnyTransaction SInitiated initiatedTxn
  Pending -> AnyTransaction SPending pendingTxn
  Success -> AnyTransaction SSuccess succeededTxn
  Failed -> AnyTransaction SFailed failedTxn
  Timeout -> AnyTransaction STimeout timedOutTxn
  RefundPending -> AnyTransaction SRefundPending refundPendingTxn
  Refunded -> AnyTransaction SRefunded refundedTxn

-- | One witness per 'Command' constructor. The only hand-maintained inventory in
-- this spec; the hedgehog property above draws from 'genCommand' instead, which is
-- what catches an addition here that was forgotten.
commandWitnesses :: [Command]
commandWitnesses =
  [ CmdAuthorize authRef
  , CmdDeclineAtPsp Z9
  , CmdMarkSuccess
  , CmdMarkFailed Z9
  , CmdMarkTimeout
  , CmdReconcile ReconCreditedOnline
  , CmdDropInReconciliation UB
  , CmdOpenRefund refundRef
  , CmdSettleRefund
  ]

-- | The diagram in "UPIMock.Domain.Transaction", as data. Nine moves; every other
-- @(command, state)@ pair must be refused.
legalMatrix :: [(Text, TxnState)]
legalMatrix =
  [ ("AUTHORIZE", Initiated)
  , ("DECLINE_AT_PSP", Initiated)
  , ("MARK_SUCCESS", Pending)
  , ("MARK_FAILED", Pending)
  , ("MARK_TIMEOUT", Pending)
  , ("RECONCILE", Timeout)
  , ("DROP_IN_RECONCILIATION", Timeout)
  , ("OPEN_REFUND", Success)
  , ("SETTLE_REFUND", RefundPending)
  ]

-- | Each command witness paired with the state it is legal in, derived from the
-- matrix rather than restated, so the two cannot disagree.
legalPairs :: [(Command, TxnState)]
legalPairs =
  [ (cmd, state)
  | cmd <- commandWitnesses
  , (cmdLabel, state) <- legalMatrix
  , cmdLabel == commandLabel cmd
  ]

-- | Run an assertion against a decision that must succeed, reporting the refusal
-- if it does not instead of failing on an opaque pattern match.
withDecision :: Command -> TxnState -> (Decided -> Expectation) -> Expectation
withDecision cmd state assertion =
  either (expectationFailure . show) assertion (decide cmd (aggregateIn state))

-- | 'Decided' hides the transition's indices, so it has no 'Eq' and no 'Show'.
-- The label identifies it without needing either.
outcome :: Either DomainError Decided -> Either DomainError Text
outcome = fmap (\case Decided transition _ -> transitionLabel transition)

decision :: Command -> TxnState -> Either DomainError Text
decision cmd state = outcome (decide cmd (aggregateIn state))

-- | A log row as the store would have written it. @storedRecordedAt@ is the wall
-- clock and deliberately differs from @storedOccurredAt@: 'replay' must use the
-- latter, and using the former would show up as a failed timestamp assertion.
storedAt :: StreamVersion -> DomainEvent -> StoredEvent
storedAt version event =
  StoredEvent
    { storedEventId = EventId (unStreamVersion version)
    , storedStreamId = stream
    , storedAggregateType = AggTransaction
    , storedVersion = version
    , storedEvent = event
    , storedOccurredAt = at (unStreamVersion version)
    , storedRecordedAt = epoch
    , storedPayloadVersion = currentPayloadVersion
    }

-- | Number a list of events from version one, the way a healthy stream is
-- numbered. Every corruption case in the spec bypasses this and numbers by hand.
logOf :: [DomainEvent] -> [StoredEvent]
logOf = zipWith storedAt (map StreamVersion [1 ..])

settledLog :: [StoredEvent]
settledLog = logOf [TxnInitiated collectSeed, TxnAuthorized authRef, TxnSucceeded]

-- | 'AnyTransaction' has no 'Eq' either; the version and the state are what the
-- replay assertions are about.
summary ::
  Either ReplayError (StreamVersion, AnyTransaction) ->
  Either ReplayError (StreamVersion, TxnState)
summary = fmap (fmap anyTxnState)
