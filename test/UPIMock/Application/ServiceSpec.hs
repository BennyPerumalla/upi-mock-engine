-- |
-- Module      : UPIMock.Application.ServiceSpec
-- Description : The write path, end to end, over the in-memory ports.
--
-- The domain specs assert that a transition is legal. This one asserts that the
-- effects a legal transition implies happen in the right order and are
-- all-or-nothing: the log is appended, the outbox row is written in the same
-- commit, and the read model is published from the version the store returned.
-- Every example runs in "UPIMock.Support.Sim", so a failure here is a failure of
-- the use case and not of SQLite.
--
-- Two claims are worth naming, because a reader of
-- "UPIMock.Application.Service" cannot check either by inspection:
--
--   * /The rebuild is the projection./ The live read model is folded forward one
--     command at a time; 'rebuildReadModel' folds the same streams from the log
--     through 'UPIMock.Engine.StateMachine.replay'. The two maps are compared
--     whole. That comparison is what makes a crash between the commit and the
--     publish survivable rather than merely unlikely.
--   * /A refusal writes nothing./ Every rejection example asserts on the log and
--     the projection as well as on the error value, because a use case that
--     returns 'Left' after appending is precisely the defect an @Either@ return
--     type conceals.
--
-- What is deliberately /not/ asserted here is the store's version guard.
-- Provoking it requires two writers interleaved between one caller's read and its
-- commit, which a pure @State@ monad cannot express; it is asserted against the
-- double directly in "UPIMock.Support.SimSpec". What a repeated command meets in
-- this module is 'decide'.
module UPIMock.Application.ServiceSpec (spec) where

import Control.Monad.State.Strict (gets, modify')
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import UPIMock.Application.Ports
  ( MonadReadModel (..)
  , OutboxDraft (..)
  , StoreError (DuplicateRrn)
  , ViewQuery (..)
  , defaultViewQuery
  )
import UPIMock.Application.Service
  ( InitiateRequest (..)
  , ServiceError (..)
  , applyCommand
  , fetchEventLog
  , initiateTransaction
  , rebuildReadModel
  , renderServiceError
  )
import UPIMock.Domain.ErrorCodes (ErrorCode (UB, Z9), ReconCode (ReconCreditedOnline))
import UPIMock.Domain.Events
  ( AggregateType (AggTransaction)
  , DomainEvent (TxnSucceeded)
  , StoredEvent (..)
  , currentPayloadVersion
  , eventTypeText
  )
import UPIMock.Domain.Transaction (TxnSeed (..), TxnState (..), TxnView (..))
import UPIMock.Domain.Types
  ( AuthRef (..)
  , EventId (..)
  , Flow (..)
  , RefundRef (..)
  , StreamVersion (..)
  , TxnId
  , ValidationError (..)
  , streamIdOfTxn
  )
import UPIMock.Engine.StateMachine
  ( Command (..)
  , DomainError (..)
  , ReplayError (..)
  , flowAuthorizesImplicitly
  )
import UPIMock.Support.Gen (genFlow)
import UPIMock.Support.Sim
  ( Sim
  , SimState (..)
  , epoch
  , merchant
  , requestFor
  , runSim
  , seedFor
  )

spec :: Spec
spec = do
  describe "initiateTransaction" $ do
    it "leaves a Collect request waiting for the payer" $
      withRun (initiateTransaction (requestFor FlowP2PCollect)) $ \view state -> do
        viewState view `shouldBe` Initiated
        viewVersion view `shouldBe` StreamVersion 1
        -- The seed's timestamp is the first clock read, not the commit's.
        viewCreatedAt view `shouldBe` epoch
        map (eventTypeText . storedEvent) (simLog state) `shouldBe` ["TXN_INITIATED"]

    it "authorises an Intent payment inside the creation commit" $
      withRun (initiateTransaction (requestFor FlowP2PIntent)) $ \view state -> do
        viewState view `shouldBe` Pending
        viewVersion view `shouldBe` StreamVersion 2
        map (eventTypeText . storedEvent) (simLog state)
          `shouldBe` ["TXN_INITIATED", "TXN_AUTHORIZED"]

    it "publishes the view under both indices before returning" $
      withRun (initiateTransaction (requestFor FlowP2MQr)) $ \view state -> do
        Map.lookup (viewTxnId view) (simViews state) `shouldBe` Just view
        Map.lookup (viewRrn view) (simRrnViews state) `shouldBe` Just (viewTxnId view)

    it "writes one topic-tagged outbox row per event" $
      withRun (initiateTransaction (requestFor FlowP2PIntent)) $ \_ state ->
        map outboxTopic (simOutbox state) `shouldBe` ["TXN_INITIATED", "TXN_AUTHORIZED"]

    it "draws the same entropy whatever the request asks for" $ hedgehog $ do
      -- Unconditional draws are a decision, not an oversight: a seeded Phase-2 run
      -- must mint the same identifiers for the same request ordinal regardless of
      -- the flow, and regardless of whether the client supplied an RRN of its own
      -- and left the minted one dead.
      flow <- forAll genFlow
      let drawsFor request = simCounter (snd (runSim (initiateTransaction request)))
      drawsFor (requestFor flow) === 3
      drawsFor ((requestFor flow) {initRrn = Just (seedRrn (seedFor flow 42))}) === 3

    it "refuses a cross-field violation and writes nothing" $ do
      let payingAMerchantPerson = (requestFor FlowP2PCollect) {initPayee = merchant}
          (result, state) = runSim (initiateTransaction payingAMerchantPerson)
      result `shouldBe` Left (ServiceInvalid MerchantIdForbidden)
      simLog state `shouldBe` []
      simOutbox state `shouldBe` []

    it "refuses a second claim on an RRN, leaving the first transaction whole" $ do
      let held = seedRrn (seedFor FlowP2PCollect 42)
          reuse = (requestFor FlowP2PCollect) {initRrn = Just held}
          (result, state) = runSim (initiateTransaction reuse >> initiateTransaction reuse)
      result `shouldBe` Left (ServiceStore (DuplicateRrn held))
      length (simLog state) `shouldBe` 1
      Map.size (simViews state) `shouldBe` 1

  describe "applyCommand" $ do
    it "walks a Collect through the payer's decision to settlement" $
      withRun (walk FlowP2PCollect [CmdAuthorize authRef, CmdMarkSuccess]) $ \views _ -> do
        map viewState (NE.toList views) `shouldBe` [Initiated, Pending, Success]
        map viewVersion (NE.toList views) `shouldBe` map StreamVersion [1, 2, 3]

    it "walks an Intent payment to the same place in one step fewer" $
      withRun (walk FlowP2PIntent [CmdMarkSuccess]) $ \views _ -> do
        map viewState (NE.toList views) `shouldBe` [Pending, Success]
        map viewVersion (NE.toList views) `shouldBe` map StreamVersion [2, 3]

    it "reaches SUCCESS at version three whatever the flow" $ hedgehog $ do
      -- The number of commands differs by flow; the number of events does not.
      flow <- forAll genFlow
      let (result, state) = runSim (walk flow (settlementPath flow))
          final = fmap NE.last result
      fmap viewState final === Right Success
      fmap viewVersion final === Right (StreamVersion 3)
      -- And the projection holds the last committed version, not an earlier one.
      fmap (\view -> Map.lookup (viewTxnId view) (simViews state)) final === fmap Just final

    it "stamps the settlement from the settling command's own clock read" $
      withRun (walk FlowP2PIntent [CmdMarkSuccess]) $ \views _ ->
        case NE.toList views of
          [opened, settled] -> do
            viewSettledAt opened `shouldBe` Nothing
            viewSettledAt settled `shouldBe` Just (viewUpdatedAt settled)
            viewUpdatedAt settled `shouldSatisfy` (> viewUpdatedAt opened)
          _ -> expectationFailure "expected one view per write"

    it "records the decline code, and settles nothing" $
      withRun (walk FlowP2PIntent [CmdMarkFailed Z9]) $ \views _ -> do
        let declined = NE.last views
        viewState declined `shouldBe` Failed
        viewErrorCode declined `shouldBe` Just Z9
        viewSettledAt declined `shouldBe` Nothing

    it "credits a timed-out payment when reconciliation says the money moved" $
      withRun (walk FlowP2MQr [CmdMarkTimeout, CmdReconcile ReconCreditedOnline]) $ \views _ -> do
        let reconciled = NE.last views
        viewState reconciled `shouldBe` Success
        viewReconCode reconciled `shouldBe` Just ReconCreditedOnline
        viewErrorCode reconciled `shouldBe` Nothing
        viewSettledAt reconciled `shouldBe` Just (viewUpdatedAt reconciled)

    it "opens and settles a refund against a settled transaction" $
      withRun (walk FlowP2PIntent [CmdMarkSuccess, CmdOpenRefund refundRef, CmdSettleRefund]) $
        \views _ -> do
          map viewState (NE.toList views)
            `shouldBe` [Pending, Success, RefundPending, Refunded]
          viewRefundRef (NE.last views) `shouldBe` Just refundRef

    it "refuses a command the diagram has no move for, and writes nothing" $ do
      let (result, state) = runSim (walk FlowP2PCollect [CmdMarkSuccess])
      result `shouldBe` Left (ServiceDomain (IllegalTransition "MARK_SUCCESS" Initiated))
      length (simLog state) `shouldBe` 1

    it "refuses a repeat of a command that already landed" $ do
      -- This is what a lost race looks like to a Phase-1 client: the second writer
      -- is refused by the state machine, and the rejection names the state it lost
      -- to. The store's version guard covers the interleaving this monad cannot
      -- produce; see "UPIMock.Support.SimSpec".
      let (result, state) = runSim (walk FlowP2PIntent [CmdMarkSuccess, CmdMarkSuccess])
      result `shouldBe` Left (ServiceDomain (IllegalTransition "MARK_SUCCESS" Success))
      length (simLog state) `shouldBe` 3

    it "reports an unknown transaction as absent, not as a corrupt log" $ do
      let missing = seedTxnId (seedFor FlowP2PIntent 99)
      fst (runSim (applyCommand missing CmdMarkSuccess))
        `shouldBe` Left (ServiceNotFound missing)

  describe "fetchEventLog" $ do
    it "returns the stream the write path wrote, in version order" $ do
      let (result, _) = runSim . runExceptT $ do
            views <- ExceptT (walk FlowP2PCollect [CmdAuthorize authRef, CmdMarkSuccess])
            ExceptT (fetchEventLog (viewTxnId (NE.head views)))
      fmap (map (eventTypeText . storedEvent)) result
        `shouldBe` Right ["TXN_INITIATED", "TXN_AUTHORIZED", "TXN_SUCCEEDED"]
      fmap (map storedVersion) result `shouldBe` Right (map StreamVersion [1, 2, 3])

    it "reports an unknown transaction as absent" $ do
      let missing = seedTxnId (seedFor FlowP2PIntent 98)
      fst (runSim (fetchEventLog missing)) `shouldBe` Left (ServiceNotFound missing)

  describe "rebuildReadModel" $ do
    it "reproduces exactly what the write path published" $ do
      let ((built, live, counted, rebuilt), _) = runSim $ do
            outcome <- population
            before <- gets simViews
            replaceAllViews []
            total <- rebuildReadModel
            after <- gets simViews
            pure (outcome, before, total, after)
      fmap length built `shouldBe` Right 3
      counted `shouldBe` Right 3
      rebuilt `shouldBe` live

    it "leaves the projection untouched when a stream will not fold" $
      withRun forgedRebuild $ \(txnId, rebuilt) state -> do
        rebuilt
          `shouldBe` Left
            ( ServiceReplay
                ( ReplayIllegalEvent
                    (streamIdOfTxn txnId)
                    (StreamVersion 2)
                    "TXN_SUCCEEDED"
                    Initiated
                )
            )
        -- 'replaceAllViews' is never reached, so a corrupt log leaves the live
        -- projection standing instead of replacing it with a partial one.
        fmap viewState (Map.lookup txnId (simViews state)) `shouldBe` Just Initiated

  describe "queryViews" $
    it "filters the collection by what the write path actually left behind" $ do
      let ((everything, settled, qrOnly), _) = runSim $ do
            _ <- population
            (,,)
              <$> queryViews defaultViewQuery
              <*> queryViews (defaultViewQuery {queryState = Just Success})
              <*> queryViews (defaultViewQuery {queryFlow = Just FlowP2MQr})
      map viewState everything `shouldMatchList` [Initiated, Success, Failed]
      map viewState settled `shouldBe` [Success]
      map viewFlow qrOnly `shouldBe` [FlowP2MQr]

authRef :: AuthRef
authRef = AuthRef "auth-fixture"

refundRef :: RefundRef
refundRef = RefundRef "rfnd-fixture"

-- | Open a transaction for a flow, then apply commands in order, collecting the
-- view each write published.
--
-- The trace is the assertion surface. A use case that projected a stale aggregate,
-- or published before the commit returned its version, shows up here as a version
-- out of step rather than as a final state that happens to look right.
walk :: Flow -> [Command] -> Sim (Either ServiceError (NonEmpty TxnView))
walk flow commands = runExceptT $ do
  opened <- ExceptT (initiateTransaction (requestFor flow))
  rest <- traverse (ExceptT . applyCommand (viewTxnId opened)) commands
  pure (opened :| rest)

-- | The commands that carry a flow from initiation to @SUCCESS@: a Collect waits
-- on the payer's decision, an Intent or QR payment has already made it.
settlementPath :: Flow -> [Command]
settlementPath flow
  | flowAuthorizesImplicitly flow = [CmdMarkSuccess]
  | otherwise = [CmdAuthorize authRef, CmdMarkSuccess]

-- | Three streams left in three different states, so that a rebuild has something
-- to get wrong: one waiting on the payer, one settled, one failed because
-- reconciliation confirmed the credit never happened.
population :: Sim (Either ServiceError [NonEmpty TxnView])
population =
  runExceptT $
    traverse
      (ExceptT . uncurry walk)
      [ (FlowP2PCollect, [])
      , (FlowP2PIntent, [CmdMarkSuccess])
      , (FlowP2MQr, [CmdMarkTimeout, CmdDropInReconciliation UB])
      ]

-- | Open a Collect, forge an event onto its stream, then attempt a rebuild. The
-- rebuild's own failure is returned as a value rather than thrown, because it is
-- the thing under test.
forgedRebuild :: Sim (Either ServiceError (TxnId, Either ServiceError Int))
forgedRebuild = runExceptT $ do
  view <- ExceptT (initiateTransaction (requestFor FlowP2PCollect))
  lift (forge (viewTxnId view))
  rebuilt <- lift rebuildReadModel
  pure (viewTxnId view, rebuilt)

-- | Append a row the write path could not have produced: @TXN_SUCCEEDED@ on a
-- stream resting in @INITIATED@.
--
-- It goes straight into the log rather than through 'commit', because 'commit' is
-- not what is under test — a hand-edited database is, and 'rebuildReadModel' is
-- the only code that reads the whole log.
forge :: TxnId -> Sim ()
forge txnId = modify' (\state -> state {simLog = simLog state <> [row (simLog state)]})
  where
    row existing =
      StoredEvent
        { storedEventId = EventId (fromIntegral (length existing) + 1)
        , storedStreamId = streamIdOfTxn txnId
        , storedAggregateType = AggTransaction
        , storedVersion = StreamVersion 2
        , storedEvent = TxnSucceeded
        , storedOccurredAt = epoch
        , storedRecordedAt = epoch
        , storedPayloadVersion = currentPayloadVersion
        }

-- | Run a write path that must succeed, handing its result and the final state to
-- the assertion. A refusal is reported through 'renderServiceError' — the same
-- text the HTTP layer would have returned — rather than as an opaque
-- pattern-match failure.
withRun :: Sim (Either ServiceError a) -> (a -> SimState -> Expectation) -> Expectation
withRun action assertion = case runSim action of
  (Left err, _) -> expectationFailure (T.unpack (renderServiceError err))
  (Right value, state) -> assertion value state
