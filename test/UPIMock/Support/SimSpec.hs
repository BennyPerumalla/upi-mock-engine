-- |
-- Module      : UPIMock.Support.SimSpec
-- Description : Tests for the test double.
--
-- The in-memory store is load-bearing: every property in the suite is only as
-- meaningful as the double's fidelity to the port laws. So the double is tested
-- first, against the same three guarantees the SQLite adapter claims — versioning,
-- RRN uniqueness, and no partial writes — and against the determinism the rest of
-- the suite assumes.
--
-- The fixture check exists because "UPIMock.Support.Sim" is the one module allowed
-- to call 'error'. If a smart constructor tightens and a fixture literal stops
-- parsing, this spec fails with a clear message instead of an @error@ call
-- surfacing from the middle of an unrelated property.
module UPIMock.Support.SimSpec (spec) where

import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Time (addUTCTime)
import Test.Hspec

import UPIMock.Application.Ports
  ( CommitRequest (..)
  , CommitResult (..)
  , MonadClock (..)
  , MonadEntropy (..)
  , MonadEventStore (..)
  , RrnClaim (..)
  , StoreError (..)
  )
import UPIMock.Domain.Events
  ( AggregateType (..)
  , DomainEvent (..)
  , PendingEvent (..)
  , StoredEvent (..)
  )
import UPIMock.Domain.Transaction (TxnSeed (..))
import UPIMock.Domain.Types
  ( AuthRef (..)
  , EventId (..)
  , Flow (..)
  , Money (..)
  , Party (..)
  , StreamVersion (..)
  , amountPaise
  , noStreamVersion
  , rrnText
  , streamIdOfTxn
  , vpaText
  )
import UPIMock.Support.Sim
  ( SimState (..)
  , alice
  , bob
  , epoch
  , evalSim
  , merchant
  , oneRupee
  , runSim
  , seedFor
  )

spec :: Spec
spec = do
  describe "fixtures" $ do
    it "parse through their smart constructors" $ do
      vpaText (partyVpa alice) `shouldBe` "alice@psp"
      vpaText (partyVpa bob) `shouldBe` "bob@psp"
      vpaText (partyVpa merchant) `shouldBe` "chai.stall@psp"
      amountPaise (moneyAmount oneRupee) `shouldBe` 100
      rrnText (seedRrn (seedFor FlowP2PIntent 7)) `shouldBe` "000000000007"

  describe "commit" $ do
    it "numbers appended events from the expected version plus one" $ do
      let seed = seedFor FlowP2PIntent 1
          (outcome, state) = runSim $ do
            _ <- commit (creationOf seed (TxnInitiated seed :| [TxnAuthorized (AuthRef "auth-1")]))
            commit (appendOf seed (StreamVersion 2) (TxnSucceeded :| []))
      fmap committedVersion outcome `shouldBe` Right (StreamVersion 3)
      map storedVersion (simLog state) `shouldBe` map StreamVersion [1, 2, 3]
      map (unEventId . storedEventId) (simLog state) `shouldBe` [1, 2, 3]

    it "rejects a stale expected version and writes nothing" $ do
      let seed = seedFor FlowP2PIntent 2
          stream = streamIdOfTxn (seedTxnId seed)
          (outcome, state) = runSim $ do
            _ <- commit (creationOf seed (TxnInitiated seed :| []))
            commit (appendOf seed noStreamVersion (TxnSucceeded :| []))
      fmap committedVersion outcome
        `shouldBe` Left (VersionConflict stream noStreamVersion (StreamVersion 1))
      length (simLog state) `shouldBe` 1

    it "rejects a second claim on an RRN and writes nothing" $ do
      let held = seedFor FlowP2PIntent 3
          clashing = (seedFor FlowP2PCollect 4) {seedRrn = seedRrn held}
          (outcome, state) = runSim $ do
            _ <- commit (creationOf held (TxnInitiated held :| []))
            commit (creationOf clashing (TxnInitiated clashing :| []))
      fmap committedVersion outcome `shouldBe` Left (DuplicateRrn (seedRrn held))
      length (simLog state) `shouldBe` 1

    it "keeps streams apart and returns one of them in version order" $ do
      let mine = seedFor FlowP2PIntent 5
          other = seedFor FlowP2PIntent 6
          (events, _) = runSim $ do
            _ <- commit (creationOf mine (TxnInitiated mine :| [TxnAuthorized (AuthRef "auth-5")]))
            _ <- commit (creationOf other (TxnInitiated other :| []))
            readStream (streamIdOfTxn (seedTxnId mine))
      fmap (map storedVersion) events `shouldBe` Right (map StreamVersion [1, 2])

  describe "determinism" $ do
    it "advances the clock by exactly one second per read" $ do
      evalSim (replicateM 3 currentTime)
        `shouldBe` [epoch, addUTCTime 1 epoch, addUTCTime 2 epoch]

    it "never repeats an entropy draw" $ do
      evalSim (replicateM 4 freshWord64) `shouldBe` [1, 2, 3, 4]

-- | A creation commit: expected version zero, and a claim on the seed's RRN.
creationOf :: TxnSeed -> NonEmpty DomainEvent -> CommitRequest
creationOf seed events =
  (appendOf seed noStreamVersion events)
    { commitRrnClaims = [RrnClaim {claimRrn = seedRrn seed, claimTxnId = seedTxnId seed}]
    }

-- | An append to an existing stream: no RRN claim, because the claim was made once
-- at creation and claiming twice is what the duplicate test provokes on purpose.
appendOf :: TxnSeed -> StreamVersion -> NonEmpty DomainEvent -> CommitRequest
appendOf seed expected events =
  CommitRequest
    { commitStreamId = streamIdOfTxn (seedTxnId seed)
    , commitAggregateType = AggTransaction
    , commitExpectedVersion = expected
    , commitEvents = fmap pend events
    , commitRrnClaims = []
    , commitOutbox = []
    }
  where
    pend event = PendingEvent {pendingEvent = event, pendingOccurredAt = epoch}
