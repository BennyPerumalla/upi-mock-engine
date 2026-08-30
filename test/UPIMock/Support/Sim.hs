-- |
-- Module      : UPIMock.Support.Sim
-- Description : The in-memory port implementation the property tests run against.
--
-- Every use case in "UPIMock.Application.Service" is polymorphic in @m@ and
-- constrained only by "UPIMock.Application.Ports". This module supplies a second
-- inhabitant of those constraints — a pure @State@ monad — which is what makes the
-- test suite a test of the /engine/ rather than a test of SQLite.
--
-- __The double is not a stub.__ It enforces the same three laws the SQLite adapter
-- does: per-stream optimistic concurrency, global RRN uniqueness, and
-- all-or-nothing commits. A double that accepted writes the real store would
-- reject would turn every passing property into a false negative, so
-- "UPIMock.Support.SimSpec" tests the double itself against those laws before any
-- other spec relies on it.
--
-- __Determinism is the point.__ 'currentTime' advances by exactly one second per
-- call and 'freshUuid' counts, so a failing property prints a scenario that
-- reproduces byte for byte. This is the same discipline Phase 2 applies to the
-- real binary with a seeded generator; here it comes for free.
module UPIMock.Support.Sim
  ( -- * The monad
    Sim
  , runSim
  , evalSim

    -- * State
  , SimState (..)
  , emptySim

    -- * Fixtures
  , epoch
  , alice
  , bob
  , merchant
  , oneRupee
  , seedFor
  , requestFor
  , fixture
  ) where

import Control.Monad.State.Strict (MonadState, State, get, gets, modify', put, runState)
import Data.List (find, foldl', sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import Data.UUID qualified as UUID
import Data.Word (Word64)

import UPIMock.Application.Ports
  ( CommitRequest (..)
  , CommitResult (..)
  , MonadClock (..)
  , MonadEntropy (..)
  , MonadEventStore (..)
  , MonadReadModel (..)
  , OutboxDraft
  , RrnClaim (..)
  , StoreError (..)
  , ViewQuery (..)
  )
import UPIMock.Application.Service (InitiateRequest (..))
import UPIMock.Domain.Events
  ( PendingEvent (..)
  , StoredEvent (..)
  , currentPayloadVersion
  )
import UPIMock.Domain.Transaction (TxnSeed (..), TxnView (..))
import UPIMock.Domain.Types
  ( Currency (INR)
  , EventId (..)
  , Flow
  , MerchantId (..)
  , Money (..)
  , Party (..)
  , Rrn
  , StreamId
  , StreamVersion
  , TxnId (..)
  , flowIsMerchant
  , mkAmount
  , mkRrn
  , mkVpa
  , nextVersion
  , noStreamVersion
  )

-- | Log, indices, projection, clock and entropy counter.
--
-- The log is one list for the whole simulator rather than a @Map StreamId [..]@,
-- because that is the shape the real table has: 'readAllEvents' must return a
-- global order, and a per-stream map would make the boot-time rebuild pass
-- trivially while the SQL version still had something to get wrong.
data SimState = SimState
  { simLog :: [StoredEvent]
  , simRrnClaims :: Map Rrn TxnId
  , simOutbox :: [OutboxDraft]
  , simViews :: Map TxnId TxnView
  , simRrnViews :: Map Rrn TxnId
  , simClock :: UTCTime
  , simCounter :: Word64
  }
  deriving stock (Eq, Show)

emptySim :: SimState
emptySim =
  SimState
    { simLog = []
    , simRrnClaims = Map.empty
    , simOutbox = []
    , simViews = Map.empty
    , simRrnViews = Map.empty
    , simClock = epoch
    , simCounter = 0
    }

newtype Sim a = Sim (State SimState a)
  deriving newtype (Functor, Applicative, Monad, MonadState SimState)

runSim :: Sim a -> (a, SimState)
runSim (Sim action) = runState action emptySim

evalSim :: Sim a -> a
evalSim = fst . runSim

-- | One second per call, never repeating. Distinct timestamps are what make the
-- read model's @updatedAt@ ordering assertable; a frozen clock would let a
-- pagination bug hide behind ties.
instance MonadClock Sim where
  currentTime = do
    now <- gets simClock
    modify' (\state -> state {simClock = addUTCTime 1 now})
    pure now

-- | Counting, not random. Both draws come off the same counter for the same
-- reason the real instance derives both from one UUID: an identifier that appears
-- in a failing scenario has to be traceable to the draw that produced it.
instance MonadEntropy Sim where
  freshUuid = do
    n <- nextCounter
    pure (UUID.fromWords 0 0 0 (fromIntegral n))

  freshWord64 = nextCounter

nextCounter :: Sim Word64
nextCounter = do
  n <- gets simCounter
  modify' (\state -> state {simCounter = n + 1})
  pure (n + 1)

-- | The three laws, enforced.
--
-- 'commit' is written as a pure function over the state ('applyCommit') and then
-- installed, so a rejected commit cannot leave a partial write behind: there is
-- no intermediate state to leak. That is the double's version of the SQLite
-- adapter's @withTransaction@, and it is the reason a property that passes here
-- means something about the engine.
instance MonadEventStore Sim where
  readStream stream = Right . filter ((== stream) . storedStreamId) <$> gets simLog

  readAllEvents = Right <$> gets simLog

  commit request = do
    recordedAt <- currentTime
    state <- get
    case applyCommit recordedAt state request of
      Left err -> pure (Left err)
      Right (result, state') -> do
        put state'
        pure (Right result)

applyCommit ::
  UTCTime ->
  SimState ->
  CommitRequest ->
  Either StoreError (CommitResult, SimState)
applyCommit recordedAt state request
  | actual /= expected = Left (VersionConflict stream expected actual)
  | Just clash <- taken = Left (DuplicateRrn (claimRrn clash))
  | otherwise = Right (result, state')
  where
    stream = commitStreamId request
    expected = commitExpectedVersion request
    actual = currentVersion stream state
    taken =
      find (\claim -> Map.member (claimRrn claim) (simRrnClaims state)) (commitRrnClaims request)

    numbered = NE.zip (NE.iterate nextVersion (nextVersion expected)) (commitEvents request)
    offsets = NE.iterate (+ 1) (1 :: Int)
    baseId = length (simLog state)
    stored = fmap place (NE.zip offsets numbered)
    place (offset, (version, pending)) =
      StoredEvent
        { storedEventId = EventId (fromIntegral (baseId + offset))
        , storedStreamId = stream
        , storedAggregateType = commitAggregateType request
        , storedVersion = version
        , storedEvent = pendingEvent pending
        , storedOccurredAt = pendingOccurredAt pending
        , storedRecordedAt = recordedAt
        , storedPayloadVersion = currentPayloadVersion
        }

    result =
      CommitResult
        { committedVersion = storedVersion (NE.last stored)
        , committedEvents = stored
        }
    state' =
      state
        { simLog = simLog state <> NE.toList stored
        , simRrnClaims = foldl' hold (simRrnClaims state) (commitRrnClaims request)
        , simOutbox = simOutbox state <> commitOutbox request
        }
    hold claims claim = Map.insert (claimRrn claim) (claimTxnId claim) claims

-- | Highest version in a stream, or 'noStreamVersion' for a stream that does not
-- exist. @foldr (max . f)@ rather than @maximum . map f@ so that the empty case is
-- the identity instead of an exception.
currentVersion :: StreamId -> SimState -> StreamVersion
currentVersion stream =
  foldr (max . storedVersion) noStreamVersion
    . filter ((== stream) . storedStreamId)
    . simLog

-- | The projection, with the real adapter's semantics reproduced exactly: a
-- monotonic version guard on writes, both indices updated together, and a total
-- sort order so that pagination is stable.
--
-- The guard matters more than it looks. Two commits on the same stream can be
-- projected out of order by a concurrent caller, and without the guard the older
-- view would win and the read model would go backwards. Reproducing the guard here
-- is what lets a property assert that the projection always shows the latest
-- committed version.
instance MonadReadModel Sim where
  putView view = modify' (insertView view)

  getView txnId = gets (Map.lookup txnId . simViews)

  getViewByRrn rrn = do
    state <- get
    pure (Map.lookup rrn (simRrnViews state) >>= \txnId -> Map.lookup txnId (simViews state))

  queryViews query =
    gets (page . sortOn ordering . filter matches . Map.elems . simViews)
    where
      ordering view = (Down (viewUpdatedAt view), Down (viewVersion view), viewTxnId view)
      matches view =
        maybe True (== viewState view) (queryState query)
          && maybe True (== viewFlow view) (queryFlow query)
      page = take (max 0 (queryLimit query)) . drop (max 0 (queryOffset query))

  replaceAllViews views =
    modify' (\state -> foldl' (flip insertView) (clear state) views)
    where
      clear state = state {simViews = Map.empty, simRrnViews = Map.empty}

insertView :: TxnView -> SimState -> SimState
insertView view state
  | Just existing <- Map.lookup key (simViews state)
  , viewVersion existing >= viewVersion view =
      state
  | otherwise =
      state
        { simViews = Map.insert key view (simViews state)
        , simRrnViews = Map.insert (viewRrn view) key (simRrnViews state)
        }
  where
    key = viewTxnId view

-- | Fixed start of every simulated timeline. A date rather than
-- 'Data.Time.getCurrentTime' so that a golden assertion on a timestamp is
-- possible at all.
epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

-- | Force a fixture through its smart constructor, aborting the suite if it no
-- longer parses.
--
-- This is the only 'error' in the project, and @.hlint.yaml@ exempts this module
-- by name. The alternative — threading 'Either' through every fixture — would put
-- a validation case in every test and obscure what each test is actually about. A
-- fixture that stops parsing is a broken suite, not a failing assertion, and
-- 'UPIMock.Support.SimSpec' asserts that all of them still parse so the exemption
-- cannot hide a defect in the constructors themselves.
fixture :: Show e => Either e a -> a
fixture = either (error . ("invalid fixture: " <>) . show) id

-- | Payer for every fixture flow.
alice :: Party
alice =
  Party
    { partyVpa = fixture (mkVpa "alice@psp")
    , partyName = Just "Alice"
    , partyMerchantId = Nothing
    }

-- | Payee for the P2P flows.
bob :: Party
bob =
  Party
    { partyVpa = fixture (mkVpa "bob@psp")
    , partyName = Just "Bob"
    , partyMerchantId = Nothing
    }

-- | Payee for @P2M_QR@. Carries the merchant id that 'checkSeed' requires of that
-- flow and forbids of the others, so a test that swaps payees is testing the rule.
merchant :: Party
merchant =
  Party
    { partyVpa = fixture (mkVpa "chai.stall@psp")
    , partyName = Just "Chai Stall"
    , partyMerchantId = Just (MerchantId "MER0000001")
    }

-- | INR 1.00, in paise.
oneRupee :: Money
oneRupee = Money {moneyAmount = fixture (mkAmount 100), moneyCurrency = INR}

-- | A seed whose identity is derived from @n@, so that two seeds in one test are
-- distinguishable without a generator and the same @n@ always yields the same
-- transaction.
seedFor :: Flow -> Int -> TxnSeed
seedFor flow n =
  TxnSeed
    { seedTxnId = TxnId (UUID.fromWords 0 0 0 (fromIntegral n))
    , seedRrn = fixture (mkRrn (T.justifyRight 12 '0' (T.pack (show n))))
    , seedFlow = flow
    , seedPayer = alice
    , seedPayee = payeeFor flow
    , seedMoney = oneRupee
    , seedNote = Nothing
    , seedCreatedAt = epoch
    }

-- | The initiation request the HTTP layer would have produced for a flow, with
-- the RRN left to the simulator.
requestFor :: Flow -> InitiateRequest
requestFor flow =
  InitiateRequest
    { initFlow = flow
    , initPayer = alice
    , initPayee = payeeFor flow
    , initMoney = oneRupee
    , initNote = Nothing
    , initRrn = Nothing
    }

payeeFor :: Flow -> Party
payeeFor flow
  | flowIsMerchant flow = merchant
  | otherwise = bob
