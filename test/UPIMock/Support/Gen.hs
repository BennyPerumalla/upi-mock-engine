-- |
-- Module      : UPIMock.Support.Gen
-- Description : Generators for the domain's value objects and events.
--
-- Every generator here produces values the smart constructors /accept/. Rejection
-- is tested by constructing the counterexample explicitly in the spec that cares,
-- because a generator that mostly produces garbage tests the generator.
--
-- No generator uses a partial function. Where a value can only be reached through
-- an @Either@-returning constructor, 'valid' filters on the @Right@ — and the
-- ranges are chosen so the discard path is unreachable, which keeps the generator
-- from silently shrinking its own coverage.
module UPIMock.Support.Gen
  ( -- * Identity and addressing
    genTxnId
  , genRrn
  , genVpaParts
  , genVpa
  , genMerchantId
  , genPartyPair

    -- * Money, notes, references
  , genAmount
  , genMoney
  , genNote
  , genAuthRef
  , genRefundRef

    -- * Enumerations
  , genFlow
  , genTxnState
  , genErrorCode
  , genReconCode

    -- * Aggregate inputs
  , genTime
  , genSeed
  , genSeedFor

    -- * Events and commands
  , genDomainEvent
  , genCommand
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.UUID qualified as UUID
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import UPIMock.Domain.ErrorCodes
  ( ErrorCode (..)
  , ReconCode
  , errorCodeText
  , knownErrorCodes
  )
import UPIMock.Domain.Events (DomainEvent (..))
import UPIMock.Domain.Transaction (TxnSeed (..), TxnState)
import UPIMock.Domain.Types
  ( Amount
  , AuthRef (..)
  , Currency (INR)
  , Flow
  , MerchantId (..)
  , Money (..)
  , Note
  , Party (..)
  , RefundRef (..)
  , Rrn
  , TxnId (..)
  , Vpa
  , flowIsMerchant
  , mkAmount
  , mkNote
  , mkVpa
  , rrnOfWord
  , upiPerTxnCapPaise
  )
import UPIMock.Engine.StateMachine (Command (..))

-- | Keep only the values a smart constructor accepts.
--
-- The ranges below are inside every constructor's domain, so no draw is ever
-- discarded. 'Gen.just' is here to make that claim total rather than to filter:
-- if a constructor tightens, this generator starves loudly instead of a partial
-- helper crashing the suite.
valid :: (a -> Either e b) -> Gen a -> Gen b
valid construct = Gen.just . fmap (either (const Nothing) Just . construct)

genTxnId :: Gen TxnId
genTxnId = TxnId <$> (UUID.fromWords <$> word <*> word <*> word <*> word)
  where
    word = Gen.word32 Range.constantBounded

-- | Minted rather than parsed: 'rrnOfWord' is total, so this needs no filter and
-- shrinks towards @000000000000@.
genRrn :: Gen Rrn
genRrn = rrnOfWord <$> Gen.word64 Range.constantBounded

-- | Local part and handle, separately, so a spec can reassemble them with the
-- case or the separator it wants to test. Lengths sit inside the 2..50 and 2..20
-- windows 'mkVpa' enforces.
genVpaParts :: Gen (Text, Text)
genVpaParts = (,) <$> genLocal <*> genHandle
  where
    genLocal = T.pack <$> Gen.list (Range.linear 2 20) genLocalChar
    genHandle = T.pack <$> Gen.list (Range.linear 2 10) Gen.alphaNum
    genLocalChar = Gen.frequency [(9, Gen.alphaNum), (1, Gen.element ['.', '-', '_'])]

genVpa :: Gen Vpa
genVpa = valid mkVpa (joinVpa <$> genVpaParts)

joinVpa :: (Text, Text) -> Text
joinVpa (local, handle) = local <> "@" <> handle

genMerchantId :: Gen MerchantId
genMerchantId =
  MerchantId . ("MER" <>) . T.justifyRight 7 '0' . T.pack . show
    <$> Gen.int (Range.linear 1 9_999_999)

-- | Payer and payee for a flow, satisfying every cross-field rule
-- 'UPIMock.Engine.StateMachine.checkSeed' checks: the addresses differ, and the
-- payee carries a merchant id exactly when the flow is a merchant flow.
--
-- The addresses differ by construction — distinct first characters — rather than
-- by rejection sampling, so the generator cannot stall on a coincidence.
genPartyPair :: Bool -> Gen (Party, Party)
genPartyPair merchantFlow = do
  payerVpa <- valid mkVpa (joinVpa . prefix "a" <$> genVpaParts)
  payeeVpa <- valid mkVpa (joinVpa . prefix "b" <$> genVpaParts)
  merchantId <- if merchantFlow then Just <$> genMerchantId else pure Nothing
  pure
    ( Party {partyVpa = payerVpa, partyName = Just "Payer", partyMerchantId = Nothing}
    , Party {partyVpa = payeeVpa, partyName = Just "Payee", partyMerchantId = merchantId}
    )
  where
    prefix c (local, handle) = (c <> local, handle)

genAmount :: Gen Amount
genAmount = valid mkAmount (Gen.word64 (Range.linear 1 upiPerTxnCapPaise))

genMoney :: Gen Money
genMoney = (\amount -> Money {moneyAmount = amount, moneyCurrency = INR}) <$> genAmount

-- | Alphanumeric so that the trimming 'mkNote' performs is not what is under
-- test; the trimming property builds its own padded input.
genNote :: Gen Note
genNote = valid mkNote (Gen.text (Range.linear 0 50) Gen.alphaNum)

genAuthRef :: Gen AuthRef
genAuthRef = AuthRef . ("auth-" <>) <$> genToken

genRefundRef :: Gen RefundRef
genRefundRef = RefundRef . ("rfnd-" <>) <$> genToken

genToken :: Gen Text
genToken = T.pack <$> Gen.list (Range.linear 4 12) Gen.alphaNum

genFlow :: Gen Flow
genFlow = Gen.element [minBound .. maxBound]

genTxnState :: Gen TxnState
genTxnState = Gen.element [minBound .. maxBound]

-- | The named codes, plus 'OtherCode' tokens that are /not/ names — otherwise
-- @OtherCode \"UB\"@ would be generated and @parseErrorCode . errorCodeText@
-- would answer @UB@, failing a round trip that is in fact correct.
genErrorCode :: Gen ErrorCode
genErrorCode = Gen.choice [Gen.element knownErrorCodes, OtherCode <$> genUnnamedToken]
  where
    genUnnamedToken = Gen.filter (`notElem` map errorCodeText knownErrorCodes) genWireToken
    genWireToken = T.pack <$> Gen.list (Range.linear 2 3) (Gen.choice [Gen.upper, Gen.digit])

genReconCode :: Gen ReconCode
genReconCode = Gen.element [minBound .. maxBound]

-- | A second inside one fixed day. Bounded so that a shrunk counterexample is
-- still a plausible timestamp, and distinct from
-- 'UPIMock.Support.Sim.epoch' only in the time of day.
genTime :: Gen UTCTime
genTime =
  UTCTime (fromGregorian 2026 1 1) . secondsToDiffTime
    <$> Gen.integral (Range.linear 0 86_399)

-- | A seed that passes 'UPIMock.Engine.StateMachine.checkSeed'.
genSeed :: Gen TxnSeed
genSeed = genFlow >>= genSeedFor

genSeedFor :: Flow -> Gen TxnSeed
genSeedFor flow = do
  (payer, payee) <- genPartyPair (flowIsMerchant flow)
  txnId <- genTxnId
  rrn <- genRrn
  money <- genMoney
  note <- Gen.maybe genNote
  createdAt <- genTime
  pure
    TxnSeed
      { seedTxnId = txnId
      , seedRrn = rrn
      , seedFlow = flow
      , seedPayer = payer
      , seedPayee = payee
      , seedMoney = money
      , seedNote = note
      , seedCreatedAt = createdAt
      }

-- | One draw per constructor, uniformly. Uniformity matters for the tag-drift
-- property: a constructor that is generated rarely is a constructor whose
-- @event_type@ column is rarely checked.
genDomainEvent :: Gen DomainEvent
genDomainEvent =
  Gen.choice
    [ TxnInitiated <$> genSeed
    , TxnAuthorized <$> genAuthRef
    , TxnDeclinedAtPsp <$> genErrorCode
    , pure TxnSucceeded
    , TxnFailed <$> genErrorCode
    , pure TxnTimedOut
    , TxnReconciled <$> genReconCode
    , TxnReconciliationDropped <$> genErrorCode
    , RefundOpened <$> genRefundRef
    , pure RefundSettled
    ]

genCommand :: Gen Command
genCommand =
  Gen.choice
    [ CmdAuthorize <$> genAuthRef
    , CmdDeclineAtPsp <$> genErrorCode
    , pure CmdMarkSuccess
    , CmdMarkFailed <$> genErrorCode
    , pure CmdMarkTimeout
    , CmdReconcile <$> genReconCode
    , CmdDropInReconciliation <$> genErrorCode
    , CmdOpenRefund <$> genRefundRef
    , pure CmdSettleRefund
    ]
