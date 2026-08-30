-- |
-- Module      : UPIMock.API.WireSpec
-- Description : The boundary: what a request may say, and what a response promises.
--
-- Everything in "UPIMock.API.Wire" is a total function from an untrusted value to
-- either a domain value or a named refusal. This spec pins the names. Three claims
-- are load-bearing:
--
--   * /The refusal names the field./ A client told @{"error":"INVALID_VPA"}@ can
--     fix its request; one told @400@ cannot. Every counterexample below asserts
--     the whole 'ValidationError', not merely that parsing failed.
--   * /Cross-field rules are not enforced here./ 'parseInitiate' accepts a body
--     whose payer is also its payee, deliberately: that invariant belongs to
--     'UPIMock.Engine.StateMachine.checkSeed', so that a Phase-2 scenario driver
--     which never speaks HTTP meets the same check. The test for it asserts an
--     /acceptance/, which reads like a hole until you know where the wall is.
--   * /An operand is never invented./ 'parseCommand' refuses @AUTHORIZE@ without
--     an @authRef@ rather than minting one, because a fabricated authorisation
--     reference is a forged proof in a log whose whole purpose is to be unforgeable.
--
-- Field spellings are asserted against hand-written JSON rather than against
-- 'UPIMock.API.Wire.wireOptions' applied to itself, for the reason
-- "UPIMock.Domain.EventsSpec" restates the event vocabulary: an expectation
-- derived from the code under test survives the rename it exists to catch.
module UPIMock.API.WireSpec (spec) where

import Data.Aeson (Value (String), decode, encode, object, toJSON, (.=))
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Hedgehog (forAll, (===))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import UPIMock.API.Wire
  ( CodeResource (..)
  , CommandAction (..)
  , CommandBody (..)
  , ErrorResource (..)
  , EventResource (..)
  , InitiateBody (..)
  , PartyResource (..)
  , TxnResource (..)
  , parseCommand
  , parseInitiate
  , parseTxnId
  , parseViewQuery
  , toEventResource
  , toTxnResource
  )

import UPIMock.Application.Ports (ViewQuery (..), defaultViewQuery)
import UPIMock.Application.Service (InitiateRequest (..))
import UPIMock.Domain.ErrorCodes (ErrorCode (OtherCode, UB, Z9), ReconCode (..))
import UPIMock.Domain.Events
  ( AggregateType (AggTransaction)
  , DomainEvent (TxnFailed)
  , StoredEvent (..)
  , currentPayloadVersion
  )
import UPIMock.Domain.Transaction
  ( Transaction
  , Transition (..)
  , TxnSeed (seedTxnId)
  , TxnState (..)
  , applyTransition
  , newTransaction
  , project
  , step
  )
import UPIMock.Domain.Types
  ( AuthRef (..)
  , EventId (..)
  , Flow (..)
  , MerchantId (..)
  , Money (..)
  , Party (..)
  , RefundRef (..)
  , StreamVersion (..)
  , TxnId (..)
  , ValidationError (..)
  , amountPaise
  , noteText
  , rrnText
  , streamIdOfTxn
  , vpaText
  )
import UPIMock.Engine.StateMachine (Command (..))
import UPIMock.Support.Gen (genTxnId)
import UPIMock.Support.Sim (epoch, merchant, seedFor)

spec :: Spec
spec = do
  describe "parseInitiate" $ do
    it "turns a documented body into value objects" $
      parsed (parseInitiate qrBody) $ \request -> do
        initFlow request `shouldBe` FlowP2MQr
        vpaText (partyVpa (initPayer request)) `shouldBe` "alice@psp"
        partyMerchantId (initPayee request) `shouldBe` Just (MerchantId "MER0000001")
        amountPaise (moneyAmount (initMoney request)) `shouldBe` 100
        fmap noteText (initNote request) `shouldBe` Just "chai"
        fmap rrnText (initRrn request) `shouldBe` Just "000000000042"

    it "normalises the address on the way in" $
      -- The only transformation the boundary performs. Were it to stop, ALICE@PSP
      -- and alice@psp would become two payers sharing one RRN index.
      parsed (parseInitiate (qrBody {ibPayerVpa = "ALICE@PSP"})) $ \request ->
        vpaText (partyVpa (initPayer request)) `shouldBe` "alice@psp"

    it "names the field it refused, one refusal at a time" $ do
      parseInitiate (qrBody {ibFlow = "P2P"}) `shouldBe` Left (UnknownEnum "flow" "P2P")
      parseInitiate (qrBody {ibPayerVpa = "alice"}) `shouldBe` Left (InvalidVpa "alice")
      parseInitiate (qrBody {ibAmountPaise = 0}) `shouldBe` Left (AmountOutOfRange 0)
      parseInitiate (qrBody {ibNote = Just longNote}) `shouldBe` Left (NoteTooLong 51)
      parseInitiate (qrBody {ibRrn = Just "42"}) `shouldBe` Left (InvalidRrn "42")

    it "leaves the cross-field rules to the aggregate" $ do
      -- Both of these are refused on the way into the log, by 'checkSeed'.
      -- Asserting the acceptance here is what documents where the line between
      -- \"malformed\" and \"invalid\" was drawn, and why.
      parseInitiate (qrBody {ibPayeeVpa = "alice@psp"}) `shouldSatisfy` isRight
      parseInitiate (p2pBody {ibPayeeMerchantId = Just "MER0000009"}) `shouldSatisfy` isRight

  describe "InitiateBody" $ do
    it "spells its fields the way this spec names them independently" $
      toJSON qrBody `shouldBe` qrJson

    it "reads back what it writes" $
      decode (encode qrBody) `shouldBe` Just qrBody

    it "omits an absent optional field rather than sending null" $
      -- @omitNothingFields@ is load-bearing for the OpenAPI document: a field that
      -- could arrive as null would have to be documented as nullable everywhere it
      -- appears, and clients would have to branch on the difference.
      toJSON p2pBody
        `shouldBe` object
          [ "flow" .= ("P2P_COLLECT" :: Text)
          , "payerVpa" .= ("alice@psp" :: Text)
          , "payeeVpa" .= ("bob@psp" :: Text)
          , "amountPaise" .= (100 :: Int)
          ]

  describe "CommandAction" $ do
    it "spells every action in screaming snake case, with no Act prefix" $
      map toJSON [minBound .. maxBound :: CommandAction] `shouldBe` map String actionTags

    it "reads back every one of those spellings" $
      map (decode . encode) [minBound .. maxBound :: CommandAction]
        `shouldBe` map Just [minBound .. maxBound]

  describe "parseCommand" $ do
    it "maps every action to its command when the operands are present" $
      map (parseCommand . bodyFor) [minBound .. maxBound] `shouldBe` map Right commands

    it "will not invent an operand that identifies something" $ do
      parseCommand (bare ActAuthorize) `shouldBe` Left (MissingField "authRef")
      parseCommand (bare ActOpenRefund) `shouldBe` Left (MissingField "refundRef")

    it "will not invent a reason for a failure" $ do
      parseCommand (bare ActDecline) `shouldBe` Left (MissingField "code")
      parseCommand (bare ActFail) `shouldBe` Left (MissingField "code")
      parseCommand (bare ActDropInReconciliation) `shouldBe` Left (MissingField "code")
      parseCommand (bare ActReconcile) `shouldBe` Left (MissingField "reconCode")

    it "asks for nothing from the actions that carry no information" $
      map (parseCommand . bare) [ActSucceed, ActTimeout, ActSettleRefund]
        `shouldBe` map Right [CmdMarkSuccess, CmdMarkTimeout, CmdSettleRefund]

    it "rejects a code that is not one, naming the field it came from" $ do
      parseCommand ((bare ActFail) {cbCode = Just "nope"})
        `shouldBe` Left (UnknownEnum "code" "nope")
      parseCommand ((bare ActReconcile) {cbReconCode = Just "104"})
        `shouldBe` Left (UnknownEnum "reconCode" "104")

    it "accepts a well-formed code it has no name for" $
      -- 'OtherCode' is why a scenario can drive a code NPCI publishes next quarter
      -- without waiting for a release of this library.
      parseCommand ((bare ActFail) {cbCode = Just "X7"})
        `shouldBe` Right (CmdMarkFailed (OtherCode "X7"))

  describe "parseTxnId" $ do
    it "accepts the spelling the API itself printed" $ hedgehog $ do
      -- 'toTxnResource' renders the id with 'UUID.toText'. A capture that could not
      -- read that back would make every identifier in a response undereferenceable.
      txnId <- forAll genTxnId
      parseTxnId (UUID.toText (unTxnId txnId)) === Right txnId

    it "reports a malformed capture as malformed" $
      parseTxnId "not-a-uuid" `shouldBe` Left (MalformedUuid "not-a-uuid")

  describe "parseViewQuery" $ do
    it "defaults to the documented first page" $
      parseViewQuery Nothing Nothing Nothing Nothing `shouldBe` Right defaultViewQuery

    it "reads the documented filter spellings" $
      parseViewQuery (Just "SUCCESS") (Just "P2P_COLLECT") Nothing Nothing
        `shouldBe` Right
          defaultViewQuery
            { queryState = Just Success
            , queryFlow = Just FlowP2PCollect
            }

    it "clamps paging rather than refusing it" $ do
      -- There is no reading of @limit=0@ a client could have intended, and a 400
      -- would be a less useful answer than the first page.
      fmap queryLimit (parseViewQuery Nothing Nothing (Just 0) Nothing) `shouldBe` Right 1
      fmap queryLimit (parseViewQuery Nothing Nothing (Just 5000) Nothing) `shouldBe` Right 200
      fmap queryOffset (parseViewQuery Nothing Nothing Nothing (Just (-3))) `shouldBe` Right 0

    it "refuses a filter it does not recognise" $ do
      -- A typo'd filter that silently matched everything is how a suite comes to
      -- assert nothing at all while still passing.
      parseViewQuery (Just "DONE") Nothing Nothing Nothing
        `shouldBe` Left (UnknownEnum "state" "DONE")
      parseViewQuery Nothing (Just "p2p_collect") Nothing Nothing
        `shouldBe` Left (UnknownEnum "flow" "p2p_collect")

  describe "toTxnResource" $ do
    it "serves the amount as an integer and as prose" $ do
      -- Both, deliberately: the integer is what a client compares and stores, the
      -- string is what a human reads in the output of a failing test.
      trAmountPaise settled `shouldBe` 100
      trAmount settled `shouldBe` "1.00"
      trCurrency settled `shouldBe` "INR"

    it "renders the state, the flow and the version the projection was built from" $ do
      trState settled `shouldBe` "SUCCESS"
      trFlow settled `shouldBe` "P2M_QR"
      trVersion settled `shouldBe` 3
      trSettledAt settled `shouldBe` Just (at 2)

    it "passes the payee's merchant id through unchanged" $ do
      prVpa (trPayee settled) `shouldBe` vpaText (partyVpa merchant)
      prMerchantId (trPayee settled) `shouldBe` fmap unMerchantId (partyMerchantId merchant)

    it "leaves the failure fields empty on a settled transaction" $ do
      trErrorCode settled `shouldBe` Nothing
      trReconCode settled `shouldBe` Nothing
      trRefundRef settled `shouldBe` Nothing

    it "attaches the meaning of a decline to the code" $ do
      -- The value of a simulator is that its responses explain themselves:
      -- @{"code":"UB"}@ sends the reader to a specification PDF instead.
      fmap crCode (trErrorCode declined) `shouldBe` Just "UB"
      fmap crCategory (trErrorCode declined) `shouldBe` Just "TECHNICAL"
      fmap (T.null . crDescription) (trErrorCode declined) `shouldBe` Just False
      trSettledAt declined `shouldBe` Nothing

  describe "toEventResource" $
    it "serves the payload in its log encoding, verbatim" $ do
      -- The audit endpoint exists to show what was actually written; re-encoding
      -- the payload into a friendlier shape would defeat its only purpose.
      let resource = toEventResource storedFailure
      evType resource `shouldBe` "TXN_FAILED"
      evPayload resource `shouldBe` toJSON (TxnFailed UB)
      evEventId resource `shouldBe` 7
      evVersion resource `shouldBe` 3
      evOccurredAt resource `shouldBe` at 2
      evRecordedAt resource `shouldBe` epoch

  describe "ErrorResource" $ do
    it "keeps the machine-readable class apart from the prose" $
      toJSON (ErrorResource "ILLEGAL_TRANSITION" "MARK_SUCCESS is illegal in INITIATED" Nothing)
        `shouldBe` object
          [ "error" .= ("ILLEGAL_TRANSITION" :: Text)
          , "message" .= ("MARK_SUCCESS is illegal in INITIATED" :: Text)
          ]

    it "carries the switch code only when the failure is one" $
      -- The difference between \"the simulator rejected your request\" and \"the
      -- simulated switch declined the payment\". A client that cannot tell those
      -- apart cannot tell a bug in itself from the behaviour it is testing.
      toJSON (ErrorResource "TXN_FAILED" "Insufficient funds in the payer account" (Just "Z9"))
        `shouldBe` object
          [ "error" .= ("TXN_FAILED" :: Text)
          , "message" .= ("Insufficient funds in the payer account" :: Text)
          , "npciCode" .= ("Z9" :: Text)
          ]

-- | Run an assertion against a parse that must succeed, reporting the refusal
-- rather than failing on an opaque pattern match.
parsed :: Show e => Either e a -> (a -> Expectation) -> Expectation
parsed result assertion = either (expectationFailure . show) assertion result

-- | @n@ seconds after 'epoch'. Distinct per step, so a timestamp assertion can
-- only pass if the value came from where it claims to have come from.
at :: Int64 -> UTCTime
at n = addUTCTime (fromIntegral n) epoch

-- | Every field populated. @P2M_QR@ is the flow that requires a merchant id, so
-- this is the body that exercises the widest path through 'parseInitiate'.
qrBody :: InitiateBody
qrBody =
  InitiateBody
    { ibFlow = "P2M_QR"
    , ibPayerVpa = "alice@psp"
    , ibPayerName = Just "Alice"
    , ibPayeeVpa = "chai.stall@psp"
    , ibPayeeName = Just "Chai Stall"
    , ibPayeeMerchantId = Just "MER0000001"
    , ibAmountPaise = 100
    , ibNote = Just "chai"
    , ibRrn = Just "000000000042"
    }

-- | The required fields and nothing else, so that what the encoder omits is
-- visible.
p2pBody :: InitiateBody
p2pBody =
  InitiateBody
    { ibFlow = "P2P_COLLECT"
    , ibPayerVpa = "alice@psp"
    , ibPayerName = Nothing
    , ibPayeeVpa = "bob@psp"
    , ibPayeeName = Nothing
    , ibPayeeMerchantId = Nothing
    , ibAmountPaise = 100
    , ibNote = Nothing
    , ibRrn = Nothing
    }

-- | 'qrBody' on the wire, written out by hand.
qrJson :: Value
qrJson =
  object
    [ "flow" .= ("P2M_QR" :: Text)
    , "payerVpa" .= ("alice@psp" :: Text)
    , "payerName" .= ("Alice" :: Text)
    , "payeeVpa" .= ("chai.stall@psp" :: Text)
    , "payeeName" .= ("Chai Stall" :: Text)
    , "payeeMerchantId" .= ("MER0000001" :: Text)
    , "amountPaise" .= (100 :: Int)
    , "note" .= ("chai" :: Text)
    , "rrn" .= ("000000000042" :: Text)
    ]

-- | Fifty-one characters: one past what 'UPIMock.Domain.Types.mkNote' accepts.
longNote :: Text
longNote = T.replicate 51 "x"

-- | The wire vocabulary of 'CommandAction', restated. The @Act@ prefix is an
-- internal disambiguator and must never appear here.
actionTags :: [Text]
actionTags =
  [ "AUTHORIZE"
  , "DECLINE"
  , "SUCCEED"
  , "FAIL"
  , "TIMEOUT"
  , "RECONCILE"
  , "DROP_IN_RECONCILIATION"
  , "OPEN_REFUND"
  , "SETTLE_REFUND"
  ]

-- | A body with /every/ operand supplied, so that the mapping test asserts what
-- each action reads rather than what the fixture happened to carry.
bodyFor :: CommandAction -> CommandBody
bodyFor action =
  CommandBody
    { cbAction = action
    , cbCode = Just "Z9"
    , cbReconCode = Just "102"
    , cbAuthRef = Just "auth-1"
    , cbRefundRef = Just "rfnd-1"
    }

-- | The same body with every operand withheld.
bare :: CommandAction -> CommandBody
bare action =
  (bodyFor action)
    { cbCode = Nothing
    , cbReconCode = Nothing
    , cbAuthRef = Nothing
    , cbRefundRef = Nothing
    }

-- | What 'bodyFor' parses to, in constructor order. Hand-maintained, and held to
-- @[minBound .. maxBound]@ by the example that uses it: an action added without an
-- entry here fails on a length mismatch rather than going untested.
commands :: [Command]
commands =
  [ CmdAuthorize (AuthRef "auth-1")
  , CmdDeclineAtPsp Z9
  , CmdMarkSuccess
  , CmdMarkFailed Z9
  , CmdMarkTimeout
  , CmdReconcile ReconCreditedOnline
  , CmdDropInReconciliation Z9
  , CmdOpenRefund (RefundRef "rfnd-1")
  , CmdSettleRefund
  ]

-- The projections the resource mapping runs against. Built by walking the legal
-- path with 'applyTransition', because a 'TxnView' assembled field by field could
-- claim a combination — settled and failed, say — that the engine cannot produce,
-- and the mapping is only worth asserting over views it can.
pendingQr :: Transaction 'Pending
pendingQr =
  applyTransition (at 1) (Authorize (AuthRef "auth-1")) (newTransaction (seedFor FlowP2MQr 7))

settled :: TxnResource
settled = toTxnResource (project (StreamVersion 3) (step (at 2) MarkSuccess pendingQr))

declined :: TxnResource
declined = toTxnResource (project (StreamVersion 3) (step (at 2) (MarkFailed UB) pendingQr))

-- | A log row as the store would have written it. @recordedAt@ is deliberately
-- /earlier/ than @occurredAt@ — a physical impossibility — so that a resource
-- which confused the two clocks fails here instead of passing by coincidence.
storedFailure :: StoredEvent
storedFailure =
  StoredEvent
    { storedEventId = EventId 7
    , storedStreamId = streamIdOfTxn (seedTxnId (seedFor FlowP2MQr 7))
    , storedAggregateType = AggTransaction
    , storedVersion = StreamVersion 3
    , storedEvent = TxnFailed UB
    , storedOccurredAt = at 2
    , storedRecordedAt = epoch
    , storedPayloadVersion = currentPayloadVersion
    }
