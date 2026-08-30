-- |
-- Module      : UPIMock.API.Wire
-- Description : The public HTTP encoding, and the boundary where text becomes a
--               value object.
--
-- This module exists because the event log's JSON and the API's JSON are two
-- different contracts with two different rates of change. The log's encoding is
-- versioned by @payload_version@ and may only change through an upcaster
-- ("UPIMock.Domain.Types" says so in its header); the API's encoding may change
-- whenever the API version does. Sharing one set of instances between them would
-- couple a wire-format decision to a stored-data decision, and the first time
-- someone renamed a JSON field to be friendlier they would have silently
-- invalidated every event already on disk.
--
-- So the domain types carry no @ToSchema@ instance and no knowledge of HTTP, the
-- resources below carry no invariants, and the functions in between are the only
-- place either representation is converted.
--
-- __Parsing happens here and nowhere else.__ Every request field arrives as
-- 'Text' or a number and leaves as a value object or a 'ValidationError'. Below
-- this line, an unparseable VPA is not representable.
module UPIMock.API.Wire
  ( -- * Request bodies
    InitiateBody (..)
  , parseInitiate
  , CommandAction (..)
  , CommandBody (..)
  , parseCommand

    -- * Path and query parameters
  , parseTxnId
  , parseRrn
  , parseViewQuery

    -- * Response resources
  , TxnResource (..)
  , toTxnResource
  , PartyResource (..)
  , CodeResource (..)
  , EventResource (..)
  , toEventResource
  , TxnPage (..)
  , HealthResource (..)
  , ErrorResource (..)
  ) where

import Data.Aeson
  ( FromJSON (..)
  , Options (..)
  , ToJSON (..)
  , Value
  , camelTo2
  , defaultOptions
  , genericParseJSON
  , genericToJSON
  )
import Data.Char (toLower, toUpper)
import Data.Int (Int64)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Word (Word64)
import GHC.Generics (Generic)

import UPIMock.Application.Ports (ViewQuery (..), defaultViewQuery)
import UPIMock.Application.Service (InitiateRequest (..))
import UPIMock.Domain.ErrorCodes
  ( ErrorCode
  , FailureCategory (..)
  , ReconCode
  , errorCodeCategory
  , errorCodeDescription
  , errorCodeText
  , parseErrorCode
  , parseReconCode
  , reconCodeDescription
  , reconCodeText
  )
import UPIMock.Domain.Events (StoredEvent (..), eventTypeText)
import UPIMock.Domain.Transaction (TxnView (..), parseTxnState, renderTxnState)
import UPIMock.Domain.Types
  ( AuthRef (..)
  , Currency (INR)
  , EventId (..)
  , MerchantId (..)
  , Money (..)
  , Party (..)
  , RefundRef (..)
  , Rrn
  , StreamVersion (..)
  , TxnId (..)
  , ValidationError (..)
  , amountPaise
  , mkAmount
  , mkNote
  , mkRrn
  , mkVpa
  , noteText
  , parseFlow
  , renderAmount
  , renderFlow
  , rrnText
  , vpaText
  )
import UPIMock.Engine.StateMachine (Command (..))

-- | Aeson options for a record whose fields carry a disambiguating prefix.
--
-- Haskell records share one namespace per module, so every resource below prefixes
-- its fields; the prefix is stripped on the wire. @DuplicateRecordFields@ would
-- avoid the prefixes and break every selector used point-free, which is a worse
-- trade.
--
-- The same 'Options' value feeds both the aeson instances and the @ToSchema@
-- instance through 'fromAesonOptions', so the documented schema and the actual
-- encoding cannot drift. That is not a convention here — it is a single value used
-- twice.
wireOptions :: String -> Options
wireOptions prefix =
  defaultOptions
    { fieldLabelModifier = lowerFirst . dropPrefix
    , constructorTagModifier = map toUpper . camelTo2 '_' . dropPrefix
    , omitNothingFields = True
    }
  where
    dropPrefix name = fromMaybe name (stripPrefix prefix name)
    lowerFirst = \case
      c : cs -> toLower c : cs
      [] -> []

-- | Open a transaction.
--
-- No @currency@ field: Phase 1 is single-currency by construction
-- ('UPIMock.Domain.Types.Currency' has one constructor), and an accepted-then-
-- ignored field is worse documentation than an absent one.
--
-- @rrn@ is optional because the /switch/ mints the RRN. Supply one only to
-- provoke the duplicate-RRN path (NPCI @DF@); a harness that wants a specific
-- reference gets to have it.
data InitiateBody = InitiateBody
  { ibFlow :: Text
  -- ^ @P2P_INTENT@, @P2P_COLLECT@ or @P2M_QR@.
  , ibPayerVpa :: Text
  , ibPayerName :: Maybe Text
  , ibPayeeVpa :: Text
  , ibPayeeName :: Maybe Text
  , ibPayeeMerchantId :: Maybe Text
  -- ^ Required for @P2M_QR@, rejected otherwise.
  , ibAmountPaise :: Word64
  -- ^ Paise, so @12345@ is INR 123.45. Never a decimal fraction on the wire.
  , ibNote :: Maybe Text
  , ibRrn :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON InitiateBody where
  parseJSON = genericParseJSON (wireOptions "ib")

instance ToJSON InitiateBody where
  toJSON = genericToJSON (wireOptions "ib")

instance ToSchema InitiateBody where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (wireOptions "ib"))

-- | Every field, parsed. Cross-field rules (a merchant id that belongs or does
-- not belong to the flow, a payer who is also the payee) are deliberately /not/
-- checked here: they are invariants of the aggregate and live in
-- 'UPIMock.Engine.StateMachine.checkSeed', so that a Phase-2 scenario driver that
-- never speaks HTTP cannot bypass them.
parseInitiate :: InitiateBody -> Either ValidationError InitiateRequest
parseInitiate body = do
  flow <- maybe (Left (UnknownEnum "flow" (ibFlow body))) Right (parseFlow (ibFlow body))
  payerVpa <- mkVpa (ibPayerVpa body)
  payeeVpa <- mkVpa (ibPayeeVpa body)
  amount <- mkAmount (ibAmountPaise body)
  note <- traverse mkNote (ibNote body)
  rrn <- traverse mkRrn (ibRrn body)
  pure
    InitiateRequest
      { initFlow = flow
      , initPayer =
          Party
            { partyVpa = payerVpa
            , partyName = ibPayerName body
            , partyMerchantId = Nothing
            }
      , initPayee =
          Party
            { partyVpa = payeeVpa
            , partyName = ibPayeeName body
            , partyMerchantId = MerchantId <$> ibPayeeMerchantId body
            }
      , initMoney = Money {moneyAmount = amount, moneyCurrency = INR}
      , initNote = note
      , initRrn = rrn
      }

-- | The verb of a command request. One flat enumeration rather than a tagged
-- union of nine distinct bodies: the OpenAPI document stays a single readable
-- object, and 'parseCommand' is where the operand rules are stated once.
--
-- Constructors are @Act@-prefixed so that nothing in this module shadows the
-- 'UPIMock.Domain.Transaction.Transition' constructors of the same names. The
-- prefix never reaches the wire.
data CommandAction
  = -- | Payer approves a Collect request. Requires @authRef@.
    ActAuthorize
  | -- | Payer\'s PSP rejects before the debit. Requires @code@.
    ActDecline
  | -- | Switch reports a completed credit.
    ActSucceed
  | -- | Switch reports a failure. Requires @code@.
    ActFail
  | -- | No response inside the switch window.
    ActTimeout
  | -- | Reconciliation resolves a timed-out transaction. Requires @reconCode@.
    ActReconcile
  | -- | Reconciliation resolves it as failed. Requires @code@.
    ActDropInReconciliation
  | -- | Open a refund leg on a settled transaction. Requires @refundRef@.
    ActOpenRefund
  | -- | Settle the open refund leg.
    ActSettleRefund
  deriving stock (Eq, Show, Enum, Bounded, Generic)

instance FromJSON CommandAction where
  parseJSON = genericParseJSON (wireOptions "Act")

instance ToJSON CommandAction where
  toJSON = genericToJSON (wireOptions "Act")

instance ToSchema CommandAction where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (wireOptions "Act"))

-- | A command and its operands. Every operand is optional in the schema and
-- required by 'parseCommand' for exactly the actions that need it.
data CommandBody = CommandBody
  { cbAction :: CommandAction
  , cbCode :: Maybe Text
  -- ^ NPCI response code, e.g. @Z9@, @U30@, @ZM@.
  , cbReconCode :: Maybe Text
  -- ^ Reconciliation outcome code, e.g. @102@.
  , cbAuthRef :: Maybe Text
  , cbRefundRef :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON CommandBody where
  parseJSON = genericParseJSON (wireOptions "cb")

instance ToJSON CommandBody where
  toJSON = genericToJSON (wireOptions "cb")

instance ToSchema CommandBody where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (wireOptions "cb"))

-- | Turn a request into a domain 'Command', or say which operand is missing.
--
-- __The rule, stated once:__ an operand that /identifies/ something
-- (@authRef@, @refundRef@) or that /gives a reason/ (@code@, @reconCode@) is
-- required, and the engine will not invent one. Contrast the RRN on
-- 'InitiateBody', which the switch mints because it is the switch's own
-- identifier. Fabricating an authorisation reference would mean the simulator
-- produced a proof of authorisation that nobody supplied, and the entire point of
-- the audit log is that it cannot do that.
parseCommand :: CommandBody -> Either ValidationError Command
parseCommand body = case cbAction body of
  ActAuthorize -> CmdAuthorize . AuthRef <$> required "authRef" (cbAuthRef body)
  ActDecline -> CmdDeclineAtPsp <$> code
  ActSucceed -> Right CmdMarkSuccess
  ActFail -> CmdMarkFailed <$> code
  ActTimeout -> Right CmdMarkTimeout
  ActReconcile -> CmdReconcile <$> recon
  ActDropInReconciliation -> CmdDropInReconciliation <$> code
  ActOpenRefund -> CmdOpenRefund . RefundRef <$> required "refundRef" (cbRefundRef body)
  ActSettleRefund -> Right CmdSettleRefund
  where
    required field = maybe (Left (MissingField field)) Right
    code = required "code" (cbCode body) >>= parseCode
    recon = required "reconCode" (cbReconCode body) >>= parseRecon
    parseCode raw = maybe (Left (UnknownEnum "code" raw)) Right (parseErrorCode raw)
    parseRecon raw = maybe (Left (UnknownEnum "reconCode" raw)) Right (parseReconCode raw)

-- | A path capture. Servant hands over 'Text' because 'TxnId' has no
-- @FromHttpApiData@ instance and will not be given one: that instance would be a
-- second parsing path into the domain, outside this module.
parseTxnId :: Text -> Either ValidationError TxnId
parseTxnId raw = maybe (Left (MalformedUuid raw)) (Right . TxnId) (UUID.fromText raw)

-- | Re-exported under an API-layer name so a handler needs exactly one import to
-- parse everything a request can carry.
parseRrn :: Text -> Either ValidationError Rrn
parseRrn = mkRrn

-- | Query parameters for the collection endpoint, in the order Servant supplies
-- them: @state@, @flow@, @limit@, @offset@.
--
-- An unknown @state@ or @flow@ is an error rather than an ignored filter — a
-- typo'd filter that silently returns everything is how a test suite comes to
-- assert nothing. Out-of-range paging is clamped instead, because there is no
-- interpretation of @limit=-1@ a client could have intended.
parseViewQuery ::
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Either ValidationError ViewQuery
parseViewQuery rawState rawFlow rawLimit rawOffset = do
  state <- traverse (parseWith "state" parseTxnState) rawState
  flow <- traverse (parseWith "flow" parseFlow) rawFlow
  pure
    ViewQuery
      { queryState = state
      , queryFlow = flow
      , queryLimit = maybe (queryLimit defaultViewQuery) (min 200 . max 1) rawLimit
      , queryOffset = maybe 0 (max 0) rawOffset
      }
  where
    parseWith field parser raw = maybe (Left (UnknownEnum field raw)) Right (parser raw)

data PartyResource = PartyResource
  { prVpa :: Text
  , prName :: Maybe Text
  , prMerchantId :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON PartyResource where
  toJSON = genericToJSON (wireOptions "pr")

instance FromJSON PartyResource where
  parseJSON = genericParseJSON (wireOptions "pr")

instance ToSchema PartyResource where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (wireOptions "pr"))

toPartyResource :: Party -> PartyResource
toPartyResource party =
  PartyResource
    { prVpa = vpaText (partyVpa party)
    , prName = partyName party
    , prMerchantId = unMerchantId <$> partyMerchantId party
    }

-- | A code with its meaning attached.
--
-- The description is served rather than left to the client's lookup table
-- because the whole value of a simulator is that the response explains itself:
-- @{"code":"U30"}@ sends the reader to a specification PDF,
-- @{"code":"U30","description":"Debit has failed at the remitter CBS",
-- "category":"TECHNICAL"}@ does not. Shared by response codes and reconciliation
-- codes, which differ only in which enumeration they came from.
data CodeResource = CodeResource
  { crCode :: Text
  , crDescription :: Text
  , crCategory :: Text
  -- ^ @TECHNICAL@ or @BUSINESS@ for a response code, @RECONCILIATION@ for a
  -- reconciliation outcome. Technical failures are the ones a Phase-2 chaos run
  -- will make diverge from their reconciliation.
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CodeResource where
  toJSON = genericToJSON (wireOptions "cr")

instance FromJSON CodeResource where
  parseJSON = genericParseJSON (wireOptions "cr")

instance ToSchema CodeResource where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (wireOptions "cr"))

toCodeResource :: ErrorCode -> CodeResource
toCodeResource code =
  CodeResource
    { crCode = errorCodeText code
    , crDescription = errorCodeDescription code
    , crCategory = case errorCodeCategory code of
        Technical -> "TECHNICAL"
        Business -> "BUSINESS"
    }

toReconResource :: ReconCode -> CodeResource
toReconResource code =
  CodeResource
    { crCode = reconCodeText code
    , crDescription = reconCodeDescription code
    , crCategory = "RECONCILIATION"
    }

-- | The read model, as the API presents it.
--
-- @amountPaise@ and @amount@ are both present and that is deliberate: the integer
-- is what a client should compare and store, the string is what a human reads in
-- a failing test's output. Serving only the formatted string would invite
-- floating-point parsing on the client side, which is the bug this codebase
-- refuses to have.
--
-- @version@ is the stream version the projection was built from. A client that
-- polls can use it to detect that it is looking at a stale read, which is
-- precisely the anomaly Phase 2 will inject on purpose.
data TxnResource = TxnResource
  { trTxnId :: Text
  , trRrn :: Text
  , trState :: Text
  , trFlow :: Text
  , trPayer :: PartyResource
  , trPayee :: PartyResource
  , trAmountPaise :: Word64
  , trAmount :: Text
  , trCurrency :: Text
  , trNote :: Maybe Text
  , trErrorCode :: Maybe CodeResource
  , trReconCode :: Maybe CodeResource
  , trRefundRef :: Maybe Text
  , trCreatedAt :: UTCTime
  , trUpdatedAt :: UTCTime
  , trSettledAt :: Maybe UTCTime
  , trVersion :: Int64
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON TxnResource where
  toJSON = genericToJSON (wireOptions "tr")

instance FromJSON TxnResource where
  parseJSON = genericParseJSON (wireOptions "tr")

instance ToSchema TxnResource where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (wireOptions "tr"))

toTxnResource :: TxnView -> TxnResource
toTxnResource view =
  TxnResource
    { trTxnId = UUID.toText (unTxnId (viewTxnId view))
    , trRrn = rrnText (viewRrn view)
    , trState = renderTxnState (viewState view)
    , trFlow = renderFlow (viewFlow view)
    , trPayer = toPartyResource (viewPayer view)
    , trPayee = toPartyResource (viewPayee view)
    , trAmountPaise = amountPaise (moneyAmount (viewMoney view))
    , trAmount = renderAmount (moneyAmount (viewMoney view))
    , trCurrency = case moneyCurrency (viewMoney view) of INR -> "INR"
    , trNote = noteText <$> viewNote view
    , trErrorCode = toCodeResource <$> viewErrorCode view
    , trReconCode = toReconResource <$> viewReconCode view
    , trRefundRef = unRefundRef <$> viewRefundRef view
    , trCreatedAt = viewCreatedAt view
    , trUpdatedAt = viewUpdatedAt view
    , trSettledAt = viewSettledAt view
    , trVersion = unStreamVersion (viewVersion view)
    }

-- | One entry of the audit log.
--
-- @payload@ is the domain event in its /log/ encoding, verbatim. This is the one
-- place the two encodings meet, and it is intentional: the audit endpoint's
-- purpose is to show what was actually written, so re-encoding the payload into a
-- friendlier shape would defeat it.
data EventResource = EventResource
  { evEventId :: Int64
  -- ^ Global log offset. Ascending, never reused; a client can page on it.
  , evVersion :: Int64
  -- ^ Position within this transaction's stream, 1-based.
  , evType :: Text
  , evOccurredAt :: UTCTime
  -- ^ When the domain says it happened.
  , evRecordedAt :: UTCTime
  -- ^ When the store wrote it. The gap between the two is what a chaos run
  -- widens.
  , evPayload :: Value
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON EventResource where
  toJSON = genericToJSON (wireOptions "ev")

instance ToSchema EventResource where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (wireOptions "ev"))

toEventResource :: StoredEvent -> EventResource
toEventResource stored =
  EventResource
    { evEventId = unEventId (storedEventId stored)
    , evVersion = unStreamVersion (storedVersion stored)
    , evType = eventTypeText (storedEvent stored)
    , evOccurredAt = storedOccurredAt stored
    , evRecordedAt = storedRecordedAt stored
    , evPayload = toJSON (storedEvent stored)
    }

-- | A page of the collection endpoint. @count@ is the size of /this/ page, not a
-- total: counting the whole projection on every request would be a lie dressed as
-- a number the moment the next commit lands.
data TxnPage = TxnPage
  { tpItems :: [TxnResource]
  , tpLimit :: Int
  , tpOffset :: Int
  , tpCount :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON TxnPage where
  toJSON = genericToJSON (wireOptions "tp")

instance ToSchema TxnPage where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (wireOptions "tp"))

-- | What @GET \/health@ answers. Deliberately more than @{"status":"ok"}@: the
-- three facts below are the ones that make a confusing run make sense — whether
-- the schema is the one this binary expects, whether WAL was actually adopted (it
-- silently is not, on some network filesystems), and how many transactions the
-- boot rebuild found.
data HealthResource = HealthResource
  { hrStatus :: Text
  , hrVersion :: Text
  , hrSchemaVersion :: Int
  , hrJournalMode :: Text
  , hrTransactions :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON HealthResource where
  toJSON = genericToJSON (wireOptions "hr")

instance FromJSON HealthResource where
  parseJSON = genericParseJSON (wireOptions "hr")

instance ToSchema HealthResource where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (wireOptions "hr"))

-- | The single error shape, on every failing status.
--
-- @error@ is a stable, machine-readable class — @VERSION_CONFLICT@,
-- @DUPLICATE_RRN@, @ILLEGAL_TRANSITION@ — and @message@ is prose that may be
-- reworded at any time. Clients branch on the former; humans read the latter.
--
-- @npciCode@ carries the switch code when the failure /is/ one, which is the
-- difference between \"the simulator rejected your request\" and \"the simulated
-- switch declined the payment\". Conflating those two would make the harness
-- unable to tell a bug in itself from the behaviour it is testing.
data ErrorResource = ErrorResource
  { xrError :: Text
  , xrMessage :: Text
  , xrNpciCode :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ErrorResource where
  toJSON = genericToJSON (wireOptions "xr")

instance FromJSON ErrorResource where
  parseJSON = genericParseJSON (wireOptions "xr")

instance ToSchema ErrorResource where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (wireOptions "xr"))
