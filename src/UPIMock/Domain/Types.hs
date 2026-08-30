-- |
-- Module      : UPIMock.Domain.Types
-- Description : Value objects of the Transaction Orchestration bounded context.
--
-- Every type here is either total by construction or reachable only through a
-- smart constructor returning 'ValidationError'. No function in the domain or
-- engine layers accepts a raw 'Text' where a value object exists; parsing
-- happens once, at the HTTP boundary (see "UPIMock.API.Wire").
--
-- The JSON instances in this module are the /event log/ encoding, not the public
-- API encoding. They are covered by @event_version@ (see
-- "UPIMock.Infrastructure.Schema") and must be changed only through an upcaster.
module UPIMock.Domain.Types
  ( -- * Identity
    TxnId (..)
  , StreamId (..)
  , streamIdOfTxn
  , txnIdOfStream
  , StreamVersion (..)
  , noStreamVersion
  , nextVersion
  , EventId (..)

    -- * Retrieval Reference Number
  , Rrn
  , rrnText
  , mkRrn
  , rrnOfWord

    -- * Addressing
  , Vpa
  , vpaText
  , vpaHandle
  , mkVpa
  , MerchantId (..)
  , Party (..)

    -- * Money
  , Currency (..)
  , Amount
  , amountPaise
  , mkAmount
  , upiPerTxnCapPaise
  , Money (..)
  , renderAmount

    -- * Remaining value objects
  , Note
  , noteText
  , mkNote
  , AuthRef (..)
  , RefundRef (..)
  , Flow (..)
  , renderFlow
  , parseFlow
  , flowIsMerchant

    -- * Validation
  , ValidationError (..)
  , renderValidationError
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | Globally unique transaction identifier. Also the aggregate identity.
newtype TxnId = TxnId {unTxnId :: UUID}
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Event-store stream key. Aggregate-type-prefixed so that the Mandate and
-- Dispute aggregates can share one @events@ table without key collisions.
newtype StreamId = StreamId {unStreamId :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

streamIdOfTxn :: TxnId -> StreamId
streamIdOfTxn (TxnId u) = StreamId ("txn-" <> UUID.toText u)

-- | Inverse of 'streamIdOfTxn'. Total: an unparseable stream key is a corrupt
-- log, which callers must surface rather than silently skip.
txnIdOfStream :: StreamId -> Maybe TxnId
txnIdOfStream (StreamId s) = TxnId <$> (UUID.fromText =<< T.stripPrefix "txn-" s)

-- | Position of an event inside its stream, 1-based. @0@ denotes \"no stream\"
-- and is the expected version of a creation command.
newtype StreamVersion = StreamVersion {unStreamVersion :: Int64}
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

noStreamVersion :: StreamVersion
noStreamVersion = StreamVersion 0

nextVersion :: StreamVersion -> StreamVersion
nextVersion (StreamVersion v) = StreamVersion (v + 1)

-- | Global, monotonic event-log offset. Assigned by the store, never by the
-- domain. Phase 2 uses it as the outbox cursor.
newtype EventId = EventId {unEventId :: Int64}
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Retrieval Reference Number: exactly twelve digits, unique across the
-- simulator instance. Duplicate RRNs are rejected with NPCI @DF@.
newtype Rrn = Rrn Text
  deriving stock (Eq, Ord, Show)

rrnText :: Rrn -> Text
rrnText (Rrn t) = t

mkRrn :: Text -> Either ValidationError Rrn
mkRrn raw
  | T.length trimmed == 12, T.all isDigit trimmed = Right (Rrn trimmed)
  | otherwise = Left (InvalidRrn raw)
  where
    trimmed = T.strip raw

-- | Mint an RRN from an entropy draw. Total by construction: the low twelve
-- decimal digits of any 'Word64', zero-padded, always satisfy the invariant, so
-- this needs no error channel and the engine never has to handle an
-- \"impossible\" 'Left'. 'mkRrn' remains the only way in from untrusted input.
--
-- Collision handling is not this function's job: the store's unique RRN index
-- is, and a collision surfaces as NPCI @DF@.
rrnOfWord :: Word64 -> Rrn
rrnOfWord w = Rrn (T.justifyRight 12 '0' (T.pack (show (w `mod` 1_000_000_000_000))))

instance ToJSON Rrn where
  toJSON = toJSON . rrnText

instance FromJSON Rrn where
  parseJSON = withText "Rrn" (either (fail . T.unpack . renderValidationError) pure . mkRrn)

-- | Virtual Payment Address, normalised to lower case because UPI treats the
-- address as case-insensitive. Normalising in the constructor is what makes
-- @alice\@psp@ and @Alice\@PSP@ collide in the RRN and idempotency indices.
newtype Vpa = Vpa Text
  deriving stock (Eq, Ord, Show)

vpaText :: Vpa -> Text
vpaText (Vpa t) = t

-- | The PSP handle, i.e. everything after @\@@.
vpaHandle :: Vpa -> Text
vpaHandle = T.drop 1 . T.dropWhile (/= '@') . vpaText

mkVpa :: Text -> Either ValidationError Vpa
mkVpa raw = case T.splitOn "@" (T.strip raw) of
  [local, handle]
    | okLocal local
    , okHandle handle ->
        Right (Vpa (T.toLower (local <> "@" <> handle)))
  _ -> Left (InvalidVpa raw)
  where
    okLocal t = let n = T.length t in n >= 2 && n <= 50 && T.all localChar t
    okHandle t = let n = T.length t in n >= 2 && n <= 20 && T.all alnum t
    localChar c = alnum c || c == '.' || c == '-' || c == '_'
    alnum c = isAsciiLower c || isAsciiUpper c || isDigit c

instance ToJSON Vpa where
  toJSON = toJSON . vpaText

instance FromJSON Vpa where
  parseJSON = withText "Vpa" (either (fail . T.unpack . renderValidationError) pure . mkVpa)

-- | Acquirer-assigned merchant identity. Present on the payee of a P2M flow.
newtype MerchantId = MerchantId {unMerchantId :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | An entity inside the 'UPIMock.Domain.Transaction.Transaction' aggregate.
-- Parties have no lifecycle of their own in Phase 1: the account and CBS models
-- arrive with the Dispute & Reconciliation context.
data Party = Party
  { partyVpa :: Vpa
  , partyName :: Maybe Text
  , partyMerchantId :: Maybe MerchantId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Phase 1 is single-currency by construction rather than by validation.
data Currency = INR
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Amount in paise. Integral, because binary floating point has no place in a
-- ledger and 'Double' cannot represent @0.01@.
newtype Amount = Amount Word64
  deriving stock (Eq, Ord, Show)

amountPaise :: Amount -> Word64
amountPaise (Amount p) = p

-- | NPCI per-transaction ceiling for standard P2P\/P2M rails: INR 1,00,000.
upiPerTxnCapPaise :: Word64
upiPerTxnCapPaise = 10_000_000

mkAmount :: Word64 -> Either ValidationError Amount
mkAmount p
  | p == 0 = Left (AmountOutOfRange p)
  | p > upiPerTxnCapPaise = Left (AmountOutOfRange p)
  | otherwise = Right (Amount p)

-- | Fixed-point rendering, e.g. @1234@ paise becomes @"12.34"@.
renderAmount :: Amount -> Text
renderAmount (Amount p) =
  T.pack (show (p `div` 100)) <> "." <> T.justifyRight 2 '0' (T.pack (show (p `mod` 100)))

instance ToJSON Amount where
  toJSON = toJSON . amountPaise

instance FromJSON Amount where
  parseJSON v = do
    p <- parseJSON v
    either (fail . T.unpack . renderValidationError) pure (mkAmount p)

data Money = Money
  { moneyAmount :: Amount
  , moneyCurrency :: Currency
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Free-text remark. Capped at the 50 characters the NPCI payload allows.
newtype Note = Note Text
  deriving stock (Eq, Show)

noteText :: Note -> Text
noteText (Note t) = t

mkNote :: Text -> Either ValidationError Note
mkNote raw
  | T.length trimmed > 50 = Left (NoteTooLong (T.length trimmed))
  | otherwise = Right (Note trimmed)
  where
    trimmed = T.strip raw

instance ToJSON Note where
  toJSON = toJSON . noteText

instance FromJSON Note where
  parseJSON = withText "Note" (either (fail . T.unpack . renderValidationError) pure . mkNote)

-- | Opaque proof that the payer authorised the debit (MPIN entry, or in-app
-- authorisation for an Intent flow). The simulator never models MPIN material.
newtype AuthRef = AuthRef {unAuthRef :: Text}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Identity of the refund leg attached to a settled transaction.
newtype RefundRef = RefundRef {unRefundRef :: Text}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | The three flows Phase 1 implements. The flow is immutable for the lifetime
-- of the aggregate and decides whether authorisation is implicit ('FlowP2PIntent',
-- 'FlowP2MQr') or an explicit payer decision ('FlowP2PCollect').
data Flow
  = FlowP2PIntent
  | FlowP2PCollect
  | FlowP2MQr
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

renderFlow :: Flow -> Text
renderFlow = \case
  FlowP2PIntent -> "P2P_INTENT"
  FlowP2PCollect -> "P2P_COLLECT"
  FlowP2MQr -> "P2M_QR"

parseFlow :: Text -> Maybe Flow
parseFlow t = lookup t [(renderFlow f, f) | f <- [minBound .. maxBound]]

-- | Whether the flow requires a merchant identity on the payee.
flowIsMerchant :: Flow -> Bool
flowIsMerchant = \case
  FlowP2MQr -> True
  FlowP2PIntent -> False
  FlowP2PCollect -> False

-- | Boundary-parse failures. These are /our/ errors, not NPCI decline codes:
-- see "UPIMock.Domain.ErrorCodes" for the simulated switch outcomes.
data ValidationError
  = InvalidVpa Text
  | InvalidRrn Text
  | AmountOutOfRange Word64
  | NoteTooLong Int
  | MalformedUuid Text
  | UnknownEnum Text Text
  | -- | A field the /requested operation/ requires, absent. Not the same as a
    -- field missing from the JSON object, which aeson rejects before this type is
    -- reached: this is @{"action":"DECLINE"}@ with no decline code, where the
    -- object is well formed and the request still cannot be carried out.
    MissingField Text
  | MerchantIdRequired
  | MerchantIdForbidden
  | PayerIsPayee Text
  deriving stock (Eq, Show)

renderValidationError :: ValidationError -> Text
renderValidationError = \case
  InvalidVpa v -> "malformed VPA: " <> v
  InvalidRrn r -> "RRN must be exactly 12 digits: " <> r
  AmountOutOfRange p ->
    "amount (paise) must be in 1.."
      <> T.pack (show upiPerTxnCapPaise)
      <> ", got "
      <> T.pack (show p)
  NoteTooLong n -> "note exceeds 50 characters: " <> T.pack (show n)
  MalformedUuid t -> "malformed UUID: " <> t
  UnknownEnum field t -> "unknown " <> field <> ": " <> t
  MissingField field -> "missing required field: " <> field
  MerchantIdRequired -> "P2M_QR requires payeeMerchantId"
  MerchantIdForbidden -> "payeeMerchantId is only valid for P2M_QR"
  PayerIsPayee v -> "payer and payee resolve to the same VPA: " <> v
