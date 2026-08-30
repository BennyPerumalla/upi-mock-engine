-- |
-- Module      : UPIMock.Domain.ErrorCodes
-- Description : NPCI decline and reconciliation codes.
--
-- These are the codes the /simulated switch/ returns. They are deliberately kept
-- separate from 'UPIMock.Domain.Types.ValidationError', which describes a
-- malformed request to the simulator itself: a client that cannot tell the two
-- apart will mis-handle real NPCI traffic as well.
--
-- Semantics for @UB@, @K1@, @UP@, @DF@, @15@, @102@ and @103@ are taken from the
-- engineering design document (§4.3, §8.2). The remaining mnemonics are the
-- widely published PSP mappings; treat their descriptions as documentation, not
-- as a normative NPCI reference, and check them against the current NPCI
-- circular before relying on the exact wording in a compliance context.
-- 'OtherCode' exists so that a scenario can drive any code the specification
-- defines without a library release.
module UPIMock.Domain.ErrorCodes
  ( ErrorCode (..)
  , FailureCategory (..)
  , errorCodeText
  , errorCodeCategory
  , errorCodeDescription
  , parseErrorCode
  , knownErrorCodes

    -- * Reconciliation
  , ReconCode (..)
  , reconCodeText
  , reconCodeDescription
  , parseReconCode
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Char (isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as T

-- | Whether a decline was a technical fault (retryable, and a candidate for
-- reconciliation divergence) or a business rejection (terminal).
data FailureCategory
  = Technical
  | Business
  deriving stock (Eq, Show, Enum, Bounded)

-- | NPCI response codes modelled by the engine.
data ErrorCode
  = -- | Internal exception at the beneficiary CBS.
    UB
  | -- | Debit has failed at the remitter CBS.
    U30
  | -- | PSP did not respond inside the switch window.
    UP
  | -- | Risk threshold exceeded at the PSP.
    U16
  | -- | Insufficient funds.
    Z9
  | -- | Transaction declined by the customer.
    ZA
  | -- | Incorrect MPIN.
    ZM
  | -- | Invalid or unresolvable virtual address.
    ZH
  | -- | Beneficiary account does not exist.
    XH
  | -- | Suspected fraud; declined on business grounds.
    K1
  | -- | Duplicate RRN.
    DF
  | -- | Issuer not live on UPI (wire code @15@).
    IssuerNotLive
  | -- | Any code the specification defines that this enumeration does not name.
    -- Constrained to two or three upper-case alphanumerics by 'parseErrorCode'.
    OtherCode Text
  deriving stock (Eq, Show)

-- | The codes with first-class semantics. Excludes 'OtherCode'.
knownErrorCodes :: [ErrorCode]
knownErrorCodes = [UB, U30, UP, U16, Z9, ZA, ZM, ZH, XH, K1, DF, IssuerNotLive]

errorCodeText :: ErrorCode -> Text
errorCodeText = \case
  UB -> "UB"
  U30 -> "U30"
  UP -> "UP"
  U16 -> "U16"
  Z9 -> "Z9"
  ZA -> "ZA"
  ZM -> "ZM"
  ZH -> "ZH"
  XH -> "XH"
  K1 -> "K1"
  DF -> "DF"
  IssuerNotLive -> "15"
  OtherCode t -> t

errorCodeCategory :: ErrorCode -> FailureCategory
errorCodeCategory = \case
  UB -> Technical
  U30 -> Technical
  UP -> Technical
  U16 -> Business
  Z9 -> Business
  ZA -> Business
  ZM -> Business
  ZH -> Business
  XH -> Business
  K1 -> Business
  DF -> Business
  IssuerNotLive -> Business
  OtherCode _ -> Technical

errorCodeDescription :: ErrorCode -> Text
errorCodeDescription = \case
  UB -> "Unable to process due to internal exception at beneficiary server/CBS"
  U30 -> "Debit has failed at the remitter CBS"
  UP -> "PSP did not respond within the switch timeout window"
  U16 -> "Risk threshold exceeded at the PSP"
  Z9 -> "Insufficient funds in the payer account"
  ZA -> "Transaction declined by the customer"
  ZM -> "Incorrect MPIN"
  ZH -> "Invalid virtual payment address"
  XH -> "Beneficiary account does not exist"
  K1 -> "Suspected fraud; declined on business grounds"
  DF -> "Duplicate retrieval reference number"
  IssuerNotLive -> "Issuer not live on UPI"
  OtherCode _ -> "Unmapped NPCI response code"

-- | Accepts the named codes and, as a fallback, any two- or three-character
-- upper-case alphanumeric token. Rejects everything else so that a typo in a
-- scenario file fails at the boundary instead of at reconciliation time.
parseErrorCode :: Text -> Maybe ErrorCode
parseErrorCode raw = case lookup token table of
  Just c -> Just c
  Nothing
    | T.length token `elem` [2, 3], T.all wireChar token -> Just (OtherCode token)
    | otherwise -> Nothing
  where
    token = T.toUpper (T.strip raw)
    table = [(errorCodeText c, c) | c <- knownErrorCodes]
    wireChar c = isAsciiUpper c || isDigit c

instance ToJSON ErrorCode where
  toJSON = toJSON . errorCodeText

instance FromJSON ErrorCode where
  parseJSON =
    withText "ErrorCode" $ \t ->
      maybe (fail ("unknown NPCI error code: " <> T.unpack t)) pure (parseErrorCode t)

-- | Reconciliation outcomes that promote a @TIMEOUT@ to @SUCCESS@ (design
-- document §4.3). Carrying the code in the transition type is what makes
-- \"reconciled without a TTUM record\" unrepresentable.
data ReconCode
  = -- | @102@ beneficiary account credited online.
    ReconCreditedOnline
  | -- | @103@ beneficiary account credited manually post-reconciliation.
    ReconCreditedManually
  deriving stock (Eq, Show, Enum, Bounded)

reconCodeText :: ReconCode -> Text
reconCodeText = \case
  ReconCreditedOnline -> "102"
  ReconCreditedManually -> "103"

reconCodeDescription :: ReconCode -> Text
reconCodeDescription = \case
  ReconCreditedOnline -> "Beneficiary account credited online"
  ReconCreditedManually -> "Beneficiary account credited manually post-reconciliation"

parseReconCode :: Text -> Maybe ReconCode
parseReconCode raw =
  lookup (T.strip raw) [(reconCodeText c, c) | c <- [minBound .. maxBound]]

instance ToJSON ReconCode where
  toJSON = toJSON . reconCodeText

instance FromJSON ReconCode where
  parseJSON =
    withText "ReconCode" $ \t ->
      maybe (fail ("unknown reconciliation code: " <> T.unpack t)) pure (parseReconCode t)
