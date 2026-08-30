-- |
-- Module      : UPIMock.Domain.EventsSpec
-- Description : The log's on-disk vocabulary, pinned.
--
-- @event_type@ is a denormalised column: the payload already carries its own tag,
-- and the column exists so that SQL can index and group without decoding JSON.
-- That redundancy is only safe while the two agree, and nothing in the type system
-- makes them agree — hence this spec.
--
-- 'expectedTag' deliberately restates the vocabulary instead of calling
-- 'eventTypeText'. A test that compared 'eventTypeText' with itself would pass
-- after a careless rename; this one fails, and because it is a @case@ over a
-- closed type, adding a constructor without extending it is a compile error under
-- @-Wincomplete-patterns@.
module UPIMock.Domain.EventsSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Hedgehog (forAll, (===))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import UPIMock.Domain.ErrorCodes (ErrorCode (Z9))
import UPIMock.Domain.Events
  ( AggregateType (..)
  , DomainEvent (..)
  , PayloadVersion (..)
  , aggregateTypeText
  , currentPayloadVersion
  , eventTypeText
  , parseAggregateType
  )
import UPIMock.Support.Gen (genDomainEvent)

spec :: Spec
spec = do
  describe "eventTypeText" $ do
    it "agrees with the tag inside the payload" $ hedgehog $ do
      event <- forAll genDomainEvent
      jsonTag event === Just (eventTypeText event)

    it "agrees with the vocabulary this spec names independently" $ hedgehog $ do
      event <- forAll genDomainEvent
      eventTypeText event === expectedTag event

  describe "payload" $ do
    it "round-trips through JSON" $ hedgehog $ do
      -- Not a formality: every value object inside a seed re-parses through its
      -- smart constructor on the way back in, so this also asserts that the
      -- encoding never emits a value its own parser would reject.
      event <- forAll genDomainEvent
      decode (encode event) === Just event

    it "carries no data field for a nullary event" $ do
      toJSON TxnSucceeded `shouldBe` object ["type" .= ("TXN_SUCCEEDED" :: Text)]
      toJSON RefundSettled `shouldBe` object ["type" .= ("REFUND_SETTLED" :: Text)]

    it "carries the argument under data for the rest" $ do
      toJSON (TxnFailed Z9)
        `shouldBe` object ["type" .= ("TXN_FAILED" :: Text), "data" .= ("Z9" :: Text)]

  describe "AggregateType" $ do
    it "round-trips through its column spelling" $ do
      let types = [minBound .. maxBound] :: [AggregateType]
      map (parseAggregateType . aggregateTypeText) types `shouldBe` map Just types

    it "does not yet admit the Phase-2 aggregates" $ do
      -- The column is wide enough for them; the parser is not. A stream labelled
      -- MANDATE in a Phase-1 database is corruption, not forward compatibility.
      parseAggregateType "MANDATE" `shouldBe` Nothing
      parseAggregateType "DISPUTE" `shouldBe` Nothing

  describe "PayloadVersion" $ do
    it "is 1, and stays 1 until an upcaster exists" $
      -- This assertion is a tripwire, not a fact worth testing: bumping the
      -- version is exactly the change that must not pass review without the
      -- corresponding upcaster (CONTRIBUTING.md § Changing an event).
      unPayloadVersion currentPayloadVersion `shouldBe` 1

-- | The @type@ field of the encoded payload, or 'Nothing' if the encoding is not
-- a tagged object at all — which is itself a failure worth reporting.
jsonTag :: DomainEvent -> Maybe Text
jsonTag event = case toJSON event of
  Object fields -> case KM.lookup "type" fields of
    Just (String tag) -> Just tag
    _ -> Nothing
  _ -> Nothing

-- | The on-disk vocabulary, restated. Exhaustive by compiler check.
expectedTag :: DomainEvent -> Text
expectedTag = \case
  TxnInitiated{} -> "TXN_INITIATED"
  TxnAuthorized{} -> "TXN_AUTHORIZED"
  TxnDeclinedAtPsp{} -> "TXN_DECLINED_AT_PSP"
  TxnSucceeded -> "TXN_SUCCEEDED"
  TxnFailed{} -> "TXN_FAILED"
  TxnTimedOut -> "TXN_TIMED_OUT"
  TxnReconciled{} -> "TXN_RECONCILED"
  TxnReconciliationDropped{} -> "TXN_RECONCILIATION_DROPPED"
  RefundOpened{} -> "REFUND_OPENED"
  RefundSettled -> "REFUND_SETTLED"
