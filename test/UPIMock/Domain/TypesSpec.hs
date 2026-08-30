-- |
-- Module      : UPIMock.Domain.TypesSpec
-- Description : Properties of the value objects.
--
-- These are the invariants everything above depends on. If 'mkRrn' admits an
-- eleven-digit string, the RRN index stops being an index; if 'mkVpa' stops
-- normalising case, @alice\@psp@ and @Alice\@PSP@ become two payers and the
-- duplicate-RRN test starts passing for the wrong reason.
--
-- The generators only produce strings the constructors should accept, and the
-- rejection properties construct their counterexamples explicitly. Generating
-- \"random text\" and asserting that most of it fails would test the generator,
-- not the parser.
module UPIMock.Domain.TypesSpec (spec) where

import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import UPIMock.Domain.Types
  ( ValidationError (..)
  , amountPaise
  , mkAmount
  , mkNote
  , mkRrn
  , mkVpa
  , noteText
  , parseFlow
  , renderAmount
  , renderFlow
  , rrnOfWord
  , rrnText
  , streamIdOfTxn
  , txnIdOfStream
  , upiPerTxnCapPaise
  , vpaHandle
  , vpaText
  )
import UPIMock.Support.Gen (genFlow, genRrn, genTxnId, genVpaParts)

spec :: Spec
spec = do
  describe "Rrn" $ do
    it "is exactly twelve digits" $ hedgehog $ do
      rrn <- forAll genRrn
      T.length (rrnText rrn) === 12

    it "rejects every other length" $ hedgehog $ do
      n <- forAll (Gen.filter (/= 12) (Gen.int (Range.linear 0 20)))
      raw <- forAll (T.pack <$> Gen.list (Range.singleton n) Gen.digit)
      mkRrn raw === Left (InvalidRrn raw)

    it "rejects non-digits at the right length" $ hedgehog $ do
      raw <- forAll (T.pack <$> Gen.list (Range.singleton 12) Gen.alpha)
      mkRrn raw === Left (InvalidRrn raw)

    it "mints a parseable RRN from any entropy draw" $ hedgehog $ do
      -- The reason 'rrnOfWord' needs no error channel: this holds for every
      -- 'Word64', so the engine never has to handle an impossible 'Left'.
      draw <- forAll (Gen.word64 Range.constantBounded)
      let minted = rrnOfWord draw
      T.length (rrnText minted) === 12
      mkRrn (rrnText minted) === Right minted

  describe "Vpa" $ do
    it "is case-insensitive" $ hedgehog $ do
      (local, handle) <- forAll genVpaParts
      let raw = local <> "@" <> handle
      mkVpa (T.toUpper raw) === mkVpa raw

    it "normalises to lower case" $ hedgehog $ do
      (local, handle) <- forAll genVpaParts
      let raw = local <> "@" <> handle
      fmap vpaText (mkVpa raw) === Right (T.toLower raw)

    it "exposes the handle" $ hedgehog $ do
      (local, handle) <- forAll genVpaParts
      fmap vpaHandle (mkVpa (local <> "@" <> handle)) === Right (T.toLower handle)

    it "rejects an address with no handle" $ do
      mkVpa "alice" `shouldBe` Left (InvalidVpa "alice")
      mkVpa "alice@" `shouldBe` Left (InvalidVpa "alice@")
      mkVpa "a@b@c" `shouldBe` Left (InvalidVpa "a@b@c")

  describe "Amount" $ do
    it "accepts one paise up to the per-transaction cap" $ hedgehog $ do
      paise <- forAll (Gen.word64 (Range.linear 1 upiPerTxnCapPaise))
      fmap amountPaise (mkAmount paise) === Right paise

    it "rejects zero" $ do
      mkAmount 0 `shouldBe` Left (AmountOutOfRange 0)

    it "rejects anything above the cap" $ hedgehog $ do
      paise <- forAll (Gen.word64 (Range.linear (upiPerTxnCapPaise + 1) maxBound))
      mkAmount paise === Left (AmountOutOfRange paise)

    it "renders paise as fixed-point rupees" $ do
      fmap renderAmount (mkAmount 1) `shouldBe` Right "0.01"
      fmap renderAmount (mkAmount 100) `shouldBe` Right "1.00"
      fmap renderAmount (mkAmount 1234) `shouldBe` Right "12.34"
      fmap renderAmount (mkAmount upiPerTxnCapPaise) `shouldBe` Right "100000.00"

  describe "Note" $ do
    it "accepts up to fifty characters and trims" $ hedgehog $ do
      body <- forAll (Gen.text (Range.linear 0 50) Gen.alphaNum)
      fmap noteText (mkNote ("  " <> body <> "  ")) === Right body

    it "rejects fifty-one" $ hedgehog $ do
      body <- forAll (Gen.text (Range.linear 51 200) Gen.alphaNum)
      mkNote body === Left (NoteTooLong (T.length body))

  describe "StreamId" $ do
    it "round-trips a transaction id" $ hedgehog $ do
      -- 'txnIdOfStream' is what the boot-time rebuild uses to attribute a stream
      -- to an aggregate. If it stopped inverting 'streamIdOfTxn', every restart
      -- would come up with an empty read model and no error.
      txnId <- forAll genTxnId
      txnIdOfStream (streamIdOfTxn txnId) === Just txnId

  describe "Flow" $ do
    it "round-trips through its wire spelling" $ hedgehog $ do
      flow <- forAll genFlow
      parseFlow (renderFlow flow) === Just flow

    it "rejects an unknown spelling" $ do
      parseFlow "P2P" `shouldBe` Nothing
      parseFlow "p2p_intent" `shouldBe` Nothing
