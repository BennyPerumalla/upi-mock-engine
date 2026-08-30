-- |
-- Module      : UPIMock.API.RoutesSpec
-- Description : The published OpenAPI document, pinned.
--
-- The document is /derived/ from 'UPIMock.API.Routes.UPIApi' and the @ToSchema@
-- instances in "UPIMock.API.Wire", which means nobody reviews it. A route renamed,
-- a method changed, a resource that stops reaching the document under its own
-- name: all three are silent, and all three break every generated client. This
-- spec is the review.
--
-- Assertions are against @toJSON openApiSpec@ rather than against @openapi3@'s
-- record or lens API, because the JSON /is/ the artifact. A change that leaves the
-- Haskell value equal while altering its encoding is still a breaking change
-- downstream, and only the encoded form can catch it.
--
-- What this spec does not do is exercise a server: there is no @hspec-wai@ here
-- and Phase 1 does not have one. The HTTP layer's own seam — untrusted text to
-- domain value, and use case to resource — is covered by "UPIMock.API.WireSpec"
-- and "UPIMock.Application.ServiceSpec" without a socket. End-to-end tests arrive
-- with the Phase-2 harness that needs a live store anyway.
module UPIMock.API.RoutesSpec (spec) where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (foldl', for_, toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Test.Hspec

import Paths_upi_mock_engine (version)
import UPIMock.API.Routes (openApiSpec)

spec :: Spec
spec = do
  describe "openApiSpec" $ do
    it "is an OpenAPI 3 document" $
      fmap (T.isPrefixOf "3.") (textAt ["openapi"] doc) `shouldBe` Just True

    it "publishes the package version as the API version" $ do
      -- A consumer caches the document against this string. A release that changed
      -- a schema without changing the version would be undetectable to it.
      textAt ["info", "title"] doc `shouldBe` Just "UPI-MockEngine"
      textAt ["info", "version"] doc `shouldBe` Just (T.pack (showVersion version))

    it "carries the description a derived document cannot invent" $
      fmap T.null (textAt ["info", "description"] doc) `shouldBe` Just False

    it "documents every endpoint a client can call" $
      keysAt ["paths"] doc `shouldMatchList` documentedPaths

    it "does not document the endpoint that serves it" $
      -- The document describes 'UPIMock.API.Routes.UPIApi', not @FullApi@.
      -- Self-reference would need a schema for 'Data.OpenApi.OpenApi' itself and
      -- buys a code generator nothing.
      keysAt ["paths"] doc `shouldNotContain` ["/openapi.json"]

    it "offers exactly the methods each endpoint implements" $ do
      keysAt ["paths", "/v1/transactions"] doc `shouldMatchList` ["get", "post"]
      keysAt ["paths", "/v1/transactions/{txnId}"] doc `shouldMatchList` ["get"]
      keysAt ["paths", "/v1/transactions/{txnId}/events"] doc `shouldMatchList` ["get"]
      keysAt ["paths", "/v1/transactions/{txnId}/commands"] doc `shouldMatchList` ["post"]
      keysAt ["paths", "/v1/rrn/{rrn}"] doc `shouldMatchList` ["get"]
      keysAt ["paths", "/health"] doc `shouldMatchList` ["get"]

    it "documents the creation endpoint as 201" $
      -- 'Servant.PostCreated' is a decision in the route type. A 200 here would
      -- tell a generated client that nothing was created.
      keysAt ["paths", "/v1/transactions", "post", "responses"] doc `shouldContain` ["201"]

    it "documents the four filters of the collection endpoint" $
      namesAt ["paths", "/v1/transactions", "get", "parameters"] doc
        `shouldMatchList` ["state", "flow", "limit", "offset"]

    it "declares a named schema for every resource it serves" $ do
      let declared = keysAt ["components", "schemas"] doc
      for_ declaredSchemas $ \name -> declared `shouldContain` [name]

-- | The route table of "UPIMock.API.Routes", restated as the paths it must
-- produce. Written out rather than derived from the type, for the reason every
-- vocabulary in this suite is written out: a URL is a promise to a client, and a
-- promise that changes whenever the code changes was never one.
documentedPaths :: [Text]
documentedPaths =
  [ "/v1/transactions"
  , "/v1/transactions/{txnId}"
  , "/v1/transactions/{txnId}/events"
  , "/v1/transactions/{txnId}/commands"
  , "/v1/rrn/{rrn}"
  , "/health"
  ]

-- | Schemas the document must name. Deliberately not exhaustive over
-- "UPIMock.API.Wire": what is asserted is that each resource reached the document
-- under its own name, which is what makes a generated client's types legible.
--
-- 'UPIMock.API.Wire.ErrorResource' is absent on purpose. It is served on failing
-- statuses, which Servant renders outside the typed route table, so it cannot
-- appear in a derived document — the price of not writing the document by hand.
declaredSchemas :: [Text]
declaredSchemas =
  [ "InitiateBody"
  , "CommandBody"
  , "CommandAction"
  , "TxnResource"
  , "TxnPage"
  , "EventResource"
  , "PartyResource"
  , "CodeResource"
  , "HealthResource"
  ]

-- | The document as a client receives it.
doc :: Value
doc = toJSON openApiSpec

-- | Follow a path of object keys. A missing key, or a non-object encountered on
-- the way, is 'Nothing'.
at :: [Text] -> Value -> Maybe Value
at path value = foldl' descend (Just value) path
  where
    descend (Just (Object fields)) key = KM.lookup (Key.fromText key) fields
    descend _ _ = Nothing

-- | The keys of the object at a path, or @[]@ when there is no object there.
-- Returning the empty list rather than failing is what keeps a failure legible:
-- @shouldMatchList@ then prints every expected key as missing, which is the truth.
keysAt :: [Text] -> Value -> [Text]
keysAt path value = case at path value of
  Just (Object fields) -> map Key.toText (KM.keys fields)
  _ -> []

textAt :: [Text] -> Value -> Maybe Text
textAt path value = case at path value of
  Just (String text) -> Just text
  _ -> Nothing

-- | The @name@ of every element of the array at a path — the shape OpenAPI uses
-- for parameter lists.
namesAt :: [Text] -> Value -> [Text]
namesAt path value = case at path value of
  Just (Array items) -> [name | item <- toList items, Just name <- [textAt ["name"] item]]
  _ -> []
