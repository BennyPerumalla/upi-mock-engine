-- |
-- Module      : UPIMock.API.Routes
-- Description : The API as a type, and the OpenAPI document derived from it.
--
-- The route table is a type, so the compiler checks the handlers against it. A
-- handler with the wrong argument order, a missing capture, or a response type
-- that no longer matches the resource is a type error at the point where
-- 'UPIMock.API.Handlers.server' is defined, not a 404 discovered by a client.
--
-- __The OpenAPI document is derived, never written.__ 'openApiSpec' is a function
-- of 'UPIApi' and the @ToSchema@ instances in "UPIMock.API.Wire". A route added
-- below appears in the document with no further work; a resource whose fields
-- change updates its own schema. A hand-maintained specification file would be
-- wrong within a week, and the wrongness would be invisible.
--
-- Note that the document describes 'UPIApi' and not 'FullApi': the specification
-- endpoint is not itself part of the specification. Including it would be
-- self-reference for no benefit, and it makes the document noisier for the
-- generators that consume it.
module UPIMock.API.Routes
  ( -- * Route table
    FullApi
  , UPIApi
  , TransactionApi
  , fullApi

    -- * Derived specification
  , openApiSpec
  ) where

import Data.OpenApi (Info (..), OpenApi (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Servant
  ( Capture
  , Get
  , JSON
  , Post
  , PostCreated
  , QueryParam
  , ReqBody
  , (:<|>)
  , (:>)
  )
import Servant.OpenApi (toOpenApi)

import Paths_upi_mock_engine (version)
import UPIMock.API.Wire
  ( CommandBody
  , EventResource
  , HealthResource
  , InitiateBody
  , TxnPage
  , TxnResource
  )

-- | Everything under @\/v1\/transactions@.
--
-- Captures are 'Text' and not 'UPIMock.Domain.Types.TxnId' on purpose: a
-- @FromHttpApiData TxnId@ instance would be a second way into the domain that
-- bypasses "UPIMock.API.Wire", and it would turn a malformed id into Servant's
-- generic 400 with a message nobody wrote. Parsing in the handler produces the
-- same error shape as every other rejection.
--
-- __Why one @commands@ endpoint__ rather than @\/authorize@, @\/decline@,
-- @\/timeout@ and six more siblings: every one of them would be the same handler
-- with a different constructor, and the state machine — not the URL space —
-- decides which are legal from here. The set of legal commands is a property of
-- the aggregate's current state, so it cannot be expressed as a set of routes
-- without lying about it.
type TransactionApi =
  ReqBody '[JSON] InitiateBody :> PostCreated '[JSON] TxnResource
    :<|> QueryParam "state" Text
      :> QueryParam "flow" Text
      :> QueryParam "limit" Int
      :> QueryParam "offset" Int
      :> Get '[JSON] TxnPage
    :<|> Capture "txnId" Text :> Get '[JSON] TxnResource
    :<|> Capture "txnId" Text :> "events" :> Get '[JSON] [EventResource]
    :<|> Capture "txnId" Text
      :> "commands"
      :> ReqBody '[JSON] CommandBody
      :> Post '[JSON] TxnResource

-- | The documented API.
--
-- @\/v1\/rrn\/{rrn}@ is a read-model lookup, not a search: the RRN is unique
-- across the instance (design document §8.2.3) so it addresses exactly one
-- transaction, and a harness that only kept the RRN from an NPCI-shaped response
-- can still find its transaction.
type UPIApi =
  "v1" :> "transactions" :> TransactionApi
    :<|> "v1" :> "rrn" :> Capture "rrn" Text :> Get '[JSON] TxnResource
    :<|> "health" :> Get '[JSON] HealthResource

-- | What the server actually serves: the API plus its own specification.
type FullApi =
  UPIApi
    :<|> "openapi.json" :> Get '[JSON] OpenApi

fullApi :: Proxy FullApi
fullApi = Proxy

-- | The OpenAPI 3 document for 'UPIApi', with the metadata a generated document
-- cannot know.
--
-- Built by record update on the generated value rather than with the lens API
-- that @openapi3@ also offers. The result is identical and the dependency
-- footprint is one package smaller, which for a project whose selling point is
-- that it builds is the better trade.
openApiSpec :: OpenApi
openApiSpec = generated {_openApiInfo = described}
  where
    generated = toOpenApi (Proxy :: Proxy UPIApi)
    described =
      (_openApiInfo generated)
        { _infoTitle = "UPI-MockEngine"
        , _infoVersion = T.pack (showVersion version)
        , _infoDescription =
            Just
              "A deterministic simulator for the UPI transaction lifecycle.\n\n\
              \State transitions are enforced by the type system: the engine cannot \
              \be driven into an illegal state, and a command that does not apply to \
              \the current state is rejected as 409 with the state it was rejected \
              \from. Every accepted command appends to an event log, which is the \
              \system of record; the resources returned here are a projection of \
              \that log and carry the stream version they were built from.\n\n\
              \Response codes are the NPCI codes the specification defines, served \
              \with their descriptions so that a failing test explains itself."
        }
