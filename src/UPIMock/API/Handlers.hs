-- |
-- Module      : UPIMock.API.Handlers
-- Description : The HTTP surface. Parse, delegate, translate the failure.
--
-- Every handler in this module does exactly three things: it turns text into
-- value objects with "UPIMock.API.Wire", it calls one function from
-- "UPIMock.Application.Service", and it translates a 'ServiceError' into a status
-- code. There is no branching on transaction state here, no event-log knowledge,
-- and no SQL — a handler that needed any of those would be a sign that a use case
-- is missing from the application layer.
--
-- __No @hoistServer@.__ Each handler calls 'runApp' on the environment it was
-- given and lifts the result. Threading @App@ through Servant's natural
-- transformation would buy nothing here: the handlers return 'Either' rather than
-- throwing, so there is no error layer to unify, and 'Handler' would still be the
-- outer monad. This way the type of every handler says precisely what it does.
--
-- __Where status codes come from.__ 'UPIMock.Application.Service.ServiceError'
-- was deliberately kept as five constructors so that this module can map them
-- without inspecting the payload; the mapping is in 'serviceServerError' and is
-- the only place in the codebase that mentions an HTTP status.
module UPIMock.API.Handlers
  ( -- * Server
    server
  , application

    -- * Failure translation
  , serviceServerError
  , validationServerError
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application)
import Servant
  ( Handler
  , Server
  , ServerError (..)
  , err400
  , err404
  , err409
  , err500
  , err503
  , serve
  , (:<|>) (..)
  )

import UPIMock.API.Routes (FullApi, fullApi, openApiSpec)
import UPIMock.API.Wire
  ( CommandBody
  , ErrorResource (..)
  , EventResource
  , HealthResource (..)
  , InitiateBody
  , TxnPage (..)
  , TxnResource
  , parseCommand
  , parseInitiate
  , parseRrn
  , parseTxnId
  , parseViewQuery
  , toEventResource
  , toTxnResource
  )
import UPIMock.App (App, Diagnostics (..), Env, diagnostics, runApp)
import UPIMock.Application.Ports
  ( StoreError (..)
  , ViewQuery (..)
  , getView
  , getViewByRrn
  , queryViews
  )
import UPIMock.Application.Service
  ( ServiceError (..)
  , applyCommand
  , fetchEventLog
  , initiateTransaction
  , renderServiceError
  )
import UPIMock.Domain.ErrorCodes (ErrorCode (DF), errorCodeText)
import UPIMock.Domain.Types (Rrn, ValidationError, renderValidationError, rrnText)

-- | The whole server, parameterised by the environment it runs against.
--
-- The nesting mirrors 'FullApi' exactly, and @:<|>@ is right-associative in both
-- the type and the value, which is why the transaction handlers can be written as
-- a flat chain. Reordering two handlers here is a type error unless their
-- signatures happen to coincide; adding a route to 'FullApi' without a handler is
-- a type error at this definition.
server :: Env -> Server FullApi
server env =
  ( ( initiateHandler env
        :<|> listHandler env
        :<|> getHandler env
        :<|> eventsHandler env
        :<|> commandHandler env
    )
      :<|> rrnHandler env
      :<|> healthHandler env
  )
    :<|> specHandler

-- | WAI application. Middleware is applied in @Main@, not here, so that this
-- function stays usable from a test that wants the bare application.
application :: Env -> Application
application env = serve fullApi (server env)

-- | An infallible read against the projection.
runRead :: Env -> App a -> Handler a
runRead env = liftIO . runApp env

-- | A use case, with its failure translated to a status code.
runUseCase :: Env -> App (Either ServiceError a) -> Handler a
runUseCase env action = runRead env action >>= either (throwError . serviceServerError) pure

-- | Boundary parse, or 400. Every capture, query parameter and body field in this
-- module passes through here, which is what guarantees that no unvalidated text
-- reaches the application layer.
orInvalid :: Either ValidationError a -> Handler a
orInvalid = either (throwError . validationServerError) pure

-- | @POST \/v1\/transactions@ — 201 with the projected resource.
--
-- The RRN in the response is the one the transaction will keep for its lifetime,
-- whether the client supplied it or the simulator minted it, so a harness can
-- record it here and use @\/v1\/rrn\/{rrn}@ from then on.
initiateHandler :: Env -> InitiateBody -> Handler TxnResource
initiateHandler env body = do
  request <- orInvalid (parseInitiate body)
  toTxnResource <$> runUseCase env (initiateTransaction request)

-- | @GET \/v1\/transactions@ — a page of the projection.
--
-- @count@ is the size of this page, not of the result set. A total would have to
-- be computed by a second traversal under the same snapshot to be honest, and a
-- number that can disagree with @items@ is worse than no number at all.
listHandler :: Env -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler TxnPage
listHandler env state flow limit offset = do
  query <- orInvalid (parseViewQuery state flow limit offset)
  views <- runRead env (queryViews query)
  let items = map toTxnResource views
  pure
    TxnPage
      { tpItems = items
      , tpLimit = queryLimit query
      , tpOffset = queryOffset query
      , tpCount = length items
      }

-- | @GET \/v1\/transactions\/{txnId}@ — read model, not a replay.
--
-- Served from the projection because that is what the projection is for. A client
-- that needs the authoritative history asks for @\/events@, which reads the log.
getHandler :: Env -> Text -> Handler TxnResource
getHandler env raw = do
  txnId <- orInvalid (parseTxnId raw)
  found <- runRead env (getView txnId)
  maybe (throwError (serviceServerError (ServiceNotFound txnId))) (pure . toTxnResource) found

-- | @GET \/v1\/transactions\/{txnId}\/events@ — the stream, in order.
--
-- Reads the log rather than the projection, and therefore 404s on an empty stream
-- rather than on a missing view: the log is the system of record, so \"does this
-- transaction exist\" is a question only it can answer.
eventsHandler :: Env -> Text -> Handler [EventResource]
eventsHandler env raw = do
  txnId <- orInvalid (parseTxnId raw)
  map toEventResource <$> runUseCase env (fetchEventLog txnId)

-- | @POST \/v1\/transactions\/{txnId}\/commands@ — the only write path after
-- creation.
--
-- A command that does not apply to the current state comes back as 409 carrying
-- the state it was rejected from, which is the single most useful thing this
-- simulator tells a client under test.
commandHandler :: Env -> Text -> CommandBody -> Handler TxnResource
commandHandler env raw body = do
  txnId <- orInvalid (parseTxnId raw)
  command <- orInvalid (parseCommand body)
  toTxnResource <$> runUseCase env (applyCommand txnId command)

-- | @GET \/v1\/rrn\/{rrn}@ — the same resource, addressed the way a switch
-- addresses it.
rrnHandler :: Env -> Text -> Handler TxnResource
rrnHandler env raw = do
  rrn <- orInvalid (parseRrn raw)
  found <- runRead env (getViewByRrn rrn)
  maybe (throwError (missingRrn rrn)) (pure . toTxnResource) found

-- | @GET \/health@ — liveness plus the three facts that explain a misbehaving
-- instance.
--
-- Always 200 while the process is listening. A store that has become unreachable
-- surfaces as 503 on the endpoint that touched it, not here: a health check that
-- performs a write to decide its answer is a health check that changes the state
-- it reports on.
healthHandler :: Env -> Handler HealthResource
healthHandler env = do
  diag <- liftIO (diagnostics env)
  pure
    HealthResource
      { hrStatus = "ok"
      , hrVersion = diagVersion diag
      , hrSchemaVersion = diagSchemaVersion diag
      , hrJournalMode = diagJournalMode diag
      , hrTransactions = diagProjectionSize diag
      }

-- | @GET \/openapi.json@ — the document derived from the route table.
specHandler :: Handler OpenApi
specHandler = pure openApiSpec

-- | The one place in the codebase that decides an HTTP status.
--
-- The @error@ field is a stable machine-readable class; the @message@ is the
-- domain's own rendering and may be reworded. Clients are expected to branch on
-- the class and log the message.
--
-- Two mappings deserve their reasoning stated. A 'DuplicateRrn' is 409 and not
-- 400, because the request was well formed and lost a race for a unique value —
-- and it carries NPCI @DF@, so a harness driving duplicate-RRN behaviour sees the
-- code the real switch would return rather than having to infer it from prose. A
-- 'CorruptPayload' is 500 and not 422: the client sent nothing wrong, the stored
-- log did, and an operator has to know that the failure is on this side.
serviceServerError :: ServiceError -> ServerError
serviceServerError err =
  jsonError status ErrorResource {xrError = klass, xrMessage = message, xrNpciCode = npci}
  where
    message = renderServiceError err
    (status, klass, npci) = case err of
      ServiceInvalid _ -> (err400, "VALIDATION_FAILED", Nothing)
      ServiceDomain _ -> (err409, "ILLEGAL_TRANSITION", Nothing)
      ServiceStore VersionConflict {} -> (err409, "VERSION_CONFLICT", Nothing)
      ServiceStore (DuplicateRrn _) -> (err409, "DUPLICATE_RRN", Just (errorCodeText DF))
      ServiceStore CorruptPayload {} -> (err500, "CORRUPT_LOG", Nothing)
      ServiceStore (StoreUnavailable _) -> (err503, "STORE_UNAVAILABLE", Nothing)
      ServiceReplay _ -> (err500, "CORRUPT_LOG", Nothing)
      ServiceNotFound _ -> (err404, "NOT_FOUND", Nothing)

-- | A boundary parse failure. Exported because the test suite asserts on the
-- shape of a 400 body, and asserting against the function that produces it is
-- worth more than asserting against a copy of it.
validationServerError :: ValidationError -> ServerError
validationServerError err =
  jsonError
    err400
    ErrorResource
      { xrError = "VALIDATION_FAILED"
      , xrMessage = renderValidationError err
      , xrNpciCode = Nothing
      }

-- | An RRN with no transaction behind it. Distinct from 'ServiceNotFound', which
-- is keyed by transaction id and would have to invent one here.
missingRrn :: Rrn -> ServerError
missingRrn rrn =
  jsonError
    err404
    ErrorResource
      { xrError = "NOT_FOUND"
      , xrMessage = "no transaction with RRN " <> rrnText rrn
      , xrNpciCode = Nothing
      }

-- | Give a 'ServerError' a JSON body.
--
-- Servant's @errXXX@ values carry an empty body and no content type, so a client
-- that always parses JSON would choke on the error path of an otherwise
-- all-JSON API. Setting both here means every failure this server emits is the
-- same shape, and 'ErrorResource' is that shape.
jsonError :: ServerError -> ErrorResource -> ServerError
jsonError base resource =
  base
    { errBody = encode resource
    , errHeaders = [(hContentType, "application/json;charset=utf-8")]
    }
