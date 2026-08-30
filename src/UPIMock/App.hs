-- |
-- Module      : UPIMock.App
-- Description : The concrete monad. Where the ports meet the adapters, and the
--               only module that knows about both.
--
-- @App@ is @ReaderT Env IO@ and nothing more. It carries no state, because the
-- state is in the event log; it carries no error layer, because failures are
-- values in the ports; and it carries no writer, because logging is an effect
-- like any other. What that leaves is an environment holding two handles.
--
-- __This module is the seam.__ Every instance below delegates to a handle
-- obtained from 'Env'. Phase 2's PostgreSQL migration is therefore: write
-- @UPIMock.Infrastructure.Postgres@ with the same three @IO@ functions, change
-- the type of 'envStore', and fix the three lines in the
-- 'UPIMock.Application.Ports.MonadEventStore' instance that name them. No
-- use case, no handler, and no test changes, because none of them mention
-- @Env@'s field types.
--
-- __Why the instances live here.__ The class is declared in
-- "UPIMock.Application.Ports", the implementation in
-- "UPIMock.Infrastructure.Database". An instance in either module would be an
-- orphan; an instance here is not, because @App@ is defined here. That is the
-- entire reason this module exists rather than folding into @Main@.
module UPIMock.App
  ( -- * Environment
    Env (..)
  , withEnv

    -- * The monad
  , App
  , runApp

    -- * Boot
  , bootstrap

    -- * Operational facts
  , Diagnostics (..)
  , diagnostics
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Data.Bifunctor (first)
import Data.Bits (shiftL, (.|.))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Data.Version (showVersion)

import Paths_upi_mock_engine (version)

import UPIMock.Application.Ports
  ( MonadClock (..)
  , MonadEntropy (..)
  , MonadEventStore (..)
  , MonadReadModel (..)
  )
import UPIMock.Application.Service (rebuildReadModel, renderServiceError)
import UPIMock.Config (AppConfig (..))
import UPIMock.Infrastructure.Database
  ( SqliteHandle
  , commitIO
  , readAllEventsIO
  , readStreamIO
  , sqliteJournalMode
  , withSqlite
  )
import UPIMock.Infrastructure.ReadModel
  ( ReadModel
  , getViewByRrnIO
  , getViewIO
  , newReadModel
  , putViewIO
  , queryViewsIO
  , readModelCount
  , replaceAllViewsIO
  )
import UPIMock.Infrastructure.Schema (schemaVersion)

-- | The two handles and the configuration that produced them.
--
-- Phase 2 adds @envChaos :: TVar ChaosProfile@ and @envPrng :: TVar SMGen@ here.
-- Both are 'TVar's for the same reason the read model is: the chaos profile is
-- mutated over HTTP while requests are in flight, and STM is the only way to do
-- that without a lock discipline nobody will follow.
data Env = Env
  { envConfig :: AppConfig
  , envStore :: SqliteHandle
  , envReadModel :: ReadModel
  }

-- | Acquire both handles for the lifetime of an action.
--
-- The read model is created empty and is /not/ populated here: rebuilding it is
-- 'bootstrap', which can fail, and a resource-acquisition bracket is the wrong
-- place to decide whether the log is readable.
withEnv :: AppConfig -> (Env -> IO a) -> IO a
withEnv config action =
  withSqlite (cfgDatabase config) (cfgReaderPoolSize config) $ \store -> do
    readModel <- newReadModel
    action Env {envConfig = config, envStore = store, envReadModel = readModel}

-- | Deliberately not exported with its constructor. A caller that can build an
-- @App@ from an arbitrary @ReaderT Env IO@ can bypass the ports, and the ports
-- are the architecture.
newtype App a = App (ReaderT Env IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    )

runApp :: Env -> App a -> IO a
runApp env (App action) = runReaderT action env

instance MonadClock App where
  currentTime = liftIO getCurrentTime

-- | Both draws come from one source.
--
-- 'freshWord64' takes the high two words of a version-4 UUID rather than calling
-- into a second generator, so the whole system consumes exactly one stream of
-- randomness. In Phase 2 that stream becomes a seeded splitmix generator and
-- every identifier in a run becomes reproducible from the seed; had entropy been
-- drawn from two places, seeding one of them would have produced a run that is
-- only half deterministic — the worst of both.
instance MonadEntropy App where
  freshUuid = liftIO UUIDv4.nextRandom

  freshWord64 = do
    (high, low, _, _) <- UUID.toWords <$> freshUuid
    pure (fromIntegral high `shiftL` 32 .|. fromIntegral low)

-- | @recordedAt@ is taken from 'currentTime' here, in the instance, rather than
-- inside the adapter. The adapter has no clock; this is the one place where
-- wall-clock time enters the write path, which is what makes Phase 2's injected
-- clock skew a change to 'MonadClock' and nothing else.
instance MonadEventStore App where
  readStream stream = do
    store <- asks envStore
    liftIO (readStreamIO store stream)

  readAllEvents = do
    store <- asks envStore
    liftIO (readAllEventsIO store)

  commit request = do
    store <- asks envStore
    now <- currentTime
    liftIO (commitIO store now request)

instance MonadReadModel App where
  putView view = do
    readModel <- asks envReadModel
    liftIO (putViewIO readModel view)

  getView txnId = do
    readModel <- asks envReadModel
    liftIO (getViewIO readModel txnId)

  getViewByRrn rrn = do
    readModel <- asks envReadModel
    liftIO (getViewByRrnIO readModel rrn)

  queryViews q = do
    readModel <- asks envReadModel
    liftIO (queryViewsIO readModel q)

  replaceAllViews views = do
    readModel <- asks envReadModel
    liftIO (replaceAllViewsIO readModel views)

-- | Rebuild the read model from the log, returning the number of transactions
-- projected.
--
-- Called once, before the listener is bound. A 'Left' here means the log contains
-- a stream this binary cannot fold, and the only honest response is to refuse to
-- start: serving a projection with a known hole in it would make the simulator
-- lie about state, which is the one thing it must never do. The error is rendered
-- to 'Text' rather than propagated, because @Main@'s job is to print it and exit,
-- not to interpret it.
bootstrap :: Env -> IO (Either Text Int)
bootstrap env = first renderServiceError <$> runApp env rebuildReadModel

-- | Facts about the running process that the API layer reports but must not
-- discover for itself.
--
-- This record exists so that "UPIMock.API.Handlers" can serve @\/health@ without
-- importing "UPIMock.Infrastructure.Database": the handler asks 'Env' a question
-- and gets an answer whose type says nothing about SQLite. When Phase 2 swaps the
-- store, @diagJournalMode@ becomes something like @"postgres 16"@ and the health
-- handler does not change.
data Diagnostics = Diagnostics
  { diagVersion :: Text
  -- ^ The binary's own version, so that a client reading @\/health@ and an
  -- operator reading the startup banner cannot be looking at different numbers.
  , diagSchemaVersion :: Int
  -- ^ Migration level the binary requires and the open database satisfies.
  , diagJournalMode :: Text
  -- ^ As reported by the store at open time, not as requested. A database on a
  -- filesystem that cannot support WAL silently stays in @delete@ mode, and an
  -- operator debugging a concurrency stall needs to see which of the two is true.
  , diagProjectionSize :: Int
  -- ^ Transactions currently in the read model. Non-zero after a restart is the
  -- observable proof that 'bootstrap' replayed the log.
  }
  deriving stock (Eq, Show)

diagnostics :: Env -> IO Diagnostics
diagnostics env = do
  projected <- readModelCount (envReadModel env)
  pure
    Diagnostics
      { diagVersion = T.pack (showVersion version)
      , diagSchemaVersion = schemaVersion
      , diagJournalMode = sqliteJournalMode (envStore env)
      , diagProjectionSize = projected
      }
