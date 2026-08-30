-- |
-- Module      : Main
-- Description : Process lifecycle. The only module allowed to exit.
--
-- Four responsibilities, in order: parse the command line, acquire the store,
-- replay the log, bind the listener. Everything else is library code, which is
-- what makes the simulator embeddable — a Phase-2 scenario runner will import
-- "UPIMock.App" and "UPIMock.API.Handlers" and skip this file entirely.
--
-- __Boot refuses rather than degrades.__ If the replay in 'bootstrap' fails, the
-- log contains a stream this binary cannot fold, and the process exits non-zero
-- before the port is bound. A simulator that started anyway would serve a
-- projection with a known hole in it, and every test written against it would be
-- measuring the hole.
module Main (main) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Network.Wai (Application)
import Network.Wai.Handler.Warp
  ( Settings
  , defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setPort
  )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Options.Applicative (execParser)
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

import UPIMock.API.Handlers (application)
import UPIMock.API.Routes (openApiSpec)
import UPIMock.App (Diagnostics (..), Env, bootstrap, diagnostics, withEnv)
import UPIMock.Config (AppConfig (..), Invocation (..), invocationInfo)

main :: IO ()
main =
  execParser invocationInfo >>= \case
    DumpOpenApi destination -> dumpOpenApi destination
    Serve config -> serveWith config

-- | Emit the derived document and exit.
--
-- Compact, not pretty-printed: the consumer of this file is a code generator or
-- @jq@, and shipping an @aeson-pretty@ dependency to insert whitespace that
-- @jq .@ inserts for free is not a trade worth making.
dumpOpenApi :: Maybe FilePath -> IO ()
dumpOpenApi = \case
  Nothing -> BL.putStr document >> BL.putStr "\n"
  Just path -> BL.writeFile path document
  where
    document = encode openApiSpec

-- | Acquire, replay, announce, serve.
--
-- 'withEnv' brackets the store, so an exception from warp closes the connection
-- pool and the writer on the way out. Line buffering is set explicitly because the
-- interesting case is @docker logs@ against a pipe, where GHC's default block
-- buffering would withhold the banner until the buffer filled — an operator would
-- see silence and conclude the boot had hung.
serveWith :: AppConfig -> IO ()
serveWith config = do
  hSetBuffering stdout LineBuffering
  withEnv config $ \env ->
    bootstrap env >>= \case
      Left reason -> die ("upi-mock-engine: cannot replay the event log: " <> T.unpack reason)
      Right projected -> do
        announce config env projected
        runSettings (warpSettings config) (middleware config (application env))

-- | The startup banner: the facts that decide whether a later surprise is a bug
-- or a configuration mistake.
--
-- @journal mode@ is what the database reported, not what was asked for. WAL is
-- unavailable on some network filesystems and SQLite falls back silently; an
-- operator debugging serialised writes needs to see @delete@ here rather than
-- infer it.
announce :: AppConfig -> Env -> Int -> IO ()
announce config env projected = do
  diag <- diagnostics env
  mapM_
    putStrLn
    [ "upi-mock-engine " <> T.unpack (diagVersion diag)
    , "  database        " <> cfgDatabase config
    , "  journal mode    " <> T.unpack (diagJournalMode diag)
    , "  schema version  " <> show (diagSchemaVersion diag)
    , "  read pool       " <> show (cfgReaderPoolSize config)
    , "  replayed        " <> show projected <> " transaction(s) from the log"
    ]

-- | Bind the port and say so once it is bound.
--
-- 'setBeforeMainLoop' runs after the socket is listening, so the line is a
-- promise a client can act on. Printing it before 'runSettings' would make it a
-- guess, and a harness that races the log line would then race a closed port.
warpSettings :: AppConfig -> Settings
warpSettings config =
  setPort port
    . setBeforeMainLoop (putStrLn ("  listening       http://localhost:" <> show port))
    $ defaultSettings
  where
    port = cfgHttpPort config

-- | Request logging, opt-in.
--
-- Off by default because the output of a simulator run that matters is the event
-- log, and an access log interleaved with the banner obscures it. Applied here
-- rather than inside 'application' so that a test can exercise the bare
-- application without capturing stdout.
middleware :: AppConfig -> Application -> Application
middleware config
  | cfgVerbose config = logStdoutDev
  | otherwise = id
