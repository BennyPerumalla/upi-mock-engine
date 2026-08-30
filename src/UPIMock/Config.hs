-- |
-- Module      : UPIMock.Config
-- Description : Runtime configuration and the command-line surface.
--
-- Configuration is a value, parsed once, at the edge. Nothing below
-- "UPIMock.App" reads it, and nothing anywhere reads the environment at a random
-- moment during a request — a simulator whose behaviour depends on ambient state
-- is a simulator whose failing run cannot be reproduced from its command line.
--
-- Defaults are chosen so that @upi-mock-engine@ with no arguments is a useful
-- program: an on-disk database in the working directory, so a scenario survives a
-- restart and can be inspected with @sqlite3@ afterwards.
module UPIMock.Config
  ( -- * Configuration
    AppConfig (..)
  , defaultConfig

    -- * Command line
  , Invocation (..)
  , invocationInfo
  ) where

import Control.Applicative ((<|>))
import Data.Version (showVersion)
import Options.Applicative
  ( Parser
  , ParserInfo
  , auto
  , command
  , footer
  , fullDesc
  , header
  , help
  , helper
  , hsubparser
  , info
  , infoOption
  , long
  , metavar
  , option
  , optional
  , progDesc
  , short
  , showDefault
  , strOption
  , switch
  , value
  , (<**>)
  )

import Paths_upi_mock_engine (version)

-- | Everything the process needs to know about the world it runs in.
--
-- Phase 2 grows this record — @cfgSeed@ for a deterministic PRNG, @cfgChaos@ for
-- a failure-injection profile, @cfgPostgres@ for the hasql connection string.
-- Adding a field with a default leaves every existing call site compiling, which
-- is the whole reason this is a record and not a positional argument list.
data AppConfig = AppConfig
  { cfgDatabase :: FilePath
  -- ^ SQLite file, or @:memory:@ for a database that vanishes at exit.
  , cfgHttpPort :: Int
  , cfgReaderPoolSize :: Int
  -- ^ Concurrent read connections. Ignored for an in-memory database, which
  -- cannot be pooled; see "UPIMock.Infrastructure.Database".
  , cfgVerbose :: Bool
  -- ^ Log every request. Off by default because the interesting output of a
  -- simulator run is the event log, not the access log.
  }
  deriving stock (Eq, Show)

defaultConfig :: AppConfig
defaultConfig =
  AppConfig
    { cfgDatabase = "upi-mock.db"
    , cfgHttpPort = 8080
    , cfgReaderPoolSize = 4
    , cfgVerbose = False
    }

-- | What the operator asked for. Named 'Invocation' rather than @Command@ because
-- 'UPIMock.Engine.StateMachine.Command' already means something in this codebase,
-- and two things called @Command@ in one program is one too many.
data Invocation
  = Serve AppConfig
  | -- | Write the OpenAPI document and exit. 'Nothing' is stdout, so
    -- @upi-mock-engine openapi | jq@ works.
    DumpOpenApi (Maybe FilePath)
  deriving stock (Eq, Show)

invocationInfo :: ParserInfo Invocation
invocationInfo =
  info
    (invocationParser <**> helper <**> versionOption)
    ( fullDesc
        <> header "upi-mock-engine — a deterministic UPI switch simulator"
        <> progDesc
          "Serves an HTTP façade over an event-sourced UPI transaction engine. \
          \State transitions are checked at compile time; the event log is the \
          \system of record and the read model is rebuilt from it at boot."
        <> footer
          "The OpenAPI document is also served at GET /openapi.json while running."
    )

-- | A bare invocation serves with defaults. The @serve@ subcommand exists so the
-- flags have somewhere to be documented, and the fallback exists so the common
-- case needs no subcommand.
invocationParser :: Parser Invocation
invocationParser =
  hsubparser
    ( command
        "serve"
        (info (Serve <$> configParser) (progDesc "Run the HTTP server (default)"))
        <> command
          "openapi"
          (info (DumpOpenApi <$> outputParser) (progDesc "Emit the OpenAPI 3 document"))
    )
    <|> (Serve <$> configParser)

configParser :: Parser AppConfig
configParser =
  AppConfig
    <$> strOption
      ( long "database"
          <> short 'd'
          <> metavar "PATH"
          <> value (cfgDatabase defaultConfig)
          <> showDefault
          <> help "SQLite database file, or :memory: for an ephemeral run"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value (cfgHttpPort defaultConfig)
          <> showDefault
          <> help "TCP port to bind"
      )
    <*> option
      auto
      ( long "readers"
          <> metavar "N"
          <> value (cfgReaderPoolSize defaultConfig)
          <> showDefault
          <> help "Size of the read-connection pool"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Log every HTTP request"
      )

outputParser :: Parser (Maybe FilePath)
outputParser =
  optional
    ( strOption
        ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Write to FILE instead of stdout"
        )
    )

-- | 'infoOption' rather than the newer @simpleVersioner@, because this needs to
-- work against whatever @optparse-applicative@ the resolver picks.
versionOption :: Parser (a -> a)
versionOption =
  infoOption
    (showVersion version)
    (long "version" <> help "Print the version and exit")
