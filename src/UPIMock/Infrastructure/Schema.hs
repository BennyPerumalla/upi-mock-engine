-- |
-- Module      : UPIMock.Infrastructure.Schema
-- Description : DDL, PRAGMA policy, and the forward-only migration runner.
--
-- The schema is versioned in SQLite's own @user_version@ header field rather
-- than in a table, because that keeps migration bookkeeping inside the file being
-- migrated and out of the query surface.
--
-- Migrations are forward-only and each runs in its own transaction. There is no
-- down migration: this is a simulator, its database is disposable, and a
-- reversible-migration framework would be more machinery than the problem has.
--
-- A note on PRAGMA and sqlite-simple, because it is a real trap. Most @PRAGMA@
-- statements return no rows when used to /set/ a value, and can be run with
-- 'execute_'. Two of the ones used here do return a row on assignment —
-- @journal_mode@ (the mode actually adopted) and @busy_timeout@ (the new
-- timeout) — so they are issued through 'query_' and their result is inspected
-- or discarded explicitly.
module UPIMock.Infrastructure.Schema
  ( -- * Versioning
    Migration (..)
  , migrations
  , schemaVersion
  , applyMigrations
  , readSchemaVersion

    -- * Connection setup
  , applyDatabasePragmas
  , applyConnectionPragmas
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple
  ( Connection
  , Only (..)
  , Query (..)
  , execute_
  , query_
  , withTransaction
  )

-- | One forward step of the schema. 'migrationStatements' run in order inside a
-- single transaction with the @user_version@ bump, so a partially applied
-- migration cannot be observed.
data Migration = Migration
  { migrationVersion :: Int
  , migrationName :: Text
  , migrationStatements :: [Query]
  }

-- | The version a freshly migrated database reports. Derived from 'migrations'
-- with a fold rather than @maximum@, which is partial.
schemaVersion :: Int
schemaVersion = foldr (max . migrationVersion) 0 migrations

-- | The Phase-1 schema.
--
-- Three tables, and each one earns its place.
--
--   * @events@ is the system of record. @UNIQUE (stream_id, stream_version)@ is
--     the optimistic-concurrency mechanism: two writers that both believe they
--     are appending version /n/ cannot both succeed, whatever the isolation
--     level. @event_id@ is @AUTOINCREMENT@ so that ids are monotonic and never
--     reused, which is what lets Phase 2 use it as an outbox cursor.
--   * @rrn_index@ makes design document §8.2.3 (RRN uniqueness, NPCI @DF@) a
--     database constraint rather than an application convention. It is a
--     constraint, not a lookup path — resolving an RRN to a transaction is the
--     read model's job.
--   * @outbox@ is written in the same transaction as the events it describes.
--     Phase 1 ships no dispatcher, so rows accumulate with @status = 'PENDING'@;
--     that is the seam, and it is exercised on every write from day one rather
--     than bolted on later.
--
-- Timestamps are stored through @sqlite-simple@'s own @UTCTime@ field
-- instances, so the textual format is whatever that library round-trips; no
-- query orders or compares on a timestamp column, precisely so that the format
-- stays an implementation detail.
--
-- The tables are not declared @STRICT@. That would be stronger, but it requires
-- SQLite 3.37+, and this project must build both against the SQLite that
-- @direct-sqlite@ vendors and against a system library of unknown vintage.
-- @CHECK@ constraints carry the invariants that matter instead.
migrations :: [Migration]
migrations =
  [ Migration
      { migrationVersion = 1
      , migrationName = "event log, RRN uniqueness index, transactional outbox"
      , migrationStatements =
          [ "CREATE TABLE IF NOT EXISTS events (\
            \ event_id        INTEGER PRIMARY KEY AUTOINCREMENT,\
            \ stream_id       TEXT    NOT NULL,\
            \ aggregate_type  TEXT    NOT NULL,\
            \ stream_version  INTEGER NOT NULL CHECK (stream_version > 0),\
            \ event_type      TEXT    NOT NULL,\
            \ payload         TEXT    NOT NULL,\
            \ payload_version INTEGER NOT NULL CHECK (payload_version > 0),\
            \ occurred_at     TEXT    NOT NULL,\
            \ recorded_at     TEXT    NOT NULL,\
            \ UNIQUE (stream_id, stream_version))"
          , "CREATE INDEX IF NOT EXISTS events_type_idx ON events (event_type)"
          , "CREATE TABLE IF NOT EXISTS rrn_index (\
            \ rrn        TEXT PRIMARY KEY CHECK (length(rrn) = 12),\
            \ txn_id     TEXT NOT NULL,\
            \ claimed_at TEXT NOT NULL)"
          , "CREATE TABLE IF NOT EXISTS outbox (\
            \ outbox_id     INTEGER PRIMARY KEY AUTOINCREMENT,\
            \ topic         TEXT    NOT NULL,\
            \ payload       TEXT    NOT NULL,\
            \ occurred_at   TEXT    NOT NULL,\
            \ status        TEXT    NOT NULL DEFAULT 'PENDING'\
            \                       CHECK (status IN ('PENDING', 'SENT', 'FAILED')),\
            \ attempts      INTEGER NOT NULL DEFAULT 0,\
            \ dispatched_at TEXT)"
          , "CREATE INDEX IF NOT EXISTS outbox_dispatch_idx ON outbox (status, outbox_id)"
          ]
      }
  ]

-- | Bring a database up to 'schemaVersion'. Returns the number of migrations
-- applied, which is zero for a file that is already current. Idempotent, so the
-- boot path can call it unconditionally.
applyMigrations :: Connection -> IO Int
applyMigrations conn = do
  current <- readSchemaVersion conn
  let pending = filter ((> current) . migrationVersion) migrations
  forM_ pending $ \migration ->
    withTransaction conn $ do
      mapM_ (execute_ conn) (migrationStatements migration)
      execute_ conn (setUserVersion (migrationVersion migration))
  pure (length pending)

-- | @user_version@ is @0@ in a database this project has never touched, which is
-- exactly the answer we want for a new file.
readSchemaVersion :: Connection -> IO Int
readSchemaVersion conn = do
  rows <- query_ conn "PRAGMA user_version" :: IO [Only Int]
  pure $ case rows of
    Only v : _ -> v
    [] -> 0

-- | @PRAGMA@ accepts no bind parameters, so the version is spliced into the
-- statement text. The value comes from 'migrations' in this module and never
-- from input.
setUserVersion :: Int -> Query
setUserVersion v = Query ("PRAGMA user_version = " <> T.pack (show v))

-- | Pragmas that are properties of the database file and persist inside it.
--
-- Returns the journal mode actually adopted. WAL is a request, not a guarantee:
-- on a filesystem that cannot support shared memory (some network mounts)
-- SQLite keeps the previous mode and reports it, and the boot path logs that
-- rather than assuming concurrent readers are safe.
applyDatabasePragmas :: Connection -> IO Text
applyDatabasePragmas conn = do
  modes <- query_ conn "PRAGMA journal_mode = WAL" :: IO [Only Text]
  execute_ conn "PRAGMA synchronous = NORMAL"
  pure $ case modes of
    Only mode : _ -> mode
    [] -> "unknown"

-- | Pragmas that are properties of a /connection/, and therefore must be set on
-- every connection the reader pool opens, not once at boot.
applyConnectionPragmas :: Connection -> IO ()
applyConnectionPragmas conn = do
  _ <- query_ conn "PRAGMA busy_timeout = 5000" :: IO [Only Int]
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA temp_store = MEMORY"
