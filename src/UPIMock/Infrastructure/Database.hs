-- |
-- Module      : UPIMock.Infrastructure.Database
-- Description : SQLite adapter for the event store. The only module that knows
--               SQL exists.
--
-- This module exports plain @IO@ functions over an opaque 'SqliteHandle', not
-- instances. The 'UPIMock.Application.Ports.MonadEventStore' instance lives in
-- "UPIMock.App", where the @App@ monad is defined, so that neither instance is an
-- orphan and neither this module nor the ports module has to know about the
-- other's monad. Replacing SQLite with hasql in Phase 2 means writing a sibling
-- of this module and changing one line of "UPIMock.App".
--
-- __Concurrency model.__ One writer, many readers.
--
--   * Writes are serialised through an 'MVar' holding a dedicated connection.
--     SQLite permits exactly one writer per database anyway; making that explicit
--     turns @SQLITE_BUSY@ from a race into a queue, and — the part that matters
--     for correctness — it means the read-then-insert inside 'commitIO' is
--     atomic with respect to every other write this process makes. That is why
--     'commitIO' can pre-check the stream version and the RRN claim and treat
--     the answers as authoritative, rather than parsing constraint-violation
--     messages after the fact. The @UNIQUE@ constraints remain as a backstop for
--     a second process writing the same file, which is not a supported
--     configuration and surfaces as 'StoreUnavailable'.
--   * Reads go through a 'Pool' of separate connections in WAL mode, so a slow
--     projection scan never blocks a commit.
--   * An in-memory database is the exception: a pool of @:memory:@ connections
--     would be a pool of unrelated empty databases, so for that path reads share
--     the writer connection and everything serialises. Correct, and slow in a way
--     nobody will notice on a database that vanishes at exit.
module UPIMock.Infrastructure.Database
  ( -- * Handle
    SqliteHandle
  , sqliteJournalMode
  , openSqlite
  , closeSqlite
  , withSqlite

    -- * Store operations
  , readStreamIO
  , readAllEventsIO
  , commitIO
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (Exception, bracket, catch, throwIO)
import Control.Monad (forM_, when)
import Data.Aeson (ToJSON, eitherDecodeStrict, encode)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Database.SQLite.Simple
  ( Connection
  , FromRow (..)
  , Only (..)
  , Query (..)
  , SQLError
  , close
  , execute
  , field
  , lastInsertRowId
  , open
  , query
  , query_
  , withTransaction
  )

import UPIMock.Application.Ports
  ( CommitRequest (..)
  , CommitResult (..)
  , OutboxDraft (..)
  , RrnClaim (..)
  , StoreError (..)
  )
import UPIMock.Domain.Events
  ( PayloadVersion (..)
  , PendingEvent (..)
  , StoredEvent (..)
  , aggregateTypeText
  , currentPayloadVersion
  , eventTypeText
  , parseAggregateType
  )
import UPIMock.Domain.Types
  ( EventId (..)
  , StreamId (..)
  , StreamVersion (..)
  , TxnId (..)
  , noStreamVersion
  , nextVersion
  , rrnText
  )
import UPIMock.Infrastructure.Schema
  ( applyConnectionPragmas
  , applyDatabasePragmas
  , applyMigrations
  )

-- | An open database. Opaque: the 'MVar' /is/ the write lock, and handing it out
-- would let a caller take a connection out of the discipline that makes
-- 'commitIO' atomic.
data SqliteHandle = SqliteHandle
  { sqliteWriter :: MVar Connection
  , sqliteReaders :: Readers
  , sqliteJournalMode :: Text
  -- ^ The journal mode the database actually adopted, for the boot log. WAL is
  -- requested but not guaranteed; see
  -- 'UPIMock.Infrastructure.Schema.applyDatabasePragmas'.
  }

-- | Where reads come from. See the module header for why @:memory:@ cannot use a
-- pool.
data Readers
  = PooledReaders (Pool Connection)
  | SharedWriter

-- | Open a database, bring its schema current, and prepare the reader pool.
--
-- The writer connection is migrated before the pool is created, so no reader can
-- observe a half-migrated schema. Both the write connection and every pooled
-- read connection get 'applyConnectionPragmas', because those pragmas are
-- per-connection state and a pooled connection that misses them would silently
-- run without a busy timeout.
openSqlite :: FilePath -> Int -> IO SqliteHandle
openSqlite path readerCount = do
  writer <- open path
  applyConnectionPragmas writer
  mode <- applyDatabasePragmas writer
  _ <- applyMigrations writer
  writerVar <- newMVar writer
  readers <-
    if isInMemory path || readerCount < 1
      then pure SharedWriter
      else
        PooledReaders
          <$> newPool (defaultPoolConfig (openReader path) close idleSeconds readerCount)
  pure
    SqliteHandle
      { sqliteWriter = writerVar
      , sqliteReaders = readers
      , sqliteJournalMode = mode
      }
  where
    idleSeconds = 60
    openReader p = do
      conn <- open p
      applyConnectionPragmas conn
      pure conn

-- | Both spellings SQLite understands for a private, process-local database.
isInMemory :: FilePath -> Bool
isInMemory path = path == ":memory:" || "file::memory:" `isPrefixOf` path

-- | Idempotent enough to be safe in a @bracket@ cleanup.
closeSqlite :: SqliteHandle -> IO ()
closeSqlite handle = do
  case sqliteReaders handle of
    PooledReaders pool -> destroyAllResources pool
    SharedWriter -> pure ()
  withMVar (sqliteWriter handle) close

withSqlite :: FilePath -> Int -> (SqliteHandle -> IO a) -> IO a
withSqlite path readerCount = bracket (openSqlite path readerCount) closeSqlite

withReadConnection :: SqliteHandle -> (Connection -> IO a) -> IO a
withReadConnection handle action = case sqliteReaders handle of
  PooledReaders pool -> withResource pool action
  SharedWriter -> withMVar (sqliteWriter handle) action

-- | The columns of @events@ as they come off the wire, before validation.
--
-- @event_type@ is deliberately absent. It exists for operators and for the
-- Phase-2 outbox dispatcher to filter on; if decoding consulted it, the column
-- would become part of the log's semantics and a drift between it and the JSON
-- tag would turn from a cosmetic bug into a corrupt read. The payload is
-- self-describing, so the payload is what is decoded.
data EventRow = EventRow
  { rowEventId :: Int64
  , rowStreamId :: Text
  , rowAggregateType :: Text
  , rowStreamVersion :: Int64
  , rowPayload :: Text
  , rowPayloadVersion :: Int
  , rowOccurredAt :: UTCTime
  , rowRecordedAt :: UTCTime
  }

instance FromRow EventRow where
  fromRow =
    EventRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

-- | Column order here is the order 'FromRow' expects; the two are edited
-- together or not at all, which is why the list is spelled out once and shared by
-- both readers. Held as 'Text' and wrapped in 'Query' at each use site, so the
-- adapter depends on nothing but the newtype.
eventColumns :: Text
eventColumns =
  "event_id, stream_id, aggregate_type, stream_version,\
  \ payload, payload_version, occurred_at, recorded_at"

selectStreamSql :: Query
selectStreamSql =
  Query
    ( "SELECT "
        <> eventColumns
        <> " FROM events WHERE stream_id = ? ORDER BY stream_version ASC"
    )

selectAllSql :: Query
selectAllSql = Query ("SELECT " <> eventColumns <> " FROM events ORDER BY event_id ASC")

-- | A row this binary cannot interpret is 'CorruptPayload', never a partial
-- result: a projection built from a stream with a hole in it is worse than no
-- projection, so 'traverse' propagates the first failure and the caller (boot, or
-- an HTTP handler) decides what to do about it.
decodeEventRow :: EventRow -> Either StoreError StoredEvent
decodeEventRow row = do
  aggregate <-
    maybe (Left (corrupt ("unknown aggregate type: " <> rowAggregateType row))) Right $
      parseAggregateType (rowAggregateType row)
  event <- first (corrupt . T.pack) (eitherDecodeStrict (TE.encodeUtf8 (rowPayload row)))
  pure
    StoredEvent
      { storedEventId = EventId (rowEventId row)
      , storedStreamId = stream
      , storedAggregateType = aggregate
      , storedVersion = version
      , storedEvent = event
      , storedOccurredAt = rowOccurredAt row
      , storedRecordedAt = rowRecordedAt row
      , storedPayloadVersion = PayloadVersion (rowPayloadVersion row)
      }
  where
    stream = StreamId (rowStreamId row)
    version = StreamVersion (rowStreamVersion row)
    corrupt = CorruptPayload stream version

-- | One stream, ascending. Ordering is by @stream_version@ rather than by
-- @event_id@ because it is @stream_version@ that
-- 'UPIMock.Engine.StateMachine.replay' asserts contiguity over.
readStreamIO :: SqliteHandle -> StreamId -> IO (Either StoreError [StoredEvent])
readStreamIO handle stream =
  guarded $
    withReadConnection handle $ \conn -> do
      rows <- query conn selectStreamSql (Only (unStreamId stream))
      pure (traverse decodeEventRow rows)

-- | The whole log, ascending by the global offset. Boot-time rebuild only. It
-- loads the log into memory, which is the honest Phase-1 tradeoff: the read model
-- is a @TVar@ of maps and is bounded by the same thing.
readAllEventsIO :: SqliteHandle -> IO (Either StoreError [StoredEvent])
readAllEventsIO handle =
  guarded $
    withReadConnection handle $ \conn -> do
      rows <- query_ conn selectAllSql
      pure (traverse decodeEventRow rows)

-- | Turn the one exception class sqlite-simple raises for operational faults into
-- the port's error value, so that 'UPIMock.Application.Ports.MonadEventStore'\'s
-- totality law holds at the adapter boundary rather than being a hope.
--
-- Deliberately narrow: only 'SQLError' is caught. Catching 'Control.Exception.SomeException'
-- here would swallow asynchronous exceptions and make the process unkillable
-- mid-query, and a programming error in this module should crash loudly rather
-- than be reported to a client as a database problem.
guarded :: IO (Either StoreError a) -> IO (Either StoreError a)
guarded action = action `catch` (pure . Left . sqlFailure)

sqlFailure :: SQLError -> StoreError
sqlFailure = StoreUnavailable . T.pack . show

-- | Append a batch atomically.
--
-- @recordedAt@ is a parameter, not a @getCurrentTime@ call. The adapter has no
-- clock: "UPIMock.Application.Ports"' 'UPIMock.Application.Ports.MonadClock' is
-- the only clock in the system, and an adapter that quietly acquired a second one
-- would put a hole in Phase 2's injected clock skew. @occurred_at@ comes from the
-- domain event, @recorded_at@ from the caller, and the difference between them is
-- exactly the information a chaos run needs.
--
-- The version check, the RRN claims, the event inserts and the outbox rows are
-- one transaction on the writer connection. Rejections travel as a private
-- exception ('CommitAborted') rather than an 'Either' because 'withTransaction'
-- decides between @COMMIT@ and @ROLLBACK@ on whether its action threw: returning
-- @Left@ from inside it would commit a partial write. The exception is caught one
-- layer out and converted back to a value, so nothing escapes.
commitIO :: SqliteHandle -> UTCTime -> CommitRequest -> IO (Either StoreError CommitResult)
commitIO handle recordedAt request =
  guarded $
    withMVar (sqliteWriter handle) $ \conn ->
      (Right <$> withTransaction conn (runCommit conn recordedAt request))
        `catch` (\(CommitAborted reason) -> pure (Left reason))

-- | Not exported, and not part of any signature outside this module: a rollback
-- signal, not an error type.
newtype CommitAborted = CommitAborted StoreError
  deriving stock (Show)

instance Exception CommitAborted

abort :: StoreError -> IO a
abort = throwIO . CommitAborted

runCommit :: Connection -> UTCTime -> CommitRequest -> IO CommitResult
runCommit conn recordedAt request = do
  actual <- currentVersion conn stream
  when (actual /= expected) (abort (VersionConflict stream expected actual))
  forM_ (commitRrnClaims request) (insertRrnClaim conn recordedAt)
  stored <- traverse (insertEvent conn recordedAt request) numbered
  forM_ (commitOutbox request) (insertOutbox conn)
  pure
    CommitResult
      { committedVersion = storedVersion (NE.last stored)
      , committedEvents = stored
      }
  where
    stream = commitStreamId request
    expected = commitExpectedVersion request

    -- Versions are assigned here, not by the caller: the store owns the
    -- numbering, and the pre-checked @expected@ is what makes it safe to compute
    -- them without a round trip per event.
    numbered :: NonEmpty (StreamVersion, PendingEvent)
    numbered =
      NE.zip
        (NE.iterate nextVersion (nextVersion expected))
        (commitEvents request)

-- | @MAX@ over an empty selection is one row holding @NULL@, which is precisely
-- 'noStreamVersion' — so a stream that does not exist and a stream at version
-- zero are the same answer, as an event-sourced store requires.
currentVersion :: Connection -> StreamId -> IO StreamVersion
currentVersion conn stream = do
  rows :: [Only (Maybe Int64)] <-
    query
      conn
      "SELECT MAX(stream_version) FROM events WHERE stream_id = ?"
      (Only (unStreamId stream))
  pure $ case rows of
    Only (Just v) : _ -> StreamVersion v
    _ -> noStreamVersion

-- | Design document §8.2.3 — RRN uniqueness, NPCI @DF@ — as a pre-check inside
-- the write transaction.
--
-- The @SELECT@ is authoritative because this process has exactly one writer and
-- we are holding it. The @PRIMARY KEY@ on @rrn_index@ still guards the file
-- against an unsupported second process, but reaching it would mean an
-- 'SQLError', not a 'DuplicateRrn' — and the distinction is honest: one is a
-- business rule, the other is a broken deployment.
insertRrnClaim :: Connection -> UTCTime -> RrnClaim -> IO ()
insertRrnClaim conn claimedAt claim = do
  existing <-
    query conn "SELECT txn_id FROM rrn_index WHERE rrn = ?" (Only (rrnText (claimRrn claim))) ::
      IO [Only Text]
  case existing of
    [] ->
      execute
        conn
        "INSERT INTO rrn_index (rrn, txn_id, claimed_at) VALUES (?, ?, ?)"
        ( rrnText (claimRrn claim)
        , UUID.toText (unTxnId (claimTxnId claim))
        , claimedAt
        )
    _ -> abort (DuplicateRrn (claimRrn claim))

-- | @lastInsertRowId@ is read immediately after the insert on the same
-- connection, inside the writer lock, so it cannot report another statement's
-- row.
insertEvent ::
  Connection ->
  UTCTime ->
  CommitRequest ->
  (StreamVersion, PendingEvent) ->
  IO StoredEvent
insertEvent conn recordedAt request (version, pending) = do
  execute
    conn
    "INSERT INTO events\
    \ (stream_id, aggregate_type, stream_version, event_type,\
    \  payload, payload_version, occurred_at, recorded_at)\
    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    ( unStreamId (commitStreamId request)
    , aggregateTypeText (commitAggregateType request)
    , unStreamVersion version
    , eventTypeText event
    , encodeJson event
    , unPayloadVersion currentPayloadVersion
    , pendingOccurredAt pending
    , recordedAt
    )
  eventId <- lastInsertRowId conn
  pure
    StoredEvent
      { storedEventId = EventId eventId
      , storedStreamId = commitStreamId request
      , storedAggregateType = commitAggregateType request
      , storedVersion = version
      , storedEvent = event
      , storedOccurredAt = pendingOccurredAt pending
      , storedRecordedAt = recordedAt
      , storedPayloadVersion = currentPayloadVersion
      }
  where
    event = pendingEvent pending

-- | @status@ and @attempts@ take their column defaults, so the shape of a
-- freshly written outbox row is stated once, in the DDL.
insertOutbox :: Connection -> OutboxDraft -> IO ()
insertOutbox conn draft =
  execute
    conn
    "INSERT INTO outbox (topic, payload, occurred_at) VALUES (?, ?, ?)"
    (outboxTopic draft, encodeJson (outboxPayload draft), outboxOccurredAt draft)

-- | Payloads are stored as @TEXT@, not @BLOB@, so that @sqlite3@ on the command
-- line and @json_extract@ both work on a log captured from a failing run.
-- 'TE.decodeUtf8Lenient' cannot fail; aeson's output is valid UTF-8 by
-- construction, so leniency here is a totality device, not a guess.
encodeJson :: ToJSON a => a -> Text
encodeJson = TE.decodeUtf8Lenient . BL.toStrict . encode
