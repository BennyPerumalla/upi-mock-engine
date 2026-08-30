# 4. SQLite with one writer and a reader pool

- **Status:** accepted
- **Date:** 2026-02-10
- **Deciders:** maintainers
- **Supersedes:** —
- **Superseded by:** — (Phase 2 adds a PostgreSQL adapter beside this one; see ADR 0003)

## Context

Phase 1 must be *locally deployable*: cloned, built, run, with no service to
provision. That rules out PostgreSQL as the Phase-1 store and makes SQLite the
default. But the store has to hold an append-only log under concurrent HTTP load with
per-stream optimistic concurrency, and SQLite's writer semantics are a single writer
per database whatever the client library pretends.

`SQLITE_BUSY` under contention is the failure this decision exists to prevent. It is
not a correctness bug — the `UNIQUE` constraint still holds — but a simulator that
intermittently returns 500 under load teaches its user nothing about their integration.

## Decision

Writes serialise through an `MVar` holding one dedicated connection. Reads go through
a `Data.Pool` of `--readers` connections (default 4) in WAL mode, so readers never
block the writer and the writer never blocks readers.

`:memory:` is the exception: each connection to `:memory:` is a distinct database, so
pooling it would produce readers that see nothing. For `:memory:` the reader pool is
bypassed and reads take the writer connection; `--readers` is ignored and the flag
table in `README.md` says so.

Pragmas are split by scope. Database-level pragmas (`journal_mode=WAL`,
`synchronous=NORMAL`) are applied once at open; connection-level pragmas
(`busy_timeout=5000`, `foreign_keys=ON`, `temp_store=MEMORY`) are applied to every
connection the pool creates, because they do not persist in the file.

Both `journal_mode` and `busy_timeout` **return a row on assignment**, so they must be
issued with `query_` and not `execute_`. `sqlite-simple` raises on a statement that
yields rows to `execute_`, and the resulting error names neither the pragma nor the
cause.

## Consequences

Write throughput is one transaction at a time by construction. For a simulator this is
correct rather than merely tolerable: the interesting concurrency is *clients racing
each other for the same aggregate*, which is exactly what the `UNIQUE (stream_id,
stream_version)` conflict is there to expose, and serialising the writer makes that
conflict deterministic instead of intermittent.

WAL is requested but not guaranteed. It is unavailable on some network filesystems and
SQLite falls back silently, so the startup banner reports the mode the database
*adopted*, and `/health` exposes it. An operator debugging serialised writes needs to
read `delete` there rather than infer it from a stall.

Tables are not declared `STRICT`. That would require SQLite 3.37+, which is newer than
what several supported distributions ship, and this is the one place where "locally
deployable on the machine you have" beat a stronger guarantee. Column affinities are
declared conventionally and the adapter is the only writer.

The reader pool is a Phase-1 fixture, not a design commitment. The PostgreSQL adapter
replaces both halves — its pool is `hasql-pool` and its writer is not exclusive —
which is fine, because `MonadEventStore`'s laws say nothing about how serialisation is
achieved.

## Alternatives considered

**One connection for everything.** Simplest, and correct. Rejected because
`GET /v1/transactions` would queue behind an unrelated write, and the read model exists
precisely so reads are cheap.

**A connection pool for writes as well.** This is the default shape of most SQLite
wrappers and it is the source of most `SQLITE_BUSY` reports. `busy_timeout` converts
the error into latency rather than removing it.

**PostgreSQL in a container from the start.** Kills local deployability, which is the
Phase-1 requirement. It arrives in Phase 2 as an additional adapter, not a replacement
for this one.

**`STRICT` tables.** Wanted; deferred on the toolchain-availability grounds above.
Revisit when 3.37 is a safe floor.
