# 1. The event log is the system of record

- **Status:** accepted
- **Date:** 2026-02-03
- **Deciders:** maintainers
- **Supersedes:** —
- **Superseded by:** —

## Context

The simulator's value is that a failing integration test can be replayed. That
requires the sequence of state changes to be recoverable, not just the final state —
"the transaction is `SUCCESS`" is not a reproduction; "`INITIATED`, `AUTHORIZED`,
`TIMED_OUT`, `RECONCILED` with these codes at these instants" is.

A mutable `transactions` row cannot answer *how did it get here*, and the awkward
paths this engine exists to provoke (§8.2 of the design document: a timeout resolved
by a later credit, a decline reversed by reconciliation) are defined by their history
rather than their outcome. UPI reconciliation itself works this way: TTUM and UDIR
are ledger corrections applied on top of a record of what was previously believed,
not edits to it.

## Decision

`events` is append-only and authoritative. Every state change is an append of one or
more rows inside a single SQLite transaction, with per-stream optimistic concurrency
enforced by `UNIQUE (stream_id, stream_version)` rather than by application logic.

The aggregate is folded from the log on every write — `loadAggregate` calls
`readStream` then `replay`, never the projection. The read model is a projection held
in an STM `TVar`, rebuilt from the log at boot by `rebuildReadModel`. `event_id` is
`INTEGER PRIMARY KEY AUTOINCREMENT` so it is monotonic and never reused, which makes
it usable as a cursor. `occurred_at` (domain time, supplied by the caller) and
`recorded_at` (wall clock at insert) are separate columns.

## Consequences

Replay is exact rather than approximate: `replay` supplies `occurredAt` from the
stored row, so a projection rebuilt at boot is identical to the one the write path
produced, not merely equivalent. `GET /v1/transactions/{txnId}/events` is a first-class
endpoint serving the stored encoding verbatim.

`replay` re-checks that stream versions are contiguous instead of trusting
`ORDER BY stream_version`. A gap means a write was lost, and an aggregate folded over
a gap is a plausible-looking lie — the most dangerous output this system could
produce. It returns `ReplayError` instead.

The costs: every write pays a read of the full stream, which is bounded only because
UPI transactions have short lives; the stored JSON is a compatibility surface, which
is why `payload_version` exists and why `CONTRIBUTING.md § The compatibility contract`
treats it as covered by SemVer; and boot time grows with the log. Boot failure is fatal
by design — the process exits before binding the port rather than serving a projection
with a hole in it.

Reversing this means giving up replay, which is the product.

## Alternatives considered

**Mutable state table with an audit trail.** The audit trail is then decorative: it
can disagree with the row it describes and nothing detects it. The failure mode is
silent and appears exactly when the history is needed.

**Event log with a snapshot per aggregate.** A performance answer to a problem this
workload does not have, and snapshots introduce a second thing that can be stale.
Available later without changing the ports.

**Rebuild the projection lazily on read.** Makes the first read after boot
unpredictable in latency and hides a corrupt log until a client happens to ask.
Failing at boot is the more honest schedule.
