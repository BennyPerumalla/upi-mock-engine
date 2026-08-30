# Architecture

This document is the map a maintainer needs before changing anything: where each
decision lives, which direction dependencies point, and which invariants are
carried by types rather than by tests.

Section references of the form §8.2.1 are to the engineering design document.

## 1. Layering

Seven layers. Dependencies point strictly downward, and every arrow that exists is
listed here.

```
app/Main.hs                    process lifecycle; parse, acquire, replay, bind
        |
UPIMock.API.{Handlers,Routes,Wire}     HTTP: parse, delegate, translate the failure
        |
UPIMock.{App,Config}           ReaderT Env IO; the port instances live here
        |
UPIMock.Infrastructure.{Database,ReadModel,Schema}      SQLite and STM adapters
        |
UPIMock.Application.{Service,Ports}    use cases, and the classes they are written against
        |
UPIMock.Engine.StateMachine            pure decisions: command + aggregate -> proof
        |
UPIMock.Domain.{Transaction,Events,Types,ErrorCodes}    value objects and the GADT
```

The rule that makes this structural rather than aspirational:

- `Domain` imports nothing from this project except `Domain`.
- `Engine` imports `Domain` only.
- `Application.Ports` **declares** effects and imports no `Infrastructure`. This is
  the single import restriction that makes "the domain logic must not know SQLite
  exists" checkable by reading one import list.
- `Application.Service` imports `Ports`, `Engine`, `Domain` — and is polymorphic in
  the monad, so it cannot name an adapter even by accident.
- `Infrastructure` imports `Ports` for its types (`CommitRequest`, `StoreError`)
  and exports plain `IO` functions. It defines **no instances**.
- `App` imports both `Ports` and `Infrastructure`, and is the only module that
  does. Every `MonadEventStore` / `MonadReadModel` / `MonadClock` /
  `MonadEntropy` instance in the library is here.
- `API` imports `App`, `Service`, `Ports` and `Domain`, and never
  `Infrastructure`.

`App` exists precisely because of the orphan-instance rule: the class is in
`Ports`, the implementation is in `Infrastructure`, and an instance in either would
be an orphan. Defining `App` in the same module as its instances is the cheapest
way to keep GHC's orphan warning meaningful.

## 2. The compile-time state machine

`UPIMock.Domain.Transaction` is the whole of it. Four declarations do the work.

```haskell
data TxnState = Initiated | Pending | Success | Failed | Timeout
              | RefundPending | Refunded

newtype Transaction (s :: TxnState) = Transaction { txnCore :: TxnCore }
--      ^ constructor NOT exported

data Transition (from :: TxnState) (to :: TxnState) where
  Authorize            :: AuthRef   -> Transition 'Initiated     'Pending
  DeclineAtPsp         :: ErrorCode -> Transition 'Initiated     'Failed
  MarkSuccess          ::              Transition 'Pending       'Success
  MarkFailed           :: ErrorCode -> Transition 'Pending       'Failed
  MarkTimeout          ::              Transition 'Pending       'Timeout
  Reconcile            :: ReconCode -> Transition 'Timeout       'Success
  DropInReconciliation :: ErrorCode -> Transition 'Timeout       'Failed
  OpenRefund           :: RefundRef -> Transition 'Success       'RefundPending
  SettleRefund         ::              Transition 'RefundPending 'Refunded

applyTransition :: UTCTime -> Transition from to -> Transaction from -> Transaction to
```

`Transaction`'s constructor is not exported, so `newTransaction` (which returns
`Transaction 'Initiated`) and `applyTransition` are the *only* ways to obtain a
value of this type. Everything below follows from that and from the indices above,
with no runtime check anywhere.

### What the indices make unrepresentable

| Invariant | Why it holds |
| --- | --- |
| `FAILED` is absorbing (§8.2.1) | No `Transition` constructor has `'Failed` as its source index. There is no function to call |
| `REFUNDED` is absorbing | Likewise for `'Refunded` |
| Reconciliation cannot lose its TTUM code (§8.2.2) | `Reconcile` *takes* a `ReconCode`. `TIMEOUT -> SUCCESS` without a `102`/`103` record has no term |
| A refund is only opened against a settled payment | `OpenRefund :: … Transition 'Success 'RefundPending` |
| A refund is only settled from `REFUND_PENDING` | `SettleRefund :: Transition 'RefundPending 'Refunded` |
| The state index cannot drift from the data | `TxnCore` is state-*independent* and `Transaction` is a `newtype` over it, so `applyTransition` changes the index by rebuilding the wrapper — no phantom-changing record update, no `unsafeCoerce` |

`SUCCESS` and `TIMEOUT` are deliberately **not** terminal: the first can be
refunded, the second reconciled. `isTerminalState` says so as a total `case`, so
adding a state forces the question to be answered.

`DeclineAtPsp` extends the diagram in §8.1. It models a pre-switch rejection — the
payer declining or mistyping the MPIN on a Collect (`ZA`, `ZM`), or the collect
window expiring. Without it a declined Collect would have to be laundered through
`PENDING`, which would tell the client that the switch forwarded a debit that never
left the PSP.

### Crossing between the typed and untyped worlds

An HTTP request and a database row do not know the state index. Three types carry
it across.

```haskell
data StateSing (s :: TxnState) where { SInitiated :: StateSing 'Initiated ; … }
data AnyTransaction where AnyTransaction :: StateSing s -> Transaction s -> AnyTransaction
data Step (from :: TxnState) where Step :: Transition from to -> Step from
```

`StateSing` is hand-rolled rather than pulled from `singletons`: seven constructors
do not justify the dependency or the Template Haskell.

`AnyTransaction` hides *both* indices and is what the store and the read model deal
in. `Step` hides only the **target**, which is the shape a fold over a log needs:
the source index is what the fold currently knows, and the target is what it is
about to learn.

The entire command-authorisation logic is then one `case` on the pair:

```haskell
decide :: Command -> AnyTransaction -> Either DomainError Decided
decide cmd (AnyTransaction sing txn) = case (cmd, sing) of
  (CmdAuthorize ref, SInitiated) -> Right (Decided (Authorize ref) txn)
  …
  _                              -> Left rejection
```

Each right-hand side typechecks *only* because matching on the singleton refines
the aggregate's index to the transition's source. The fallback can only produce
`Left`, so there is no branch through which an illegal move could be admitted. The
nine-line body is the whole authorisation model; there is no rule engine, no
transition table to keep in sync, and nothing to unit-test for internal
consistency because the compiler has already done it.

`Decided` is an existential pair of a proof and the aggregate it applies to, so
`applyDecided` cannot fail and needs no re-validation.

### The one invariant types cannot carry

RRN uniqueness (§8.2.3) is a statement about the *set* of aggregates, and no
aggregate can enforce it about itself. It is a `UNIQUE` constraint plus a
pre-checked insert inside the write transaction (§5), and a violation surfaces as
`DuplicateRrn` carrying NPCI `DF`. This is the only stated invariant of Phase 1
enforced by the store rather than by the type system, and it is documented in both
places in the source.

## 3. The write path

`UPIMock.Application.Service` is the only place where a decision, a commit and a
projection are sequenced. The order is the contract, and `applyCommand` is the
canonical form:

```
POST /v1/transactions/{txnId}/commands
  |
  |  API.Wire            parseTxnId, parseCommand      untrusted Text -> value objects, or 400
  v
  |  Service             loadAggregate
  |    MonadEventStore   readStream  -----------------> [StoredEvent]
  |    Engine            replay      -----------------> (StreamVersion, AnyTransaction)
  |                                                     re-checks version contiguity
  v
  |  Engine              decide      -----------------> Either DomainError Decided   (409)
  v
  |  MonadClock          currentTime
  |  Engine              applyDecided ----------------> (DomainEvent, AnyTransaction)
  v
  |  MonadEventStore     commit CommitRequest{ expectedVersion = loaded version, … }
  |                                  ----------------> Either StoreError CommitResult (409/503)
  v
  |  Domain              project (committedVersion result) next
  |  MonadReadModel      putView   ------------------->  read model
  v
  201/200 with TxnResource carrying `version`
```

Five properties of that sequence are load-bearing.

**The aggregate is folded from the log, not read from the projection.** The read
model is derived state and may lag; a decision taken against it would be a decision
against a stale state. `readStream` + `replay` on every command is O(stream length)
and a stream is at most nine events long.

**`expectedVersion` is the version the fold ended at.** Two writers that both
loaded version *n* cannot both append *n+1*: the loser gets
`VersionConflict expected actual` and writes nothing. Nothing is retried in the
service — for a simulator, a lost race is often the behaviour under test, so the
caller decides.

**The commit is atomic across three tables.** Events, RRN claims and outbox rows go
in one SQLite transaction, or none of them do.

**The projection happens after the commit is durable, and its failure is
survivable.** A crash between `commit` and `putView` loses nothing, because
`rebuildReadModel` recomputes the view from the log at the next boot. The reverse
order would not be recoverable.

**`putView` must not regress a row.** If the stored view's `viewVersion` is greater
than or equal to the incoming one, the write is dropped. Without that, two commits
that interleave between their append and their projection can leave the read model
showing the older state indefinitely. It is a stated law of `MonadReadModel`, not
an implementation detail of the STM adapter.

`initiateTransaction` differs in three ways. It draws identity, an implicit
`AuthRef` and an RRN word from `MonadEntropy` *before* anything is written, so the
seed is fixed before the commit; it runs `checkSeed` for the cross-field rules
(payer ≠ payee, merchant id required for P2M and forbidden otherwise); and it
commits against `noStreamVersion` (`0`), which is how "this stream must not already
exist" is expressed in the same mechanism as every other concurrency check.

The entropy draws are unconditional. Minting an `AuthRef` that a Collect flow
immediately discards costs one UUID and buys a property worth more: the number of
draws per request does not depend on the request, so a seeded Phase-2 run produces
identical identifiers regardless of flow. Had the draw been conditional, seeding
would have produced runs that are reproducible only per-flow.

`initiate` is where the one flow-dependent rule of Phase 1 lives. Intent and QR are
initiated *by the payer*, who has already authorised in their PSP app, so the
initiation writes `TXN_INITIATED` and `TXN_AUTHORIZED` in one commit and the
aggregate answers `PENDING`. A Collect is initiated by the *payee*, so the aggregate
rests in `INITIATED` until `CmdAuthorize` or `CmdDeclineAtPsp` — which is precisely
the state in which a Collect can expire.

## 4. The read path

```
GET /v1/transactions?state=SUCCESS&flow=P2M_QR&limit=50&offset=0
  |
  |  API.Wire        parseViewQuery      unknown filter -> 400; paging clamped, not refused
  v
  |  MonadReadModel  queryViews          one atomically over one TVar
  v
  |  API.Wire        toTxnResource
  v
  200 TxnPage { items, limit, offset, count }
```

The projection is one `TVar` over a record holding both indices — `Map TxnId
TxnView` and `Map Rrn TxnId` — rather than two `TVar`s. Two would be equally correct
as long as every operation stayed inside a single `atomically`, but that is a
convention a reviewer has to check; one makes a torn read *unrepresentable*, since a
reader takes one snapshot in which both indices agree by construction. The cost is
that unrelated writes conflict and retry, which for a projection updated once per
committed transaction is not worth optimising in Phase 1. `projByRrn` maps to a
`TxnId` rather than to a `TxnView`, so a view exists exactly once in memory and the
indices cannot disagree about its contents.

Queries are applied in memory, ordered most-recently-updated first. `count` is the
size of the returned page, not of the result set: an honest total would require a
second traversal under the same snapshot, and a number that can disagree with
`items` is worse than no number.

Paging is **clamped, not refused** — `limit` into `[1, 200]`, `offset` at `0`. There
is no reading of `limit=0` that a client could have intended, and a `400` would be a
less useful answer than the first page. An unrecognised `state` or `flow`, by
contrast, is a `400`: a typo'd filter that silently matched everything is how a test
suite comes to assert nothing at all while still passing.

`GET …/events` bypasses the projection entirely and serves the stream from the log,
with each payload in its stored encoding, verbatim. Re-encoding it into a friendlier
shape would defeat the endpoint's only purpose, which is to show what was actually
written.

## 5. Persistence

Three tables, in `UPIMock.Infrastructure.Schema`, and each earns its place.

```sql
CREATE TABLE events (
  event_id        INTEGER PRIMARY KEY AUTOINCREMENT,
  stream_id       TEXT    NOT NULL,
  aggregate_type  TEXT    NOT NULL,
  stream_version  INTEGER NOT NULL CHECK (stream_version > 0),
  event_type      TEXT    NOT NULL,
  payload         TEXT    NOT NULL,
  payload_version INTEGER NOT NULL CHECK (payload_version > 0),
  occurred_at     TEXT    NOT NULL,
  recorded_at     TEXT    NOT NULL,
  UNIQUE (stream_id, stream_version));

CREATE TABLE rrn_index (rrn TEXT PRIMARY KEY CHECK (length(rrn) = 12), …);
CREATE TABLE outbox   (outbox_id INTEGER PRIMARY KEY AUTOINCREMENT, topic …,
                       status TEXT CHECK (status IN ('PENDING','SENT','FAILED')), …);
```

`UNIQUE (stream_id, stream_version)` **is** the optimistic-concurrency mechanism:
two writers who both believe they are appending version *n* cannot both succeed,
whatever the isolation level. `event_id` is `AUTOINCREMENT` so ids are monotonic and
never reused, which is what lets Phase 2 use it as an outbox cursor.

`rrn_index` makes §8.2.3 a database constraint rather than an application
convention. It is a constraint, *not* a lookup path — resolving an RRN to a
transaction is the read model's job.

`outbox` is written in the same transaction as the events it describes, with the
event type as the topic so a dispatcher can route without decoding the payload.
Phase 1 ships no dispatcher, so rows accumulate at `status = 'PENDING'`. That is the
seam, and it is exercised on every write from day one rather than bolted on later.

`occurred_at` and `recorded_at` are separate columns because they answer different
questions: when the domain says it happened, and when the store wrote it. No query
orders or compares on a timestamp column, precisely so that the textual format that
`sqlite-simple`'s `UTCTime` instances happen to use stays an implementation detail.

Schema version lives in SQLite's own `user_version` header, not in a table, which
keeps migration bookkeeping inside the file being migrated and out of the query
surface. Migrations are forward-only, one transaction each, applied unconditionally
at boot (`applyMigrations` is idempotent and returns how many it ran). There are no
down migrations: this is a simulator, its database is disposable, and a
reversible-migration framework would be more machinery than the problem has.

The tables are not `STRICT`. That would be stronger, but it requires SQLite 3.37+,
and the project must build both against the SQLite that `direct-sqlite` vendors and
against a system library of unknown vintage. `CHECK` constraints carry the invariants
that matter instead.

### Concurrency: one writer, many readers

SQLite permits exactly one writer per database. Making that explicit turns
`SQLITE_BUSY` from a race into a queue — and, the part that matters for
correctness, it makes the read-then-insert inside `commitIO` atomic with respect to
every other write this process makes.

- **Writes** are serialised through an `MVar` holding a dedicated connection. That
  is why `commitIO` can pre-check the stream version and the RRN claim and treat
  the answers as authoritative, rather than parsing constraint-violation messages
  after the fact. The `UNIQUE` constraints remain as a backstop for a second
  process writing the same file — not a supported configuration, and it surfaces as
  `StoreUnavailable`.
- **Reads** go through a `Pool` of separate connections in WAL mode, so a slow
  projection scan never blocks a commit.
- **`:memory:` is the exception.** A pool of `:memory:` connections would be a pool
  of unrelated empty databases, so that path shares the writer connection and
  everything serialises. Correct, and slow in a way nobody notices on a database
  that vanishes at exit.

`busy_timeout` and `foreign_keys` are connection properties and are therefore set on
every connection the pool opens; `journal_mode` and `synchronous` are file properties
and are set once. A trap worth naming: most `PRAGMA` statements return no rows when
*setting* a value and can go through `execute_`, but `journal_mode` and
`busy_timeout` both return a row on assignment, so they are issued with `query_` and
their result is inspected or discarded explicitly.

## 6. Ports and adapters

Four classes in `UPIMock.Application.Ports`, and three deliberate choices.

**No method throws.** Every failure a caller can act on is a value in `Either`. An
exception escaping an instance is a bug in the adapter, not part of the contract.
`StoreError` has exactly four constructors because the caller must distinguish four
outcomes: retryable-by-reloading, NPCI `DF`, corrupt data, and operational fault.

**Time and entropy are ports.** In Phase 1 they are `getCurrentTime` and a UUID
draw, which looks like ceremony. They are the reason Phase 2's seeded runs and
injected clock skew are a new instance rather than an edit to every call site.

**`CommitRequest` is a record, not an argument list.** Phase 2 adds
latency-injection metadata and mandate claims to the same atomic write; growing a
record leaves every existing construction site compiling.

`MonadEventStore` states five laws — atomicity, optimistic concurrency, ordering,
totality, RRN uniqueness — and they are stated on the class rather than in the
adapter because there are two adapters and both must satisfy them. The SQLite
adapter does so with a serialised writer and `UNIQUE`; the in-memory double in
`test/UPIMock/Support/Sim.hs` does so with `StateT`. That the property tests run
against the double is only meaningful because the laws are written down.

## 7. The boundary

`UPIMock.API.Wire` is a set of total functions from untrusted values to either a
domain value or a **named** refusal. Nothing else in the tree parses text.

The route captures are `Text`, not `TxnId`. A `FromHttpApiData TxnId` instance would
be a second way into the domain that bypasses this module, and it would render a
malformed id as Servant's generic `400` with a message nobody wrote. Parsing in the
handler produces the same error shape as every other rejection.

The boundary performs exactly **one** transformation: VPA case-folding. If it
stopped, `ALICE@PSP` and `alice@psp` would become two payers sharing one RRN index.

Cross-field rules are deliberately *not* enforced here. `parseInitiate` will accept a
body whose payer is also its payee; `checkSeed` in the engine rejects it. That is so
a Phase-2 scenario driver, which never speaks HTTP, meets the same check. The test
for this asserts an *acceptance*, which reads like a gap until you know where the
wall is.

`parseCommand` never invents an operand. `AUTHORIZE` without `authRef` is
`MissingField "authRef"`, not a minted reference — a fabricated authorisation
reference is a forged proof in a log whose entire purpose is to be unforgeable.

## 8. Error taxonomy

Four types, four audiences, and they are kept apart on purpose because a client that
conflates them cannot distinguish a bug in itself from the behaviour under test.

| Type | Means | Where decided |
| --- | --- | --- |
| `ValidationError` | The request to the *simulator* is malformed | `API.Wire`, `Engine.checkSeed` |
| `DomainError` | Well-formed request, illegal for the current state | `Engine.decide` |
| `StoreError` | The write could not be made | `Infrastructure.Database` |
| `ReplayError` | The stored log is inconsistent with itself | `Engine.replay` |
| `ErrorCode` | The simulated *switch* declined the payment | `Domain.ErrorCodes` — **not an HTTP error** |

`ServiceError` wraps the first four plus `ServiceNotFound`, as five constructors
rather than a flat sum, so that the HTTP layer can map them without inspecting a
payload. That map is the only place in the codebase that mentions a status code:

| `ServiceError` | Status | `error` class |
| --- | --- | --- |
| `ServiceInvalid` | 400 | `VALIDATION_FAILED` |
| `ServiceDomain` | 409 | `ILLEGAL_TRANSITION` |
| `ServiceStore VersionConflict{}` | 409 | `VERSION_CONFLICT` |
| `ServiceStore (DuplicateRrn _)` | 409 | `DUPLICATE_RRN`, `npciCode: "DF"` |
| `ServiceStore CorruptPayload{}` | 500 | `CORRUPT_LOG` |
| `ServiceStore (StoreUnavailable _)` | 503 | `STORE_UNAVAILABLE` |
| `ServiceReplay` | 500 | `CORRUPT_LOG` |
| `ServiceNotFound` | 404 | `NOT_FOUND` |

`DuplicateRrn` is 409 rather than 400 because the request was well formed and lost a
race for a globally unique value; it carries `DF` so a harness driving duplicate-RRN
behaviour sees the code the real switch returns instead of inferring it from prose.
`CorruptPayload` is 500 rather than 422 because the client sent nothing wrong.

Servant's `errXXX` values carry an empty body and no content type, which would make
the error path of an all-JSON API un-parseable by a client that always parses JSON.
`jsonError` sets both, so every failure this server emits has one shape:
`ErrorResource { error, message, npciCode? }`.

## 9. Boot

`app/Main.hs` has four responsibilities, in order, and is the only module permitted
to exit.

1. `execParser invocationInfo` — configuration is a value, parsed once, at the edge.
   Nothing below `UPIMock.App` reads it and nothing anywhere reads the environment
   mid-request; a simulator whose behaviour depends on ambient state is one whose
   failing run cannot be reproduced from its command line.
2. `withEnv` — `bracket` the SQLite handle and create an *empty* read model. The
   rebuild is not done here: it can fail, and a resource-acquisition bracket is the
   wrong place to decide whether the log is readable.
3. `bootstrap` — `rebuildReadModel` over the whole log, grouped by stream. **A
   `Left` here exits non-zero before the port is bound.** Starting anyway would
   serve a projection with a known hole, and every test written against it would be
   measuring the hole.
4. `runSettings` — bind, then announce from `beforeMainLoop`.

`Diagnostics` exists so that `/health` can report the version, schema version,
journal mode and projection size without `API.Handlers` importing
`Infrastructure.Database`. The handler asks `Env` a question and gets an answer whose
type says nothing about SQLite; when Phase 2 swaps the store, `diagJournalMode`
becomes something like `"postgres 16"` and the health handler does not change.

`/health` is always 200 while the process is listening. A store that has become
unreachable surfaces as 503 on the endpoint that touched it — a health check that
performs a write to decide its answer is a health check that changes the state it
reports on.

Middleware is applied in `Main`, not inside `application`, so a test can exercise the
bare WAI application without capturing stdout.

## 10. Testing

`hspec` for examples, `hedgehog` for properties, assembled by `hspec-discover`. Nine
spec modules; no `hspec-wai`, and no socket is opened anywhere in the suite. The
HTTP seam that matters — untrusted text to domain value, and use case to resource —
is covered by `API.WireSpec` and `Application.ServiceSpec` without one.

The suite runs against `UPIMock.Support.Sim`, a `State`-monad inhabitant of the same
four port classes. Three disciplines make that meaningful:

- **The double is not a stub.** It enforces per-stream optimistic concurrency, global
  RRN uniqueness and all-or-nothing commits. A double that accepted writes the real
  store would reject turns every passing property into a false negative, so
  `Support.SimSpec` tests the double against those laws before any other spec relies
  on it.
- **The double is deterministic.** `currentTime` advances exactly one second per
  call and `freshUuid` counts, so a failing property prints a scenario that
  reproduces byte for byte.
- **Generators only produce values the smart constructors accept.** Rejection
  counterexamples are written out explicitly instead. A generator that could emit an
  invalid value would make a property pass for the wrong reason.

**Vocabularies are restated, never derived.** Event tags, command tags, wire field
spellings, URL paths and schema names are written out by hand in the specs and
cross-checked against `[minBound .. maxBound]` or against a total `\case` over a
closed type. An expectation computed from the code under test survives the rename it
exists to catch; a hand-written list plus an exhaustiveness check turns "someone
renamed `TXN_SUCCEEDED`" into a failing test, and turns "someone added a state" into
a compile error.

Partial functions are banned by `.hlint.yaml` — `head`, `tail`, `read`, `fromJust`,
`undefined`, `unsafeCoerce`, `unsafePerformIO` are all `within: []`. `error` is
exempt in exactly one module, `UPIMock.Support.Sim`, because a fixture that stops
parsing must abort the suite as loudly as possible rather than degrade to a default
— and `SimSpec` asserts that every fixture still parses, so the exemption cannot
mask a defect.

## 11. Phase-2 seam map

"Zero refactoring" is a claim, so here is the audit. Each row names the capability,
the module that changes, and what does **not**.

| Phase-2 capability | Changes | Does not change |
| --- | --- | --- |
| PostgreSQL via `hasql` | New `Infrastructure.Postgres` exporting the same three `IO` functions; the type of `envStore`; three lines of the `MonadEventStore App` instance | Domain, Engine, every use case, every handler, every test — none of them mention `Env`'s field types |
| Seeded PRNG | `envPrng :: TVar SMGen` in `Env`; the `MonadEntropy App` instance | `initiateTransaction`, which already draws unconditionally, and `rrnOfWord`, which is pure |
| Injected clock skew | The `MonadClock App` instance | `applyTransition` and `replay`, which already take time as an argument |
| Fault injection | `envChaos :: TVar ChaosProfile`; a wrapper inside the `MonadEventStore App` instance | The `StoreError` vocabulary — `StoreUnavailable` already exists and already maps to 503 |
| Read-model lag | `Infrastructure.ReadModel` only — delay or drop `putViewIO` | The `MonadReadModel` interface, and the `putView`-must-not-regress law that makes lag observable rather than corrupting |
| Webhook dispatcher | A new component reading `outbox` by `outbox_id` | The write path, which already fills the table inside every commit |
| Dispute & Reconciliation context | New `Domain`/`Engine` modules and a new aggregate type | `AggregateType` is already a closed enumeration in the log, and `stream_id` is already namespaced |
| Scenario-file driver | A new caller of `Application.Service` | The use cases, which are polymorphic in `m` and mention no transport |
| Payload upcasting | A version-dispatching decoder in `Infrastructure.Database` | `payload_version` is already stored per row, and `CorruptPayload` is already the failure it produces |

The two seams most likely to be misread: `commitOutbox` accumulating unread rows is
not a leak, it is the transactional-outbox pattern with the consumer deferred; and
`MonadClock`/`MonadEntropy` in Phase 1 are not indirection for its own sake but the
only reason the two rows above them are one-line changes.

## 12. Deliberate omissions

Stated so that a reviewer does not read them as oversights.

- **No `hoistServer`.** Each handler calls `runApp` on the environment it was given.
  Threading `App` through Servant's natural transformation buys nothing here: the
  handlers return `Either` rather than throwing, so there is no error layer to
  unify, and `Handler` would still be the outer monad. This way each handler's type
  says precisely what it does.
- **No lens dependency.** `openApiSpec` is built by record update on the generated
  document. The result is identical to the `openapi3` lens API's and the dependency
  footprint is one package smaller, which for a project whose selling point is that
  it builds is the better trade.
- **No Swagger UI.** The document is served at `/openapi.json` and dumped by
  `upi-mock-engine openapi`. A bundled UI would add a dependency with a large asset
  tree to serve a page every consumer of an OpenAPI document already has a tool for.
- **`ErrorResource` is not in `components/schemas`.** It is served on failing
  statuses, which Servant renders outside the typed route table, so a derived
  document cannot see it. That is the acknowledged price of not hand-writing the
  document; `API.RoutesSpec` records the absence as intentional so it is not
  "fixed" by hand-editing the spec.
- **`/openapi.json` is not in its own document.** `openApiSpec` describes `UPIApi`,
  not `FullApi`. Self-reference would need a schema for `OpenApi` itself and buys a
  code generator nothing.
- **No `count` total on the collection endpoint**, for the reason given in §4.
- **No dispute, chaos or notification context.** §11.

