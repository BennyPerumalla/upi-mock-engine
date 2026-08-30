# UPI-MockEngine-Haskell

A deterministic, locally deployable simulator of the NPCI/UPI switch.

It exists to make the *awkward* parts of a UPI integration testable: the switch
timeout that resolves to a credit hours later, the duplicate RRN, the technical
decline that reconciliation reverses, the refund leg opened against a settled
payment. Those paths are the ones that break PSP integrations in production and
the ones a bank sandbox will not let you provoke on demand. Here they are one
`POST` away, they are reproducible, and the engine cannot be driven into a state
the specification forbids — that is a property of the type system rather than of
a validation layer.

Phase 1 (`v0.1`) implements the **Transaction Orchestration** bounded context:
P2P Intent, P2P Collect and P2M QR over a compile-time-verified state machine, an
append-only SQLite event log, an STM CQRS read model, and an OpenAPI 3 document
derived from the Servant route types.

## What it guarantees

**Illegal states do not compile.** `Transaction` is indexed by a type-level
`TxnState` and its constructor is not exported. The only way to obtain a
`Transaction 'Success` is to apply a `Transition from 'Success` to a
`Transaction from`. No constructor of `Transition` has `'Failed` in its source
index, so "a failed transaction was reconciled into success" is not a bug that
tests catch — it is a program that does not typecheck.

**The domain does not know SQLite exists.** `UPIMock.Domain.*` and
`UPIMock.Engine.*` import nothing outside themselves.
`UPIMock.Application.Ports` declares four classes (`MonadEventStore`,
`MonadReadModel`, `MonadClock`, `MonadEntropy`) and imports no infrastructure.
The SQLite adapter exports plain `IO` functions; the instances that connect the
two live in `UPIMock.App`, which is the only module in the tree that mentions
both sides.

**The event log is the system of record.** Every state change is an append with
per-stream optimistic concurrency. The read model is a projection, held in an STM
`TVar`, rebuilt from the log at boot. If the rebuild fails, the process exits
before it binds the port rather than serving a projection with a hole in it.

**Nothing is nondeterministic by accident.** Time and randomness are ports, not
ambient effects. No function below `UPIMock.App` calls `getCurrentTime`, and
every identifier the engine mints is a pure function of one `Word64` draw. The
number of draws per request does not depend on the request — which is what makes
Phase 2's seeded runs reproducible rather than approximately reproducible.

## Build and run

Two supported paths. Nix is the reproducible one; the Cabal path is what you will
use while editing.

```sh
nix develop                 # GHC 9.10.3, HLS, hlint, fourmolu, sqlite, jq
hpack && cabal build all    # package.yaml is authoritative; the .cabal is generated
cabal test all --test-show-details=direct
```

`make` wraps the same commands (`make build`, `make test`, `make lint`,
`make fmt-check`, `make ci`) and always runs `hpack` first, because a stale
`.cabal` file is the one build failure that looks like a compiler bug.

```sh
cabal run upi-mock-engine -- --port 8080 --database ./state/upi.sqlite3
```

```
upi-mock-engine 0.1.0.0
  database        ./state/upi.sqlite3
  journal mode    wal
  schema version  1
  read pool       4
  replayed        0 transaction(s) from the log
  listening       http://localhost:8080
```

The banner reports the journal mode the database *adopted*, not the one that was
requested: WAL is unavailable on some network filesystems and SQLite falls back
silently, so an operator debugging serialised writes needs to read `delete` here
rather than infer it. `listening` is printed from warp's `beforeMainLoop` hook,
after the socket is accepting, so a harness may race that line without racing a
closed port.

| Flag | Default | Meaning |
| --- | --- | --- |
| `-d`, `--database PATH` | `upi-mock.db` | SQLite file, or `:memory:` for a run that leaves nothing behind |
| `-p`, `--port PORT` | `8080` | TCP port |
| `--readers N` | `4` | Read-connection pool size; ignored for `:memory:`, which cannot be pooled |
| `-v`, `--verbose` | off | Log every request. Off by default because the output of a simulator run that matters is the event log |

`upi-mock-engine openapi [-o FILE]` writes the OpenAPI 3 document and exits, so a
client generator can run in CI without starting a server.

## HTTP surface

| Method | Path | Purpose |
| --- | --- | --- |
| `POST` | `/v1/transactions` | Open a transaction. `201` with the projected resource |
| `GET` | `/v1/transactions` | Page the projection. `?state=&flow=&limit=&offset=` |
| `GET` | `/v1/transactions/{txnId}` | One transaction, from the read model |
| `GET` | `/v1/transactions/{txnId}/events` | The raw stream, in order, from the log |
| `POST` | `/v1/transactions/{txnId}/commands` | Advance the transaction. The only write path after creation |
| `GET` | `/v1/rrn/{rrn}` | The same resource, addressed the way a switch addresses it |
| `GET` | `/health` | Liveness, plus version, schema version, journal mode, projection size |
| `GET` | `/openapi.json` | The document derived from the route table |

There is one `commands` endpoint rather than `/authorize`, `/decline`, `/timeout`
and six more siblings. Every one of those would be the same handler with a
different constructor, and *which* of them is legal is a property of the
aggregate's current state — not of the URL space. A route table that implied
otherwise would be lying.

`GET /v1/transactions/{txnId}` reads the projection; `…/events` reads the log.
That is why the two disagree about what "not found" means: an empty stream is a
404 from the log, because the log is the only thing that can answer *does this
transaction exist*.

### A Collect that times out and is reconciled

```sh
# 1. The payee requests. A Collect rests in INITIATED until the payer answers.
curl -sS -X POST localhost:8080/v1/transactions -H 'content-type: application/json' -d '{
  "flow": "P2P_COLLECT",
  "payerVpa": "alice@psp",
  "payeeVpa": "bob@psp",
  "amountPaise": 250000,
  "note": "rent"
}'
# {"txnId":"…","rrn":"418…","state":"INITIATED","flow":"P2P_COLLECT",
#  "amountPaise":250000,"amount":"2500.00","currency":"INR","version":1, …}

TXN=…   # 2. The payer approves. INITIATED -> PENDING.
curl -sS -X POST localhost:8080/v1/transactions/$TXN/commands \
  -H 'content-type: application/json' -d '{"action":"AUTHORIZE","authRef":"auth-7"}'

# 3. The switch window elapses. PENDING -> TIMEOUT.
curl -sS -X POST localhost:8080/v1/transactions/$TXN/commands \
  -H 'content-type: application/json' -d '{"action":"TIMEOUT"}'

# 4. Offline reconciliation finds the beneficiary was credited. TIMEOUT -> SUCCESS.
curl -sS -X POST localhost:8080/v1/transactions/$TXN/commands \
  -H 'content-type: application/json' -d '{"action":"RECONCILE","reconCode":"102"}'
# "state":"SUCCESS", "reconCode":{"code":"102","category":"RECONCILIATION", …},
# "settledAt":"…", "version":4

curl -sS localhost:8080/v1/transactions/$TXN/events | jq -r '.[] | "\(.version) \(.type)"'
# 1 TXN_INITIATED
# 2 TXN_AUTHORIZED
# 3 TXN_TIMED_OUT
# 4 TXN_RECONCILED
```

Step 4 cannot be issued without `reconCode`: `Reconcile` carries a `ReconCode` in
its type, so `TIMEOUT -> SUCCESS` with no TTUM record has no representation to
refuse at runtime. Issue the same command a second time and it comes back `409
ILLEGAL_TRANSITION` naming `SUCCESS` as the state it was rejected from — which is
the single most useful thing this simulator tells a client under test.

For P2P Intent and P2M QR, the payer has already entered the MPIN in their own
app, so initiation writes `TXN_INITIATED` and `TXN_AUTHORIZED` in one commit and
answers `PENDING`. Sending `AUTHORIZE` to one of those is `409
ILLEGAL_TRANSITION`, not a silent no-op.

## Three kinds of "no"

A client that cannot tell these apart cannot tell a bug in itself from the
behaviour it is testing, so they are three types in the source and three shapes on
the wire.

| Kind | Type | Example | Response |
| --- | --- | --- | --- |
| Your request was malformed | `ValidationError` | `payerVpa: "alice"` | `400 VALIDATION_FAILED` |
| Your request was well formed and does not apply | `DomainError` | `RECONCILE` against `SUCCESS` | `409 ILLEGAL_TRANSITION` |
| The simulated switch declined the payment | `ErrorCode` | `{"action":"FAIL","code":"Z9"}` | `200`, and the resource carries `errorCode` |

The third is not an HTTP error at all. A declined payment is a *successful*
simulation of a decline, and the resource says so:

```json
"errorCode": {
  "code": "Z9",
  "description": "Insufficient funds in the payer account",
  "category": "BUSINESS"
}
```

The description ships with the code deliberately. `{"code":"UB"}` alone sends the
reader of a failing test to a specification PDF; this way the failure explains
itself. Codes with first-class semantics are `UB`, `U30`, `UP`, `U16`, `Z9`, `ZA`,
`ZM`, `ZH`, `XH`, `K1`, `DF` and `15`; any other two- or three-character
upper-case alphanumeric token is accepted as an unmapped code, so a scenario can
drive a code NPCI publishes next quarter without waiting for a release.

Every refusal names the field it refused — `{"error":"VALIDATION_FAILED",
"message":"unknown flow: P2P"}` — because a client told `400` and nothing else
cannot fix its request. The `error` class is stable and machine-readable; the
`message` is prose and may be reworded. Branch on the first, log the second.

The full status map is a single `case` in `UPIMock.API.Handlers.serviceServerError`
— the only place in the codebase that mentions an HTTP status. Two entries there
are worth stating: `DuplicateRrn` is `409` and carries NPCI `DF`, because the
request was well formed and lost a race for a globally unique value; a corrupt
stored payload is `500`, because the client sent nothing wrong and an operator has
to know the fault is on this side.

## Determinism

Reproducibility is the feature. A simulator whose failing run cannot be replayed
from its inputs is a slower, less honest sandbox.

- `applyTransition` takes `now` as an argument. Nothing in `Domain` or `Engine`
  reads a clock.
- `replay` supplies `occurredAt` from the log, so a read model rebuilt at boot is
  identical to the one the write path produced — not merely equivalent.
- `replay` re-checks that stream versions are contiguous rather than trusting the
  store's `ORDER BY`. A gap means a write was lost, and the aggregate folded over
  a gap is a plausible-looking lie.
- Entropy is one stream. `freshWord64` takes the high words of a version-4 UUID
  rather than reaching for a second generator; seeding one of two sources in
  Phase 2 would have produced runs that are only half deterministic.

## Layout

```
app/Main.hs                       process lifecycle; the only module that may exit
src/UPIMock/
  Domain/       Types Events Transaction ErrorCodes    value objects, event vocabulary, the GADT
  Engine/       StateMachine                           pure decision core; commands in, proofs out
  Application/  Ports Service                          the hexagonal boundary, and the use cases
  Infrastructure/ Database ReadModel Schema             SQLite adapter, STM projection, DDL
  App.hs        Config.hs                              ReaderT Env IO; where ports meet adapters
  API/          Routes Wire Handlers                    the type-level route table, the wire codec, WAI
test/UPIMock/   … Support/{Gen,Sim}                     hspec + hedgehog against an in-memory store
docs/adr/                                              why the load-bearing decisions were made
```

Dependencies point one way, and `ARCHITECTURE.md` states the rule that keeps them
that way. `Application/` is the one addition to the design document's module tree;
`docs/adr/0006` says why.

## Phase 2

The architecture is Phase-1 code and Phase-2 seams, and the seams are load-bearing
from day one rather than sketched. The outbox table is written inside every commit
even though nothing dispatches from it; `MonadClock` and `MonadEntropy` look like
over-engineering until they are the reason clock skew and seeded runs are a new
instance rather than a rewrite.

- **PostgreSQL via `hasql`** — write `UPIMock.Infrastructure.Postgres` with the
  same three `IO` functions, change the type of `envStore`, fix three lines in one
  instance. No use case, handler or test changes: none of them mention `Env`'s
  field types.
- **Chaos engineering** — `envChaos :: TVar ChaosProfile` and
  `envPrng :: TVar SMGen` alongside the two existing handles, mutated over HTTP
  while requests are in flight. Read-model lag lands in
  `Infrastructure/ReadModel`, latency and fault injection in the `MonadClock` and
  `MonadEventStore` instances.
- **Dispute & Reconciliation** — UDIR and TTUM generation over the outbox rows
  that Phase 1 already accumulates.
- **Notification** — an asynchronous webhook dispatcher reading the outbox by
  `outbox_id` as a cursor, which is why `event_id` is `AUTOINCREMENT` and never
  reused.

`ARCHITECTURE.md § 11 Phase-2 seam map` maps each of these to the module and the line.

## Licence

BSD-3-Clause. See `LICENSE`.

