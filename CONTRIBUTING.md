# Contributing

## Toolchain

GHC **9.10.3**, GHC2021, Hpack. Two supported ways to get there.

```sh
nix develop        # ghc, HLS, cabal, hpack, hlint, fourmolu, sqlite, jq, hey
```

```sh
ghcup install ghc 9.10.3 && ghcup set ghc 9.10.3
cabal update && cabal install hpack hlint fourmolu
```

The Nix shell is authoritative for tool *versions*. If `fourmolu` reformats a file
you did not touch, you are on a different version than CI; use the shell.

## The loop

**`package.yaml` is the single source of truth.** `upi-mock-engine.cabal` is
generated and `.gitignore`d. Never commit it, never hand-edit it, and run `hpack`
after every change to `package.yaml` — a stale `.cabal` is the one build failure that
looks like a compiler bug. Every `make` target that touches `cabal` depends on `gen`
for exactly this reason.

```sh
make build          # hpack && cabal build all
make test           # hpack && cabal test all --test-show-details=direct
make lint           # hlint app src test
make fmt            # fourmolu --mode inplace over git ls-files '*.hs'
make ci             # fmt-check lint build test — what CI runs
make run            # serve on :8080 against ./state/upi.sqlite3
make docs           # dump the derived OpenAPI document through jq
```

`cabal.project` pins `index-state`, so a fresh checkout resolves the same
dependencies today and in a year. `-fwrite-ide-info -hiedir=.hie` is set there for
HLS and for `weeder`; it costs nothing at runtime.

## Non-negotiables

CI fails on all of these, and they are not style preferences.

1. **`-Wall -Wcompat` and friends are errors in CI.** The full set is in
   `package.yaml`; `-Wincomplete-uni-patterns` and `-Wmissing-deriving-strategies`
   are the two that catch real defects most often. The test component alone disables
   `-Wmissing-export-lists`, because `hspec-discover` generates `module Main where`.
2. **No partial functions.** `head`, `tail`, `init`, `last`, `read`, `fromJust`,
   `undefined`, `unsafeCoerce`, `unsafePerformIO` are `within: []` in `.hlint.yaml`.
   `error` is exempt in exactly one module (`UPIMock.Support.Sim`) and adding a
   second exemption needs an ADR, not a review comment. This process models money.
3. **`fourmolu --mode check` is clean.** Config in `fourmolu.yaml`: two-space
   indent, 100-column limit, leading commas, leading function arrows,
   diff-friendly imports and exports.
4. **Explicit import lists or `qualified`.** No open imports of project modules
   except `UPIMock.Application.Ports`, which is a deliberate exception in
   `Application.Service`: it is that module's entire vocabulary.
5. **Explicit export lists everywhere in `src/` and `app/`.** An export list is the
   module's contract; `Transaction`'s data constructor being absent from one is what
   makes the state machine sound.

## Where code goes

`ARCHITECTURE.md § 1` states the import rules. This table is the same rules read
from the other direction — you have a change, and you need to know which module
takes it.

| You are adding | It goes in | And in particular |
| --- | --- | --- |
| A value object or smart constructor | `Domain/Types.hs` | Constructor stays unexported; the parser returns `Either ValidationError` |
| An event | `Domain/Events.hs` | Add the tag to `eventTypeText`, the payload to `EventPayload`, and a case to `stepForEvent` — the compiler will demand the last one |
| A state, or a legal transition | `Domain/Transaction.hs` | A new `TxnState` means a new `StateSing`, a `KnownTxnState` instance, and a `parseTxnState`/`renderTxnState` pair |
| A command a client can send | `Engine/StateMachine.hs` + `API/Wire.hs` | The `Command` constructor carries its operand; `parseCommand` refuses a request that omits it |
| A cross-field creation rule | `Engine/StateMachine.hs`, in `checkSeed` | Not in `API/Wire.hs`. The boundary validates fields; the engine validates *combinations* |
| A use case | `Application/Service.hs` | Constraints, never a concrete monad. `MonadEventStore m => m a`, not `App a` |
| An effect | `Application/Ports.hs`, instance in `App.hs` | Never an orphan instance, and never an `IO` import in `Ports.hs` |
| A SQL query | `Infrastructure/Database.hs` | Exported as a plain `IO` function taking a `Handle`; it does not mention `App` |
| A projection field | `Infrastructure/ReadModel.hs` + `Domain/Transaction.hs` | `TxnView` is domain-owned; the read model only indexes it |
| An endpoint | `API/Routes.hs` + `API/Handlers.hs` | The route type is the contract; `/openapi.json` and `RoutesSpec` both follow from it |
| A CLI flag | `Config.hs` | And to the table in `README.md`, and to `announce` in `app/Main.hs` if it belongs in the banner |

Two placements are worth stating because both have been got wrong in similar
codebases:

**A new effect is a port, not an `IO` call.** If a use case needs to do something
new, add a class to `Application/Ports.hs` and its instance to `UPIMock.App`. Do not
`liftIO`. The reason is not purity; it is that `Support.Sim` must be able to
implement it, and a `liftIO` in a use case is a test that needs a filesystem.

**A new state is not a new field.** Encoding "settled but disputed" as a boolean on
`TxnCore` puts the state machine's soundness back in the hands of whoever writes the
next `if`. Add the `TxnState`, take the compiler errors, and let the missing
`Transition` constructors tell you which paths you have not thought about yet.

## Style

Beyond what `fourmolu` and `hlint` mechanise:

Haddock every exported identifier. Module headers carry `Description` and say what
the module is *for* — `ARCHITECTURE.md` links to modules, not the reverse, so a
module that cannot explain itself in three lines is probably two modules.

Comments say **why**, and cite the design document section or the NPCI behaviour
when there is one. `-- | Clears the terminal code, because a reconciled credit is
not a decline that also succeeded` earns its line. `-- | Applies the transition`
does not; delete it and let the signature speak.

Prefer making an illegal input unrepresentable over rejecting it. A runtime check
is what you write when the type cannot be made to carry the invariant — RRN
uniqueness is the honest example, and `ARCHITECTURE.md § 2` says why it is the only
one.

## Tests

`make test` runs hspec over `test/`, assembled by `hspec-discover`. Property tests
use hedgehog through `hspec-hedgehog`. There is no `hspec-wai` and no socket:
Phase 1's HTTP seam is covered by `WireSpec` (untrusted text to domain value) and
`ServiceSpec` (use case against `Support.Sim`).

A change without a test that would have failed before it is not finished. Three
disciplines specific to this suite:

**Restate vocabularies; never derive them.** `EventsSpec` writes out all ten event
tags and cross-checks against `[minBound .. maxBound]`. A test that maps
`eventTypeText` over the constructors asserts that the function equals itself and
passes after a rename that breaks every consumer. The wire format is a promise to
somebody else's parser, so somebody has to type it out.

**Generators produce values the smart constructors accept.** `Support.Gen` exists so
that a property failure means the property is wrong, not that the generator invented
a 40-character VPA. Test the rejection of bad input in `WireSpec`, explicitly, one
case per rule.

**`Support.Sim` is an implementation, not a stub.** It enforces the same five
`MonadEventStore` laws — per-stream version conflicts, RRN uniqueness, atomicity —
and it is deterministic: `currentTime` advances exactly one second per call and
`freshUuid` counts. If a use case needs a port behaviour `Sim` does not have,
implement it there first. A test that special-cases `Sim` is testing `Sim`.

## Dependencies

`cabal.project` pins `index-state`. Adding or bumping a dependency is therefore a
deliberate act, and belongs in **its own commit** with the `index-state` bump, the
regenerated freeze file, and the reason in the message.

Bounds go in `package.yaml` as `>= x.y && < x.(y+1)` — lower bound at the version
whose API you actually use, upper bound at the next minor. Shared dependencies live
in the top-level `dependencies:`; component-specific ones stay in the component,
which is why `openapi3` appears in both the library and the test suite. That
duplication is the point: `RoutesSpec` pins a published contract, and the dependency
list should say so rather than lean on transitive exposure.

Phase 1's dependency budget is deliberately small. `lens` is absent even though
`openapi3` is built on it — `Routes.hs` sets `_openApiInfo` with a record update
instead. Weigh a new dependency against what a contributor must learn to read the
module that uses it.

## The compatibility contract

`CHANGELOG.md` states what SemVer covers here, and it is worth restating because two
of the three are not code:

1. **The HTTP surface**, as published in `/openapi.json`.
2. **The persisted payload schemas.** `payload_version` exists so that a log
   written by an older binary stays readable. Changing the meaning of a stored
   field is a breaking change even when the JSON key is unchanged; add a field and
   bump `currentPayloadVersion` instead, and upcast on read.
3. **The exported library API.**

The event log is append-only in production and in principle. A migration that
rewrites history is not a migration; it is a different system with the same data.
Schema migrations are forward-only, idempotent, and additive — `Infrastructure/Schema.hs`
appends to `migrations` and never edits a shipped entry.

## ADRs

Decisions that constrain future contributors are recorded in `docs/adr/`, numbered,
never deleted. Copy `docs/adr/0000-template.md`, take the next number, and link it
from the code or document that depends on it — `README.md` points at `0006` for the
`Application/` layer, and that pointer is the ADR's reason to exist.

Write one when the decision closes a door: a new `error` exemption, a dependency
with a large surface, a change to the port classes, anything that makes a Phase-2
seam in `ARCHITECTURE.md § 11` harder to reach. Superseding an ADR means a new file
that says so in its header and a `Superseded by` line in the old one. An ADR that is
now wrong is more useful than no record of why the wrong thing looked right.

## Pull requests

One concern per PR. Formatting churn, dependency bumps and behaviour changes in the
same diff cannot be reviewed, and cannot be reverted independently when one of them
turns out to be the problem.

The message says *why*. The diff already says what.

Before you open it:

```sh
make ci
```

Then check what CI cannot: that new code sits where the table above says it does,
that no import crosses a layer inwards, that any new event has a payload version
story, and that `README.md` still describes the flags and endpoints the code
actually has. The docs are checked by hand — every flag, status code, JSON key and
SQL identifier in them was copied from the source, and a PR that changes one and not
the other makes the documentation worse than absent, because it is still credible.

