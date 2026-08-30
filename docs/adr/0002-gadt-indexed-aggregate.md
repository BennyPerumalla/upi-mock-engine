# 2. The aggregate is indexed by its state at the type level

- **Status:** accepted
- **Date:** 2026-02-05
- **Deciders:** maintainers
- **Supersedes:** —
- **Superseded by:** —

## Context

The transition table (§8.1, §8.2 of the design document) has a small number of legal
edges and a large number of illegal ones. The illegal ones are not exotic: reconciling
a transaction that already succeeded, authorising an Intent payment that was
authorised at initiation, refunding a payment that failed, declining something
already declined. A PSP integration under test will attempt all of them, deliberately,
and the simulator's answer must be the same every time.

Written the ordinary way — a `TxnState` field and a `case` in a handler — soundness
depends on that `case` staying exhaustive as the table grows, and on nobody
constructing a `Transaction` directly. Both are review disciplines, and review
disciplines are what fail at 4pm on a Friday. This process models money.

## Decision

`TxnState` is promoted with `DataKinds`. `Transaction (s :: TxnState)` is a newtype
over a state-independent `TxnCore`, and **its data constructor is not exported**.
`Transition (from :: TxnState) (to :: TxnState)` is a GADT whose nine constructors
are exactly the legal edges, each carrying the operand that edge requires —
`Reconcile` carries a `ReconCode`, `Fail` carries an `ErrorCode`. `applyTransition`
is the only function that produces a `Transaction`:

```haskell
applyTransition :: UTCTime -> Transition from to -> Transaction from -> (Transaction to, Event)
```

No constructor of `Transition` has `'Failed` or `'Refunded` in its source index, so
those states are absorbing as a property of the type rather than of a guard.

Because the untyped world exists — HTTP hands us a string — `StateSing`,
`SomeStateSing` and `KnownTxnState` are hand-rolled singletons, and `decide` in
`Engine/StateMachine.hs` matches on the singleton to refine the index. Each of its
right-hand sides typechecks only because that match happened; the fallback branch can
only produce `Left`, and the compiler is what enforces that.

## Consequences

"A failed transaction was reconciled into success" is not a bug a test catches; it is
a program that does not compile. Adding a state or an edge produces compiler errors at
every incomplete site, which is the point — the missing `Transition` constructors are
a list of the paths the author had not yet thought about.

Hand-rolled singletons cost about forty lines and one `KnownTxnState` instance per
state. The `singletons` library would generate them and bring a large dependency,
Template Haskell, and a second idiom for a contributor to learn; forty legible lines
won.

The existential wrappers (`AnyTransaction`, `Decided`, `Step from`) are the price of a
boundary where the state is not known statically. They are confined to
`Engine/StateMachine.hs` and `Domain/Transaction.hs`; no handler and no use case
mentions a type-level state.

One invariant the indices cannot carry: RRN uniqueness is global across streams, and
no per-aggregate type can express it. It is enforced by `UNIQUE` in `rrn_index` and
surfaces as `DuplicateRrn`. `ARCHITECTURE.md § 2` says so explicitly so that the gap
is documented rather than discovered.

## Alternatives considered

**A `TxnState` field plus validation in the service layer.** The runtime check and the
transition table drift apart, and nothing fails until a client exercises the edge.
This is the design the decision exists to reject.

**`singletons`.** Correct and more general. Rejected on dependency surface and on
readability for a contributor reading `Domain/Transaction.hs` cold.

**Free monad / indexed monad over the whole use case.** Would push the index into
`Application/Service.hs` and from there into the handlers, making every signature
mention states the HTTP layer cannot know. The existential boundary is deliberately
placed at `decide` instead.
