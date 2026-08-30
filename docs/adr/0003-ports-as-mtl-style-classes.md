# 3. Ports are mtl-style classes; the app is `ReaderT Env IO`

- **Status:** accepted
- **Date:** 2026-02-07
- **Deciders:** maintainers
- **Supersedes:** —
- **Superseded by:** —

## Context

Phase 2 replaces SQLite with PostgreSQL via `hasql`, and adds chaos engineering:
injected latency, injected faults, clock skew, read-model lag, seeded PRNG. The
requirement on Phase 1 is that none of this be a refactor.

That means the use cases must not name a concrete monad, and the domain must not be
able to name SQLite even accidentally. It also means time and randomness cannot be
ambient: a simulator that calls `getCurrentTime` in a use case has no seam for clock
skew, and one that calls a global generator cannot be seeded.

## Decision

`Application/Ports.hs` declares four classes — `MonadEventStore`, `MonadReadModel`,
`MonadClock`, `MonadEntropy` — and imports no infrastructure. Use cases in
`Application/Service.hs` are polymorphic in the constraints they need and never
mention `App`. No port method throws; every fallible one returns
`Either StoreError a`, so a caller cannot ignore a failure by not catching it.

The binary's inhabitant is `newtype App a = App (ReaderT Env IO a)`, constructor
unexported, with `Env { envConfig, envStore, envReadModel }`. `Infrastructure/Database.hs`
exports plain `IO` functions taking a handle; `UPIMock.App` holds the instances that
connect the two, and is the only module in the tree that imports both
`Application.Ports` and `Infrastructure.*`.

The tests' inhabitant is `UPIMock.Support.Sim`, a second full implementation of the
same laws.

## Consequences

`UPIMock.App` exists specifically so the instances are not orphans. An instance in
`Infrastructure/Database.hs` would force that module to import `Application.Ports`,
pointing a dependency inwards; an instance in `Ports.hs` would force it to import
infrastructure. Neither is acceptable, so a module that legitimately imports both is
the answer. This is the reason for the layer, and it is worth restating in review.

Two implementations from day one is what makes the abstraction real. An interface with
one inhabitant is a rename of that inhabitant; `Sim` is why `MonadEventStore`'s five
laws are stated on the class rather than implied by the SQLite adapter's behaviour.
Both adapters must satisfy them, so they had to be written down.

Swapping the store is then: write `UPIMock.Infrastructure.Postgres` with the same
three `IO` functions, change the type of `envStore`, fix three lines in one instance.
No use case, handler or test changes, because none of them mentions `Env`'s field
types. Chaos lands as `envChaos :: TVar ChaosProfile` and `envPrng :: TVar SMGen`
beside the existing handles, mutated over HTTP while requests are in flight; latency
and fault injection live in the `MonadClock` and `MonadEventStore` instances, which is
to say in `UPIMock.App` and nowhere else.

The costs are real. Constraint lists in `Service.hs` are verbose, and a signature that
accretes constraints is a use case doing too much — treat it as a smell, not a
formatting problem. mtl-style classes are `n × m` instances in principle; with two
inhabitants and four classes that is eight, all trivial. `RecordWildCards` plus a
`ReaderT` over a growing `Env` makes it easy to reach for a field a function should not
need, which is what code review is for.

## Alternatives considered

**A record of functions in `Env` (the handle pattern).** Equivalent in power, and it
avoids the instance boilerplate. Rejected because constraints document a use case's
requirements in its signature — `MonadEventStore m => ...` states what the function
touches, where a handle record states only that it touched `Env`.

**`RIO` or another effect framework.** Buys structure this project can express in
sixty lines of classes, and prices a contributor's first read of `Service.hs` at
learning the framework first.

**Concrete `App` in the use cases, tested through a real SQLite file.** Fast enough at
this size, and it was the obvious shortcut. It fails the actual requirement: chaos
engineering needs a *non-real* store, and clock skew needs a non-real clock. Retrofitting
the seam later is precisely the refactor this ADR exists to avoid.
