# 6. An `Application/` layer, absent from the design document

- **Status:** accepted
- **Date:** 2026-02-14
- **Deciders:** maintainers
- **Supersedes:** —
- **Superseded by:** —

## Context

The engineering design document specifies the module tree as `Domain/`, `Engine/`,
`Infrastructure/`, `API/`, plus `App.hs` and `Config.hs`. Implementing it literally
raised a question it does not answer: where do the port classes live, and where do the
use cases live?

Both candidate answers are bad. Putting `MonadEventStore` in `Infrastructure/` makes
the domain's abstraction a member of the layer it exists to abstract over, and any
module needing the class must import infrastructure to get it — the dependency the
"domain must not know SQLite exists" requirement forbids, reintroduced by file
placement. Putting the use cases in `API/Handlers.hs` fuses orchestration to HTTP:
`initiateTransaction`'s entropy-before-write ordering and `applyCommand`'s
load-decide-commit-project sequence would only be reachable through a request, and
`ServiceSpec` would need a socket to test them.

## Decision

Add one layer, two modules, between `Engine/` and `Infrastructure/`:

- `UPIMock.Application.Ports` — the four port classes, `CommitRequest`,
  `CommitResult`, `StoreError`, `ViewQuery`. Imports no infrastructure and no `IO`.
- `UPIMock.Application.Service` — the use cases (`initiateTransaction`,
  `applyCommand`, `fetchEventLog`, `rebuildReadModel`), polymorphic in their
  constraints, and `ServiceError`.

The import rule that follows is stated in `ARCHITECTURE.md § 1`: `Application` may
import `Domain` and `Engine`; `Infrastructure` may import `Application.Ports` for its
types but implements no instances; the instances live in `UPIMock.App`.

`Application.Service` holds the codebase's only open import of a project module —
`import UPIMock.Application.Ports` without a list. That is deliberate: `Ports` is that
module's entire vocabulary, and an explicit list of every port method would be
maintenance with no reader benefit. `CONTRIBUTING.md § Non-negotiables` names it as the
one exception so it is not copied.

## Consequences

Everything the ports requirement asked for becomes checkable by grep rather than by
argument: `Domain/*` and `Engine/*` import nothing outside themselves;
`Application/Ports.hs` imports no infrastructure; exactly one module in the tree
imports both `Application.Ports` and `Infrastructure.*`, and it is `App.hs`.

`ServiceSpec` tests every use case against `Support.Sim` with no server, no file and no
socket — the property that let Phase 1 ship without `hspec-wai`, and the reason the
HTTP layer's tests are only about the boundary (`WireSpec`) and the contract
(`RoutesSpec`).

`API/Handlers.hs` shrinks to what a handler should be: decode, call one use case, map
the error to a status. `serviceServerError` is the only place in the codebase that
mentions an HTTP status code.

The cost is a divergence from the specification's file tree, which is why this ADR
exists and why `README.md § Layout` points at it. A reviewer comparing the tree to
§9 of the design document will find one extra directory, and should find this file
before concluding it is drift. Anyone extending the module tree further should expect
to justify it the same way.

The secondary cost is indirection: reading a request end to end now crosses
`Handlers` → `Service` → `Ports` → `App` → `Database`. `ARCHITECTURE.md § 3` draws that
pipeline explicitly so the hop count is documented rather than discovered.

## Alternatives considered

**Follow the document literally, ports in `Infrastructure/`.** Rejected above: it
inverts the dependency the architecture is built on, and it does so invisibly, because
nothing about an import of `Infrastructure.Ports` looks wrong.

**Ports in `Domain/Ports.hs`.** Closer to correct, and a common hexagonal layout. It
makes `Domain` non-self-contained, which costs the strongest property this tree has —
that `Domain/*` and `Engine/*` import nothing but each other, verifiable in one grep.

**Use cases in `App.hs`.** Would tie every use case to the concrete `App` monad,
delete the `Sim` seam, and make chaos engineering a rewrite of the use cases rather
than of an instance. This is the specific refactor the zero-refactoring requirement
names.

**Use cases in the handlers.** Fuses orchestration to HTTP and forces a socket into the
test suite. It also puts the entropy-draw ordering — the part that must not vary per
request for Phase 2's seeded runs to be reproducible — in the layer most likely to be
edited for unrelated reasons.
