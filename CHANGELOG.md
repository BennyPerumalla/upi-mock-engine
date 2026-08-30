# Changelog

All notable changes are recorded here. This project adheres to [Semantic
Versioning](https://semver.org/spec/v2.0.0.html). The public contract under
SemVer is: the HTTP surface described by `/openapi.json`, the persisted event
payload schemas (`payload_version`), and the exported module API of the library.

## [Unreleased]

## [0.1.0.0] — Phase 1 (MVP)

### Added

- Transaction Orchestration bounded context: P2P Intent, P2P Collect and P2M QR
  flows over a `Transition` GADT whose type indices make illegal state
  representations unrepresentable.
- Append-only event log on SQLite with per-stream optimistic concurrency
  (`UNIQUE (stream_id, stream_version)`), a transactional outbox table, and a
  durable RRN uniqueness index enforcing NPCI `DF`.
- CQRS read model held in STM `TVar`s, rebuilt from the event log at boot.
- `MonadEventStore` / `MonadReadModel` / `MonadClock` / `MonadEntropy` ports; the
  domain and engine layers do not depend on the driver.
- Servant API with an OpenAPI 3 document derived from the route types, served at
  `/openapi.json` and dumpable without a server via `upi-mock-engine openapi`.
- Property tests (Hedgehog) for state-machine totality, replay determinism and
  event-tag stability, executed against an in-memory port implementation.

### Deliberately absent (see ARCHITECTURE.md § 11 Phase-2 seam map)

- Chaos engine, PRNG-seeded fault injection, asynchronous webhook dispatcher,
  `hasql`/PostgreSQL adapter, UDIR/TTUM generation, OpenTelemetry.
- A bundled Swagger UI. Point a client generator or viewer at `/openapi.json`
  instead; `docs/adr/0005` records why the dependency was declined.
