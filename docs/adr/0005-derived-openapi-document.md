# 5. The OpenAPI document is derived, not written

- **Status:** accepted
- **Date:** 2026-02-12
- **Deciders:** maintainers
- **Supersedes:** —
- **Superseded by:** —

## Context

The primary consumer of this simulator is another team's client, usually generated
from an OpenAPI document. A hand-maintained document is wrong the first time a route
changes and nobody notices, because nothing in the build depends on it being right.
For a project whose entire purpose is to be a trustworthy fixture, a lying contract is
the worst available defect.

## Decision

`UPIMock.API.Routes` defines `UPIApi` as a Servant type, and `openApiSpec` is
`toOpenApi` over it via `servant-openapi3`, with only `_openApiInfo` replaced —
title `UPI-MockEngine`, version from `Paths_upi_mock_engine`, plus a description a
derived document cannot invent. Schemas come from `ToSchema` instances in
`UPIMock.API.Wire` that share `wireOptions` with the `ToJSON` instances, so the
document and the encoder cannot disagree about field names.

It is served at `GET /openapi.json` and dumped by `upi-mock-engine openapi [-o FILE]`,
so a client generator can run in CI without starting a server.

Because a derived document is reviewed by nobody, `test/UPIMock/API/RoutesSpec.hs`
is the review. It asserts against `toJSON openApiSpec` — the JSON, not the
`openapi3` record — because a change that leaves the Haskell value equal while
altering its encoding is still a breaking change downstream, and only the encoded
form catches it. The path list and the schema names are written out by hand for the
same reason `EventsSpec` writes out the event tags.

## Consequences

A renamed route, a changed method, or a resource that stops reaching the document
under its own name fails the test suite instead of a downstream client's build. The
API version is the package version, so a consumer caching the document against that
string is not misled by a schema change shipped without a release.

Two documented consequences that read as defects and are not:

`ErrorResource` is absent from `components/schemas`. It is served on failing statuses,
which Servant renders outside the typed route table, so it cannot appear in a derived
document. The shape is specified in prose in `README.md § Three kinds of "no"`
instead. This is the price of not hand-writing the document, and it is cheaper than
the price of hand-writing it.

`/openapi.json` does not document itself. The document describes `UPIApi`, not
`FullApi`; self-reference would need a `ToSchema` for `Data.OpenApi.OpenApi` and buys
a generator nothing.

There is no Swagger UI. `servant-swagger-ui` would add a dependency and a bundle of
vendored JavaScript to serve a page any consumer can get by pointing their own tool at
`/openapi.json`. `CHANGELOG.md` must not claim otherwise.

The cost is that the document's shape is partly the library's choice, not ours: how
`servant-openapi3` renders query parameters, and the default `400`/`404` responses it
injects, are its decisions. `RoutesSpec` therefore asserts `shouldContain ["201"]`
rather than matching the response set exactly — an assertion about what we promise,
tolerant of what the library adds.

## Alternatives considered

**A checked-in `openapi.yaml` as the source of truth, with the server validated
against it.** Contract-first, and defensible for a multi-team API. Rejected here
because the route types already are the contract, and two artifacts means a
synchronisation job that fails silently in the direction nobody tests.

**Generating the document in CI and diffing it against a committed copy.** Catches the
same drift and makes every route change a two-file diff with a large generated blob.
`RoutesSpec` asserts the properties that matter to a client and stays readable.

**No document; publish the curl session in the README.** What the README does *as
well*. It does not let a consumer generate a typed client, which is the point of
publishing a contract at all.
