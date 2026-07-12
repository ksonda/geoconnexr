# ADR 0004: Mainstem and SPARQL contracts remain release gates

- Status: Accepted release gate; underlying choices unresolved
- Date: 2026-07-12
- Decision owner: ksonda
- Target decision date: 2026-08-02

## Context

Two P0 choices lack enough production evidence for a durable public contract.

First, the package must choose whether `mainstems` or `mainstems_v3` is the
default reference collection and define a vintage and migration policy. A
checked `mainstems` item route returned an upstream error while filtered
collection retrieval worked, and current, superseded, and large geometries may
behave differently.

Second, the graph currently accepts SPARQL POST requests at its root, but that
observed behavior has not been established as a supported public endpoint
contract. Spatial queries must use `geof:sfIntersects`, and arbitrary user
SPARQL must never be rewritten for pagination.

## Decision

Treat both choices as blocking gates for the 0.1.0 public API freeze. Internal
spike code, fixtures, and configurable experimental interfaces may proceed,
but the package must not tag 0.1.0 or declare the related schemas stable until
the evidence below is reviewed and a superseding ADR selects each contract.

The weekly live workflow may test bounded known answers for these services. It
must not use full-graph triple counts or fail solely because a mutable result
count drifts.

## Required evidence

### Mainstem collection

- Compare `mainstems` and `mainstems_v3` identifiers, provenance, vintage
  metadata, current/superseded relations, and geometry behavior.
- Exercise item, filtered-collection, and negotiated JSON-LD retrieval for a
  current item, a superseded item, and a large-geometry item.
- Specify the default collection, how its vintage is recorded, how migrations
  are exposed, and what fallback behavior is contractual.

### SPARQL endpoint

- Verify bounded SELECT and ASK POST behavior, response media types, timeouts,
  rate limits, and structured error responses at the candidate endpoint.
- Verify the indirect/direct mainstem UNION and one small
  `geof:sfIntersects` AOI known answer without asserting a global count.
- Decide whether POST-at-root is supported, configurable but experimental, or
  replaced with a documented endpoint before exposing it as a stable default.
- Confirm only named, stably ordered SELECT templates paginate; raw SPARQL
  executes exactly once.

## Consequences

- Mainstem and SPARQL defaults remain experimental during the P0 spike.
- Fixtures may preserve observed behavior but do not by themselves establish
  upstream guarantees.
- If the target date passes without sufficient evidence, the owner must either
  set a new dated decision point or remove the affected interfaces from the
  0.1.0 surface.
