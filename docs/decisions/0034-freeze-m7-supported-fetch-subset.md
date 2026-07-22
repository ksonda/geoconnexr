# ADR 0034: Freeze the supported M7 fetch subset and open M8

- Status: Accepted
- Date: 2026-07-22
- Owners: package maintainers
- Supersedes: the M8 sequencing gate in ADR 0033

## Context

M7 was decomposed into increasingly narrow implementation checkpoints. That
made each transport and parsing boundary reviewable, but the milestone exit
kept moving as provider breadth, pagination, registration, and replay were
treated as prerequisites for harmonization. At M7n, six fixture-backed handler
families already run under one bounded scheduler:

1. direct CSV;
2. WQP Result;
3. EDR position;
4. current USGS continuous;
5. current USGS daily; and
6. OGC API Features.

M8 needs a stable fetched-result boundary. It does not need every provider
variant, page-following strategy, plugin contract, or replay format before unit
and variable harmonization can begin. Keeping those independent concerns on
M7's critical path creates roadmap whiplash without reducing M8's core risk.

## Decision

Freeze M7 at the six-family, sequential, single-page supported subset. Publish
the existing deterministic selection boundary as `gx_fetch_plan()` and publish
`gx_fetch()` as the only execution entry point. `gx_fetch()` accepts one
validated `gx_fetch_plan`, supports only `parallel = 1L`, and returns a
validated `gx_fetched` contract version 0.1.0.

The exact top-level `gx_fetched` fields are:

```text
contract_version, plan, status, results, provenance, metadata
```

`status` contains one row per distribution with stable handler, terminal
status, attempt, byte, execution, result, and error facts. `results` contains
one row per success with the handler-native `data` payload, payload class,
dimensions, result identity, and retained raw response bytes when the compact
M7 contract possesses them. Direct CSV compact evidence deliberately lacks its
original body and reports `raw_body_available = FALSE`; all other supported
handlers retain their bounded response body. `provenance` retains the complete
validated M7n orchestration object, and whole-object validation re-derives the
public plan, status, payload projection, and metadata from it.

The public execution policy is fixed for this contract:

- sequential execution only;
- at most 32 admitted requests and 64 MiB of decoded response data per call;
- at most 8 MiB per response;
- at most 100,000 rows, 1,000 columns, and 1,000,000 fields per tabular
  payload;
- an OGC API Features page limit of 10,000;
- a 60-second request timeout and 0.2-second per-host interval;
- no cache, redirect, retry, credential augmentation, or next-page follow; and
- continue after typed per-distribution capability, transport, or parse
  failures.

`dry_run = TRUE` performs the same request derivation and admission without
package probing, DNS, provider transport, clocks, throttling, cache access, or
writes. A deterministic dry-run scope keeps repeated results identical.

The following work moves off the M7-to-M8 critical path:

- latest USGS continuous and daily variants;
- legacy NWIS IV/DV compatibility execution;
- EDR query types beyond the reviewed position subset;
- provider pagination and aggregate page ledgers;
- runtime handler registration and plugin identity; and
- portable serialization, resume, and replay.

These are later fetch enhancements. They may extend the public contract only
through a separately reviewed version; they do not reopen M7 or block M8.

## Acceptance criteria

- `gx_fetch_plan()` exposes the existing offline catalog selection and accepts
  only the exact built-in classifier registry.
- `gx_fetch()` rejects parallelism other than one and exposes no internal
  performer, resolver, scope, or limit override.
- Dry run is deterministic and performs no host or provider work.
- Live fixture execution projects handler-native tables and `sf` payloads in
  exact result order while preserving one status row per distribution.
- Status, payload, metadata, plan, or nested-provenance forgery fails through a
  public fetched-result condition.
- Existing M7 request, transport, parser, budget, redaction, and failure
  isolation tests remain green.
- Documentation marks M7 complete for the supported subset and M8 next.

## Consequences

- M8 harmonization can depend on one explicit `gx_fetched` shape instead of an
  internal scheduler object.
- Provider breadth and replay remain valuable but no longer move the milestone
  boundary.
- The current public fetch surface is intentionally narrow and experimental;
  unsupported requests remain visible in status rather than being guessed or
  silently attempted.
- Public catalog discovery remains a separate M6 concern. The plan constructor
  accepts a validated `gx_catalog` as specified, without expanding this ADR
  into live catalog discovery.
