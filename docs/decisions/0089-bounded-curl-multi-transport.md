# ADR 0089: Bind the scheduler to bounded curl multi transport

- Status: Accepted for the experimental M1 protocol slice
- Date: 2026-08-15
- Owner: ksonda
- Depends on: ADR 0088

## Context

ADR 0088 supplies central permits, budget reservations, single-flight keys, and
ordered collection without performing network work. M1 also needs a concurrent
transport that preserves the existing DNS, redirect, encoding, cache, and
physical-attempt boundaries.

The supported M7 handlers already declare one physical attempt with retries,
redirects, and cache disabled. General package-owned retries remain available
through the existing sequential HTTP client. Concurrent retry state and public
handler orchestration are not implied by adding a raw multi-request transport.

## Decision

Add an internal curl multi execution boundary with these rules:

- Every concurrent request uses a zero-retry `gx_client` and the same canonical
  request identity as the sequential HTTP client.
- The central scheduler reserves total and per-host permits plus aggregate
  request and byte budgets before a curl handle is created.
- Each admitted physical attempt reserves the package host throttle, resolves
  DNS again, rejects any non-public answer, and pins the selected public IPv4
  address through libcurl's resolve option.
- Curl redirects, proxy routing, automatic content decoding, error-status
  conversion, and HTTP/2 multiplexing are disabled. The pool's connection
  limits match the scheduler permits.
- A progress callback rejects an advertised or transferred size over the hard
  ceiling. The streaming callback never retains a chunk that would cross that
  ceiling. An overflow with uncertain transferred bytes charges the full
  reserved ceiling.
- Completion callbacks may arrive in any order. Each callback is validated and
  converted into the existing redacted physical-attempt evidence before the
  scheduler releases its permit. Final results retain original request order.
- Cache lookup occurs before scheduler admission. Compatible identical cache
  keys share one network attempt, and only a validated terminal response may be
  written. Valid offline cache hits consume no permit or physical budget.
- DNS rejection, transport failure, response rejection, offline miss, and
  aggregate budget deferral remain explicit typed outcomes.

The boundary is internal. Public `gx_fetch()` remains sequential until the M1
orchestration slice separates each handler's capability and request preparation
from its response parsing and compact-result construction.

## Consequences

- Curl completion order no longer affects result order or budget ownership.
- Concurrent cache writes occur in R callbacks after validation, and
  single-flight sharing prevents two writes for the same compatible key.
- Existing sequential retries and public fetch schemas do not change in this
  slice.
- M1 remains partial until public fetch orchestration uses the concurrent
  boundary and its frozen handler results still revalidate exactly.

## Acceptance evidence

Deterministic fake-pool tests cover connection limits, reverse completion,
public-address pinning, streaming overflow, DNS isolation, single-flight cache
reuse, offline cache hits, and aggregate request and byte deferral. A bounded
live two-request probe returned HTTP 200 for the reference collections and
conformance resources with 152,495 and 2,050 response bytes.
