# ADR 0010: Own retries and physical-attempt accounting in the package

- Status: Accepted for the experimental M1 protocol slice
- Date: 2026-07-13
- Owner: ksonda

## Context

Delegating retries to the transport library hid transient responses, transport
failures, DNS changes, response bytes, and sleep policy from the package. A
logical request could therefore consume several physical network attempts while
JSON-LD, reference, and crosswalk budgets recorded only one terminal response.
That made the fail-closed cumulative budgets in ADRs 0006–0008 incomplete.

## Decision

- The HTTP performer executes exactly one physical attempt. Package-owned
  orchestration permits at most `retries + 1` attempts with one stable request
  identity and body.
- Only HTTP 429, 500, 502, 503, and 504 responses and transport failures are
  retryable. Safety, response-contract, content-encoding, payload-limit, and
  other package conditions remain terminal. User interrupts propagate
  immediately without retry or transport-error wrapping.
- Every physical attempt repeats hostname resolution, public-address
  validation, and connection pinning before transport.
- `Retry-After` accepts decimal seconds or an HTTP date. Delay remains bounded
  by `geoconnexr.retry_max_delay` (60 seconds by default); the client stops
  instead of retrying earlier when a server minimum exceeds that ceiling.
  Injected jitter and sleep functions make the policy deterministic in tests.
- Responses and terminal conditions retain a redacted physical-attempt ledger.
  JSON-LD and reference workflows project every attempt into their existing
  nine-column request ledgers. Known response bytes are charged exactly;
  transport failures with unknown partial-transfer size conservatively consume
  the attempt's byte ceiling.
- Workflow hooks check request and remaining-byte capacity before every
  attempt, can shrink that attempt's hard byte ceiling, and record it before a
  retry, return, or error.
- Cache lookup occurs once before transport. Only a terminal eligible response
  is persisted, cached historical attempts are discarded, and a cache hit has
  zero physical attempts and performs no DNS lookup while remaining visible as
  a workflow retrieval.
- Speculative item and direct JSON-LD probes in ADR 0007 remain single-attempt.
  Explicit large-file installation in ADR 0009 also remains single-attempt
  pending a separately reviewed partial-file retry contract.

This decision supersedes only the temporary retry-accounting statements in
ADRs 0005 and 0006. Their URL, parser, cache, and safety boundaries remain in
force.

## Consequences

- JSON-LD, reference, and gage-crosswalk request/byte ceilings now account for
  transient responses and terminal transport failures rather than only logical
  requests.
- Retry behavior, `Retry-After`, exhaustion, cache admission, and DNS rebinding
  are deterministic and testable without sleeping or using the network.
- Conservative charging can exhaust a workflow after an unknown-size transport
  failure even when fewer bytes actually arrived. Failing closed is preferred
  to silently exceeding the declared budget.
- At this decision's acceptance, M1 remained partial until throttle/concurrency
  behavior and its deterministic acceptance tests were implemented. ADR 0011
  now closes the throttle portion; bounded concurrency remains open.
