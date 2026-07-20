# ADR 0029: Orchestrate CSV and OGC handlers under one bounded status contract

- Status: Accepted internal substrate
- Date: 2026-07-20
- Owners: package maintainers

## Context

M7h proved deterministic multi-request orchestration for direct CSV, while M7i
proved one reservation-bound OGC API Features request and execution. They did
not yet establish that different handlers could share one scheduler, one
admission budget, and one exact all-distribution status projection without
weakening either handler's request, attempt, result, or failure contracts.

The next checkpoint must schedule the implemented handlers in M7d global fetch
order, isolate a failure to its own distribution, and retain exactly enough
evidence to revalidate every successful result. It must also keep dry-run
planning entirely offline and leave unimplemented handlers explicit. This is
an internal orchestration proof, not authorization for public `gx_fetch()`,
pagination, runtime registration, or the remaining provider adapters.

## Decision

Add the unexported M7j `gx_fetch_orchestration` S3 contract, version 0.1.0.
The object embeds one M7d request plan and owns:

- an explicit immutable orchestration policy;
- one derived cross-handler candidate-request table;
- one scope-bound orchestration identity;
- handler-specific compact successful results;
- exactly one terminal status row per M7d coverage row; and
- reconciled authority, count, byte, and blocker metadata.

### Offline planning and shared admission

M7j reuses every M7d direct-CSV logical request and derives an M7i request for
each selected compatible `ogc_api_features` held reservation. Candidate rows
contain only handler, order, distribution, logical-request, reservation,
response-ceiling, and request-status facts. They are sorted by the original
M7d `fetch_order`; M7j does not group requests by handler.

An OGC distribution that satisfies the M7d handler reservation but has an
ambiguous source URL under M7i becomes `handler_plan_unsupported`. That local
planning incompatibility does not erase the distribution or abort unrelated
requests. Other held handlers remain `handler_unimplemented`.

Admission makes one deterministic pass through candidate order. A request is
admitted only when it fits both an explicit count ceiling no greater than 32
and an explicit aggregate reserved-response ceiling no greater than 64 MiB.
Both CSV and OGC charge their full reserved response ceiling at admission, so
neither handler can borrow capacity invisibly from the other. A request that
does not fit is `batch_limit_deferred`; later candidates are still evaluated.

### Global sequential execution and failure isolation

Live execution is sequential with parallelism fixed at one. Each admitted
candidate receives a unique domain-separated child execution scope binding the
orchestration identity, candidate order, handler, and logical request. CSV
dispatches to M7g. OGC rebuilds its M7i plan from the shared M7d object and
rechecks the native handler symbol at invocation.

Cache, redirects, and retries remain disabled. Each admitted request can make
at most one physical attempt. The scheduler catches only the typed failure
phases already owned by the handler contracts:

- CSV transport and parse failures;
- OGC invocation-time capability, transport, and parse failures.

The failure becomes a redacted terminal row with a handler-specific error code,
bounded charged-attempt facts, and no result index. Unrelated candidates
continue. An OGC capability failure records that scheduling reached the
handler but charges zero physical attempts and zero bytes because it occurs
before DNS and transport.

### Result evidence and compaction

Successful CSV values use the exact M7h compact result contract: execution and
attempt facts, response validation, parser policy, schema, character data, and
identities, without the raw body or repeated M7d-to-M7a chain.

Successful OGC values drop the repeated M7i request-plan chain but retain the
bounded GeoJSON body, normalized `sf` result, implementation facts, execution,
and attempt. Whole-object validation rebuilds the M7i request plan from the
single shared M7d plan, reconstructs a complete M7i execution, reparses the
retained bytes, and requires the normalized feature result and every identity
to match. Retaining the OGC body is deliberate until a separate portable
simple-feature serialization contract exists.

### Exact status and whole-object validation

Status contains exactly one row for every M7d coverage row in selection order.
It distinguishes dry-run planned, successful, capability failed, transport
failed, parse failed, batch deferred, handler plan unsupported, handler
unimplemented, handler budget deferred, CSV budget deferred, not selected, and
reference-only outcomes. Attempt, success, physical-attempt, byte, execution,
result-index, and error-code facts must agree with the terminal outcome.

Successful rows map one-to-one to results in global execution order. Validation
re-derives candidate planning and admission, child scopes, orchestration and
result identities, handler-specific result contracts, status identities,
terminal mappings, per-request and aggregate bytes, counts, authority flags,
and non-replayability reasons. Forged nested plans, candidate requests, feature
bytes or rows, CSV cells, scopes, statuses, or metadata fail closed.

### Dry-run and authority boundary

`dry_run = TRUE` performs the same planning, global admission, identity, and
all-distribution status projection but invokes no performer, DNS resolver,
clock, throttle, cache, filesystem operation, symbol resolver, or handler. It
does not consume a reservation or claim provider observation.

M7j remains host-specific after live work, non-replayable, not generally
execution-ready, and unexported. It does not implement:

- WQP, EDR, current or legacy USGS handlers;
- provider property, time, bbox, or parameter mappings;
- OGC or other provider pagination and aggregate page ledgers;
- optional-package invocation beyond the native OGC handler;
- reviewed runtime handler registration or plugin identity;
- portable serialization, resume, retry, or replay; or
- public `gx_fetch()` and the final `gx_fetched` payload schema.

## Acceptance criteria

- Mixed CSV and OGC fixture requests are derived offline and ordered by their
  original global M7d fetch order.
- Dry-run planning and status projection are deterministic and invoke no host
  or provider capability.
- Shared count and aggregate reserved-byte limits apply across handlers.
- Live mixed fixtures execute one request per admitted candidate with unique
  child scopes, no cache, redirects, or retries.
- One CSV transport failure remains redacted and does not prevent the later OGC
  request from succeeding.
- A missing OGC symbol becomes a capability-failure row with no DNS, transport,
  charged attempt, or bytes.
- An invalid OGC feature page becomes one handler-specific parse-failure row
  without discarding successful CSV results.
- Every M7d coverage row has exactly one status, and successful rows map
  one-to-one to compact handler-specific results.
- Whole-object validation rejects forged request, plan, scope, CSV data, OGC
  body/result, status, byte, result-index, error-code, count, or metadata facts.
- The orchestration constructor and validator and public `gx_fetch()` remain
  unexported.

## Consequences

- M7 now has one fixture-backed scheduler for its two implemented execution
  paths, with shared admission and exact cross-handler status reconciliation.
- The M7d all-handler reservation model is proven through mixed live execution,
  including a zero-transport capability failure.
- M7 is not complete: remaining provider handlers, pagination, registration,
  serialization/replay, and a public fetched-result schema remain deliberate
  gates before M8 harmonization can begin.
