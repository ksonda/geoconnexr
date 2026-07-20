# ADR 0027: Orchestrate bounded direct-CSV requests with exact status reconciliation

- Status: Accepted internal substrate
- Date: 2026-07-20
- Owners: package maintainers

## Context

M7g can execute one M7d direct-CSV logical request safely, but it deliberately
does not decide which requests in a plan should run, isolate one request's
failure from later requests, reconcile execution facts with every selected or
skipped distribution, or support a no-network preview. Calling M7g repeatedly
outside a contract would leave ordering, aggregate limits, failure policy,
scope uniqueness, and status cardinality implicit.

The future public `gx_fetch()` needs one status row per evaluated distribution,
including non-selected, reference-only, deferred-handler, batch-deferred,
dry-run, successful, and failed rows. It must also preserve M7d's global
request and byte allocation, continue unrelated work after a direct-CSV
transport or parse failure, and avoid storing the same complete M7d-to-M7a
chain and raw provider body once for every successful request.

The smallest honest next checkpoint is therefore an internal, direct-CSV-only
orchestrator. It can close multi-request scheduling, no-network dry-run, and
status-reconciliation gaps without claiming that non-CSV handlers, public
fetch registration, runtime package checks, or serialization/replay exist.

## Decision

Add the unexported internal M7h S3 class `gx_csv_orchestration`, contract
version 0.1.0. Construction requires:

1. one valid M7d request plan;
2. an explicit `dry_run` logical value;
3. an explicit positive execution-count ceiling no greater than 32;
4. an explicit positive aggregate reserved-response ceiling no greater than
   64 MiB;
5. explicit parser-field, timeout, and per-host interval ceilings; and
6. one opaque 64-lowercase-hex orchestration-scope identity.

The exact top-level fields are:

```text
contract_version, request_plan, policy, orchestration, results, status,
metadata
```

M7h embeds M7d once and leaves its complete M7c-to-M7a chain byte-identical.
The nested M7a `requests` list remains empty because M7h execution evidence is
owned by the outer M7h contract; M7h does not rewrite an earlier planning
checkpoint into an execution object.

### Admission and scheduling

M7h evaluates M7d `request_plans` in their exact global `request_order`, which
is already aligned with selected-distribution `fetch_order`. A request is
admitted only when both conditions hold:

- fewer than the explicit `max_executions` requests have been admitted; and
- adding the request's full `response_byte_limit` reservation does not exceed
  the explicit `max_total_bytes` ceiling.

Admission uses reserved response ceilings rather than observed response sizes.
This guarantees before transport that successful bodies cannot exceed the
aggregate ceiling. A request that does not fit is visibly
`batch_limit_deferred`; its M7d reservation remains allocated but unconsumed.
Later smaller requests may still be admitted in deterministic order.

Live execution is sequential with `parallelism = 1`, cache bypass, no
redirects, no retries, and `continue_unrelated_requests` failure policy. Each
admitted request receives a domain-separated child scope derived from the M7h
orchestration identity, M7d request order, and logical request identity. M7h
then delegates the physical request, DNS policy, throttle, response validation,
and parsing to M7g unchanged.

Only M7g transport and parse failures are isolatable. They become bounded
terminal M7h rows and do not stop later admitted requests. A contract, clock,
or other failure outside those phases aborts M7h rather than being mislabeled
as a provider failure.

### Strict dry run

With `dry_run = TRUE`, M7h performs the same deterministic admission and status
projection but invokes no M7g code. It therefore consults no DNS resolver,
transport performer, clock, throttle clock, throttle sleeper, cache, or output
file. Admitted CSV rows become `dry_run_planned`; over-limit rows remain
`batch_limit_deferred`.

The M7d plan validator may still reread its bounded installed classifier and
handler assets. Dry run is host-independent with respect to packages and
providers, not a promise to avoid validation of bundled package data.

### Compact successful results

Keeping a full M7g object for every success would repeat the complete plan and
retain each raw response body. M7h instead validates the M7g object immediately
and stores a compact `gx_csv_orchestration_result` containing:

```text
result_id, execution, attempt, validation, parse_policy, schema, data, parse
```

The compact result retains the scope-bound execution and charged attempt row,
M7e normalized validation facts and body digest, M7f fixed parser policy,
exact character schema/data, and parse/result identities. It intentionally
discards the raw provider body and repeated request-plan chain.

Whole-object validation re-derives the request and full canonical target from
the single embedded M7d plan; recomputes validation, execution, attempt, parse,
result, and compact-result identities; verifies the child scope; rebuilds the
schema and result hash from exact character data; and checks all time, byte,
row, column, field, and foreign-key relationships. Compacting changes storage
shape, not the authority of the validated M7g evidence.

The overall M7h object has a 256 MiB aggregate owned-text ceiling in addition
to the 64 MiB admitted response reservation. These are internal implementation
ceilings, not product defaults for a future public API.

### Exact distribution status

`status` contains exactly one row for every M7d coverage row in original
selection order. Its exact columns are:

```text
contract_version, selection_order, fetch_order, distribution_id, handler_id,
logical_request_id, request_status, orchestration_status, attempted, succeeded,
physical_attempts, encoded_bytes, decoded_bytes, execution_id, result_index,
error_code
```

Terminal orchestration statuses are:

- `dry_run_planned`;
- `provider_response_validated_and_parsed`;
- `transport_failed`;
- `parse_failed`;
- `batch_limit_deferred`;
- `csv_budget_deferred`;
- `handler_unimplemented`;
- `handler_budget_deferred`;
- `not_selected`; and
- `reference_only`.

Every admitted live request is attempted exactly once and reaches one success
or failure status. Successful rows map one-to-one and in request order to the
compact `results` list. Failed rows retain only a stable redacted error code,
execution identity, physical-attempt count, and charged byte total. They retain
no response body, arbitrary condition text, or full query-bearing URL.
Non-attempted rows have zero attempts/bytes and no execution, result, or error
identity.

Per-row and aggregate byte counts must remain within M7d reservations and the
M7h aggregate ceiling. Counts, result indexes, status identities, admission,
metadata authority flags, and blockers are all recomputed during validation.

### Metadata and remaining gates

Live M7h metadata authorizes only the completed direct-CSV attempts represented
by the object. It records whether physical attempts consumed budgets, how many
requests succeeded or failed, whether provider responses produced compact
results, and that status is reconciled. Dry-run metadata keeps transport and
budget consumption false and adds `dry_run_no_transport`.

When every M7d direct-CSV logical request is admitted and succeeds, M7h closes
the outer direct-CSV transport, attempt, response-validation, parser, and result
blockers already closed by M7g. It remains non-replayable and
non-execution-ready. Mixed plans continue to expose planned-handler and non-CSV
request blockers. Batch deferral or request failure adds an explicit blocker.

M7h does not implement:

- EDR, USGS, WQP, or OGC API Features request plans or handlers;
- runtime package/version/symbol recheck coupled to handler invocation;
- handler registration or custom plugin serialization;
- public `gx_fetch()` or `gx_fetched` schema;
- manifest/request export, persistence, resume, retry, or replay; or
- parallel execution.

## Acceptance criteria

- Empty and mixed M7d plans produce exactly one status row per coverage row and
  retain the M7d-to-M7a chain byte-identically.
- Dry run deterministically computes admission and statuses while test hooks
  prove that DNS, transport, clock, throttle, cache, and filesystem work are
  not invoked.
- Three admitted direct-CSV requests execute in exact global request order with
  unique child scopes and one physical attempt per request.
- One transport or parse failure becomes one redacted terminal row while later
  unrelated requests still execute.
- Count and aggregate reserved-response ceilings deterministically defer rows
  before transport, and all observed/charged bytes remain inside M7d and M7h
  budgets.
- Every successful row maps one-to-one to one compact result; raw bodies and
  repeated request-plan chains are absent from compact results.
- Validation recomputes all identities, exact character results, statuses,
  foreign keys, indexes, counts, authority flags, and blockers; forged plan,
  child scope, data, byte, status, or metadata facts fail closed.
- Sensitive performer messages and query values are absent from returned
  values, printing, and typed trace-redacted failures.
- The constructor, validator, compact result, and `gx_fetch()` remain
  unexported.

## Consequences

- M7 now has a bounded direct-CSV multi-request execution substrate and an
  exact all-distribution status contract suitable for later `gx_fetched`
  design work.
- Dry run can preview actual admission and deferral without touching host or
  provider state beyond validation of bundled package assets.
- Sequential continue-on-error semantics are explicit and tested; retry,
  resume, and parallel scheduling remain later decisions.
- Compact results prevent quadratic plan duplication and discard raw provider
  bodies, so future persistence must decide separately whether and how raw
  evidence is retained.
- Public fetch remains gated until non-CSV handlers, atomic runtime preflight,
  registration, serialization, and replay contracts are fixture-backed.
