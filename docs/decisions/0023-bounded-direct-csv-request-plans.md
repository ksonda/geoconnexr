# ADR 0023: Allocate bounded non-executable direct-CSV request plans

- Status: Accepted internal substrate
- Date: 2026-07-16

## Context

M7a selects distributions deterministically and records aggregate request,
encoded-byte, and decoded-byte ceilings, but its `requests` member must remain
exactly empty. Its `max_requests` ceiling counts physical transport attempts,
not intents or table rows. M7c separately records one inert GET intent for each
selected direct-CSV distribution without allocating those ceilings or binding
a response shape.

The next checkpoint must make budget contention explicit before any handler can
execute. Allocating only among CSV rows would let that handler consume request
and byte capacity that belongs to earlier selected non-CSV rows. Conversely,
turning a reservation into an executable request would bypass runtime package,
transport, response-validation, attempt-identity, ledger, and parser gates.
Parser limits also require product input: this checkpoint must not invent a
default response size, row count, or column count.

## Decision

Add the unexported internal M7d S3 class `gx_csv_request_plan`, contract version
0.1.0, as a bounded, host-independent allocation value object. Its exact
top-level fields are:

```text
contract_version, intent_set, policy, budgets, reservations, request_plans,
coverage, metadata
```

Construction requires an M7c intent set plus explicit `max_response_bytes`,
`max_rows`, and `max_columns` inputs. It embeds and revalidates M7c
byte-for-byte; therefore the nested M7a plan is also byte-identical and its
exact `requests` list remains empty. M7d does not consult or embed M7b and does
not supply product defaults for the three required limits.

### Bound policy, not an executor

The exact `policy` fields are:

```text
slice_id, method, accept, accept_encoding, body_bytes, body_sha256,
credential_policy, redirect_policy, max_redirects, retry_policy, max_retries,
max_physical_attempts, cache_policy, success_status, response_media_types,
response_content_encoding, allocation_policy, max_response_bytes, max_rows,
max_columns, parser_policy
```

The fixed values are:

```text
slice_id                    = "direct_csv_request_plan_v1"
method                      = "GET"
accept                      = "text/csv, application/csv;q=0.9"
accept_encoding             = "identity"
body_bytes                  = 0L
body_sha256                 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
credential_policy           = "source_url_opaque_no_additional_credentials"
redirect_policy             = "reject"
max_redirects               = 0L
retry_policy                = "none"
max_retries                 = 0L
max_physical_attempts       = 1L
cache_policy                = "bypass"
success_status              = 200L
response_media_types        = c("text/csv", "application/csv")
response_content_encoding   = "identity"
allocation_policy           = "global_selected_fair_share_v1"
parser_policy               = "shape_limits_only"
```

`max_response_bytes`, `max_rows`, and `max_columns` are the caller's explicit
positive integer inputs.

The request representation preserves M7c's GET method, CSV `Accept` value,
identity request encoding, and zero-byte body and digest. The canonical source
target is opaque: no query parameter or credential is added. Redirects are
rejected with a maximum of zero; retries are disabled with a maximum of zero;
each reservation permits exactly one physical attempt; and cache use is
bypassed. The response contract accepts only status 200, media type `text/csv`
or `application/csv`, and identity content encoding.

The explicit response-byte, row, and column values are hard shape ceilings.
They do not define delimiter, quoting, escaping, header, encoding, missing-value,
type-inference, schema, or parser-engine semantics, and no parser is invoked.
Binding an expected response contract does not implement response validation.

### Global physical-attempt and byte allocation

Allocation considers every selected M7a distribution in global `fetch_order`,
not only CSV distributions. Let `K` be the minimum of:

- the selected-distribution count;
- M7a `max_requests`;
- the whole-byte M7a encoded-byte budget; and
- the whole-byte M7a decoded-byte budget.

The first `K` selected distributions receive reservations. This consumes one
physical-attempt slot per reservation. A reservation for a non-CSV handler is
held for that handler, so a CSV plan cannot steal its request or byte share.
Selected distributions after the prefix remain explicitly budget-deferred.

Encoded and decoded bytes are allocated independently. For each kind, the
reserved total is the lesser of its M7a aggregate budget and
`K * max_response_bytes`. Integer quotient/remainder allocation divides that
total across all `K` reservations: every reservation receives the quotient and
the earliest rows in global fetch order receive one additional byte until the
remainder is exhausted. The exact `budgets` fields are:

```text
source_budgets, reserved_requests, reserved_encoded_bytes,
reserved_decoded_bytes, remaining_requests, remaining_encoded_bytes,
remaining_decoded_bytes
```

Reserved plus remaining values reconcile exactly to the unchanged source
budgets. Encoded and decoded shares can differ. Empty input and any zero
request or byte ceiling produce no reservation and do not divide by zero.

The `reservations` table has the exact columns:

```text
contract_version, reservation_order, reservation_id, distribution_id,
fetch_order, handler_id, max_physical_attempts, max_encoded_bytes,
max_decoded_bytes, reservation_status
```

Its status is exactly `csv_request_planned` for a reserved CSV distribution or
`held_deferred_handler` for a reserved non-CSV distribution. Reservation IDs
domain-separate and bind distribution/order and handler identity, reservation
status, source budgets, allocated attempt/byte ceilings, allocation policy, and
the per-response cap. `gx_contract_hash()` uses namespace
`geoconnexr.fetch-budget-reservation.v1` and contract version 0.1.0. These are
allocation identities, not physical-attempt identities or ledger entries.
Every whole-byte value enters the hash as an unsigned base-10 digit string,
never through R's option-sensitive numeric display.

### Direct-CSV logical request plans

Only a reserved CSV distribution with a matching M7c intent receives a logical
request-plan row. The exact columns are:

```text
contract_version, request_order, logical_request_id, intent_id, reservation_id,
distribution_id, fetch_order, handler_id, method, canonical_url_redacted,
declared_media_type, max_physical_attempts, max_encoded_bytes,
max_decoded_bytes, response_byte_limit, max_rows, max_columns, request_status
```

Every row has `handler_id = "csv"` and
`request_status = "planned_non_executable"`. The full target is re-derived
offline from the embedded M7a plan, used only as an identity input, and never
copied to the request table. `canonical_url_redacted` stores the package-redacted
target. A domain-separated `logical_request_id` binds the M7c intent, full
canonical target, reservation, exact policy, byte ceilings, and row/column
limits. `gx_contract_hash()` uses namespace
`geoconnexr.csv-logical-request.v1` and contract version 0.1.0. The result is
not a cache key, physical-attempt ID, attempt-ledger row, manifest request hash,
execution record, or provenance claim.
Its whole-byte ceiling inputs use the same option-independent unsigned base-10
digit representation as reservation identity.

`response_byte_limit` is the lesser of that reservation's encoded and decoded
shares. Thus the logical response cannot exceed either global allocation even
when those independently divided budgets differ.

`declared_media_type` remains the source-catalog fact copied through M7c. It is
advisory and does not expand the exact accepted response-media set.

M7d adds no credential to an existing source query and does not interpret a
provider-defined query value as a package credential. Query values remain only
inside the nested M7a plan and the re-derived identity input; they are absent
from M7d-owned tables, printing, warnings, errors, and traces.

### Coverage and metadata

Coverage retains every M7a distribution in selection order. Its exact columns
are:

```text
contract_version, selection_order, fetch_order, distribution_id, handler_id,
selected, plan_decision, intent_id, reservation_id, logical_request_id,
request_status
```

Coverage status is exactly:

- `csv_request_planned` for a reserved selected CSV distribution;
- `csv_budget_deferred` for an otherwise selected CSV distribution outside the
  reserved prefix;
- `handler_reserved` for a reserved selected non-CSV distribution;
- `handler_budget_deferred` for an otherwise selected non-CSV distribution
  outside the reserved prefix;
- `reference_only` for an M7a reference-only distribution; or
- `not_selected` for every other unselected distribution.

Only `csv_request_planned` coverage rows have all three intent, reservation,
and logical-request foreign keys. `handler_reserved` rows have a reservation
but no CSV intent or logical request. Budget-deferred rows have no reservation
or logical request; selected CSV rows retain their M7c intent link.

Metadata has the exact fields:

```text
host_specific, replayable, execution_ready, transport_authorized,
budgets_allocated, budgets_consumed, allocation_complete, counts,
non_replayable_reasons
```

`host_specific`, `replayable`, `execution_ready`, and
`transport_authorized` are `FALSE`; `budgets_allocated` and
`allocation_complete` are `TRUE`; and `budgets_consumed` is `FALSE`.
Allocation completeness means only that the deterministic reservation pass is
complete. It does not mean a request is executable or any budget has been
consumed.

`counts` has the exact fields:

```text
distributions, selected, intents, reservations, request_plans,
csv_request_planned, csv_budget_deferred, handler_reserved,
handler_budget_deferred, not_selected, reference_only,
physical_attempts_reserved, requests_executed, physical_attempts_executed
```

Coverage statuses partition distributions, reservation and request-plan counts
reconcile with their foreign keys, `physical_attempts_reserved` reconciles with
reservation limits, and both execution counts remain zero.

Reservation, request-plan, and coverage cardinality remains bounded by the M7a
distribution ceiling. M7d-owned text has a separate 128 MiB aggregate ceiling;
the byte allocations above remain response-capacity reservations and are not
used as object-storage allowances.

The active non-replayability blockers are byte-sorted and exact:

- `arbitrary_provider_client_unimplemented`;
- `attempt_identity_unbound`;
- `attempt_ledger_unbound`;
- `csv_parser_enforcement_unimplemented`;
- `csv_parser_semantics_unbound`;
- `handler_implementations_planned`;
- `non_csv_request_plans_absent`;
- `provider_transport_unauthorized`;
- `response_validator_unimplemented`;
- `result_schema_unbound`;
- `runtime_package_preflight_required`;
- `serialization_unbound`;
- conditionally, `source_catalog_incomplete`;
- `timeout_policy_unbound`; and
- `transport_adapter_unimplemented`.

M7d resolves M7c's allocation, credential, redirect, retry, cache,
response-shape, and parser-limit unknowns only to the extent stated above. It
does not claim that response validation or CSV parsing exists, does not bind
attempt identity or a ledger, binds no timeout, and does not upgrade any
arbitrary-provider handler.

Construction and validation perform no DNS lookup, network request, redirect,
cache read or write, optional-package inspection, handler call, transport,
provider-response validation, CSV parsing, or file write. Revalidating the
nested M7a plan may reread its bounded bundled handler assets. M7d introduces no
public constructor, planning or fetch API, public schema, execution path,
serialization contract, or replay authority.

## Acceptance criteria

- Empty, zero-budget, CSV-only, mixed-handler, and budget-constrained inputs
  produce exact bounded objects while nested M7c and M7a remain byte-identical
  and M7a requests remain empty.
- Required response-byte, row, and column ceilings reject missing, zero,
  fractional, non-finite, out-of-range, incorrectly typed, or forged values;
  no product default is substituted.
- Global selection order, request-prefix allocation, independent encoded and
  decoded quotient/remainder shares, and held non-CSV reservations are exact
  and deterministic under catalog-row permutation.
- Reservation IDs domain-separate and rebind their allocation inputs; logical
  request IDs separately rebind their full inputs, including the re-derived
  query-bearing target, without copying sensitive query values into M7d-owned
  tables or conditions. Whole-byte identity remains invariant under numeric
  display options.
- Reservation, request-plan, and coverage shapes, types, attributes, statuses,
  order, foreign keys, budgets, counts, authority flags, and byte-sorted
  blockers revalidate without trusting mutable fields.
- Only reserved CSV intents receive `planned_non_executable` request plans;
  held non-CSV shares and all budget-deferred selected rows remain visible.
- Redirect, retry, physical-attempt, cache, success-status, response-media,
  response-encoding, byte, row, and column constraints are bound exactly, but
  no response validator, parser semantics, or parser enforcement is claimed.
- Construction and validation perform no DNS, network, redirect, cache,
  optional-package, handler, transport, response-validation, CSV-parsing, or
  write operation.
- Forged nested objects, targets, policies, reservations, hashes, limits,
  coverage, counts, blockers, or authority flags fail closed under typed,
  redacted conditions.
- No M7d request-planning, fetch, execution, schema, serialization, or replay
  API is exported.

## Consequences

- M7 now has a deterministic global allocation checkpoint that cannot let
  direct CSV consume capacity reserved for earlier non-CSV selections.
- Direct-CSV logical requests have exact inert request-policy and response-shape
  facts, but remain separated from transport, physical attempts, and
  provenance.
- Runtime package/symbol preflight, transport adaptation, DNS revalidation,
  response validation, result schema, parser semantics/enforcement, attempt
  identity/ledger, and serialization remain mandatory later gates.
- Other handlers still require fixture-backed provider-specific request plans;
  their M7d reservations preserve capacity but do not implement them.

M7e is defined separately by
[ADR 0024](0024-offline-direct-csv-response-validation.md). It validates one
bounded caller-supplied response candidate against one logical request without
mutating M7d, proving provider or attempt provenance, consuming budgets,
authorizing transport, or parsing CSV.
