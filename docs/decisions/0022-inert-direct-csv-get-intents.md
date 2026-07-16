# ADR 0022: Record inert direct-CSV GET intents before transport

- Status: Accepted internal substrate
- Date: 2026-07-16

## Context

M7a deterministically selects distributions but requires its `requests` member
to remain exactly empty. M7b reports host package-version capability separately
and never grants execution authority. Direct CSV is the narrowest next handler
case because describing retrieval of the represented resource does not require
a provider-specific parameter mapping, server-side time filter, or pagination
contract.

A GET description is still not permission to perform provider transport.
Request and physical-attempt ledger identities, redirect and credential
behavior, response media acceptance, aggregate byte allocation, and bounded CSV
parsing remain unresolved. Treating an inert description as an executable
request would bypass those gates and would also make deterministic intent
construction depend on M7b's machine-local package state.

## Decision

Add the unexported internal M7c S3 class `gx_csv_get_intents`, contract version
0.1.0, as a separate host-independent value object. Its exact top-level fields
are:

```text
contract_version, plan, policy, intents, coverage, metadata
```

Construction embeds and revalidates a byte-identical M7a plan. It does not
consume or modify M7a, depend on an M7b report, or populate the plan's exact
empty `requests` list.

### Exact inert policy

The `policy` member is an exact named list with these fields and values:

```text
slice_id         = "direct_csv_get_v1"
method           = "GET"
accept           = "text/csv, application/csv;q=0.9"
accept_encoding  = "identity"
body_bytes       = 0L
body_sha256      = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
credential_policy = "unbound"
redirect_policy  = "unbound"
cache_policy     = "unbound"
parser_policy    = "unbound"
```

The empty-body digest is recomputed during validation. These are exact policy
facts only. They do not authorize transport, select a credential, redirect, or
cache policy, declare an accepted response type, or allocate parser or byte
budgets. In particular, a query-bearing source may already contain a
provider-defined access value; `credential_policy = "unbound"` does not call it
credential-free.

### Intent table and identity

The intent table contains exactly one row for each selected distribution whose
bound handler is `csv`, ordered by the plan's global `fetch_order`. Its exact
columns are:

```text
contract_version, intent_order, intent_id, distribution_id, fetch_order,
handler_id, declared_media_type, canonical_url_redacted, intent_status
```

Every row has `handler_id = "csv"` and `intent_status = "inert"`.
`declared_media_type` is the source plan value and may be missing. The fixed
method, headers, body, credential posture, and unbound policies live only in the
shared exact `policy` member.

The full target is re-derived during construction and validation from the
embedded plan's selected `distribution_url` through
`gx_safe_target(..., resolve_dns = FALSE)`: scheme and host are canonical, a
default port and fragment are removed, and the canonical query is retained.
M6c rejects a noncanonical default port before M7a, while an admitted fragment
is excluded from transport identity here; M7c never repairs an invalid plan.
That full canonical target is bound into `intent_id` but is not copied into the
intent table. `canonical_url_redacted` stores only the package-redacted form;
the raw query-bearing URL remains only in the embedded M7a plan.

`intent_id` is produced by `gx_contract_hash()` with
`namespace = "geoconnexr.csv-get-intent.v1"` and contract version 0.1.0. Its
ordered input is the following sequence of labeled scalar pairs:

```text
distribution_id, <distribution_id>
fetch_order, <fetch_order>
handler_id, <handler_id>
declared_media_type, <declared_media_type>
canonical_url, <full offline-canonical URL>
slice_id, <policy$slice_id>
method, <policy$method>
accept, <policy$accept>
accept_encoding, <policy$accept_encoding>
body_bytes, <policy$body_bytes>
body_sha256, <policy$body_sha256>
credential_policy, <policy$credential_policy>
redirect_policy, <policy$redirect_policy>
cache_policy, <policy$cache_policy>
parser_policy, <policy$parser_policy>
```

M7c uses the existing `gx_contract_hash()` canonicalization; it defines no
separate UTF-8/LF serialization. The resulting digest is intent identity only.
It is not an HTTP cache key, logical request ID, physical-attempt ID, manifest
request hash, or provenance claim.

### Coverage and metadata

The coverage table projects every M7a distribution in selection order. Its
exact columns are:

```text
contract_version, selection_order, fetch_order, distribution_id, handler_id,
selected, plan_decision, intent_id, intent_status
```

`intent_id` is present only for a selected CSV row. Coverage status is exactly:

- `intent_created` for a selected `csv` distribution;
- `deferred_handler` for a selected non-CSV distribution;
- `reference_only` for an M7a reference-only distribution; or
- `not_selected` for every other unselected distribution.

Coverage and intent foreign keys, ordering, one-to-one CSV cardinality, and
counts reconcile with the embedded plan. No time, variable, credential, or
other query parameter is appended; bounded local filtering remains future
parser work.

Metadata has the exact fields:

```text
host_specific, replayable, execution_ready, transport_authorized,
budgets_allocated, counts, non_replayable_reasons
```

All five flags are `FALSE`. `counts` has the exact integer fields
`distributions`, `selected`, `intents`, `intent_created`, `deferred_handler`,
`not_selected`, `reference_only`, and `requests`. Distribution statuses
partition `distributions`; selected rows partition into `intent_created` and
`deferred_handler`; `intents` equals `intent_created`; and `requests` is zero.

Non-replayability reasons are the complete M7a reason set plus these byte-sorted
tokens:

- `attempt_ledger_unbound`;
- `cache_policy_unbound`;
- `credential_policy_unbound`;
- `parser_limits_unbound`;
- `provider_transport_unauthorized`;
- `redirect_policy_unbound`;
- `request_budgets_unallocated`; and
- `response_contract_unproven`.

M7c does not assign per-intent encoded- or decoded-byte ceilings, row limits,
column limits, or reservations against the M7a aggregate budgets. Intent count
is not request count. Intents and coverage retain the M7a distribution ceiling,
and M7c-owned text has a separate 128 MiB aggregate ceiling.

Construction and validation call no handler or package symbol, do not inspect
or load `readr` or any other optional package, and perform no DNS lookup,
provider request, redirect, cache access, response parsing, or file write.
Revalidating the M7a plan may reread its bounded bundled handler assets. M7c
introduces no public constructor, planning or fetch API, serialized schema,
execution path, or replay authority.

## Acceptance criteria

- Empty and populated M7a plans produce exact bounded objects; the embedded plan
  remains byte-identical and request-empty.
- The exact policy, intent columns, coverage columns, metadata flags, counts,
  and non-replayability reasons revalidate without trusting mutable fields.
- Selected CSV distributions map one-to-one to inert intents in global fetch
  order; repeated catalog variable rows still produce one distribution intent.
- Mixed-handler, reference-only, and unselected distributions remain explicit
  in coverage, and all foreign keys, statuses, order, and counts reconcile.
- Full targets are re-derived from the embedded plan and bound through the exact
  `gx_contract_hash()` input; the intent table stores only redacted URLs.
- Query values remain only in the embedded M7a plan and are absent from the
  intent table, printing, warnings, errors, and traces.
- Results are independent of M7b and installed optional packages; no package
  satisfaction state upgrades an intent.
- Construction and validation perform no DNS, network, cache, package, handler,
  redirect, provider-response parsing, CSV parsing, or write operation.
- Aggregate request/byte budgets and parser limits remain visibly unallocated,
  all authority flags remain false, and no intent is represented as a request
  or ledger attempt.
- Exact bounded synthetic fixtures cover the shared default-port canonicalizer,
  M7c fragment exclusion, query retention/redaction, media-type-only
  classification, duplicate variables, handler precedence, and unselected CSV
  decisions.
- Forged object shape, policy, hashes, URLs, links, statuses, counts, blockers,
  or authority flags fail closed under typed redacted conditions.
- No M7c, request-planning, fetch, execution, or serialization API is exported.

## Consequences

- M7 now pins deterministic direct-CSV method, target, representation
  preference, empty-body, and policy identity without authorizing external work.
- M7a selection, M7b package capability, and M7c intent construction remain
  separate contracts with different determinism and replay properties.
- ADR 0023 consumes M7c as a byte-identical input and allocates aggregate
  request and byte reservations, binds a non-executable direct-CSV logical
  request and response-shape contract, and records explicit row and column
  limits. It does not mutate M7c, implement transport or response validation,
  define CSV parser semantics, or create physical-attempt or ledger identity.
- Other handlers still require fixture-backed provider-specific mappings and
  request contracts.
