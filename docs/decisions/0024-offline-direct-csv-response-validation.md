# ADR 0024: Validate caller-supplied direct-CSV response candidates offline

- Status: Accepted internal substrate
- Date: 2026-07-16
- Owners: package maintainers

## Context

M7d allocates global physical-attempt and byte capacity and emits bounded,
non-executable direct-CSV logical request plans. Those plans bind status 200,
CSV response media, identity response encoding, exact-target/no-redirect
behavior, and response-byte ceilings, but M7d intentionally implements no
response validator. No later parser should consume bytes merely because a
caller says they satisfy those facts.

Transport is not ready. Attempt identity, attempt-ledger alignment, runtime
package and symbol checks, timeout policy, and provider provenance remain
unbound. A response validator can still be implemented honestly before
transport if its input is explicitly a caller-supplied in-memory candidate and
its output does not imply that a provider request occurred or that any budget
was consumed.

## Decision

Add the unexported internal M7e S3 class `gx_csv_validated_response`, contract
version 0.1.0. It validates one explicitly selected M7d logical request against
one caller-supplied response candidate without performing transport.

The exact top-level fields are:

```text
contract_version, request_plan, body, validation, metadata
```

`request_plan` is the byte-identical revalidated M7d object. M7e neither edits
M7d metadata nor fills the nested M7a request list. `body` is the exact
unclassed raw vector admitted by this boundary. Keeping the bounded bytes with
their validation facts lets the next parser checkpoint consume the same bytes
whose digest and length were checked rather than trusting a detached receipt.

### Input boundary

Construction requires:

1. one valid M7d object;
2. one exact `logical_request_id` present exactly once in M7d's direct-CSV
   request-plan table; and
3. one exact candidate list with ordered fields `status`, `headers`, `body`,
   and `url`.

This candidate shape matches the package's raw HTTP-performer boundary, but its
presence does not establish that the performer or a provider produced it.
There is no client, cache state, retrieval time, clock, decoded-byte claim, or
attempt record in the constructor.

Headers must be a named character vector or named list of scalar character
values. Validation applies fixed contract-level limits before semantic work:

- at most 64 header fields;
- at most 256 bytes per header name;
- at most 8 KiB per header value; and
- at most 64 KiB across names and values.

Names are case-insensitive ASCII HTTP tokens. Values must satisfy the package's
bounded UTF-8/control-character text contract. Only `Content-Type`,
`Content-Encoding`, and `Content-Length` are projected. Duplicate critical
headers are rejected even when their values agree. Other bounded headers are
discarded and cannot affect validation identity.

### Response rules

The selected request's full canonical target is re-derived from the embedded
M7a distribution through the offline URL-safety boundary. The candidate URL
must canonicalize to that exact target. Default ports and fragments therefore
follow the package's existing canonicalization behavior, while path, host, and
query bytes remain bound. User information, local/private targets, redirects,
and changed queries fail closed. The full URL is never copied into M7e-owned
fields.

Validation then requires:

- status exactly 200;
- one `Content-Type` whose lowercased base media type is exactly `text/csv` or
  `application/csv`;
- no comma-ambiguous media value; bounded parameters after the first semicolon
  may be present but are neither interpreted nor retained;
- absent `Content-Encoding`, normalized to `identity`, or one explicit trimmed
  case-insensitive `identity` value;
- optional `Content-Length` containing only decimal digits after surrounding
  whitespace is trimmed, with leading zeroes canonicalized and the value equal
  to the exact raw body length; and
- raw body length no greater than the logical request's encoded-byte,
  decoded-byte, and response-byte ceilings.

Under identity encoding, encoded and decoded bytes both equal `length(body)`.
The caller cannot supply a separate decoded size. Empty and non-UTF-8 bodies
may pass this envelope boundary: CSV syntax, character encoding, delimiter,
header, quoting, missing-value, type, row, column, and result-schema semantics
remain later parser work.

The exact `validation` fields are:

```text
validation_id, logical_request_id, intent_id, reservation_id, distribution_id,
status, media_type, content_encoding, content_length_present, content_length,
encoded_bytes, decoded_bytes, body_sha256, validation_status
```

`body_sha256` hashes the exact raw bytes with `serialize = FALSE`.
`validation_status` is `validated_caller_supplied`. Arbitrary response headers,
the original Content-Type parameters, the full target, retrieval time, cache
origin, and attempt facts are not retained.

### Validation identity

`validation_id` uses `gx_contract_hash()` under namespace
`geoconnexr.csv-response-validation.v1`. It binds the selected logical,
intent, reservation, and distribution identities; re-derived full canonical
target; normalized status, media type, and content encoding; Content-Length
presence and canonical value; encoded and decoded byte counts; exact body
digest; and the three applicable request byte ceilings. Whole-byte hash inputs
use unsigned base-10 digit strings and are invariant to numeric display
options.

The validation identity is not a request ID, physical-attempt ID, cache key,
ledger entry, manifest request hash, authenticity proof, provider provenance,
or CSV semantic result identity.

### Metadata and blockers

Metadata is exact:

```text
host_specific = FALSE
replayable = FALSE
execution_ready = FALSE
transport_authorized = FALSE
response_candidate_validated = TRUE
provider_response_observed = FALSE
budgets_consumed = FALSE
parser_executed = FALSE
observation_origin = "caller_supplied"
```

The outer blocker set retains M7d's byte-sorted blockers, removes only
`response_validator_unimplemented`, and adds `response_origin_unbound`. The
embedded byte-identical M7d metadata still contains its original response-
validator blocker. The resulting normal blocker set is:

- `arbitrary_provider_client_unimplemented`;
- `attempt_identity_unbound`;
- `attempt_ledger_unbound`;
- `csv_parser_enforcement_unimplemented`;
- `csv_parser_semantics_unbound`;
- `handler_implementations_planned`;
- `non_csv_request_plans_absent`;
- `provider_transport_unauthorized`;
- `response_origin_unbound`;
- `result_schema_unbound`;
- `runtime_package_preflight_required`;
- `serialization_unbound`;
- conditionally, `source_catalog_incomplete`;
- `timeout_policy_unbound`; and
- `transport_adapter_unimplemented`.

M7e-owned text has a separate 1 MiB aggregate ceiling. The raw body is governed
by the selected M7d response limits and is checked before hashing during whole-
object revalidation. Construction and validation perform no DNS lookup,
network request, redirect, cache access, package inspection, handler call,
transport, clock read, CSV parsing, or file write. Revalidating the nested M7a
plan may reread its bounded bundled handler assets.

M7e adds no public constructor, fetch API, response schema, parser API,
serialization contract, execution path, or replay authority.

## Acceptance criteria

- A known-answer synthetic candidate produces an exact object while M7d, M7c,
  and M7a remain byte-identical and the nested M7a request list stays empty.
- Status, singleton critical headers, admitted media, identity encoding,
  optional exact Content-Length, canonical final target, and all three byte
  ceilings fail closed under typed, trace-redacted conditions.
- Empty, non-UTF-8, and exact-limit raw bodies pass envelope validation without
  claiming CSV semantics; limit-plus-one bodies fail before hashing during
  revalidation.
- Header count and text budgets are applied before list-wide work; invalid
  encodings and comma-joined or duplicated critical fields produce no leaked
  warnings or values.
- Arbitrary bounded headers are discarded. Header name/order/case, admitted
  Content-Type parameters, and absent versus explicit identity encoding have
  the documented identity behavior.
- Content-Length absence versus presence, normalized media type, body bytes,
  body digest, selected request, target, and request limits rebind validation
  identity deterministically under numeric display changes.
- Full query-bearing URLs and arbitrary headers are absent from M7e-owned
  fields, printing, warnings, errors, and traces. The embedded M7d plan and
  intentional raw body remain explicitly outside that narrower claim.
- Forged nested contracts, bodies, facts, identities, attributes, metadata,
  blockers, or authority flags fail whole-object revalidation.
- Pinned synthetic fixtures state that they prove neither transport provenance
  nor CSV semantics.
- No M7e validation, fetch, parser, execution, serialization, or replay API is
  exported.

## Consequences

- Direct-CSV has a deterministic, reusable response-envelope validator before
  any transport adapter is authorized.
- Parser work can accept only the exact raw bytes admitted by this boundary,
  while still requiring separate runtime and semantic contracts.
- A validated caller-supplied candidate remains evidence about internal shape
  and bytes only. Physical-attempt identity, attempt-ledger binding, runtime
  package/symbol preflight, parser/result semantics, provider transport,
  provenance, timeout policy, serialization, execution, and replay remain
  mandatory later gates.
- M7d remains immutable and honest about the absence of its own validator; M7e
  records the narrower resolved blocker only at the outer boundary.

ADR 0025 consumes the exact retained M7e body through a strict bounded offline
UTF-8 CSV parser and character-only result contract. It does not mutate M7e,
prove provider or attempt provenance, consume fetch budgets, invoke the planned
optional handler, authorize transport, or make either object replayable.
