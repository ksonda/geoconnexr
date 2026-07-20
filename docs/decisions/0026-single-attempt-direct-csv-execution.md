# ADR 0026: Execute one direct-CSV request through the package transport

- Status: Accepted internal substrate
- Date: 2026-07-20
- Owners: package maintainers

## Context

M7d allocates one physical-attempt reservation to each admitted direct-CSV
logical request. M7e can validate a caller-supplied response envelope, and M7f
can parse those exact bytes into a bounded character table. None of those
contracts authorizes transport, binds a timeout, observes a provider response,
charges a reservation, or creates an attempt identity and ledger row.

The package request core already provides the required network safety boundary:
DNS is revalidated immediately before a physical attempt, the selected public
IPv4 address is pinned to the connection, redirects and transparent response
decoding are disabled, response bytes are streamed under a hard ceiling, and
physical attempts are recorded. Reimplementing those controls in M7 would
create a second transport policy and make the attempt ledger disagree with the
rest of the package.

The smallest honest execution checkpoint is one direct-CSV request with one
physical attempt. It must consume only the reservation selected by M7d, feed
the observed response through M7e and M7f before returning success, and remain
internal until multi-request orchestration and the other handlers exist.

## Decision

Add the unexported internal M7g S3 class `gx_csv_execution`, contract version
0.1.0. Construction requires:

1. one valid M7d request plan;
2. one direct-CSV `logical_request_id` present exactly once;
3. an explicit positive parser `max_fields` ceiling;
4. an explicit timeout in seconds;
5. an explicit nonnegative per-host minimum interval; and
6. an opaque 64-lowercase-hex execution-scope identity supplied by the future
   orchestrator.

The exact top-level fields are:

```text
contract_version, parsed_response, execution, attempts, metadata
```

`parsed_response` is the byte-identical valid M7f result created from the
provider response. Its nested M7e object remains unchanged and therefore keeps
its deliberately narrow statement that the response candidate supplied to M7e
does not, by itself, prove transport. M7g supplies that proof separately by
binding the M1 physical-attempt record to the same logical request, target,
body digest, byte counts, and completion time.

### Transport policy

M7g derives the full canonical target from M7a instead of accepting a URL. It
uses the package-owned request executor with the following exact policy:

- method `GET`;
- M7d's `Accept` value and `Accept-Encoding: identity`;
- empty body;
- cache disabled;
- redirects disabled;
- retries disabled;
- exactly one possible physical attempt;
- the explicit timeout and per-host interval supplied to M7g; and
- an attempt byte ceiling equal to the minimum of M7d's response, encoded, and
  decoded byte ceilings for the selected logical request.

The existing request core revalidates DNS, rejects non-public targets, pins a
public IPv4 address, streams identity bytes under the ceiling, and rejects a
changed final URL. M7e runs as the response validator before the request core
may return success. M7f then parses the retained bytes. A status, header,
target, byte, or CSV failure therefore cannot produce a successful M7g object.

M7g never reads from or writes to the HTTP cache. It does not add credentials
to the source URL and does not persist the full query-bearing target in its own
fields, printing, warnings, or errors.

### Execution and attempt identity

`execution` has the exact fields:

```text
execution_scope_id, execution_id, logical_request_id, reservation_id,
distribution_id, started_at, completed_at, timeout_seconds,
min_interval_seconds, encoded_bytes, decoded_bytes,
physical_attempt_count, execution_status
```

Times are canonical UTC text with six fractional digits. `execution_id` uses
`gx_contract_hash()` under namespace `geoconnexr.csv-execution.v1` and binds
the scope identity, logical request, reservation, distribution, start time,
timeout, and interval. The scope identity is an orchestration input, not an
authenticity claim; reusing it is forbidden by the future orchestrator but
cannot be detected globally by this local object.

`attempts` contains exactly one row with these columns:

```text
contract_version, attempt_number, attempt_id, execution_id,
logical_request_id, reservation_id, method, canonical_url_redacted,
resolved_host, resolved_ip, status, outcome, media_type, encoded_bytes,
decoded_bytes, body_sha256, completed_at
```

The row must represent one physical network response, status 200, and the exact
body accepted by M7e. `attempt_id` uses namespace
`geoconnexr.csv-attempt.v1` and binds the execution identity, attempt number,
full re-derived canonical target, pinned host/IP, status, media type, exact
byte counts, body digest, and completion time. It is not a manifest request
hash or proof that the remote data are correct or authentic.

The attempt ledger stores only the redacted canonical URL. Whole-object
validation re-derives the full target from the embedded M7a plan when checking
the attempt identity.

### Metadata and remaining gates

M7g metadata sets `host_specific`, `transport_authorized`,
`execution_completed`, `provider_response_observed`, `budgets_consumed`,
`response_candidate_validated`, `parser_executed`,
`csv_semantics_validated`, `result_contract_bound`, and
`attempt_ledger_bound` true. It keeps `replayable` and `execution_ready` false;
the completed object is evidence of one execution, not authority to execute it
again. `observation_origin` is `provider_transport`.

M7g removes these outer blockers from M7f:

- `attempt_identity_unbound`;
- `attempt_ledger_unbound`;
- `provider_transport_unauthorized`;
- `response_origin_unbound`;
- `timeout_policy_unbound`; and
- `transport_adapter_unimplemented`.

The byte-identical nested contracts retain their original metadata. The outer
object keeps the arbitrary-provider, planned-handler, non-CSV request,
runtime-package preflight, serialization, replay, and public orchestration
gates. Direct CSV does not make EDR, USGS, WQP, or OGC API Features executable.

### Failure semantics

Transport, policy, response, and parse failures are rethrown as typed,
trace-redacted M7g conditions. When a physical attempt occurred, the condition
may carry only the bounded redacted M1 attempt facts; it never carries response
body bytes or a full query-bearing URL. A failed execution consumes real
network capacity even though no successful M7g value object is returned.

## Acceptance criteria

- A deterministic mocked public target produces exactly one M1 physical
  attempt, one M7g ledger row, one M7e validation, and one M7f character result.
- The embedded M7f-to-M7a chain remains byte-identical after whole-object
  validation, and the nested M7a request list remains empty.
- Method, headers, empty body, DNS-pinned target, disabled redirects/retries and
  cache, timeout, interval, and the minimum applicable byte ceiling reach the
  performer exactly.
- Attempt and execution identities bind scope, logical request, reservation,
  distribution, target, resolved host/IP, response facts, bytes, body digest,
  times, timeout, and interval without exposing query values.
- Invalid scope, timeout, interval, logical identity, DNS result, changed URL,
  status, media, encoding, length, byte ceiling, CSV syntax, or forged nested or
  owned facts fails under typed redacted conditions.
- No cache directory or other file is created; one call performs at most one
  network attempt and never retries.
- The constructor, validator, transport adapter, and `gx_fetch()` remain
  unexported.

## Consequences

- M7 has its first provider-observed, budget-consuming result with a bounded
  attempt ledger, while public multi-request execution remains gated.
- Direct CSV now uses the same DNS, connection pinning, streaming, throttle,
  response, and attempt-accounting boundary as the protocol clients.
- Failed transport or parsing after transport is observable as a failure and
  may still consume the reserved attempt; retry and resume semantics remain a
  future orchestration decision.
- Other handlers still require fixture-backed request mappings, atomic runtime
  package/symbol checks where applicable, and equivalent ledger bindings.
