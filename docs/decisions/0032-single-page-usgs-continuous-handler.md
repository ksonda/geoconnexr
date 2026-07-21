# ADR 0032: Execute one current USGS continuous-data page

- Status: Accepted internal substrate
- Date: 2026-07-21
- Owners: package maintainers

## Context

M7d already reserves one physical attempt and bounded encoded/decoded capacity
for every selected handler. M7l schedules direct CSV, WQP Result, EDR position,
and OGC API Features under those reservations, but current USGS Water Data API
distributions still end as `handler_unimplemented`.

The current USGS OGC API exposes continuous observations through
`/ogcapi/{version}/collections/continuous/items`. The official
`dataRetrieval::read_waterdata_continuous()` function represents the supported R
capability, but its internal transport and iterative paging cannot emit the
DNS-pinned, identity-encoded, single-attempt ledger required by M7d. The API's
GeoJSON schema is public and describes measurement `value` as a string so its
transmitted precision can be retained.

## Decision

Add the unexported `gx_usgs_continuous_request_plan` and
`gx_usgs_continuous_execution` S3 contracts, both version 0.1.0. Upgrade the
unexported `gx_fetch_orchestration` contract to 0.4.0 so current USGS continuous
data joins CSV, WQP, EDR, and OGC Features in the global scheduler.

The handler registry stays portable and `planned`; the fixture-backed internal
execution proof does not register a public handler or `gx_fetch()` API.

### Exact continuous request planning

Planning requires one selected `usgs_waterdata_continuous` distribution with a
matching M7d `handler_reserved` coverage row and `held_deferred_handler`
reservation. The endpoint must be HTTPS on `api.waterdata.usgs.gov` and match
exactly `/ogcapi/{safe-version}/collections/continuous/items`.

The inherited URL must contain exactly one `monitoring_location_id` shaped as
`USGS-{alphanumeric}` and one five-digit `parameter_code`. Fragments, duplicate
names, extra filters, multiple values, `latest-continuous`, daily collections,
foreign hosts, and conflicting inherited options fail closed.

The deterministic request records and binds:

- the redacted base URL, API version, and `continuous` collection;
- one exact monitoring location and parameter code;
- the exact M7d UTC interval in `time`;
- the fixed ten-property list, `skipGeometry=true`, `f=json`, and `lang=en-US`;
- a page `limit` no larger than 50,000 or the row/field ceilings;
- GeoJSON/JSON acceptance with identity encoding;
- no added credentials, body, redirect, retry, cache, or next-page follow;
- one physical attempt and the held byte, row, column, and field ceilings; and
- `dataRetrieval >= 2.7.22` plus exported
  `read_waterdata_continuous()` capability facts.

### Capability check and package-owned transport

Immediately before provider work, execution resolves the required
`dataRetrieval` version, exported function, and reviewed formals. A missing or
old package, missing symbol, or incompatible function shape fails as
`gx_error_usgs_continuous_execution_capability` before DNS and consumes no
attempt or bytes.

geoconnexr owns the sole HTTP request. It revalidates and pins public DNS,
enforces the held transfer ceiling, requests identity encoding, bypasses cache,
rejects redirects and changed final URLs, and retries zero times. A response
must be status 200, GeoJSON or JSON, identity encoded, internally
length-consistent, and within every byte ceiling before parsing.

`read_waterdata_continuous()` is capability-checked but not invoked. Invoking
its provider transport would surrender the exact one-attempt and paging
authority. The implementation record states this split explicitly:
dataRetrieval supplies the checked ecosystem capability; geoconnexr owns
transport and normalization.

### Strict fixed GeoJSON result

M7m accepts one bounded `FeatureCollection` page. Every feature must have an
identity, null geometry, and exactly these properties:

`time_series_id`, `monitoring_location_id`, `parameter_code`, `statistic_id`,
`time`, `value`, `unit_of_measure`, `approval_status`, `qualifier`, and
`last_modified`.

Site and parameter must equal the plan. Observation and modification times must
be RFC 3339 values, and observation time must fall within the requested UTC
interval. All identifiers, status, unit, and measurement values remain strings;
only `time` and `last_modified` become UTC `POSIXct` columns. A null qualifier
becomes `NA_character_`. This preserves the API's string-valued measurements
without locale or floating-point reinterpretation.

`numberReturned` must equal the feature count. `numberMatched` may be null or a
consistent bounded whole number. JSON depth/member, duplicate-member, UTF-8,
row, column, field, and input-byte limits fail closed.

A `next` link, a larger known match count, or a full page sets `truncated=TRUE`
with an exact stop reason. No next link is followed. Successful execution
retains the bounded raw body, fixed eleven-column tibble/schema, parse hashes,
truncation facts, implementation and execution facts, and one charged attempt.

### Scheduling, compaction, and authority

The 0.4.0 scheduler derives the continuous request at its original M7d
`fetch_order` and admits it with CSV, WQP, EDR, and OGC Features under one
request-count and aggregate reserved-byte pass. Capability, transport, and
parse failures map to `usgs_continuous_capability_failed`,
`usgs_continuous_transport_failed`, or `usgs_continuous_parse_failed` and do not
stop later candidates.

A compact success drops the repeated M7d-to-M7a chain but retains the exact
response and normalized evidence. Whole-object validation rebuilds the request,
strictly reparses retained GeoJSON without loading dataRetrieval, reconstructs
the execution, and rebinds scopes, identities, attempts, bytes, counts,
statuses, and metadata.

M7m is internal, host-specific after live work, non-replayable, and single-page.
It does not implement `latest-continuous`, daily values, legacy NWIS services,
API-key policy, pagination, portable serialization/replay, a public fetched
result schema, or `gx_fetch()`.

## Acceptance criteria

- Offline planning binds the exact endpoint, site, parameter, UTC interval,
  property set, representation, limits, and M7d reservation without host work.
- Foreign paths/hosts, fragments, duplicate or extra queries, ambiguous site or
  parameter values, and conflicting inherited options fail before execution.
- Invocation checks `dataRetrieval >= 2.7.22` and the exported continuous
  function before DNS; capability failure consumes no attempt.
- One fixture response is DNS-pinned, byte-capped, identity encoded,
  cache/redirect/retry-free, and represented by one charged attempt.
- Strict parsing preserves measurement strings, validates site/parameter/time,
  exposes next-page truncation, and never follows the next link.
- Envelope, structure, duplicate-member, type, count, semantic, depth, and
  shape-limit failures are typed and redacted.
- Compact evidence fully revalidates without the optional package; forged plan,
  bytes, data, schema, parse, implementation, attempt, status, or metadata facts
  fail closed.
- Global dry run plans all five implemented handler families in fetch order
  without host work; a failed USGS request does not prevent a later OGC request.
- M7m constructors, handler, validator, scheduler, and `gx_fetch()` remain
  unexported.

## Consequences

- M7 now has five fixture-backed execution paths under one scheduler and exact
  all-distribution status contract.
- The current USGS continuous API is preferred over legacy NWIS compatibility
  paths, while the implemented subset stays narrow and explicit.
- Capability attribution is honest: dataRetrieval is checked, but package-owned
  transport and parsing preserve the M7d authority boundary.
- M7 is not complete. Current daily and legacy USGS handlers, registration,
  multi-page budgets, serialization/replay, and a public fetched-result schema
  remain gates before M8 harmonization begins.

## Primary references

- [USGS Water Data APIs: OGC API](https://api.waterdata.usgs.gov/docs/ogcapi/)
- [`read_waterdata_continuous()` reference](https://doi-usgs.github.io/dataRetrieval/reference/read_waterdata_continuous.html)
- [USGS continuous collection schema](https://api.waterdata.usgs.gov/ogcapi/v0/collections/continuous/schema?f=html)
