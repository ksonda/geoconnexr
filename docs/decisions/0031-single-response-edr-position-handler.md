# ADR 0031: Execute one EDR position CoverageJSON response

- Status: Accepted internal substrate
- Date: 2026-07-20
- Owners: package maintainers

## Context

M7d reserves one physical attempt and bounded encoded/decoded capacity for each
selected handler. M7k schedules direct CSV, WQP Result, and OGC API Features
under those reservations, but an EDR distribution still ended as
`handler_unimplemented`.

OGC API - Environmental Data Retrieval defines a `position` query at
`/collections/{collectionId}/position`. The query requires a WKT coordinate and
can carry parameter, datetime, CRS, and representation fields. The optional R
package `edr4r` exposes both `edr_position()` and
`covjson_to_tibble()`. Delegating transport to `edr_position()` would hide the
DNS pinning, byte ceiling, and one-attempt evidence required by M7d. The
normalizer can instead operate offline on an already retained CoverageJSON
object.

## Decision

Add the unexported `gx_edr_request_plan` and `gx_edr_execution` S3 contracts,
both version 0.1.0. Upgrade the unexported `gx_fetch_orchestration` contract to
0.3.0 so EDR joins CSV, WQP, and OGC in the global scheduler. The portable
registry remains `planned`; this internal execution proof does not register a
public handler.

### Exact position request planning

Planning requires one selected `edr` distribution with an exact M7d
`handler_reserved` coverage row and matching `held_deferred_handler`
reservation. The source must be one safely canonicalized HTTPS endpoint shaped
as `/collections/{safe-id}/position` and must contain exactly one `coords` and
one `parameter-name`. Fragments, duplicate names, unreviewed query fields, and
ambiguous multi-parameter values fail closed.

M7l accepts one finite, two-dimensional WKT `POINT(longitude latitude)` in
CRS84. An inherited `datetime` must exactly equal the M7d UTC interval; an
inherited CRS must be either `CRS84` or its canonical OGC URI
`http://www.opengis.net/def/crs/OGC/1.3/CRS84`; and an inherited representation must be
CoverageJSON or JSON. The deterministic request always records and binds:

- the redacted base URL, collection, and `position` query type;
- canonical WKT, longitude, latitude, and one exact parameter name;
- the exact UTC interval and interval-form `datetime`;
- the accepted CRS84 spelling and `f=CoverageJSON` or `f=json` inherited from
  the reviewed source (defaulting to `CRS84` and `CoverageJSON`);
- GET with CoverageJSON/JSON acceptance and identity encoding;
- no added credentials, body, redirect, retry, cache, or pagination follow;
- one physical attempt and the held byte, row, column, and field ceilings; and
- `edr4r >= 0.1.1`, `edr_position`, and `covjson_to_tibble` capability facts.

### One package-owned transport attempt

Immediately before provider work, execution resolves the required `edr4r`
version and both functions. Missing or old packages and missing/non-callable
symbols fail as `gx_error_edr_execution_capability` before DNS or transport and
consume no attempt or bytes.

geoconnexr owns the HTTP request. It revalidates and pins public DNS, enforces
the held transfer ceiling, bypasses cache, rejects redirects or a changed final
URL, and retries zero times. A response must be status 200, CoverageJSON or
JSON (`application/prs.coverage+json`, `application/vnd.cov+json`, or
`application/json`), identity encoded, internally length-consistent, and within all byte
ceilings before parsing. `edr_position()` is capability-checked but not invoked,
because its transport is outside the M7d attempt ledger.

### Strict CoverageJSON and normalizer agreement

M7l deliberately implements one small CoverageJSON subset:

- one `Coverage`, whose optional identifier defaults to the stable
  within-document identifier `"1"` used by `edr4r`;
- one inline `Domain` with `domainType=PointSeries`;
- exact `x`, `y`, and `t` axes, with the planned point coordinates; RFC 3339
  UTC is preferred, while pygeoapi's timezone-less ISO date-times are treated
  as UTC to match the reviewed normalizer;
- exactly the planned parameter in both `parameters` and `ranges`;
- one numeric/integer `NdArray`, either on `t` alone or on the exact
  `t`, `y`, `x` PointSeries axes with singleton spatial dimensions, whose
  value count equals the time axis; and
- bounded localized parameter/unit labels and numeric or null values.

The package parser normalizes that subset to a fixed nine-column tibble:
`coverage_id`, `parameter`, `parameter_label`, `unit`, `datetime`, `x`, `y`,
`z`, and `value`. JSON nesting/member, row, column, field, input-byte, and UTF-8
limits are checked, and duplicate object members are rejected throughout.
It then calls `covjson_to_tibble()` on the already parsed in-memory document
with POSIXct datetimes. The external result must equal the strict result exactly.
The optional package cannot initiate transport during this normalization step.

Malformed CoverageJSON, coordinate/parameter/shape disagreement, unsupported
domains or ranges, external-normalizer warnings/errors, and result disagreement
become one charged parse failure. Successful execution retains bounded raw
bytes, schema/data, parse hashes, implementation facts, execution facts, and
one charged attempt row.

### Scheduling, compaction, and authority

The 0.3.0 scheduler derives EDR at its original M7d `fetch_order` and admits it
with CSV, WQP, and OGC under one request-count and aggregate reserved-byte pass.
Capability, transport, and parse failures map to
`edr_capability_failed`, `edr_transport_failed`, or `edr_parse_failed`, and do
not stop later candidates.

A compact EDR success drops the repeated M7d-to-M7a chain but retains the exact
response and result evidence. Whole-object validation rebuilds the request,
strictly reparses the retained CoverageJSON without loading `edr4r`,
reconstructs the execution, and rebinds scopes, identities, attempts, bytes,
counts, statuses, and metadata.

M7l is internal, host-specific after live work, non-replayable, single-response,
and not generally execution-ready. It does not implement EDR area, cube,
radius, trajectory, corridor, locations, instances, multi-parameter requests,
pagination, portable serialization/replay, a public fetched-result schema, or
`gx_fetch()`.

## Acceptance criteria

- Offline planning records exact base, collection, position, point, parameter,
  UTC interval, CRS84, representation, limits, and reservation facts.
- Foreign paths, fragments, duplicate/extra queries, invalid points, multiple
  parameters, and conflicting datetime/CRS/format values fail before host work.
- Invocation checks `edr4r >= 0.1.1`, `edr_position`, and
  `covjson_to_tibble` before DNS; a capability failure consumes no attempt.
- One fixture response is DNS-pinned, byte-capped, identity encoded,
  cache/redirect/retry-free, and represented by one charged attempt.
- Strict PointSeries normalization and `covjson_to_tibble()` must agree exactly.
- Response-envelope, CoverageJSON, shape, coordinate, parameter, external
  normalizer, and limit failures are typed and redacted.
- Compact evidence fully revalidates without reinvoking the optional package;
  forged request, bytes, result, schema, parse, implementation, attempt, status,
  or metadata facts fail closed.
- Global dry run plans CSV, WQP, EDR, and OGC in fetch order without host or
  provider work; a failed EDR request does not prevent a later OGC request.
- M7l constructors, handler, validator, scheduler, and `gx_fetch()` remain
  unexported.

## Consequences

- M7 now has four fixture-backed execution paths under one scheduler and exact
  all-distribution status contract.
- Optional-package semantics are checked at invocation while geoconnexr retains
  transport, attempt, and byte authority.
- The intentionally narrow PointSeries subset is explicit rather than implying
  general EDR or CoverageJSON support.
- M7 is not complete. Current/legacy USGS adapters, multi-response budgets,
  registration, serialization/replay, and a public fetched-result schema remain
  gates before M8 harmonization begins.

## Primary references

- [OGC API - Environmental Data Retrieval, Part 1: Core](https://docs.ogc.org/is/19-086r6/19-086r6.html)
- [OGC CoverageJSON standard](https://www.ogc.org/standards/coveragejson/)
- [`edr4r` on CRAN](https://CRAN.R-project.org/package=edr4r)
