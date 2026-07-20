# ADR 0028: Add a reservation-bound single-page OGC API Features handler

- Status: Accepted internal substrate
- Date: 2026-07-20
- Owners: package maintainers

## Context

M7d reserves request and byte capacity for every selected handler, but only
direct CSV had a provider request plan and execution path through M7h. The
generic OGC API Features classifier could identify an items distribution, and
the package already had a bounded public client for the configured Geoconnex
reference service, but neither fact was sufficient to execute an arbitrary
provider distribution safely.

The next M7 checkpoint needs to prove the complete shape of one non-CSV
handler: provider-specific request planning, reuse of the global reservation,
an implementation capability check at the point of invocation, one charged
transport attempt, strict response parsing, and an `sf` payload whose facts can
be revalidated. It must not imply that provider query parameters, pagination,
cross-handler orchestration, registration, serialization, replay, or public
`gx_fetch()` are ready.

## Decision

Add two unexported M7i S3 contracts, both version 0.1.0:

- `gx_oaf_request_plan` records one host-independent, non-executable OGC API
  Features request plan; and
- `gx_oaf_execution` binds one provider response, one physical attempt, the
  invocation-time implementation check, and one normalized simple-feature
  result.

The implementation symbol is the internal package function `gx_handler_oaf`,
matching the reviewed `geoconnexr:native-oaf` registry entry. The registry
continues to describe executable handlers as planned because its portable
registration and public execution states are not being changed by this
internal slice.

### Compatible source boundary

Construction requires one valid M7d request plan and one selected
`ogc_api_features` distribution with a `handler_reserved` coverage row and a
matching `held_deferred_handler` reservation. The source URL must be a safe,
absolute, query-free and fragment-free HTTP(S) URL whose canonical path ends
exactly in:

```text
/collections/{canonically encoded collection id}/items
```

M7i rejects inherited query strings rather than guessing whether they contain
credentials, filters, output formats, offsets, or provider-specific behavior.
The plan adds only deterministic `f=json` and `limit={positive integer}` query
parameters. It stores redacted source and request URLs; whole-object validation
re-derives the full target from the embedded M7d distribution.

The fixed request policy is one empty-body `GET` with GeoJSON/JSON acceptance,
identity encoding, no added credentials, redirects, retries, or cache, status
200, and one physical attempt. The response-byte ceiling is the lower of the
selected M7d encoded- and decoded-byte reservations.

### Single-page semantics

M7i intentionally spends exactly one M7d physical-attempt reservation. It
requests and parses one items page and never dereferences an advertised
`rel=next` link. The response must be a bounded JSON `FeatureCollection`; the
feature array may not exceed the planned `limit`, and a present
`numberReturned` must equal its actual length.

The result records `truncated = TRUE` when a next link is advertised,
`numberMatched` exceeds the returned count, or the page exactly reaches the
limit. Its stop reason distinguishes `single_page_budget`,
`missing_next_link`, `limit`, and `no_next`. This is an explicit partial result,
not silent completeness.

Pagination requires its own later allocation and attempt-ledger decision. M7i
does not borrow another distribution's reservation or treat one logical page
request as permission for additional physical requests.

### Invocation-time capability check

M7b remains an advisory package-metadata report and is not consulted as an
execution authorization. Immediately before M7i calls the handler, it resolves
the implementation package and symbol again. The resolver result must be a
function. No DNS lookup or transport occurs when resolution fails.

The resolved function is invoked directly with already validated arguments;
there is no provider or package work between resolution and invocation. Tests
record the exact event order `resolve`, `invoke`, `transport`. Successful
execution records `verified_at_invocation` and `invoked`; it does not promote
the overall fetch plan to replayable or generally execution-ready.

### Transport, result, and evidence

The handler uses the package-owned DNS-revalidated, public-address-pinned HTTP
transport with explicit timeout and per-host interval policy. The performer
must return the same canonical target because redirects are disabled. Exactly
one network response attempt is required. Status, media type, content encoding,
optional exact Content-Length, response bytes, final URL, body digest, public
host/IP, and completion time are bound to the request and execution identities.

Existing fail-closed reference GeoJSON parsing helpers validate JSON depth and
member budgets, duplicate object members, the `FeatureCollection` and feature
shape, geometry conversion, feature order, and protected columns. M7i supplies
an empty queryables schema because this slice sends no provider property
filters and does not claim provider-advertised property typing. The normalized
payload is an `sf` object with the additional `gx_oaf_features` class and exact
top-level GeoJSON identifiers in `feature_id`.

The execution object retains the bounded raw response body. Whole-object
validation reparses those bytes and requires the rebuilt `sf` value to be
identical, then recomputes request, execution, attempt, body, count, truncation,
implementation, and metadata relationships. Later multi-handler orchestration
may compact successful evidence under a separate accepted contract, as M7h
does for direct CSV.

### Failure and authority boundaries

Capability, transport, and parse failures use typed trace-redacted conditions.
Returned failure evidence is restricted to a bounded attempt projection; raw
condition text, response bodies, and full query-bearing URLs are withheld.
Changed final URLs, unsafe DNS, invalid status/media/encoding/length, oversized
bodies, malformed GeoJSON, excess features, and forged result or ledger facts
fail closed.

M7i authorizes only the completed attempt represented by a successful
`gx_oaf_execution`. It remains host-specific, non-replayable, unexported, and
not generally execution-ready. It does not change M7h statuses or implement:

- arbitrary provider filters, bbox, time, or parameter mappings;
- queryables discovery or provider-advertised property typing;
- multi-page retrieval or aggregate page budgets;
- cross-handler scheduling or failure isolation;
- optional-package handler invocation;
- runtime handler registration or plugin identity;
- serialization, resume, retry, or replay; or
- public `gx_fetch()` or a `gx_fetched` schema.

## Acceptance criteria

- Offline planning deterministically rebinds one OGC distribution to its exact
  M7d coverage and reservation without DNS, transport, clocks, cache, package
  resolution, or writes.
- A request-plan snapshot fixes the method, headers, empty body, handler and
  implementation identities, redirect/retry/cache policy, one-attempt budget,
  single-page policy, limit, redacted URLs, reservation, and logical request
  identity.
- A successful fixture executes exactly one request, records event order
  `resolve`, `invoke`, `transport`, returns the expected `sf` rows and opaque
  identifiers, and reports the unfollowed next page as truncation.
- A missing symbol fails before DNS or transport.
- A changed final URL and an over-limit feature page consume at most one
  charged attempt and fail under typed redacted conditions.
- Whole-object validation reparses the retained body and rejects mutations to
  bytes, result rows, implementation facts, attempt bytes, execution
  truncation, or metadata authority.
- The plan, executor, handler, and result contracts remain unexported.

## Consequences

- M7 now contains one complete non-CSV handler slice whose request, capability,
  transport, parsing, result, and evidence boundaries are fixture-backed.
- The M7d all-handler reservation is proven reusable without weakening its
  one-attempt and encoded/decoded-byte accounting.
- Single-page partial results are explicit and safe, while paginated attempt
  allocation remains a deliberate later decision.
- The next M7 work is cross-handler orchestration and exact status
  reconciliation for CSV plus OGC execution, followed by the remaining
  provider handlers, registration, and serialization/replay decisions.
