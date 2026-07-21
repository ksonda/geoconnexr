# geoconnexr — validated specification and build roadmap

**Status:** Accepted implementation roadmap; a stable 0.1.0 contract
freeze is gated on the remaining P0 decisions and vertical-spike
evidence below **Version:** 0.2.0 **Reviewed:** 2026-07-20

## 0. Executive decision

The product concept is worth building and the layered architecture is
sound. The earlier v0.1 draft is not build-ready because several
live-service assumptions, public contracts, and reproducibility claims
are incorrect or incomplete. This revision keeps the core product but
makes these changes:

1.  adds a P0 architecture spike before public schemas are frozen;
2.  corrects GeoSPARQL spatial queries to use `geof:sfIntersects`;
3.  treats production payloads as variable profiles rather than assuming
    one ideal JSON-LD shape;
4.  replaces `gx_twin()` with `gx_snapshot()` and separates replay from
    reproducibility;
5.  separates dataset, distribution, and variable identity;
6.  restricts automatic pagination to named, stably ordered SELECT
    templates;
7.  defines a four-stage fetch-handler protocol and a discovery
    diagnostics contract;
8.  makes request safety, cache representation, payload budgets, and
    optional dependencies explicit;
9.  uses current `dataRetrieval` and `edr4r` APIs; and
10. splits the original flagship phase into catalog/package and
    fetch/harmonize phases.

The target remains an R-first package for discovery, identifier
crosswalks, and watershed data snapshots across the Geoconnex ecosystem.

### Implementation status (2026-07-20)

| Module | Status |
|----|----|
| M1 | Partial experimental slice: bounded transport, cache/offline behavior, redirects, PID resolution, package-owned retries, full physical-attempt accounting, and per-host throttling are implemented; bounded concurrency remains open. |
| M2 | Experimental PID/JSON-LD/profile slice implemented with a hash-pinned provider corpus; contract freeze remains P0-gated. |
| M3 | Experimental native reference-client slice implemented with typed schemas, bounded pagination, and identity-checked fallbacks; cross-vintage and full large-geometry evidence remain open under ADR 0004. |
| M4 | Partial experimental slices M4a/M4b/M4c: [`gx_gage_to_pid()`](https://ksonda.github.io/geoconnexr/reference/gx_gage_to_pid.md) is implemented, and the v3.2 COMID lookup now has an explicit verified install lifecycle plus internal offline forward and release-scoped inverse mappers; public COMID, HUC12, point, inverse, and currentness contracts remain gated under ADRs 0004, 0008, 0009, and 0015. |
| M5 | Partial experimental M5a/M5b: an unexported one-logical-request SELECT/ASK substrate provides strict bounded SPARQL 1.1 Results JSON parsing and provenance, while the public local renderer now consumes an exact-byte-pinned render-only v2 template manifest with explicit disabled execution, chunking, and pagination; public graph APIs, endpoint support, and paging remain gated under ADRs 0004, 0012, and 0013. |
| M6 | Partial M6a/M6b/M6c: [`gx_aoi()`](https://ksonda.github.io/geoconnexr/reference/gx_aoi.md) canonicalizes one custom polygonal `sf`/`sfc` geometry offline, internal bounded hydration reconstructs AOI-only recipes while independently rebinding canonical GeoJSON to their WKB digest, and an internal catalog value object validates typed sites, flattened datasets, problems, requests, and completeness. Public `gx_catalog()`, live discovery/merge, nonempty reference layers, full replay, and upstream-derived AOI modes remain gated under ADRs 0014, 0016, and 0018. |
| M7 | Partial M7a–M7k: the internal chain selects catalog distributions, records direct-CSV intent, allocates one-attempt reservations, validates response envelopes, and parses strict character tables. M7g executes direct CSV; M7h orchestrates CSV; M7i adds one reservation-bound, single-page OGC API Features request; M7j introduces shared global scheduling; and M7k adds one reservation-bound WQP Result request with explicit service/profile/site/characteristic/time facts, package-owned transport, and invocation-time [`dataRetrieval::importWQP()`](https://rdrr.io/pkg/dataRetrieval/man/importWQP.html) validation. The scheduler now runs CSV, WQP, and OGC in global order with shared admission, isolated failures, compact evidence, and exact all-distribution status. The nested M7a request list remains empty; public fetch, pagination, remaining provider handlers, registration, serialization, and replay remain gated under ADRs 0020–0030. |
| M8 | Planned. |
| M9 | Partial M9a/M9b: an unexported offline verifier validates the bounded manifest and embedded request-ledger shape, rebinds AOI identity through M6b, inventories a closed portable resource tree, and verifies exact local bytes; an unexported creation-only writer stages, verifies, and publishes deterministic redacted catalog CSV resources plus manifest-v1. Public packaging/snapshot APIs, overwrite, loading, Frictionless acceptance, authenticity, and replay remain gated under ADRs 0017 and 0019. |
| M10 | Planned. |

## 1. Validation ledger

These observations were checked against production or current primary
package documentation on 2026-07-12, 2026-07-13, and 2026-07-20. They
are evidence for this plan, not permanent API guarantees.

| Surface | Observation | Planning consequence |
|----|----|----|
| PID registry | `GET https://geoconnex.us/ref/mainstems/29559` returns 303 to the reference server | Preserve the PID as identity and record the redirect chain |
| Reference inventory | The 20 collection IDs listed in v0.1 are currently advertised | Discover live; never hard-code the list as exhaustive |
| Gage lookup | `provider_id=USGS-08332622` returns gage `1000001`, mainstem `1622734`, and COMID `17789327` | Retain as a fixture-backed known answer |
| COMID/mainstem mapping | NHDPlus VAA `levelpathi` values do not equal Geoconnex mainstem identifiers; `ref_rivers` v3.2 supplies a 120,422,425-byte, 2,357,730-row lookup with unique COMIDs | Never construct a mainstem PID from `levelpathi`; require an explicit checksum-pinned install and never auto-download from a crosswalk |
| Gage JSON-LD | Content negotiation works, but the live document uses a literal hydro-location type, string provider IRI, nested `HY_IndirectPosition`, and a WKT value object | Standards expansion plus tolerant profile extraction and diagnostics are required |
| Mainstem item | `/collections/mainstems/items/29559` returned HTTP 500 while filtered collection retrieval worked | Implement item → filtered collection → negotiated JSON-LD fallbacks; include a large geometry fixture |
| SPARQL | POSTing a small SELECT to the graph root works; the verified indirect-position path returns four sites for mainstem `1622734` | POST only; retain the two-path UNION while the graph shape is mixed |
| GeoSPARQL | Spatial functions use namespace `http://www.opengis.net/def/function/geosparql/` and prefix `geof:` | `gsp:sfIntersects` from v0.1 is a correctness bug and must not appear in templates |
| NLDI | `huc12pp` is advertised, but its HUC12 response contract is not yet fixture-pinned | Gate HUC12 implementation on bounded response evidence, then resolve any returned COMID through the checksum-pinned mainstem lookup |
| `nhdplusTools` | Current docs expose `get_vaa(atts=)`, `discover_nhdplus_id()`, `get_nldi_basin()`, and Geoconnex reference helpers | Use guarded wrappers; do not re-export a package in Suggests |
| USGS retrieval | Current `dataRetrieval` docs mark `readNWIS*` as legacy and provide `read_waterdata_continuous()` / `read_waterdata_daily()` | New APIs are primary; legacy handlers are compatibility paths |
| EDR client | Package name is lowercase `edr4r`; CRAN 0.1.1 exists and the repository is `ksonda/edr4r` | Require and test `edr4r >= 0.1.1`; remove the old open question |

Do not use a full graph triple count as a smoke test. It is expensive
and not a compatibility contract.

## 2. Scope

### 2.1 User problem

Analysts can retrieve observations once they know a provider and API,
but they lack a stable R workflow to:

- discover monitoring locations and described datasets for an area;
- translate COMIDs, HUC12s, gage IDs, points, mainstems, and Geoconnex
  PIDs;
- inspect what can and cannot be fetched automatically; and
- save a versioned, inspectable watershed data snapshot with provenance.

### 2.2 Product boundary

`geoconnexr` owns PID/JSON-LD handling, graph discovery, crosswalk
composition, catalog diagnostics, fetch planning, and snapshot
packaging. It calls current `dataRetrieval`, `edr4r`, and `nhdplusTools`
APIs rather than duplicating them.

It does not operate servers, mutate Geoconnex, promise immutable
re-execution of changing upstream data, execute arbitrary fetched code,
or present a live simulation/digital twin.

### 2.3 Primary workflow

``` r

aoi <- gx_aoi("02070010", type = "huc")
catalog <- gx_catalog(aoi)
plan <- gx_fetch_plan(catalog, time = c("2025-01-01", "2025-01-31"))
fetched <- gx_fetch(plan)
harmonized <- gx_harmonize(fetched)
snapshot <- gx_snapshot(harmonized, dir = "potomac-snapshot")
```

Each stage remains independently usable. `gx_snapshot()` dispatches on
`x`: an AOI-like input runs the remaining pipeline, while a
`gx_catalog`, `gx_fetched`, or `gx_harmonized` object resumes from that
completed stage.

``` r

gx_snapshot(x, dir, time = NULL, fetch = TRUE, report = TRUE, ...)
```

## 3. Architecture and package policy

### 3.1 Layers

``` text
L4  Snapshot/report       gx_snapshot(), gx_package(), gx_report(), gx_replay()
L3  Orchestration         gx_catalog(), gx_fetch_plan(), gx_fetch(), gx_harmonize()
L2  Discovery/identity    gx_query(), gx_sites(), gx_datasets(), crosswalks
L1  Protocol clients      PID, JSON-LD, OGC API Features, SPARQL transport
L0  Versioned assets      queries, registry rules, vocabularies, JSON Schemas
```

All network calls go through an L1 request executor or an explicitly
named external handler. External package calls must still return
request/provenance metadata to the orchestrator.

### 3.2 Package dependencies

Proposed minimums must be confirmed during P0 and then pinned in
`DESCRIPTION`.

- **Imports:** R (\>= 4.1), `cachem`, `cli`, `digest`, `fs`, `httr2`,
  `jsonld`, `jsonlite`, `rlang`, `sf`, `tibble`, `vctrs`, `xml2`,
  `yaml`.
- **Suggests:** `arrow`, `dataRetrieval`, `edr4r` (\>= 0.1.1),
  `frictionless`, `httptest2`, `nhdplusTools`, `quarto`, `readr`,
  `testthat`.
- Optional-package absence is tested. A handler requiring a missing
  Suggests package produces `skipped_missing_pkg`, not package-load
  failure.
- CSV is the default portable timeseries output. Parquet is opt-in and
  requires `arrow`.
- The native reference client is canonical. Results must not change
  merely because `nhdplusTools` happens to be installed.

Also specify `Encoding: UTF-8`, testthat edition 3, minimum dependency
versions, system requirements for `sf`/`jsonld`, and CRAN-safe examples
before P0 exits.

### 3.3 Object and schema governance

- Table functions return `tibble`/`sf`; orchestration functions return
  documented S3 objects (`gx_aoi`, `gx_catalog`, `gx_fetch_plan`,
  `gx_fetched`, `gx_harmonized`, `gx_package`).
- Every S3 class has a constructor, validator, print method, zero-row
  behavior, and contract tests.
- Every serialized table and manifest carries `contract_version`.
- Contracts are **experimental during 0.x** and become fixed only after
  the P0 vertical slice and P2 provider audit.
- Breaking serialized-contract changes require a migration reader and a
  package minor-version change during 0.x.
- Hydrologic identifiers, HUCs, FIPS codes, reach codes, COMIDs, and
  level-path IDs are always character values.

## 4. P0 release gates and architecture spike

Implementation may scaffold before these finish, but public 0.1.0
contracts may not be tagged until all gates close.

### 4.1 Product-owner decisions

1.  package name and owning GitHub organization — closed by ADR 0001;
2.  MIT vs Apache-2.0 license — closed by ADR 0002;
3.  CRAN intent and release channel — closed by ADR 0003;
4.  `mainstems` vs `mainstems_v3` default and vintage/migration policy —
    open;
5.  whether graph POST-at-root is a supported public contract — open;
6.  support/ownership for weekly live-service alerts — open.

### 4.2 Required vertical spike

Build disposable code and fixtures—not production abstractions—to prove:

1.  one correct `geof:sfIntersects` AOI query;
2.  parsing and diagnostics for at least five real JSON-LD profiles,
    including the current reference gage;
3.  one HUC10 path from AOI → sites → described dataset → one USGS or
    EDR fetch → harmonized rows → offline package validation;
4.  mainstem and v3 behavior for current, superseded, and large-geometry
    items;
5.  handler request planning for EDR and current USGS Water Data APIs;
6.  safe replay metadata, size ceilings, redirects, and cache behavior.

### 4.3 P0 exit criteria

- A decision log closes gates 1–5 or names an owner and deadline.
- Spike fixtures and an evidence report are committed.
- The data contracts in Section 6 have survived the vertical slice.
- A threat model covers provider-controlled URLs, redirects, remote
  JSON-LD contexts, decompression, and local/private addresses.
- A measured delivery estimate replaces the provisional roadmap ranges.

## 5. Module specifications

### M1 — Request, cache, and safety core

``` r

gx_client(endpoint = c("graph", "reference", "pid"), ...)
gx_cache_info()
gx_cache_clear(confirm = interactive())
```

Internal execution supports method, canonical URL, headers, body,
endpoint policy, timeout, retry, throttle, redirect policy, maximum
encoded/decoded bytes, and offline mode.

Requirements:

- retry 429 and selected 5xx/transport failures, honor `Retry-After`,
  and use jitter; every physical attempt must revalidate DNS and consume
  aggregate request/byte budgets;
- throttle and parallelize per host; PID/JSON-LD maximum active requests
  default to four;
- graph queries are POST-only and reject redirects or HTML responses;
- cache key includes method, canonical URL, body hash, `Accept`,
  `Content-Type`, endpoint base, and cache schema version;
- cache entries record response status, relevant headers, retrieval
  time, final URL, and SHA-256;
- never cache error/partial responses as successes; use atomic writes
  and recover from corruption;
- store under `tools::R_user_dir("geoconnexr", "cache")` by default;
- `offline=TRUE` permits valid cached responses only and reports cache
  misses clearly;
- for provider-controlled URLs, allow HTTP(S) only; reject loopback,
  private, link-local, and cloud-metadata targets by default and
  re-check every redirect.

[`httr2::req_cache()`](https://httr2.r-lib.org/reference/req_cache.html)
may be used for compliant GETs, but POST SPARQL caching needs the
package cache because `req_cache()` does not cache POST requests.

**Acceptance:** deterministic tests with injected clock/jitter cover
retry, throttle, representation-specific cache keys, expiry, offline
misses, concurrent writes, corrupt entries, redirect revalidation,
decompression ceiling, and graph HTML errors. A lint rule forbids direct
[`httr2::request()`](https://httr2.r-lib.org/reference/request.html)
outside L1, excluding documented external-package handlers.

### M2 — PID resolution and Geoconnex-profile JSON-LD

``` r

gx_resolve(uri, follow = TRUE)
gx_jsonld(uri, as = c("expanded", "raw", "text"))
gx_parse_location(x, strict = FALSE)
gx_parse_datasets(x, strict = FALSE)
```

[`gx_resolve()`](https://ksonda.github.io/geoconnexr/reference/gx_resolve.md)
returns one row per input with `pid_uri`, `initial_status`,
`final_status`, `landing_url`, `redirect_chain` (list-column),
`resolved_at`, and any problem code. Try HEAD, but fall back to a
minimal GET when HEAD is rejected or loses required redirect behavior.

[`gx_jsonld()`](https://ksonda.github.io/geoconnexr/reference/gx_jsonld.md)
negotiates JSON-LD, then inspects HTML
`<script type="application/ld+json">` blocks. It performs
standards-based expansion with bundled/allowlisted contexts by default;
arbitrary remote context loading is disabled unless explicitly enabled.

The extractor supports documented Geoconnex profiles and known malformed
variants. It must handle provider as object or IRI, WKT as string or
value object, nested indirect-position keys, `@graph`,
arrays/singletons, expanded/prefixed properties, and missing geometry.
Unsupported or contradictory values produce structured parser
diagnostics; they are never silently discarded.

**Acceptance:** fixtures cover at least five pages from three providers,
the current reference-gage shape, `@graph`, aliased terms, multiple
datasets/distributions/variables, missing geometry, open temporal
intervals, malformed contexts, and oversized/deep documents.
Build→expand→parse round trips are property-tested in M10.

### M3 — OGC API Features reference client

``` r

gx_ref_collections(refresh = FALSE)
gx_ref_queryables(collection)
gx_ref_features(collection, query = list(), bbox = NULL,
                limit = 1000L, allow_unbounded = FALSE)
gx_ref_feature(collection, id)
```

Use the native client as the canonical backend. Validate simple property
filters against `/queryables`. Preserve server-advertised types while
normalizing identifiers to character. Follow `rel=next`; stop on the
requested limit, repeated next URL, empty page, or `numberMatched`. Add
`truncated` and retrieval metadata.

[`gx_ref_feature()`](https://ksonda.github.io/geoconnexr/reference/gx_ref_feature.md)
tries the item endpoint, then a collection filter using the sole
queryable marked `x-ogc-role: id`, then negotiated JSON-LD. It returns a
classed error only after all compatible paths fail.

Unfiltered collection-wide retrieval requires `allow_unbounded=TRUE` and
still obeys a configurable row/byte budget.

**Acceptance:** fixtures for `mainstems`, `mainstems_v3`, `hu12`,
`gages`, `counties`, pagination, unknown queryables, the current
large-mainstem item failure, and an empty collection.

### M4 — Crosswalks

``` r

gx_comid_to_mainstem(comid, check = TRUE)
gx_huc12_to_mainstem(huc12, method = c("outlet", "intersects"))
gx_point_to_mainstem(points)
gx_gage_to_pid(provider_id)
gx_mainstem_to_comids(mainstem_uri)
gx_mainstem_to_gages(mainstem_uri)
gx_mainstem(mainstem_uri)
```

- COMID→mainstem uses a checksum-pinned, versioned upstream mapping
  asset or a vetted adapter that exposes that asset. VAA `levelpathi` is
  a different identifier namespace and must never be interpolated into a
  Geoconnex PID. A missing optional adapter or mapping asset produces an
  actionable classed error and never triggers an undisclosed large
  download.
- Check current/superseded state in the P0-selected mainstem vintage and
  expose both requested and current URIs.
- HUC12 outlet mode calls NLDI `huc12pp`, preferring its mainstem value,
  then resolving its COMID through the checksum-pinned mainstem lookup.
  Intersects mode returns every spatial match and explicit ranking
  metrics; it never silently selects one.
- Point mode validates point geometry/CRS before
  `discover_nhdplus_id()`.
- Gage and inverse-gage mappings use validated reference queryables
  first.

The implemented M4a slice is
[`gx_gage_to_pid()`](https://ksonda.github.io/geoconnexr/reference/gx_gage_to_pid.md).
It verifies that the service honored each provider filter and that
feature, property, and advertised PID identities agree; returns explicit
not-found and ambiguous rows; preserves duplicate input order while
deduplicating transport; and enforces aggregate batch budgets.
Advertised mainstem URIs are retained as vintage-unverified related
values.

The implemented M4b substrate pins the `ref_rivers` v3.2 asset in an
immutable runtime registry.
[`gx_mainstem_lookup_install()`](https://ksonda.github.io/geoconnexr/reference/gx_mainstem_lookup_install.md)
is the only disclosed download or local-import path;
[`gx_mainstem_lookup_info()`](https://ksonda.github.io/geoconnexr/reference/gx_mainstem_lookup_info.md)
re-verifies availability and provenance without network access. The
internal vectorized COMID mapper scans verified local bytes in bounded
chunks, returns explicit not-found rows, and records that current
service state was not checked. It never installs or refreshes
implicitly.

The internal M4c inverse scans the same verified bytes by canonical
mainstem URI and returns every member COMID in deterministic order, or
one explicit not-found sentinel. It preserves duplicate input order,
caps aggregate matches and expanded rows, and carries release and
checksum provenance. Completeness and active status are scoped only to
that mapping release; current service state is not checked. Neither
public direction is exported until supersession/currentness,
point-provenance, and ranking contracts are resolved.

The intersects spike must decide whether ranking means
outlet-in-polygon, intersection length, drainage area, or a documented
combination. `is_largest` alone is not an outlet semantic.

**Acceptance:** retain the checked gage answer; pin and verify the
mapping registry, explicit install/import/offline/integrity behavior,
both COMID known answers, and release-scoped zero-to-many inverse
behavior; add checked HUC12 and superseded fixtures; test leading
zeros/duplicates/NA/not-found inputs; and verify all identifier columns
remain character.

### M5 — SPARQL and discovery

``` r

gx_sparql(query, endpoint = getOption("geoconnexr.endpoint_graph"))
gx_query(template, ..., limit = 1000L, max_rows = 10000L)
gx_templates()
gx_sites(aoi = NULL, mainstem = NULL, provider = NULL,
         site_type = NULL, limit = 1000L)
gx_datasets(sites = NULL, aoi = NULL, variable = NULL,
            provider = NULL, active_during = NULL, limit = 1000L)
```

`gx_sparql()` executes exactly once and supports SELECT/ASK with
explicit result types. It does not rewrite or paginate arbitrary SPARQL.

The implemented M5a checkpoint is deliberately narrower and unexported.
`gx_graph_execute_once()` accepts only trusted package-controlled
read-only SELECT/ASK text, an explicit graph client and expected result
form, and finite row, variable, bound-term, link, request,
cumulative-byte, per-response, member, atomic-byte, and depth budgets.
It sends one logical POST without rewriting or pagination; package-owned
retries may repeat only the unchanged body. Successful responses require
SPARQL 1.1 Results JSON and pass semantic validation before cache
admission.

SELECT results preserve response row cardinality plus a sparse binding
table with `row`, `variable_index`, `variable`, `term_type`, `value`,
`datatype`, `language`, and an opaque per-result `bnode_scope`. This
distinguishes zero rows, zero-width solutions, all-unbound solutions,
and bound empty literals without allocating a dense rows-by-variables
table. ASK preserves one logical value. Both forms retain query/document
hashes and redacted request/physical- attempt provenance, never the raw
query or response body. This checkpoint does not implement or export
`gx_sparql()`, satisfy M5 acceptance, prove the graph root as a
supported endpoint, or authorize arbitrary user SPARQL.

The implemented M5b checkpoint replaces the earlier aspirational
manifest with a render-only v2 contract. Every reviewed SELECT template
declares its exact stored byte count and SHA-256, ordered projected and
required variables, the literal `ORDER BY` variables, result-key
uniqueness/scope, a finite row budget, and explicit pagination blockers.
Root capabilities enable local rendering but disable execution,
chunking, and pagination. Runtime and JSON Schema validation reject
unknown fields and malformed cross-field contracts; runtime additionally
checks every stored query’s UTF-8 bytes, slots, projection, ordering,
and terminal slice controls. This metadata is inspection evidence, not
paging authorization.

HTTP IRI/VALUES parameters are bounded and injection-safe; lists reject
exact duplicates and sort by UTF-8 bytes for deterministic rendering.
CRS84 WKT AOIs must parse as finite, valid, non-empty, explicitly closed
polygons or multipolygons. Integer encoding is locale-independent, while
literal and canonical UTC datetime encoders are available for future
reviewed templates. No parameter type accepts raw SPARQL. Because SPARQL
does not totally order all RDF terms, blank-node labels are
response-scoped, optional variables can be unbound, and the graph has no
snapshot contract, no current named template paginates.

Required templates include sites on mainstem, sites in AOI, datasets for
site URIs, datasets by variable, sites by provider, and bounded provider
coverage. The mainstem template unions the verified indirect and direct
paths. Spatial templates use:

``` sparql
PREFIX geof: <http://www.opengis.net/def/function/geosparql/>
FILTER(geof:sfIntersects(?site_wkt, ?aoi_wkt))
```

An AOI or other selective filter is required unless a finite low limit
is supplied. If graph spatial capability becomes unavailable, the
function returns a partial-result diagnostic; it must not substitute
reference gages and present them as all monitoring sites.

**Acceptance:** fixture tests for all templates and encoders; mutation
tests for injection; stable pagination tests with duplicate/missing page
scenarios; live bounded checks for the known mainstem and one AOI; no
full-graph counts.

### M6 — AOI and catalog

``` r

gx_aoi(x, type = c("auto", "huc", "county", "state", "sf"))
gx_catalog(aoi, include = c("sites", "datasets", "reference"),
           providers = NULL, variables = NULL, progress = interactive())
```

Auto-dispatch rules are ordered and deterministic:

1.  `sf`/`sfc` geometry;
2.  HUC character strings of length 2, 4, 6, 8, 10, or 12;
3.  five-digit county FIPS;
4.  two-letter state abbreviation;
5.  otherwise error and require explicit `type`.

The implemented M6a slice accepts `type = "sf"` or `"auto"` for exactly
one `sf`/`sfc` XY `POLYGON` or `MULTIPOLYGON` with an explicit CRS. It
rejects invalid or empty geometry, non-finite coordinates, transformed
coordinates outside CRS84 bounds, and inputs over 100,000 coordinate
positions rather than repairing them. Antimeridian-crossing rings
require explicit pre-cut geometry. Accepted geometry is transformed to
`OGC:CRS84`; ring start and GeoJSON winding, hole order, and
multipolygon member order are canonicalized after rounding to a
nine-decimal-degree grid. PROJ networking is disabled for the
transformation and its prior state is restored afterward.

Spatial recipes record canonical GeoJSON and a SHA-256 of portable
little-endian WKB, with an independent 8 MiB ceiling on each
representation.
[`gx_aoi()`](https://ksonda.github.io/geoconnexr/reference/gx_aoi.md)
performs no network, graph, catalog, mainstem-basin, or point-upstream
work. Its pipeline fields describe the intended replay boundary, not a
completed catalog. `mainstem_basin` and `point_upstream` remain excluded
pending their own evidence and provenance contracts, and public
`gx_catalog()` remains gated under ADR 0014.

The internal M6b hydration boundary accepts only the exact three-field
AOI recipe emitted by
[`gx_aoi()`](https://ksonda.github.io/geoconnexr/reference/gx_aoi.md),
either as a decoded list or literal bounded JSON. It never treats a
string as a path or URL. Identifier recipes must regenerate exactly;
spatial recipes are manually reconstructed in CRS84, canonicalized again
with PROJ networking disabled, and accepted only when both normalized
GeoJSON and the portable WKB SHA-256 match the regenerated AOI.
Duplicate JSON members, type confusion, noncanonical geometry, malformed
or marked encoding, and depth/member/coordinate/byte bombs fail closed.
The reader remains unexported and does not authorize catalog or full
recipe replay under ADR 0016.

`gx_catalog` returns `gx_catalog` with `sites`, flattened dataset
records, selected reference layers, `problems`, `requests`, AOI, and
metadata. `problems` has `stage`, `source_uri`, `code`, `severity`,
`message`, `recoverable`, and timestamp. Metadata includes per-stage
completeness/truncation flags. Invalid input and contract corruption
abort; provider-specific recoverable failures do not.

The internal M6c checkpoint implements this as a strict offline value
object, not a live orchestrator. Sites are typed CRS84 points, dataset
rows retain their distribution-by-variable cardinality, problems and
request attempts have exact typed schemas, and procedural completeness
must reconcile its counts. Contract 0.1.0 requires an empty
reference-layer list while portable reference serialization remains
unresolved. Fixed cardinality, text, list-entry, and geometry budgets
fail before serialization. Deterministic export views redact URI
credentials and query/fragment values across schemes without mutating
the catalog, while namespace-bound SHA-256 site and variable
fingerprints preserve identity and site/dataset joins after display
redaction. Public `gx_catalog()` and all discovery/merge adapters remain
gated under ADR 0018.

Graph results are preferred for discovery. JSON-LD fallback is bounded
and its attempted/skipped/failed counts are recorded. Merge precedence
and field-level provenance are deterministic.

**M6a acceptance:** custom polygon and multipolygon inputs normalize to
CRS84; equivalent ring starts/directions and reordered holes or members
produce the same canonical recipe identity, including ordinary
projection round-trip noise; missing CRS, non-XY, multiple, empty,
invalid, non-finite, out-of-bounds, over-coordinate, and over-byte
inputs fail closed; AOI construction makes no upstream request.

**M6b acceptance:** identifier and spatial recipes round-trip with exact
AOI identity; object-member order and integer/double JSON spellings
normalize, while duplicate/unknown members, altered hashes or
coordinates, noncanonical geometry, invalid encodings and structures,
and all declared budgets fail closed; hydration performs no file or
upstream operation.

**M6c acceptance:** zero and populated value objects preserve exact
typed schemas; site geometry, dataset foreign keys/identities,
recoverable problems, unique request attempts, UTC timestamps, metadata
counts, and procedural completeness fail closed under fixed budgets
without count overflow; aggregate text rejects before expensive
validators; bytewise export order, generic URI redaction, and stable
identity fingerprints are deterministic; construction performs no
external work, and no catalog API is exported.

**Remaining M6 acceptance:** HUC8, county, and upstream-derived basin
fixtures; leading-zero IDs; partial graph/reference/JSON-LD failures;
deduplication and provenance; count reconciliation across catalog data
and diagnostics.

### M7 — Fetch planning and execution

``` r

gx_fetch_plan(catalog, time = NULL, handlers = gx_handlers(),
              max_datasets = 100L, max_bytes = 1e9)
gx_fetch(plan, parallel = 1L, dry_run = FALSE)
gx_handlers()
gx_handler_register(name, classifier, implementation,
                    precedence = NULL, scope = c("session", "call"))
```

The implemented M7a checkpoint is deliberately narrower and unexported.
It accepts only a revalidated M6c catalog whose selected components
include datasets. The loader strictly binds the portable first-match
classifiers in `registry.yml` to the R implementation and
optional-package metadata in `implementations-r.json`; both exact asset
hashes are recorded in the plan.

M7a groups catalog rows by `distribution_id`, requires all non-variable
facts within a group to agree, and emits one distribution row plus
ordered parameter rows for the group’s variables. Each admitted URL
passes the offline target safety policy without DNS and must reclassify
to the catalog’s handler. The requested time range is intersected with
catalog coverage. Selection is bytewise by provider-URI
missingness/value, provider-name missingness/value, site URI, and
distribution ID; `max_datasets` counts only otherwise eligible fetch
candidates. Request and byte ceilings are recorded for later stages.

Contract 0.1.0 requires an exact empty request list and leaves parameter
keys unmapped. Every fetch implementation is `planned`, every handler is
non-replayable, and `execution_ready` is false. Construction and
validation do not probe installed packages, call handlers, resolve DNS,
use the network or cache, or write files. Public planning/execution,
request construction, runtime registration, and serialized/replayable
plans remain deferred under ADR 0020.

M7b adds a separate unexported package-capability report without
changing the M7a plan. It embeds and revalidates that plan, then reads
only the installed `DESCRIPTION` identity and version of each unique
allowlisted package required by selected distributions. The built-in
probe scans a bounded
[`.libPaths()`](https://rdrr.io/r/base/libPaths.html) view, performs
bounded direct raw-DCF reads, and ignores `Meta/package.rds`. Packages
are inspected once in bytewise name order without loading namespaces or
inspecting or calling symbols. Missing and too-old versions become
explicit skip statuses. Native handlers, present unpinned requirements,
and satisfying minimum versions still remain
`blocked_implementation_planned` because every implementation is
metadata-only and provider request semantics remain unproven.

The M7b report is host-specific, advisory, non-replayable, and never
execution-ready. It preserves the M7a plan and exact empty request list,
does not consume request or byte budgets. With the built-in probe it
performs no handler call, namespace load, DNS lookup, provider
transport, cache access, or write. Revalidating M7a rereads the bounded
bundled handler assets; the package probe’s only additional I/O is
bounded host library metadata reads, which can reside on a mounted
filesystem. Future execution must recheck its actual loaded package,
version, and symbols immediately before invocation. Provider request
planning remains gated under ADR 0021 on fixture-backed mappings plus
reviewed transport, ledger, redirect, credential, and budget contracts.

M7c adds the unexported `gx_csv_get_intents` S3 value object with
contract version 0.1.0 and exact top-level fields `contract_version`,
`plan`, `policy`, `intents`, `coverage`, and `metadata`. It embeds and
revalidates the byte-identical M7a plan without consulting M7b. Its
exact policy fixes slice `direct_csv_get_v1`, GET,
`Accept: text/csv, application/csv;q=0.9`, identity encoding, a
zero-byte empty body and hash, and unbound credential, redirect, cache,
and parser behavior.

The intent table contains only selected `csv` distributions in global
fetch order and records distribution and intent identity, declared media
type, a redacted canonical URL, and `intent_status = "inert"`. The
coverage table keeps every plan distribution and links selected CSV rows
to their intent while distinguishing `intent_created`,
`deferred_handler`, `not_selected`, and `reference_only`. The full
target is re-derived through the offline safety policy, retaining its
canonical query and removing its fragment. It is bound, with
distribution ID, fetch order, handler, declared media type, and every
exact policy field, by
[`gx_contract_hash()`](https://ksonda.github.io/geoconnexr/reference/gx_contract_hash.md)
under namespace `geoconnexr.csv-get-intent.v1`; it is not stored outside
the embedded M7a plan.

M7c intent identity is not a cache key, request ID, physical-attempt ID,
manifest request hash, or provenance claim. The object assigns no
per-intent encoded- or decoded-byte ceiling, row or column limit, or
aggregate budget reservation. Metadata sets `host_specific`,
`replayable`, `execution_ready`, `transport_authorized`, and
`budgets_allocated` to false and exactly reconciles distribution,
selection, intent, coverage-status, and zero-request counts. It inherits
every M7a non-replayability reason and adds `attempt_ledger_unbound`,
`cache_policy_unbound`, `credential_policy_unbound`,
`parser_limits_unbound`, `provider_transport_unauthorized`,
`redirect_policy_unbound`, `request_budgets_unallocated`, and
`response_contract_unproven`. It performs no package inspection, handler
call, DNS, network, redirect, cache, write, provider-response parsing,
or CSV parsing and introduces no public API or serialized schema.

M7d adds the unexported `gx_csv_request_plan` S3 value object with
contract version 0.1.0 and exact top-level fields `contract_version`,
`intent_set`, `policy`, `budgets`, `reservations`, `request_plans`,
`coverage`, and `metadata`. It requires an M7c intent set and explicit
`max_response_bytes`, `max_rows`, and `max_columns` inputs. It embeds
and revalidates M7c byte-for-byte, so the nested M7a plan is also
unchanged and its exact request list stays empty. No product defaults
are invented for the three shape limits.

M7a `max_requests` counts physical attempts. M7d therefore allocates
across every selected distribution in global `fetch_order`, not only CSV
rows. It defines `K` as the minimum of the selected count,
`max_requests`, and the whole- byte encoded and decoded aggregate
budgets, then reserves the first `K` rows. Each reservation permits
exactly one physical attempt. Non-CSV rows retain held reservations,
preventing direct CSV from stealing their global capacity. Encoded and
decoded totals are separately capped at `K * max_response_bytes`, then
divided across all reservations by integer quotient and remainder, with
earlier rows receiving the remainder bytes. Budget fields preserve the
exact source budgets and reconcile reserved and remaining request,
encoded-byte, and decoded-byte amounts.

Reservations distinguish `csv_request_planned` from
`held_deferred_handler`. Only a reserved CSV row with a matching M7c
intent receives a logical request plan, whose status is exactly
`planned_non_executable`. Coverage preserves every distribution and uses
exact statuses `csv_request_planned`, `csv_budget_deferred`,
`handler_reserved`, `handler_budget_deferred`, `not_selected`, and
`reference_only`. Intent, reservation, and logical-request foreign keys
are present only where their contracts apply; selected CSV rows outside
the reserved prefix retain the M7c intent link but no reservation or
logical request.

The M7d policy preserves M7c’s GET, CSV `Accept`, identity request
encoding, and empty-body facts; treats the source URL as opaque and adds
no credential; rejects redirects with maximum zero; disables retry with
maximum zero; permits one physical attempt; bypasses cache; and accepts
only status 200, `text/csv` or `application/csv`, and identity response
content encoding. The explicit response-byte, row, and column values are
shape limits only. M7d does not define delimiter, quoting, header,
encoding, missing-value, type-inference, schema, or parser-engine
semantics and does not implement response validation or parser
enforcement. No timeout policy is bound.

Reservation identity domain-separates the exact allocation facts. Full
canonical targets are separately re-derived offline from the embedded
M7a plan and bound into domain-separated logical-request identity
without being copied into M7d-owned tables. Stored URLs are redacted.
Neither identity is a cache key, physical-attempt ID, ledger entry,
manifest request hash, execution record, or provenance claim. Whole-byte
identity inputs are canonical unsigned base-10 digit strings and do not
vary with R numeric display options.

M7d metadata keeps `host_specific`, `replayable`, `execution_ready`, and
`transport_authorized` false, sets `budgets_allocated` and
`allocation_complete` true, and keeps `budgets_consumed` false. Zero
execution counts reconcile with exact coverage, reservation,
request-plan, and reserved- attempt counts. Its byte-sorted active
blockers are `arbitrary_provider_client_unimplemented`,
`attempt_identity_unbound`, `attempt_ledger_unbound`,
`csv_parser_enforcement_unimplemented`, `csv_parser_semantics_unbound`,
`handler_implementations_planned`, `non_csv_request_plans_absent`,
`provider_transport_unauthorized`, `response_validator_unimplemented`,
`result_schema_unbound`, `runtime_package_preflight_required`,
`serialization_unbound`, conditionally `source_catalog_incomplete`,
`timeout_policy_unbound`, and `transport_adapter_unimplemented`.

M7d construction and validation perform no DNS lookup, network request,
redirect, cache access, package inspection, handler call, transport,
response validation, CSV parsing, or file write. Revalidating M7a may
reread its bounded bundled handler assets. M7d introduces no public API
or schema, execution path, serialization contract, or replay authority.

M7e adds the unexported `gx_csv_validated_response` S3 value object with
contract version 0.1.0 and exact top-level fields `contract_version`,
`request_plan`, `body`, `validation`, and `metadata`. It embeds and
revalidates M7d byte-for-byte and selects exactly one existing
direct-CSV `logical_request_id`. Its sole response input is an exact
in-memory candidate with fields `status`, `headers`, raw `body`, and
`url`; the candidate is explicitly caller-supplied and is not an
observation that transport occurred.

The boundary applies fixed header count and text limits before semantic
work, then requires status 200, one unambiguous `Content-Type` with base
media `text/csv` or `application/csv`, absent or explicit identity
`Content-Encoding`, and an optional digits-only `Content-Length` equal
to the exact raw body length. The final URL must equal the full
canonical target re-derived from M7a under the existing offline safety
policy, so default ports and fragments canonicalize consistently while
path, host, and query changes fail. Under identity encoding, encoded and
decoded bytes both equal the raw length and must fit M7d’s encoded,
decoded, and response-byte ceilings.

M7e discards arbitrary headers, Content-Type parameters, and the full
target. It retains the exact bounded raw body, body SHA-256, normalized
response facts, and their logical/intent/reservation/distribution
foreign keys. A `validation_id` under namespace
`geoconnexr.csv-response-validation.v1` binds those facts, the
re-derived full target, body digest, byte counts, and request limits
using canonical whole-byte strings. This identity is not a request,
attempt, cache, ledger, manifest, authenticity, provenance, or
parsed-result identity.

Metadata sets `response_candidate_validated` true but keeps
`host_specific`, `replayable`, `execution_ready`,
`transport_authorized`, `provider_response_observed`,
`budgets_consumed`, and `parser_executed` false; `observation_origin` is
exactly `caller_supplied`. The outer blocker set replaces only
`response_validator_unimplemented` with `response_origin_unbound`; the
byte-identical embedded M7d metadata remains unchanged. Empty and
non-UTF-8 raw bodies may pass because delimiter, character encoding,
header, quoting, row, column, missing-value, type, and result-schema
semantics remain parser work.

M7e construction and validation perform no DNS lookup, network request,
redirect, cache access, package inspection, handler call, transport,
clock read, CSV parsing, or file write. Revalidating M7a may reread its
bounded bundled handler assets. M7e introduces no public API or schema,
execution path, serialization contract, or replay authority under ADR
0024.

M7f adds the unexported `gx_csv_parsed_response` S3 value object with
contract version 0.1.0 and exact top-level fields `contract_version`,
`validated_response`, `policy`, `schema`, `data`, `parse`, and
`metadata`. It embeds and revalidates M7e byte-for-byte, accepts one
explicit positive `max_fields` parser ceiling, and parses only M7e’s
exact retained raw body.

The fixed profile admits strict UTF-8 with one optional leading BOM,
comma delimiters, double quotes with doubled-quote escaping, an exact
nonempty unique header, LF or CRLF records, and an optional final
terminator. It rejects quoted record terminators, bare CR, blank
records, comments, trimming, missing-value tokens, type inference, name
repair, ragged rows, and alternate encodings or dialects. Every cell
remains a non-missing character value; header-only input produces a
typed zero-row result.

A package-owned raw scanner validates encoding, controls, syntax, exact
width, and every budget before allocating the result. Fixed ceilings are
16 MiB input, 1 MiB per decoded field, 16 KiB per header name, 1 MiB
across header names, 1,000,000 implementation rows, and 10,000
implementation columns. The lower of the selected M7d and implementation
row/column ceilings applies. The explicit `max_fields` ceiling, no
greater than 1,000,000, counts header and data cells.

`schema` binds ordered character columns and `data` is an exact
character-only tibble. Chunked length-prefixed hashes bind each exact
column, the ordered result, and then the M7e validation/body identities,
parser policy, limits, BOM presence, and counts under the
`geoconnexr.csv-parse.v1` namespace. Metadata records parser and result
validation but keeps provider observation, attempt execution,
fetch-budget consumption, transport authorization, execution readiness,
replay, and serialization false; origin remains `caller_supplied`.

M7f invokes no optional parser package and performs no transport or
body-file I/O under ADR 0025. The registry’s planned `readr` adapter
still requires an actual package/version/symbol check coupled atomically
to future invocation. Physical-attempt identity and ledger rows remain
coupled to transport, where a real execution scope,
DNS/outcome/timing/charging facts, and provider provenance can exist.

M7g adds the unexported `gx_csv_execution` S3 value object with contract
version 0.1.0. It accepts one M7d logical request plus explicit
parser-field, timeout, per-host interval, and execution-scope inputs.
The full target is re-derived rather than accepted from the caller. One
cache-bypassing GET runs through the package-owned request executor with
identity encoding, no redirects or retries, DNS revalidation and
public-address connection pinning, and the minimum selected
encoded/decoded/response byte ceiling.

M7e validates the observed response before transport may return success,
and M7f parses its exact retained body. M7g then binds the
byte-identical M7f-to-M7a chain to one host-specific execution identity
and one physical-attempt ledger row containing only a redacted URL plus
resolved host/IP, status, media, exact bytes, body digest, and
completion time. Its outer metadata honestly records provider
observation, transport authorization, attempt/budget consumption, and
successful parsing while remaining non-replayable and
non-execution-ready. It closes the direct-CSV attempt, response-origin,
timeout, and transport-adapter blockers without making other handlers
executable or exporting `gx_fetch()` under ADR 0026.

M7h adds the unexported `gx_csv_orchestration` S3 value object with
contract version 0.1.0. It embeds M7d once, binds an explicit
orchestration scope and policy, and admits direct-CSV logical requests
in exact global request order. Admission requires both an explicit count
ceiling no greater than 32 and an explicit aggregate reserved-response
ceiling no greater than 64 MiB. A request that does not fit is visibly
`batch_limit_deferred`; later smaller requests may still be considered
in deterministic order.

Live execution invokes M7g sequentially with one domain-separated child
scope per request, no cache, redirects, or retries, and explicit
continue-on-error semantics. An M7g transport or parse failure becomes a
bounded redacted terminal status while later unrelated direct-CSV
requests continue. A strict dry run applies the same admission and
all-distribution status projection but does not consult DNS, transport,
clocks, throttling, cache, or the filesystem.

Every M7d coverage row maps to exactly one M7h status row in selection
order. Successful rows map one-to-one to compact results containing
execution, attempt, normalized validation, parser policy, exact
character schema/data, and result identities. M7h validates the complete
M7g value first, then drops the raw provider body and repeated
M7d-to-M7a chain. Whole-object validation re-derives the full target
from the single embedded M7d plan and rebinds child scopes,
validation/execution/attempt/parse/result identities, exact data,
statuses, indexes, counts, budgets, metadata, and blockers under ADR
0027. M7h remains internal and does not implement non-CSV handlers,
runtime symbol preflight coupled to invocation, registration,
serialization/replay, or public `gx_fetch()`.

M7i adds the unexported `gx_oaf_request_plan` and `gx_oaf_execution` S3
value objects, both contract version 0.1.0. Planning requires one
selected `ogc_api_features` distribution whose query-free canonical path
ends in `/collections/{id}/items` and whose M7d `handler_reserved`
coverage row binds one `held_deferred_handler` reservation. It adds only
deterministic `f=json` and `limit` parameters, stores redacted URLs,
fixes GET/identity/status/media, rejects redirects, retries, cache, and
credentials, and allocates no more than the held one-attempt and
encoded/decoded-byte shares.

Immediately before execution, M7i resolves `geoconnexr::gx_handler_oaf`
as a function and invokes the returned binding without intervening
provider work. Missing-symbol failure occurs before DNS or transport.
The handler performs one DNS-revalidated, public-address-pinned request
and parses one bounded GeoJSON FeatureCollection into `sf`. It never
follows `rel=next`; a next link, a larger `numberMatched`, or a
full-limit page produces explicit truncation. The host-specific
execution retains the bounded body and binds it to one charged attempt;
whole-object validation reparses the bytes and recomputes request,
execution, attempt, result, truncation, implementation, and metadata
facts under ADR 0028. Provider filters, queryables, multi-page budgets,
cross-handler scheduling, optional-package invocation, registration,
serialization/replay, and public `gx_fetch()` remain gated.

M7j adds the unexported `gx_fetch_orchestration` S3 value object with
contract version 0.1.0. It embeds M7d once, derives direct-CSV and
compatible M7i OGC candidates offline, and sorts them by the original
global `fetch_order` rather than grouping by handler. One admission pass
applies an explicit count ceiling no greater than 32 and an aggregate
reserved-response ceiling no greater than 64 MiB across both handlers.
An incompatible OGC source becomes `handler_plan_unsupported`; other
held handlers remain `handler_unimplemented`.

Live execution is sequential with one domain-separated child scope per
candidate, no cache, redirects, or retries, and explicit
continue-on-error semantics. CSV uses M7g; OGC rebuilds M7i from the
shared M7d plan and performs its invocation-time native-symbol check.
Typed CSV transport/parse and OGC capability/transport/parse failures
become handler-specific redacted terminal rows while unrelated
candidates continue. An OGC capability failure charges no physical
attempt or bytes because it occurs before DNS and transport.

Every M7d coverage row maps to exactly one M7j status row. Successful
CSV evidence uses the M7h compact contract. Successful OGC evidence
removes the repeated request-plan chain but retains the bounded GeoJSON
body, normalized `sf`, implementation, execution, and attempt so
validation can rebuild and fully revalidate M7i. Whole-object validation
re-derives candidate requests, admission, child scopes, handler result
identities, terminal statuses, result indexes, per-request and aggregate
bytes, counts, metadata, and blockers under ADR 0029. Dry run performs
the same planning and status projection without performer, DNS, clock,
throttle, cache, filesystem, symbol resolution, or handler work.
Remaining handlers, pagination, registration, serialization/replay, a
public fetched-result schema, and `gx_fetch()` remain gated.

M7k adds the unexported `gx_wqp_request_plan` and `gx_wqp_execution` S3
contracts, both version 0.1.0, and upgrades `gx_fetch_orchestration` to
contract 0.2.0. Planning accepts one selected WQP Result endpoint whose
M7d `handler_reserved` row binds a held one-attempt reservation. It
admits only the reviewed inherited query fields `siteid`, optional
`characteristicName`, and `mimeType=csv`; rejects fragments, duplicates,
extra filters, and partial-day time ranges; and records Result service,
`narrowResult` profile, exact site, characteristic status, UTC interval,
WQP date filters, redacted targets, shape limits, and the held
byte/attempt ceilings. When no characteristic is present, the plan
records that absence rather than guessing a mapping.

Execution resolves exported
[`dataRetrieval::importWQP()`](https://rdrr.io/pkg/dataRetrieval/man/importWQP.html)
immediately before provider work. geoconnexr—not `readWQPdata()`—owns
the DNS-pinned, identity- encoded, cache/redirect/retry-free GET,
because the external retrieval helper’s transport and retry policy
cannot satisfy the exact M7d attempt ledger. The bounded response is
parsed offline by `importWQP(convertType = FALSE)` and by the package’s
strict UTF-8 CSV parser. Both exact character tables must agree,
including WQP slash-to-dot column-name normalization, before one
successful attempt and result can be recorded. Missing parser capability
occurs before DNS/transport; response-envelope, transport,
strict-parser, or parser- disagreement failures retain only typed
redacted evidence.

The 0.2.0 scheduler derives WQP at its original global fetch order and
shares the same count and aggregate reserved-byte admission with CSV and
OGC. Successful WQP evidence removes the repeated M7d chain but retains
the bounded CSV bytes, character table, schema, parse hashes,
external-parser invocation facts, execution, and attempt. Whole-object
validation rebuilds the WQP plan and strict result without requiring the
optional package after execution. WQP failures do not prevent later OGC
work. M7k remains internal, single- response, non-replayable, and not
generally execution-ready under ADR 0030.

Every handler implements `probe → plan → fetch → normalize`:

- **probe:** determine whether the distribution is compatible;
- **plan:** create explicit requests, query type,
  site/collection/parameter mapping, time range, pagination, estimated
  requests/bytes, and required package;
- **fetch:** execute within per-request and total budgets and preserve
  raw response metadata;
- **normalize:** return a declared payload class and column map without
  forcing non-timeseries data into observations.

Registry YAML contains portable classification facts only. R package
requirements and functions live in R implementation metadata; Python may
reuse the classifiers without R-specific `requires` values. First-match
precedence is deterministic and recorded. Runtime registrations are
session/call scoped and serialized only as non-replayable provenance
unless supplied by a package/plugin with a stable identifier.

Initial handlers:

1.  `edr` using `edr4r >= 0.1.1`, with explicit base URL, collection,
    query verb, AOI/location, parameter, datetime, and response format;
2.  current USGS continuous/daily APIs via
    [`dataRetrieval::read_waterdata_continuous()`](https://rdrr.io/pkg/dataRetrieval/man/read_waterdata_continuous.html)
    / `read_waterdata_daily()`;
3.  legacy NWIS IV/DV only for classified legacy URLs, with a
    deprecation warning and tests;
4.  WQP with selected service/profile recorded in the plan;
5.  OGC API Features;
6.  direct CSV with content-type, encoded/decoded size, and parser
    limits;
7.  `unknown` → `reference_only`.

SensorThings is deferred until the provider audit demonstrates material
coverage.

M7h direct-CSV status contains one row per evaluated distribution,
including skipped, reference-only, handler-deferred, batch-deferred,
dry-run, failed, and successful rows, plus exact attempt and byte facts.
M7a selection order is fixed; a reviewed user-supplied ordering
interface remains a later proposal.

**M7a acceptance:** empty and populated M6c catalogs produce exact
bounded objects; variables collapse to one distribution plus
deterministic parameter rows; catalog row permutations preserve source
identity and plan order; conflicting group facts, unsafe URLs,
classifier drift, malformed times, invalid budgets, and forged
asset/handler bindings fail closed; count, foreign key, time, decision,
and cap relationships reconcile; construction performs no package
probing, DNS resolution, network or cache access, handler call, or file
write; no planning or fetch API is exported.

**M7b acceptance:** empty and populated plans yield exact host-specific
reports; only selected handlers’ unique allowlisted packages are probed
in bytewise order; missing, exact, newer, too-old, unpinned, native, and
classifier-only requirements remain distinct; malformed or
warning-producing package metadata fails closed; handler/distribution
foreign keys, statuses, counts, and non-replayability reasons reconcile;
the embedded plan remains byte-identical and request-empty; the built-in
probe loads no namespace, calls no symbol or handler, and initiates no
DNS, provider transport, cache, or write operation; no package
satisfaction status claims execution readiness; and no preflight,
planning, or fetch API is exported.

**M7c acceptance:** empty and populated plans yield exact bounded
host-independent objects; the M7a plan remains byte-identical and
request-empty; selected CSV distributions map one-to-one to fixed inert
GET intents in global fetch order while every distribution retains exact
coverage; re-derived canonical targets, stored redacted URLs, exact
shared policy, empty-body facts, domain-separated intent IDs, foreign
keys, statuses, order, counts, authority flags, and non-replayability
reasons revalidate exactly; query values are not copied outside the
embedded plan or disclosed by printing or conditions; results do not
vary with M7b or installed packages; request/byte/parser budgets remain
visibly unallocated; construction performs no package, handler, DNS,
network, redirect, cache, write, provider-response parsing, or CSV
parsing; fixture snapshots cover empty, CSV, mixed-handler,
query/fragment, permutation, and forgery cases; and no M7c, planning,
fetch, execution, or serialization API is exported.

**M7d acceptance:** empty, zero-budget, CSV-only, mixed-handler, and
constrained M7c inputs produce exact bounded host-independent objects
while M7c and M7a remain byte-identical and the M7a request list remains
empty; required response- byte, row, and column limits reject missing,
fractional, non-finite, out-of-range, incorrectly typed, or forged
values without substituting product defaults; global prefix reservation
and separate encoded/decoded quotient- remainder shares are
deterministic and preserve non-CSV capacity; reservation,
logical-request, and coverage identities, shapes, types, attributes,
statuses, foreign keys, budgets, counts, authority flags, and
byte-sorted blockers revalidate exactly; only reserved CSV intents
receive redacted `planned_non_executable` logical request plans; full
query-bearing targets are re-derived for identity without disclosure in
M7d-owned tables, printing, or conditions; bound redirect, retry, cache,
response, and shape constraints do not claim a response validator,
result schema, or CSV parser implementation; construction performs no
package, handler, DNS, network, redirect, cache, transport,
response-validation, CSV-parsing, or write operation; adversarial tests
cover zero, asymmetric, remainder, mixed-handler, permutation, identity,
redaction, and forgery cases; and no M7d, fetch, execution, schema,
serialization, or replay API is exported.

**M7e acceptance:** one synthetic caller-supplied response candidate
produces an exact bounded host-independent object while M7d, M7c, and
M7a remain byte- identical and the nested M7a request list remains
empty; status 200, bounded singleton critical headers, admitted CSV
media, identity encoding, optional strict Content-Length equality, the
full canonical no-redirect target, and encoded/decoded/response byte
ceilings fail closed; empty, non-UTF-8, and exact-limit raw bodies pass
envelope validation without claiming CSV semantics; full target,
arbitrary headers, and Content-Type parameters are absent from M7e-owned
facts while the exact bounded raw body and its digest are retained;
validation identity deterministically rebinds request foreign keys, full
query-bearing target, normalized facts, Content-Length presence, body
hash, byte counts, and request limits; metadata explicitly denies
provider observation, attempt execution, budget consumption, parsing,
transport, execution readiness, and replay; forged nested objects,
bodies, validation facts, identities, attributes, blockers, or authority
flags fail closed under typed trace-redacted conditions; construction
performs no package, handler, DNS, network, redirect, cache, transport,
clock, CSV-parsing, or write operation; pinned fixtures assert no
transport or CSV-semantic provenance; and no M7e, fetch, parser,
execution, schema, serialization, or replay API is exported.

**M7f acceptance:** one pinned M7e body produces an exact bounded
host-independent parser policy, character schema/data table, counts,
chunked result hash, parse identity, and non-authoritative metadata
while M7e through M7a remain byte-identical; BOM/no-BOM, LF/CRLF, quoted
commas and doubled quotes, literal leading zeroes/`NA`/`#`/spaces, empty
cells, header-only data, and valid multibyte UTF-8 follow the fixed
profile; invalid/control UTF-8, misplaced BOMs, bare or embedded
terminators, blank records, malformed quotes, empty/duplicate headers,
ragged rows, and every selected or implementation budget fail under
typed trace-redacted errors before result allocation; all columns remain
exact non-missing character vectors without inference or name repair;
result and parse identities bind exact values, dimensions, policy,
limits, BOM, M7e validation, and body digest without locale or
numeric-display drift; forged nested objects, policy, schema/data,
hashes, attributes, metadata, or blockers fail whole-object
revalidation; construction loads no optional parser, performs no
DNS/network/cache/clock/body-file/write work, and claims no provider
observation, attempt, fetch-budget consumption, transport, execution,
serialization, or replay; no M7f parser/result API is exported.

**M7g acceptance:** one selected direct-CSV logical request produces
exactly one DNS-revalidated, connection-pinned, cache-bypassing physical
attempt and an exact host-specific execution object; M7e validates the
provider response and M7f parses the same body while the complete nested
chain remains byte-identical; method, headers, empty body,
no-redirect/no-retry policy, timeout, interval, and the minimum selected
byte ceiling reach the performer exactly; execution and attempt
identities bind scope, request/reservation/distribution, full re-derived
target, public host/IP, response facts, body digest, times, and charged
bytes while storing only a redacted URL; unsafe DNS, changed URL,
invalid status/media/ encoding/length, oversized body, invalid CSV,
malformed inputs, and forged nested or owned facts fail under typed
trace-redacted conditions; failure never retries or exposes body/query
values; no cache or file is used; and M7g plus `gx_fetch()` remain
unexported.

**M7h acceptance:** empty and mixed M7d plans produce exact bounded
objects with one status row per coverage row while the embedded
M7d-to-M7a chain stays byte-identical and request-empty; dry-run
admission and statuses are deterministic and invoke no DNS, transport,
clock, throttle, cache, or write; live requests execute sequentially in
global request order under explicit count and aggregate
reserved-response ceilings with unique derived child scopes; one
transport or parse failure is redacted, visible, and does not abort
later unrelated requests; successful rows map one-to-one to compact
execution, attempt, validation, fixed parser, exact character-data, and
identity facts without raw bodies or repeated plan chains; deferred and
attempted rows, result indexes, error codes, charged bytes, counts,
budgets, authority flags, and blockers reconcile exactly; forged plan,
child-scope, result, status, byte, or metadata facts fail whole-object
validation; and M7h plus `gx_fetch()` remain unexported.

**M7i acceptance:** one selected query-free OGC items distribution
rebinds offline to its exact M7d held reservation and deterministic
request snapshot; one live fixture resolves and invokes the native
handler in immediate order, performs exactly one bounded attempt,
strictly parses GeoJSON to exact `sf`, and reports an unfollowed next
link as truncation; missing-symbol failure occurs before DNS/transport;
changed final URLs, over-limit pages, malformed payloads, and forged
bytes/result/invocation/ledger/metadata facts fail under typed
trace-redacted conditions; retained bytes and all identities revalidate;
no M7i API or public `gx_fetch()` is exported.

**M7j acceptance:** mixed CSV and OGC fixtures derive deterministic
candidates in original global fetch order and share explicit count and
aggregate reserved-byte admission; dry run produces identical request,
identity, and one-row-per-distribution status facts without host or
provider work; live requests run sequentially with unique child scopes
and at most one physical attempt each; one redacted CSV transport
failure does not prevent the later OGC request; a missing OGC symbol
charges no transport or bytes; an invalid OGC page becomes one charged
parse-failure row without discarding CSV successes; successful status
rows map one-to-one to handler-specific compact results; whole-object
validation rebuilds M7i and rejects forged candidate, plan, scope, CSV
data, OGC body/result, status, byte, index, error, count, or metadata
facts; and M7j plus public `gx_fetch()` remain unexported.

**M7k acceptance:** one WQP Result fixture rebinds to its exact M7d held
reservation and deterministic service/profile/site/characteristic/time
request snapshot without host work; ambiguous endpoints, extra or
duplicate query fields, non-CSV intent, and partial-day filters fail
closed; invocation checks the
[`dataRetrieval::importWQP`](https://rdrr.io/pkg/dataRetrieval/man/importWQP.html)
symbol before DNS, then performs exactly one bounded request with no
retry, redirect, cache, or attribute follow-up; the external character
parser must match the independently strict parsed table; missing-symbol
failure charges no attempt or bytes, while transport and parser failures
produce one typed redacted terminal row and do not prevent later OGC
work; compact retained bytes, schema, table, parse, scope, execution,
and attempt identities fully revalidate; shared dry-run and live
scheduling now orders CSV, WQP, and OGC candidates globally under one
count/byte admission pass; forged plan/result/ledger/status/metadata
facts fail closed; and M7k plus public `gx_fetch()` remain unexported.

**Remaining M7 acceptance:** provider-specific request-plan snapshots
and fixture tests for the remaining non-CSV handlers; optional-package
symbol rechecks coupled to invocation; multi-provider and paginated
execution ledgers; missing-package fixtures; handler-specific aggregate
page budgets; reviewed registration and serialization/replay contracts;
and one public fetched-result schema.

### M8 — Harmonization

``` r

gx_harmonize(fetched, target_units = gx_target_units())
```

Map variables by reviewed URI alignments. Unit conversions use directed,
reviewed rows with `from_unit_uri`, `to_unit_uri`, `scale`, `offset`,
dimension, source, and review date:

``` text
converted_value = original_value * scale + offset
```

This supports affine conversions such as Fahrenheit→Celsius.
Unknown/ambiguous mappings remain unchanged and `harmonized = FALSE`;
never infer solely from a label. Preserve raw values/units and
conversion rule IDs.

**Acceptance:** forward/reverse affine and multiplicative tests,
incompatible-dimension rejection, missing/ambiguous mappings,
qualifiers, duplicate timestamps, timezone normalization, and lossless
access to raw payloads.

### M9 — Package, report, snapshot, and replay

``` r

gx_package(x, dir, timeseries = c("csv", "parquet"),
           keep_raw = TRUE, overwrite = FALSE)
gx_report(x, output = NULL)
gx_snapshot(x, dir, time = NULL, fetch = TRUE, report = TRUE, ...)
gx_replay(manifest, dir, refresh = TRUE, ...)
```

`x` may be a catalog, fetched object, or harmonized object. Package
creation is catalog-only capable. Report rendering happens before final
manifest/resource hashes are written.

Suggested layout:

``` text
<dir>/
  datapackage.json
  manifest.json
  requests.csv
  catalog/sites.gpkg
  catalog/datasets.csv
  catalog/problems.csv
  catalog/fetch_status.csv
  reference/reference.gpkg
  data/observations.csv|parquet
  data/raw/...
  report/snapshot_report.html
```

The manifest distinguishes:

- **recipe replay:** rerun the same procedure against current services;
- **snapshot verification:** verify stored resource SHA-256 values and
  request ledger offline.

It does not promise identical future row counts. Manifest fields include
schema/package versions, canonical AOI recipe and geometry, effective
options, endpoint and hydrologic vintage, query/registry/vocabulary
hashes, request/final URLs, request/body/content hashes,
ETag/Last-Modified where present, timestamps, cache origin, handler
versions, resource hashes, source licences, completeness, and session
details.

`gx_replay(refresh = TRUE)` re-executes the recipe. `refresh = FALSE`
verifies/loads stored assets and errors if required assets are absent.
Custom non-package handlers are reported as non-replayable.

The implemented M9a checkpoint is deliberately narrower and unexported.
`gx_snapshot_verify_impl(dir)` reads only `<dir>/manifest.json`,
validates the full manifest-v1 shape with mandatory package code, and
treats planned replay fields as inert metadata. It extracts the AOI into
M6b’s exact `aoi` to `catalog` fragment so identifier or custom-geometry
identity is independently re-established without executing the recipe.
The embedded request array is shape-validated as the authoritative
ledger; request hashes and historical claims are not recomputed.

M9a inventories a closed snapshot tree before and after verification.
Resource paths use bounded portable ASCII POSIX names; aliases, prefix
collisions, symlinks, hard-link aliases, special files, undeclared
entries, unreadable directories, and directories outside declared
resource prefixes fail closed. Declared resources remain opaque bytes
and are hashed in bytewise path order under 1 GiB per-resource and
aggregate ceilings. A missing optional resource is visible but allowed;
a missing required resource or any present size/hash/type mismatch
aborts. The verifier makes no network, DNS, resource-parser,
decompression, cache, repair, or write call.

This proves internal consistency relative to an unsigned manifest, not
authenticity, historical request provenance, licence truth, or
protection from coordinated replacement of both manifest and resources.
Public `gx_replay(refresh = FALSE)` remains gated on loading/result
semantics, request-export binding, and the selected Frictionless profile
under ADR 0017.

The internal M9b checkpoint adds creation-only catalog snapshot writing
without exporting `gx_package()` or `gx_snapshot()`. It accepts only M6c
catalogs, revalidates immediately before serialization, writes
deterministic redacted UTF-8 CSV resources and manifest-v1 in a sibling
staging tree, validates the exact manifest bytes, verifies the closed
tree through M9a, and publishes only to an absent destination before
verifying again. Its manifest is explicitly non-replayable. Frictionless
metadata, deterministic GeoPackage bytes, overwrite ownership, loading,
signatures, refresh, fetch/harmonize resources, and reports remain gated
under ADR 0019.

Frictionless compatibility is validated with the Python Frictionless CLI
in CI; the R `frictionless` package alone is not treated as
comprehensive validation. Profile and non-tabular resources must be
declared explicitly.

**M9a acceptance:** identifier and custom-geometry manifests rebind
exact AOI identity; bounded closed-tree checksum verification is
deterministic and offline; tampered, missing-required, aliased,
undeclared, symlinked, and special entries fail closed; optional absence
remains visible; request metadata is described as shape-validated rather
than authenticated.

**M9b acceptance:** empty and populated internal catalogs serialize to
four fixed deterministic redacted CSV resources; the exact manifest and
resource bytes pass staged and final M9a verification; existing targets
and unsafe parents are preserved; pre-publication failures clean owned
staging content; post-publication verification failures never
recursively delete a possibly replaced target; and no network,
discovery, overwrite, or public API is added.

**Remaining M9 acceptance:** fetched package creation; public offline
load/replay semantics; missing Arrow/Quarto behavior; a public canonical
request-export loading/binding contract; and full Frictionless CLI
validation of the chosen profile.

### M10 — Publisher tools (post-flagship)

``` r

gx_jsonld_build(sites, datasets = NULL, provider, context = gx_context())
gx_jsonld_validate(x)
gx_sitemap(uris, dir)
```

Builder and validator target an explicit, versioned Geoconnex profile.
Location types are validated as IRIs or profile-approved literals based
on the P0 evidence; the package must not claim that current production
pages uniformly follow a stricter ideal. Validation findings have
severity, JSON pointer, rule ID, profile version, and suggested fix.

## 6. Data contracts

All contracts include `contract_version`; exact columns freeze after the
P0 vertical slice and P2 provider audit.

### 6.1 Sites

One row per canonical `site_uri`: name/type/provider fields, provider
ID, mainstem URI, landing URL, typed POINT geometry (empty allowed),
primary source, and field-level provenance. Duplicate/cluster
relationships are preserved rather than silently collapsed.

### 6.2 Dataset records

Cardinality is one row per dataset × distribution × variable. Required
identifiers:

- `dataset_id`: SHA-256 of schema version plus canonical site URI and
  dataset URI; if no dataset URI exists, use a documented fallback
  fingerprint of provider URI/name and dataset name;
- `distribution_id`: SHA-256 of schema version plus dataset ID,
  canonical access URL, and media type;
- `variable_id`: canonical variable URI, or a namespaced hash of
  provider and normalized label when unmapped.

Rows also retain `dataset_uri`, names/descriptions, temporal coverage,
variable/unit URIs and labels, distribution URL/media type, provider,
licence/access rights, source provenance, classifier result, and
fetchability. Hash canonicalization and NA/Unicode handling are
specified in a language-neutral JSON document and shared with Python
tests.

### 6.3 Catalog problems and requests

`problems` records
stage/source/code/severity/message/recoverability/time. `requests`
records request identity, stage, method, redacted canonical URL, request
hash, final URL, response status/type/bytes/hash, timing, cache origin,
and error code. Sensitive query values are redacted from logs and error
messages.

### 6.4 Fetch plan/status

The M7a plan records distribution identity, bound handler metadata,
stable selection order, effective time, unplanned parameters, and
count/byte budgets. It contains no requests and is explicitly
non-executable and non-replayable. The separate M7b report records host
package-version capability as advisory state while keeping every
implementation blocked and preserving the empty request list. The M7c
object records exact inert direct-CSV GET policy, identity, and
all-distribution coverage without allocating budgets or authorizing
transport. M7d keeps those nested objects byte-identical while reserving
global physical-attempt and encoded/decoded-byte capacity for every
selected handler and emitting only bounded, non-executable direct-CSV
logical request plans. M7e keeps those objects byte-identical while
validating one bounded caller- supplied direct-CSV response envelope and
raw body without claiming provider origin or budget consumption. M7f
keeps that chain byte-identical while parsing the exact retained bytes
under a strict bounded UTF-8 profile into an exact character-only schema
and table. M7g binds one provider-observed direct-CSV response to an
exact physical-attempt ledger row. M7h orchestrates admitted direct-CSV
requests sequentially, continues after isolatable transport/parse
failures, compacts successful evidence, and reconciles one exact status
row for every distribution; its dry run performs no host or provider
work. Later M7 contracts build on M7i’s first reservation-bound
single-page OGC API Features handler and native invocation-time symbol
check. M7j adds shared CSV/OGC admission and global scheduling. M7k adds
the first optional-package provider path: one exact WQP Result request,
package-owned transport, offline
[`dataRetrieval::importWQP`](https://rdrr.io/pkg/dataRetrieval/man/importWQP.html)
agreement with strict CSV, compact retained evidence, and CSV/WQP/OGC
status reconciliation. Later contracts add remaining provider
request/query semantics, optional-package symbol checks, multi-provider
pagination, registration, serialization/replay, and a public
fetched-result schema. The eventual public fetch status may add
user-facing elapsed/message fields and fetched times without weakening
M7h’s identity, attempt, byte, and one-to-one reconciliation rules.

### 6.5 Observations

`site_uri`, `dataset_id`, `distribution_id`, `variable_id`, variable
URI/name, UTC datetime, converted value/unit URI/label, original
value/unit URI/label, qualifier, harmonized flag, and conversion rule
ID. Non-timeseries data keep native shape and are indexed as separate
resources.

### 6.6 Manifest

Publish a JSON Schema under `inst/schema/manifest-v1.json`. The manifest
supports recipe replay and offline resource verification as distinct
operations.

## 7. Non-functional requirements

- **Bounded by default:** finite row, page, request, time, encoded-byte,
  and decoded-byte budgets; unbounded actions require explicit opt-in.
- **Partial failure is visible:** recoverable source failures create
  diagnostics and completeness flags; invalid inputs and broken internal
  contracts abort.
- **Polite:** per-host throttling, maximum concurrency, descriptive
  user-agent, cache reuse, and bounded live CI.
- **Safe:** SSRF and redirect checks, safe schemes, remote-context
  allowlist, parser-depth/decompression limits, no code execution, and
  log redaction.
- **Inspectable:** request ledger, field provenance, exact asset hashes,
  handler/template/vocabulary versions.
- **Portable:** Windows/macOS/Linux checks; UTF-8 and leading-zero
  tests; CSV baseline with optional Parquet.

Performance budgets are measured during P0. The former cold/warm HUC8
targets remain goals, not acceptance criteria, until measured against
representative provider coverage.

## 8. Testing, CI, and documentation

### 8.1 Test layers

1.  pure unit tests with deterministic clock/jitter and no network;
2.  protocol fixtures, minimized and sanitized for package size;
3.  contract tests for every S3 object and zero-row output;
4.  installed-package tests for
    [`system.file()`](https://rdrr.io/r/base/system.file.html) assets;
5.  one offline vertical-slice package and checksum verification;
6.  bounded live checks run weekly, skipped on CRAN, with an owner and
    notification path.

Live checks test stable invariants (PID redirects, advertised
collection/queryable, bounded SPARQL semantic answer) and record count
drift as a diagnostic rather than failing solely because a mutable count
changed.

### 8.2 CI matrix

- R release, devel, and oldrel on Linux; release on macOS/Windows;
- `R CMD build` and `R CMD check --as-cran`;
- minimal optional-dependency job;
- offline/no-network job;
- lint, exported-API snapshot, JSON Schema validation, and Frictionless
  CLI validation;
- scheduled live smoke workflow separate from required PR checks.

### 8.3 Documentation before 0.1.0

README quickstart, package help, crosswalk and discovery vignettes,
cache/offline guide, 200 MB VAA-download notice, optional capability
table, error/diagnostics reference, provenance/licensing guide,
lifecycle/deprecation policy, `CITATION`, `CONTRIBUTING`, security
policy, and support expectations.

## 9. Revised roadmap

| Phase | Scope | Exit | Provisional effort |
|----|----|----|----|
| P0 — Decisions and spike | Release gates, live evidence, threat model, one vertical slice, draft schemas | Section 4.3 complete | 2–3 weeks |
| P1 — Protocol and identity | M1–M3; fixtures; package/CI scaffold | Request/PID/JSON-LD/reference ACs; 0.1.0-dev | 3–4 weeks |
| P2 — Crosswalk and discovery | M4–M5; provider audit; schema freeze for v1 | Crosswalk/discovery ACs; contracts v1 | 4–5 weeks |
| P3a — Catalog and package | M6 plus catalog-only M9 | Offline-verifiable catalog snapshot case study | 3–4 weeks |
| P3b — Fetch and harmonize | M7–M8 plus fetched M9; USGS, EDR, WQP, Features, CSV | End-to-end HUC case study; 0.5.0 | 5–7 weeks |
| P4 — Report/publisher/port | Report polish, M10, shared conformance assets, Python feasibility | Round trip and shared known-answer suite | 4–6 weeks |

P0 must replace these ranges with evidence-based estimates. P3a can ship
useful discovery/package value even if a provider handler delays P3b.

De-scope in this order: SensorThings, report polish, optional reference
layers, mainstem-basin convenience, publisher tools, Python port. Never
de-scope contract versioning, diagnostics, request safety,
recipe/snapshot distinction, or raw-value preservation.

## 10. Definition of ready and done

### Ready for production implementation

- P0 gates closed;
- vertical slice and threat model reviewed;
- mainstem vintage and SPARQL stability decisions recorded;
- schemas and handler protocol accepted;
- dependencies/release channel confirmed.

### Done for each module

- acceptance criteria and contract tests pass;
- fixtures/evidence cover new network behavior;
- invalid, empty, missing-package, offline, and partial-failure cases
  pass;
- docs and NEWS updated;
- no undocumented dependencies or raw transport calls;
- relevant request, schema, security, and migration metadata are
  preserved.

## 11. Remaining decisions

Only these product decisions remain open after this review:

1.  mainstem vintage/default and migration behavior;
2.  supported SPARQL public endpoint contract;
3.  whether administrative layers such as water rights are future scope;
4.  live-monitor ownership and response expectations.

The old EDR package-status question and the existence of `huc12pp` are
no longer open. SensorThings is evidence-gated rather than assumed.
