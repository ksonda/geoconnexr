# Review of `geoconnexr` specification v0.1

**Reviewed:** 2026-07-12
**Verdict:** Strong product direction and modular decomposition; not build-ready without revision.
**Replacement:** [`geoconnexr-spec-v0.2.md`](./geoconnexr-spec-v0.2.md)

## What was evaluated

- the complete attached v0.1 specification;
- current production PID, reference-feature, JSON-LD, and SPARQL behavior;
- current primary documentation for `nhdplusTools`, `dataRetrieval`, `edr4r`, `httr2`, GeoSPARQL, and Frictionless tooling;
- internal consistency of the public API, data contracts, failure semantics, roadmap, testing, and release plan.

The workspace contained no package repository or implementation, so no source code or tests could be reviewed. Validation was specification- and service-level.

## Overall assessment

The plan gets several important things right:

- a clear ecosystem boundary rather than reimplementing retrieval packages;
- a useful L0–L4 layered architecture;
- fixture-first development and named acceptance criteria;
- explicit handling of superseded mainstems, partial fetch failures, and unknown unit mappings;
- an R-first strategy that matches the user ecosystem;
- reusable language-neutral query/classification/vocabulary assets.

The main issue is premature certainty. The document calls its schemas frozen and the plan build-ready before the proposed provider audit and before any end-to-end vertical slice. Several supposedly verified assumptions are already inconsistent with live behavior.

## Blocking findings

### 1. The AOI query has a silent GeoSPARQL bug

The spec names `gsp:sfIntersects`, but GeoSPARQL query functions use `geof:sfIntersects` from `http://www.opengis.net/def/function/geosparql/`. The incorrect form can return an empty successful result, which is worse than an explicit error because it makes a catalog appear complete.

The fallback through reference collections is not semantically equivalent: it can see reference gages but not all monitoring locations represented only in the graph.

**Resolution in v0.2:** correct namespace; live anchored test; incomplete spatial fallback must be reported as partial, never passed off as complete.

### 2. The parser contract does not match live JSON-LD

The checked reference-gage document contains a literal `hyf:HY_HydroLocationType`, a provider IRI string, nested `hyf:HY_IndirectPosition`, and WKT in an `@value` object. It also contains a `gsp:` key while declaring `geo:` in its context. Local prefix expansion aimed at one ideal shape is not robust enough.

**Resolution in v0.2:** standards-based expansion with bundled/allowlisted contexts, a tolerant Geoconnex-profile extractor, and structured diagnostics for unsupported or malformed variants.

### 3. A verified PID currently lands on a failing item route

The PID for mainstem `29559` returned the expected 303, but the direct `mainstems/items/29559` route returned HTTP 500 during review. A filtered collection query still returned the item.

**Resolution in v0.2:** item → filtered collection → negotiated JSON-LD fallback, with a large-mainstem fixture. The mainstems/v3 decision becomes a P0 gate.

### 4. The dataflow and naming contradict the stated product boundary

- “Every exported function returns tibble or sf” conflicts with client, list, S3 orchestration, path, and text returns.
- `gx_twin(aoi, dir, ...)` is later called with undeclared `recipe=`.
- `gx_package(fetched)` cannot accept the explicit harmonized stage and does not explain catalog-only packaging.
- The non-goal forbids “twin” language while the flagship API and report use it.

**Resolution in v0.2:** documented S3 objects; coherent catalog→plan→fetch→harmonize→package flow; `gx_snapshot()`; separate `gx_replay()`.

### 5. The frozen dataset ID can collide

The original row represents site × dataset × distribution, potentially with multiple variables, while `dataset_id` hashes only site, name, and distribution URL. It conflates dataset and distribution identity and can collide across variables.

**Resolution in v0.2:** separate dataset, distribution, and variable identifiers; explicit row cardinality; cross-language SHA-256 canonicalization; contracts remain experimental until the spike and provider audit.

### 6. Arbitrary SPARQL pagination is unsafe

Appending LIMIT/OFFSET to an arbitrary query is invalid or meaning-changing for ASK, CONSTRUCT, DESCRIBE, aggregates, nested queries, and existing limits. Without stable ordering, OFFSET pagination can omit or duplicate mutable results. The signature also omits the `page` argument described by the prose.

**Resolution in v0.2:** raw SPARQL executes once; only named SELECT templates with declared order, result key, and budget paginate.

### 7. Replay was incorrectly described as reproducibility

A future run against changing services cannot guarantee identical row counts. The manifest omitted query/registry/vocabulary hashes, response and asset hashes, HTTP validators, custom AOI geometry, and custom-handler replayability.

**Resolution in v0.2:** distinguish recipe replay from offline snapshot verification; preserve a request ledger and SHA-256 resource hashes; remove identical-live-row-count claims.

### 8. The handler interface was classification, not an executable protocol

An EDR URL is not enough to decide collection, query verb, AOI/location, parameter, time, or response format. URL matching also did not define request/byte/page budgets or normalized payload contracts. Runtime R package names in supposedly language-neutral YAML weaken the Python-port claim.

**Resolution in v0.2:** `probe → plan → fetch → normalize`; portable classifiers separated from language-specific implementations; a visible, deterministic fetch plan.

### 9. Provider URLs create an unaddressed SSRF boundary

Crawled `contentUrl` values and redirects can point at local/private/link-local/cloud-metadata destinations or oversized/compressed payloads. CSV content-type checking alone is insufficient.

**Resolution in v0.2:** HTTP(S)-only policy, address and redirect revalidation, encoded/decoded byte budgets, parser limits, and explicit trusted-private override policy.

### 10. The unit-conversion model is mathematically wrong

The spec calls conversions multiplicative but includes Fahrenheit↔Celsius. Temperature conversions require scale and offset.

**Resolution in v0.2:** directed reviewed rules using `converted = original * scale + offset`, canonical unit URIs, dimensional checks, and raw-value preservation.

## Important corrections and gaps

- `edr4r` is lowercase, current CRAN 0.1.1, and hosted at `ksonda/edr4r`; the old Q3 is resolved.
- Current `dataRetrieval` documentation is moving users from `readNWIS*` to `read_waterdata_*`; the new APIs should be primary and legacy handlers explicit compatibility paths.
- `huc12pp` is currently advertised by NLDI; prefer a returned mainstem value before COMID/VAA fallback.
- Re-exporting a Suggests-only `nhdplusTools` API is not a sound namespace contract. Use guarded wrappers.
- Mandatory code paths omitted dependencies for cache, YAML, HTML, hashing, CSV, Arrow, Quarto, and validation.
- `limit = Inf` defaults contradict the stated graph-politeness rules.
- HUC auto-detection must accept only lengths 2/4/6/8/10/12 and preserve leading zeros; five digits are county FIPS.
- Catalog partial failures need their own problems/request table, not only fetch status.
- A cache key must include representation-affecting headers, not only method/URL/body.
- CSV should be the portable default if Arrow remains optional.
- The R Frictionless package provides limited validation; full CLI validation belongs in CI.
- A weekly full triple count is needless load and a mutable exact count is a brittle smoke test.

## Roadmap assessment

The original 12–17 week flagship estimate is too confident before testing graph spatial performance, real provider coverage, EDR planning, manifest replay, and the mainstem vintage. The revised roadmap adds a 2–3 week P0 spike and splits the former P3:

- P3a can ship catalog-only, offline-verifiable snapshots;
- P3b adds fetch and harmonization handlers after their contracts are proven.

This sequencing contains the highest uncertainty and preserves useful intermediate releases.

## Recommendation

Adopt v0.2 as the working plan, close the P0 release gates, and perform the single vertical slice before freezing public schemas or committing to the remaining effort estimate. The first production PR should scaffold the package and evidence fixtures; it should not attempt all of M1 before the spike has validated the cache, JSON-LD, and redirect contracts together.

## Primary references consulted

- OGC GeoSPARQL 1.1: <https://opengeospatial.github.io/ogc-geosparql/geosparql11/document.html>
- `nhdplusTools` reference: <https://doi-usgs.github.io/nhdplusTools/reference/index.html>
- `dataRetrieval` status and reference: <https://doi-usgs.github.io/dataRetrieval/articles/Status.html>
- `edr4r` source: <https://github.com/ksonda/edr4r>
- `httr2` request/cache documentation: <https://httr2.r-lib.org/reference/index.html>
- R Frictionless documentation: <https://docs.ropensci.org/frictionless/>
