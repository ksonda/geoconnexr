# geoconnexr

<!-- badges: start -->
[![R-CMD-check](https://github.com/ksonda/geoconnexr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ksonda/geoconnexr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## About this experiment

This repository is an experiment in autonomous software development. I gave
ChatGPT Codex only an initializing prompt and access to relevant documentation
and related repositories, then let it research, critique, plan, scaffold, and
test the package autonomously. This is what it came up with; the result remains
experimental rather than production-ready.

The initializing prompt was:

> Design a build-ready plan for an R package called `geoconnexr` that helps
> water-data users discover Geoconnex resources, crosswalk among Geoconnex
> PIDs, NHDPlus COMIDs, HUCs, gage identifiers, and coordinates, and assemble
> reproducible watershed data packages. Use the available Geoconnex
> documentation and related repositories—including `nhdplusTools`,
> `dataRetrieval`, and `edr4r`—to ground the design and avoid duplicating
> existing capabilities. Specify the package architecture, public API, data
> contracts, error handling, provenance, security constraints, testing and CI
> strategy, and a phased implementation roadmap. Validate live infrastructure
> assumptions where practical, identify unresolved decisions explicitly, and
> make the plan detailed enough for a developer or coding agent to implement.

`geoconnexr` is an R-first discovery, identifier-crosswalk, and watershed
data-packaging client for the Geoconnex ecosystem.

The repository is currently in its P0 architecture-spike phase. The initial
scaffold provides versioned contracts, safe SPARQL template metadata,
identifier/recipe foundations, recorded infrastructure evidence, and offline
tests. Its first protocol slices add bounded, cache-aware PID resolution,
fail-closed JSON-LD negotiation and profile parsing, a native OGC API Features
client for Geoconnex reference collections, and a validated provider-gage PID
crosswalk. A second M4 substrate provides an explicit, checksum-pinned install
lifecycle for the optional 120 MB COMID-to-mainstem lookup without yet exposing
the evidence-gated public crosswalks; internal forward and release-scoped
inverse mappers operate only on verified local bytes. An unexported M5a
substrate now
supports bounded one-shot SELECT/ASK evidence through the package safety and
cache boundary. The M5b named-query manifest is separately hardened for local
rendering with exact template-byte pins and explicit disabled execution and
pagination. Offline M6a/M6b boundaries now canonicalize one custom polygonal
`sf`/`sfc` AOI into a bounded CRS84 recipe and safely hydrate AOI-only recipes
by recomputing geometry and digest integrity. Internal M6c and M9a/M9b
boundaries now validate a strict offline catalog value object and can create
then verify a deterministic, redacted catalog-only snapshot at a new local
destination. These are packaging substrates, not exported discovery, package,
snapshot, loading, or replay APIs. Public graph discovery, catalog
orchestration, and provider data retrieval remain gated on fixture-backed
production evidence.

## Intended workflow

```r
aoi <- gx_aoi("02070010", type = "huc")
catalog <- gx_catalog(aoi)
plan <- gx_fetch_plan(catalog, time = c("2025-01-01", "2025-01-31"))
fetched <- gx_fetch(plan)
harmonized <- gx_harmonize(fetched)
gx_snapshot(harmonized, dir = "potomac-snapshot")
```

This workflow is the target API, not a claim that every function above is
implemented in the P0 scaffold.

## Available in the P0 scaffold

```r
# Deterministic identifier recipe (no network)
aoi <- gx_aoi("02070010")
aoi$recipe

# Canonical custom polygon recipe (also no network or catalog work)
geometry <- sf::st_as_sfc(
  "POLYGON ((-77.2 38.8, -77.0 38.8, -77.0 39.0, -77.2 39.0, -77.2 38.8))",
  crs = "OGC:CRS84"
)
custom_aoi <- gx_aoi(geometry, type = "sf")
custom_aoi$recipe$aoi[c("crs", "wkb_sha256")]

# Resolve a PID while preserving its identity and redirect chain
resolution <- gx_resolve("https://geoconnex.us/ref/gages/1000001")
resolution[c("pid_uri", "landing_url", "problem_code")]

# Retrieve and safely expand its JSON-LD, then extract profile tables
document <- gx_jsonld("https://geoconnex.us/ref/gages/1000001")
location <- gx_parse_location(document)
datasets <- gx_parse_datasets(document)
attr(location, "diagnostics")

# Discover typed reference filters, then retrieve a bounded feature result
gx_ref_collections()
gx_ref_queryables("hu12")
mainstem <- gx_ref_features(
  "mainstems", query = list(id = "29559"), limit = 2L
)
gage <- gx_ref_feature("gages", "1000001")
attr(mainstem, "gx_reference")[c("truncated", "stop_reason", "requests")]

# Crosswalk opaque provider identifiers without coercing leading zeroes
gages <- gx_gage_to_pid(c("USGS-08332622", "USGS-00000000"))
gages[c("requested_provider_id", "status", "gage_uri", "mainstem_uri", "comid")]
attr(gages, "gx_crosswalk")[c("complete", "requests", "diagnostics")]

# Inspect optional mapping data without downloading or repairing it
gx_mainstem_lookup_info()

# The only operation allowed to make the disclosed 120,422,425-byte transfer:
# gx_mainstem_lookup_install(source = "release")
# Air-gapped installations can import the identical pinned bytes instead:
# gx_mainstem_lookup_install(source = "file", file = "nhdpv2_lookup.csv")

# Inspect and safely render a bounded named SPARQL template
gx_templates()
query <- gx_render_query(
  "sites_on_mainstem",
  list(
    mainstem_uri = "https://geoconnex.us/ref/mainstems/1622734",
    limit = 100,
    offset = 0
  )
)

# Inspect portable handler classification and reviewed unit rules
gx_classify_distribution(
  "https://reference.geoconnex.us/collections/gages/items"
)
gx_unit_conversions()
```

For custom geometry, `gx_aoi()` accepts exactly one valid, non-empty XY
`POLYGON` or `MULTIPOLYGON` with an explicit CRS. It transforms the geometry to
`OGC:CRS84` with PROJ networking disabled, rounds coordinates to a declared
nine-decimal-degree grid, applies GeoJSON ring winding, canonicalizes ring start
plus hole and multipolygon member order, and records canonical GeoJSON with a
portable little-endian WKB SHA-256. Inputs are limited to 100,000 coordinate
positions and 8 MiB for each canonical GeoJSON/WKB representation. Invalid,
non-finite, out-of-bounds, or grid-collapsed geometry is rejected rather than
repaired; antimeridian rings must be explicitly pre-cut. This M6a operation is
entirely offline: recipe pipeline fields
describe an intended replay boundary and do not mean that `gx_catalog()` ran.
The internal M6b reader can hydrate only this exact AOI fragment from a decoded
list or literal bounded JSON. It rejects paths, URLs, duplicate members,
noncanonical geometry, and GeoJSON/WKB hash disagreement; it does not execute
replay. See [ADR 0014](docs/decisions/0014-offline-custom-aoi-boundary.md) and
[ADR 0016](docs/decisions/0016-offline-aoi-recipe-hydration.md).

The internal M9a verifier reads only a fixed `manifest.json` beneath one
non-symlink snapshot root. It validates the bounded manifest and embedded
request-ledger shape, rehydrates the AOI through M6b, inventories a closed tree
of portable relative paths, and checks every present resource's exact size and
SHA-256 without parsing its contents. Missing optional resources are reported;
missing required resources, present mismatches, symlinks, hard-link aliases,
unreadable directories, special files, and undeclared files fail closed. This
proves consistency relative to the supplied,
unsigned manifest—not authenticity or historical request provenance—and remains
an unexported prerequisite for future `gx_replay(refresh = FALSE)`. See
[ADR 0017](docs/decisions/0017-offline-snapshot-verification.md).

The internal M6c catalog is a separately validated value object rather than an
alias for any protocol response. It requires typed CRS84 point sites,
site-linked flattened datasets, explicit recoverable problems,
manifest-shaped physical/cache attempts, and count-reconciled procedural
completeness. Its export views redact sensitive URI components for all schemes
and add stable site/variable fingerprints so redacted displays cannot corrupt
identity joins. Its first contract intentionally permits no reference layers and
does not implement graph/profile merge or live discovery. The internal M9b
writer accepts only that revalidated object, writes redacted deterministic CSV
views and manifest-v1 through a sibling staging directory, verifies the closed
tree before and after publication, and refuses any existing destination. The
writer remains unexported and its manifest is explicitly non-replayable. See
[ADR 0018](docs/decisions/0018-internal-catalog-value-object.md) and
[ADR 0019](docs/decisions/0019-catalog-only-snapshot-writer.md).

`gx_resolve()`, `gx_jsonld()`, and the `gx_ref_*()` functions make bounded
network requests, account for every physical retry, and validate DNS and every
redirect target before transport. A package-owned monotonic per-host throttle
spaces physical attempts across clients; cache hits do not wait. `gx_jsonld()`
never lets the HTML or JSON-LD parser fetch a URL: it replaces exact allowlisted
contexts with hash-pinned bundled assets and rejects unknown contexts. The two
profile parsers are offline and return structured diagnostics instead of
silently dropping tolerated production quirks.

The rendered SPARQL templates are inspectable, but no public graph executor is
exported. `gx_templates()` exposes exact stored bytes/hashes, projected and
required result variables, the reviewed `ORDER BY`, result-key scope, and the
specific reasons paging is disabled. `gx_render_query()` only renders locally;
it does not execute, chunk, or paginate. Internal opt-in evidence checks accept
only trusted read-only SELECT/ASK text, make one logical POST without rewriting
or pagination, parse bounded SPARQL 1.1 Results JSON before cache admission,
and preserve redacted request/attempt provenance. ADR 0004 still governs the
endpoint and public API gate.

The native reference client discovers collection and queryable schemas,
validates equality filters before sending them, forces classic GeoJSON for item
pages, normalizes advertised identifier fields to character, and follows only
same-endpoint pagination links. Collection-wide requests require an explicit
`allow_unbounded = TRUE` opt-in and still obey row, page, response-byte, and
cumulative-byte ceilings. Single-feature lookup records its ordered item,
validated-filter, and JSON-LD fallback attempts; JSON-LD fallback results are
visibly marked incomplete. Dataset fetch handlers and public snapshot writing
remain planned interfaces; the internal M9b writer is limited to validated
catalog-only resources and is labeled non-replayable in its manifest.

The first crosswalk validates the reference service's advertised
`provider_id`, gage identity, and PID before returning a match. Repeated inputs
are queried once and expanded in order; no match receives an explicit sentinel
row, and multiple distinct matches are all returned as ambiguous. Advertised
COMIDs remain character values. Advertised mainstem URIs are retained but are
diagnosed as vintage-unverified until the mainstem policy is resolved.

Query-bearing feature responses are intentionally non-cacheable, so filtered
offline replay and gage crosswalk lookup are not promised. Cross-vintage
`mainstems_v3` identity aliasing also remains unresolved and is never inferred
silently. VAA `levelpathi` values are not Geoconnex mainstem identifiers, so
the package does not construct mainstem PIDs from them.

The optional NHDPlusV2 lookup is stored outside the expiring HTTP cache and is
addressed by its pinned v3.2 SHA-256 digest. Installation streams to a staging
file, validates each HTTPS redirect, exact size, digest, CSV schema, row count,
known answers, and a non-sensitive provenance receipt before atomic exposure.
Lookup inspection and the internal vectorized forward and inverse mappers never
download, refresh, or repair data. Inverse matches are complete only within the
pinned mapping release, use deterministic COMID ordering, and explicitly do not
assert current service state. The public `gx_comid_to_mainstem()` and
`gx_mainstem_to_comids()` functions remain unexported until the mainstem
current/superseded contract is selected.

JSON-LD and parser contracts remain experimental. The fixture corpus now
contains six observed, minimized pages from four landing hosts and five
semantic providers, plus synthetic conformance/adversarial cases. This closes
the P0 five-real-pages/three-providers evidence gate with one-page margin;
synthetic fixtures remain explicitly excluded from that count.

## Design commitments

- Preserve the original `geoconnex.us` PID as the identity key across 303
  redirects.
- Discover live OGC API collections and queryables rather than treating a
  checked inventory as permanent.
- POST SPARQL queries and use typed template parameters; never splice raw user
  strings into queries.
- Keep recipe replay distinct from offline snapshot verification.
- Treat successful offline verification as manifest-relative consistency, not
  authenticity or proof of historical request provenance.
- Treat source-specific failures and incomplete discovery as visible data,
  not silent empty results.
- Preserve provider provenance and original values throughout harmonization.
- Keep provider-controlled JSON-LD bounded by request, byte, depth, member,
  expansion, HTML-candidate, node-fragment, and output-row budgets.
- Disclose large optional assets, verify their content identity, and never
  trigger their download from a crosswalk call.

## Development

```r
pak::pak(c("devtools", "testthat"))
devtools::test()
devtools::check()
```

Live infrastructure checks are bounded, opt-in, and separate from normal unit
tests. See `data-raw/README.md` after cloning.

## Planning documents

- [Validated build roadmap](geoconnexr-spec-v0.2.md)
- [Review of the original proposal](geoconnexr-plan-review.md)
- [Architecture decisions](docs/decisions/)

## Status

The package is experimental and has no stable API yet. Contract changes during
the 0.x series will be versioned and accompanied by migration notes once
serialized artifacts are released.
