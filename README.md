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
crosswalk. A second M4 slice provides an explicit, checksum-pinned install
lifecycle for the optional 120 MB COMID-to-mainstem lookup and public
release-scoped forward and inverse crosswalks over verified local bytes. These
crosswalks make no live currentness claim. An unexported M5a
substrate now
supports bounded one-shot SELECT/ASK evidence through the package safety and
cache boundary. The M5b named-query manifest is separately hardened for local
rendering with exact template-byte pins and explicit disabled execution and
pagination. Offline M6a/M6b boundaries now canonicalize one custom polygonal
`sf`/`sfc` AOI into a bounded CRS84 recipe and safely hydrate AOI-only recipes
by recomputing geometry and digest integrity. Internal M6c and M9b boundaries
validate a strict offline catalog value object and its creation-only writer.
Public `gx_snapshot()` can now create that deterministic, redacted catalog-only
snapshot at a new local destination, and `gx_snapshot_verify()` exposes M9a
closed-tree integrity evidence without loading or replaying resources. Public
`gx_snapshot_requests()` exposes M9e's typed manifest ledger only after binding
the canonical stored `requests.csv` bytes and re-verifying the tree. Public
`gx_snapshot_catalog_view()` exposes the fixed typed redacted sites, datasets,
problems, and requests while remaining explicitly distinct from a live
`gx_catalog` because redaction is not lossless.
Public `gx_package()` can now publish or replace validated catalog, fetched, or
harmonized inputs through deterministic CSV/raw resources and final closed-tree
verification. Harmonized inputs may opt into the fixed Arrow/Parquet profile
with `timeseries = "parquet"`. Explicit `report = TRUE` adds one fixed,
execution-disabled and cache-disabled verified HTML report through the reviewed
Quarto runtime; the default performs no rendering. Replacement admits only an intact fixed-writer
package, stages
the new generation before moving the prior package, and restores the prior
generation on detected installation or verification failure. Fetched and
harmonized inputs require their explicit source catalog. Public
`gx_package_load()` verifies those fixed packages before and
after reading and returns every declared resource as exact bounded bytes;
`gx_package_tables()` adds canonical character-only tibbles for every CSV and
leaves verified Parquet bytes opaque.
An internal M9p substrate now applies exact fixed storage types to package-owned
tables without interpreting provider-native payloads or reconstructing live
workflow objects. Public `gx_package_hydrate()` exposes that exact redacted,
read-only inspection view, including exact typed Parquet observations when
Arrow is available. Public `gx_report()` exposes exact verified report HTML in
memory or atomically copies it to one absent file without invoking Quarto.
Public `gx_replay(..., refresh = FALSE)` now provides one offline, read-only
stored-state inspection result over the fixed catalog-snapshot and package
profiles. It verifies and loads the typed redacted view plus any stored report,
but does not execute a recipe or reconstruct live workflow objects. Live
refresh and procedural replay remain deferred.
Internal M9t inspects Arrow and Quarto package metadata without loading either
namespace. M9u pins Arrow R 14.0.0 and a fixed in-memory Parquet-2.4 profile:
required symbols are resolved only after the selected operation, exact
redacted typed observations are written and read back from bounded raw bytes,
and determinism is claimed only within one Arrow version. M9v now carries that
profile through public package creation, replacement, loading, and hydration;
M9w pins Quarto R 1.5.1 and resolves its reviewed report-facing symbols without
invoking the CLI or rendering. M9x separately admits only a normalized,
unchanged Quarto CLI 1.8.27 or newer through one bounded `--version` command;
it does not render, and the repository host's CLI 1.3.433 remains blocked.
M9y derives one fixed code-free report from a typed package view, invokes only
that admitted CLI path with execution and caching disabled, and accepts only a
bounded minimal HTML document in a closed private render tree. Scripts, active
embedded objects, refreshes, external links, unexpected artifacts, source or
CLI mutation, and output forgery fail closed. Exact source and HTML bytes are
retained in memory after private-stage cleanup. M9z binds those exact bytes to
their originating M9k bundle as one `report/index.html` resource and carries
the profile through verified staged creation and owned replacement. M9aa now
exposes that path through explicit `gx_package(..., report = TRUE)`, admits
report packages through offline loading/table/hydration inspection, and adds
`gx_report()` for exact in-memory access or absent-file export. Report-free
manifests remain unchanged. M9ab wraps those fixed loading profiles in public
offline `gx_replay(refresh = FALSE)` evidence while retaining explicit
non-replayable, unsigned, and no-refresh limitations.
M9ac adds an internal deterministic Frictionless Data Package v1 descriptor
over the exact fixed resource bundle. Canonical CSVs have exact all-string
Table Schemas, non-CSV resources are explicit and generic, and a dedicated CI
gate validates the fixed catalog, fetched, and harmonized CSV profiles with
Python Frictionless CLI 5.19.0. It does not publish a descriptor or authorize
refresh or replay.
M9ad exposes that reviewed all-CSV profile through explicit
`gx_package(..., frictionless = TRUE)`. The resulting `datapackage.json` is a
manifest-declared resource, survives verified replacement and offline typed
inspection, and is rederived from stored CSV bytes during loading. Defaults
remain descriptor-free.
M9ae validates those remaining fixed resource families before public exposure.
Retained provider bodies, Parquet observations, and report HTML are described
as opaque binary Data Resources whose custom `geoconnexr` metadata retains the
true format. Core Python Frictionless CLI 5.19.0 validates their exact files,
sizes, and hashes without invoking optional semantic format parsers.
M9af completes the fixed-package M9 roadmap by composing that validated profile
through public raw, Parquet, and report publication, replacement, loading,
typed inspection, report access, and offline stored-state inspection. Live
refresh and procedural replay remain explicitly deferred under ADR 0066 until
a complete reproducible request recipe exists; they are not another
fixed-package implementation gate.
M10 adds offline publisher tools under one explicit profile. `gx_context()`
returns the fixed local context, and `gx_jsonld_build()` turns exact catalog
site and dataset tables into deterministic publisher-profile 1.0.0 JSON-LD.
`gx_jsonld_validate()` reports errors and warnings with stable rule IDs, JSON
pointers, and suggested fixes. `gx_sitemap()` writes up to 50,000 canonical
HTTP(S) URIs to one verified XML sitemap at a new destination. None of these
functions submits or publishes content remotely.
The installed publisher conformance corpus now supplies shared known answers
for the language-neutral input, deterministic JSON-LD, exact validation
findings, and sitemap bytes. The R suite and a standard-library Python harness
both reproduce those answers. This establishes P4 port feasibility without
claiming a supported Python client.
Internal M7a adds deterministic, selection-only fetch plans bound
to strict portable-classifier and R-implementation metadata assets; M7b adds a
separate host-specific advisory check using bounded direct reads of selected
optional-package metadata without loading their namespaces. Internal M7c adds a
host-independent value object for inert, policy-bound direct-CSV GET intents;
M7d allocates the global physical-attempt and byte budgets into held
all-handler reservations and bounded, non-executable direct-CSV logical request
plans. M7e validates one bounded caller-supplied direct-CSV response candidate
offline while preserving the exact raw bytes and explicitly declining provider
provenance, budget-consumption, transport, replay, or execution claims. M7f
parses only those bytes under a strict bounded UTF-8 comma/header profile into
an exact character-only table while retaining the caller-supplied provenance
limit. M7g executes one selected direct-CSV request through the package-owned
DNS-pinned transport, validates and parses the observed provider response, and
binds it to one charged physical-attempt ledger row without exposing a public
fetch API. M7h adds bounded sequential direct-CSV orchestration, exact
one-row-per-distribution status reconciliation, isolated transport/parse
failures, and a deterministic dry run that performs no host or provider work.
Successful results retain compact execution and character-table evidence
without raw bodies or repeated plan chains. M7i adds the first non-CSV handler
slice: one held-reservation OGC API Features request, an invocation-time native
symbol check, strict single-page GeoJSON-to-`sf` normalization, and one exact
charged attempt. M7j introduced shared global scheduling, M7k added WQP Result,
and M7n now schedules CSV, WQP, EDR position, current USGS continuous and daily
data, and OGC in one global order. All six paths share count and byte admission, isolate
handler failures, and
reconcile one terminal status per distribution. WQP requires invocation-time
`dataRetrieval::importWQP()` output to match the strict CSV parser. EDR records
one CRS84 WKT point, parameter, UTC interval, and CoverageJSON representation,
then requires invocation-time `edr4r::covjson_to_tibble()` output to exactly
match the strict bounded PointSeries result. The USGS slice checks
`dataRetrieval::read_waterdata_continuous()` or
`dataRetrieval::read_waterdata_daily()` capability before one package-owned
request. Continuous observations bind an exact site, parameter, and UTC
interval. Daily values additionally bind one statistic code, preserve local
calendar dates as `Date`, and keep last-modified instants in UTC. Both strict
single-page GeoJSON parsers preserve measurement values as strings and report,
but never follow, a next page.

ADR 0034 freezes those six paths as the supported M7 subset. Public
`gx_fetch_plan()` now exposes deterministic planning for a validated
`gx_catalog`, and public `gx_fetch()` returns a validated `gx_fetched` status,
payload, and provenance object. Public `gx_catalog()` now builds the validated
catalog from one bounded graph page, explicit site PIDs, or caller-supplied
local JSON-LD profiles. Automatic graph discovery remains dependent on an
upstream service that can time out; provider variants beyond the supported
subset, pagination, packaging, loading, and replay remain separate work.
M8a–M8e now add an offline `gx_harmonize()` boundary over the frozen fetched
object. It normalizes strict EDR position, current USGS continuous/daily, and
catalog-aligned single-characteristic WQP time-series tables. WQP civil times
use only the reviewed fixed offset for an active EPA timezone code; unknown
codes remain native-only. Explicit `gx_csv_mapping()` objects can normalize
single-variable direct-CSV tables with exact UTC datetime, value, unit, and
optional qualifier columns; no column is guessed. The result preserves source
order, qualifiers, original values, and native payload access, and applies only
exact directed reviewed unit rules. Explicit `gx_feature_mapping()` objects
apply the same distribution-bound model to OGC API Features properties while
excluding generated feature identifiers and geometry from observation roles.
Ambiguous variables, catalog/native unit conflicts, and unknown units remain
visible and unconverted. Unfiltered, mixed-characteristic, or incomplete WQP
results remain native-only, as do unmapped CSV and Features results.

## Intended workflow

```r
aoi <- gx_aoi("VA", type = "state")
catalog <- gx_catalog(
  aoi,
  site_uri = "https://geoconnex.us/iow/wqp/21VASWCB-WMPO001",
  max_sites = 1L
)
fetch_window <- as.POSIXct(
  c("2017-01-01 00:00:00", "2017-12-30 23:59:59"),
  tz = "UTC"
)
plan <- gx_fetch_plan(
  catalog, time = fetch_window, max_datasets = 1L, max_bytes = 1024^2
)
fetched <- gx_fetch(plan)
fetched$results$data[[1L]]
harmonized <- gx_harmonize(fetched)
package <- gx_package(
  harmonized,
  "path/to/new-package",
  catalog = catalog
)
loaded <- gx_package_load(package$path)
loaded$contents[["data/observations.csv"]]
tables <- gx_package_tables(package$path)
tables$tables[["data/observations.csv"]]
hydrated <- gx_package_hydrate(package$path)
hydrated$harmonized$observations

parquet_package <- gx_package(
  harmonized,
  "path/to/new-parquet-package",
  catalog = catalog,
  timeseries = "parquet"
)
parquet_loaded <- gx_package_load(parquet_package$path)
parquet_loaded$contents[["data/observations.parquet"]]
gx_package_hydrate(parquet_package$path)$harmonized$observations

# Requires Quarto R >= 1.5.1 and Quarto CLI >= 1.8.27.
reported_package <- gx_package(
  harmonized,
  "path/to/new-reported-package",
  catalog = catalog,
  report = TRUE
)
report <- gx_report(reported_package)
report$html
gx_report(reported_package, "path/to/report-copy.html")
```

Catalog → plan → fetch → harmonize → package is now public for the frozen
supported subset and deterministic CSV/raw or opt-in Arrow/Parquet package
profiles. Package resources
can be loaded offline as exact verified bytes without reconstructing live
workflow objects or authorizing replay, and canonical CSV resources can be
inspected as character-only tibbles without type inference.
Public `gx_package_hydrate()` applies fixed types to package-owned schemas while
keeping provider-native resources character-only or opaque. It remains an
inspection view and never reconstructs live workflow objects. Fixed report
access is offline and byte-preserving; only explicit package creation with
`report = TRUE` invokes the reviewed Quarto runtime.

The [HUC10 end-to-end case study](vignettes/end-to-end-huc10.Rmd) runs this
whole chain against the current USGS daily API and includes a verified
network-free package for local demonstration.

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

# Build and validate the same fixed publisher profile without network access
publisher <- list(
  uri = "https://example.org/provider/water-office",
  name = "Water Office",
  url = "https://example.org"
)
# published <- gx_jsonld_build(catalog$sites, catalog$datasets, publisher)
# gx_jsonld_validate(published)
# gx_sitemap(published_site_uris, "path/to/new-sitemap-directory")

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
gx_target_units()

# Discover one known PID and preview its fetch without provider work
catalog <- gx_catalog(
  gx_aoi("VA"),
  site_uri = "https://geoconnex.us/iow/wqp/21VASWCB-WMPO001",
  max_sites = 1L
)
fetch_window <- as.POSIXct(
  c("2017-01-01 00:00:00", "2017-12-30 23:59:59"),
  tz = "UTC"
)
plan <- gx_fetch_plan(
  catalog, time = fetch_window, max_datasets = 1L, max_bytes = 1024^2
)
preview <- gx_fetch(plan, dry_run = TRUE)

# After a live fetch, harmonization itself is offline:
# fetched <- gx_fetch(plan)
# harmonized <- gx_harmonize(fetched)
# harmonized$observations
#
# A schema-free direct CSV needs an explicit distribution-scoped mapping:
# csv_map <- gx_csv_mapping(
#   distribution_id = fetched$results$distribution_id[[1]],
#   datetime_column = "datetime", value_column = "value",
#   unit_column = "unit", qualifier_column = "qualifier",
#   missing_values = c("", "NA")
# )
# harmonized <- gx_harmonize(fetched, csv_mappings = csv_map)
#
# OGC API Features also requires an explicit property mapping:
# feature_map <- gx_feature_mapping(
#   distribution_id = fetched$results$distribution_id[[1]],
#   datetime_property = "observed_at", value_property = "result_value",
#   unit_property = "result_unit", qualifier_property = "result_qualifier",
#   missing_values = c("", "NA")
# )
# harmonized <- gx_harmonize(fetched, feature_mappings = feature_map)
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

The public `gx_snapshot_verify()` boundary reads only a fixed `manifest.json`
beneath one
non-symlink snapshot root. It validates the bounded manifest and embedded
request-ledger shape, rehydrates the AOI through M6b, inventories a closed tree
of portable relative paths, and checks every present resource's exact size and
SHA-256 without parsing its contents. Missing optional resources are reported;
missing required resources, present mismatches, symlinks, hard-link aliases,
unreadable directories, special files, and undeclared files fail closed. This
proves consistency relative to the supplied,
unsigned manifest—not authenticity or historical request provenance—and remains
an evidence-only boundary rather than loading or replay. See
[ADR 0017](docs/decisions/0017-offline-snapshot-verification.md) and
[ADR 0041](docs/decisions/0041-public-offline-snapshot-verification.md).

```r
verification <- gx_snapshot_verify("path/to/snapshot")
verification$resources

snapshot <- gx_snapshot(catalog, "path/to/new-snapshot")
snapshot$verification

requests <- gx_snapshot_requests("path/to/snapshot")
requests$requests

catalog_view <- gx_snapshot_catalog_view("path/to/snapshot")
catalog_view$sites
catalog_view$datasets

package <- gx_package(
  harmonized,
  "path/to/new-package",
  catalog = catalog
)
package$verification
```

The M6c catalog is a separately validated value object rather than an alias for
any protocol response. It requires typed CRS84 point sites,
site-linked flattened datasets, explicit recoverable problems,
manifest-shaped physical/cache attempts, and count-reconciled procedural
completeness. Its export views redact sensitive URI components for all schemes
and add stable site/variable fingerprints so redacted displays cannot corrupt
identity joins. Its first contract intentionally permits no reference layers
and is now populated publicly by one bounded graph page, explicit PID profiles,
or caller-supplied local profiles under ADR 0035. Reference-layer population
and general graph/profile merge remain deferred. The internal M9b
writer accepts only that revalidated object, writes redacted deterministic CSV
views and manifest-v1 through a sibling staging directory, verifies the closed
tree before and after publication, and refuses any existing destination. The
writer remains internal, while public `gx_snapshot()` exposes only its
catalog-only, creation-only path and rejects fetch, report, and overwrite. Its
manifest is explicitly non-replayable. See
[ADR 0018](docs/decisions/0018-internal-catalog-value-object.md) and
[ADR 0019](docs/decisions/0019-catalog-only-snapshot-writer.md), then
[ADR 0042](docs/decisions/0042-public-catalog-only-snapshot-creation.md).

The internal M7a plan groups the catalog's distribution-by-variable rows into
one deterministic distribution row plus ordered parameter rows per distribution
identity. It reclassifies every admitted URL against exact-byte-hashed portable
and R implementation registries, checks target safety offline without DNS,
intersects requested and catalog time ranges, and applies a stable
provider/site/distribution order before `max_datasets`. The plan contains an
exact empty request list: all implementations are planned, all handlers are
non-replayable, and `execution_ready` is false. Construction probes no package,
calls no handler, and performs no DNS, network, cache, or file write. Request
construction, execution, registration, and serialization remain later M7 work.

The separate M7b package-capability report embeds and revalidates that plan,
then uses the built-in probe to read a bounded installed `DESCRIPTION` directly
for each unique allowlisted package needed by a selected distribution. It
ignores `Meta/package.rds`, does not load namespaces, and does not inspect or
call package symbols. Missing and old versions become explicit skip statuses,
while present unpinned or version-satisfying packages remain
`blocked_implementation_planned`: package metadata never means a handler is
ready. The report is host-specific, advisory, non-replayable, and never
execution-ready; future execution must recheck immediately before invocation.
See [ADR 0020](docs/decisions/0020-internal-fetch-plan-selection.md) and
[ADR 0021](docs/decisions/0021-host-package-capability-preflight.md).

The separate M7c `gx_csv_get_intents` S3 object implements contract 0.1.0. Its
top level contains the byte-identical revalidated M7a plan, an exact shared
inert policy, selected-CSV intent rows in global fetch order, coverage for every
distribution, and exact non-executable metadata. Policy fixes GET, CSV `Accept`,
identity encoding, and an empty body while leaving credential, redirect, cache,
and parser behavior unbound. Each intent stores the declared media type and only
a redacted canonical URL. Its full offline-canonical target is re-derived from
the embedded plan and bound, with every policy field, through
`gx_contract_hash()` under the `geoconnexr.csv-get-intent.v1` namespace. M7c
does not consult M7b or `readr`, allocate request/byte/parser budgets, or
authorize DNS, redirects, transport, caching, parsing, or execution. See
[ADR 0022](docs/decisions/0022-inert-direct-csv-get-intents.md).

The internal M7d `gx_csv_request_plan` S3 object implements contract 0.1.0 and
embeds that M7c object byte-for-byte, leaving the nested M7a `requests` list
empty. Its exact allocation pass considers every selected distribution in
global fetch order, reserves at most one physical attempt per admitted prefix
row, and independently fair-shares the aggregate encoded- and decoded-byte
budgets. Non-CSV shares are held instead of being reassigned to CSV. Only a
reserved CSV intent receives a redacted `planned_non_executable` logical
request plan. Callers must explicitly supply response-byte, row, and column
ceilings; no product defaults are inferred. M7d binds GET, zero redirects and
retries, one possible physical attempt, cache bypass, status 200, CSV response
media, identity content encoding, and shape limits. It does not implement DNS,
transport, response validation, a result schema, CSV parser
semantics/enforcement, attempt identity or ledgers, timeout policy,
serialization, execution, or replay. See
[ADR 0023](docs/decisions/0023-bounded-direct-csv-request-plans.md).

The internal M7e `gx_csv_validated_response` S3 object implements contract
0.1.0 and embeds M7d byte-for-byte. It accepts one exact caller-supplied
in-memory candidate for one existing direct-CSV logical request, then validates
status 200, bounded singleton critical headers, `text/csv` or
`application/csv`, absent or identity content encoding, optional strict
Content-Length equality, the exact re-derived canonical no-redirect target, and
the encoded, decoded, and response-byte ceilings. The full target and arbitrary
headers are discarded; the exact bounded raw body, digest, normalized response
facts, and a domain-separated validation identity are retained. This proves
only that caller-supplied facts and bytes satisfy the envelope. Metadata keeps
provider observation, budget consumption, parsing, transport authorization,
execution readiness, and replay false. See
[ADR 0024](docs/decisions/0024-offline-direct-csv-response-validation.md).

The internal M7f `gx_csv_parsed_response` S3 object implements contract 0.1.0
and embeds M7e byte-for-byte. It accepts one explicit total-field ceiling and
parses only M7e's retained body under a fixed strict UTF-8 profile: optional
leading BOM, comma delimiter, doubled-quote escape, required exact unique
header, LF/CRLF records, no quoted newlines or blank records, no trimming,
missing-value conversion, type inference, or name repair, and character
storage for every cell. A raw-byte scan enforces selected and implementation
row/column limits plus input, field, header, and aggregate-field budgets before
allocating the result. Chunked result and parse identities bind the exact
names, values, dimensions, policy, limits, BOM presence, M7e validation, and
body digest. Metadata records parser and result validation but still denies
provider observation, physical attempts, fetch-budget consumption, transport,
execution, serialization, and replay. See
[ADR 0025](docs/decisions/0025-bounded-offline-direct-csv-parsing.md).

The internal M7g `gx_csv_execution` S3 object implements contract 0.1.0. It
re-derives one selected direct-CSV target, binds explicit timeout and per-host
interval policy, and performs one cache-bypassing GET through the same
DNS-revalidated, public-address-pinned, redirect-disabled streaming transport
used by the protocol clients. M7e validates the observed response before M7f
parses it. M7g preserves that nested chain byte-for-byte and adds one
host-specific execution identity plus one charged physical-attempt ledger row
with a redacted URL, resolved host/IP, status, media, exact bytes, body digest,
and completion time. The completed evidence remains non-replayable and does not
authorize another request or any non-CSV handler. See
[ADR 0026](docs/decisions/0026-single-attempt-direct-csv-execution.md).

The internal M7h `gx_csv_orchestration` S3 object implements contract 0.1.0.
It evaluates M7d direct-CSV requests in exact global request order, admits them
under explicit count and aggregate reserved-response ceilings, and invokes
M7g sequentially with one derived child scope per request. A transport or parse
failure produces one stable redacted terminal row and does not abort later
unrelated direct-CSV requests. Every M7d coverage row has exactly one status,
including dry-run, batch-deferred, handler-unimplemented, not-selected, and
reference-only rows. `dry_run = TRUE` performs the same admission and status
projection without DNS, transport, clocks, throttling, cache, or writes.
Successful M7g objects are validated and compacted into execution, attempt,
response-validation, fixed parser-policy, exact character schema/data, and
identity facts; raw response bodies and repeated M7d-to-M7a plan chains are not
retained. M7h is still unexported and does not make non-CSV handlers,
registration, runtime invocation preflight, serialization/replay, or public
`gx_fetch()` available. See
[ADR 0027](docs/decisions/0027-bounded-direct-csv-orchestration.md).

The internal M7i `gx_oaf_request_plan` and `gx_oaf_execution` S3 objects add
the first complete non-CSV handler slice. A selected, query-free OGC API
Features `/collections/{id}/items` distribution reuses its exact M7d
one-attempt and byte reservation. Planning adds only fixed `f=json` and
`limit` parameters. Execution re-resolves the native package symbol directly
before invocation, then performs one DNS-pinned, cache-bypassing request with
no redirect, retry, or next-page follow. Strict GeoJSON validation returns an
`sf` result; an advertised next page is reported as truncation. Retained
response bytes, result rows, invocation facts, and the charged attempt are
rebound during whole-object validation. M7i remains unexported and does not yet
join OGC results to M7h, map provider filters, paginate, register plugins,
serialize/replay, or expose `gx_fetch()`. See
[ADR 0028](docs/decisions/0028-single-page-oaf-handler.md).

The internal M7n `gx_fetch_orchestration` S3 object implements contract 0.5.0.
It derives direct-CSV, compatible WQP Result, EDR position, current USGS
continuous and daily, and OGC API Features candidates from one M7d plan, keeps
their original global fetch order, and admits all six handlers under one explicit
request-count and aggregate reserved-response budget. Live work is sequential
and continues after typed handler failures; every M7d coverage row receives one
exact terminal status. A missing OGC symbol, WQP parser, required EDR
capability, or required dataRetrieval continuous/daily capability charges no physical
attempt, while transport and parse failures retain only bounded redacted
evidence. Dry run performs the same planning and status projection without DNS,
transport, clocks, throttling, cache, writes, or symbol resolution. CSV and WQP
successes retain their established compact evidence. EDR successes retain the
bounded CoverageJSON body, fixed PointSeries table/schema, parse hashes,
`edr4r` normalizer-agreement facts, and one attempt so strict validation does
not require the optional package later. OGC successes retain their bounded
GeoJSON body so validation can rebuild M7i and reparse the exact `sf` result.
USGS continuous successes retain the bounded GeoJSON body, fixed eleven-column
table/schema, string-valued measurements, parse hashes, truncation facts, and
one attempt. Daily successes retain the same evidence shape while preserving
observation dates as `Date`, binding an exact statistic code, and allowing an
absent `numberMatched` only as unknown completeness. Validation does not load
dataRetrieval later. M7n remains the internal execution substrate. ADR 0034
publishes `gx_fetch_plan()` and `gx_fetch()` over this exact supported subset,
returning `gx_fetched` 0.1.0 with one public status row per distribution,
handler-native payloads, and the validated orchestration provenance. Other EDR
queries, latest or legacy USGS execution, pagination, registration, and
serialization/replay are deferred enhancements rather than M8 gates. See
[ADR 0029](docs/decisions/0029-cross-handler-orchestration.md) and
[ADR 0030](docs/decisions/0030-single-response-wqp-handler.md), then
[ADR 0031](docs/decisions/0031-single-response-edr-position-handler.md) and
[ADR 0032](docs/decisions/0032-single-page-usgs-continuous-handler.md), then
[ADR 0033](docs/decisions/0033-single-page-usgs-daily-handler.md) and
[ADR 0034](docs/decisions/0034-freeze-m7-supported-fetch-subset.md). M8a then
normalizes only the three strict time-series result shapes and applies reviewed
single-step affine rules without provider work. Other payloads remain
losslessly available through the embedded `gx_fetched` object; see
[ADR 0036](docs/decisions/0036-conservative-harmonization-boundary.md) and
[ADR 0037](docs/decisions/0037-filtered-utc-wqp-harmonization.md), followed by
[ADR 0038](docs/decisions/0038-reviewed-wqp-timezone-offsets.md) and
[ADR 0039](docs/decisions/0039-explicit-direct-csv-mappings.md), then
[ADR 0040](docs/decisions/0040-explicit-oaf-observation-mappings.md).

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
remain planned interfaces: M7a selects distributions without constructing or
executing requests, M7b only inspects host package metadata, and M7c records
inert direct-CSV intent identity without granting authority. M7d allocates
non-consumed all-handler reservations and inert direct-CSV request plans while
keeping transport authority false. M7e validates bounded caller-supplied
direct-CSV response candidates without claiming provider provenance. M7f
strictly parses their exact retained bytes into non-authoritative character
tables without loading an optional parser package. M7g can execute exactly one
selected direct-CSV request through the bounded package transport and bind the
provider response to its charged attempt. M7h can orchestrate multiple bounded
direct-CSV requests with exact statuses and continue-on-error behavior, but
the public M7 boundary now uses the later six-handler orchestrator. The M8a
harmonizer consumes only the frozen public result and performs no transport.
The M9b/M9d writer path is limited to validated catalog-only resources and is
labeled non-replayable in its manifest.
The M9e loader recognizes only that exact writer profile and proves
that `requests.csv` is the canonical deterministic projection of the typed
manifest request ledger; public `gx_snapshot_requests()` exposes that exact
typed evidence. No redacted value is reconstructed as an identity. Internal M9g
provides
their loading prerequisite: each fixed catalog CSV can be parsed only as an
exact character table whose quote-all UTF-8 LF serialization matches the
verified bytes. Blank cells, WKT, JSON, timestamps, and logicals remain
uninterpreted. Internal M9h builds an exact typed redacted view over that
evidence: site WKT becomes bounded CRS84 point geometry, fixed writer
timestamps become UTC values, logical fields accept only `true` or `false`, and
conforms-to text becomes canonical JSON string arrays. All other redacted and
blank strings remain unchanged, and the result is deliberately not a live
`gx_catalog`. Public `gx_snapshot_catalog_view()` exposes that exact verified
M9h object as an offline, read-only, non-replayable inspection boundary.
Internal M9j establishes the next packaging prerequisite: exact catalog inputs
are self-contained, while fetched or harmonized inputs require the explicit
source catalog and must rebind its AOI and dataset identity to their embedded
fetch plan. The resulting package-input object retains native payloads and
harmonization evidence. Internal M9k converts that exact input into bounded,
path-sorted in-memory resources: canonical catalog/status/index CSVs, exact
retained provider bodies, canonical direct-CSV tables where no body was
retained, and harmonized observation/resource CSVs. M9k itself does not write
or publish. Internal M9l can now write those exact bytes and a deterministic
manifest through a verified sibling staging tree, atomically publish only to
an absent destination, and verify the exposed tree again. Its request ledger
is explicitly catalog-only and its result non-replayable; public package
creation is now available through `gx_package()` for catalog, fetched, and
harmonized inputs, with an explicit source catalog required for the latter two.
Public `gx_package_load()` now reads that fixed profile as exact bounded bytes
between complete closed-tree verifications. It remains read-only, unsigned,
and non-replayable and does not reconstruct live catalog, fetched, or
harmonized objects. For an opt-in M9ad package, loading rederives and preserves
its Frictionless compatibility evidence without invoking an external runtime.
Public `gx_package_tables()` parses those
verified in-memory CSV bytes only when they round-trip exactly through the
canonical quote-all UTF-8/LF profile; all columns remain character values and
native raw resources remain opaque. Internal M9p provides exact typed
package-owned projections, and public `gx_package_hydrate()` exposes them as a
redacted, read-only, non-reconstructing view under M9q. Internal M9r supplies
fixed-profile ownership admission and sibling-backup rollback; public M9s now
exposes that exact path through `gx_package(..., overwrite = TRUE)` and returns
evidence binding the prior and final verified generations. M9v integrates the
fixed Arrow/Parquet observation profile through bundle creation, manifests,
loading, table inspection, hydration, and replacement. M9w/M9x admit the
reviewed Quarto R and CLI runtime, and M9y now renders and verifies one isolated
internal HTML report. M9z integrates those bytes under a private staged and
replaceable package profile; M9aa exposes explicit public creation and offline
report access while authenticity, refresh, and replay remain deferred.
Internal M9t gives those optional dependencies an explicit host-
specific preflight. Internal M9u classifies Arrow against a reviewed 14.0.0
minimum and resolves its required exports only at the private serialization
boundary. The fixed redacted observation table is serialized and read back
entirely in memory; M9v exposes that profile only for harmonized packages. See
[ADR 0051](docs/decisions/0051-public-verified-package-creation.md) and
[ADR 0052](docs/decisions/0052-public-byte-preserving-package-loading.md) and
[ADR 0053](docs/decisions/0053-public-canonical-package-table-views.md), and
[ADR 0054](docs/decisions/0054-internal-typed-package-hydration.md), and
[ADR 0055](docs/decisions/0055-public-typed-package-hydration.md), and
[ADR 0056](docs/decisions/0056-internal-owned-package-replacement.md), and
[ADR 0057](docs/decisions/0057-public-verified-package-replacement.md), and
[ADR 0058](docs/decisions/0058-host-specific-optional-package-preflight.md),
and [ADR 0059](docs/decisions/0059-fixed-in-memory-arrow-parquet.md), and
[ADR 0060](docs/decisions/0060-public-verified-parquet-packages.md), and
[ADR 0061](docs/decisions/0061-reviewed-quarto-runtime-capability.md), and
[ADR 0062](docs/decisions/0062-reviewed-quarto-cli-admission.md), and
[ADR 0063](docs/decisions/0063-fixed-quarto-html-report.md), and
[ADR 0064](docs/decisions/0064-private-report-package-integration.md), and
[ADR 0065](docs/decisions/0065-public-verified-package-reports.md), and
[ADR 0066](docs/decisions/0066-public-offline-replay-inspection.md), and
[ADR 0067](docs/decisions/0067-internal-frictionless-data-package-profile.md),
[ADR 0068](docs/decisions/0068-public-all-csv-frictionless-packages.md), and
[ADR 0069](docs/decisions/0069-pinned-mixed-resource-frictionless-validation.md),
and [ADR 0070](docs/decisions/0070-public-mixed-resource-frictionless-packages.md),
which closes the fixed-package M9 roadmap. Publisher profile and sitemap
boundaries are defined by
[ADR 0071](docs/decisions/0071-versioned-publisher-profile.md).
The shared R and Python known-answer boundary is recorded in
[ADR 0072](docs/decisions/0072-shared-publisher-conformance.md).

The first crosswalk validates the reference service's advertised
`provider_id`, gage identity, and PID before returning a match. Repeated inputs
are queried once and expanded in order; no match receives an explicit sentinel
row, and multiple distinct matches are all returned as ambiguous. Advertised
COMIDs remain character values. Advertised mainstem URIs are retained without
an implicit service lookup. ADR 0075 selects `mainstems_v3` and dataset
vintage 3.0 for explicit currentness checks.

Query-bearing feature responses are intentionally non-cacheable, so filtered
offline replay and gage crosswalk lookup are not promised. The
`mainstems_v3` collection shares the persistent `/ref/mainstems/` PID
namespace. Superseded PIDs and every advertised replacement remain explicit;
the package never follows them automatically. VAA `levelpathi` values are not
Geoconnex mainstem identifiers, so the package does not construct mainstem
PIDs from them.

The optional NHDPlusV2 lookup is stored outside the expiring HTTP cache and is
addressed by its pinned v3.2 SHA-256 digest. Installation streams to a staging
file, validates each HTTPS redirect, exact size, digest, CSV schema, row count,
known answers, and a non-sensitive provenance receipt before atomic exposure.
Lookup inspection and the public vectorized forward and inverse mappers never
download, refresh, or repair data. Inverse matches are complete only within the
pinned mapping release, use deterministic COMID ordering, and explicitly do not
assert current service state. `gx_comid_to_mainstem(..., check = FALSE)` and
`gx_mainstem_to_comids(..., check = FALSE)` expose this release-only contract.
They return `currentness_policy = "not_checked"`. With `check = TRUE`, each
source match keeps its original PID and adds bounded live-v3 status, observation
provenance, and every advertised replacement. The inverse also checks a
requested PID that is absent from the selected mapping release.

`gx_huc12_to_mainstem(..., method = "outlet", check = FALSE)` retrieves one
validated HUC12 pour point from the USGS NLDI `huc12pp` source. It deduplicates
repeated HUC12 requests, returns explicit not-found rows, and prefers the
upstream mainstem PID. If NLDI supplies only a COMID, the function uses the
same explicitly installed pinned mapping. The `intersects` method remains
separate: it retrieves the reference HUC12 polygon and bounded `mainstems_v3`
bounding-box candidates, computes true intersections locally with S2, and
returns every match. Rows are ranked by disclosed currentness, outlet-HUC12,
intersection-length, drainage-area, and PID metrics without selecting one.

`gx_point_to_mainstem(points, check = FALSE)` accepts nonempty
two-dimensional `sf` or `sfc` Points with a declared CRS. It transforms them to
OGC CRS84 with PROJ networking disabled, deduplicates identical transformed
points, and retrieves containing COMIDs through the bounded NLDI position
route. Every COMID then passes through the explicitly installed pinned mapping.
An NLDI miss and a COMID absent from the mapping release remain distinct rows;
neither state triggers an implicit download. `check = TRUE` adds the same
bounded live currentness record to every matched PID.

`gx_mainstem(mainstem_uri)` performs the separate live currentness check
against `mainstems_v3`. It deduplicates transport while preserving input order,
reports current, superseded, and superseded-without-replacement states, and
retains every advertised replacement PID. It never follows or ranks a
replacement or falls back to legacy mainstem geometry. ADR 0084 composes this
contract into the checked COMID, HUC12 outlet, and Point crosswalks without
changing their source matches.

`gx_mainstem_to_gages(mainstem_uri)` queries the reference service's advertised
`mainstem_uri` property and returns every matching gage in deterministic PID
order. It deduplicates repeated inputs, returns a sentinel row for a complete
empty answer, and validates gage, provider, mainstem, and optional COMID
identity. The result records that live mainstem currentness was not checked.

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
