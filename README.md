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
destination. Internal M7a adds deterministic, selection-only fetch plans bound
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
charged attempt. M7j introduced shared global scheduling. M7k now schedules
all three implemented paths in one global order, applies shared count and byte
admission limits, isolates handler failures, and reconciles one exact terminal
status for every distribution. The WQP slice records Result/`narrowResult`,
site, optional characteristic, and UTC time facts; performs one package-owned
bounded request; and requires invocation-time `dataRetrieval::importWQP()`
output to match the strict internal CSV parser.

These are internal substrates, not exported discovery, fetch, package,
snapshot, loading, or replay APIs. Public graph
discovery, catalog and fetch APIs, remaining handlers, pagination, and general
provider data retrieval remain gated on fixture-backed production evidence.

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

The internal M7k `gx_fetch_orchestration` S3 object implements contract 0.2.0.
It derives direct-CSV, compatible WQP Result, and OGC API Features candidates
from one M7d plan, keeps their original global fetch order, and admits all three
handlers under one explicit request-count and aggregate reserved-response
budget. Live work is sequential and continues after typed CSV, WQP, or OGC
failures; every M7d coverage row receives one exact terminal status. A missing
OGC symbol or WQP parser charges no physical attempt, while transport and parse
failures retain only bounded redacted evidence. Dry run performs the same
planning and status projection without DNS, transport, clocks, throttling,
cache, writes, or symbol resolution. CSV successes reuse the M7h compact
contract. WQP successes retain the bounded CSV body, exact character table and
schema, parse hashes, and attempt facts so strict validation does not require
the optional package later. OGC successes retain their bounded GeoJSON body so
validation can rebuild M7i and reparse the exact `sf` result. M7k remains
unexported and does not implement the remaining provider handlers, pagination,
registration, serialization/replay, a public fetched-result schema, or
`gx_fetch()`. See
[ADR 0029](docs/decisions/0029-cross-handler-orchestration.md) and
[ADR 0030](docs/decisions/0030-single-response-wqp-handler.md).

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
public and multi-handler fetch orchestration remains gated.
The internal M9b
writer is limited to validated catalog-only resources and is labeled
non-replayable in its manifest.

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
