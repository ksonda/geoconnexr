# geoconnexr 0.0.0.9000

## Repository foundation

- Established the package and repository identity as `ksonda/geoconnexr` under
  the MIT license.
- Added cross-platform R CMD check, test coverage, and isolated weekly bounded
  live-smoke workflows.
- Added a responsive `pkgdown` documentation site with a task-focused homepage,
  curated function reference, getting-started and safety guides, searchable
  light/dark themes, and automatic deployment to GitHub Pages from `main`.
- Recorded CRAN and r-universe as the intended stable and development release
  channels, respectively.
- Documented the unresolved mainstem-vintage and SPARQL endpoint contract gates
  that must close before the 0.1.0 public API is frozen.
- Added offline-tested HUC/COMID validation, deterministic AOI recipes,
  versioned contract fingerprints, and configurable service endpoints.
- Added the offline M6a custom-AOI boundary. `gx_aoi()` now accepts exactly one
  XY polygon or multipolygon with an explicit CRS through `type = "sf"` or
  `"auto"`, transforms it to CRS84 with PROJ networking disabled, rounds to a
  nine-decimal-degree grid, applies GeoJSON ring winding, canonicalizes ring and
  member ordering, and records canonical GeoJSON plus a portable little-endian
  WKB SHA-256. Invalid, empty, non-finite, out-of-bounds, grid-collapsed,
  over-100,000-coordinate, or over-8-MiB representations fail closed without
  repair or upstream work; antimeridian rings require explicit pre-cut geometry.
  Recipe pipeline fields remain intended replay metadata; public `gx_catalog()`
  is still gated.
- Added the unexported M6b AOI hydration boundary for decoded recipes and
  literal JSON text/bytes. It applies strict UTF-8, duplicate-member,
  structure, depth, member, coordinate, and byte limits; reconstructs
  identifiers or CRS84 polygonal geometry; and requires regenerated canonical
  GeoJSON and portable WKB SHA-256 identity to agree. It never opens a supplied
  path or URL and does not authorize catalog or full replay execution.
- Added the unexported M9a offline snapshot verifier. It strictly validates a
  bounded `manifest-v1` document, rebinds AOI identity through M6b, inventories
  a closed tree of portable regular-file resources with lstat-aware type and
  symlink checks, verifies each exact size and SHA-256 value in a deterministic
  pass, and repeats filesystem metadata checks afterward. Optional absence is
  visible; present corruption,
  undeclared entries, special files, and path aliases fail closed. Embedded
  requests are shape-validated only, and the unsigned manifest establishes
  internal consistency rather than authenticity or historical provenance.
- Added the unexported M6c catalog value-object boundary. It validates exact
  typed CRS84 point sites, flattened datasets, recoverable problems,
  manifest-shaped request attempts, and reconciled procedural completeness
  under fixed budgets. Export views redact credentials and query/fragment
  values for every URI scheme while retaining stable site/variable identity
  fingerprints; early aggregate accounting and bounded count arithmetic close
  resource and overflow escapes. Nonempty reference layers, live
  discovery/merge policy, and public `gx_catalog()` remain gated.
- Added the unexported M7a deterministic selection-only fetch plan. A strict
  dual-asset loader binds portable classifiers to R implementation metadata;
  plans collapse catalog variables into one row per distribution plus ordered
  parameter rows, apply offline URL safety, time intersection, stable ordering,
  and count/byte budgets, and remain explicitly non-executable. Request lists
  are empty, handler implementations are planned and non-replayable, and the
  boundary performs no package probing, DNS, network, cache, or file writes.
- Added the unexported M7b host package-capability preflight. It revalidates and
  preserves the M7a plan, probes only selected handlers' unique allowlisted
  packages through bounded direct `DESCRIPTION` reads without loading
  namespaces or deserializing `Meta/package.rds`, and reports missing, too-old,
  satisfying, or unpinned requirements as host-specific advisory state. Even
  satisfied requirements remain blocked on planned implementations; requests
  stay empty and the report is non-replayable and never execution-ready.
- Added the unexported M7c `gx_csv_get_intents` S3 contract 0.1.0. It preserves
  the M7a plan, records an exact shared inert policy, emits one intent for each
  selected CSV distribution in global fetch order, and retains exact coverage
  for every distribution. Intent hashes use `gx_contract_hash()` under the
  `geoconnexr.csv-get-intent.v1` namespace to bind the re-derived full canonical
  target, declared media type, and every policy field, while intent rows store
  only redacted URLs. The boundary allocates no request, byte, row, or column
  budget; grants no transport or replay authority; does not depend on M7b or
  `readr`; and adds no public API, schema, or execution path.
- Added the unexported M7d `gx_csv_request_plan` S3 contract 0.1.0. It preserves
  M7c and its nested request-empty M7a plan byte-for-byte, requires explicit
  per-response byte, row, and column ceilings, and allocates M7a's physical
  attempt, encoded-byte, and decoded-byte budgets across every selected
  distribution in global fetch order. Fair quotient/remainder shares and held
  non-CSV reservations prevent direct CSV from stealing global capacity. Only
  reserved CSV intents receive domain-separated, redacted, non-executable
  logical request plans. The boundary binds zero redirects and retries, one
  possible physical attempt, cache bypass, status/media/encoding response
  constraints, and shape limits, but implements no DNS, transport, response
  validator, result schema, parser semantics, attempt ledger, timeout policy,
  serialization, replay, or public API.
- Added the unexported M7e `gx_csv_validated_response` S3 contract 0.1.0. It
  preserves M7d byte-for-byte and validates one exact caller-supplied in-memory
  response candidate against one direct-CSV logical request. Status 200,
  singleton bounded critical headers, admitted CSV media, identity encoding,
  optional exact Content-Length, exact canonical no-redirect target, and all
  three response-byte ceilings fail closed. The object retains the exact
  bounded raw body and its SHA-256 while discarding arbitrary headers and the
  full URL. Validation identity is domain-separated and query-bound, but
  metadata explicitly says that no provider response was observed, no budget
  was consumed, no parser ran, and transport, execution, replay, runtime
  preflight, attempt/ledger alignment, result semantics, and serialization
  remain blocked.
- Added the unexported M7f `gx_csv_parsed_response` S3 contract 0.1.0. It
  preserves M7e and the complete nested plan chain byte-for-byte, requires an
  explicit total-field ceiling, and parses only M7e's exact retained raw body
  under one strict UTF-8 comma/header profile. A preallocation scan rejects
  invalid encoding, controls, misplaced BOMs, blank or embedded records,
  malformed quotes, ragged width, empty/duplicate headers, and every byte,
  row, column, field, and header limit before creating the exact all-character
  schema and table. Chunked domain-separated hashes bind exact names, values,
  dimensions, policy, limits, BOM presence, and M7e body/validation identity.
  Metadata records parser/result validation while retaining caller-supplied
  origin and denying provider observation, attempt/ledger provenance, fetch-
  budget consumption, transport, execution, replay, and serialization. M7f
  loads no optional parser package and exports no parser or result API.
- Added the unexported M7g `gx_csv_execution` S3 contract 0.1.0. It executes
  one selected direct-CSV logical request through the package-owned
  DNS-revalidated, public-address-pinned, redirect-disabled streaming
  transport with explicit timeout and per-host interval policy, no cache or
  retry, and the minimum selected response-byte ceiling. M7e validates the
  provider response before M7f parses the same exact body; M7g preserves that
  nested chain and adds one scope-bound execution identity plus one charged,
  redacted physical-attempt ledger row. Outer metadata now records provider
  observation and budget consumption while keeping replay, multi-request
  orchestration, non-CSV handlers, registration, serialization, and public
  `gx_fetch()` gated.
- Added the unexported M7h `gx_csv_orchestration` S3 contract 0.1.0. It admits
  direct-CSV logical requests in deterministic global order under explicit
  count and 64-MiB aggregate reserved-response ceilings, then runs M7g
  sequentially with continue-on-error semantics. A strict dry run computes the
  same admission and one-row-per-distribution status projection without DNS,
  transport, clocks, throttling, cache, or writes. Live transport and parse
  failures become bounded redacted terminal rows while unrelated requests
  continue. Successful M7g values are validated and compacted into exact
  execution, attempt, validation, parser, schema, character-data, and identity
  facts without retaining raw bodies or repeating the complete M7d-to-M7a
  chain. Whole-object validation rebinds child scopes, identities, results,
  statuses, indexes, counts, budgets, and authority metadata. Non-CSV handlers,
  runtime invocation preflight, registration, serialization/replay, and public
  `gx_fetch()` remain gated.
- Added the unexported M7i `gx_oaf_request_plan` and `gx_oaf_execution` S3
  contracts 0.1.0 plus the internal native `gx_handler_oaf` implementation.
  One selected query-free OGC API Features items URL is rebound to its exact
  M7d held reservation, planned with only fixed `f=json` and `limit`
  parameters, and executed through one DNS-pinned, redirect/retry/cache-free
  physical attempt. The implementation symbol is resolved immediately before
  invocation; a missing symbol fails before DNS or transport. Strict bounded
  GeoJSON parsing returns `sf`, records explicit single-page truncation, and
  binds retained response bytes, result rows, execution, and attempt evidence.
  Fixture snapshots, poisoned-redirect, over-limit-page, missing-symbol, and
  forged-object tests keep provider filters, pagination, cross-handler
  orchestration, registration, serialization/replay, and public `gx_fetch()`
  gated.
- Added the unexported M7j `gx_fetch_orchestration` S3 contract 0.1.0. It
  derives direct-CSV and compatible OGC API Features candidates from one M7d
  plan, admits them together under shared count and 64-MiB reserved-response
  ceilings, and executes them sequentially in exact global fetch order. Typed
  CSV and OGC capability/transport/parse failures become handler-specific
  redacted terminal rows while unrelated work continues. A strict dry run
  performs the same offline planning, admission, identity, and exact
  one-row-per-distribution status projection without host or provider work.
  Successful CSV evidence uses the M7h compact contract; OGC evidence removes
  the repeated plan chain while retaining bounded GeoJSON bytes so validation
  can rebuild M7i, reparse exact `sf`, and rebind scopes, attempts, statuses,
  result indexes, counts, bytes, and authority. Remaining handlers,
  pagination, registration, serialization/replay, a public fetched-result
  schema, and `gx_fetch()` remain gated.
- Added the unexported M7k `gx_wqp_request_plan` and `gx_wqp_execution` S3
  contracts 0.1.0 and upgraded `gx_fetch_orchestration` to 0.2.0. One selected
  WQP Result URL now binds its exact held M7d reservation, fixed Result/
  `narrowResult` service profile, site, optional characteristic, UTC date
  interval, shape limits, and one-attempt byte ceiling. geoconnexr performs the
  sole DNS-pinned, identity-encoded, no-cache/no-redirect/no-retry request,
  resolves `dataRetrieval::importWQP()` immediately before transport, invokes
  it only on the retained response bytes with type conversion disabled, and
  requires its character table to equal the independently strict CSV result.
  The shared scheduler now runs CSV, WQP, and OGC in global order, isolates WQP
  capability/transport/parse failures, compacts successful WQP evidence, and
  revalidates it without a later optional-package dependency. Pagination,
  remaining handlers, registration, serialization/replay, a public result
  schema, and `gx_fetch()` remain gated.
- Added the unexported M7l `gx_edr_request_plan` and `gx_edr_execution` S3
  contracts 0.1.0 and upgraded `gx_fetch_orchestration` to 0.3.0. One selected
  EDR collection `position` URL now binds its held M7d reservation, exact CRS84
  WKT point, parameter, UTC interval, CoverageJSON representation, shape limits,
  and one-attempt byte ceiling. Invocation verifies `edr4r >= 0.1.1` plus
  `edr_position()` and `covjson_to_tibble()` before provider work; geoconnexr
  owns the DNS-pinned, cache/redirect/retry-free request and requires the
  offline external normalizer to exactly match a strict bounded PointSeries
  table. The shared scheduler now runs CSV, WQP, EDR, and OGC globally, isolates
  EDR capability/transport/parse failures, and revalidates compact successes
  without a later optional-package dependency. Other EDR query types, USGS
  execution, pagination, registration, serialization/replay, a public result
  schema, and `gx_fetch()` remain gated.
- Added the unexported M7m `gx_usgs_continuous_request_plan` and
  `gx_usgs_continuous_execution` S3 contracts 0.1.0 and upgraded
  `gx_fetch_orchestration` to 0.4.0. One current USGS Water Data API
  `continuous` items URL now binds its held M7d reservation, exact site,
  five-digit parameter, UTC interval, fixed property set, single-page limit,
  and one-attempt byte ceiling. Invocation verifies
  `dataRetrieval >= 2.7.22` and exported
  `read_waterdata_continuous()` capability before provider work; geoconnexr
  owns the DNS-pinned, identity-encoded, cache/redirect/retry-free request and
  strict GeoJSON parser. Measurement values remain strings and an advertised
  next page becomes explicit truncation without another request. The scheduler
  now runs CSV, WQP, EDR, USGS continuous, and OGC globally, isolates USGS
  capability/transport/parse failures, and revalidates compact successes
  without loading dataRetrieval later. Current daily and legacy USGS execution,
  pagination, registration, serialization/replay, a public result schema, and
  `gx_fetch()` remain gated.
- Added the unexported M9b catalog-only snapshot writer. It revalidates a
  catalog, creates deterministic redacted UTF-8 CSV views in a sibling staging
  tree, writes a manifest-v1 document last, verifies the closed tree through
  M9a, and publishes only to a new destination. Path-bearing filesystem
  warnings are suppressed behind typed errors, and owned-stage cleanup is
  checked. It does not claim Frictionless
  compatibility, authenticity, replayability, loading semantics, or overwrite
  ownership.
- Added typed SPARQL template discovery and local rendering with injection
  guards, slice/query byte budgets, and the correct GeoSPARQL function
  namespace.
- Added the unexported experimental M5a graph substrate for one logical POST of
  trusted read-only SELECT/ASK text, strict bounded SPARQL 1.1 Results JSON
  shape and term-kind parsing, normalized sparse RDF-term bindings with
  per-result blank-node scope, semantic cache admission, redacted request and
  physical-attempt provenance, and explicit transport/parser budgets. Public
  `gx_sparql()`, endpoint support, and named pagination remain gated by ADR 0004.
- Replaced the aspirational query-manifest v1 with a fail-closed render-only v2
  contract. It pins every `.rq` file by exact bytes and SHA-256, validates the
  SELECT projection, terminal slice slots, and declared `ORDER BY`, exposes
  required result variables and honest key/order stability facts, and records
  explicit blockers with execution, chunking, and pagination disabled. HTTP
  IRI lists now reject duplicates and render in bytewise order; CRS84 AOIs must
  be finite, valid, closed polygonal WKT. Literal and canonical UTC datetime
  encoders are available for future reviewed templates without raw SPARQL.
- Added portable distribution classification with overlap tests, reviewed
  affine unit-conversion rules, four JSON Schemas, and installed-asset contract
  tests.
- Added three explicitly gated live semantic checks for PID redirects, bounded
  graph discovery, and reference-gage JSON-LD negotiation.
- Added the first runtime protocol slice: bounded endpoint clients,
  representation-specific HTTP caching and offline misses, public-target and
  redirect safety checks, and vectorized PID resolution with recorded redirect
  chains and `HEAD`-to-`GET` fallback.
- Added a fail-closed HTTP safety policy with DNS-to-connection pinning,
  identity-only streamed bodies, fixed cache freshness, credential-aware cache
  exclusion, and package-owned cache clearing.
- Replaced opaque transport-library retries with a package-owned physical-
  attempt loop. Transient status and transport retries now revalidate DNS,
  honor bounded `Retry-After`, expose redacted attempt metadata, cache only the
  terminal eligible response, and consume JSON-LD, reference, and crosswalk
  request/byte budgets deterministically.
- Added a package-owned per-host throttle with a monotonic clock. Physical
  retries, redirects, ordinary requests, and explicit file-download hops share
  hostname reservations across clients, while cache/offline paths remain free
  of artificial waits. Bounded concurrent scheduling remains a separate M1
  task because shared workflow budgets require atomic reservations.
- Added `gx_jsonld()` with bounded PID resolution, JSON-LD negotiation, raw-byte
  HTML embedded/alternate discovery, a hash-pinned Schema.org context, unknown
  remote-context rejection, redacted request ledgers, and offline cache replay.
- Added tolerant `gx_parse_location()` and `gx_parse_datasets()` tables for
  compact, expanded, aliased, and current reference-gage profiles, including
  structured strict-mode diagnostics, open temporal intervals, deterministic
  dataset/distribution/variable IDs, and amplification budgets.
- Added a provenance- and hash-pinned JSON-LD fixture manifest. Six observed
  profiles from four landing hosts and five semantic providers now close the
  five-real-pages/three-providers P0 evidence gate with one-page margin.
- Added tolerant parsing for sparse state-service profiles that expose a
  generic `schema:Place` plus GeoSPARQL geometry, with an explicit diagnostic
  instead of inferring semantics from unmapped source properties.
- Hardened cache privacy and parser amplification boundaries: query-bearing and
  private responses are not persisted, URL metadata is fail-closed redacted,
  and atomic members plus bundled-context replacement costs are preflighted.
- Added the native M3 OGC API Features reference client:
  `gx_ref_collections()`, `gx_ref_queryables()`, `gx_ref_features()`, and
  `gx_ref_feature()` return versioned, typed tables with request provenance.
- Added queryable-validated equality filters, explicit unbounded-retrieval
  opt-in, identifier normalization, same-endpoint pagination, deterministic
  truncation diagnostics, and cumulative page/byte budgets.
- Added identity-checked single-feature fallback from the item route to the
  advertised `x-ogc-role: id` filter and then bounded JSON-LD negotiation.
  Hash-pinned collection/queryable schemas cover gages, both mainstem
  collections, HUC12s, and counties; checked evidence records the legacy
  large-item failure, while deterministic protocol tests cover pagination and
  empty results. Cross-vintage and full large-geometry evidence remain open.
- Added the first experimental M4 crosswalk, `gx_gage_to_pid()`, with validated
  gage queryables, exact provider/feature/PID identity checks, explicit
  not-found and ambiguous rows, character identifier columns, per-row and
  aggregate diagnostics, and batch-wide input/match/row/request/byte ceilings.
- Added hash-pinned minimized gage crosswalk evidence for the checked
  `USGS-08332622` to reference-gage `1000001` mapping and an opt-in bounded live
  assertion.
- Corrected the M4 architecture after checked evidence showed that NHDPlus VAA
  `levelpathi` values are not Geoconnex mainstem identifiers. COMID/HUC/point
  and inverse-mainstem APIs now remain gated on a versioned mapping asset and
  mainstem-vintage policy rather than constructing false PIDs.
- Preserved feature/property identity alignment when GDAL reorders GeoJSON
  features by top-level feature ID.
- Added the M4b optional-data lifecycle: `gx_mainstem_lookup_install()` makes
  the only disclosed transfer or air-gapped import of the 120,422,425-byte
  `ref_rivers` v3.2 COMID lookup, while `gx_mainstem_lookup_info()` verifies an
  installation without downloading, refreshing, or repairing it.
- Added an immutable runtime lookup registry and non-sensitive receipt that pin
  the upstream tag commit, asset ID, exact schema, 2,357,730 rows, SHA-256,
  zero-or-one forward cardinality, known answers, and CC0-1.0 provenance.
- Added a disk-streaming HTTP boundary with manual HTTPS redirect validation,
  DNS pinning, identity encoding, exact byte ceilings, same-directory staging,
  and atomic replacement outside the expiring in-memory HTTP cache.
- Added an internal vectorized COMID mapping substrate with character-only
  identifiers, explicit not-found/ambiguity semantics, offline reuse, bounded
  chunk scanning, tamper detection, and explicit unchecked-currentness
  diagnostics. The public COMID API remains gated by ADR 0004.
- Added an internal release-scoped mainstem-to-COMID inverse over the same
  verified local bytes. It returns zero-to-many release members in deterministic
  order, expands duplicate inputs under aggregate match and row ceilings, and
  distinguishes absence in v3.2 from current service state. The public inverse
  remains gated by ADRs 0004, 0008, and 0015.

Public APIs and serialized contracts remain experimental during the P0
architecture spike.
