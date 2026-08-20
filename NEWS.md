# geoconnexr 0.0.0.9000

## Repository foundation

- Added the internal M1 curl multi transport under ADR 0089. Central scheduler
  reservations now drive public-address-pinned concurrent handles with hard
  streaming ceilings, completion-order-independent results, single-flight
  cache reuse, offline cache admission, and exact aggregate budget deferral.
  Public `gx_fetch()` remains sequential until the handler orchestration slice.
- Added the internal M1 central scheduler under ADR 0088. It reserves total and
  per-host permits plus physical-attempt request and byte budgets before
  dispatch, coalesces compatible identical cache keys, releases unused byte
  reservations on completion, and collects results in source order even when
  work completes out of order. Curl multi and public fetch integration remain
  separate slices.
- Replaced the provisional phase ranges with the measured remaining-delivery
  baseline in ADR 0087. The plan now separates focused implementation days,
  observed CI latency, additive provider-family work, and the upstream-blocked
  graph query.
- Closed the remaining product-scope decision under ADR 0086. Water rights and
  other administrative layers stay outside the supported 0.x core until a
  named source completes a separate authority, licence, identity, temporal,
  spatial, and legal-semantics review.
- Recorded the automatic HUC10 graph gate under ADR 0085. A bound-site
  `geof:sfIntersects` control succeeds, while unbound one-row HUC10 searches
  still exceed the transport timeout after type, bounding-box, and operand-order
  probes. The package does not substitute reference gages as a complete graph
  site catalog.
- Completed M4 currentness composition under ADR 0084. The public COMID,
  inverse COMID, HUC12 outlet, and Point crosswalks now accept `check = TRUE`,
  retain the original match, preserve every live replacement, and merge the
  bounded request evidence. No replacement is followed or selected.
- Added public `gx_huc12_to_mainstem(..., method = "intersects")` under ADR
  0083. It validates reference HUC12 and `mainstems_v3` contracts, rejects
  incomplete bounding-box candidate sets, computes true intersections locally
  with S2, and returns every ranked match with currentness and replacements.
  It never selects the first row.
- Selected the HUC12 intersection ranking under ADR 0082. The bounded reference
  probe reduces 41 bounding-box candidates to 17 local S2 intersections and
  ranks every retained result by currentness, exact outlet-HUC12 agreement,
  intersection length, drainage area, and PID. Ranking never selects one
  mainstem.
- Added public `gx_mainstem_to_gages()` under ADR 0081. It uses the reference
  service's advertised `mainstem_uri` filter, returns every matching gage in
  deterministic order, preserves complete empty answers, and validates all
  repeated identities. It does not require the optional COMID mapping and does
  not imply that the requested mainstem is current.
- Added public `gx_mainstem()` under ADR 0080. It performs bounded live
  `mainstems_v3` currentness checks, deduplicates repeated PIDs, preserves every
  replacement in input order, and keeps unresolved supersession visible. It
  never follows, ranks, or selects a replacement and never falls back to the
  legacy collection.
- Added public `gx_point_to_mainstem()` under ADR 0079. It accepts declared-CRS
  two-dimensional Points, disables PROJ networking while transforming to OGC
  CRS84, deduplicates NLDI position requests, validates repeated COMID identity,
  and resolves only through the installed pinned mapping. NLDI and mapping
  not-found states remain distinct, and live mainstem currentness is unchecked.
- Selected the USGS NLDI `comid/position` route as the point-to-COMID
  provenance boundary under ADR 0078. Bounded live evidence confirms that the
  route returns NHDPlusV2 COMID identity rather than a Geoconnex mainstem, so
  point implementation must compose with the pinned mapping and keep
  currentness unchecked.
- Added public `gx_huc12_to_mainstem(..., method = "outlet")` under ADR 0077.
  It performs bounded, deduplicated USGS NLDI `huc12pp` lookups, validates the
  HUC12 outlet identity and Point geometry, prefers the advertised mainstem,
  and uses the installed pinned COMID mapping only when needed. Not-found rows
  and release-only currentness semantics stay explicit.
- Exported release-scoped `gx_comid_to_mainstem()` and
  `gx_mainstem_to_comids()` under ADR 0076. Both operate only on an explicitly
  installed checksum-pinned lookup and keep
  `currentness_policy = "not_checked"` unless the caller requests the separate
  bounded live-v3 composition.
- Selected `mainstems_v3` and dataset vintage 3.0 as the default mainstem
  representation under ADR 0075 while preserving the shared
  `/ref/mainstems/` PID namespace. The v3 JSON-LD fallback now accepts that
  exact shared identity. Superseded identifiers and all advertised
  replacements remain explicit and are never followed automatically.
- Added an installed HUC10 case study that runs catalog, bounded current-USGS
  daily fetch, harmonization, Frictionless packaging, closed-tree verification,
  typed hydration, and offline stored-state inspection. The live guide uses a
  caller-supplied profile and states that AOI membership is not independently
  rechecked. Its verified retained-response package provides a network-free
  demonstration and regression fixture.
- Fixed exact UTC microsecond serialization for execution ledgers and package
  catalog views. Fractional instants no longer lose one microsecond when R
  converts between decimal seconds and `POSIXct`, which had caused valid live
  USGS, WQP, EDR, and OGC Features executions or package hydration to fail their
  identity rebound checks intermittently.
- Added the P4 shared publisher conformance suite under ADR 0072. One installed
  synthetic corpus now pins a language-neutral input, deterministic JSON-LD,
  exact validation findings, and sitemap bytes. R rebuilds every known answer,
  and a dependency-free Python harness independently reproduces the profile,
  findings, canonical digest, and sitemap. Dedicated CI runs the harness on
  Python 3.9 and 3.13 without network access or third-party packages.
- Completed M10 publisher tools under ADR 0071. `gx_context()` returns the
  fixed local publisher context, `gx_jsonld_build()` emits deterministic
  publisher-profile 1.0.0 JSON-LD from exact catalog site and dataset tables,
  and `gx_jsonld_validate()` reports local profile findings with severity,
  JSON pointer, rule ID, profile version, and suggested fix. `gx_sitemap()`
  writes one deterministic, bounded XML sitemap through verified
  absent-destination staging. These APIs do not fetch contexts, publish
  profiles remotely, notify search engines, or overwrite existing output.
- Completed the fixed-package M9 roadmap through M9af under ADR 0070. Public
  `gx_package(..., frictionless = TRUE)` now composes with retained provider
  bodies, fixed Arrow/Parquet observations, and verified report HTML. The
  descriptor remains manifest-bound and is rederived during offline loading;
  canonical CSVs retain exact string schemas while non-CSV resources use the
  pinned opaque-file profile. Public loading, table inspection, typed
  hydration, report access, replacement, and stored-state inspection preserve
  the combined evidence without invoking Python or executing a recipe. Live
  refresh and procedural replay are explicitly deferred under ADR 0066 until
  a complete reproducible request recipe exists; they are not another
  fixed-package M9 gate.
- Added the internal M9ae mixed-resource Frictionless validation gate under
  ADR 0069. Non-canonical resources now use a deterministic `format = "bin"`
  Data Resource transport profile while retaining their true extension and
  `opaque-file-v1` marker in custom `geoconnexr` metadata. This keeps exact
  path, byte-length, SHA-256, and media-type validation without dispatching
  semantic Python parsers. The pinned core Frictionless 5.19.0 CI job now
  validates real retained-raw, Arrow/Parquet, and verified-report bundles with
  zero errors or warnings and requires every opaque resource to remain a file
  task. M9af now supplies the public composition boundary.
- Added M9ad public all-CSV Frictionless package publication under ADR 0068.
  Explicit `gx_package(..., frictionless = TRUE)` now adds one deterministic
  manifest-declared `datapackage.json` derived from the finalized base bundle,
  carries its exact identity through creation and owned replacement, and
  rederives the descriptor from verified bytes during offline loading. Table,
  hydration, and stored-state replay evidence preserve the compatibility claim
  while leaving JSON opaque and procedural replay disabled. Default manifests
  remain descriptor-free; raw, Parquet, report, and mixed-resource combinations
  remain gated. The pinned Frictionless 5.19.0 CI job now validates a publicly
  generated package in standards-v1 mode.
- Added the internal M9ac Frictionless Data Package v1 profile under ADR 0067.
  Exact M9k/M9z resource bundles now produce a deterministic bounded
  `datapackage.json` description in memory: canonical CSV resources carry
  exact all-string Table Schemas, non-CSV bytes are declared explicitly as
  generic resources, and every resource remains bound to its path, size, and
  SHA-256 digest. Construction performs no write, publication, CLI, refresh,
  or replay work and remains internal. A dedicated CI gate pins Python
  Frictionless CLI 5.19.0 and validates the catalog, fetched, and harmonized
  CSV package stages in standards-v1 mode without errors or warnings.
- Added M9ab public offline stored-state inspection under ADR 0066.
  `gx_replay(..., refresh = FALSE)` now accepts only fixed catalog snapshots
  and fixed public packages, reuses their closed-tree verification and exact
  typed loaders, and returns evidence binding the current manifest, typed
  view, optional stored report, explicit limitations, and deterministic
  inspection identity. It performs no recipe execution, network, cache,
  optional runtime, write, destination publication, authenticity check, or
  Frictionless interpretation. `refresh = TRUE`, destinations, extra options,
  arbitrary manifest-v1 trees, mutations, and forged evidence fail closed.
- Added M9aa public verified package reports under ADR 0065. Explicit
  `gx_package(..., report = TRUE)` now builds one fixed execution-disabled,
  cache-disabled HTML report from an owned typed package view, binds its exact
  bytes through the M9z profile, and removes the private source after staged or
  replacement verification. Report packages now load, parse tables, and
  hydrate through the public offline inspection APIs while HTML remains opaque
  exact bytes. New `gx_report()` revalidates and returns those bytes without
  Quarto and can atomically copy them to one absent output file. Verified
  replacement supports report-free, report-bearing, and report-removal
  transitions; report-free defaults and manifests remain unchanged.
- Added the internal M9z private report-package profile under ADR 0064. Exact
  M9y evidence can now add one byte-identical `report/index.html` resource only
  to the M9k bundle from which its typed source package originated. The private
  manifest binds base-bundle, hydration, source-manifest, source, HTML, Quarto
  R, and CLI identities and passes the existing staged closed-tree creation and
  owned replacement workflow, including repeated replacement of an intact
  report package. Report-free manifests remain unchanged; M9aa now exposes the
  reviewed public creation, loading, and access boundary.
- Added the internal M9y fixed Quarto HTML report boundary under ADR 0063. One
  exact typed package view now derives a bounded code-free `report.qmd`, then
  renders only through the M9x-admitted CLI path with execution and caching
  disabled under a 30-second timeout. The private render tree must contain only
  the unchanged source and one bounded minimal `report.html`; HTML verification
  rejects scripts, active embedded objects, refreshes, and external links and
  binds all redacted summary counts. Exact source/output bytes and runtime
  evidence are retained in memory after the private tree is removed. M9z now
  supplies private package integration, and M9aa now supplies public report
  creation and access.
- Added the internal M9x Quarto CLI admission boundary under ADR 0062. After
  validating the M9w Quarto R capability, the boundary accepts only the
  normalized executable returned by `quarto_path(normalize = TRUE)`, runs that
  exact path only with `--version` under a five-second timeout, detects file
  metadata races, and requires Quarto CLI 1.8.27 or newer. The resulting
  host-specific evidence authorizes version admission only: no report is
  inspected or rendered, no file is written, and no public API is added. The
  repository host's Quarto CLI 1.3.433 remains correctly blocked; M9y now
  supplies the separate fixed report contract.
- Added the internal M9w Quarto R capability boundary under ADR 0061. Quarto
  R 1.5.1 is now the reviewed minimum in package metadata and the advisory M9t
  preflight. Operation-time resolution rejects missing or old metadata before
  namespace loading, detects metadata/namespace races, and requires the
  reviewed `quarto_render()`, `quarto_path()`, `quarto_version()`, and
  `quarto_available()` exports and formals. Resolution invokes none of those
  functions, does not locate or execute the Quarto CLI, renders nothing, and
  remains internal; M9x now supplies the separate CLI admission boundary.
- Added the public M9v Arrow/Parquet package path. Harmonized inputs now accept
  `gx_package(..., timeseries = "parquet")` and publish one fixed redacted
  typed `data/observations.parquet` resource through the existing staged
  closed-tree verification and owned replacement guarantees. Manifests bind
  the Arrow writer version and reviewed Parquet-2.4 profile. Byte-preserving
  loading recognizes Parquet without loading Arrow, canonical table views
  leave it opaque, and `gx_package_hydrate()` reads the verified bytes in
  memory through the reviewed Arrow capability and requires the exact typed
  observation schema. Catalog/fetched Parquet requests fail before
  publication; CSV remains the default; reports, authenticity, refresh, and
  replay remain deferred.
- Established the package and repository identity as `ksonda/geoconnexr` under
  the MIT license.
- Added cross-platform R CMD check, test coverage, and isolated weekly bounded
  live-smoke workflows.
- Added a responsive `pkgdown` documentation site with a task-focused homepage,
  curated function reference, getting-started and safety guides, searchable
  light/dark themes, and automatic deployment to GitHub Pages from `main`.
- Exported bounded `gx_catalog()` construction from one spatial graph page,
  explicit PID profiles, or named caller-supplied local JSON-LD. The catalog
  preserves typed sites, flattened distributions, diagnostics, request
  evidence, and count-reconciled completeness; explicit inputs record that AOI
  membership or provider provenance was not independently established.
- Updated live WQP compatibility for current `/wqx3/Result/search` profile
  URLs while executing the stable narrow `/data/Result/search` CSV route, and
  fixed `dataRetrieval::importWQP()` agreement without a second provider
  request. Updated EDR compatibility for `edr4r` 0.1.1, pygeoapi's
  CoverageJSON media type, full CRS84 URI, timezone-less UTC timestamps, and
  valid PointSeries `t,y,x` ranges.
- Added a complete catalog → plan → fetch guide with verified WQP and EDR
  examples and explicit upstream graph/profile limitations.
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
  Recipe pipeline fields remain intended replay metadata; catalog execution is
  a separate public boundary.
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
  resource and overflow escapes. Nonempty reference layers and general
  graph/profile merge policy remain gated.
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
- Added the unexported M7n `gx_usgs_daily_request_plan` and
  `gx_usgs_daily_execution` S3 contracts 0.1.0 and upgraded
  `gx_fetch_orchestration` to 0.5.0. One current USGS Water Data API `daily`
  items URL now binds its held M7d reservation, exact site, five-digit
  parameter and statistic codes, closed local-date interval, fixed property
  set, single-page limit, and one-attempt byte ceiling. Invocation verifies
  `dataRetrieval >= 2.7.22` and exported `read_waterdata_daily()` capability
  before provider work; geoconnexr owns the DNS-pinned,
  identity-encoded, cache/redirect/retry-free request and strict GeoJSON
  parser. Measurement values remain strings, observation time remains a
  `Date`, last-modified time becomes UTC `POSIXct`, and an absent
  `numberMatched` remains unknown. The six-family scheduler isolates daily
  capability/transport/parse failures, never follows the next page, and
  revalidates compact successes without loading dataRetrieval later. Latest
  and legacy USGS execution, pagination, registration, serialization/replay,
  a public result schema, and `gx_fetch()` remained gated at that checkpoint.
- Closed M7 at the reviewed six-family supported subset under ADR 0034 instead
  of extending the milestone through every provider variant. Public
  `gx_fetch_plan()` now exposes deterministic built-in-registry selection and
  public `gx_fetch()` composes the existing bounded request pipeline into a
  validated `gx_fetched` 0.1.0 object. Its exact status table retains one row
  per distribution; its result table exposes handler-native tabular or `sf`
  payloads and bounded raw bodies where retained; and its provenance embeds
  the fully revalidated M7n orchestration. Execution remains sequential,
  single-page, retry/redirect/cache-free, and failure-isolating under fixed
  count, byte, shape, timeout, and per-host limits. Latest/legacy USGS, other
  EDR queries, pagination, registration, serialization, and replay are now
  explicit later enhancements rather than M8 gates.
- Began M8 with public `gx_target_units()` and `gx_harmonize()` contracts under
  ADR 0036. The reviewed target asset admits dimension-safe metric or imperial
  targets, while the offline harmonizer normalizes strict EDR position and
  current USGS continuous/daily results to one UTC observation table. It
  preserves source order, duplicate timestamps, qualifiers, original
  value/unit facts, the exact fetched object, and retained raw payloads.
  Conversion requires one unambiguous catalog variable/unit URI, exact
  native-label corroboration, and one directed reviewed affine rule; conflicts
  and missing mappings remain visibly unchanged. CSV, WQP, and general
  Features results are indexed as native-only resources rather than guessed.
- Extended `gx_harmonize()` to contract 0.2.0 under ADR 0037 with a bounded WQP
  slice. A WQP Result is normalized only when its request selects one exact
  characteristic and site, the catalog supplies one matching variable URI and
  label, every native row corroborates both facts, and every timestamp is
  explicitly UTC. Values, WQP measure qualifiers, duplicate instants, and the
  exact fetched payload are preserved. Unfiltered, mixed-characteristic,
  non-UTC, or incomplete WQP results remain indexed as native-only resources.
- Extended `gx_harmonize()` to contract 0.3.0 under ADR 0038 with reviewed WQX
  timezone normalization. The bundled asset freezes the 23 active EPA
  `TimeZoneCode` values and their explicit fixed offsets, including half-hour
  Newfoundland codes, while excluding three retired aliases. Unknown codes
  remain native-only. The exact timezone-asset hash is retained and revalidated
  with every harmonized object; no geographic or daylight-saving inference is
  performed.
- Added public `gx_csv_mapping()` and upgraded `gx_harmonize()` to contract
  0.4.0 under ADR 0039. A mapping binds one planned direct-CSV distribution to
  exact UTC datetime, value, unit-label, optional qualifier, and explicit
  missing-token columns. Mappings are identity-bound, embedded in the
  harmonized object, and revalidated before rows are re-derived. Missing
  columns, invalid timestamps, multiple catalog variables, unit conflicts, and
  unmapped CSV resources remain native-only; column names and missing tokens
  are never guessed.
- Added public `gx_feature_mapping()` and upgraded `gx_harmonize()` to contract
  0.5.0 under ADR 0040. A mapping binds one planned OGC API Features
  distribution to exact UTC datetime, value, unit-label, optional qualifier,
  and explicit missing-token properties. Generated feature identifiers and
  geometry are excluded from observation roles. Mappings are identity-bound,
  embedded, and revalidated; missing properties, incompatible types, invalid
  timestamps, multiple catalog variables, unit conflicts, and unmapped
  feature collections remain native-only without changing the retained `sf`
  result or raw GeoJSON.
- Added the unexported M9b catalog-only snapshot writer. It revalidates a
  catalog, creates deterministic redacted UTF-8 CSV views in a sibling staging
  tree, writes a manifest-v1 document last, verifies the closed tree through
  M9a, and publishes only to a new destination. Path-bearing filesystem
  warnings are suppressed behind typed errors, and owned-stage cleanup is
  checked. It does not claim Frictionless
  compatibility, authenticity, replayability, loading semantics, or overwrite
  ownership.
- Added public `gx_snapshot_verify()` as the M9c evidence-only wrapper over the
  hardened M9a verifier under ADR 0041. It validates one fixed manifest,
  rebinds the AOI recipe, inventories the closed tree twice, and verifies
  declared resource sizes and SHA-256 values without parsing resources or
  performing network, cache, replay, repair, or write work. The returned
  `gx_snapshot_verification` object revalidates its normalized manifest and
  resource evidence. Success proves unsigned internal consistency at
  verification time, not authenticity, historical provenance, licence truth,
  loading semantics, or coordinated-replacement resistance.
- Added public `gx_snapshot()` for the M9d catalog-only creation boundary under
  ADR 0042. It accepts one validated catalog and writes only four deterministic
  redacted CSV resources plus `manifest.json` through M9b's sibling staging,
  pre-publication verification, creation-only rename, and final verification
  workflow. The returned `gx_snapshot` object embeds validated M9c evidence and
  an identity over its normalized path, manifest hash, scope, and counts.
  `fetch`, `report`, and `overwrite` are explicit `FALSE`-only gates; fetched
  or harmonized resources, Frictionless metadata, loading, signatures, replay,
  and refresh remain unsupported.
- Added the internal M9e catalog request-export loader under ADR 0043. It
  recognizes only the exact M9b writer profile, re-derives canonical
  `requests.csv` bytes from the authoritative manifest ledger, loads that
  ledger to the exact typed catalog request schema, and verifies the closed
  tree again afterward. Unknown profiles, validly rehashed but noncanonical
  CSV bytes, malformed evidence, mutation, and exports above 64 MiB fail
  closed. Catalog resource loading and public replay remain gated.
- Added public `gx_snapshot_requests()` as the M9f read-only accessor under ADR
  0044. It exposes M9e's exact typed request table and evidence binding the
  normalized snapshot path, request count, manifest hash, request-resource
  hash, and deterministic export identity. It does not parse other resources,
  authenticate historical claims, or authorize loading or replay.
- Added the internal M9g canonical catalog-CSV loader under ADR 0045. It
  recognizes only the fixed M9b profiles and loads `sites.csv`, `datasets.csv`,
  or `problems.csv` as exact character tables under 64 MiB and fixed
  row/column/field ceilings. Parsed tables must reserialize byte-for-byte to
  the quote-all UTF-8 LF resource, and the complete tree is verified again.
  Blank cells, WKT, JSON, timestamps, logicals, and redacted identities remain
  uninterpreted; public catalog-resource loading is still gated.
- Added the internal M9h typed redacted catalog view under ADR 0046. It combines
  M9c verification, all three M9g character-table evidence objects, and M9f
  request evidence, then types only exact CRS84 point WKT, writer UTC
  timestamps, `true`/`false` fields, and canonical conforms-to JSON arrays.
  Blank and redacted strings remain unchanged, all typed projections are
  re-derived during validation, and the result explicitly denies live
  `gx_catalog` reconstruction and replay.
- Added public `gx_snapshot_catalog_view()` as the M9i inspection boundary
  under ADR 0047. It exposes M9h's exact verified sites, datasets, problems,
  requests, and canonical character evidence while remaining offline,
  read-only, unsigned, non-replayable, and explicitly distinct from a live
  `gx_catalog`.
- Added the internal M9j package-input boundary under ADR 0048. Exact catalog
  inputs are self-contained; fetched and harmonized inputs require the
  explicit source catalog and rebind its AOI and dataset identity to the
  embedded fetch plan. The resulting value object retains native payloads and
  binds canonical catalog projections, fetch status/result identities,
  harmonization evidence, counts, stage, and input identity without
  serializing, writing, publishing, or replaying.
- Normalized empty logical snapshot-export columns to `character(0)`, closing
  an in-memory canonical-evidence mismatch without changing written CSV bytes.
- Made canonical snapshot CSV loading locale-independent by reusing the
  bounded bytewise UTF-8 parser. Non-ASCII cells now retain their exact bytes
  even when R starts in the `C` locale, while noncanonical quoting, encoding,
  line endings, and record shapes still fail closed.
- Added the internal M9k deterministic package-resource bundle under ADR 0049.
  It derives path-sorted in-memory bytes from one exact M9j input: canonical
  catalog/status/index CSVs, exact retained provider bodies, canonical
  direct-CSV tables where no body was retained, and harmonized observation and
  resource CSVs. Every resource binds its path, format, byte count, SHA-256,
  dimensions, and result lineage under fixed per-resource, aggregate, field,
  and count budgets. Filesystem writes, publication, manifests, loading,
  Arrow/Parquet, Quarto, Frictionless, reports, refresh, and replay remain
  deferred.
- Added the internal M9l verified package-publication boundary under ADR 0050.
  It writes one exact M9k bundle and deterministic manifest-v1 into a private
  sibling staging tree, verifies the closed tree, atomically exposes only an
  absent destination, and verifies it again. Exact catalog, fetched, and
  harmonized resources can now be published without overwrite; the manifest
  honestly labels its request ledger catalog-only and the result
  non-replayable. Pre-publication failures clean only owned staging content,
  while a failure after exposure never deletes the destination. Public package
  creation/loading, Frictionless, optional formats, reports, refresh, and
  replay remain deferred.
- Added public `gx_package()` as the M9m creation boundary under ADR 0051.
  Validated catalog inputs are self-contained; fetched and harmonized inputs
  require their explicit source catalog and rebind it to the embedded fetch
  plan before serialization. The function publishes only the fixed
  deterministic CSV/raw profile through the exact M9j–M9l admission, in-memory
  serialization, verified sibling staging, and atomic exposure chain. It
  preserves retained provider bytes, refuses overwrite, and returns compact
  final verification evidence. Package loading, optional formats,
  Frictionless metadata, reports, authenticity, refresh, and replay remain
  deferred.
- Added public `gx_package_load()` as the M9n byte-preserving loading boundary
  under ADR 0052. It accepts only the fixed M9m writer profile, verifies the
  closed tree before and after loading, performs replacement-aware bounded
  reads, and returns every path-sorted resource as exact bytes rebound to its
  manifest SHA-256. CSVs remain raw UTF-8 CSV bytes and provider payloads
  remain opaque; typed hydration, authenticity, overwrite, optional formats,
  Frictionless validation, reports, refresh, and replay remain deferred.
- Added public `gx_package_tables()` as the M9o canonical table-view boundary
  under ADR 0053. It parses every M9n-loaded CSV from verified in-memory bytes
  with the bounded bytewise parser and requires exact quote-all UTF-8/LF
  round-tripping. Results retain their full byte-loading evidence and expose
  only character columns; native raw resources remain opaque, and no live
  workflow object, type inference, authenticity, overwrite, refresh, or replay
  claim is added.
- Added the internal M9p typed package-hydration substrate under ADR 0054. It
  starts from exact M9o evidence, reuses the redacted catalog typing rules,
  rebinds requests to the manifest ledger, and applies only fixed canonical
  storage types to package-owned fetch-index and harmonization tables.
  Provider-native tables remain character-only and raw payloads opaque; the
  result is fully revalidated, offline, read-only, unsigned, non-replayable,
  and not yet public.
- Added public `gx_package_hydrate()` as the M9q typed inspection boundary
  under ADR 0055. It exposes M9p's exact typed package-owned catalog,
  fetch-index, and harmonization tables while retaining complete canonical and
  byte-loading evidence. Provider-native tables remain character-only and raw
  resources opaque. The result is explicitly redacted, read-only, unsigned,
  non-replayable, and distinct from a reconstructed live workflow object.
- Added the internal M9r owned-package replacement substrate under ADR 0056.
  An existing destination is admitted only after complete closed-tree and
  fixed-writer-profile verification. The new bundle is independently staged
  and verified before the prior package moves to a sibling backup; install and
  final-verification failures restore and re-verify the prior generation.
  Failed restoration or cleanup retains typed recovery paths and committed
  state. At the M9r checkpoint, public overwrite remained gated for M9s.
- Enabled public `gx_package(..., overwrite = TRUE)` as M9s under ADR 0057.
  Replacement admits only a completely verified fixed-writer package and
  delegates staging, sibling backup, rollback, and retained recovery paths to
  M9r. The public `gx_package` v0.2 result distinguishes creation from
  replacement, embeds prior verification for replacement, and binds both
  manifest generations into its identity. Creation behavior remains
  absent-destination-only, and replacement performs no external work.
- Added the internal M9t Arrow/Quarto package preflight under ADR 0058. It
  safely inspects bounded installed-package metadata without loading either
  namespace. Missing packages produce `skipped_missing_pkg`; installed
  packages remain `blocked_version_unpinned` until reviewed minimum versions
  and symbols are frozen. Exact host-specific evidence binds both feature rows,
  observed versions, statuses, counts, limitations, and identity while keeping
  Parquet serialization and report rendering explicitly unauthorized.
- Added the internal M9u fixed Arrow/Parquet serializer under ADR 0059. Arrow
  R 14.0.0 is the reviewed minimum; required writer, reader, and in-memory
  stream exports are rechecked after namespace loading. Exact redacted typed
  observations serialize through a pinned uncompressed Parquet-2.4 profile,
  are read back from the same bounded raw bytes, and must match exactly.
  Evidence binds the complete source, table, bytes, Arrow version, writer
  controls, digest, and identity. Determinism is claimed only within one Arrow
  version; bundle integration and public Parquet were deferred to M9v.
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
