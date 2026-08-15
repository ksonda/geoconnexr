# Architecture decision records

This directory records durable repository, product, and architecture choices.
An accepted ADR is append-only: if a decision changes, add a superseding ADR
and link both records rather than rewriting history.

| ADR | Status | Decision |
|---|---|---|
| [0001](0001-repository-identity.md) | Accepted | Use `ksonda/geoconnexr` |
| [0002](0002-mit-license.md) | Accepted | License package code under MIT |
| [0003](0003-release-channels.md) | Accepted | Target CRAN and r-universe |
| [0004](0004-upstream-contract-gates.md) | Accepted release gate | Defer mainstem and SPARQL contracts pending evidence |
| [0005](0005-http-safety-boundary.md) | Accepted experimental policy | Fail closed at the provider-controlled HTTP boundary |
| [0006](0006-jsonld-processing-boundary.md) | Accepted experimental policy | Expand JSON-LD without parser-controlled network access |
| [0007](0007-reference-client-boundary.md) | Accepted experimental policy | Use a typed, bounded native reference client |
| [0008](0008-crosswalk-boundary.md) | Accepted experimental policy | Start M4 with a validated reference-gage crosswalk |
| [0009](0009-pinned-mainstem-lookup-lifecycle.md) | Accepted experimental policy | Install and verify the large COMID lookup explicitly |
| [0010](0010-package-owned-retry-accounting.md) | Accepted experimental policy | Own retries and account for every physical attempt |
| [0011](0011-package-owned-host-throttling.md) | Accepted experimental policy | Reserve physical attempts through a per-host throttle |
| [0012](0012-bounded-internal-graph-substrate.md) | Accepted internal spike | Bound one-shot graph results behind the SPARQL release gate |
| [0013](0013-render-only-query-manifest.md) | Accepted experimental policy | Pin and validate named queries without authorizing execution or pagination |
| [0014](0014-offline-custom-aoi-boundary.md) | Accepted experimental policy | Canonicalize custom polygon AOIs at an offline replay boundary |
| [0015](0015-release-scoped-mainstem-inverse.md) | Accepted internal substrate | Invert the pinned lookup only within its mapping release |
| [0016](0016-offline-aoi-recipe-hydration.md) | Accepted internal substrate | Re-establish AOI integrity when hydrating recipes |
| [0017](0017-offline-snapshot-verification.md) | Accepted internal substrate | Verify snapshot contents as a closed offline tree |
| [0018](0018-internal-catalog-value-object.md) | Accepted internal substrate | Separate the catalog value object from live discovery |
| [0019](0019-catalog-only-snapshot-writer.md) | Accepted internal substrate | Publish catalog-only snapshots through a verified staging tree |
| [0020](0020-internal-fetch-plan-selection.md) | Accepted internal substrate | Build an internal deterministic selection-only fetch plan |
| [0021](0021-host-package-capability-preflight.md) | Accepted internal substrate | Keep package capability preflight host-specific and non-executable |
| [0022](0022-inert-direct-csv-get-intents.md) | Accepted internal substrate | Record inert direct-CSV GET intents before transport |
| [0023](0023-bounded-direct-csv-request-plans.md) | Accepted internal substrate | Allocate bounded non-executable direct-CSV request plans |
| [0024](0024-offline-direct-csv-response-validation.md) | Accepted internal substrate | Validate caller-supplied direct-CSV response candidates offline |
| [0025](0025-bounded-offline-direct-csv-parsing.md) | Accepted internal substrate | Parse validated direct-CSV bytes under a strict offline profile |
| [0026](0026-single-attempt-direct-csv-execution.md) | Accepted internal substrate | Execute one direct-CSV request through the package transport |
| [0027](0027-bounded-direct-csv-orchestration.md) | Accepted internal substrate | Orchestrate bounded direct-CSV requests with exact status reconciliation |
| [0028](0028-single-page-oaf-handler.md) | Accepted internal substrate | Execute one reservation-bound single-page OGC API Features request |
| [0029](0029-cross-handler-orchestration.md) | Accepted internal substrate | Orchestrate CSV and OGC handlers under one bounded status contract |
| [0030](0030-single-response-wqp-handler.md) | Accepted internal substrate | Execute one WQP Result response under the held M7d reservation |
| [0031](0031-single-response-edr-position-handler.md) | Accepted internal substrate | Execute one EDR position CoverageJSON response under the held M7d reservation |
| [0032](0032-single-page-usgs-continuous-handler.md) | Accepted internal substrate | Execute one current USGS continuous-data page under the held M7d reservation |
| [0033](0033-single-page-usgs-daily-handler.md) | Accepted internal substrate | Execute one current USGS daily-values page under the held M7d reservation |
| [0034](0034-freeze-m7-supported-fetch-subset.md) | Accepted public boundary | Freeze the supported M7 subset, publish fetched results, and open M8 |
| [0035](0035-public-bounded-catalog-discovery.md) | Accepted experimental policy | Publish bounded catalog discovery without reopening M7 |
| [0036](0036-conservative-harmonization-boundary.md) | Accepted public boundary | Normalize reviewed time series without inferring semantic mappings |
| [0037](0037-filtered-utc-wqp-harmonization.md) | Accepted public boundary | Normalize exact filtered UTC WQP results without broad label inference |
| [0038](0038-reviewed-wqp-timezone-offsets.md) | Accepted public boundary | Convert active WQX timezone codes through reviewed fixed offsets |
| [0039](0039-explicit-direct-csv-mappings.md) | Accepted public boundary | Normalize strict direct CSV only through embedded distribution mappings |
| [0040](0040-explicit-oaf-observation-mappings.md) | Accepted public boundary | Normalize OGC Features observations only through explicit property mappings |
| [0041](0041-public-offline-snapshot-verification.md) | Accepted public boundary | Publish closed-tree snapshot verification as evidence, not loading or replay |
| [0042](0042-public-catalog-only-snapshot-creation.md) | Accepted public boundary | Publish creation-only catalog snapshots without broader package claims |
| [0043](0043-canonical-request-export-loading.md) | Accepted internal substrate | Bind canonical request-export bytes to typed manifest ledger rows |
| [0044](0044-public-snapshot-request-ledgers.md) | Accepted public boundary | Publish typed request-ledger access without catalog loading or replay |
| [0045](0045-canonical-redacted-catalog-csv-loading.md) | Accepted internal substrate | Load fixed redacted catalog CSVs only as canonical character tables |
| [0046](0046-typed-redacted-catalog-view.md) | Accepted internal substrate | Type redacted snapshot catalogs without reconstructing live identity |
| [0047](0047-public-typed-redacted-catalog-view.md) | Accepted public boundary | Publish typed redacted snapshot views without live reconstruction |
| [0048](0048-explicit-package-input-lineage.md) | Accepted internal substrate | Rebind explicit catalog lineage before package serialization |
| [0049](0049-deterministic-in-memory-package-resources.md) | Accepted internal substrate | Serialize fixed package resources before filesystem publication |
| [0050](0050-verified-creation-only-package-publication.md) | Accepted internal substrate | Publish fixed package resources through verified sibling staging |
| [0051](0051-public-verified-package-creation.md) | Accepted public boundary | Publish verified creation-only catalog, fetched, and harmonized packages |
| [0052](0052-public-byte-preserving-package-loading.md) | Accepted public boundary | Load fixed verified package resources as exact bounded bytes |
| [0053](0053-public-canonical-package-table-views.md) | Accepted public boundary | Parse verified package CSVs as canonical character tables |
| [0054](0054-internal-typed-package-hydration.md) | Accepted internal substrate | Hydrate only fixed package-owned table schemas |
| [0055](0055-public-typed-package-hydration.md) | Accepted public boundary | Publish fixed typed package inspection |
| [0056](0056-internal-owned-package-replacement.md) | Accepted internal substrate | Replace only verified owned packages with synchronous rollback |
| [0057](0057-public-verified-package-replacement.md) | Accepted public boundary | Expose verified package replacement through `gx_package()` |
| [0058](0058-host-specific-optional-package-preflight.md) | Accepted internal substrate | Inspect optional package metadata without authorizing execution |
| [0059](0059-fixed-in-memory-arrow-parquet.md) | Accepted internal substrate | Freeze an internal in-memory Arrow Parquet profile |
| [0060](0060-public-verified-parquet-packages.md) | Accepted public boundary | Publish verified Parquet observation packages end to end |
| [0061](0061-reviewed-quarto-runtime-capability.md) | Accepted internal substrate | Pin and resolve the Quarto R report capability without rendering |
| [0062](0062-reviewed-quarto-cli-admission.md) | Accepted internal substrate | Admit one reviewed Quarto CLI version command without rendering |
| [0063](0063-fixed-quarto-html-report.md) | Accepted internal substrate | Render and verify one fixed isolated execution-disabled HTML report |
| [0064](0064-private-report-package-integration.md) | Accepted internal substrate | Integrate verified HTML into private staged and replaceable packages |
| [0065](0065-public-verified-package-reports.md) | Accepted public boundary | Publish and access fixed verified package reports |
| [0066](0066-public-offline-replay-inspection.md) | Accepted public boundary | Inspect fixed stored state without executing replay |
| [0067](0067-internal-frictionless-data-package-profile.md) | Accepted internal substrate | Describe fixed package bundles as Frictionless Data Package v1 |
| [0068](0068-public-all-csv-frictionless-packages.md) | Accepted public boundary | Publish Frictionless descriptors for fixed all-CSV packages |
| [0069](0069-pinned-mixed-resource-frictionless-validation.md) | Accepted internal validation gate | Validate raw, Parquet, and report resources as opaque Frictionless files |
| [0070](0070-public-mixed-resource-frictionless-packages.md) | Accepted public boundary | Complete fixed-package M9 with mixed-resource Frictionless publication |
| [0071](0071-versioned-publisher-profile.md) | Accepted public boundary | Publish one versioned JSON-LD and sitemap profile |
| [0072](0072-shared-publisher-conformance.md) | Accepted P4 portability boundary | Share publisher conformance assets across R and Python |
| [0073](0073-owned-live-smoke-alerts.md) | Accepted operations policy | Assign and notify the weekly live-smoke owner |
| [0074](0074-configurable-experimental-graph-endpoint.md) | Accepted experimental upstream contract | Keep the documented graph root configurable and experimental |
| [0075](0075-mainstems-v3-default-and-explicit-migration.md) | Accepted experimental upstream contract | Use mainstems_v3 with explicit migration |
| [0076](0076-public-release-scoped-comid-crosswalks.md) | Accepted public boundary | Export COMID crosswalks with release-only currentness semantics |
| [0077](0077-public-huc12-outlet-crosswalk.md) | Accepted public boundary | Map HUC12 outlets through bounded NLDI evidence |
| [0078](0078-nldi-position-to-pinned-comid-contract.md) | Accepted experimental upstream contract | Resolve points through NLDI COMIDs and the pinned mapping |
| [0079](0079-public-point-to-mainstem-crosswalk.md) | Accepted public boundary | Publish bounded Point to mainstem crosswalking |
| [0080](0080-public-mainstem-currentness.md) | Accepted public boundary | Publish bounded mainstem currentness without following replacements |
| [0081](0081-public-mainstem-to-gages.md) | Accepted public boundary | Map mainstem PIDs to every matching reference gage |
| [0082](0082-huc12-intersection-ranking.md) | Accepted experimental upstream contract | Rank every HUC12 mainstem intersection without selecting one |
| [0083](0083-public-huc12-intersections.md) | Accepted public boundary | Publish bounded HUC12 mainstem intersections |
| [0084](0084-composed-crosswalk-currentness.md) | Accepted public boundary | Compose release and NLDI crosswalks with live mainstem currentness |
| [0085](0085-huc10-graph-spatial-gate.md) | Recorded upstream gate | Keep automatic HUC10 discovery behind a bounded graph spatial probe |
| [0086](0086-administrative-layers-future-scope.md) | Accepted product scope | Keep administrative layers outside the 0.x core |
| [0087](0087-measured-remaining-delivery-estimate.md) | Accepted planning baseline | Replace provisional phase ranges with measured remaining delivery units |

Each ADR states its context, decision, consequences, and status. Proposed ADRs
may change; accepted ADRs govern implementation until superseded.
