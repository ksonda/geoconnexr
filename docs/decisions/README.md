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

Each ADR states its context, decision, consequences, and status. Proposed ADRs
may change; accepted ADRs govern implementation until superseded.
