# geoconnexr

<!-- badges: start -->
[![R-CMD-check](https://github.com/ksonda/geoconnexr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ksonda/geoconnexr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<div class="gx-hero">
  <p class="gx-eyebrow">Geoconnex for R</p>
  <h2>Follow the water, keep the evidence.</h2>
  <p class="gx-lede">
    Discover connected water resources, crosswalk durable identifiers, and
    assemble bounded, provenance-aware workflows without losing the identity
    of the source data.
  </p>
<div class="gx-actions"><a class="btn btn-light" href="articles/geoconnexr.html">Get started</a><a class="btn btn-outline-light" href="reference/index.html">Browse functions</a></div>
</div>

`geoconnexr` is an experimental R client for the
[Geoconnex](https://internetofwater.org/about-geoconnex/) ecosystem. Its current
public surface focuses on persistent-identifier resolution, bounded JSON-LD
profiles, OGC API Features reference data, provider-gage crosswalks, spatial
areas of interest, and safe query assets.

<div class="gx-card-grid">
<div class="gx-card"><h3>Identity first</h3><p>Preserve PIDs and leading-zero identifiers instead of silently coercing or replacing them.</p></div>
<div class="gx-card"><h3>Bounded by design</h3><p>Make request, redirect, byte, row, and parsing limits part of the contract.</p></div>
<div class="gx-card"><h3>Honest provenance</h3><p>Represent partial results and upstream failures explicitly, with no invented authority.</p></div>
</div>

## A five-minute tour

Install the development package from GitHub:

```r
install.packages("pak")
pak::pak("ksonda/geoconnexr")
```

Create an offline area-of-interest recipe, then inspect connected reference
features when you are ready to make a bounded network request:

```r
library(geoconnexr)

# Offline: preserve the HUC as a character identifier.
aoi <- gx_aoi("02070010")
aoi$recipe

# Online: retrieve at most two matching reference features.
mainstem <- gx_ref_features(
  "mainstems",
  query = list(id = "29559"),
  limit = 2L
)
attr(mainstem, "gx_reference")[c("truncated", "stop_reason", "requests")]
```

Resolve a Geoconnex PID and parse its profile while keeping retrieval evidence
attached to the result:

```r
pid <- "https://geoconnex.us/ref/gages/1000001"

resolution <- gx_resolve(pid)
document <- gx_jsonld(pid)
location <- gx_parse_location(document)

resolution[c("pid_uri", "landing_url", "problem_code")]
attr(location, "diagnostics")
```

## What works today

- Offline AOI recipes for HUC, COMID, PID, and canonical custom polygon inputs.
- Bounded PID resolution and fail-closed JSON-LD retrieval.
- Typed Geoconnex location and dataset profile parsing with diagnostics.
- OGC API Features collection, queryable, list, and single-feature clients.
- Provider-gage to Geoconnex PID crosswalking.
- Explicit installation and verification of the optional NHDPlus lookup.
- Typed SPARQL template rendering and portable distribution classification.

### Fetch roadmap progress

The internal M7k checkpoint now schedules the implemented direct-CSV, WQP
Result, and OGC API Features paths in one deterministic global order. They share explicit
request-count and byte admission limits, continue after typed handler failures,
and reconcile one exact terminal status per distribution. Dry run performs the
same planning without host or provider work. OGC next-page links remain
reported as truncation and are not followed. WQP performs one bounded request,
records Result/`narrowResult` plus site, optional characteristic, and UTC time,
and requires the invocation-time `dataRetrieval::importWQP()` result to match
the strict package parser. This remains tested infrastructure
for the future `gx_fetch()` workflow, not a newly exported fetch API. See
[ADR 0027](https://github.com/ksonda/geoconnexr/blob/main/docs/decisions/0027-bounded-direct-csv-orchestration.md)
and
[ADR 0028](https://github.com/ksonda/geoconnexr/blob/main/docs/decisions/0028-single-page-oaf-handler.md),
then
[ADR 0029](https://github.com/ksonda/geoconnexr/blob/main/docs/decisions/0029-cross-handler-orchestration.md).
The WQP boundary is specified in
[ADR 0030](https://github.com/ksonda/geoconnexr/blob/main/docs/decisions/0030-single-response-wqp-handler.md).

<div class="gx-status">
  <strong>Experimental status.</strong> The intended end-to-end catalog, fetch,
  harmonize, and snapshot workflow is still being implemented. The reference
  pages distinguish the public functions available now from internal roadmap
  substrates; do not treat the target API as released behavior.
</div>

## Choose your path

- [Get started](articles/geoconnexr.html) with installation and practical workflows.
- Read about [safety and reproducibility](articles/safety-and-reproducibility.html).
- Browse the complete [function reference](reference/index.html).
- Follow the [validated build roadmap](https://github.com/ksonda/geoconnexr/blob/main/geoconnexr-spec-v0.2.md).
- Review the [architecture decision records](https://github.com/ksonda/geoconnexr/tree/main/docs/decisions).
