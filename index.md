# geoconnexr

Geoconnex for R

## Follow the water, keep the evidence.

Discover connected water resources, crosswalk durable identifiers, and
assemble bounded, provenance-aware workflows without losing the identity
of the source data.

[Get
started](https://ksonda.github.io/geoconnexr/articles/geoconnexr.md)[Browse
functions](https://ksonda.github.io/geoconnexr/reference/index.md)

`geoconnexr` is an experimental R client for the
[Geoconnex](https://internetofwater.org/about-geoconnex/) ecosystem. Its
current public surface focuses on persistent-identifier resolution,
bounded JSON-LD profiles, OGC API Features reference data, provider-gage
crosswalks, spatial areas of interest, and safe query assets.

### Identity first

Preserve PIDs and leading-zero identifiers instead of silently coercing
or replacing them.

### Bounded by design

Make request, redirect, byte, row, and parsing limits part of the
contract.

### Honest provenance

Represent partial results and upstream failures explicitly, with no
invented authority.

## A five-minute tour

Install the development package from GitHub:

``` r

install.packages("pak")
pak::pak("ksonda/geoconnexr")
```

Create an offline area-of-interest recipe, then inspect connected
reference features when you are ready to make a bounded network request:

``` r

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

Resolve a Geoconnex PID and parse its profile while keeping retrieval
evidence attached to the result:

``` r

pid <- "https://geoconnex.us/ref/gages/1000001"

resolution <- gx_resolve(pid)
document <- gx_jsonld(pid)
location <- gx_parse_location(document)

resolution[c("pid_uri", "landing_url", "problem_code")]
attr(location, "diagnostics")
```

## What works today

- Offline AOI recipes for HUC, COMID, PID, and canonical custom polygon
  inputs.
- Bounded PID resolution and fail-closed JSON-LD retrieval.
- Typed Geoconnex location and dataset profile parsing with diagnostics.
- OGC API Features collection, queryable, list, and single-feature
  clients.
- Provider-gage to Geoconnex PID crosswalking.
- Explicit installation and verification of the optional NHDPlus lookup.
- Typed SPARQL template rendering and portable distribution
  classification.

### Fetch roadmap progress

The internal M7l checkpoint now schedules direct CSV, WQP Result, EDR
position, and OGC API Features in one deterministic global order. They
share explicit request-count and byte admission limits, continue after
typed handler failures, and reconcile one exact terminal status per
distribution. Dry run performs the same planning without host or
provider work. OGC next-page links remain reported as truncation and are
not followed. WQP performs one bounded request and requires
[`dataRetrieval::importWQP()`](https://rdrr.io/pkg/dataRetrieval/man/importWQP.html)
to match the strict package parser. EDR performs one bounded collection
`position` request for an exact CRS84 point, parameter, and UTC
interval, then requires
[`edr4r::covjson_to_tibble()`](https://rdrr.io/pkg/edr4r/man/covjson_to_tibble.html)
to match the package’s strict CoverageJSON PointSeries result. This
remains tested infrastructure for the future `gx_fetch()` workflow, not
a newly exported fetch API. See [ADR
0027](https://github.com/ksonda/geoconnexr/blob/main/docs/decisions/0027-bounded-direct-csv-orchestration.md)
and [ADR
0028](https://github.com/ksonda/geoconnexr/blob/main/docs/decisions/0028-single-page-oaf-handler.md),
then [ADR
0029](https://github.com/ksonda/geoconnexr/blob/main/docs/decisions/0029-cross-handler-orchestration.md).
The WQP boundary is specified in [ADR
0030](https://github.com/ksonda/geoconnexr/blob/main/docs/decisions/0030-single-response-wqp-handler.md).
The EDR boundary is specified in [ADR
0031](https://github.com/ksonda/geoconnexr/blob/main/docs/decisions/0031-single-response-edr-position-handler.md).

**Experimental status.** The intended end-to-end catalog, fetch,
harmonize, and snapshot workflow is still being implemented. The
reference pages distinguish the public functions available now from
internal roadmap substrates; do not treat the target API as released
behavior.

## Choose your path

- [Get
  started](https://ksonda.github.io/geoconnexr/articles/geoconnexr.md)
  with installation and practical workflows.
- Read about [safety and
  reproducibility](https://ksonda.github.io/geoconnexr/articles/safety-and-reproducibility.md).
- Browse the complete [function
  reference](https://ksonda.github.io/geoconnexr/reference/index.md).
- Follow the [validated build
  roadmap](https://github.com/ksonda/geoconnexr/blob/main/geoconnexr-spec-v0.2.md).
- Review the [architecture decision
  records](https://github.com/ksonda/geoconnexr/tree/main/docs/decisions).
