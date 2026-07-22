# Get started with geoconnexr

`geoconnexr` helps R users navigate connected water data without
discarding identifier identity or the evidence needed to interpret a
result. This guide uses only the package’s current public functions.
Fetch planning and execution are available for the frozen supported
subset; public catalog discovery, harmonization, and snapshot
composition remain under active development.

## Install

Install the development version from GitHub:

``` r

install.packages("pak")
pak::pak("ksonda/geoconnexr")
```

Then attach the package:

``` r

library(geoconnexr)
```

## Start offline with an area of interest

Hydrologic identifiers stay as character values, so significant leading
zeroes are preserved. Creating a HUC recipe is deterministic and makes
no network request:

``` r

aoi <- gx_aoi("02070010")
aoi$recipe
```

Custom polygon recipes are offline too. Supply exactly one valid
`POLYGON` or `MULTIPOLYGON` with an explicit coordinate reference
system:

``` r

geometry <- sf::st_as_sfc(
  "POLYGON ((-77.2 38.8, -77.0 38.8, -77.0 39.0, -77.2 39.0, -77.2 38.8))",
  crs = "OGC:CRS84"
)

custom_aoi <- gx_aoi(geometry, type = "sf")
custom_aoi$recipe$aoi[c("crs", "wkb_sha256")]
```

## Resolve a persistent identifier

[`gx_resolve()`](https://ksonda.github.io/geoconnexr/reference/gx_resolve.md)
follows a Geoconnex PID through the package’s bounded request layer and
retains both the original PID and the resolved landing location:

``` r

pid <- "https://geoconnex.us/ref/gages/1000001"
resolution <- gx_resolve(pid)

resolution[c("pid_uri", "landing_url", "problem_code")]
```

A landing URL is not substituted for the PID as the resource’s identity.

## Retrieve and parse a Geoconnex profile

Retrieve bounded JSON-LD, then parse the monitoring-location and dataset
views:

``` r

document <- gx_jsonld(pid)
location <- gx_parse_location(document)
datasets <- gx_parse_datasets(document)

location
datasets
attr(location, "diagnostics")
```

Diagnostics are part of the result contract. Inspect them instead of
assuming that an empty or partial table means the upstream service had
no data.

## Explore reference features

Discover the available collections and their queryable fields before
issuing a bounded feature request:

``` r

gx_ref_collections()
gx_ref_queryables("hu12")

mainstem <- gx_ref_features(
  "mainstems",
  query = list(id = "29559"),
  limit = 2L
)

attr(mainstem, "gx_reference")[c("truncated", "stop_reason", "requests")]
```

Use
[`gx_ref_feature()`](https://ksonda.github.io/geoconnexr/reference/gx_ref_feature.md)
when you already know the collection and feature ID:

``` r

gage <- gx_ref_feature("gages", "1000001")
gage
```

## Crosswalk a provider gage identifier

Provider identifiers are opaque character values. They are never coerced
to numbers:

``` r

gages <- gx_gage_to_pid(c("USGS-08332622", "USGS-00000000"))

gages[c("requested_provider_id", "status", "gage_uri", "mainstem_uri", "comid")]
attr(gages, "gx_crosswalk")[c("complete", "requests", "diagnostics")]
```

## Render a reviewed query template

Inspect the bundled typed templates and render one with validated
parameters:

``` r

gx_templates()

query <- gx_render_query(
  "sites_on_mainstem",
  list(
    mainstem_uri = "https://geoconnex.us/ref/mainstems/1622734",
    limit = 100,
    offset = 0
  )
)

query
```

Rendering is local. It does not execute the query.

## Plan and fetch the supported subset

When a workflow has a validated `gx_catalog`, build the bounded plan
before performing provider work. Public catalog discovery is not yet
available, so this boundary currently serves catalog objects produced by
the validated experimental catalog pipeline.

``` r

plan <- gx_fetch_plan(
  catalog,
  time = as.POSIXct(c(
    "2025-01-01 00:00:00", "2025-01-31 23:59:59"
  ), tz = "UTC"),
  max_datasets = 20L,
  max_bytes = 64 * 1024^2
)

preview <- gx_fetch(plan, dry_run = TRUE)
preview$status

fetched <- gx_fetch(plan)
fetched$status
fetched$results[c("handler_id", "payload_class", "row_count")]
```

The frozen M7 contract is sequential and single-page. It supports direct
CSV, WQP Result, EDR position, current USGS continuous and daily data,
and OGC API Features. Inspect every status row: one failed or
unsupported distribution does not abort later work.
`fetched$results$data` retains handler-native tables or `sf`;
`fetched$provenance` retains the validated bounded execution evidence.

## Where to go next

- Read [Safety and
  reproducibility](https://ksonda.github.io/geoconnexr/articles/safety-and-reproducibility.md)
  before building operational workflows.
- Browse the [function
  reference](https://ksonda.github.io/geoconnexr/reference/index.md) for
  limits, return values, and error behavior.
- Follow the [build
  roadmap](https://github.com/ksonda/geoconnexr/blob/main/geoconnexr-spec-v0.2.md)
  for the distinction between available APIs and planned capabilities.
