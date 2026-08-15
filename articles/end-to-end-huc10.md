# Build and verify a HUC10 package end to end

This case study runs the complete public workflow:

``` text
HUC10 -> catalog -> plan -> USGS daily fetch -> harmonize -> package -> offline verification
```

It uses HUC10 `0206000502` and USGS monitoring location `USGS-01491000`,
Choptank River near Greensboro, Maryland. The installed JSON-LD profile
is caller supplied and keeps the live example independent of the
upstream spatial graph. The catalog records that it did not
independently recheck whether the site lies inside the AOI.

The live fetch requires the optional `dataRetrieval` package. The
installed known-answer package at the end of the guide works offline
without that adapter.

## Build the catalog and bounded plan

``` r

library(geoconnexr)

site_uri <- "https://example.org/geoconnexr/usgs-01491000"
profile_path <- system.file(
  "extdata", "huc10-usgs-daily-profile.json",
  package = "geoconnexr"
)
profile <- jsonlite::fromJSON(profile_path, simplifyVector = FALSE)

catalog <- gx_catalog(
  gx_aoi("0206000502"),
  site_uri = site_uri,
  profiles = stats::setNames(list(profile), site_uri),
  max_sites = 1L
)

catalog$sites[c("site_uri", "name")]
catalog$datasets[c(
  "variable_name", "unit_uri", "distribution_url", "handler_id", "fetchable"
)]
```

The request covers two complete UTC days and selects one distribution. A
dry run derives and admits the request without contacting the provider.

``` r

fetch_window <- as.POSIXct(
  c("2025-06-01 00:00:00", "2025-06-02 23:59:59"),
  tz = "UTC"
)

plan <- gx_fetch_plan(
  catalog,
  time = fetch_window,
  max_datasets = 1L,
  max_bytes = 1024^2
)

gx_fetch(plan, dry_run = TRUE)$status
```

## Fetch and harmonize

``` r

fetched <- gx_fetch(plan)
fetched$status

stopifnot(
  nrow(fetched$results) == 1L,
  fetched$status$succeeded[[1L]]
)

harmonized <- gx_harmonize(fetched)
harmonized$observations[c(
  "datetime", "value", "unit_uri",
  "original_value", "original_unit_label", "status"
)]
```

The provider values use cubic feet per second. The reviewed conversion
rule stores cubic metres per second in `value` and retains the original
values and unit.

## Package and verify

[`gx_package()`](https://ksonda.github.io/geoconnexr/reference/gx_package.md)
requires an absent destination.
[`tempfile()`](https://rdrr.io/r/base/tempfile.html) returns a path that
has not been created.

``` r

package_dir <- tempfile("geoconnexr-huc10-")

package <- gx_package(
  harmonized,
  package_dir,
  catalog = catalog,
  frictionless = TRUE
)

verification <- gx_snapshot_verify(package$path)
verification$status

hydrated <- gx_package_hydrate(package$path)
hydrated$harmonized$observations[c(
  "datetime", "value", "unit_uri", "original_value", "original_unit_label"
)]

replay <- gx_replay(package$path)
replay$status
replay$metadata$recipe_executed
```

[`gx_replay()`](https://ksonda.github.io/geoconnexr/reference/gx_replay.md)
is an offline stored-state inspection. It verifies and loads the package
without issuing a request, but it does not execute a recipe or claim
that the package is cryptographically authentic.

## Use the installed offline demo

The package ships the result of this case study as a small archive.
Extracting it to a temporary directory preserves the verified package
tree. This path performs no network work and is suitable for a quick
demonstration.

``` r

demo_archive <- system.file(
  "extdata", "huc10-usgs-daily-demo.tgz",
  package = "geoconnexr"
)
demo_parent <- tempfile("geoconnexr-demo-")
dir.create(demo_parent)
utils::untar(demo_archive, exdir = demo_parent)
demo_path <- file.path(demo_parent, "huc10-usgs-daily-demo")

gx_snapshot_verify(demo_path)$status

demo <- gx_package_hydrate(demo_path)
demo$harmonized$observations[c(
  "datetime", "value", "unit_uri", "original_value", "original_unit_label"
)]

gx_replay(demo_path)$status
```

The fixture contains the exact retained provider response, catalog
tables, harmonized observations, manifest, and Frictionless descriptor.
It is a stable test and demonstration artifact, not a promise that a
later live response will contain identical values.
