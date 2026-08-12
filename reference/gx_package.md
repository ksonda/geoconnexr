# Create or replace a verified geoconnexr data package

Serializes one validated catalog, fetched result, or harmonized result
to a directory using the fixed deterministic CSV/raw or Arrow/Parquet
profile. The complete resource bundle is determined in memory and
verified through a private sibling staging directory before final
destination verification.

## Usage

``` r
gx_package(
  x,
  dir,
  catalog = NULL,
  timeseries = "csv",
  keep_raw = TRUE,
  overwrite = FALSE,
  report = FALSE,
  frictionless = FALSE
)
```

## Arguments

- x:

  A validated `gx_catalog`, `gx_fetched`, or `gx_harmonized` object.

- dir:

  Destination directory beneath an existing safe parent.

- catalog:

  `NULL` for a catalog input. For fetched or harmonized input, the exact
  validated source `gx_catalog`.

- timeseries:

  `"csv"` (default) or `"parquet"`. Parquet is available only for
  harmonized inputs and requires Arrow 14.0.0 or newer.

- keep_raw:

  Must be `TRUE`; exact retained provider bodies are preserved.

- overwrite:

  One logical value. `FALSE` requires an absent destination; `TRUE`
  replaces only an intact package from the fixed writer profile.

- report:

  One logical value. When `TRUE`, render and integrate one fixed Quarto
  HTML report before final package publication.

- frictionless:

  One logical value. When `TRUE`, add one deterministic Frictionless
  Data Package v1 descriptor to the finalized package.

## Value

A validated `gx_package` object containing the normalized absolute path,
source stage, final
[`gx_snapshot_verify()`](https://ksonda.github.io/geoconnexr/reference/gx_snapshot_verify.md)
evidence, optional prior package verification for replacement, fixed
scope metadata, and deterministic package identity.

## Details

Fetched and harmonized inputs require their explicit source `catalog`
because those contracts do not embed the complete catalog. The catalog
is rebound to the input's exact AOI and dataset identity before
serialization.

This public package contract remains non-replayable. `timeseries` may be
`"csv"` or `"parquet"`; Parquet requires a harmonized input and Arrow
14.0.0 or newer. `keep_raw` must be `TRUE`. `report = TRUE` renders one
fixed, execution-disabled, cache-disabled, embedded-resource HTML report
through the reviewed Quarto R and CLI capability and binds its exact
bytes into the package. With `overwrite = FALSE`, the destination must
be absent. With `overwrite = TRUE`, the destination must be an intact
package produced by this fixed writer profile. The replacement is fully
staged and verified before the prior package moves to a sibling backup;
detected installation or final-verification failures synchronously
restore and re-verify the prior package. Report HTML can be read through
[`gx_report()`](https://ksonda.github.io/geoconnexr/reference/gx_report.md).
`frictionless = TRUE` adds a manifest-bound `datapackage.json`.
Canonical CSV resources carry exact string schemas; retained raw bytes,
Parquet, and report HTML remain opaque file resources. Refresh, replay,
and authenticity claims remain unsupported.
