# Map Points to release-scoped mainstem PIDs

Transforms Point geometries to OGC CRS84 with PROJ networking disabled,
retrieves containing NHDPlusV2 COMIDs from the USGS NLDI position route,
then maps those COMIDs through the explicitly installed checksum-pinned
lookup. Lookup data is never installed or refreshed implicitly. With
`check = TRUE`, matched PIDs are checked against live `mainstems_v3`
while retaining every advertised replacement.

## Usage

``` r
gx_point_to_mainstem(
  points,
  check = FALSE,
  version = "v3.2",
  data_dir = gx_default_data_dir(),
  client = gx_client("nldi"),
  currentness_client = NULL
)
```

## Arguments

- points:

  An `sf` or `sfc` object containing nonempty two-dimensional Point
  geometries with a declared CRS.

- check:

  Whether to compose mapping matches with bounded live `mainstems_v3`
  currentness.

- version:

  Registered mapping release.

- data_dir:

  Package data directory containing an explicitly installed lookup.

- client:

  An NLDI client created by
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

- currentness_client:

  A reference client used when `check = TRUE`, or `NULL` to construct
  the default.

## Value

A `gx_point_crosswalk` tibble. Its `gx_crosswalk` attribute records NLDI
requests, mapping provenance, counts, and diagnostics.
