# Map HUC12 outlets to mainstem PIDs

Retrieves one HUC12 pour point from the USGS NLDI `huc12pp` source. An
advertised mainstem PID is preferred. If the response has only a COMID,
the function resolves it through the explicitly installed
checksum-pinned mapping release. No lookup data is downloaded or
refreshed implicitly. In outlet mode, `check = TRUE` composes every
match with live `mainstems_v3` currentness without following or ranking
replacements.

## Usage

``` r
gx_huc12_to_mainstem(
  huc12,
  method = c("outlet", "intersects"),
  check = FALSE,
  version = "v3.2",
  data_dir = gx_default_data_dir(),
  client = NULL,
  currentness_client = NULL
)
```

## Arguments

- huc12:

  Character vector containing exact 12-digit hydrologic unit codes.

- method:

  `"outlet"` uses the NLDI pour point. `"intersects"` retrieves the
  reference HUC12 polygon and returns every locally intersecting
  `mainstems_v3` geometry with disclosed ranking metrics.

- check:

  Whether outlet matches should include bounded live currentness.
  Intersection matches already carry observed `mainstems_v3` state.

- version:

  Registered mapping release used only when an NLDI response omits its
  mainstem PID.

- data_dir:

  Package data directory containing an explicitly installed lookup when
  COMID fallback is needed.

- client:

  An NLDI client for `method = "outlet"`, a reference client for
  `method = "intersects"`, or `NULL` to construct the matching default.

- currentness_client:

  A reference client used for checked outlet matches, or `NULL` to
  construct the default.

## Value

For `method = "outlet"`, a `gx_huc12_crosswalk` tibble with NLDI and
optional mapping provenance. For `method = "intersects"`, a
`gx_huc12_intersection_crosswalk` tibble containing every ranked
geometry match, its metrics, observed currentness, replacements, and
reference request ledger.
