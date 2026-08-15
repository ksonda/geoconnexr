# Map HUC12 outlets to mainstem PIDs

Retrieves one HUC12 pour point from the USGS NLDI `huc12pp` source. An
advertised mainstem PID is preferred. If the response has only a COMID,
the function resolves it through the explicitly installed
checksum-pinned mapping release. No lookup data is downloaded or
refreshed implicitly.

## Usage

``` r
gx_huc12_to_mainstem(
  huc12,
  method = c("outlet", "intersects"),
  check = FALSE,
  version = "v3.2",
  data_dir = gx_default_data_dir(),
  client = gx_client("nldi")
)
```

## Arguments

- huc12:

  Character vector containing exact 12-digit hydrologic unit codes.

- method:

  Only `"outlet"` is currently available. Intersection ranking remains a
  separate roadmap decision.

- check:

  Must currently be `FALSE`; returned mainstems do not assert current
  live-service state.

- version:

  Registered mapping release used only when an NLDI response omits its
  mainstem PID.

- data_dir:

  Package data directory containing an explicitly installed lookup when
  COMID fallback is needed.

- client:

  An NLDI client created by
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

## Value

A `gx_huc12_crosswalk` tibble with one or more rows per input. Its
`gx_crosswalk` attribute records NLDI requests, diagnostics, counts, and
release provenance when the COMID fallback was used.
