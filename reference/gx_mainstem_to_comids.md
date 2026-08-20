# Map mainstem PIDs to NHDPlus COMIDs in a pinned release

Returns every NHDPlus COMID associated with each canonical mainstem PID
in an explicitly installed, checksum-pinned `ref_rivers` lookup. The
function never downloads or refreshes lookup data. With `check = TRUE`,
the requested PIDs are also checked against `mainstems_v3`, including
PIDs with no COMID in the selected mapping release.

## Usage

``` r
gx_mainstem_to_comids(
  mainstem_uri,
  check = FALSE,
  version = "v3.2",
  data_dir = gx_default_data_dir(),
  currentness_client = NULL
)
```

## Arguments

- mainstem_uri:

  Character vector of canonical Geoconnex mainstem PIDs.

- check:

  Whether to compose release membership with bounded live `mainstems_v3`
  currentness.

- version:

  Registered mapping release.

- data_dir:

  Package data directory containing an explicitly installed lookup. See
  [`gx_mainstem_lookup_install()`](https://ksonda.github.io/geoconnexr/reference/gx_mainstem_lookup_install.md).

- currentness_client:

  A reference client used only when `check = TRUE`, or `NULL` to
  construct the default.

## Value

A `gx_mainstem_comid_crosswalk` tibble. Its `gx_crosswalk` attribute
records mapping release, checksum provenance, counts, currentness
policy, and the redacted live request ledger.
