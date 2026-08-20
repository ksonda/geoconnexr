# Map NHDPlus COMIDs to mainstem PIDs in a pinned release

Maps character NHDPlus COMIDs through an explicitly installed,
checksum-pinned `ref_rivers` lookup. The function never downloads or
refreshes lookup data. With `check = TRUE`, every matched PID is also
checked against `mainstems_v3`; superseded PIDs retain every advertised
replacement without following or ranking it.

## Usage

``` r
gx_comid_to_mainstem(
  comid,
  check = FALSE,
  version = "v3.2",
  data_dir = gx_default_data_dir(),
  currentness_client = NULL
)
```

## Arguments

- comid:

  Character vector of positive NHDPlus COMID identifiers.

- check:

  Whether to compose the release mapping with bounded live
  `mainstems_v3` currentness.

- version:

  Registered mapping release.

- data_dir:

  Package data directory containing an explicitly installed lookup. See
  [`gx_mainstem_lookup_install()`](https://ksonda.github.io/geoconnexr/reference/gx_mainstem_lookup_install.md).

- currentness_client:

  A reference client used only when `check = TRUE`, or `NULL` to
  construct the default.

## Value

A `gx_comid_crosswalk` tibble. Its `gx_crosswalk` attribute records
mapping release, checksum provenance, counts, currentness policy, and
redacted live request ledger.
