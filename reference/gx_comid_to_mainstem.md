# Map NHDPlus COMIDs to mainstem PIDs in a pinned release

Maps character NHDPlus COMIDs through an explicitly installed,
checksum-pinned `ref_rivers` lookup. The function never downloads or
refreshes lookup data. Returned matches describe membership in the
mapping release and do not claim that a mainstem is current in the live
reference service.

## Usage

``` r
gx_comid_to_mainstem(
  comid,
  check = FALSE,
  version = "v3.2",
  data_dir = gx_default_data_dir()
)
```

## Arguments

- comid:

  Character vector of positive NHDPlus COMID identifiers.

- check:

  Must currently be `FALSE`. Live `mainstems_v3` currentness and
  supersession checks remain a separate roadmap slice.

- version:

  Registered mapping release.

- data_dir:

  Package data directory containing an explicitly installed lookup. See
  [`gx_mainstem_lookup_install()`](https://ksonda.github.io/geoconnexr/reference/gx_mainstem_lookup_install.md).

## Value

A `gx_comid_crosswalk` tibble. Its `gx_crosswalk` attribute records
mapping release, checksum provenance, counts, and the `not_checked`
currentness policy.
