# Map mainstem PIDs to NHDPlus COMIDs in a pinned release

Returns every NHDPlus COMID associated with each canonical mainstem PID
in an explicitly installed, checksum-pinned `ref_rivers` lookup. The
function never downloads or refreshes lookup data. Results are complete
only within that mapping release and do not claim current live-service
state.

## Usage

``` r
gx_mainstem_to_comids(
  mainstem_uri,
  check = FALSE,
  version = "v3.2",
  data_dir = gx_default_data_dir()
)
```

## Arguments

- mainstem_uri:

  Character vector of canonical Geoconnex mainstem PIDs.

- check:

  Must currently be `FALSE`. Live `mainstems_v3` currentness and
  supersession checks remain a separate roadmap slice.

- version:

  Registered mapping release.

- data_dir:

  Package data directory containing an explicitly installed lookup. See
  [`gx_mainstem_lookup_install()`](https://ksonda.github.io/geoconnexr/reference/gx_mainstem_lookup_install.md).

## Value

A `gx_mainstem_comid_crosswalk` tibble. Its `gx_crosswalk` attribute
records mapping release, checksum provenance, counts, and the
`not_checked` currentness policy.
