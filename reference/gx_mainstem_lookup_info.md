# Inspect an installed mainstem lookup

Verifies the content-addressed optional `ref_rivers` lookup used by the
experimental M4 crosswalk substrate. Inspection never downloads or
repairs data. A missing or invalid installation is reported in the
returned row. The lookup contains mainstems that were non-superseded
when v3.2 was generated; current reference-service state is deliberately
not checked.

## Usage

``` r
gx_mainstem_lookup_info(version = "v3.2", data_dir = gx_default_data_dir())
```

## Arguments

- version:

  Pinned lookup release. Currently only `"v3.2"` is registered.

- data_dir:

  Persistent package data directory. This is separate from the HTTP
  cache managed by
  [`gx_cache_info()`](https://ksonda.github.io/geoconnexr/reference/gx_cache_info.md).

## Value

A one-row tibble describing availability, integrity, pinned and observed
provenance, release-state semantics, and verification time.
