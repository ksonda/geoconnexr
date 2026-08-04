# Create a verified catalog-only snapshot

Writes one validated
[`gx_catalog()`](https://ksonda.github.io/geoconnexr/reference/gx_catalog.md)
to a new snapshot directory using the fixed deterministic catalog CSV
profile and `manifest.json`. Creation occurs in a sibling staging
directory, the closed tree is verified before atomic exposure, and the
final directory is verified again. Existing destinations are never
replaced, repaired, or removed.

## Usage

``` r
gx_snapshot(x, dir, fetch = FALSE, report = FALSE, overwrite = FALSE)
```

## Arguments

- x:

  A validated `gx_catalog` object.

- dir:

  New destination directory beneath an existing safe parent.

- fetch:

  Must be `FALSE`; fetching during snapshot creation is not supported.

- report:

  Must be `FALSE`; report rendering is not supported.

- overwrite:

  Must be `FALSE`; existing destinations are preserved.

## Value

A validated `gx_snapshot` object containing the normalized absolute
path, final
[`gx_snapshot_verify()`](https://ksonda.github.io/geoconnexr/reference/gx_snapshot_verify.md)
evidence, and an exact catalog-only scope declaration.

## Details

This first public snapshot contract is deliberately catalog-only.
`fetch`, `report`, and `overwrite` must remain `FALSE`. It creates no
Frictionless data package, fetched or harmonized resources, report,
loading contract, signature, or replay authority. It performs no
network, DNS, discovery, cache, or optional-package work.
