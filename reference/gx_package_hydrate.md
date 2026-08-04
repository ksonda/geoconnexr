# Hydrate verified package-owned tables with fixed storage types

Loads a package through
[`gx_package_tables()`](https://ksonda.github.io/geoconnexr/reference/gx_package_tables.md)
and applies only the exact storage types declared by the fixed package
profile. Catalog tables receive the same redacted geometry, UTC
timestamp, logical, and JSON-array typing as
[`gx_snapshot_catalog_view()`](https://ksonda.github.io/geoconnexr/reference/gx_snapshot_catalog_view.md).
Package-owned fetch indexes and harmonization tables receive explicit
integer, double, logical, and UTC timestamp types. Parquet observations
are read in memory through the reviewed Arrow capability and must
reproduce the exact fixed typed schema.

## Usage

``` r
gx_package_hydrate(dir)
```

## Arguments

- dir:

  Existing package directory created with the fixed public
  [`gx_package()`](https://ksonda.github.io/geoconnexr/reference/gx_package.md)
  profile.

## Value

A validated `gx_package_hydrated` object containing typed package-owned
tables, character-only provider-native tables, the complete canonical
table evidence, fixed scope metadata, and deterministic hydration
identity.

## Details

Provider-native CSV tables remain character-only and raw provider
resources remain opaque in the embedded loading evidence. The result is
a redacted, read-only inspection view; it does not reconstruct a live
`gx_catalog`, `gx_fetched`, or `gx_harmonized` object, authenticate the
unsigned manifest, write, refresh, or replay the package.
