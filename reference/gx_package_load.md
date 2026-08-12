# Load verified geoconnexr package bytes offline

Loads every declared resource from a package created by
[`gx_package()`](https://ksonda.github.io/geoconnexr/reference/gx_package.md)
as exact bounded raw bytes. The directory is verified before and after
loading, each file is checked for replacement while open, and all loaded
bytes are rebound to their manifest SHA-256 values.

## Usage

``` r
gx_package_load(dir)
```

## Arguments

- dir:

  Existing package directory created with the fixed public
  [`gx_package()`](https://ksonda.github.io/geoconnexr/reference/gx_package.md)
  profile.

## Value

A validated `gx_package_loaded` object containing the normalized path,
source stage, final verification, path-sorted resource inventory, exact
named resource bytes, fixed scope metadata, and load identity.

## Details

This boundary is deliberately byte-preserving. CSV and Parquet resources
remain raw bytes, a fixed report remains exact HTML bytes, and provider
payloads remain opaque raw bytes; it does not reconstruct a live
catalog, fetched result, or harmonized result. Loading is read-only,
offline, unsigned, and non-replayable. When the fixed package declares
Frictionless metadata, loading rederives its descriptor without an
external runtime and preserves that compatibility evidence.
