# Parse verified package CSV resources as canonical character tables

Loads a fixed-profile package with
[`gx_package_load()`](https://ksonda.github.io/geoconnexr/reference/gx_package_load.md)
and parses every CSV resource from its already verified in-memory bytes.
Parsing uses the same strict bytewise CSV implementation as snapshot
loading and admits only quote-all canonical UTF-8/LF tables under the
package resource, column, and field ceilings.

## Usage

``` r
gx_package_tables(dir)
```

## Arguments

- dir:

  Existing package directory created with the fixed public
  [`gx_package()`](https://ksonda.github.io/geoconnexr/reference/gx_package.md)
  profile.

## Value

A validated `gx_package_tables` object containing the embedded
byte-preserving load evidence, path-named canonical character tables,
fixed scope metadata, and deterministic table-view identity.

## Details

All CSV columns remain character vectors. Parquet and native raw
resources remain available only through the embedded `gx_package_loaded`
object. This function does not reconstruct a live catalog, fetched
result, or harmonized result, infer types or semantics, authenticate the
manifest, write, refresh, or replay.
