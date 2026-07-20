# Define an area of interest

Identifier AOIs accept HUCs, five-digit county FIPS codes, and
recognized two-letter state or territory abbreviations. Spatial AOIs
accept exactly one valid, non-empty, two-dimensional `POLYGON` or
`MULTIPOLYGON` in an
[sf](https://r-spatial.github.io/sf/reference/sf.html) or
[sfc](https://r-spatial.github.io/sf/reference/sfc.html) object with an
explicit CRS.

## Usage

``` r
gx_aoi(x, type = "auto")
```

## Arguments

- x:

  One character identifier, or one polygonal `sf`/`sfc` geometry.

- type:

  Exactly one of `"auto"`, `"huc"`, `"county"`, `"state"`, or `"sf"`.
  Partial matching is not supported.

## Value

An object of class `gx_aoi` containing a validated replay recipe.

## Details

Spatial inputs are transformed to OGC CRS84 with PROJ networking
disabled, rounded to a declared nine-decimal-degree grid with a
deterministic half-grid tie rule, bounded to 100,000 coordinate
positions and 8 MiB per canonical representation, normalized to GeoJSON
ring winding plus deterministic ring start and hole/member order, and
recorded as canonical GeoJSON with a SHA-256 of portable little-endian
WKB. Invalid or grid-collapsed geometry is rejected rather than
repaired. Antimeridian rings must be explicitly pre-cut. This function
performs no network or catalog work; the recipe's pipeline fields
describe the intended replay boundary.
