# Build a versioned Geoconnex publisher profile

Builds deterministic JSON-LD from the exact site and dataset tables
owned by the catalog 0.1.0 contract. The function performs no network
access. Dataset rows for one dataset must form a complete distribution
by variable product, because JSON-LD represents those two dimensions
independently.

## Usage

``` r
gx_jsonld_build(sites, datasets = NULL, provider, context = gx_context())
```

## Arguments

- sites:

  An exact catalog 0.1.0 `sf` sites table.

- datasets:

  `NULL` or an exact catalog 0.1.0 datasets table whose site identities
  are present in `sites`.

- provider:

  A list with exact fields `uri`, `name`, and `url`. Present provider
  values in the input tables must agree with this publisher.

- context:

  A local JSON-LD context. It must retain the mappings returned by
  [`gx_context()`](https://ksonda.github.io/geoconnexr/reference/gx_context.md).

## Value

A parsed JSON-LD document with profile version 1.0.0.
