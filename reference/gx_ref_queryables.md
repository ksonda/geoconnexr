# Discover queryable collection properties

Reads the collection's OGC queryables schema. The complete property
schema is retained so
[`gx_ref_features()`](https://ksonda.github.io/geoconnexr/reference/gx_ref_features.md)
can validate simple equality filters before sending them.

## Usage

``` r
gx_ref_queryables(collection, client = gx_client("reference"))
```

## Arguments

- collection:

  One advertised collection identifier.

- client:

  A reference client created by
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

## Value

A `gx_ref_queryables` tibble. The `json_types`, `enum`, and `schema`
columns are list-columns that preserve server-advertised JSON Schema
data.
