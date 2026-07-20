# Discover reference feature collections

Retrieves the live OGC API Features collection inventory through the
bounded reference client. `refresh = TRUE` bypasses the package cache
for this call without mutating the supplied client.

## Usage

``` r
gx_ref_collections(refresh = FALSE, client = gx_client("reference"))
```

## Arguments

- refresh:

  Whether to bypass cached collection metadata.

- client:

  A reference client created by
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

## Value

A `gx_ref_collections` tibble with one row per advertised collection.
Nested `crs`, `extent`, `links`, and `raw` values are retained as
list-columns. Request and retrieval metadata are stored in the
`gx_reference` attribute.
