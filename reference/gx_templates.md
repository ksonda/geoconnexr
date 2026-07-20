# List bundled, typed SPARQL templates

Returns the render-only named-query manifest. The `order`, `result_key`,
and `pagination` list columns distinguish reviewed query facts from
unproven cross-request stability. `pagination[[i]]$enabled` is currently
always `FALSE`; listing a candidate strategy does not authorize paging.

## Usage

``` r
gx_templates()
```

## Value

A tibble with one row per bundled template. It includes exact source
byte/hash pins, ordered result-variable contracts, render-only runtime
flags, and list columns for parameters, ordering, result keys, and
blocked pagination metadata.
