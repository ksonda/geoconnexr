# Clear the package HTTP cache

Clear the package HTTP cache

## Usage

``` r
gx_cache_clear(confirm = interactive(), cache_dir = gx_default_cache_dir())
```

## Arguments

- confirm:

  Prompt before clearing when `TRUE`. Non-interactive callers must pass
  `FALSE` explicitly.

- cache_dir:

  Cache directory to clear.

## Value

`TRUE` invisibly if the cache was cleared, otherwise `FALSE`.
