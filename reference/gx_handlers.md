# List portable distribution classifiers

Returns the language-neutral, first-match classifier registry. Runtime R
implementations are deliberately kept in a separate asset so another
language can reuse these facts without inheriting R package names.

## Usage

``` r
gx_handlers()
```

## Value

A tibble with one row per classifier, ordered by precedence.
