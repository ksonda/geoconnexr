# Build a bounded fetch plan

Collapses a validated `gx_catalog` to one deterministic row per
distribution, intersects requested time coverage, applies the built-in
first-match handler registry, and records count and byte budgets.
Planning performs no DNS lookup or provider request.

## Usage

``` r
gx_fetch_plan(
  catalog,
  time = NULL,
  handlers = gx_handlers(),
  max_datasets = 100L,
  max_bytes = 1e+09
)
```

## Arguments

- catalog:

  A validated `gx_catalog` object.

- time:

  `NULL`, two UTC `POSIXct` values, or an exact list containing `start`
  and `end` UTC bounds.

- handlers:

  The exact built-in registry returned by
  [`gx_handlers()`](https://ksonda.github.io/geoconnexr/reference/gx_handlers.md).

- max_datasets:

  Maximum number of distributions to select.

- max_bytes:

  Maximum aggregate encoded bytes and maximum aggregate decoded bytes.

## Value

A validated `gx_fetch_plan` object.

## Details

The frozen M7 contract accepts only the built-in registry returned by
[`gx_handlers()`](https://ksonda.github.io/geoconnexr/reference/gx_handlers.md).
Runtime registration is deferred. `max_bytes` is applied independently
to encoded and decoded response budgets, and each selected distribution
receives at most one physical-attempt reservation.
