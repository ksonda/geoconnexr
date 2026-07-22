# Fetch the supported M7 data subset

Executes the six frozen handler families—direct CSV, WQP Result, EDR
position, current USGS continuous, current USGS daily, and OGC API
Features—in deterministic global order. Failures are isolated to one
distribution and remain visible in the returned status table.

## Usage

``` r
gx_fetch(plan, parallel = 1L, dry_run = FALSE)
```

## Arguments

- plan:

  A validated `gx_fetch_plan` from
  [`gx_fetch_plan()`](https://ksonda.github.io/geoconnexr/reference/gx_fetch_plan.md).

- parallel:

  Must be exactly `1L`; parallel execution is not part of the frozen M7
  contract.

- dry_run:

  If `TRUE`, return planned statuses without provider work.

## Value

A `gx_fetched` object. `$status` contains one row per distribution;
`$results` contains one row per successful handler-native payload with
`data` and retained `raw_body` list-columns; `$provenance` retains the
validated bounded execution contract.

## Details

Execution is deliberately sequential and single-page. Each response is
capped at 8 MiB, one call admits at most 32 requests and 64 MiB, tabular
payloads are bounded to 100,000 rows, 1,000 columns, and 1,000,000
fields, and OGC API Features requests use a 10,000-feature page ceiling.
Redirects, retries, and cache reuse remain disabled by the underlying
execution contract. Latest and legacy USGS variants, page following,
runtime handler registration, serialization, and replay are deferred
rather than silently attempted.

A dry run performs the same planning and admission without package
probing, DNS, provider transport, clocks, throttling, cache access, or
writes.
