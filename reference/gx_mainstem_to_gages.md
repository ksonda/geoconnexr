# Map mainstem PIDs to reference gages

Queries the reference service using its advertised `mainstem_uri`
property, validates every gage, provider, and mainstem identity, and
returns all matching gages. Repeated mainstem PIDs share transport and
are expanded in the caller's input order.

## Usage

``` r
gx_mainstem_to_gages(mainstem_uri, client = gx_client("reference"))
```

## Arguments

- mainstem_uri:

  Character vector of canonical Geoconnex mainstem PIDs.

- client:

  A reference client created by
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

## Value

A `gx_inverse_gage_crosswalk` tibble with every matching reference gage
or one explicit not-found row. Its `gx_crosswalk` attribute contains
aggregate counts, diagnostics, and the redacted request ledger.

## Details

This release does not compose the result with
[`gx_mainstem()`](https://ksonda.github.io/geoconnexr/reference/gx_mainstem.md).
Advertised mainstem membership therefore has
`currentness_policy = "not_checked"` and no superseded PID is followed
automatically.
