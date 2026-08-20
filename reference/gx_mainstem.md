# Check current and superseded mainstem PIDs

Retrieves each unique PID from the `mainstems_v3` reference collection
and preserves the requested PID, superseded state, every advertised
replacement, collection, dataset vintage, observation time, retrieval
mode, and request ledger. Replacement PIDs are never followed or ranked
automatically.

## Usage

``` r
gx_mainstem(mainstem_uri, client = gx_client("reference"))
```

## Arguments

- mainstem_uri:

  Character vector of canonical Geoconnex mainstem PIDs.

- client:

  A reference client created by
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

## Value

A `gx_mainstem_currentness` tibble. Superseded one-to-many mappings
occupy one row per replacement. Its `gx_crosswalk` attribute contains
aggregate counts, diagnostics, and the redacted request ledger.
