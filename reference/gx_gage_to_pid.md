# Map provider gage identifiers to Geoconnex reference PIDs

Queries the native reference client using the advertised `provider_id`
property and verifies that every returned feature has consistent
provider, feature, property, and PID identities. Inputs are deduplicated
for transport and expanded back into their original order.

## Usage

``` r
gx_gage_to_pid(provider_id, client = gx_client("reference"))
```

## Arguments

- provider_id:

  A character vector of opaque provider identifiers. Numeric values,
  missing values, blanks, invalid UTF-8, and control characters are
  rejected without network access.

- client:

  A reference client created by
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

## Value

A `gx_gage_crosswalk` tibble with one row for a unique match, one
explicit sentinel row for no match, or every distinct match when a
provider identifier is ambiguous. Identifier columns are character. The
`gx_crosswalk` attribute contains aggregate counts, diagnostics,
retrieval time, completeness, and the redacted reference request ledger.

## Details

This experimental M4 slice retains advertised `mainstem_uri` values
without asserting that they belong to a selected current mainstem
vintage. Query- bearing reference responses are intentionally
non-cacheable, so offline crosswalk retrieval is not promised.

## Budgets

`geoconnexr.crosswalk_max_inputs` defaults to 100,
`geoconnexr.crosswalk_max_matches` to 100,
`geoconnexr.crosswalk_max_rows` to 1,000,
`geoconnexr.crosswalk_max_requests` to 256, and
`geoconnexr.crosswalk_total_bytes` to eight times the client response
ceiling. Invalid limits or incomplete reference pagination fail closed.
