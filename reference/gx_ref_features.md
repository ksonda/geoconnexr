# Retrieve bounded reference features

Validates simple property filters against the collection's advertised
queryables and follows same-endpoint `rel=next` links until the
requested row limit or a deterministic stop condition is reached.
Query-bearing responses follow the package privacy policy and are not
persisted, so offline filtered replay is not promised.

## Usage

``` r
gx_ref_features(
  collection,
  query = list(),
  bbox = NULL,
  limit = 1000L,
  allow_unbounded = FALSE,
  client = gx_client("reference")
)
```

## Arguments

- collection:

  One advertised collection identifier.

- query:

  Named list of simple equality filters. Every name and scalar value is
  checked against
  [`gx_ref_queryables()`](https://ksonda.github.io/geoconnexr/reference/gx_ref_queryables.md).

- bbox:

  Optional numeric bounding box with four or six coordinates.

- limit:

  Positive overall row limit, not merely a page size.

- allow_unbounded:

  Explicit opt-in for a request without `query` or `bbox`; row, page,
  response-byte, and cumulative-byte ceilings still apply.

- client:

  A reference client created by
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

## Value

A `gx_ref_features` simple-feature table. Top-level GeoJSON IDs are
normalized to character `feature_id`, and hydrologic identifier property
columns are also character. The `gx_reference` attribute records
`truncated`, `stop_reason`, `number_matched`, page and byte counts,
diagnostics, and the redacted request ledger.

## Budgets

The per-response ceiling comes from `client$max_bytes`. Additional
options bound the workflow: `geoconnexr.ref_page_size` (default 1000),
`geoconnexr.ref_max_pages` (100), `geoconnexr.ref_total_bytes` (eight
times the client response ceiling), and `geoconnexr.ref_max_members`
(250,000 JSON members per response). Filter values default to 4,096
bytes through `geoconnexr.ref_max_query_value_bytes`, and every
reference URL defaults to 16,384 bytes through
`geoconnexr.ref_max_url_bytes`. Invalid option values fail before an
items request.
