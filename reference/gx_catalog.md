# Discover a bounded Geoconnex catalog

Builds the public catalog value object used by
[`gx_fetch_plan()`](https://ksonda.github.io/geoconnexr/reference/gx_fetch_plan.md).
With no explicit site PIDs, the function resolves identifier AOIs
through the Geoconnex reference service, executes one bounded
`sites_in_aoi` graph page, then retrieves each selected PID's JSON-LD
profile. Supplying `site_uri` skips graph discovery and retrieves
exactly those profiles; this is useful for reproducible examples, but
does not assert that the PIDs fall inside the AOI. Alternatively,
`profiles` can provide those exact profiles as local JSON-LD inputs
accepted by
[`gx_parse_location()`](https://ksonda.github.io/geoconnexr/reference/gx_parse_location.md).
This performs no PID profile request and is intended for offline catalog
construction or for providers whose distribution description is not yet
published through a PID.

## Usage

``` r
gx_catalog(
  aoi,
  site_uri = NULL,
  profiles = NULL,
  max_sites = 25L,
  client = gx_client("pid"),
  graph_client = gx_client("graph"),
  reference_client = gx_client("reference")
)
```

## Arguments

- aoi:

  A validated object returned by
  [`gx_aoi()`](https://ksonda.github.io/geoconnexr/reference/gx_aoi.md).

- site_uri:

  Optional unique canonical HTTP(S) site PID URIs. When supplied,
  automatic AOI membership discovery is skipped.

- profiles:

  Optional named list of local JSON-LD inputs, with names exactly
  matching `site_uri`. Each value may be a decoded JSON-LD list, a JSON
  string, raw JSON bytes, or a `gx_jsonld` object. Supplying profiles
  also requires `site_uri` and performs no PID profile requests.

- max_sites:

  Maximum graph rows or explicit site PIDs, from 1 through 100.

- client:

  A PID client from
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

- graph_client:

  A graph client from
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

- reference_client:

  A reference client from
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

## Value

A validated `gx_catalog` with typed sites, flattened datasets,
recoverable problems, request evidence, and procedural completeness.

## Details

Discovery is deliberately single-page and sequential. Reaching
`max_sites` records a partial, truncated catalog rather than following a
graph page. Individual profile failures remain visible in
`catalog$problems` and completeness metadata while unrelated sites
continue.
