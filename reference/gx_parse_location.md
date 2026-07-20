# Parse Geoconnex monitoring-location profiles

Extracts documented and known production Geoconnex location shapes from
safely expanded JSON-LD. A generic `schema:Place` is accepted only for
reviewed state-gage PID namespaces when it carries nonempty GeoSPARQL
WKT; this emits a warning-level `generic_place_geometry` diagnostic. WKT
remains text at this protocol layer.

## Usage

``` r
gx_parse_location(x, strict = FALSE)
```

## Arguments

- x:

  A
  [`gx_jsonld()`](https://ksonda.github.io/geoconnexr/reference/gx_jsonld.md)
  object, parsed JSON-LD list, JSON string, or raw JSON bytes.

- strict:

  Whether any warning/error diagnostic should abort after deterministic
  parsing.

## Value

A `gx_location` tibble with one row per supported location. Its exact
columns are `contract_version`, `site_uri`, `name`, `description`,
`site_type`, `rdf_types`, `provider_uri`, `provider_name`,
`provider_url`, `mainstem_uri`, `geometry_wkt`, `geometry_crs`,
`landing_url`, `source_url`, and `diagnostics`. All are character except
the two list-columns `rdf_types` and `diagnostics`. Zero-row results
preserve those types.

## Diagnostics and provenance

Each row's `diagnostics` entry has fixed character columns `severity`,
`code`, `path`, and `message`, plus logical `recoverable`. The table's
`diagnostics` attribute includes document-level and row-level
diagnostics, including for a zero-row result. With `strict = TRUE`, any
warning or error is reported as a `gx_error_parser_strict` after
deterministic parsing. The table retains exact provenance URLs; its
print method redacts query values and credentials.

This P0 contract is provisional: WKT remains text, duplicate
relationships remain diagnostic, and no canonical-site or typed-geometry
contract is yet asserted.
