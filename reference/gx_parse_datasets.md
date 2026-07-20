# Parse datasets described by Geoconnex location profiles

Produces one row per dataset by distribution by variable. A missing
distribution or variable contributes one explicit `NA` dimension so an
incomplete real-world dataset is not silently discarded.

## Usage

``` r
gx_parse_datasets(x, strict = FALSE)
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

A `gx_datasets` tibble with stable identifiers, classifier facts,
provenance, and structured diagnostics. Its exact columns are
`contract_version`, `site_uri`, `dataset_id`, `distribution_id`,
`variable_id`, `dataset_uri`, `dataset_name`, `dataset_description`,
`temporal_coverage`, `temporal_start`, `temporal_end`, `variable_uri`,
`variable_name`, `unit_uri`, `unit_label`, `measurement_technique`,
`distribution_url`, `media_type`, `conforms_to`, `provider_uri`,
`provider_name`, `provider_url`, `license`, `access_rights`,
`handler_id`, `fetchable`, `source_url`, and `diagnostics`. Temporal
columns are UTC `POSIXct`, `fetchable` is logical, `conforms_to` and
`diagnostics` are list columns, and all others are character. Zero-row
results preserve types.

Dataset identifiers follow the installed language-neutral identity
contract. Absolute HTTP(S) and opaque IRIs preserve semantic fragments;
label fallback uses UTF-8 NFC and Unicode default case folding. Unsafe
distribution URLs are retained as provenance but never marked fetchable.
`gx_error_parser_budget` is raised before a distribution-by-variable
product exceeds the configured row ceiling. Diagnostics and strict-mode
behavior match
[`gx_parse_location()`](https://ksonda.github.io/geoconnexr/reference/gx_parse_location.md).
