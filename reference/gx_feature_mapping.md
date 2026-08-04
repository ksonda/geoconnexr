# Declare one OGC API Features observation mapping

Creates an exact distribution-scoped property mapping for one bounded
OGC API Features result. Property names are never guessed. Generated
feature identifiers and geometry cannot be assigned observation roles.
This first contract accepts only UTC instants spelled
`YYYY-MM-DDTHH:MM:SSZ`, one value property, one unit-label property, and
an optional qualifier property.

## Usage

``` r
gx_feature_mapping(
  distribution_id,
  datetime_property,
  value_property,
  unit_property,
  qualifier_property = NULL,
  missing_values = ""
)
```

## Arguments

- distribution_id:

  Exact distribution SHA-256 from a
  [`gx_fetch_plan()`](https://ksonda.github.io/geoconnexr/reference/gx_fetch_plan.md)
  or `gx_fetched` object.

- datetime_property:

  Exact UTC timestamp property name.

- value_property:

  Exact observation-value property name.

- unit_property:

  Exact native unit-label property name.

- qualifier_property:

  `NULL` or one exact qualifier property name.

- missing_values:

  Up to 16 exact text tokens treated as missing in character value and
  qualifier properties. The original value remains preserved.

## Value

A validated `gx_feature_mapping` object for
[`gx_harmonize()`](https://ksonda.github.io/geoconnexr/reference/gx_harmonize.md).
