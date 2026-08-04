# Declare one direct-CSV observation mapping

Creates an exact distribution-scoped mapping for one strict direct-CSV
result. Column names are never guessed. This first mapping contract
accepts only UTC instants spelled `YYYY-MM-DDTHH:MM:SSZ`, one value
column, one unit label column, and an optional qualifier column.
Missing-value tokens apply only to values and qualifiers.

## Usage

``` r
gx_csv_mapping(
  distribution_id,
  datetime_column,
  value_column,
  unit_column,
  qualifier_column = NULL,
  missing_values = ""
)
```

## Arguments

- distribution_id:

  Exact distribution SHA-256 from a
  [`gx_fetch_plan()`](https://ksonda.github.io/geoconnexr/reference/gx_fetch_plan.md)
  or `gx_fetched` object.

- datetime_column:

  Exact UTC timestamp column name.

- value_column:

  Exact observation-value column name.

- unit_column:

  Exact native unit-label column name.

- qualifier_column:

  `NULL` or one exact qualifier column name.

- missing_values:

  Up to 16 exact text tokens treated as missing in value and qualifier
  columns. The original value text remains preserved.

## Value

A validated `gx_csv_mapping` object for
[`gx_harmonize()`](https://ksonda.github.io/geoconnexr/reference/gx_harmonize.md).
