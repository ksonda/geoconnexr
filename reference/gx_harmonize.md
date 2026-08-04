# Harmonize reviewed fetched time-series observations

Normalizes strict EDR position, current USGS continuous/daily,
single-characteristic WQP Result schemas with reviewed fixed-offset
timezone codes, explicitly mapped direct-CSV tables, and explicitly
mapped OGC API Features properties to a shared UTC observation table.
Conversion occurs only when one catalog variable and unit URI is
unambiguous, native labels exactly corroborate catalog labels, and one
reviewed directed conversion rule reaches the requested target. Unknown
or conflicting mappings remain unchanged with `harmonized = FALSE`.

## Usage

``` r
gx_harmonize(
  fetched,
  target_units = gx_target_units(),
  csv_mappings = list(),
  feature_mappings = list()
)
```

## Arguments

- fetched:

  A validated `gx_fetched` object from
  [`gx_fetch()`](https://ksonda.github.io/geoconnexr/reference/gx_fetch.md).

- target_units:

  A validated target selection from
  [`gx_target_units()`](https://ksonda.github.io/geoconnexr/reference/gx_target_units.md).

- csv_mappings:

  An empty list, one
  [`gx_csv_mapping()`](https://ksonda.github.io/geoconnexr/reference/gx_csv_mapping.md)
  object, or an unnamed list of mappings for distinct planned direct-CSV
  distributions.

- feature_mappings:

  An empty list, one
  [`gx_feature_mapping()`](https://ksonda.github.io/geoconnexr/reference/gx_feature_mapping.md)
  object, or an unnamed list of mappings for distinct planned OGC API
  Features distributions.

## Value

A validated `gx_harmonized` object containing `$observations`,
`$resources`, normalized `$csv_mappings` and `$feature_mappings`, and
the exact original `$fetched` object.

## Details

Unfiltered, mixed-characteristic, unknown-timezone, or incomplete WQP
results remain native-only. Unmapped or schema-incompatible direct CSV
or OGC API Features payloads remain losslessly available through
`$fetched` and are indexed as native-only `$resources`. Feature
identifiers and geometry never supply observation semantics. Timestamps
are UTC, daily dates become UTC midnight, qualifiers and duplicate
timestamps are preserved, and no network or optional-package work
occurs.
