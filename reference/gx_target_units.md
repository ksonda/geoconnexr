# Select reviewed harmonization target units

Selects exactly one target for each dimension covered by the bundled,
reviewed conversion vocabulary. `NULL` selects the reviewed default:
Celsius, metre, and cubic metre per second. Overrides must be exact unit
URIs already reviewed for the corresponding dimension.

## Usage

``` r
gx_target_units(
  thermodynamic_temperature = NULL,
  length = NULL,
  volume_flow_rate = NULL
)
```

## Arguments

- thermodynamic_temperature:

  `NULL` or one reviewed temperature unit URI.

- length:

  `NULL` or one reviewed length unit URI.

- volume_flow_rate:

  `NULL` or one reviewed volume-flow-rate unit URI.

## Value

A validated `gx_target_units` object.
