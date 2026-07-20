# Effective Geoconnex service endpoints

Returns the configured service endpoints. These values are defaults, not
a guarantee that an upstream service is available or stable. Override
them with `options(geoconnexr.endpoint_<name> = "...")`.

## Usage

``` r
gx_endpoints()
```

## Value

A named character vector with `graph`, `reference`, and `pid`.
