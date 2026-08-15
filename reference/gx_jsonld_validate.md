# Validate a Geoconnex publisher JSON-LD profile

Validation is local and bounded. Findings use stable rule identifiers
and JSON pointers. Warnings do not make a document invalid; error
findings do.

## Usage

``` r
gx_jsonld_validate(x)
```

## Arguments

- x:

  A
  [`gx_jsonld()`](https://ksonda.github.io/geoconnexr/reference/gx_jsonld.md)
  object, JSON string, raw JSON, or parsed JSON-LD list.

## Value

A `gx_jsonld_validation` tibble with `severity`, `json_pointer`,
`rule_id`, `profile_version`, `message`, and `suggested_fix`. Its
`valid` attribute is true when no error finding is present.
