# Classify a described distribution

Applies the portable first-match classifier registry. Classification
does not fetch or trust the supplied URL; request safety is enforced
separately by the future fetch planner and transport layer.

## Usage

``` r
gx_classify_distribution(
  access_url,
  media_type = NULL,
  conforms_to = character()
)
```

## Arguments

- access_url:

  One absolute HTTP(S) URL.

- media_type:

  Optional media type.

- conforms_to:

  Optional character vector of advertised conformance URIs.

## Value

One handler ID.
