# Retrieve one reference feature with compatible fallbacks

Tries the OGC item route first, then a collection filter using the
queryable marked `x-ogc-role: id`, and finally direct JSON-LD
negotiation on the item URL. Every successful path verifies the
requested identity. The JSON-LD path is marked incomplete because it can
expose fewer properties than GeoJSON.

## Usage

``` r
gx_ref_feature(collection, id, client = gx_client("reference"))
```

## Arguments

- collection:

  One advertised collection identifier.

- id:

  One feature identifier. It is always returned as character.

- client:

  A reference client created by
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

## Value

A one-row `gx_ref_feature` simple-feature table. Its `gx_reference`
attribute records `retrieval_mode`, `complete`, attempts, diagnostics,
and a redacted ledger for every physical response or transport attempt
in the workflow, plus cache retrievals. A stage that fails before
receiving a response remains visible in `attempts` with a missing
status.
