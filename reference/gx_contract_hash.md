# Compute a versioned contract fingerprint

Creates a deterministic SHA-256 fingerprint from a namespace and a
vector of scalar values. Values are UTF-8 encoded and length-prefixed;
missing values use a distinct marker. This helper is intended for
contract keys, not cryptographic authentication.

## Usage

``` r
gx_contract_hash(values, namespace, contract_version = "0.1.0")
```

## Arguments

- values:

  An atomic vector or list of scalar atomic values.

- namespace:

  A non-empty scalar character namespace.

- contract_version:

  A non-empty scalar contract version.

## Value

A lowercase hexadecimal SHA-256 string.
