# Verify a closed geoconnexr snapshot offline

Verifies one existing snapshot directory against its fixed
`manifest.json`. The verifier validates the bounded manifest and AOI
recipe, inventories the closed resource tree, rejects links and
undeclared entries, and checks declared byte counts and SHA-256 values.
Resources remain opaque: this function does not parse, load, repair,
replay, or refresh them.

## Usage

``` r
gx_snapshot_verify(dir)
```

## Arguments

- dir:

  Existing snapshot directory containing `manifest.json`.

## Value

A validated `gx_snapshot_verification` evidence object with the
normalized manifest, rebound AOI, per-resource verification status,
request-ledger shape status, and manifest SHA-256.

## Details

Verification performs no network, DNS, cache, optional-package, or write
work. A successful result proves internal consistency relative to the
unsigned manifest at verification time. It does not prove authenticity,
historical request truth, licence accuracy, or protection against
coordinated replacement of both manifest and resources.
