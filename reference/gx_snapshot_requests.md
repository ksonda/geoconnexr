# Load a verified snapshot request ledger offline

Loads the authoritative request ledger from an existing catalog-only
snapshot after proving that `requests.csv` is its exact canonical
quote-all UTF-8 projection. The complete snapshot is verified before and
after loading, and the returned typed request table is rebound to the
normalized manifest ledger.

## Usage

``` r
gx_snapshot_requests(dir)
```

## Arguments

- dir:

  Existing catalog-only snapshot directory.

## Value

A validated `gx_snapshot_request_export` object containing the typed
request table, normalized path, request count, manifest and resource
hashes, and a deterministic export identity.

## Details

This function recognizes only snapshots created with the fixed
geoconnexr catalog writer and request-export profiles. It does not load
catalog sites, datasets, problems, fetched data, or harmonized data. It
performs no network, DNS, cache, write, repair, refresh, authenticity
check, or replay.
