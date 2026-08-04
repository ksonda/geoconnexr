# Load a typed redacted catalog view from a snapshot

Loads the fixed catalog resources and request ledger from an existing
catalog-only snapshot as a verified typed redacted view. The complete
snapshot is verified before and after loading, and every typed
projection is rebound to the canonical stored character evidence.

## Usage

``` r
gx_snapshot_catalog_view(dir)
```

## Arguments

- dir:

  Existing catalog-only snapshot directory.

## Value

A validated `gx_snapshot_catalog_view` object containing typed sites,
datasets, problems, and requests; the canonical character-table
evidence; final verification evidence; and an exact deterministic view
identity.

## Details

This function recognizes only snapshots created with the fixed
geoconnexr catalog writer profile. It types exact CRS84 point geometry,
writer UTC timestamps, `true`/`false` fields, and canonical ordered JSON
string arrays. All other strings remain unchanged, including blank cells
and redacted values.

The result is not a live `gx_catalog`: it does not reconstruct discarded
identities or missing values and does not authorize replay. Loading
performs no network, DNS, cache, write, repair, refresh, authenticity
check, Frictionless interpretation, or replay.
