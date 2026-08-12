# Inspect a verified stored snapshot or package offline

Verifies and loads one existing fixed geoconnexr catalog snapshot or
data package without executing its recipe. Catalog snapshots return the
exact typed redacted
[`gx_snapshot_catalog_view()`](https://ksonda.github.io/geoconnexr/reference/gx_snapshot_catalog_view.md);
packages return the exact typed
[`gx_package_hydrate()`](https://ksonda.github.io/geoconnexr/reference/gx_package_hydrate.md)
view and, when present, read-only
[`gx_report()`](https://ksonda.github.io/geoconnexr/reference/gx_report.md)
evidence. Verification is repeated around loading by the underlying
fixed loaders, and the result is rebound to the current closed tree.

## Usage

``` r
gx_replay(manifest, dir = NULL, refresh = FALSE, ...)
```

## Arguments

- manifest:

  A `gx_snapshot`, `gx_package`, `gx_package_loaded`, existing
  snapshot/package directory, or exact `manifest.json` file path.

- dir:

  Must be `NULL`. Replay publication to a destination is deferred.

- refresh:

  Must be `FALSE`. Recipe execution and refresh are deferred.

- ...:

  Must be empty; future replay options are not yet authorized.

## Value

A validated `gx_replay` object containing the fixed source kind and
stage, final verification evidence, typed read-only view, optional
report evidence, explicit limitations, and deterministic inspection
identity.

## Details

This checkpoint supports only `refresh = FALSE`. It performs no network,
DNS, cache, optional runtime, write, repair, destination creation,
external Frictionless execution, authenticity check, or live
workflow-object reconstruction. It is stored-state inspection, not
procedural replay.
