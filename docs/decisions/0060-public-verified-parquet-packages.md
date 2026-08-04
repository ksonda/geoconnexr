# ADR 0060: Publish verified Parquet observation packages

- Status: Accepted public boundary
- Date: 2026-08-03
- Owners: package maintainers
- Related: ADR 0049, ADR 0052, ADR 0055, ADR 0057, ADR 0059
- Supersedes: the public-exposure deferral in ADR 0059

## Context

ADR 0059 fixed and verified an in-memory Arrow/Parquet profile but deliberately
left bundle, manifest, loading, hydration, replacement, and public API behavior
unconnected. Exposing only filesystem writing would create packages that the
public readers could not recognize or type safely. The format therefore has to
cross every fixed-package boundary as one coordinated contract change.

## Decision

Add the public M9v Parquet package profile.

1. `gx_package(..., timeseries = "parquet")` is admitted only for an exact
   harmonized input and requires the reviewed Arrow 14.0.0-or-newer capability.
   Catalog and fetched inputs fail before staging or destination creation.
2. The fixed resource bundle stores the redacted typed observations as
   `data/observations.parquet` with media type
   `application/vnd.apache.parquet`. All catalog, fetch-index, native, and
   harmonization-index resources keep their existing CSV/raw representations.
3. Initial bundle construction uses ADR 0059's in-memory writer and exact
   read-back check. Bundle evidence binds the source input, selected format,
   Arrow writer version, resource bytes, digest, dimensions, and bundle
   identity. Subsequent validation checks the bounded Parquet markers and all
   bound evidence without repeatedly executing the optional serializer.
4. Manifest-v1 records `timeseries: parquet`, the fixed Parquet profile, Arrow
   writer and minimum versions, and the existing non-replayable package
   limitations. Closed-tree verification remains format-agnostic and binds the
   exact file bytes.
5. `gx_package_load()` recognizes and returns Parquet as exact raw bytes without
   loading Arrow. `gx_package_tables()` parses only canonical CSV resources and
   reports the Parquet resource separately.
6. `gx_package_hydrate()` resolves the reviewed Arrow capability, reads the
   verified Parquet bytes in memory, and requires the exact typed, redacted
   observation schema. It still does not reconstruct a live workflow object.
7. The owned-package replacement path accepts verified CSV and Parquet
   generations under the same staging, rollback, and recovery policy.
8. Parquet byte determinism remains scoped to the Arrow version that wrote the
   package. The manifest is unsigned, and reports, Frictionless validation,
   refresh, and replay remain deferred.

## Acceptance criteria

- Empty and populated harmonized packages publish one exact Parquet observation
  resource and pass staged and final closed-tree verification.
- Catalog/fetched Parquet requests and unknown formats fail before filesystem
  publication.
- Byte-preserving loading identifies the Parquet resource without Arrow
  execution; canonical table views leave it opaque.
- Typed hydration reads the exact verified bytes through the reviewed Arrow
  boundary and reproduces the fixed observation schema.
- CSV creation and inspection remain the default and retain their behavior.
- CSV-to-Parquet replacement preserves prior-generation evidence and the
  existing rollback guarantees.
- Resource, manifest, loaded-byte, typed-table, metadata, and identity forgery
  fail closed; no network, report, refresh, or replay work occurs.

## Consequences

- Parquet is now a complete opt-in public storage path rather than an isolated
  serializer.
- Loading remains available without Arrow, while typed Parquet hydration
  explicitly requires Arrow.
- Packages disclose the Arrow writer version and do not claim cross-version
  byte stability.
- Quarto/report pinning and Frictionless CLI validation are the next M9
  roadmap checkpoints.
