# ADR 0052: Load verified package resources as exact bytes

- Status: Accepted public boundary
- Date: 2026-07-30
- Owners: package maintainers
- Related: ADR 0041, ADR 0047, ADR 0049, ADR 0050, ADR 0051

## Context

M9m exposes deterministic creation of catalog, fetched, and harmonized
packages, but users still lack a supported way to read the resulting resource
bundle. Broad hydration would require separate contracts for reconstructing
live catalog, fetched, and harmonized objects; interpreting provider payloads;
and authorizing refresh or replay. Those semantics are not implied by closed
tree integrity.

The fixed writer already provides enough evidence for a narrower useful
boundary: a path-sorted manifest inventory, exact sizes and SHA-256 values,
bounded aggregate storage, fixed resource roles, and explicit non-replayable
metadata.

## Decision

Export `gx_package_load(dir)` as the M9n byte-preserving package loader.

The function:

1. accepts only an existing package using the exact M9m writer and resource
   profiles;
2. verifies the complete closed tree before loading, reads each required
   resource under the M9k per-file and aggregate byte ceilings, and verifies
   the complete tree again afterward;
3. checks file identity around every bounded read and rebinds the loaded bytes
   to the manifest byte count and SHA-256;
4. returns a path-sorted `gx_package_loaded` value object containing the exact
   named raw vectors, final verification, fixed role/format inventory, source
   stage, scope metadata, and deterministic load identity; and
5. remains read-only, offline, byte-preserving, unsigned, non-Frictionless,
   and non-replayable.

CSV resources remain exact raw UTF-8 CSV bytes. Native provider resources
remain opaque bytes. The loader does not construct a `gx_catalog`,
`gx_fetched`, or `gx_harmonized` object and does not parse, repair, write,
authenticate, refresh, or replay any resource.

## Acceptance criteria

- Packages created from catalog, fetched, and harmonized inputs load through
  the same exact fixed profile and retain their source-stage evidence.
- Every returned resource is byte-identical to its stored file and binds its
  manifest path, roles, format, media type, byte count, and SHA-256.
- Catalog-only snapshots and manifests outside the fixed package profile fail
  closed.
- Corruption before loading and mutation during a resource read or between
  whole-tree verifications fail closed.
- Path, stage, verification, inventory, content, metadata, or identity forgery
  fails whole-object validation.
- Loading performs no network, DNS, cache, optional-package, writer, repair,
  report, refresh, or replay work.
- Only `gx_package_load()` and the `gx_package_loaded` print method are added to
  the public surface.

## Consequences

- Users can safely inspect or hand exact package bytes to their own tooling
  without bypassing integrity checks.
- Byte-preserving loading makes no lossy or executable reconstruction claim.
- Typed table access, live object hydration, overwrite ownership,
  Arrow/Parquet, Quarto, Frictionless validation, reports, authenticity,
  refresh, and replay remain later M9 work.
