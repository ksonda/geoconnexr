# ADR 0051: Publish verified creation-only packages

- Status: Accepted public boundary
- Date: 2026-07-30
- Owners: package maintainers
- Related: ADR 0048, ADR 0049, ADR 0050

## Context

M9j–M9l now admit exact catalog, fetched, and harmonized inputs, determine a
bounded deterministic resource bundle, and publish it through verified sibling
staging. Those boundaries remained internal while the package profile and
lineage rules were being established. Their creation semantics are now
independently complete, but loading, overwrite ownership, optional formats,
Frictionless metadata, reports, refresh, and replay remain unresolved.

Fetched and harmonized contracts do not embed the complete source catalog.
Public package creation must therefore require that catalog explicitly and
rebind it to the embedded fetch plan rather than reconstructing provenance.

## Decision

Export `gx_package(x, dir, catalog = NULL, timeseries = "csv",
keep_raw = TRUE, overwrite = FALSE)` as the M9m creation boundary.

The function:

1. accepts an exact `gx_catalog`, `gx_fetched`, or `gx_harmonized` object;
2. requires the explicit source catalog for fetched or harmonized input and
   rejects a redundant catalog for catalog input;
3. supports only the fixed CSV/raw profile, requires retained provider bytes
   to be preserved, and refuses overwrite;
4. delegates lineage admission, deterministic serialization, staged writing,
   atomic exposure, and closed-tree verification to M9j–M9l;
5. returns a compact exact `gx_package` object binding the normalized path,
   source stage, final M9a verification, fixed public scope, M9j input identity,
   M9k bundle identity, and deterministic package identity; and
6. remains offline and makes no loading, authenticity, Frictionless,
   Arrow/Parquet, Quarto, report, refresh, or replay claim.

The stored manifest continues to declare its request ledger `catalog_only` and
the package non-replayable.

## Acceptance criteria

- Catalog, fetched, and harmonized inputs publish through the exact M9j–M9l
  chain and return final verified public evidence.
- Missing or mismatched source catalogs and unsupported format, raw-retention,
  or overwrite choices fail before publication.
- Existing destinations are preserved and underlying staging cleanup and
  post-exposure preservation semantics remain unchanged.
- Public path, stage, verification, metadata, or identity forgery fails closed.
- Creation performs no network, DNS, cache, provider, optional-format,
  loading, report, refresh, or replay work.
- Only `gx_package()` and its print method are exported; serializers, writers,
  validators, and intermediate evidence remain internal.

## Consequences

- Users can now create integrity-checked catalog, fetched, and harmonized
  packages without calling internal APIs.
- The explicit catalog argument makes source lineage visible until a future
  fetched contract carries complete catalog provenance itself.
- Package loading, overwrite, optional formats, Frictionless validation,
  reports, authenticity, refresh, and replay remain later M9 work.
