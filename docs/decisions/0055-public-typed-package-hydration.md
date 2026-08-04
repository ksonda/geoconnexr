# ADR 0055: Publish fixed typed package inspection

- Status: Accepted public boundary
- Date: 2026-08-03
- Owners: package maintainers
- Related: ADR 0047, ADR 0052, ADR 0053, ADR 0054

## Context

M9p proves that fixed package-owned CSV schemas can be hydrated without general
type inference or live workflow reconstruction. Users need a supported entry
point to those typed inspection tables, while the byte-preserving M9n and
character-only M9o views must remain available for lower-level evidence and
provider-native data.

## Decision

Export `gx_package_hydrate(dir)` as the M9q public typed inspection boundary.

The function delegates all loading, canonical parsing, exact typing, and
whole-object validation to M9n–M9p. It returns the validated
`gx_package_hydrated` object unchanged and adds a print method that states the
source stage, typed and native-table counts, redacted/read-only scope, and lack
of live workflow reconstruction.

The public result:

1. exposes typed redacted catalog tables for every fixed package stage;
2. exposes typed fetch status and native-resource indexes for fetched and
   harmonized stages;
3. exposes typed observations and harmonized-resource indexes only for the
   harmonized stage;
4. retains provider-native CSV tables as character-only and raw resources as
   opaque bytes in the embedded evidence; and
5. remains offline, read-only, unsigned, non-Frictionless, and non-replayable.

It does not reconstruct `gx_catalog`, `gx_fetched`, or `gx_harmonized`, infer
provider-native schemas, authenticate provenance, write, overwrite, repair,
refresh, or replay.

## Acceptance criteria

- `gx_package_hydrate()` exposes the exact revalidated M9p value for catalog,
  fetched, and harmonized packages.
- The print contract names the result as redacted, read-only, non-replayable,
  and non-reconstructing.
- Corrupt packages and forged internal evidence continue to fail closed.
- Public hydration performs no network, DNS, cache, optional-package, write,
  overwrite, report, refresh, or replay work.
- Only `gx_package_hydrate()` and `print.gx_package_hydrated()` are added to the
  public surface.

## Consequences

- Users can choose exact bytes, canonical character tables, or fixed typed
  tables without confusing any of them with restored workflow state.
- Typed package inspection is complete for the fixed CSV/raw profile.
- Overwrite ownership, Arrow/Parquet, Quarto, Frictionless validation, reports,
  authenticity, refresh, and replay remain later M9 work.
