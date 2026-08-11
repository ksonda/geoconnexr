# ADR 0070: Publish mixed-resource Frictionless packages

- Status: Accepted public boundary; fixed-package M9 complete
- Date: 2026-08-10
- Owners: package maintainers
- Related: ADR 0060, ADR 0065, ADR 0066, ADR 0068, ADR 0069

## Context

M9ad publishes deterministic Frictionless descriptors for all-CSV packages.
M9ae proves that the already fixed retained-raw, Arrow/Parquet, and report
resources pass pinned core Frictionless 5.19.0 validation as opaque files. The
remaining implementable M9 work is public composition of those existing
contracts, not another resource format or replay design.

Live refresh and procedural replay still lack a complete reproducible request
recipe. Expanding that work without the missing recipe would invent behavior
rather than implement a reviewed contract.

## Decision

Add M9af as the final fixed-package M9 boundary.

1. Apply `frictionless = TRUE` after the requested fixed raw, Parquet, and/or
   report bundle has been finalized.
2. Preserve exact Table Schemas for canonical CSV resources. Preserve every
   non-CSV resource as a generic opaque file with true-format metadata under
   the M9ae profile.
3. Bind both report and Frictionless serialization evidence when a report is
   described, and preserve Arrow evidence when Parquet is described.
4. Admit the combined profiles through verified creation, owned replacement,
   byte-preserving loading, canonical table inspection, typed hydration,
   report access, and offline stored-state inspection.
5. Continue rederiving `datapackage.json` from verified stored resources during
   loading. Do not invoke Python during ordinary package operations.
6. Keep default packages descriptor-free and keep every published profile
   non-replayable.
7. Mark the fixed-package M9 roadmap complete through M9af. Defer live refresh
   and procedural replay under ADR 0066 until a complete request recipe is
   available; do not represent that dependency as another code gate.

## Acceptance criteria

- Public retained-raw, Parquet, and report packages publish exact
  manifest-bound Frictionless descriptors.
- Combined manifest parsing preserves the underlying report and Parquet
  contracts as well as Frictionless evidence.
- Loading rederives descriptors for mixed profiles, and table, hydration,
  report, replacement, and stored-state APIs preserve the combined evidence.
- Existing opaque-resource CLI cases continue to pass pinned core
  Frictionless 5.19.0 validation with zero errors or warnings.
- Default behavior remains descriptor-free; no runtime Python, refresh,
  recipe execution, or replay authority is added.

## Consequences

- All currently fixed package resource families share one public opt-in
  Frictionless boundary.
- M9 fixed-package implementation is complete; further format additions are
  enhancements, not implied roadmap gates.
- Live refresh and procedural replay remain a documented dependency-bound
  follow-up, contingent on a complete request recipe.
