# ADR 0066: Inspect fixed stored state without executing replay

- Status: Accepted public boundary
- Date: 2026-08-09
- Owners: package maintainers
- Related: ADR 0041, ADR 0047, ADR 0052, ADR 0055, ADR 0065
- Supersedes: the `refresh = FALSE` public-loading gate in ADR 0017

## Context

The fixed catalog-snapshot and package profiles now have complete public
offline inspection chains. Catalog snapshots can be verified and loaded as a
typed redacted view. Packages can be verified, byte-loaded, parsed into
canonical tables, hydrated with fixed types, and optionally expose one
verified stored report. These contracts do not reconstruct the original live
workflow objects and every stored manifest remains explicitly non-replayable.

The roadmap also reserves `gx_replay()` for both stored-state inspection and
future recipe refresh. Publishing the offline branch must not imply that
procedural replay, destination publication, authenticity, or arbitrary
manifest-v1 loading has been authorized.

## Decision

Add M9ab public offline stored-state inspection.

1. Export `gx_replay(manifest, dir = NULL, refresh = FALSE, ...)` only for the
   fixed catalog snapshot writer and fixed public package profiles.
2. Accept a validated snapshot/package result, validated loaded package,
   existing source directory, or its exact `manifest.json` path. Do not accept
   verification evidence alone because it does not carry an owned source path.
3. Reject `refresh = TRUE` before inspecting the source. Require `dir = NULL`
   and empty `...`; destination publication and replay options remain gated.
4. Reuse `gx_snapshot_catalog_view()` for catalog snapshots and
   `gx_package_hydrate()` for packages. Include read-only `gx_report()` evidence
   when the fixed package profile declares a report. Never invoke Quarto while
   inspecting a stored report.
5. Bind the result to the current closed tree, manifest hash, typed-view
   identity, optional report identity, source kind, and source stage. Ignore
   verification timestamps only when comparing otherwise exact verification
   evidence from repeated passes.
6. State explicitly that the result is offline, read-only, unsigned,
   non-Frictionless, non-replayable, and did not execute a recipe. Preserve
   historical-request-truth, authenticity, and coordinated-replacement
   limitations.
7. Reject arbitrary verified manifest-v1 trees, optional-resource profiles,
   malformed sources, filesystem mutation, and forged result evidence.

## Acceptance criteria

- Fixed catalog snapshots load as exact typed catalog views from either a
  publication object, directory, or exact manifest path.
- Catalog, fetched, and harmonized packages load as exact typed package views;
  fixed report packages additionally include verified stored HTML evidence
  without invoking Quarto.
- Refresh, destination, and extra-option requests fail before source loading or
  external work.
- Inspection invokes no network, DNS, cache, writer, report runtime, repair,
  or workflow execution seam.
- Current tree mutation, arbitrary profiles, metadata/view/identity forgery,
  and verification divergence fail closed.

## Consequences

- The public name `gx_replay()` now has a deliberately bounded offline branch,
  but its result remains `replayable = FALSE`: it is verified stored-state
  inspection, not recipe replay.
- Users receive one common evidence envelope around the existing exact typed
  inspection APIs without losing their underlying view contracts.
- Live `refresh = TRUE` procedure execution and full Frictionless CLI
  validation remain separate M9 gates.
