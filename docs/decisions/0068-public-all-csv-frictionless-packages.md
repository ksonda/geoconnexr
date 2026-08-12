# ADR 0068: Publish Frictionless descriptors for fixed all-CSV packages

- Status: Accepted public boundary
- Date: 2026-08-10
- Owners: package maintainers
- Related: ADR 0049, ADR 0051, ADR 0057, ADR 0066, ADR 0067

## Context

M9ac derives a deterministic Frictionless Data Package v1 descriptor from one
exact in-memory package bundle and validates the fixed catalog, fetched, and
harmonized CSV profiles with pinned Python Frictionless CLI 5.19.0. It does not
publish that descriptor. The package writer, loader, typed inspection, owned
replacement, and offline replay-inspection boundaries already support closed,
manifest-declared resource trees.

Publishing the descriptor must not introduce a recursive self-description,
claim that the external CLI ran during package creation, change default package
bytes, or silently extend the proven profile to raw, Parquet, or report bytes.

## Decision

Add M9ad public, opt-in Frictionless integration for exact all-CSV packages.

1. Extend `gx_package()` with `frictionless = FALSE`. Preserve the existing
   default resource bundle and manifest shape when it is false.
2. When true, require CSV time-series storage, no report, and an exact finalized
   bundle whose resources are all canonical CSV. Reject the request before
   optional Arrow or Quarto work when its options are incompatible.
3. Derive `datapackage.json` from the base bundle only, then add the descriptor
   as one manifest-declared generic JSON metadata resource. The descriptor does
   not list itself or `manifest.json`.
4. Bind the base bundle identity, descriptor identity, exact descriptor bytes
   and SHA-256, Data Package v1 standard, pinned CLI profile, and explicit
   `runtime_cli_executed = FALSE` evidence in manifest serialization metadata.
5. Carry the profile through verified creation and owned replacement, including
   plain-to-described and described-to-plain transitions under the existing
   sibling-backup rollback contract.
6. On loading, rederive the full descriptor from the already verified CSV bytes
   and reject a descriptor that is merely checksum-consistent with a modified
   unsigned manifest. Keep the JSON bytes opaque to canonical table hydration.
7. Preserve `frictionless = TRUE` through byte loading, table inspection, typed
   hydration, and offline `gx_replay(refresh = FALSE)` evidence. This remains a
   compatibility claim, not authenticity or procedural replay authority.
8. Validate a publicly generated package with pinned Python Frictionless CLI
   5.19.0 in the dedicated CI workflow.

## Acceptance criteria

- Catalog, fetched, and harmonized all-CSV inputs publish one exact
  `datapackage.json` and retain complete closed-tree verification.
- Descriptor resources exactly match every non-descriptor stored path, byte
  length, SHA-256, and canonical all-string CSV schema.
- Default packages remain descriptor-free and keep their prior manifest shape.
- Loading rederives the descriptor; byte, manifest, metadata, identity, or
  semantically self-consistent unsigned forgery fails closed.
- Plain-to-described and described-to-plain replacement preserve prior/final
  evidence and rollback behavior.
- Parquet, reports, non-CSV finalized resources, invalid flags, refresh, and
  procedural replay remain rejected or unclaimed.
- A public generated package passes pinned Frictionless CLI 5.19.0 standards-v1
  validation without errors or warnings.

## Consequences

- Users can explicitly create portable Frictionless metadata for the exact
  fixed all-CSV package profile without an operation-time Python dependency.
- The manifest records CI profile evidence but truthfully states that package
  creation did not execute the CLI.
- End-to-end mixed-resource raw, Parquet, and report validation remains a later
  M9 gate. Live refresh remains blocked on a complete provider request recipe.
