# ADR 0050: Publish fixed package resources through verified sibling staging

- Status: Accepted internal substrate
- Date: 2026-07-29
- Owners: package maintainers
- Related: ADR 0017, ADR 0019, ADR 0042, ADR 0048, ADR 0049

## Context

M9k determines and validates the exact bytes for catalog, fetched, and
harmonized package resources, but deliberately performs no filesystem work.
The next boundary must publish those bytes without interpreting live upstream
objects during writes, exposing a partial tree, overwriting an existing
destination, or claiming loading and replay semantics that do not yet exist.

The existing manifest-v1 verifier can describe a pipeline ending at `package`
and can verify the resulting resource tree. The M9k fetched object does not,
however, contain a complete provider request ledger: its exact catalog request
export is authoritative only for discovery. A package manifest must preserve
that limitation rather than implying replayable fetch provenance.

## Decision

Add the internal M9l `gx_package_write_impl(bundle, dir)` boundary and exact
`gx_package_publication` evidence object.

The writer:

1. accepts and revalidates only an exact M9k resource bundle;
2. requires an absent destination and creates a private sibling staging tree;
3. writes the bundle's already-determined bytes plus one deterministic
   manifest-v1 document;
4. verifies the closed staging tree through M9a and requires its resource
   paths, sizes, and SHA-256 values to equal the M9k bundle;
5. rechecks the destination parent and destination absence immediately before
   one atomic rename;
6. verifies the exposed tree again and requires the manifest digest to equal
   the staged digest; and
7. removes only its owned staging tree after a pre-publication failure. Once
   the rename exposes the target, a later verification failure never deletes
   that path.

The manifest ends the recipe at `package`, declares the exact M9k source stage,
resource and bundle identities, CSV/raw serialization profiles, selected fetch
handler order, harmonization target units where applicable, and package-stage
completeness. It marks the request ledger as `catalog_only`, the package as
non-replayable, and fetched packages as lacking a complete fetch request
ledger. It makes no Frictionless, Arrow/Parquet, Quarto, report, refresh,
authenticity, or replay claim.

The resulting internal publication object binds the normalized destination,
source stage, exact M9k bundle, final M9a verification, fixed creation-only
metadata, and a deterministic publication identity. No public API or overwrite
mode is added.

## Acceptance criteria

- Catalog and dry-run harmonized bundles publish their exact bytes with a
  deterministic manifest and pass staged and final M9a verification.
- Manifest recipe, serialization, resource, completeness, and non-replayable
  evidence agree with the exact M9k bundle and its lineage.
- Equivalent bundles produce byte-identical trees apart from destination-bound
  publication evidence, which is not stored in the tree.
- Existing destinations are preserved; injected write, staged-verification,
  and rename failures remove only the owned staging tree.
- A final verification failure leaves the already-exposed target untouched.
- Publication-object forgery and stored-resource mutation fail closed.
- Publication invokes no network, DNS, cache, provider, optional-format,
  loading, report, refresh, or replay seam.
- The writer and publication object remain internal.

## Consequences

- Exact catalog, fetched, and harmonized M9k bundles now have one verified
  creation-only filesystem publication path.
- Publication reuses the hardened closed-tree verifier instead of establishing
  a second integrity model.
- The stored request ledger remains honestly catalog-only, so the package is
  useful as integrity-checked data evidence but is not yet a replay recipe.
- Public package creation/loading, overwrite ownership, Frictionless metadata
  and validation, optional formats, reports, refresh, and replay remain later
  M9 contracts.
