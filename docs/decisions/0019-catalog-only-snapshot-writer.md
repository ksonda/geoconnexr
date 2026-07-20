# ADR 0019: Publish catalog-only snapshots through a verified staging tree

- Status: Accepted internal substrate
- Date: 2026-07-15

## Context

M9a can verify a closed snapshot tree and its unsigned manifest entirely
offline, but it deliberately does not write or interpret resources. Catalog-
only packaging is the next useful vertical boundary. Public `gx_package()` and
`gx_snapshot()` still depend on unresolved overwrite ownership, Frictionless,
fetch, harmonization, report, loading, and recipe-replay semantics.

A writer must not expose exact provenance URLs containing credentials or query
values, serialize list columns through implementation-dependent defaults, or
leave a partially written destination that appears complete. The M9a verifier
also rejects undeclared files, empty undeclared directories, links, aliases,
and special entries.

## Decision

Add an unexported M9b catalog-only writer that accepts only a successfully
revalidated internal `gx_catalog`. It is creation-only: the destination and
any link at that literal path must be absent, its existing parent must be a
regular non-symlink directory, and the writer never replaces or repairs an
existing path.

The writer creates a uniquely named staging directory beside the destination,
writes four fixed UTF-8/LF, quote-all CSV resources, and writes
`manifest.json` last:

```text
catalog/sites.csv
catalog/datasets.csv
catalog/problems.csv
requests.csv
manifest.json
```

All resource projections are package-owned and deterministic. CRS84 site
geometry is exported as WKT; arrays use compact JSON; UTC values use RFC 3339;
URI-bearing provenance is redacted across schemes; and stable identity
fingerprints preserve joins that display redaction could otherwise collapse.
The catalog request table drives both
the embedded manifest ledger and `requests.csv`; optional sensitive final URLs
are omitted rather than writing redaction markers where manifest-v1 requires a
URI.

Resource sizes and SHA-256 values, package/session facts, effective catalog
selection, endpoint and asset hashes, AOI recipe, hydrologic vintage, and
stage completeness are assembled into manifest-v1. The manifest explicitly
marks itself non-replayable as a catalog-only internal writer result. It does
not claim request authenticity, full logical-to-physical ledger adaptation,
Frictionless compatibility, or resource-loading semantics.

Before publication, the writer parses and runtime-validates the exact manifest
bytes and runs M9a over the closed staging tree. It then renames the verified
staging directory within the same parent and verifies the final tree again.
Failures clean up only a still-owned staging tree. All writer failures inherit
`gx_error_snapshot`; messages and traces do not echo paths, URLs, or digests.
Path-bearing filesystem warnings are muffled and translated into those generic
typed failures; failure to remove an owned stage is itself reported.

The local-filesystem boundary uses pre/post type and existence checks but, like
M9a, cannot provide a hostile-filesystem `renameat2(RENAME_NOREPLACE)` or
`openat` guarantee from portable R. Public overwrite support remains gated on
an ownership marker and rollback/recovery contract.

## Consequences

- A catalog can now make a deterministic, offline-verifiable closed snapshot
  without fetch handlers, Arrow, Quarto, or a public replay API.
- No partial staging content is intentionally published, and arbitrary
  existing destinations are never overwritten.
- CSV is the contracted baseline for this internal slice; deterministic
  GeoPackage and Frictionless data-package behavior remain open.
- Successful verification proves internal consistency relative to an unsigned
  manifest, not authenticity, historical truth, currentness, or recipe
  replayability.
