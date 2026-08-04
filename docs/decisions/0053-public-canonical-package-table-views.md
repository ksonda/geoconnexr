# ADR 0053: Parse verified package CSVs as canonical character tables

- Status: Accepted public boundary
- Date: 2026-07-30
- Owners: package maintainers
- Related: ADR 0045, ADR 0047, ADR 0049, ADR 0051, ADR 0052

## Context

M9n loads every fixed-profile package resource as exact manifest-bound bytes.
That boundary intentionally leaves CSVs opaque. Users need a supported table
view, but live-object hydration would require additional contracts for typing,
redacted identity, provider-specific native schemas, lineage reconstruction,
and replay authority.

The package writer already emits every package-owned CSV through one canonical
quote-all UTF-8/LF serializer. The bounded bytewise parser used by snapshot
loading can therefore provide a narrower lossless table boundary without
inferring semantics or reconstructing workflow objects.

Overwrite remains separately gated: ADR 0019 requires an ownership marker and
rollback/recovery contract before an existing destination can be replaced.

## Decision

Export `gx_package_tables(dir)` as the M9o canonical package-table view.

The function:

1. delegates closed-tree verification and exact resource reads to
   `gx_package_load()`;
2. parses every resource declared by the fixed package profile as CSV from its
   already verified in-memory bytes;
3. applies the M9k 128 MiB per-resource, 128-column, and five-million-field
   ceilings through the strict bytewise parser;
4. requires every table to round-trip exactly to its original quote-all
   UTF-8/LF bytes;
5. returns a `gx_package_tables` value object containing the embedded M9n
   evidence, path-named character-only tibbles, aggregate table metadata, and
   an identity binding every path, resource digest, row count, and column
   count; and
6. leaves native raw resources opaque in the embedded byte-preserving load.

The boundary is offline, read-only, unsigned, non-Frictionless, and
non-replayable. It does not infer types, reconstruct `gx_catalog`,
`gx_fetched`, or `gx_harmonized`, authenticate provenance, write, overwrite,
repair, refresh, or replay.

## Acceptance criteria

- Catalog, fetched, and harmonized packages expose every fixed CSV resource as
  one path-named character-only tibble.
- Every table serializes byte-for-byte to the exact M9n resource content.
- UTF-8 values are preserved independently of the process locale.
- Unquoted, BOM-marked, CRLF, malformed, or otherwise noncanonical CSV bytes
  fail closed.
- Snapshot profiles, corrupted packages, and forged path, stage, load,
  table, metadata, or identity evidence fail closed.
- Table loading performs no network, DNS, cache, optional-package, writer,
  overwrite, report, refresh, or replay work.
- Only `gx_package_tables()` and its print method are added to the public
  surface.

## Consequences

- Users can inspect deterministic package CSVs as ordinary tibbles while
  retaining exact byte and manifest evidence.
- Character-only parsing avoids silent type inference and preserves redacted
  and provider-specific values exactly.
- Typed package hydration, overwrite ownership, Arrow/Parquet, Quarto,
  Frictionless validation, reports, authenticity, refresh, and replay remain
  later M9 work.
