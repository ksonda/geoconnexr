# ADR 0059: Freeze an internal in-memory Arrow Parquet profile

- Status: Accepted internal substrate
- Date: 2026-08-03
- Owners: package maintainers
- Related: ADR 0049, ADR 0051, ADR 0058
- Supersedes: the Arrow-unpinned portion of ADR 0058

## Context

M9t can safely observe whether Arrow is installed, but deliberately blocks
serialization because its minimum version, required symbols, writer controls,
and read-back semantics were not reviewed. Enabling Parquet directly in the
public package profile would also require coordinated bundle, manifest,
loading, table-view, hydration, replacement, and compatibility changes.

Arrow R 14.x documents in-memory output streams, raw-vector Parquet reads, and
the writer controls needed to avoid optional codec and dictionary/statistics
defaults. Byte identity is still an Arrow-version-scoped property: this
decision does not claim identical files across different Arrow releases.

## Decision

Add the internal M9u `gx_package_parquet_impl()` boundary for one harmonized
observation table.

1. Pin Arrow R `>= 14.0.0` in `Suggests` and require the exported
   `write_parquet`, `read_parquet`, and `BufferOutputStream` capabilities with
   their reviewed formals.
2. Safely inspect installed metadata before loading Arrow. Immediately after
   namespace loading, require the loaded version to equal the inspected
   version and re-resolve every required export.
3. Revalidate the complete `gx_harmonized` source, select its exact package-
   owned observation schema, preserve integer/double/logical/UTC timestamp
   types, and apply the existing URI/URL redaction rule.
4. Serialize only through `BufferOutputStream` with Parquet 2.4, uncompressed
   pages, dictionaries and statistics disabled, a 1 MiB data page, a fixed
   whole-table chunk size, microsecond timestamp coercion, no deprecated INT96,
   and no timestamp truncation.
5. Cap output at 128 MiB, require both `PAR1` markers, read the exact raw vector
   back in memory, and require exact typed table equality.
6. Bind the source, redacted table, bytes, Arrow version, complete writer
   profile, SHA-256, dimensions, limitations, and identity in a fully
   revalidated internal value object.
7. Claim deterministic bytes only within the same resolved Arrow version.
   Keep cross-version byte stability, filesystem writing, bundle integration,
   public exposure, loading, hydration, and replay false or deferred.

M9t now classifies Arrow against the reviewed minimum but remains advisory:
`version_satisfied` leads only to `blocked_symbols_unchecked`. The M9u
capability boundary is the first place that loads and verifies Arrow symbols.
Quarto remains installed-but-unpinned and blocked.

## Acceptance criteria

- Missing and too-old Arrow installations fail before namespace loading.
- Metadata/namespace version races and missing or changed symbols fail closed.
- Empty and populated exact typed observation tables serialize in memory,
  read back exactly, and repeat byte-for-byte under one Arrow version.
- URI/URL values are redacted without changing storage types.
- Source, table, byte, metadata, or identity forgery fails whole-object
  validation.
- No network, DNS, cache, filesystem publication, bundle mutation, public API,
  report, refresh, or replay work occurs.
- Existing deterministic CSV/raw package creation and loading are unchanged.

## Consequences

- The optional Parquet serializer now has a reviewed executable substrate and
  honest host/version scope.
- Public `gx_package(timeseries = "parquet")` remains rejected until the
  package resource, manifest, load, table, hydrate, and replacement contracts
  admit the format together.
- Quarto/report capability review and Frictionless CLI validation remain later
  M9 checkpoints.

## Follow-up

[ADR 0060](0060-public-verified-parquet-packages.md) supersedes the public
Parquet deferral, and [ADR 0061](0061-reviewed-quarto-runtime-capability.md)
closes the separate Quarto R package review while leaving CLI admission and
rendering deferred.
