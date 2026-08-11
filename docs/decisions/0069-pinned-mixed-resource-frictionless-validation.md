# ADR 0069: Validate fixed mixed resources as opaque Frictionless files

- Status: Accepted internal validation gate
- Date: 2026-08-10
- Owners: package maintainers
- Related: ADR 0059, ADR 0060, ADR 0064, ADR 0067, ADR 0068

## Context

M9ac describes non-CSV package resources as generic Frictionless Data
Resources, while M9ad publishes only the proven all-CSV profile. The remaining
fixed package bundles can contain retained provider bodies, Arrow/Parquet
observations, or verified report HTML. Those bytes are deliberately opaque to
the Frictionless boundary: their package-specific contracts already perform
the reviewed semantic validation.

Using a non-CSV resource's native extension as its Frictionless `format` causes
the CLI to dispatch optional semantic parsers. That exceeds this boundary and
is not reliable evidence of opaque byte compatibility. In particular,
Frictionless 5.19.0's Parquet adapter does not report a byte count for the
otherwise valid fixed Parquet resource, so its standard byte-count check fails
even when the exact file digest and length are correct.

## Decision

Add M9ae as an internal mixed-resource CLI validation gate.

1. Keep canonical package CSVs as `tabular-data-resource` entries with exact
   all-string schemas.
2. Describe every other resource as a generic `data-resource` with
   `format = "bin"`. Preserve its normalized media type and exact path, byte
   length, and SHA-256 digest.
3. Add a custom `geoconnexr` descriptor block containing the true extension in
   `format` and `validation = "opaque-file-v1"`. This records that `bin` is a
   validation transport profile, not a claim that the underlying format is
   unknown.
4. Bump the internal descriptor contract to 0.2.0 and continue exact descriptor
   round-trip, metadata, and identity validation.
5. In the dedicated CI job, generate real fixed bundles containing retained
   raw provider bodies, Arrow/Parquet observations, and verified report HTML.
   Validate all three with core Python Frictionless CLI 5.19.0 in standards-v1
   mode and require every opaque task to be handled as a file.
6. Do not install or invoke Python format extras. Frictionless validates only
   closed-file structure, byte length, and digest for opaque resources; the R
   package's existing fixed profiles retain semantic authority.
7. Keep this gate internal. Public mixed-resource descriptor publication,
   refresh, and procedural replay remain out of scope.

## Acceptance criteria

- Retained raw bodies, fixed Parquet observations, and fixed report HTML are
  represented as opaque binary Data Resources whose custom metadata retains
  their true extension.
- Descriptor paths, sizes, hashes, media types, true formats, and opaque-file
  markers rebind the exact validated bundle; forgery continues to fail closed.
- A populated raw package, a real Arrow/Parquet package, and a verified report
  package pass core Frictionless CLI 5.19.0 standards-v1 validation with no
  errors or warnings.
- The CLI reports each non-CSV resource as a file task, not a parsed table.
- Construction and normal package operations do not execute Python or add an
  operation-time dependency.
- The public M9ad profile remains restricted to finalized all-CSV bundles.

## Consequences

- Every currently fixed resource family now has pinned end-to-end Frictionless
  CLI acceptance at its intended semantic depth.
- The validation job requires the reviewed R Arrow dependency to generate the
  real fixed Parquet fixture, but Python Parquet or HTML parsers are unnecessary.
- The next M9 gate can focus on safe public composition of already validated
  raw, Parquet, and report descriptors rather than discovering CLI behavior at
  the API boundary.
