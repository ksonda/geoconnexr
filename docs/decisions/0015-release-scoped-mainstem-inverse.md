# ADR 0015: Invert the pinned mainstem lookup only within its release

- Status: Accepted internal substrate
- Date: 2026-07-13

## Context

The roadmap includes `gx_mainstem_to_comids()`, but ADR 0004 still leaves the
default mainstem collection, cross-vintage migration, and current/superseded
service behavior unresolved. Exporting that function now would make a
release-scoped membership result look like a current-service assertion.

The checksum-pinned `ref_rivers` v3.2 asset established by ADR 0009 has exact
`uri,comid` rows. Its COMIDs are unique, but a mainstem URI can own zero or
many COMIDs in that release. The generator selected mainstems that were not
superseded at the release; this says nothing about their state today. The
minimized six-row test fixture preserves observed rows but is not evidence of
complete production inverse groups.

## Decision

Add `gx_mainstem_to_comids_impl()` as an unexported M4c substrate over the
verified local lookup:

- accept only canonical Geoconnex mainstem HTTPS PIDs and preserve duplicate
  input order;
- allow an empty typed result without requiring an installed optional asset;
- for non-empty input, require the explicitly installed, receipt- and
  checksum-verified mapping and scan it in bounded chunks by URI;
- return every release-member COMID in deterministic radix order, with a
  one-based `match_index` per input, or one explicit not-found sentinel;
- retain character identifiers and exact mapping-release, source-commit,
  asset, checksum, installation, and verification provenance;
- mark matches `active_in_mapping_release` and attach
  `mainstem_currentness_not_checked`; define not-found only as absence from the
  selected mapping release;
- enforce input, aggregate unique-match, and duplicate-expanded output-row
  ceilings; and
- never download, install, refresh, repair, query the reference service, infer
  a new vintage, or migrate an identifier.

The scanner continues to perform the pinned schema, row-count, final-newline,
known-answer, and known-absence checks used by the COMID direction. The exact
verified SHA-256 binds the separately audited global uniqueness and cardinality
facts without materializing a 2.35-million-identifier in-memory index on every
scan; duplicate and declared forward-cardinality checks are repeated for rows
selected by a request. `complete = TRUE` means complete for the selected mapping
release and requested inputs only.

Do not export `gx_mainstem_to_comids()` yet. ADRs 0004 and 0008 still govern
the public currentness and migration contract.

## Consequences

- Internal HUC, point, and catalog work can compose against a deterministic
  zero-to-many release-membership primitive without contacting the network.
- Absence cannot be interpreted as an unknown, deleted, or superseded
  mainstem outside v3.2, and a matched URI cannot be described as current.
- Repeated inverse lookups scan the full optional CSV. A content-addressed
  index may optimize this later without changing the row contract.
- Fixture tests establish adapter behavior only. Production inverse
  completeness comes from the fully verified installed asset, not the
  minimized excerpt.
