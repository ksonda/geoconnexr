# ADR 0046: Type redacted snapshot catalogs without reconstructing live identity

- Status: Accepted internal substrate
- Date: 2026-07-28
- Owners: package maintainers
- Related: ADR 0042, ADR 0044, ADR 0045

## Context

M9g proves that each fixed catalog CSV is a canonical character-table
projection of its verified bytes. Those raw tables are safe to inspect, but
common fields such as geometry, times, logicals, and conforms-to arrays remain
encoded as text.

Typing those fixed fields is useful, provided the result remains explicitly a
redacted snapshot view rather than a reconstructed live `gx_catalog`. Blank
cells cannot be reversed to their original missing-versus-empty state, and
redacted URIs cannot recover discarded identity components.

## Decision

Add the internal M9h `gx_snapshot_load_catalog_view_impl(dir)` boundary and
exact `gx_snapshot_catalog_view` value object.

The boundary:

1. embeds final M9c verification, all three M9g resource-evidence objects, and
   M9f request evidence from the same normalized path and manifest hash;
2. converts only fixed writer-declared fields:
   - site `geometry_wkt` to exact bounded CRS84 `POINT` geometry;
   - dataset start/end strings to UTC `POSIXct`;
   - dataset `conforms_to` text to canonical ordered JSON string arrays;
   - dataset `fetchable` and problem `recoverable` to logical values; and
   - problem occurrence strings to UTC `POSIXct`;
3. preserves all other redacted strings and every blank cell unchanged;
4. retains the original canonical character tables as resource evidence;
5. re-derives every typed projection during whole-object validation; and
6. binds path, manifest, resource table identities, request-export identity,
   counts, scope metadata, and a deterministic view identity.

The object declares `redacted_catalog_view_v1`,
`preserved_as_empty_strings`, `redacted_values_not_reconstructed`, and
`replayable = FALSE`. It is not a `gx_catalog`, does not restore secrets or
missing values, and performs no network, write, repair, refresh, authenticity,
Frictionless, or replay work.

## Acceptance criteria

- Empty and populated snapshots produce exact `sf` sites and typed dataset,
  problem, and request tibbles.
- Geometry is exact round-trip CRS84 point WKT; timestamps are exact writer UTC
  instants; logicals accept only `true`/`false`; conforms-to values are exact
  canonical JSON arrays.
- Redacted query markers and blank cells remain visible, while discarded
  secret text never reappears.
- A canonical rehashed CSV with an invalid typed field fails at the typed-view
  boundary even though M9g accepts its character representation.
- Typed tables, raw evidence, request rows, metadata, path, hashes, counts,
  status, or identity forgery fails closed.
- Loading remains offline and read-only, and no public API is added.

## Consequences

- The complete catalog-only snapshot now has an honest typed loading model.
- Public exposure can wrap this exact value object without implying live
  catalog reconstruction.
- Consumers can use spatial and temporal types while retaining the source
  character evidence that produced them.
- General `gx_catalog` hydration, Frictionless loading, replay, and
  fetched/harmonized resource loading remain separate contracts.
