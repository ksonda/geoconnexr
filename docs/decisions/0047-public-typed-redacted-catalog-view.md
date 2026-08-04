# ADR 0047: Publish typed redacted snapshot views without live reconstruction

- Status: Accepted public boundary
- Date: 2026-07-29
- Owners: package maintainers
- Related: ADR 0041, ADR 0044, ADR 0045, ADR 0046

## Context

M9h provides an exact typed view over the complete fixed catalog-only snapshot.
It retains canonical character-table evidence, re-derives every typed
projection during validation, preserves blank and redacted strings, and
explicitly denies live catalog reconstruction and replay.

Users need a supported way to inspect those typed sites, datasets, problems,
and requests. A broad loader name could overstate the result by implying that
the lossy redacted export can become a live `gx_catalog` or authorize replay.

## Decision

Export the narrow M9i `gx_snapshot_catalog_view(dir)` boundary as an exact
validating wrapper over M9h.

The public boundary:

1. recognizes only the fixed catalog-only writer and loading profiles;
2. returns the exact `gx_snapshot_catalog_view` value object, including final
   verification, canonical character evidence, typed tables, scope metadata,
   and view identity;
3. preserves M9h's before-and-after closed-tree verification, loading budgets,
   type restrictions, and whole-object revalidation;
4. names and prints the result as a typed redacted snapshot view rather than a
   live catalog; and
5. remains offline, read-only, unsigned, non-replayable, and non-Frictionless.

It does not reconstruct discarded URI components, distinguish original
missing values from exported blanks, return a `gx_catalog`, authenticate the
manifest, load fetched or harmonized resources, write, repair, refresh, or
replay.

## Acceptance criteria

- Empty and populated fixed-profile snapshots return the exact validated M9h
  value object through the public function.
- Public results expose typed `sf` sites and typed dataset, problem, and
  request tibbles while retaining their canonical evidence.
- The final manifest hash agrees with independent public verification.
- Path, typed data, raw evidence, metadata, hash, count, status, or view
  identity forgery continues to fail closed.
- Public loading invokes no network, DNS, cache, writer, removal, repair,
  refresh, or replay seam.
- Documentation and print output explicitly state that the result is redacted,
  non-replayable, and not a live `gx_catalog`.

## Consequences

- Catalog-only snapshots now have a supported public inspection path without
  broadening their reproducibility or identity claims.
- Consumers can work with exact spatial, temporal, logical, and JSON-array
  types while retaining the canonical stored evidence.
- A future live-catalog hydration contract, if justified, must use a distinct
  API and identity model.
- Fetched and harmonized package loading, Frictionless acceptance,
  authenticity, reports, refresh, and replay remain separate roadmap work.
