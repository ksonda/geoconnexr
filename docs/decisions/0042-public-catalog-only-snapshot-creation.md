# ADR 0042: Publish creation-only catalog snapshots

- Status: Accepted experimental policy
- Date: 2026-07-28
- Owners: package maintainers
- Related: ADR 0018, ADR 0019, ADR 0041

## Context

M9b already writes deterministic catalog-only snapshots through a verified
sibling staging tree, but deliberately kept that substrate internal while
overwrite, fetched resources, Frictionless metadata, loading, reports, and
replay were unresolved. M9c now exposes exact offline verification evidence.

The catalog-only writer's creation semantics are independently complete:
catalog input is revalidated, sensitive URI components are redacted, four
fixed CSV resources and manifest-v1 are deterministic, existing destinations
are preserved, pre-publication failures clean owned staging content, and both
staged and final trees pass the closed-tree verifier. None of this requires the
broader package or replay contracts.

## Decision

Export `gx_snapshot(x, dir, fetch = FALSE, report = FALSE, overwrite = FALSE)`
for validated `gx_catalog` inputs only.

The public boundary:

1. accepts one exact catalog and one absent destination under an existing safe
   parent;
2. requires `fetch`, `report`, and `overwrite` to be explicit scalar `FALSE`
   values before staging begins;
3. delegates serialization and atomic exposure to the unchanged M9b writer;
4. returns an exact `gx_snapshot` object with normalized absolute path, final
   `gx_snapshot_verification` evidence, catalog-only scope metadata, and a
   deterministic snapshot identity;
5. revalidates that the final manifest ends at the catalog stage, is
   non-replayable, and declares exactly the four verified M9b resources; and
6. performs no DNS, network, discovery, cache, optional-package, fetch,
   harmonization, report, loading, repair, refresh, or overwrite work.

The result metadata explicitly records that Frictionless compatibility,
fetching, harmonization, reports, overwrite, and replay are false. The function
does not export the internal writer or serializer seams.

## Acceptance criteria

- Empty and populated catalogs publish only the four fixed CSV resources and
  `manifest.json`, with staged and final closed-tree verification.
- The public result exactly binds its normalized path, final manifest hash,
  resource/request counts, and catalog-only scope.
- Result, embedded verification, metadata, status, or identity forgery fails.
- Any true or malformed scope flag and any non-catalog input fails before
  staging.
- Existing destinations, links, unsafe parents, and injected writer failures
  preserve the M9b failure and cleanup behavior.
- Blocked external-work seams are never invoked.

## Consequences

- Users can create and verify a useful offline catalog snapshot through public
  APIs without waiting for the full packaging roadmap.
- The snapshot is not a Frictionless data package and is not loadable or
  replayable by this contract.
- Fetched/harmonized resources cannot be added because the frozen M7 result
  omits full source AOI/catalog context required by manifest-v1. That context
  decision remains separate rather than being inferred or reconstructed.
- General `gx_package()`, fetched/harmonized `gx_snapshot()`, reports,
  overwrite, loading, signing, refresh, and replay remain later work.
