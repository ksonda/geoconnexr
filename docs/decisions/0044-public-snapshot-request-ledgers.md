# ADR 0044: Publish verified snapshot request ledgers without catalog loading

- Status: Accepted public boundary
- Date: 2026-07-28
- Owners: package maintainers
- Related: ADR 0041, ADR 0042, ADR 0043

## Context

M9e now provides a complete typed request-export loading and binding substrate.
It recognizes only the fixed catalog writer profile, proves that
`requests.csv` is the canonical byte projection of the authoritative manifest
ledger, returns the exact typed request table, and verifies the complete tree
again afterward.

This capability is useful independently of broader resource loading. Publishing
it does not require reconstruction of redacted catalog identities, a
Frictionless profile, fetched or harmonized resources, or replay semantics.

## Decision

Export `gx_snapshot_requests(dir)` as the M9f public request-ledger accessor.

The function:

1. delegates to the unchanged M9e loader;
2. accepts only an existing snapshot with the exact M9b catalog and request
   export profiles;
3. returns validated `gx_snapshot_request_export` evidence containing the typed
   request table, normalized snapshot path, count, manifest and resource hashes,
   and a deterministic identity that binds all of those facts;
4. exposes a print method that describes canonical CSV-to-manifest binding and
   unsigned offline consistency; and
5. performs no network, DNS, cache, write, repair, refresh, catalog-resource
   loading, authenticity check, or replay.

The API is an accessor for recorded request evidence. It does not assert that
the historical requests occurred, that their endpoints or licences were
truthful, or that an unsigned snapshot resists coordinated replacement.

## Acceptance criteria

- Empty and populated fixed-profile snapshots return exact typed request rows.
- Public evidence revalidates its path, rows, counts, hashes, and deterministic
  identity.
- Path, request, hash, count, status, or identity forgery fails closed.
- Unknown profiles, noncanonical resource bytes, mutation, and loading budgets
  preserve M9e failures.
- The accessor invokes no network or write seam and leaves all snapshot bytes
  unchanged.
- No public catalog loader, Frictionless claim, authenticity claim, or replay
  API is introduced.

## Consequences

- Users can inspect a snapshot's typed request ledger without parsing CSV or
  trusting a schema inferred from file contents.
- The canonical request-export loading/binding roadmap prerequisite is public.
- Sites, datasets, and problems remain opaque verified resources.
- Broader catalog loading must define an honest redacted-view value object
  rather than reconstructing discarded live identities.
