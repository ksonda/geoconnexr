# ADR 0049: Serialize fixed package resources before filesystem publication

- Status: Accepted internal substrate
- Date: 2026-07-29
- Owners: package maintainers
- Related: ADR 0019, ADR 0034, ADR 0036, ADR 0048

## Context

M9j admits and revalidates exact catalog, fetched, and harmonized lineage but
deliberately produces no bytes. The next writer needs one deterministic
package-owned resource profile before staging, manifests, atomic publication,
loading, or optional formats can be specified.

Fetched results have two honest native representations. WQP, EDR, current USGS,
and OGC API Features retain the exact provider response body. Direct-CSV
results retain their exact validated character table but not the original
response bytes. Treating both cases as though the same original raw body were
available would overstate the M7 contract.

## Decision

Add the internal M9k `gx_package_resources_impl(input)` boundary and exact
`gx_package_resources` in-memory bundle.

The fixed profile contains:

1. the four canonical redacted catalog resources from M9b/M9j;
2. canonical `catalog/fetch_status.csv` and `data/native/index.csv` for fetched
   or harmonized stages;
3. one native resource per successful fetched result:
   - exact retained provider bytes under a deterministic `.bin` path when
     `raw_body_available = TRUE`; or
   - canonical quote-all UTF-8 LF CSV for strict direct-CSV result tables when
     no retained body exists;
4. canonical redacted `data/observations.csv` and
   `data/harmonized_resources.csv` for harmonized stages; and
5. an exact path-sorted resource table plus named raw-vector contents.

Every entry binds its portable path, role, format, media type, byte count,
SHA-256, dimensions, result identity, distribution identity, and handler.
Catalog bytes must reproduce the M9j resource hashes. Result paths use the
validated result index and result ID, and the native index binds every stored
result path and hash.

Serialization is limited to 10,000 resources, 128 MiB per resource, 256 MiB
aggregate, and five million CSV fields. The bundle declares deterministic
in-memory serialization but no write or publication. Arrow/Parquet, Quarto,
Frictionless metadata, manifests, loading, reports, refresh, and replay remain
deferred.

## Acceptance criteria

- Catalog, empty fetched, empty harmonized, and populated harmonized M9j inputs
  produce exact stage-specific resource sets.
- The populated supported-subset fixture produces four catalog resources, two
  fetch resources, seven native resources, and two harmonized resources.
- Retained provider response bodies are byte-identical to M7 evidence.
- Direct-CSV results without retained bodies serialize only as canonical
  quote-all UTF-8 LF CSV.
- Catalog resource hashes equal M9j evidence; all content lengths and SHA-256
  values equal the resource table.
- Paths are portable, deterministic, bytewise sorted, unique, and free of
  ASCII aliases.
- Input, resource metadata, content, bundle metadata, or bundle identity
  forgery fails closed.
- Serialization invokes no network, DNS, filesystem writer, removal,
  publication, optional-format, loading, report, refresh, or replay seam.
- No public API is added.

## Consequences

- The next package writer can stage already-determined bytes rather than
  interpreting live catalog/fetch/harmonization objects while mutating disk.
- The bundle preserves provider bytes only where M7 actually retained them and
  labels direct-CSV canonical tables honestly.
- The initial package profile is bounded and CSV/raw only; large observation
  sets above the in-memory profile must fail rather than consume unbounded
  memory.
- Manifest construction, closed-tree staging and publication, package loading,
  Arrow/Parquet, reports, Frictionless acceptance, refresh, and replay remain
  later M9 contracts.
