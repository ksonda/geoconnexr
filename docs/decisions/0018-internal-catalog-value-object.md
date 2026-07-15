# ADR 0018: Separate the catalog value object from live discovery

- Status: Accepted internal substrate
- Date: 2026-07-15

## Context

The M6 roadmap calls for one catalog containing typed sites, flattened dataset
records, selected reference layers, problems, requests, and explicit
completeness. Existing protocol objects are not that contract. Location
profiles retain WKT and row diagnostics, reference features have
query-dependent columns and attribute metadata, and the current request
ledgers do not have unique physical-attempt identities or the manifest-v1
shape. Treating any of them as the catalog would freeze unresolved discovery,
merge, geometry, and provenance policies.

Catalog-only snapshot writing nevertheless needs a strict offline input. That
input must distinguish a complete bounded procedure from exhaustive knowledge
of mutable services, preserve partial failure, and be safe to revalidate
immediately before serialization.

## Decision

Add an unexported M6c `gx_catalog` value-object contract with exact top-level
members: contract version, AOI, sites, datasets, an empty reference list,
problems, requests, and metadata. Construction and validation perform no
network, cache, query, profile, reference, or filesystem work.

Sites are an exact CRS84 `sf` table with unique canonical site URIs and POINT
geometry; empty points are allowed. Datasets retain the established one-row-per
dataset-by-distribution-by-variable semantics, typed UTC intervals,
`conforms_to` arrays, fetchability, and foreign keys to sites, but row
diagnostics are adapted into the separate problem table. Reference layers must
remain an exact empty list in contract 0.1.0 because their portable schema,
field-level provenance, and deterministic GeoPackage representation are not
settled.

Problems record stage, redacted-safe source identity, diagnostic path, code,
severity, recoverability, and UTC occurrence time. Requests use the
manifest-v1 attempt shape and require unique row identities; legacy cache-key
request IDs are not silently treated as unique retry attempts. Metadata has an
exact selection, completeness, count, endpoint, hydrologic-vintage, and source-
contract shape.

Completeness is procedural. Counts must reconcile, truncation and known
failures are sticky, and a selected stage can be `complete` only when its
bounded declared procedure has no failed or skipped inputs and was not
truncated. A graph document that parsed successfully or a reference-gage
fallback cannot be promoted into complete catalog discovery.

Fixed cardinality, scalar/aggregate text, geometry, and list-entry budgets are
validated before expensive traversal. Aggregate text accounting short-circuits
before URI and geometry validation, and completeness arithmetic uses bounded
non-overflowing counts. Invalid inputs, nonrecoverable problems,
identity corruption, and count disagreements abort with redacted
`gx_error_catalog` conditions. Deterministic export views sort by UTF-8 bytes,
format UTC timestamps explicitly, encode arrays as compact JSON, and redact
absolute-URI userinfo plus query/fragment values across every URI scheme without
mutating the in-memory catalog. Because redaction can collapse distinct
identities, exports add namespace/version-bound SHA-256 columns for site URIs
in both site and dataset views and for variable IDs in dataset views. The
shared site fingerprint, not a redacted display URI, is the portable join key.

Do not export `gx_catalog()` or the internal constructor in this slice. A
future live orchestrator must adapt its evidence into this value object only
after graph paging, merge precedence, canonical-site clustering, and provider
fallback contracts close.

## Consequences

- Snapshot creation can accept one revalidated, bounded catalog without
  claiming that live catalog discovery is implemented.
- Protocol result classes remain independently evolvable and require explicit
  adapters, preventing accidental completeness upgrades or lossy retry merges.
- Typed site geometry is available in memory while the deterministic snapshot
  view can use portable WKT text; GeoPackage remains a later compatibility
  decision.
- Nonempty reference layers, field-level provenance, upstream AOI resolution,
  and public catalog orchestration remain gated.
