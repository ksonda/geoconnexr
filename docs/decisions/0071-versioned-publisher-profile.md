# ADR 0071: Publish one versioned JSON-LD and sitemap profile

- Status: Accepted public boundary
- Date: 2026-08-12
- Owners: package maintainers
- Related: ADR 0006, ADR 0018, ADR 0070

## Context

M10 calls for JSON-LD builder, validation, and sitemap tools. Existing parsing
is deliberately tolerant of current production differences, but publisher
output needs a narrower contract. Tightening the reader would reject evidence
that the package already supports, while leaving the writer unspecified would
make round trips and validation findings unstable.

The catalog contract already supplies bounded sites and dataset rows with
canonical identities, typed point geometry, provider provenance, and one row
per dataset, distribution, and variable combination. It is the available
publisher input boundary.

## Decision

1. Define publisher profile 1.0.0 as a local, network-free JSON-LD profile over
   exact catalog 0.1.0 site and dataset tables.
2. Require one explicit provider with a canonical IRI, name, and HTTP(S) URL.
   Any provider facts already present in the rows must agree with it.
3. Accept canonical location-type IRIs. Also accept `hydrometricStation` and
   `Unknown` as literals because both occur in the checked P0 evidence.
4. Require each dataset's rows to form a complete distribution by variable
   product before representing those dimensions as independent JSON-LD arrays.
5. Return validation findings with severity, JSON pointer, stable rule ID,
   profile version, message, and suggested fix. Warnings do not invalidate a
   document; errors do.
6. Write at most 50,000 canonical HTTP(S) URIs to one sitemap. Publish only to
   an absent destination through sibling staging and verify exact bytes after
   exposure.
7. Keep all three publisher operations offline. They do not submit profiles,
   notify search engines, fetch contexts, or overwrite existing directories.

## Acceptance criteria

- Built profiles expand and parse through the existing safe JSON-LD boundary.
- Sites, datasets, distributions, variables, provider facts, point geometry,
  and supported location types survive the build and parse round trip.
- Sparse distribution by variable products fail before serialization.
- Validation reports malformed JSON-LD, wrong profile identity, missing sites,
  unsupported location types, parser diagnostics, and incomplete records with
  stable structured findings.
- Sitemap output is deterministic, XML escapes URI text, enforces protocol
  count and byte limits, rejects overwrite, and verifies final bytes.

## Consequences

- M10 has one explicit publisher profile instead of implying that every
  production page already follows the same ideal shape.
- Publisher inputs remain tied to the experimental catalog contract. A future
  catalog contract requires a new publisher profile or an explicit adapter.
- Sitemap indexes, profile submission, and remote publication remain outside
  this offline boundary.
