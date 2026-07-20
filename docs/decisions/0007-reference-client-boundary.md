# ADR 0007: Use a typed, bounded native reference client

- Status: Accepted for the experimental M3 protocol slice
- Date: 2026-07-13
- Owner: ksonda

## Context

The Geoconnex reference service exposes an OGC API Features surface, but its
collection schemas, identity-property names, pagination, representation
negotiation, and item-route behavior vary. Unknown query parameters can be
ignored by an upstream implementation, a large legacy-mainstem item route can
fail while a bounded property filter succeeds, and negotiated JSON-FG differs
from the classic GeoJSON contract expected by this package. Provider-controlled
links must also remain inside the HTTP safety boundary established by ADR 0005.

## Decision

- The package-owned native client is the canonical reference backend. It
  discovers collections and queryables instead of freezing the live inventory.
- Item-list requests set `f=json`. Simple equality filters are sent only after
  their names, scalar shapes, advertised JSON types, enum values, and OGC roles
  have been validated against the collection queryables schema. Primary
  geometry and temporal roles are not accepted as property filters.
- Results are typed `sf` tables with a versioned contract. Top-level feature
  IDs, `x-ogc-role: id` properties, and hydrologic identifiers remain character
  values even when the service advertises or emits numeric values.
- Redirects and `rel=next` links must retain the configured endpoint origin and
  base path. Pagination stops at the requested limit, `numberMatched`, an empty
  page, a missing or repeated next link, or a page/byte ceiling. Incomplete
  results carry visible diagnostics and `truncated = TRUE`.
- Unfiltered collection retrieval requires `allow_unbounded = TRUE`; the opt-in
  does not disable row, page, per-response, JSON-complexity, or cumulative-byte
  limits.
- Single-feature retrieval tries the item route, then exactly one queryable
  marked `x-ogc-role: id`, then direct JSON-LD negotiation. Every successful
  route must match the requested identity. Speculative item and JSON-LD probes
  disable transport retries so the ordered stage-attempt ledger remains exact;
  a separate redacted ledger records completed responses. A JSON-LD result is
  marked incomplete because it may contain fewer properties.
- The observed `mainstems_v3` JSON-LD identity is not silently rewritten to the
  legacy `mainstems` identity. A vintage or alias policy remains a separate P0
  product decision.
- ADR 0005's cache policy remains authoritative. Query-bearing feature
  responses are not persisted, so offline replay of an uncached filtered
  request reports a cache miss rather than implying snapshot support.

## Consequences

- Feature retrieval performs a queryables request so zero-row results and
  successful item responses retain advertised types and identity roles.
- Servers that omit queryables, return contradictory identity values, escape
  the configured endpoint, or advertise unsupported filter types fail closed.
- Exact collection contents remain live and mutable; minimized, hash-pinned
  schema fixtures, deterministic protocol tests, and bounded opt-in smoke tests
  protect the package contract without treating a checked inventory as
  permanent. Cross-vintage and full large-geometry evidence remain open under
  ADR 0004.
- The API remains experimental during P0 and does not resolve the mainstem
  vintage release gate.

## Follow-up

ADR 0010 leaves the speculative item and direct JSON-LD probes single-attempt,
but expands the redacted request ledger to include transport-error attempts and
enables attempt-aware retries for queryables and the bounded identity filter.
