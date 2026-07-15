# ADR 0014: Treat custom polygon AOIs as an offline replay boundary

- Status: Accepted experimental policy
- Date: 2026-07-13

## Context

M6 needs a portable way to record a user-supplied area of interest before any
catalog or provider work begins. Equivalent polygon geometries can arrive in
different coordinate reference systems, with different ring start positions or
directions, and with holes or multipolygon members in different orders. Storing
those inputs as received would make replay identities depend on incidental
serialization choices.

Accepting arbitrary simple-feature shapes would also blur the boundary between
an AOI definition and upstream hydrologic delineation. Invalid geometry repair,
mainstem-basin delineation, and point-upstream discovery each require separate
policies and provenance. The public catalog pipeline is not yet implemented or
authorized by this decision.

## Decision

Implement M6a as an offline custom-AOI boundary in `gx_aoi()`:

- Accept `type = "auto"` or `type = "sf"` for exactly one `sf` or `sfc` XY
  `POLYGON` or `MULTIPOLYGON` with an explicit CRS.
- Reject missing CRS, additional geometries or dimensions, empty or invalid
  geometry, non-finite coordinates, coordinates outside CRS84 longitude and
  latitude bounds after transformation, and inputs over 100,000 coordinate
  positions. Do not repair invalid geometry.
- Transform accepted spatial input to `OGC:CRS84`. Canonicalize every ring's
  coordinates to a nine-decimal-degree grid, apply GeoJSON exterior/hole
  winding, canonicalize every ring's start position, then deterministically
  order polygon holes and multipolygon members. Values within `1e-12` degrees
  of a half-grid boundary use the exact boundary and ties resolve toward positive
  infinity so ordinary projection noise does not choose adjacent cells. Reject
  geometry made invalid or zero-area by grid normalization. Reject rings that
  cross the antimeridian until an explicit pre-cutting policy can preserve
  GeoJSON semantics without silently choosing a world-spanning complement.
- Temporarily disable the PROJ network grid facility around transformation and
  restore its prior state afterward. A custom CRS must not turn AOI construction
  into an undisclosed grid download.
- Record canonical GeoJSON and the SHA-256 of portable little-endian WKB in the
  AOI recipe. Enforce an 8 MiB ceiling independently on the canonical GeoJSON
  and WKB representations.
- Perform no network, graph, catalog, mainstem-basin, point-upstream, or other
  provider work while constructing the AOI.

The recipe pipeline fields describe the intended replay boundary from `aoi` to
`catalog`; they do not assert that catalog discovery has run or that a catalog
exists. `mainstem_basin` and `point_upstream` remain outside the accepted
`gx_aoi()` type set. The public `gx_catalog()` interface remains gated.

## Consequences

- Ring rotation, ring direction, hole order, and multipolygon member order no
  longer create distinct recipe identities for the same accepted coordinate
  sequences. The declared coordinate grid also removes ordinary floating-point
  noise from equivalent CRS transformations; it is not a promise that distinct
  datum operations are semantically interchangeable.
- The canonical GeoJSON is the portable replay input, while the little-endian
  WKB digest is an integrity identity. Neither is evidence of catalog
  completeness or provider currentness.
- Callers must repair or otherwise resolve invalid geometry before calling
  `gx_aoi()`; the package does not make a silent topology choice.
- JSON Schema validation establishes only the serialized recipe's structural
  shape. No serialized-recipe replay reader exists yet; that future boundary
  must reconstruct geometry, enforce runtime budgets and canonical form, and
  recompute the WKB digest before trusting it.
- M6 is now partially implemented as M6a. Catalog assembly, upstream-derived
  AOIs, partial-provider diagnostics, and catalog replay acceptance remain open.

## Follow-up

ADR 0016 adds the internal, bounded M6b hydration boundary required here. It
reconstructs AOI-only recipes and independently re-establishes canonical
GeoJSON and WKB-digest integrity without authorizing full replay.
