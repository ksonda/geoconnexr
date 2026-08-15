# ADR 0083: Publish bounded HUC12 mainstem intersections

- Status: Accepted public boundary
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the intersection implementation gate in ADR 0082

## Context

ADR 0082 selected a ranking contract over reference HUC12 polygons and
`mainstems_v3` geometry. The outlet method already occupies the public
`gx_huc12_to_mainstem()` boundary but uses NLDI, returns pour-point membership,
and has different cardinality. Intersection results need their own typed shape
and evidence while remaining available through the documented `method`
argument.

## Decision

Enable `gx_huc12_to_mainstem(..., method = "intersects")`. A null client
selects the NLDI endpoint for `outlet` and the reference endpoint for
`intersects`; an explicitly supplied client must match the chosen method.

For intersections, fetch and validate the `hu12` and `mainstems_v3` queryable
schemas once. Deduplicate HUC12 inputs. Retrieve each HUC12 by its advertised
identity filter, query mainstem candidates by the polygon's CRS84 bounding box,
and reject incomplete or ambiguous reference results. Disable retries in the
composed workflow and preserve every request in one aggregate ledger.

Validate HUC12 polygon identity and every mainstem feature, PID, ranking field,
geometry, supersession state, and replacement list. Enable S2 only during the
local operation and restore its prior process state. Return every true geometry
intersection in the ADR 0082 order with rank, geodesic intersection length,
outlet-HUC12 agreement, drainage area, observed currentness, and every
replacement PID. The result class is `gx_huc12_intersection_crosswalk` because
the outlet result has a different schema.

A missing HUC12 and a HUC12 with no mainstem intersections each receive one
explicit not-found row with distinct diagnostics. Ranking is not selection.
Never use NLDI or the legacy mainstem collection as a fallback for this method.

Apply aggregate request, response-byte, bounding-box candidate, true-match,
and expanded-row ceilings. The candidate ceiling uses
`geoconnexr.crosswalk_max_matches` conservatively, so a large bounding-box set
fails even if local intersection might later remove most candidates.

## Consequences

- HUC12 outlet and intersection workflows are both public and remain
  semantically distinct.
- The known HUC12 returns all 17 checked intersections, with the NLDI outlet
  mainstem ranked first but not selected.
- Duplicate HUC12 inputs share schema and feature transport.
- Superseded geometry and replacements remain visible.
- Large or incomplete candidate sets fail instead of returning a partial rank.
