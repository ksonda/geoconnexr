# ADR 0079: Publish Point to mainstem crosswalking

- Status: Accepted public boundary
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the point implementation gate in ADR 0078

## Context

ADR 0078 selected the USGS NLDI `comid/position` endpoint as the point
provenance boundary. Its checked response returns NHDPlusV2 COMID identity, not
a Geoconnex mainstem PID. ADR 0009 supplies the separate checksum-pinned mapping
from COMID to release-scoped mainstem membership.

Point input also needs a local geometry boundary. CRS transformation must not
enable a hidden grid download, and duplicate coordinates should not repeat
identical upstream requests.

## Decision

Export `gx_point_to_mainstem(points, check = FALSE)`. Accept `sf` and `sfc`
inputs containing only nonempty two-dimensional Points with a declared CRS.
Reject mixed, empty, Z, missing-CRS, nonfinite, or out-of-bounds geometry before
transport.

Transform to OGC CRS84 with PROJ network access disabled and restored to its
prior state afterward. Canonicalize finite longitude and latitude into one WKT
Point request key, deduplicate identical keys, and preserve original input
order in the expanded result.

Require NLDI success to contain exactly one LineString feature from source
`comid`, with the same valid COMID in its top-level ID,
`properties.identifier`, and `properties.comid`. Resolve that COMID only through
the explicitly installed pinned lookup. Never install or refresh lookup data
implicitly.

Keep NLDI HTTP 404 distinct from a COMID absent in the mapping release. Record
NLDI requests, mapping provenance, diagnostics, and release-only currentness in
the result. Reject `check = TRUE` before geometry, transport, or lookup work.

## Consequences

- Point to mainstem crosswalking is public without an optional R package.
- Coordinate transformation remains offline and bounded.
- Repeated Points share transport while retaining input order.
- Mapping matches do not claim current `mainstems_v3` state.
- Live-v3 supersession remains a separate M4 slice.
