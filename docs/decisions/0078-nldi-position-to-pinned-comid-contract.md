# ADR 0078: Resolve points through NLDI COMIDs

- Status: Accepted experimental upstream contract
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the point provenance gate in ADR 0008

## Context

The official USGS NLDI documentation exposes
`/linked-data/comid/position` for point-in-polygon catchment lookup. The bounded
probes in `data-raw/spike/nldi-position-evidence-v1.json` include the HUC12 demo
outlet and one point outside indexed catchments.

The successful response contains one NHDPlusV2 flowline with the same COMID in
its top-level ID, `properties.identifier`, and `properties.comid`. It does not
advertise a Geoconnex mainstem PID. The absent case returns HTTP 404 problem
JSON.

## Decision

Use the NLDI `comid/position` route as the point provenance boundary. A public
point crosswalk must accept only nonempty two-dimensional Point geometries with
a declared CRS. It must disable PROJ network access during transformation to
OGC CRS84, enforce longitude and latitude bounds, and deduplicate identical
transformed points before transport.

Require a successful response to contain exactly one LineString feature from
source `comid`, with equal valid COMIDs in all three advertised identity
fields. Treat HTTP 404 as an explicit not-found result. Other status, payload,
identity, geometry, or source changes fail closed.

Resolve every returned COMID through the explicitly installed checksum-pinned
mapping from ADR 0009. Never interpret the NLDI LineString ID as a Geoconnex
mainstem ID, and never download lookup data implicitly. The resulting mainstem
remains release-scoped with currentness unchecked.

## Consequences

- The point provenance and identifier namespace are selected.
- Point crosswalking does not require `nhdplusTools`.
- Public implementation still needs fixture-backed geometry, transport,
  mapping, duplicate, not-found, and budget tests.
- Live-v3 currentness remains a separate contract.
