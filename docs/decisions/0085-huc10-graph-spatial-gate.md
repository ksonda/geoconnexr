# ADR 0085: Keep automatic HUC10 site discovery behind the graph spatial gate

- Status: Recorded upstream gate
- Date: 2026-08-15
- Owner: ksonda
- Extends: ADR 0074

## Context

The installed HUC10 case study can run through fetch, harmonization, package
creation, verification, hydration, and stored-state inspection when the caller
supplies a reviewed site profile. Automatic AOI membership still depends on the
named `sites_in_aoi` graph query.

The graph accepts bounded SELECT and ASK requests. It also evaluates
`geof:sfIntersects` in 0.206 seconds when a known gage PID is bound before the
spatial filter. The same function times out when it must discover an unbound
site for HUC10 `0206000502`, even with a one-row limit. Restricting the site to
`HY_HydrometricFeature`, replacing the exact polygon with its four-corner
bounding box, and reversing the spatial operands all reached the 20-second
transport boundary.

## Decision

Keep automatic HUC10 site discovery open until the graph can answer one
bounded unbound-site `geof:sfIntersects` query within the package timeout.
Retain the existing one-page, finite-row catalog query and its visible failure
condition.

Do not substitute reference gages for graph-discovered monitoring sites. The
reference collection is not evidence that it contains every monitoring site
represented in the graph. Do not use a bounding-box-only result as AOI
membership or hide a truncated candidate set behind a complete result.

The installed case study remains a verified end-to-end data workflow with a
caller-supplied profile. Its documentation must keep that provenance and the
missing automatic membership claim explicit.

## Consequences

- The endpoint supports the spatial function but does not yet satisfy the
  bounded discovery contract.
- The recorded controls distinguish an upstream search-plan limit from a
  missing GeoSPARQL function.
- Automatic discovery can be enabled without changing the public catalog shape
  after a bounded live probe succeeds.
- The package continues to fail visibly instead of returning an incomplete set
  of sites as a complete AOI catalog.
