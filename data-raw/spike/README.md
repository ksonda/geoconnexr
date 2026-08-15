# P0 vertical-spike workspace

Code in `data-raw/` is maintainer-operated evidence code, not package runtime
code and not an automatic test target.

The P0 slice is complete only after it records:

1. one bounded AOI query using `geof:sfIntersects`;
2. five minimized JSON-LD profiles from at least three providers;
3. one HUC10 through catalog, one current USGS or EDR fetch, harmonization, and
   offline package verification;
4. current, superseded, v3, and large-mainstem behavior;
5. explicit EDR and current USGS request plans; and
6. replay metadata, redirect/cache behavior, and encoded/decoded size ceilings.

Recommended live-run ceiling: 20 requests, 60 seconds per request, 2 MiB per
metadata response, 10 MiB total metadata, and no redirect following for graph
POSTs. Store large/raw captures outside the package; commit only minimized,
sanitized fixtures and SHA-256 evidence sidecars.

The M2 JSON-LD corpus closes its module-level profile fixture requirement. M3
has hash-pinned collection/queryable schemas, checked fallback behavior, and
deterministic pagination/empty-result tests. ADR 0075 selects `mainstems_v3`
and records current, superseded, replacement, and full large-geometry evidence
in `mainstem-vintage-evidence-v1.json`. ADR 0087 replaces the provisional
roadmap ranges with a measured remaining delivery estimate. ADR 0074 closes
the graph endpoint decision by selecting the documented
root as a configurable experimental contract. Its bounded SELECT and ASK
evidence is recorded in `graph-contract-evidence-v1.json`. ADR 0085 adds the
successful bound-site spatial control and the remaining unbound-search timeout
evidence.

`huc12pp-contract-evidence-v1.json` pins successful and absent USGS NLDI
`huc12pp` item responses, including one HUC12 inside the end-to-end demo HUC10.
ADR 0077 uses that evidence for the public outlet method. ADRs 0082 and 0083
select and implement the spatial intersection ranking.

`nldi-position-evidence-v1.json` pins successful and absent NLDI
`comid/position` responses. ADRs 0078 and 0079 select and expose that point
provenance boundary; every returned COMID passes through the pinned mapping.

`inverse-gage-evidence-v1.json` records the live `mainstem_uri` gage queryable,
a complete three-gage known answer, and a complete empty answer. ADR 0081 uses
that direct reference filter instead of expanding a mainstem through every
COMID in the optional mapping.

`huc12-intersection-evidence-v1.json` records the reference HUC12 polygon, 41
bounded `mainstems_v3` bounding-box candidates, and 17 local S2 geometry
intersections. ADR 0082 ranks current features first, then exact advertised
outlet-HUC12 matches, intersection length, drainage area, and PID. The ranking
never selects one result.

The installed `huc10-usgs-daily-demo.tgz` fixture records the current USGS
daily fetch, reviewed flow-unit conversion, Frictionless package, closed-tree
verification, typed hydration, and offline stored-state inspection for HUC10
`0206000502`. Its caller-supplied profile keeps the case study independent
of the upstream graph and records that AOI membership was not rechecked.
Automatic HUC10 site discovery therefore remains an open part of the P0
spatial evidence gate. A 2026-08-15 one-site probe reached the package's
30-second graph boundary without a result; the evidence report records that
failure without treating it as proof that the AOI has no sites.

The M4a gage crosswalk has separate hash-pinned queryable and known-answer
fixtures. `m4-upstream-evidence-v1.json` pins the checked upstream commit,
release asset, checksum, and contrasting known answers that invalidated the
earlier COMID→VAA `levelpathi`→mainstem assumption: those identifiers are not
interchangeable. M4b now mirrors that audit in an immutable installed registry
and implements an explicit, integrity-checked download/import/offline lifecycle
plus public release-scoped forward and inverse mappers. ADR 0077 adds the
fixture-pinned HUC12 outlet path, and ADR 0079 adds the Point position path.
ADR 0080 exposes the checked `mainstems_v3` currentness and replacement
contract. ADR 0081 exposes the direct inverse-gage filter, ADR 0082 selects the
HUC12 intersection ranking, and ADR 0083 implements that method. ADR 0084
composes live currentness with the release-scoped and NLDI crosswalks under one
aggregate budget.
