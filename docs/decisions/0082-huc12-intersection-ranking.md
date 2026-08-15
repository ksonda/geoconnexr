# ADR 0082: Rank HUC12 mainstem intersections without selecting one

- Status: Accepted experimental upstream contract
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the HUC12 intersection ranking decision gate in ADR 0077

## Context

The HUC12 outlet method returns one pour-point mainstem. The `intersects`
method has a different purpose: list every `mainstems_v3` geometry that crosses
the hydrologic-unit polygon. A bounding-box response includes false positives,
and a bare `is_largest` flag would not explain why one result precedes another.

The bounded evidence in
`data-raw/spike/huc12-intersection-evidence-v1.json` retrieves the reference
polygon for HUC12 `020600050201`, then 41 `mainstems_v3` bounding-box
candidates. Local S2 intersection leaves 17 matches. The mainstem whose
advertised NHDPlusV2 outlet HUC12 equals the request ranks first and agrees with
the independently checked NLDI outlet result.

## Decision

Use the reference `hu12` polygon and bounded `mainstems_v3` bounding-box query.
Validate both collections and reject incomplete candidate retrieval. Perform
the exact geometry intersection locally with S2 enabled. Return every spatial
match, including superseded features, with its observed currentness state and
every advertised replacement.

Rank results by these fields in order:

1. current before superseded;
2. exact advertised `outlet_nhdpv2huc12` match before other intersections;
3. geodesic intersection length in kilometers, descending;
4. advertised outlet drainage area in square kilometers, descending; and
5. canonical mainstem PID in bytewise order.

Ranking is descriptive and never selects a mainstem. The result must expose the
rank inputs, candidate count, intersection count, geometry runtime, collection,
dataset vintage, request ledger, and completeness. A missing outlet HUC12 does
not exclude a geometry match; it only makes the outlet-match metric false.

Candidate, request, response-byte, match, and expanded-row ceilings apply to
the whole call. If the bounding-box candidate set is truncated, fail instead
of treating the retained prefix as complete. Do not fall back to the legacy
mainstem collection or the NLDI outlet route.

## Consequences

- `intersects` and `outlet` remain distinct methods with different cardinality.
- The first row is reproducible from disclosed metrics but is not an automatic
  choice.
- A long crossing mainstem cannot outrank an exact outlet-HUC12 match solely by
  length.
- Superseded geometry remains visible and sorts after current geometry.
- Implementation can proceed without a graph query or optional mapping asset.
