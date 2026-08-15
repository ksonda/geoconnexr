# ADR 0077: Publish the HUC12 outlet crosswalk

- Status: Accepted public boundary
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the HUC12 response-evidence gate in ADR 0008

## Context

The USGS NLDI documentation identifies `huc12pp` as the current HUC12 pour
point source. Bounded item probes recorded in
`data-raw/spike/huc12pp-contract-evidence-v1.json` returned one GeoJSON Point
with the requested HUC12 repeated in the feature and properties, plus an
NHDPlusV2 COMID and canonical Geoconnex mainstem PID. An absent HUC12 returned
HTTP 404 problem JSON.

The response establishes an indexed outlet relationship. It does not establish
that the advertised mainstem remains current in `mainstems_v3`. The separate
pinned COMID lookup is available when a valid NLDI response omits its mainstem,
but that mapping is also release-scoped.

## Decision

Export `gx_huc12_to_mainstem(..., method = "outlet", check = FALSE)`. It uses a
dedicated configurable NLDI client, sends at most one item request per unique
input HUC12, and records every physical attempt or cache retrieval in the
crosswalk ledger.

Require exactly one GeoJSON Feature with matching feature ID,
`properties.identifier`, and `properties.uri`; source `huc12pp`; valid Point
geometry; and at least one valid COMID or canonical mainstem PID. Prefer the
advertised mainstem. When only COMID is present, use the explicitly installed
checksum-pinned mapping without installing or refreshing data.

Return explicit rows for HTTP 404 and preserve duplicate input order. Keep
`currentness_policy = "not_checked"`. Reject `check = TRUE` until the bounded
live-v3 workflow exists. Reject `method = "intersects"` until a documented
multi-match ranking contract is selected.

## Consequences

- HUC12 outlet crosswalking no longer depends on an optional R package.
- Repeated inputs avoid repeated upstream requests.
- Direct and COMID-fallback matches remain distinguishable in every row.
- NLDI service changes, invalid identities, and malformed payloads fail closed.
- Spatial intersection ranking and live mainstem supersession remain open.
