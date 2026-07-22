# ADR 0035: Publish bounded catalog discovery without reopening M7

- Status: Accepted experimental policy
- Date: 2026-07-22
- Owners: package maintainers
- Related: ADR 0012, ADR 0018, ADR 0034

## Context

ADR 0034 closed M7 around a public fetch plan and result, but users still
needed an internal catalog object to reach those APIs. That made the supported
fetch subset testable without making it usable from a fresh R session.

The Geoconnex graph exposes the reviewed `sites_in_aoi` query surface, but a
live bounded polygon query timed out during this checkpoint. Current Geoconnex
documentation also describes graph query functionality as active development.
Explicit PID profile retrieval is reliable for the tested WQP location. The
tested SNOTEL PID, however, currently describes variables without distribution
URLs, so it cannot advertise the live EDR endpoint used for interoperability
testing.

## Decision

Export `gx_catalog()` as a bounded adapter into the existing strict
`gx_catalog` 0.1.0 value object. It supports three explicit modes:

1. With only an AOI, resolve identifier geometry when needed, execute exactly
   one bounded `sites_in_aoi` graph query, then retrieve selected PID profiles.
2. With `site_uri`, skip graph discovery and retrieve exactly those PID
   profiles. Record that AOI membership was not rechecked.
3. With matching named `profiles`, skip PID retrieval and adapt caller-supplied
   local JSON-LD. Record that the profiles are caller supplied and do not
   upgrade them to provider provenance.

All modes validate canonical site identities, safe distribution URLs, strict
CRS84 point sites, dataset/distribution/variable identity, structured
problems, request evidence, and reconciled completeness. Discovery is
sequential, follows no graph page, and accepts at most 100 sites. A site
profile failure leaves an explicit empty-geometry site row and a partial
catalog so input and failure counts remain reconcilable.

The live compatibility corrections made at this boundary do not extend M7:

- WQP profile URLs under either `/wqx3/Result/search` or
  `/data/Result/search` are normalized to the stable narrow Result CSV route;
- `dataRetrieval::importWQP()` validates the same inline response without a
  second provider request;
- EDR accepts the pygeoapi CoverageJSON media type and reviewed CRS84 URI, and
  normalizes valid PointSeries `t,y,x` range shapes; and
- EDR requires `edr4r` 0.1.1 or newer.

## Acceptance criteria

- Explicit live WQP PID discovery produces a validated public catalog and one
  bounded fetch succeeds through `gx_fetch()`.
- A caller-supplied local profile produces a validated public EDR catalog and
  the reviewed live pygeoapi position request succeeds through `gx_fetch()`.
- Automatic graph discovery issues no more than one bounded graph request and
  reports truncation rather than following a page.
- Profile retrieval and adaptation failures remain typed, partial, and
  count-reconciled.
- Fixture-backed tests cover all modes; live services are evidence checks, not
  required by the default test suite.
- Public documentation distinguishes observed provider results from
  caller-supplied metadata and states the current graph limitation.

## Consequences

- A new user can now run catalog → plan → fetch without internal helpers.
- WQP is the current fully live Geoconnex PID example.
- EDR execution is public and live, while its included example profile remains
  explicitly caller supplied until a tested Geoconnex PID advertises an EDR
  distribution.
- Upstream graph latency can still prevent automatic spatial discovery. An
  explicit PID is the reliable bounded fallback, not an assertion of AOI
  membership.
- M7 remains closed. M8 harmonization is still the next roadmap milestone.
