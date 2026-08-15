# ADR 0076: Export release-scoped COMID crosswalks

- Status: Accepted public boundary
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the public-export gate in ADRs 0008 and 0009

## Context

ADRs 0008 and 0009 established a checksum-pinned `ref_rivers` v3.2 lookup and
bounded forward and inverse mapping substrates. ADR 0075 later selected
`mainstems_v3` for current service checks, but it kept local mapping-release
evidence distinct from live currentness evidence.

The local mappers already preserve duplicate inputs, explicit not-found rows,
deterministic ordering, release provenance, and a
`mainstem_currentness_not_checked` diagnostic. They never download, refresh,
repair, or contact the reference service. Keeping these safe operations private
would not improve the pending live currentness workflow.

## Decision

Export `gx_comid_to_mainstem()` and `gx_mainstem_to_comids()` as public
release-scoped wrappers. They use only an explicitly installed and reverified
lookup. Their rows remain scoped to mapping-release membership, and metadata
keeps `currentness_policy = "not_checked"`.

Both functions expose `check = FALSE`. Any non-logical value fails as input,
and `check = TRUE` fails with
`gx_error_crosswalk_currentness_unavailable` before lookup or network work.
This prevents a release match from being mistaken for a live assertion while
leaving room for the separate bounded `mainstems_v3` currentness contract.

Zero-length inputs remain typed and require no installed lookup. Missing or
invalid installed bytes continue to fail with the existing actionable lookup
errors. Neither wrapper installs data implicitly.

## Consequences

- Users can run reproducible COMID and inverse crosswalks offline after one
  explicit lookup installation.
- Every result states that live currentness was not checked.
- The live-v3 workflow can extend the API only after it preserves requested
  PIDs, supersession, every replacement, observation time, and request evidence.
- HUC12, point, inverse-gage, and mainstem-resolution work remains open.
