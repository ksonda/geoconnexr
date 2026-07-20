# ADR 0008: Start crosswalks at the validated gage boundary

- Status: Accepted experimental policy
- Date: 2026-07-13

## Context

M4 originally assumed that `nhdplusTools::get_vaa(atts = "levelpathi")`
could map an NHDPlus COMID directly to a Geoconnex mainstem identifier. A
bounded upstream audit disproved that assumption: VAA level-path identifiers
and Geoconnex mainstem identifiers are different namespaces. Constructing a
mainstem PID from `levelpathi` would therefore produce a plausible but false
identity.

The checked upstream commit, release asset, checksum, and contrasting known
answers are pinned in
[`data-raw/spike/m4-upstream-evidence-v1.json`](../../data-raw/spike/m4-upstream-evidence-v1.json).

Current upstream mapping instead depends on a versioned `ref_rivers` lookup
asset exposed by the evolving `nhdplusTools`/`hydrogeofetch` adapter. That
asset is large, the package transition is in progress, and its download,
version, checksum, provenance, and offline behavior are not yet part of this
package's contract. Mainstem vintage selection also remains open under ADR
0004. Supersession may be one-to-many or unresolved, and the HUC intersection
ranking rule is still undecided.

The reference `gages` collection provides a narrower verified boundary. It
advertises `provider_id`, a sole identity role, the gage PID, COMID, and
mainstem URI. The checked provider identifier `USGS-08332622` maps to reference
gage `1000001`.

## Decision

Implement the first M4 slice as `gx_gage_to_pid()` over the native reference
client. The function:

- fetches and validates gage queryables once;
- deduplicates transport for repeated inputs and restores original order;
- verifies that the response honored the provider filter and that top-level,
  property, and PID identities agree;
- returns every distinct ambiguous match and an explicit not-found row rather
  than silently selecting or dropping either case;
- normalizes all identifier columns to character;
- treats advertised mainstem URIs as unverified related values until a
  mainstem vintage is selected; and
- applies aggregate input, match, row, request, and byte budgets across the
  batch while preserving the redacted request ledger.

Do not export COMID, HUC12, point, inverse-mainstem, or mainstem-resolution
crosswalks until their versioned lookup and mainstem policies have fixture-
backed contracts. In particular, never construct a Geoconnex mainstem PID from
a VAA `levelpathi` value.

## Consequences

- M4 is partially implemented and remains experimental.
- The first crosswalk has no optional package dependency or hidden data
  download.
- Query-bearing reference responses remain non-cacheable, so offline gage
  crosswalk lookup is not promised.
- Completing M4 requires a checksum-pinned mapping adapter, an explicit
  current/superseded one-to-many contract, checked HUC12 evidence, a point
  lookup provenance boundary, and a documented intersection ranking rule.

## Follow-up

ADR 0009 fulfills the checksum-pinned mapping-lifecycle portion of this
decision with an explicit install/import boundary and an internal COMID mapper.
The current/superseded, HUC12, point, and inverse-ranking gates remain open.
