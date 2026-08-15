# ADR 0081: Publish mainstem to gage crosswalking

- Status: Accepted public boundary
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the inverse-gage implementation gate in ADR 0008

## Context

The roadmap includes `gx_mainstem_to_gages()` as a zero-to-many inverse. The
live reference `gages` collection now advertises `mainstem_uri` as a string
queryable. The bounded evidence in
`data-raw/spike/inverse-gage-evidence-v1.json` records one complete three-gage
answer and one complete empty answer. Every known-answer feature repeats the
requested mainstem URI and has consistent feature, property, PID, provider,
and COMID identity.

Resolving through the pinned COMID mapping would turn one mainstem into many
reference requests and would exclude gages without an advertised COMID. The
native `mainstem_uri` filter is the direct and more complete service contract.
It does not prove that the requested PID is current in `mainstems_v3`.

## Decision

Export `gx_mainstem_to_gages(mainstem_uri)`. Accept only canonical Geoconnex
mainstem PIDs. Fetch the `gages` queryables once and require one `id` identity
queryable plus string `uri`, `provider_id`, and `mainstem_uri` queryables.

Issue one bounded, paginated `mainstem_uri` filter for each unique input. Every
returned feature must repeat the requested mainstem URI and have consistent
feature ID, property ID, gage PID, and provider ID. Retain a valid COMID when
advertised and keep a missing COMID explicit. Reject duplicate gage identities,
contradictory filters, incomplete pagination, and aggregate budget overruns.

Return every matching gage in deterministic gage-PID order. Preserve duplicate
input order while sharing transport. A complete empty FeatureCollection gets
one explicit `not_found` row. Multiple gages are members, not ambiguous
alternatives, so every returned member has `status = "matched"`.

Record `currentness_policy = "not_checked"`. Do not call `gx_mainstem()`,
follow replacements, or silently rewrite a superseded PID. Currentness
composition remains a separate contract because one PID can have multiple
replacements.

## Consequences

- Inverse gage lookup does not require the optional COMID mapping asset.
- Gages without COMIDs remain discoverable.
- Repeated mainstem inputs do not repeat transport.
- Multi-gage membership remains complete and deterministic within the bounded
  reference response.
- Live mainstem currentness remains explicit and separate.
