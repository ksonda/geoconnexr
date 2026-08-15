# ADR 0084: Compose crosswalks with live currentness

- Status: Accepted public boundary
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the `check = TRUE` implementation gate in ADRs 0076, 0077,
  0079, and 0080

## Context

The public COMID, HUC12 outlet, and Point crosswalks preserve stable mapping
evidence but previously rejected `check = TRUE`. `gx_mainstem()` now provides a
bounded `mainstems_v3` currentness boundary. Its output preserves current,
superseded, and unresolved superseded states plus every advertised replacement.

Flattening replacements into the original crosswalk rows would duplicate source
matches and could make a replacement look selected. Replacing the matched PID
would lose the release or NLDI identity that produced it.

## Decision

Allow `check = TRUE` for `gx_comid_to_mainstem()`,
`gx_mainstem_to_comids()`, HUC12 outlet mode, and
`gx_point_to_mainstem()`. Accept a separate reference client because HUC12 and
Point resolution use NLDI clients for their first transport stage.

Keep one row per original crosswalk match. Preserve the matched PID and add the
observed mainstem status, every replacement as a character vector, observation
time, and item or filter retrieval mode. Do not retrieve, follow, rank, or
select a replacement. The inverse COMID crosswalk checks every requested PID,
including a PID that has no COMID in the selected mapping release.

Merge the NLDI and reference request ledgers and enforce one aggregate request
and response-byte budget. Disable retries inside the composed currentness
stage. Record `mainstems_v3`, dataset vintage 3.0, and
`currentness_policy = "live_v3_observed"` in checked metadata. Keep
`check = FALSE` offline and release-scoped with empty replacement vectors and
no observation claims.

HUC12 intersection mode already reads currentness from each retrieved
`mainstems_v3` candidate. Its behavior is unchanged for either value of
`check`.

## Consequences

- Checked crosswalks preserve both their source match and all migration data.
- Repeated matched PIDs share currentness transport.
- A checked inverse miss can still report the requested PID's live state.
- Checked HUC12 and Point calls have one auditable ledger across both services.
- Callers choose whether to act on a replacement after inspecting the result.
