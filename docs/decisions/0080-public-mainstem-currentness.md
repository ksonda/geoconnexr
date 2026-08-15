# ADR 0080: Publish bounded mainstem currentness

- Status: Accepted public boundary
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the public currentness implementation gate in ADR 0075

## Context

ADR 0075 selected `mainstems_v3`, dataset vintage 3.0, and the persistent
`https://geoconnex.us/ref/mainstems/{id}` PID namespace. Its checked evidence
includes a current mainstem, a superseded mainstem, its replacement, and a
large current geometry. The public crosswalks still label their local mapping
results `not_checked` because they do not contact the live reference service.

The reference client already provides the required bounded retrieval path. It
tries the OGC item route, then an exact identity filter, and records every
request. Negotiated JSON-LD is intentionally incomplete and cannot supply the
required migration properties. The checked OGC representation exposes
`superseded` as a logical property and `new_mainstemid` as a string containing
a Python-style list of canonical PID strings.

## Decision

Export `gx_mainstem(mainstem_uri)`. Accept only canonical Geoconnex mainstem
PIDs with positive decimal identifiers. Deduplicate repeated PIDs for
transport, then expand results in the caller's original order.

Retrieve only from `mainstems_v3` and record dataset vintage 3.0. Require a
complete item or exact-filter result whose feature identity, `id`, `uri`,
`superseded`, and `new_mainstemid` properties satisfy the checked contract.
Disable transport retries for this composed workflow and enforce aggregate
input, request, and response-byte ceilings.

Parse `new_mainstemid` only when it matches the checked upstream list encoding
exactly. Preserve every replacement in advertised order. Reject invalid or
duplicate replacements and reject a current feature that advertises a
replacement. A superseded feature with an empty replacement string remains
`superseded_unresolved` with a warning. Never retrieve, follow, rank, or select
a replacement automatically.

Return requested PID, status, replacement index and URI, collection, dataset
vintage, observation time, retrieval mode, diagnostics, and the aggregate
redacted request ledger. A missing or incompatible live feature fails visibly
with its request evidence instead of falling back to the legacy collection.

Keep release-scoped crosswalk results and live currentness separate for now.
Their `check = TRUE` composition needs its own output contract so a
one-to-many replacement cannot be flattened or silently chosen.

## Consequences

- Callers can inspect current and superseded PIDs without downloading the
  large COMID mapping.
- Repeated PIDs share transport while their output rows retain input order.
- One-to-many migration remains one-to-many.
- JSON-LD fallback cannot create a partial currentness claim.
- COMID, HUC12, Point, and inverse crosswalks continue to report unchecked
  currentness until an explicit composition boundary is added.
