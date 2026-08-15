# ADR 0075: Use mainstems_v3 with explicit migration

- Status: Accepted experimental upstream contract
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the mainstem collection and migration decision gate in ADR 0004

## Context

The upstream ref_rivers repository describes data release 3.0 as the current
dataset. It retains persistent v2 identifiers, adds new identifiers, and marks
superseded identifiers with one or more replacements. The live reference
service advertises both `mainstems` and `mainstems_v3`, but neither
collection document carries an explicit machine-readable vintage.

The bounded evidence in
`data-raw/spike/mainstem-vintage-evidence-v1.json` compares a large current
item, a superseded item, and its current replacement in both collections. The
v3 route returns the large item directly and supplies the fuller geometry. The
legacy item route still needs the exact-filter fallback for that case. Both
collections use the same persistent `https://geoconnex.us/ref/mainstems/`
identity namespace.

Negotiated JSON-LD works for the current cases. The checked superseded response
has the advertised JSON-LD media type but malformed JSON in both collections.
It cannot serve as a required migration path.

## Decision

Use `mainstems_v3` as the default reference collection and record its dataset
vintage as 3.0. Keep `https://geoconnex.us/ref/mainstems/{id}` as the
persistent identity. The collection name is a representation choice and never
changes the PID namespace.

Resolve one v3 feature through the OGC item route, then its exact identity
filter. Valid negotiated JSON-LD remains a final, incomplete fallback and may
use the shared persistent PID. Do not silently fall back from
`mainstems_v3` to `mainstems`, because their geometries can differ. Callers
may still request the legacy collection explicitly.

Never replace a requested PID automatically. A currentness result must retain
the requested URI, its observed `superseded` state, every advertised
`new_mainstemid` replacement, the `mainstems_v3` collection, the 3.0
vintage, and the observation time. One-to-many replacements remain
one-to-many; the package does not rank them.

The checksum-pinned v3.2 COMID lookup remains release-scoped. Its local matches
do not become current-service assertions merely because this ADR selects the
v3 reference collection. Public COMID wrappers must compose an explicit live
v3 check or label the result as release-only.

## Consequences

- Current geometry and migration checks have one default collection.
- Existing mainstem PIDs remain unchanged across the collection transition.
- Supersession stays visible and never rewrites user input silently.
- Invalid or unavailable v3 paths fail visibly instead of downgrading geometry.
- Public COMID and inverse wrappers remain a separate M4 implementation slice.
