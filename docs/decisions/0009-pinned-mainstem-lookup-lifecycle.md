# ADR 0009: Install the pinned mainstem lookup explicitly

- Status: Accepted experimental policy
- Date: 2026-07-13

## Context

ADR 0008 requires a checksum-pinned COMID-to-mainstem mapping before the
remaining M4 crosswalks can proceed. The audited `ref_rivers` v3.2
`nhdpv2_lookup.csv` asset has exact columns `uri,comid`, 2,357,730 data rows,
and a zero-or-one forward mapping: every included COMID occurs once. The
generator selects mainstems that were not superseded at that release. This is
release-state evidence, not a guarantee of current reference-service state.

The source CSV is 120,422,425 bytes. The package HTTP cache buffers response
bodies in memory and serializes them as expiring cache entries, so it is not an
appropriate large-data store. The upstream adapter downloads the same complete
CSV and converts it to Parquet, but the inspected adapter does not enforce the
package's pinned content digest. An implicit download from a crosswalk would
also violate the roadmap's disclosure and offline requirements.

The immutable runtime registry is installed from
[`inst/mainstem-lookups/registry-v1.json`](../../inst/mainstem-lookups/registry-v1.json).
Its release, source commit, asset ID, size, SHA-256, schema, cardinality, known
answers, and license agree with the durable audit in
[`data-raw/spike/m4-upstream-evidence-v1.json`](../../data-raw/spike/m4-upstream-evidence-v1.json).

## Decision

Add an explicit optional-data lifecycle:

- `gx_mainstem_lookup_install()` is the only operation authorized to download
  the disclosed release asset. It can instead import the identical pinned bytes
  from a local file while offline.
- `gx_mainstem_lookup_info()` inspects and re-verifies an installation without
  downloading, refreshing, or repairing it.
- Store the content-addressed lookup and a non-sensitive provenance receipt in
  a separately marked package data directory, not in the HTTP cache.
- Stream release downloads to a same-directory staging path. Disable automatic
  redirects, validate DNS and an HTTPS host allowlist on every hop, require
  identity encoding, enforce byte and redirect ceilings, and atomically expose
  the asset only after its exact digest, schema, row count, and known answers
  verify.
- Never persist GitHub's expiring signed redirect query or a local import path.
- Re-verify the source CSV before every parsing operation. Scan it in bounded
  chunks for the requested vector; an index may be added later without changing
  the row contract.

Implement `gx_comid_to_mainstem_impl()` as an internal M4b substrate. It
preserves character identifiers, duplicate input order, explicit not-found
rows, deterministic ambiguity for future zero-to-many adapters, release and
checksum provenance, and an explicit `mainstem_currentness_not_checked`
diagnostic. It never installs data or contacts the network.

Do not export `gx_comid_to_mainstem()` yet. ADR 0004 still requires a selected
mainstem collection/vintage and a fixture-backed current/superseded migration
contract for the public `check` argument.

## Consequences

- M4 now has a safe, offline-capable mapping substrate without a hidden large
  transfer or heavyweight Arrow/SQLite dependency.
- A missing, corrupt, or incomplete lookup fails with an actionable classed
  error and is never repaired implicitly.
- Forward lookup is complete only against the pinned v3.2 NHDPlusV2 mapping.
  Absence means "not represented in this mapping release," not "unknown or
  superseded everywhere."
- Repeated lookups scan 120 MB from local disk. A content-addressed index is a
  compatible future optimization if measurement justifies it.
- Public COMID, HUC12, point, inverse, and current-mainstem APIs remain gated.
