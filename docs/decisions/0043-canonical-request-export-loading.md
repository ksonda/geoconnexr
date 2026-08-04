# ADR 0043: Bind catalog request exports before broader snapshot loading

- Status: Accepted internal substrate
- Date: 2026-07-28
- Owners: package maintainers
- Related: ADR 0017, ADR 0019, ADR 0041, ADR 0042

## Context

M9d can create and verify catalog-only snapshots, but public resource loading
remains gated. The catalog CSV projection is intentionally redacted and is not
losslessly reversible into the live `gx_catalog` contract: sensitive optional
IRIs may no longer be canonical identities, and blank CSV cells do not retain
the distinction between every optional empty string and missing value.
Reconstructing a live catalog directly would therefore require inference.

The request ledger has a stronger representation. The manifest contains its
authoritative typed shape, while `requests.csv` is defined as one deterministic
quote-all UTF-8 projection. Binding those two representations is a necessary
prerequisite for any later snapshot loader.

## Decision

Add the internal M9e `gx_snapshot_load_requests_impl()` boundary.

It:

1. runs the closed-tree M9a verifier before loading;
2. recognizes only the exact M9b catalog writer, CSV, request-export, resource,
   and non-replayable profiles;
3. re-derives the canonical `requests.csv` bytes from the normalized manifest
   ledger and requires exact byte equality with the stored resource;
4. caps request-export loading at 64 MiB;
5. converts the authoritative ledger to the exact typed catalog request table
   and requires a lossless manifest round trip;
6. verifies the complete snapshot again after loading and rejects any changed
   manifest or resource evidence; and
7. returns exact `gx_snapshot_request_export` evidence binding the typed rows,
   request count, manifest hash, resource hash, and deterministic export ID.

The boundary performs no CSV schema inference, network, DNS, cache, write,
repair, refresh, loading of other resources, or replay work. It remains
internal until the surrounding snapshot-loading result and redacted catalog
resource semantics are specified.

## Acceptance criteria

- Empty and populated M9b request ledgers load to the exact typed request-table
  schema and round-trip to the normalized manifest ledger.
- A resource whose hash is valid but whose bytes are not the canonical
  manifest projection fails binding.
- Unknown writer or request-export profiles fail before resource loading.
- Malformed typed evidence, resource/manifest mutation, and exports beyond the
  loading ceiling fail closed.
- Loading invokes no network, DNS, cache, snapshot-writing, cleanup, or other
  resource parser seam.
- No new public API, replayability, authenticity, or historical provenance
  claim is introduced.

## Consequences

- Later public snapshot loading has a deterministic, typed, read-only request
  ledger substrate.
- `requests.csv` is no longer merely hash-checked for M9b snapshots; its bytes
  are proven to be the declared manifest ledger projection.
- Catalog sites, datasets, and problems remain opaque verified resources.
- Public loading still requires an explicit redacted catalog-view contract;
  live `gx_catalog` reconstruction must not invent identities discarded by
  serialization.
