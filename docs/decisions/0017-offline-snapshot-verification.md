# ADR 0017: Verify snapshot contents as a closed offline tree

- Status: Accepted internal substrate
- Date: 2026-07-14

## Context

The M9 roadmap distinguishes rerunning a recipe against changing services from
verifying bytes already stored in a snapshot. The package has neither a
snapshot writer nor runtime contracts for every planned recipe stage, so
exporting `gx_replay()` would prematurely commit refresh, loading, and failure
semantics. It can nevertheless verify opaque stored resources without M7 fetch
handlers, M8 harmonization, Arrow, Quarto, or resource-specific parsers.

`manifest-v1.json` is structural metadata, not a filesystem safety boundary.
JSON Schema cannot detect duplicate object members, symlinks, special files,
path aliases, resource mutation, or whether canonical spatial AOI GeoJSON still
matches its WKB digest. Base R also reports a FIFO as a file for common
existence tests, so hashing an unchecked path could block.

## Decision

Add the unexported `gx_snapshot_verify_impl(dir)` M9a entry point. It accepts
one existing, non-symlink snapshot directory and reads only its fixed
`manifest.json`. The manifest reader uses mandatory package code, not the
suggested `jsonvalidate` package, and accepts only unmarked UTF-8 JSON. It
rejects invalid encoding and escapes, literal or escaped controls, unpaired
surrogates, malformed delimiters, duplicate decoded object names, and unknown
runtime fields.

The fixed verification budgets are:

- 16 MiB of manifest bytes, eight JSON containers, 650,000 parsed members, and
  1,950,002 pre-parse structural units;
- 10,000 embedded requests and 10,000 resources;
- 1,024 bytes per portable resource path, 255 bytes per component, 16 path
  components, and 16 roles per resource;
- 50,000 filesystem entries and 16 directory levels; and
- 1 GiB per resource and 1 GiB across all declared resources.

Planned time, catalog, fetch, harmonize, and output recipe members are checked
as inert structure only. The verifier extracts the recipe's AOI into the exact
M6b `aoi` to `catalog` fragment and calls `gx_aoi_from_recipe_impl()` to
re-establish identifier or geometry identity. It never executes those planned
stages.

Resource paths use a deliberately narrow portable ASCII POSIX grammar. They
cannot be absolute, drive-relative, UNC, URL-like, dot-segmented, hidden,
Windows-device, backslash-separated, case-fold-colliding, overlong, or a
`manifest.json` self-reference. File/directory prefix collisions also fail.
The verifier inventories the closed snapshot tree without following links,
using failing directory enumeration and `fs::file_info()` to require readable
directories and singly linked regular files. Any symlink, hard-link alias,
special file, undeclared file, unreadable directory, or directory that is not a
declared resource prefix fails closed.

All manifest structure, AOI identity, paths, resource cardinalities, and the
aggregate declared-byte budget are validated before resource hashing. Present
files must match their declared size and SHA-256, with type, containment,
inventory, and size checked again afterward. Resources are hashed in bytewise
path order. A missing required resource aborts; a missing optional resource is
reported, but any present optional resource with the wrong type, size, or hash
still aborts.

The embedded `requests` array is the authoritative ledger at this checkpoint.
M9a validates its bounded shape, identifiers, hashes, dates, and scalar types
but does not recompute request hashes or authenticate its historical claims. A
`requests.csv` resource is only opaque hash-verified bytes until its canonical
serialization receives a separate contract.

All failures inherit `gx_error_snapshot`, use redacted traces, and avoid
echoing paths, URLs, or digests. Verification performs no network, DNS,
catalog, handler, query, decompression, resource parsing, cache, repair, or
write operation. Do not export the verifier or `gx_replay()` in this slice.

## Consequences

- Identifier and custom-geometry snapshot manifests can now bind their AOI
  identity to a closed set of exact local bytes entirely offline.
- M7 and M8 are not prerequisites because resource contents remain opaque.
- A successful result proves bounded internal consistency relative to the
  supplied manifest. It does not prove authenticity, request provenance,
  licence truth, currentness, or protection against coordinated replacement of
  both manifest and resources; no signature or trusted out-of-band manifest
  digest exists yet.
- Pre/post type, inventory, and size checks around one digest pass narrow
  ordinary mutation races, but R does not provide a fully race-proof
  `openat`/`O_NOFOLLOW`
  hostile-filesystem boundary. Public replay remains gated on snapshot writing,
  loading semantics, executable recipe contracts, and Frictionless acceptance.
