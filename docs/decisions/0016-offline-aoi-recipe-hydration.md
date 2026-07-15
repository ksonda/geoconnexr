# ADR 0016: Re-establish AOI integrity when hydrating recipes

- Status: Accepted internal substrate
- Date: 2026-07-13

## Context

ADR 0014 records canonical CRS84 GeoJSON and a portable little-endian WKB
SHA-256 in each spatial AOI recipe. JSON Schema can check the serialized shape,
but it cannot prove topology, canonical ring and member order, or that the
GeoJSON and digest describe the same geometry. Trusting a decoded recipe
directly would leave snapshot verification and replay open to type coercion,
duplicate members, noncanonical geometry, and hash substitution.

The full `recipe-v1.json` schema also contains planned time, catalog, fetch,
harmonize, and output fields. Those fields do not yet have runtime replay
contracts. This decision therefore covers only the exact three-field AOI
fragment emitted by `gx_aoi()`.

## Decision

Add two unexported M6b readers:

- `gx_aoi_from_recipe_impl()` accepts one already-decoded AOI recipe;
- `gx_aoi_from_recipe_json_impl()` accepts raw JSON bytes or one literal JSON
  string, never a file path or URL.

Both require exactly `contract_version`, `aoi`, and `pipeline`, with the exact
`aoi` to `catalog` pipeline. JSON object-member order is immaterial, but missing,
extra, blank, duplicate, classed, or attributed members fail closed. Arrays
remain arrays; integer and double coordinate spellings normalize only after
each leaf is proven to be a plain finite JSON number.

The JSON boundary:

- accepts unmarked UTF-8 only and rejects byte-order marks, literal or escaped
  NUL/control values, invalid escapes, unpaired surrogates, malformed
  delimiters, and invalid UTF-8;
- uses literal parsing so the JSON library cannot interpret a string as a path
  or network URL;
- caps the serialized envelope at the 8 MiB geometry ceiling plus 4 KiB of
  fixed recipe overhead, nesting at seven containers, parsed members at
  350,011, structural preflight units at 350,022, and aggregate coordinate
  positions at 100,000; and
- detects duplicate decoded object names, including escaped-equivalent names,
  before semantic access.

Identifier recipes reconstruct through `gx_aoi(identifier, type = kind)` and
must equal the emitted canonical recipe exactly. Spatial recipes are manually
rebuilt as CRS84 polygons or multipolygons from checked XY positions, then run
through `gx_aoi()` again. The reader accepts the result only when both the
normalized submitted GeoJSON and declared WKB SHA-256 exactly equal the
regenerated canonical recipe and identity. Topologically equivalent but
rotated, rewound, reordered, or off-grid input is rejected rather than
silently normalized.

All failures inherit `gx_error_aoi_recipe` and `gx_error_aoi`. Hydration uses no
file, network, DNS, graph, reference, catalog, or `jsonvalidate` operation.
PROJ networking remains disabled and restored through ADR 0014's existing
canonicalization boundary.

Do not export either reader and do not treat this as `gx_replay()`. Full recipe
execution and snapshot resource verification remain separate M9 work.

## Consequences

- A serialized AOI can cross into runtime code only after its identifier or
  geometry identity is re-established independently.
- M9 offline snapshot verification now has a safe AOI hydration prerequisite,
  without authorizing live replay or catalog discovery.
- Whitespace counts toward the JSON envelope ceiling; compact recipes emitted
  from the maximum accepted canonical geometry retain fixed wrapper headroom.
- The broader recipe schema remains structural planning metadata until each
  optional stage receives its own runtime contract.

## Follow-up (2026-07-14)

ADR 0017 uses this AOI hydration boundary inside the internal M9a offline
snapshot verifier. It extracts only the AOI fragment from a full manifest
recipe, verifies a closed local resource tree, and still does not authorize
recipe execution or public `gx_replay()`.
