# Serialized contracts

These schemas are experimental inputs to the P0 vertical slice. They must not be
declared stable until the slice and provider audit exercise them.

- `recipe-v1.json` records a repeatable procedure against current services.
  Its implemented AOI forms are identifier recipes (`huc`, `county`, and
  `state`) plus one bounded polygonal `sf` recipe. Spatial recipes use OGC
  CRS84 canonical GeoJSON and require a portable WKB SHA-256; planned
  mainstem-basin and point-upstream forms are intentionally absent until their
  provenance contracts are implemented.
  JSON Schema checks this recipe's structural shape and identifier syntax; it
  does not prove polygon closure/validity/canonical order or bind GeoJSON to its
  WKB digest. The internal M6b AOI-only reader re-establishes those runtime
  invariants for the exact three-field fragment emitted by `gx_aoi()`. It does
  not accept the schema's planned optional pipeline fields or authorize full
  replay.
- `manifest-v1.json` records that recipe separately from offline-verifiable
  resources and embedded request evidence. Its resource counts, roles, byte
  sizes, and portable relative path grammar match the internal M9a verifier.
  Runtime code additionally rejects duplicate decoded members, path aliases,
  symlinks, hard-link aliases, unreadable directories, special or undeclared
  files, and non-prefix directories; hydrates AOI identity through M6b; and
  verifies exact resource bytes without parsing
  them. Embedded requests are shape-validated, not authenticated or
  cryptographically bound to resource exports. The manifest is unsigned, so
  successful verification proves bounded internal consistency only.
  The internal M9b writer now produces one deliberately narrow catalog-only
  instance of this manifest through a verified staging tree. Its CSV projection
  and M6c value object are experimental runtime contracts, not a Frictionless
  profile or a public loading/replay promise.
- `query-manifest-v2.json` validates the exact render-only YAML named-query
  manifest after bounded, unambiguous YAML parsing. Runtime validation also
  checks template bytes, projections, slots, ordering facts, and cross-field
  safety relationships that JSON Schema alone cannot express.

Schema validation does not make file paths, URLs, or JSON-LD contexts safe.
Runtime code must separately enforce package-root containment, SSRF/redirect
policy, payload ceilings, redaction, and the bundled-context allowlist.
