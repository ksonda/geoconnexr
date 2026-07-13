# JSON-LD context registry

`context-registry-v1.json` is the complete default allowlist for remote JSON-LD
contexts. Exact matching URLs are replaced with a hash-verified bundled context
before expansion. Unknown remote contexts and `@import` are rejected; the
JSON-LD processor is never allowed to retrieve a document.

`schemaorg-context-v30.0.json` is the official Schema.org JSON-LD context from
`https://schema.org/docs/jsonldcontext.json`, captured 2026-07-13 while the
Schema.org site reported release V30.0. The upstream byte stream had SHA-256
`58f70940892ef4edd66e9482b9ffc50559cacc0821c74add8cddc08451a2eb03`;
the bundled text adds one terminal LF and pins SHA-256
`3555b9ac81047b6f5801736808d3b913ea54a555925874bba880ca215eb50c64`.
The snapshot preserves Schema.org's URI coercion and its documented
`http://schema.org/` vocabulary behavior; it is not a package-authored
abbreviation of that context.
See `LICENSE.schemaorg.md` for its separate CC BY-SA 3.0 attribution and reuse
terms. geoconnexr's MIT license applies to the package code, not this third-party
schema snapshot.

`known_prefixes` supports explicit, diagnostic-producing repairs for known
Geoconnex profile defects such as the current reference-gage use of `gsp:`
without a matching context definition. Adding a remote context or prefix is a
reviewed contract change.
