# Serialized contracts

These schemas are experimental inputs to the P0 vertical slice. They must not be
declared stable until the slice and provider audit exercise them.

- `recipe-v1.json` records a repeatable procedure against current services.
- `manifest-v1.json` records that recipe separately from offline-verifiable
  resources and request evidence.
- `query-manifest-v1.json` validates the YAML named-query manifest after YAML
  parsing.

Schema validation does not make file paths, URLs, or JSON-LD contexts safe.
Runtime code must separately enforce package-root containment, SSRF/redirect
policy, payload ceilings, redaction, and the bundled-context allowlist.
