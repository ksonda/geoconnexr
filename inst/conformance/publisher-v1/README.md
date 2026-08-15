# Publisher profile 1.0.0 conformance corpus

This directory is a language-neutral known-answer suite for ADR 0071. The
same installed bytes are consumed by R tests and the standard-library Python
feasibility harness.

- `input.json` contains one provider, one point site, and one dataset with a
  complete two-distribution by two-variable product.
- `expected-profile.json` is the deterministic publisher JSON-LD document.
- `invalid-profile.json` contains three deliberate profile errors.
- `expected-findings.json` records the exact findings for those errors.
- `sitemap-uris.json` and `expected-sitemap.xml` form the sitemap known answer.
- `manifest.json` pins every conformance resource by byte length and SHA-256.

The corpus is synthetic. It contains no credentials, personal data, or claims
about a live provider. A port can consume these files without R, network
access, JSON-LD context retrieval, or optional packages.
