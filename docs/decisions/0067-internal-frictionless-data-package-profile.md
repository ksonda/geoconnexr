# ADR 0067: Describe fixed package bundles as Frictionless Data Package v1

- Status: Accepted internal substrate
- Date: 2026-08-10
- Owners: package maintainers
- Related: ADR 0049, ADR 0051, ADR 0060, ADR 0064, ADR 0066

## Context

M9k and M9z already produce exact, deterministic in-memory resource bundles
for the fixed catalog, fetched, harmonized, Parquet, and report-bearing package
profiles. Their manifests bind the closed published tree, but they are package-
specific integrity contracts rather than Frictionless Data Package descriptors.

The roadmap requires validation with the Python Frictionless CLI and explicit
declaration of non-tabular resources. It must not turn structural compatibility
into a publication, authenticity, refresh, or procedural-replay claim. Live
refresh also remains blocked because the current fetched contract does not
retain a complete provider request recipe capable of authorizing a fresh run.

## Decision

Add M9ac as an internal, deterministic Frictionless Data Package v1 description
of one exact validated M9k/M9z bundle.

1. Revalidate the complete input bundle and bind every descriptor resource to
   its exact relative path, byte length, and SHA-256 digest.
2. Declare canonical CSV resources as `tabular-data-resource` with UTF-8 CSV
   metadata and an exact all-string Table Schema matching the stored header.
3. Declare every non-CSV resource explicitly as generic `data-resource`, with
   its format and media type but no inferred tabular schema. In particular,
   Parquet and stored HTML remain opaque at this boundary.
4. Serialize one deterministic, bounded `datapackage.json` byte sequence in
   bundle path order and require an exact JSON round trip.
5. Return in-memory evidence only. The boundary does not write or publish the
   descriptor, execute a CLI, inspect authenticity, or run a stored recipe.
6. Keep the profile internal. Pin Python Frictionless CLI 5.19.0 in a dedicated
   CI job and validate generated catalog, fetched, and harmonized CSV package
   trees against the v1 standards mode.
7. Report CLI validation honestly as external acceptance evidence rather than
   mutable runtime state: the in-memory result records `cli_validated = FALSE`
   even though its profile is covered by the separate pinned CI gate.

## Acceptance criteria

- Catalog, fetched, and harmonized fixed bundles produce deterministic mixed-
  profile Data Package v1 descriptors in exact resource-path order.
- Declared resource byte lengths and SHA-256 hashes match the source bundle;
  byte, descriptor, metadata, or identity forgery fails closed.
- Every canonical CSV schema contains the exact stored columns and declares
  each field as a string; non-CSV resources are explicitly generic.
- Descriptor construction invokes no network, DNS, cache, writer, snapshot,
  publication, refresh, or replay seam.
- A dedicated CI job validates the three fixed CSV package stages with pinned
  Python Frictionless CLI 5.19.0 in standards-v1 mode with no errors or warnings.

## Consequences

- The repository has executable Frictionless validation for the fixed CSV
  package profile without changing public APIs or manifest shapes.
- Public descriptor publication and end-to-end CLI acceptance of optional
  mixed-resource Parquet/report packages remain later M9 work.
- Live refresh and procedural replay remain gated on a complete, reviewed
  request-recipe contract and are not implied by this compatibility profile.
