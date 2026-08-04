# ADR 0048: Rebind explicit catalog lineage before package serialization

- Status: Accepted internal substrate
- Date: 2026-07-29
- Owners: package maintainers
- Related: ADR 0019, ADR 0034, ADR 0036, ADR 0042, ADR 0047

## Context

M7 and M8 freeze exact `gx_fetched` and `gx_harmonized` value objects. Those
objects retain validated fetch-plan, execution, native-payload, mapping, and
harmonization lineage, but deliberately do not embed the complete originating
`gx_catalog`. A fetch plan binds the catalog AOI and dataset projection, not
the complete sites, problems, requests, and catalog metadata required for a
package.

Package serialization must not fabricate those missing resources, infer a
catalog from fetched distributions, or silently accept an unrelated catalog.
The input relationship needs an exact boundary before any new writer,
publication, optional format, or public API is added.

## Decision

Add the internal M9j `gx_package_input_impl(x, catalog = NULL)` boundary and
exact `gx_package_input` value object.

The boundary:

1. admits one exact `gx_catalog`, `gx_fetched`, or `gx_harmonized` object;
2. treats a catalog input as self-contained and rejects a second catalog;
3. requires an explicit validated catalog for fetched and harmonized inputs;
4. re-derives the fetch-plan source from that catalog and requires exact AOI,
   dataset hash, completeness, truncation, contract-version, and row-count
   agreement;
5. retains the exact catalog, fetched object, and, where applicable,
   harmonized object without stripping native payloads;
6. binds canonical redacted catalog-resource hashes, fetch-plan facts,
   fetched status and result identities, reviewed target/mapping evidence,
   harmonized resource facts, counts, stage, and a deterministic input
   identity; and
7. revalidates the complete lineage and all derived evidence during
   whole-object validation.

The result declares `package_input_v1`, the exact end stage, whether lineage
was self-contained or explicitly rebound, and that serialization, publication,
Frictionless metadata, Arrow, Quarto, refresh, and replay remain deferred.
Construction is offline and read-only.

## Acceptance criteria

- Exact catalog, populated fetched, and populated harmonized inputs produce
  distinct validated package-input objects for their three stages.
- Catalog-only evidence binds the four existing canonical redacted catalog
  projections without writing them.
- Fetched and harmonized stages require the explicit source catalog and reject
  a valid but mismatched catalog.
- Native fetched payloads and the exact harmonized object remain embedded.
- Catalog, fetched, harmonized, evidence, metadata, stage, count, or identity
  forgery fails closed.
- Empty logical columns normalize to canonical character columns without
  changing existing CSV bytes.
- Admission invokes no network, DNS, snapshot writer, removal, serialization,
  publication, repair, refresh, or replay seam.
- No public API is added.

## Consequences

- Future fetched/harmonized writers receive a single validated lineage object
  instead of independently interpreting three public contracts.
- The existing frozen `gx_fetched` contract does not need to be reopened merely
  to embed a full catalog.
- A future public package API must either obtain or explicitly request the
  source catalog for fetched/harmonized values; it cannot honestly reconstruct
  one from the current fetch object alone.
- Deterministic observation/native-resource serialization, staging and
  publication, loading, optional formats, Frictionless acceptance, reports,
  refresh, and replay remain later M9 contracts.
