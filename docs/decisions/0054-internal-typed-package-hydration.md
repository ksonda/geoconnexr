# ADR 0054: Hydrate only fixed package-owned table schemas

- Status: Accepted internal substrate
- Date: 2026-08-03
- Owners: package maintainers
- Related: ADR 0046, ADR 0048, ADR 0049, ADR 0052, ADR 0053

## Context

M9o exposes every verified package CSV as a canonical character-only table.
That is lossless, but callers still need fixed storage types for package-owned
catalog, fetch-index, and harmonization tables. Applying general type inference
would be ambiguous around blank cells, redacted identifiers, provider-native
tables, and exact numeric or timestamp encodings. Reconstructing live
`gx_catalog`, `gx_fetched`, or `gx_harmonized` objects would also overstate the
lineage and replay evidence retained by the fixed package profile.

## Decision

Add the internal M9p `gx_package_hydrated` substrate.

The substrate:

1. starts only from a fully validated M9o `gx_package_tables` value;
2. reuses the M9h fixed redacted catalog typing rules for CRS84 point geometry,
   UTC timestamps, logicals, and canonical conforms-to arrays;
3. rebinds `requests.csv` byte-for-byte to the manifest's authoritative typed
   request ledger;
4. applies explicit, column-named integer, double, logical, and UTC timestamp
   conversions to the fixed fetch-status, native-index, observation, and
   harmonized-resource schemas;
5. accepts only the writer's exact canonical scalar encodings, including blank
   as missing only for explicitly typed columns;
6. leaves provider-native CSV tables character-only and raw provider resources
   opaque in the embedded M9n/M9o evidence; and
7. returns a fully revalidated internal value object binding the M9o view,
   typed projections, scope metadata, and a deterministic hydration identity.

The result remains redacted, offline, read-only, unsigned, non-Frictionless,
and non-replayable. It does not reconstruct any live workflow object or expose
a public hydration API.

## Acceptance criteria

- Catalog, fetched, and harmonized package stages hydrate exactly the tables
  permitted by their fixed profiles.
- Package-owned integer, double, logical, geometry, UTC timestamp, and JSON
  fields receive their exact declared storage types.
- Noncanonical numeric or logical text, wrong schemas, request-ledger
  disagreement, and forged projections, metadata, or identities fail closed.
- Provider-native tables and raw resources are never semantically inferred.
- Hydration performs no network, DNS, cache, optional-package, write,
  overwrite, report, refresh, or replay work.
- No new export is added.

## Consequences

- The package now has an evidence-preserving typed loading substrate without
  conflating stored views with live workflow state.
- A later public boundary can expose this substrate after naming and user-facing
  access semantics are settled.
- Overwrite ownership, Arrow/Parquet, Quarto, Frictionless validation, reports,
  authenticity, refresh, and replay remain later M9 work.
