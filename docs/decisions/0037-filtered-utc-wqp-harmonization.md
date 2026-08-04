# ADR 0037: Harmonize only exact filtered UTC WQP results

- Status: Accepted experimental policy
- Date: 2026-07-27
- Owners: package maintainers
- Related: ADR 0030, ADR 0036

## Context

ADR 0036 left every Water Quality Portal Result payload native-only because one
response can contain multiple characteristics, catalog rows do not inherently
bind those native names to variable URIs, and activity dates do not by
themselves identify UTC instants.

The WQP narrow Result profile nevertheless provides the fields needed for a
smaller safe boundary: monitoring-location identifier, activity start date,
activity start time and zone, characteristic name, result value, measure unit,
and optional measure qualifier. The WQP request plan also retains an exact
`siteid` and an optional exact `characteristicName` filter.

## Decision

Upgrade `gx_harmonized` to contract 0.2.0 and admit one WQP subset.

A WQP result becomes a time-series resource only when:

1. its source request contains exactly one nonempty `siteid` and one nonempty
   `characteristicName`;
2. the fetch plan contains exactly one catalog parameter for the distribution;
3. the catalog variable name exactly equals the requested characteristic;
4. every native monitoring-location identifier and characteristic name exactly
   equals the corresponding request fact;
5. every row contains strict `YYYY-MM-DD` and `HH:MM:SS` activity fields and
   the literal time-zone code `UTC`; and
6. the existing ADR 0036 variable-URI and unit-label corroboration rules hold
   before any reviewed unit conversion is applied.

The normalized observation keeps the WQP result text as `original_value`.
Blank and literal `NA` values are treated as missing for numeric conversion
while their original text remains accessible. `MeasureQualifierCode` is
retained when present. Rows are neither sorted nor deduplicated.

If any resource-level condition fails, the entire WQP result remains
native-only. The harmonizer does not partially select matching rows from an
unfiltered or mixed-characteristic response, infer a characteristic URI from a
name, or convert a local civil time using a guessed offset.

## Acceptance criteria

- An exact filtered UTC temperature fixture aligns through its one catalog
  variable URI and converts through the requested reviewed target unit.
- A literal WQP `NA` remains visible as original text and normalizes to a
  missing numeric value.
- Measure qualifiers, duplicate instants, source order, the exact fetched
  object, and retained raw response bytes are preserved.
- Missing characteristic filters, non-UTC zones, invalid civil dates, mixed
  characteristics, site mismatches, and incomplete schemas remain native-only.
- Whole-object validation deterministically re-derives the WQP decision and
  rejects forged observations, resources, metadata, targets, or fetched facts.
- Harmonization performs no DNS, transport, cache, optional-package, or
  filesystem-write work.

## Consequences

- WQP joins the normalized observation boundary for one explicit, auditable
  subset without treating the provider's labels as standalone semantic URIs.
- Local-time WQP observations remain losslessly available but are not assigned
  guessed instants.
- Broad WQP characteristic vocabularies, reviewed timezone-code conversion,
  mixed-characteristic partitioning, direct-CSV mappings, and general OGC
  Features observation mappings remain later work.

## Sources reviewed

- [WQP Web Services Guide](https://www.waterqualitydata.us/webservices_documentation/)
- [EPA WQX 3.0 outbound schema](https://www.epa.gov/system/files/other-files/2025-07/schema_outbound_wqx3.0.csv)
- [EPA WQX 3.0 profile matrix](https://www.epa.gov/system/files/other-files/2025-09/schema_outbound_wqx3.0_profiles_1.csv)
