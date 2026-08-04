# ADR 0036: Begin M8 with conservative reviewed-unit harmonization

- Status: Accepted experimental policy
- Date: 2026-07-27
- Owners: package maintainers
- Related: ADR 0034, ADR 0035

## Context

The public `gx_fetched` 0.1.0 boundary now provides stable handler-native
payloads and complete M7 provenance. M8 can therefore begin without reopening
fetch execution. The six supported handlers do not, however, expose one common
observation schema. Direct CSV is intentionally schema-free, WQP Result can
contain multiple characteristics without catalog URI alignments, and OGC API
Features can contain non-observation feature data.

EDR position and current USGS continuous and daily payloads already have strict
time, value, unit-label, and qualifier shapes. Their source catalog rows also
carry variable and unit URIs. Treating a provider label as a URI mapping on its
own would violate the roadmap, particularly when a provider unit label
conflicts with the catalog.

## Decision

Add public `gx_target_units()` and `gx_harmonize()` boundaries.

`gx_target_units()` selects exactly one reviewed target URI for each dimension
in the bundled `target-units-v1.csv` asset. Defaults are Celsius, metre, and
cubic metre per second. Callers may choose another reviewed URI, but cannot
introduce an unreviewed unit or cross dimensions.

`gx_harmonize()` accepts only a validated `gx_fetched` 0.1.0 object. This first
M8 contract:

1. extracts observations only from EDR position, current USGS continuous, and
   current USGS daily result tables;
2. requires exactly one catalog parameter with non-missing variable and unit
   URIs for the distribution;
3. requires the native unit label to exactly corroborate the catalog unit
   label before using the catalog unit URI;
4. applies only one exact directed reviewed affine rule, using
   `converted = original * scale + offset`;
5. normalizes instants and daily dates to UTC without sorting or
   deduplicating;
6. preserves qualifiers, original value text, original unit facts, the exact
   `gx_fetched` object, and all retained raw bodies; and
7. indexes direct CSV, WQP Result, and OGC API Features results as native-only
   resources rather than guessing their semantics.

Missing values remain missing but can still have a successfully resolved unit
mapping. Invalid numeric text, ambiguous variables, conflicting labels,
unknown units, and unavailable direct rules remain visible with
`harmonized = FALSE`. Identity mappings are explicit and use no invented
conversion rule ID.

The result is a validated `gx_harmonized` 0.1.0 value object. Validation
revalidates the complete fetched object and target-unit asset identity, then
re-derives observations, resource indexes, and metadata. A call emits at most
1,000,000 normalized observation rows.

## Acceptance criteria

- Reviewed forward and reverse affine and multiplicative rules produce the
  expected values.
- A target URI from the wrong dimension or outside the reviewed asset fails
  before harmonization.
- Missing mappings, ambiguous catalog parameters, and catalog/native unit
  conflicts do not convert values.
- USGS qualifiers are preserved, daily dates become UTC midnight instants,
  and duplicate timestamps remain duplicate rows in source order.
- The embedded fetched object and native payloads remain byte-identical.
- Forged observations, resources, targets, metadata, or fetched provenance
  fail whole-object validation.
- No DNS, transport, cache, optional-package, or filesystem write occurs.

## Consequences

- M8 has a usable, bounded observation and unit-conversion boundary over the
  strict time-series handlers.
- Native-only results remain losslessly accessible without false semantic
  claims.
- Reviewed WQP characteristic alignments, declarative direct-CSV mappings,
  broader variable URI alignment, and harmonized snapshot packaging remain
  later M8/M9 work.
