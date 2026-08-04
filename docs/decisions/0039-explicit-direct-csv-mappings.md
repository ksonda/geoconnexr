# ADR 0039: Harmonize direct CSV only through explicit embedded mappings

- Status: Accepted experimental policy
- Date: 2026-07-28
- Owners: package maintainers
- Related: ADR 0025, ADR 0027, ADR 0036

## Context

The direct-CSV fetch path intentionally accepts schema-free character tables.
Unlike EDR, current USGS, or the bounded WQP subset, a CSV column named
`value`, `date`, or `unit` has no package-level semantics. Guessing roles from
common names would make harmonization depend on undocumented provider
conventions.

The fetched object and plan already supply a stable distribution identity,
handler classification, catalog variable/unit facts, exact native table, and
bounded row/column shape. A caller who knows a particular CSV schema can
therefore provide the missing column-role declaration without changing fetch
execution.

## Decision

Add public `gx_csv_mapping()` and upgrade `gx_harmonized` to contract 0.4.0.

One mapping:

1. binds exactly one planned direct-CSV distribution SHA-256;
2. names distinct datetime, value, and native unit-label columns;
3. optionally names one qualifier column;
4. declares at most 16 exact bounded missing-value tokens for value and
   qualifier fields;
5. accepts only strict UTC timestamps spelled `YYYY-MM-DDTHH:MM:SSZ`; and
6. carries a deterministic mapping identity over every declared fact.

`gx_harmonize()` accepts one mapping or a list with at most one mapping per
distribution. It normalizes and embeds the list in the returned object.
Whole-object validation revalidates each mapping, rebinds it to a planned
`csv` handler, and re-derives observations from the exact fetched table.

Extraction additionally requires exactly one catalog parameter with variable
and unit URIs. Every native unit label must exactly corroborate the catalog
unit label before the existing reviewed conversion rules apply. Declared
missing values become numeric missing values while `original_value` retains
the exact source text.

Missing columns, non-character mapped fields, invalid timestamps, multiple
catalog parameters, unit conflicts, and unmapped CSV resources remain
native-only or visibly unchanged under the existing observation status
contract. The package does not partially interpret an incompatible table.

## Acceptance criteria

- A mapped fixture produces UTC observations with original values, qualifiers,
  catalog variable identity, unit identity conversion, and source order.
- An explicitly declared `NA` token becomes a missing numeric value while the
  original text remains `NA`.
- Other direct-CSV distributions in the same fetched object remain native-only.
- Duplicate distribution mappings and mappings bound to non-CSV handlers fail.
- Missing mapped columns or invalid timestamps leave the resource native-only.
- Mapping, observation, resource, fetched, target, or metadata forgery fails
  whole-object validation.
- Harmonization performs no DNS, transport, cache, optional-package, or
  filesystem-write work.

## Consequences

- Known direct-CSV schemas can participate in M8 without a provider registry or
  column-name heuristics.
- Mapping declarations travel with the harmonized object and remain
  independently inspectable.
- This first contract is deliberately single-variable and UTC-only. Per-row
  variable columns, local-time CSVs, wider/longer reshaping, reusable provider
  mapping registries, and general OGC Features observations remain later work.
