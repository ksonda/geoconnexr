# ADR 0040: Harmonize OGC Features only through explicit property mappings

- Status: Accepted experimental policy
- Date: 2026-07-28
- Owners: package maintainers
- Related: ADR 0028, ADR 0036, ADR 0039

## Context

The bounded OGC API Features fetch path retains one GeoJSON FeatureCollection
as an `sf` object plus its exact raw response bytes. Feature properties have no
general observation semantics: common names such as `time`, `value`, or `unit`
are provider conventions rather than an OGC contract. Feature identifiers and
geometry likewise identify or locate features; they do not establish
observation roles.

The fetched object and plan already bind the result to one distribution, site,
handler, catalog variable/unit declaration, and strict native feature table. A
caller who knows that collection's property schema can therefore declare the
remaining roles without changing fetch execution or inferring semantics from
geometry.

## Decision

Add public `gx_feature_mapping()` and upgrade `gx_harmonized` to contract
0.5.0.

One mapping:

1. binds exactly one planned `ogc_api_features` distribution SHA-256;
2. names distinct datetime, value, and native unit-label properties;
3. optionally names one qualifier property;
4. declares at most 16 exact bounded missing-value tokens for character value
   and qualifier properties;
5. accepts only strict UTC timestamps spelled `YYYY-MM-DDTHH:MM:SSZ`;
6. excludes generated contract, feature-ID, ID, and geometry columns from
   observation roles; and
7. carries a deterministic mapping identity over every declared fact.

`gx_harmonize()` accepts one mapping or a list with at most one mapping per
distribution. It normalizes and embeds the mappings in the returned object.
Whole-object validation revalidates each mapping, rebinds it to the exact
planned handler, and re-derives observations from the retained `sf` table.

Extraction additionally requires exactly one catalog parameter with variable
and unit URIs. The native unit-label property must exactly corroborate the
catalog label before reviewed conversion rules apply. Site identity always
comes from the planned distribution, never from a feature ID or coordinates.
Character and finite numeric value properties are accepted; original values,
qualifiers, source order, duplicate timestamps, the original `sf` object, and
raw GeoJSON remain preserved.

Missing properties, incompatible property types, invalid timestamps, multiple
catalog parameters, and unmapped collections remain native-only. Unit
conflicts and unavailable conversions remain visible through the existing
unchanged observation statuses. No incompatible collection is partially
interpreted.

## Acceptance criteria

- A mapped fixture produces UTC observations with original values, qualifiers,
  catalog variable/site identity, unit identity conversion, and source order.
- An explicitly declared `NA` token becomes a missing numeric value while the
  original text remains `NA`.
- Generated feature IDs and geometry cannot be assigned observation roles.
- Duplicate mappings and mappings bound to non-Features handlers fail.
- Missing mapped properties or invalid timestamps leave the resource
  native-only.
- Mapping, observation, resource, fetched, target, or metadata forgery fails
  whole-object validation.
- Harmonization performs no DNS, transport, cache, optional-package, or
  filesystem-write work.

## Consequences

- Known observation-oriented feature collections can participate in M8 without
  a provider registry or property-name heuristics.
- Mapping declarations travel with the harmonized object and remain
  independently inspectable.
- Geometry and generated identifiers remain available in the native `sf`
  payload without becoming semantic shortcuts.
- This first contract is deliberately single-variable, single-page, and
  UTC-only. Per-feature variables, local-time features, pagination, reusable
  provider mapping registries, and spatial site reconciliation remain later
  work.
