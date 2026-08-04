# ADR 0038: Normalize active WQX timezone codes through reviewed offsets

- Status: Accepted experimental policy
- Date: 2026-07-28
- Owners: package maintainers
- Related: ADR 0037

## Context

ADR 0037 admitted only the literal `UTC` code because converting an abbreviation
through the host timezone database or a site location would introduce
environment-dependent or inferred daylight-saving behavior.

The EPA WQX `TimeZone` domain supplies a different safe boundary. Each allowed
`TimeZoneCode` has an explicit numeric UTC offset. The reviewed domain response
contains 26 codes: 23 active values and three aliases whose names explicitly
mark them retired. The active set includes both standard and daylight codes,
plus half-hour Newfoundland offsets.

## Decision

Upgrade `gx_harmonized` to contract 0.3.0 and bundle
`inst/vocab/wqp-timezones-v1.csv`.

The asset records the 23 active WQX codes, display names, integer offsets in
minutes, source URL, review date, and reviewed status. It excludes `AHST`,
`BST`, and `YST`, which the domain response marks retired. Its exact bytes are
SHA-256 bound into every harmonized result.

For an otherwise admissible ADR 0037 WQP resource:

1. every `ActivityStartTime.TimeZoneCode` must exactly match a reviewed code;
2. the strict local `YYYY-MM-DD HH:MM:SS` text is parsed as a civil clock
   without consulting the host timezone database;
3. UTC is computed as `local clock - reviewed offset`; and
4. source order and duplicate resulting instants are retained.

The reported code is authoritative for the offset. The package does not infer a
zone from coordinates, decide whether a standard or daylight code should have
been used on that date, substitute a modern regional timezone, or accept a
retired alias. If any row has an unknown code, the entire WQP result remains
native-only.

## Acceptance criteria

- UTC, positive, negative, and half-hour reviewed offsets normalize
  deterministically without host timezone state.
- `EDT 08:34:56` and `NST 09:04:56` on the same date both normalize to
  `12:34:56Z`.
- Unknown and retired codes remain native-only.
- Invalid civil dates and times remain native-only.
- Every `gx_harmonized` object records the exact reviewed timezone-asset hash,
  and whole-object validation re-derives it.
- The embedded fetched object, raw payloads, original civil-time fields,
  qualifiers, and row order remain exact.

## Consequences

- Filtered WQP observations using any active WQX fixed-offset code can join the
  UTC observation table.
- The result is reproducible across operating systems and timezone-database
  releases.
- This is fixed-offset normalization, not geographic timezone interpretation.
  Broad WQP characteristic vocabularies, mixed-characteristic partitioning,
  direct-CSV mappings, and general OGC Features observation mappings remain
  later work.

## Sources reviewed

- [EPA WQX domain value service](https://cdx.epa.gov/WQXWeb/Services.asmx?op=GetDomainValues)
- [EPA WQX domain services overview](https://www.epa.gov/waterdata/storage-and-retrieval-and-water-quality-exchange-domain-services-and-downloads)
- [EPA WQX XML training manual](https://www.epa.gov/sites/default/files/2014-09/documents/wqx-xml-trainingmanual-2013.pdf)
