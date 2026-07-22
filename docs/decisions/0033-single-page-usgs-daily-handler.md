# ADR 0033: Execute one current USGS daily-values page

- Status: Accepted internal substrate
- Date: 2026-07-21
- Owners: package maintainers

## Context

M7m executes one current USGS continuous-data page under an M7d reservation,
but `usgs_waterdata_daily` distributions still end as
`handler_unimplemented`. Daily values differ semantically from continuous
observations: they are identified by a statistic code and their observation
time is a local calendar date, not a UTC instant.

The current USGS OGC API exposes daily values at
`/ogcapi/{version}/collections/daily/items`. The official
`dataRetrieval::read_waterdata_daily()` function is the reviewed R capability,
but its transport and paging do not expose the DNS-pinned, single-attempt ledger
owned by M7d. The provider may omit `numberMatched`; completeness must therefore
come from the absence of a next link and from a page smaller than the limit,
not from an invented match count.

## Decision

Add unexported `gx_usgs_daily_request_plan` and `gx_usgs_daily_execution` S3
contracts, both version 0.1.0. Upgrade unexported
`gx_fetch_orchestration` to 0.5.0 so daily values join CSV, WQP, EDR, current
USGS continuous, and OGC Features in the global scheduler.

The portable handler registry remains `planned`; this internal proof does not
register a public handler or expose `gx_fetch()`.

### Exact daily request planning

Planning requires one selected `usgs_waterdata_daily` distribution with its
matching M7d held reservation. The endpoint must be HTTPS on
`api.waterdata.usgs.gov` and exactly match the current `daily/items` path.

The inherited URL must contain exactly one `USGS-{alphanumeric}` monitoring
location, one five-digit parameter code, and one five-digit statistic code.
The plan derives one closed `YYYY-MM-DD/YYYY-MM-DD` interval from the M7d UTC
coverage bounds. Foreign hosts or paths, fragments, duplicates, extra filters,
multiple values, `continuous`, `latest-daily`, and conflicting inherited
options fail closed.

The request fixes the ten official properties, `skipGeometry=true`, `f=json`,
`lang=en-US`, and a limit no larger than 50,000 or the row/field ceilings. It
adds no credentials or body, follows no redirect or next page, performs no
retry, bypasses cache, and retains the exact M7d attempt and byte authority.

### Capability check and package-owned transport

Immediately before provider work, execution checks `dataRetrieval >= 2.7.22`,
the exported `read_waterdata_daily()` function, and its reviewed formals. A
missing or incompatible capability fails before DNS and consumes no attempt or
bytes.

geoconnexr owns the sole DNS-pinned, identity-encoded HTTP request and strict
response validation. The dataRetrieval function is capability-checked but is
not allowed to perform provider transport because that would surrender the
single-attempt and no-paging contract.

### Strict fixed GeoJSON result

Every accepted feature has an identity, null geometry, and exactly these
properties: `time_series_id`, `monitoring_location_id`, `parameter_code`,
`statistic_id`, `time`, `value`, `unit_of_measure`, `approval_status`,
`qualifier`, and `last_modified`.

Site, parameter, and statistic must equal the plan. `time` must be one valid
local `YYYY-MM-DD` value inside the closed requested interval and is stored as
an R `Date`. `last_modified` must be an RFC 3339 instant and is stored as UTC
`POSIXct`. Measurement values and other provider strings are preserved exactly;
a null qualifier becomes `NA_character_`.

`numberReturned` must equal the feature count. `numberMatched` may be absent,
null, or a consistent bounded whole number. A next link, a larger known match
count, or a full page records explicit truncation, and no next link is followed.
JSON duplicate-member, depth/member, UTF-8, row, column, field, count, type, and
semantic limits fail closed.

### Scheduling, compaction, and authority

The 0.5.0 scheduler derives daily requests at their original fetch order and
shares one count and aggregate-byte admission pass across all six implemented
handler families. Daily capability, transport, and parse failures map to
`usgs_daily_capability_failed`, `usgs_daily_transport_failed`, or
`usgs_daily_parse_failed` without stopping later candidates.

Compact successes retain exact response bytes, the fixed eleven-column table
and schema, parse/truncation facts, implementation facts, execution, and one
attempt. Whole-object validation rebuilds the request, reparses retained bytes,
and rebinds all identities and budgets without loading dataRetrieval.

M7n is internal, current-daily-only, single-page, host-specific after live
work, and non-replayable. It does not implement latest-daily, legacy NWIS,
API-key policy, pagination, registration, serialization/replay, a public fetched
result schema, or harmonization.

## Acceptance criteria

- Planning binds one exact site, parameter, statistic, local-date interval,
  property set, representation, limit, and held reservation without host work.
- Unreviewed paths, filters, duplicates, ambiguous values, and conflicting
  inherited options fail before execution.
- Capability failure occurs before DNS and charges no attempt or bytes.
- Exactly one bounded provider request is permitted, with no retry, redirect,
  cache, decompression ambiguity, or page follow.
- Parsing preserves value strings, distinguishes `Date` from UTC `POSIXct`,
  accepts an absent match count conservatively, and exposes truncation.
- Compact and full execution evidence revalidates from retained bytes; forged
  request, data, schema, parse, ledger, status, or metadata facts fail closed.
- A daily failure does not prevent a later OGC request, and dry run performs no
  host or provider work.
- M7n constructors, handler, validator, scheduler, and `gx_fetch()` remain
  unexported.

## Consequences

- M7 now has six fixture-backed execution paths under one scheduler.
- Daily local-date semantics are preserved rather than incorrectly converted
  to UTC instants.
- Latest and legacy USGS variants, paging, registration, replay, and the public
  fetched-result contract remain gates before M8 begins.

## Primary references

- [USGS Water Data APIs: OGC API](https://api.waterdata.usgs.gov/docs/ogcapi/)
- [`read_waterdata_daily()` reference](https://doi-usgs.github.io/dataRetrieval/reference/read_waterdata_daily.html)
- [USGS daily collection schema](https://api.waterdata.usgs.gov/ogcapi/v0/collections/daily/schema?f=html)
