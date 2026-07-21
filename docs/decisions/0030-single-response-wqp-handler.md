# ADR 0030: Execute one WQP Result response under the held M7d reservation

- Status: Accepted internal substrate
- Date: 2026-07-20
- Owners: package maintainers

## Context

M7d reserves one physical attempt and bounded encoded/decoded capacity for each
selected handler. M7j proved that direct CSV and OGC API Features can share
those reservations, one global scheduler, and one exact status contract. WQP
still remained `handler_unimplemented`, despite its selected fixture occupying
fetch order 4 between CSV and OGC.

The R implementation registry originally named
`dataRetrieval::readWQPdata()`. The current function is useful as a general WQP
client, but its internal transport enables retries and can perform additional
attribute retrieval unless configured carefully. That hidden transport cannot
produce the package-owned one-attempt ledger and enforced transfer-byte ceiling
required by M7d. Calling it and estimating bytes after return would overstate
the reservation proof.

The exported `dataRetrieval::importWQP()` function provides a narrower boundary:
it can parse an already retained WQP CSV response with type conversion disabled.
The WQP service itself documents Result requests using `siteid`, optional
`characteristicName`, `startDateLo`, `startDateHi`, and `mimeType=csv`. M7k can
therefore keep transport inside geoconnexr while retaining an invocation-time
optional-package capability check and parser agreement.

## Decision

Add the unexported `gx_wqp_request_plan` and `gx_wqp_execution` S3 contracts,
both version 0.1.0. Upgrade the unexported `gx_fetch_orchestration` contract to
0.2.0 so WQP joins CSV and OGC in the existing global scheduler. Update the R
implementation registry's WQP function from `readWQPdata` to `importWQP`; its
portable classifier and `planned` availability remain unchanged.

### Exact request planning

Planning requires one valid M7d plan and one selected `wqp` distribution with:

- an exact `handler_reserved` coverage row;
- a matching `held_deferred_handler` reservation;
- one physical-attempt reservation;
- finite encoded and decoded byte ceilings; and
- a whole-UTC-day time range that WQP date filters can represent without
  widening the request.

The source must canonicalize safely to
`https://[www.]waterqualitydata.us/data/Result/search`. Fragments, duplicate
query names, and every inherited query field except `siteid`, optional
`characteristicName`, and `mimeType=csv` are rejected. M7k accepts one site and
at most one characteristic. It does not copy credentials, arbitrary provider
filters, or an opaque query into its owned request facts.

The deterministic target records and binds:

- service `Result` and profile `narrowResult`;
- the exact site identifier;
- either one source-declared characteristic or explicit `not_supplied` status;
- the exact M7d UTC interval plus WQP `MM-DD-YYYY` bounds;
- `mimeType=csv`, `dataProfile=narrowResult`, `count=no`, and `sorted=no`;
- GET with `Accept: text/csv` and identity response encoding;
- no body, credentials, redirect, retry, cache, or pagination follow;
- the held attempt/encoded/decoded ceilings and M7d row/column ceilings; and
- one explicit strict-parser field ceiling.

An absent characteristic remains absent. M7k does not infer a WQP characteristic
from an unreviewed catalog variable label or URI.

### One package-owned transport attempt

Immediately before provider work, execution resolves the exported
`dataRetrieval::importWQP` symbol. A missing package, namespace, symbol, or
callable fails as `gx_error_wqp_execution_capability` before DNS or transport
and consumes no attempt or bytes.

After successful resolution, the native `gx_handler_wqp` performs the provider
request through geoconnexr's request core. The client is DNS-pinned, has a
transfer ceiling equal to the held response limit, uses identity encoding,
bypasses cache, rejects changed final URLs, and fixes retries to zero. It makes
at most one physical attempt. The response must be status 200, `text/csv` or
`text/plain`, identity encoded, internally length-consistent, within every byte
ceiling, and bound to the exact canonical target.

### Dual offline parser agreement

The retained raw body is parsed twice after the response envelope is admitted:

1. `dataRetrieval::importWQP(obs_url = <retained text>, csv = TRUE,
   convertType = FALSE, tz = "UTC")`; and
2. geoconnexr's strict bounded UTF-8 CSV parser.

The external parser receives response text, not a URL or connection, so it
cannot initiate provider transport. Messages are suppressed and warnings or
errors fail the parse phase. Its result must be a bounded, character-only table.
Literal WQP `NA` values normalize back to the exact character `"NA"` because
M7k disables type inference and missing-value inference. Both results apply
WQP's slash-to-dot column-name normalization, rejecting any resulting empty or
duplicate name, and must be byte-for-byte identical as tibbles.

Parser disagreement, malformed UTF-8/CSV, excessive rows, columns, fields, or
field bytes, and non-character external output fail as one charged parse-phase
attempt. A successful result retains the raw body, exact character table,
schema, body/result hashes, row/column/field counts, parser-agreement fact,
implementation facts, execution facts, and one attempt row.

### Compaction and independent revalidation

The 0.2.0 scheduler derives WQP alongside CSV and OGC, sorts all candidates by
the original M7d `fetch_order`, and admits them through the same count and
aggregate reserved-byte pass. WQP receives one domain-separated child scope.
Capability, transport, and parse failures map to `wqp_capability_failed`,
`wqp_transport_failed`, or `wqp_parse_failed`; unrelated later candidates
continue.

A successful compact WQP result drops the repeated M7d-to-M7a chain but retains
the bounded raw response and all result/attempt evidence. Whole-object
validation rebuilds the exact WQP request plan from the shared M7d object,
reparses the raw body with the strict package parser, reconstructs a complete
WQP execution, and rebinds result, scope, attempt, byte, count, status, and
metadata identities. It does not reload or reinvoke `dataRetrieval`; the
execution-time agreement fact and deterministic strict result are sufficient
for later in-memory validation.

### Authority boundary

M7k remains internal, host-specific after live work, non-replayable, and not
generally execution-ready. The R registry remains `planned`; this ADR does not
register a public handler or export `gx_fetch()`. M7k implements one Result CSV
response only. It does not implement WQP WQX3, Station or other services,
multiple sites or characteristics, provider selection, POST, pagination,
automatic characteristic mapping, portable serialization/replay, or the final
fetched-result schema.

## Acceptance criteria

- Offline planning deterministically records Result/`narrowResult`, one site,
  optional characteristic status, exact UTC/WQP date ranges, redacted targets,
  strict shape limits, and the held attempt/byte reservation.
- Extra or duplicate query fields, fragments, foreign endpoints, non-CSV intent,
  ambiguous multi-value filters, and partial-day ranges fail closed without
  host or provider work.
- Invocation resolves `dataRetrieval::importWQP` before DNS and transport; a
  missing symbol consumes zero attempts and bytes.
- One fixture request is DNS-pinned, byte-capped, identity encoded, cache-free,
  redirect-free, retry-free, and represented by exactly one charged attempt.
- The external `convertType = FALSE` result and strict CSV result must match
  exactly after reviewed WQP name/missing-value normalization.
- Redirect, response-envelope, strict parse, external parse, parser mismatch,
  row/column/field, and byte-limit failures are typed and redacted.
- Compact WQP evidence revalidates without reinvoking the optional package;
  forged bytes, cells, schema, parse, implementation, execution, attempt, or
  metadata facts fail closed.
- Global dry run plans CSV, WQP, and OGC in fetch order without performer, DNS,
  clock, throttle, cache, filesystem, resolver, or parser work.
- WQP capability/transport/parse failure does not prevent a later admitted OGC
  request, and every distribution retains one exact terminal status.
- M7k constructors, handler, validator, scheduler, and public `gx_fetch()`
  remain unexported.

## Consequences

- M7 now has three fixture-backed execution paths under one scheduler and exact
  all-distribution status contract.
- The optional-package boundary is coupled to invocation without delegating
  attempt or byte authority to opaque external transport.
- The WQP plan honestly exposes an absent characteristic rather than claiming a
  catalog-variable mapping that has not been reviewed.
- M7 is not complete. EDR, current/legacy USGS adapters, multi-response budgets,
  registration, serialization/replay, and a public fetched-result schema remain
  gates before M8 harmonization begins.

## Primary references

- [dataRetrieval `readWQPdata()` reference](https://doi-usgs.github.io/dataRetrieval/reference/readWQPdata.html)
- [dataRetrieval package index and `importWQP()` parser](https://doi-usgs.github.io/dataRetrieval/reference/index.html)
- [Water Quality Portal Web Services Guide](https://www.waterqualitydata.us/webservices_documentation/)
