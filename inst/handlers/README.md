# Fetch-handler registry

`registry.yml` contains only portable facts used to classify a distribution.
It is ordered by numeric precedence and uses first-match semantics.
`implementations-r.json` maps those exact classifier identities to R
implementation and optional-package metadata. The runtime loader reads both as
bounded regular non-symlink UTF-8/LF files, rejects ambiguous YAML features,
validates their one-to-one contract, and retains the SHA-256 of each exact asset.

Neither asset grants permission to fetch a URL: the request layer must still
apply DNS and redirect revalidation plus scheme, host, payload, page, and
request budgets.

Every implementation must expose the package protocol
`probe -> plan -> fetch -> normalize`. A missing Suggests package eventually
yields `skipped_missing_pkg`; `unknown` is `reference_only`.

The internal M7a plan is deliberately narrower. It reclassifies admitted
catalog URLs, emits one row per unique distribution plus ordered parameter
rows, and records every implementation as planned and non-replayable. Its
request list is empty and `execution_ready` is false. M7a does not probe
packages, call implementations, resolve DNS, use the network or cache, or write
files. Request construction, execution, runtime registration, and serialization
remain later M7 work.

The separate internal M7b report checks only host package metadata for selected
handlers. Its built-in probe checks each unique allowlisted package once using a
bounded direct read of installed `DESCRIPTION` identity and version; it ignores
`Meta/package.rds` and never loads a namespace or inspects or calls a symbol.
Missing and old packages become explicit skip statuses, but a present package
or satisfied minimum version still yields `blocked_implementation_planned`.
The report is host-specific, advisory, non-replayable, and never
execution-ready. Future execution must repeat package and symbol checks at the
point of invocation; provider request planning remains fixture- and
transport-contract gated under ADR 0021.

ADR 0022 implements the separate internal M7c `gx_csv_get_intents` S3 contract
0.1.0. It embeds the byte-identical M7a plan, records an exact shared policy,
emits one inert row for each selected CSV distribution in global fetch order,
and retains exact coverage for every distribution. Policy fixes GET,
`Accept: text/csv, application/csv;q=0.9`, `Accept-Encoding: identity`, and an
empty body, with credential, redirect, cache, and parser behavior unbound. Each
intent stores the declared media type and only a redacted canonical URL. The
full offline-canonical target is re-derived from the embedded plan and bound,
with every policy field, by `gx_contract_hash()` without being copied to the
intent table. M7c does not consult M7b or `readr`, allocate request/byte/parser
budgets, or authorize DNS, redirects, transport, cache, parsing, execution,
serialization, or replay. The CSV implementation therefore remains `planned`;
M7d below binds a response shape, M7e validates caller-supplied response
candidates, and M7f parses the exact admitted bytes under a package-owned
profile. Provider transport/provenance and runtime handler invocation remain
later M7 work.

ADR 0023 implements the separate internal M7d `gx_csv_request_plan` S3 contract
0.1.0. It embeds M7c byte-for-byte, requires explicit response-byte, row, and
column ceilings, and reserves M7a's physical-attempt and encoded/decoded-byte
budgets across every selected distribution in global fetch order. Non-CSV rows
inside the reserved prefix retain held shares; only reserved CSV intents emit
`planned_non_executable` logical request plans. The policy binds GET, an opaque
source target with no added credentials, zero redirects and retries, one
possible physical attempt, cache bypass, status 200, CSV response media,
identity response encoding, and shape limits. M7d allocates but does not consume
budgets, and it implements no DNS, transport, response validator, CSV parser,
result schema, attempt ledger, timeout policy, serialization, execution, or
replay. Runtime package and symbol preflight remains required immediately
before any future invocation.

ADR 0024 implements the separate internal M7e `gx_csv_validated_response` S3
contract 0.1.0. It embeds M7d byte-for-byte and validates one exact in-memory
candidate against one planned direct-CSV logical request: status 200, bounded
singleton critical headers, an admitted CSV base media type, identity content
encoding, optional exact Content-Length, the re-derived canonical no-redirect
target, and all three response-byte ceilings. Arbitrary headers and the full
target are discarded; the exact bounded raw body and its SHA-256 remain bound
to normalized validation facts. The input origin is explicitly
`caller_supplied`: no provider response is considered observed, no request or
attempt occurred, no budget was consumed, and no CSV parser ran. M7e performs
no DNS, network, redirect, cache, package, handler, clock, parser, or write
work and grants no transport, execution, serialization, or replay authority.

ADR 0025 implements the separate internal M7f `gx_csv_parsed_response` S3
contract 0.1.0. It embeds M7e byte-for-byte and parses only M7e's exact retained
body through one package-owned strict UTF-8 comma/header profile. An explicit
total-field ceiling and fixed input, scalar, header, row, and column limits are
checked by a raw-byte lexical pass before allocating an exact character-only
schema and table. Empty cells remain empty strings; no missing-value conversion,
type inference, trimming, comments, quoted record terminators, or name repair
is allowed. M7f records parser/result validation but retains caller-supplied
origin and denies provider observation, physical-attempt or ledger provenance,
fetch-budget consumption, transport, execution, serialization, and replay.
It does not load `readr`: the registry's planned runtime implementation remains
gated on an actual package/version/symbol check coupled to future invocation.

ADR 0028 implements the first internal non-CSV execution slice without
changing the registry's portable planned state. `gx_oaf_request_plan` accepts
only one selected, query-free, canonically encoded OGC API Features items URL
with a matching M7d held reservation. It adds fixed `f=json` and `limit`
parameters and binds one GET, one attempt, the held byte ceilings, identity
encoding, status/media expectations, and no credentials, redirects, retries,
cache, or next-page follow. `gx_oaf_execution` resolves the declared native
`gx_handler_oaf` symbol again immediately before invocation, performs one
DNS-pinned request, strictly parses the bounded FeatureCollection to `sf`, and
binds the retained body and result to one charged attempt. Missing-symbol,
changed-target, excessive-feature, and forged-evidence fixtures fail closed.
M7i remains internal; provider filters, queryables, pagination, cross-handler
orchestration, registration, serialization/replay, and public fetch remain
planned.

ADR 0029 joins direct CSV and OGC API Features in the internal M7j global
scheduler with shared count/byte admission, continue-on-error execution,
handler-specific compact evidence, and one exact status per distribution.

ADR 0030 adds the internal M7k WQP Result slice and upgrades that scheduler to
contract 0.2.0. WQP planning admits only one exact `siteid`, optional
`characteristicName`, and `mimeType=csv`, then records the selected Result/
`narrowResult` service profile and the exact M7d UTC interval. geoconnexr owns
one DNS-pinned request with no cache, redirect, or retry. At invocation it
resolves the exported optional `dataRetrieval::importWQP` parser and applies it
offline with type conversion disabled; its normalized character table must
equal the package's strict bounded CSV result. Missing capability occurs before
transport, while charged transport/parse failures remain isolated. M7k retains
bounded response bytes so compact results can be revalidated without loading
the optional package later. The registry remains portable and planned; this
internal execution proof does not register or export a public WQP handler.

ADR 0031 adds the internal M7l EDR position slice and upgrades the scheduler to
contract 0.3.0. Planning admits exactly one safe collection `position` endpoint,
one two-dimensional WKT `POINT`, one parameter, the exact M7d UTC interval,
CRS84, and CoverageJSON. At invocation, geoconnexr requires `edr4r >= 0.1.1`
and resolves both `edr_position` and `covjson_to_tibble` before provider work.
The package owns one DNS-pinned, byte-capped request with no cache, redirect, or
retry, then requires the offline external normalizer to exactly match its strict
bounded CoverageJSON PointSeries table. Capability failure charges no attempt;
transport or parse failure is isolated from later handlers. Compact results can
be fully revalidated without loading `edr4r` again. The registry remains
portable and planned, and no public EDR or fetch API is registered.

Current USGS Water Data API distributions are tested before the generic OGC API
Features classifier. Legacy NWIS IV/DV URLs are compatibility-only and produce a
deprecation warning. The M7l EDR subset records the base URL, collection,
position query, point, parameter, datetime, CRS, and response format before
fetching.

ADR 0032 adds the internal M7m current USGS continuous slice and upgrades the
scheduler to contract 0.4.0. It accepts only the official HTTPS
`/ogcapi/{version}/collections/continuous/items` endpoint with one exact USGS
site, one five-digit parameter code, and the M7d UTC interval. Planning fixes
the property list, GeoJSON representation, omitted geometry, language, and a
bounded single-page limit. Invocation checks `dataRetrieval >= 2.7.22` and the
exported `read_waterdata_continuous()` function before provider work, but
geoconnexr owns the one DNS-pinned, identity-encoded, no-cache/no-redirect/
no-retry request and strict parser so the M7d ledger remains authoritative.
Measurement values stay strings, next-page presence is explicit truncation,
and no next link is followed.

ADR 0033 adds the internal M7n current USGS daily slice and upgrades the
scheduler to contract 0.5.0. It requires one exact site, parameter, statistic,
and closed local-date interval on the official `daily/items` endpoint.
Invocation checks exported `read_waterdata_daily()` before the same single
package-owned request boundary. The fixed table preserves measurement strings,
stores observation time as `Date`, stores `last_modified` as UTC `POSIXct`, and
treats an absent `numberMatched` as unknown. Latest and legacy USGS paths,
other EDR queries, registration, replay, and a public fetch API remain planned.
