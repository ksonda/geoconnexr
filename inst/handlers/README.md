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

Current USGS Water Data API distributions are tested before the generic OGC API
Features classifier. Legacy NWIS IV/DV URLs are compatibility-only and produce a
deprecation warning. EDR plans must record the base URL, collection, query verb,
geometry/location, parameter, datetime, and response format before fetching.
