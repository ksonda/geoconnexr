# ADR 0025: Parse validated direct-CSV bytes under a strict offline profile

- Status: Accepted internal substrate
- Date: 2026-07-16
- Owners: package maintainers

## Context

M7e retains the exact bounded raw bytes of one validated caller-supplied
direct-CSV response candidate. It deliberately proves only response-envelope
facts: empty and non-UTF-8 bodies may pass, and no delimiter, encoding, quoting,
header, missing-value, type, row, column, or result-schema semantics are
claimed.

The handler registry still describes `readr::read_csv` as a planned runtime
implementation and does not pin a minimum `readr` version. ADR 0021 requires an
actual package, version, and symbol check immediately before a future handler
invocation. Loading that optional package in a durable offline value object
would make this checkpoint host-specific without authorizing transport or
proving that a provider produced the bytes.

A narrower package-owned lexical profile can close the parser and result-shape
gates now. It must reject amplification before allocating a result table, avoid
locale and type-guessing behavior, and preserve M7e's explicit caller-supplied
provenance limit.

## Decision

Add the unexported internal M7f S3 class `gx_csv_parsed_response`, contract
version 0.1.0. It accepts one valid M7e object plus one explicit positive
`max_fields` ceiling and parses only the exact raw body retained by that M7e
object.

The exact top-level fields are:

```text
contract_version, validated_response, policy, schema, data, parse, metadata
```

`validated_response` is byte-identical after revalidation. Consequently M7d,
M7c, and M7a also remain unchanged and the nested M7a request list remains
empty. M7f does not accept a path, URL, connection, decoded text, alternate
body, delimiter, schema, locale, or type specification.

### Fixed parser policy

Contract 0.1.0 defines one narrow profile:

- strict shortest-form UTF-8 only;
- one optional UTF-8 BOM at the first three bytes, stripped before fields are
  materialized;
- comma delimiter and double-quote quoting;
- a doubled quote is the only quote escape;
- a required first header record with exact nonempty, case-sensitive unique
  names and no normalization or repair;
- LF and CRLF record terminators, including mixed use, with an optional final
  terminator;
- no embedded record terminator, bare carriage return, blank record, comment,
  whitespace trimming, missing-value token, type inference, or alternate
  encoding;
- exact rectangular width; and
- character storage for every cell, with an empty field represented by `""`
  rather than `NA`.

Quotes may occur only at field start. After a closing quote, only a delimiter,
record terminator, or end of input is accepted. Record terminators inside a
quoted field are rejected in this first profile. A zero-byte physical record is
blank and rejected, while a quoted empty field is an active record and is
allowed outside the header. Leading zeroes, `NA`, `#`, and surrounding spaces
are literal character data.

The exact policy fields are:

```text
slice_id, encoding, bom_policy, delimiter, quote, escape_policy,
header_policy, record_terminators, embedded_record_terminators,
comment_policy, blank_record_policy, trim_whitespace,
missing_value_policy, type_inference, storage_type, max_input_bytes,
max_field_bytes, max_header_name_bytes, max_header_bytes, max_fields,
request_max_rows, request_max_columns, implementation_max_rows,
implementation_max_columns, hash_chunk_fields
```

### Allocation and text boundaries

Before allocating a schema or data table, a package-owned raw-byte scanner
validates UTF-8, controls, BOM placement, quoting, record termination,
rectangularity, and every count and byte ceiling. It uses guarded whole-number
arithmetic and does not expand the body into an integer vector.

The fixed implementation ceilings are:

- 16 MiB of parser input bytes;
- 1 MiB of decoded UTF-8 bytes per field;
- 16 KiB per header name and 1 MiB across header names;
- 1,000,000 rows;
- 10,000 columns;
- an explicit caller-supplied `max_fields` no greater than 1,000,000, counting
  header and data cells; and
- 32 MiB of M7f-owned aggregate text.

The selected M7d request's row and column ceilings are copied into policy and
enforced independently of the implementation ceilings. The lower applicable
row and column ceiling wins. M7e has already enforced the selected encoded,
decoded, and response-byte limits; M7f's 16 MiB ceiling is an additional parser
allocation boundary. Exact-limit inputs pass and limit-plus-one inputs fail
before result allocation.

Subsequent bounded passes decode and recheck the header, allocate the known
result shape, and materialize fields. Whole-object validation repeats the scan
and materialization from the embedded raw body and rejects forged data, schema,
identities, attributes, policy, or metadata.

### Result and identity

`schema` is an exact tibble with columns:

```text
contract_version, column_index, column_name, storage_type
```

Every `storage_type` is `character`. `data` is an exact tibble whose names and
order equal `schema$column_name`; every column is an unclassed character vector
with no missing values. A valid header-only document produces zero data rows.

The exact `parse` fields are:

```text
parse_id, validation_id, body_sha256, result_sha256, bom_present, row_count,
column_count, field_count, parse_status
```

`parse_status` is `parsed_caller_supplied_validated_response`.
`result_sha256` is a domain-separated Merkle-style fingerprint: bounded
1,024-field chunks bind exact cell order and values, column fingerprints bind
exact names and row counts, and the result fingerprint binds ordered column
fingerprints and dimensions. Values use `gx_contract_hash()` length prefixes;
R serialization is not an identity input.

`parse_id` uses namespace `geoconnexr.csv-parse.v1` and binds the M7e validation
and body identities, every parser policy and ceiling, BOM presence, exact
counts, and `result_sha256`. It is not a logical request ID, physical-attempt
ID, ledger row, manifest request hash, provider-provenance proof, authenticity
proof, or serialized public-result identity.

### Metadata and blockers

Metadata keeps `host_specific`, `replayable`, `execution_ready`,
`transport_authorized`, `provider_response_observed`, and `budgets_consumed`
false. It sets `response_candidate_validated`, `parser_executed`,
`csv_semantics_validated`, and `result_contract_bound` true. The observation
origin remains exactly `caller_supplied`.

The outer blocker set removes only:

- `csv_parser_enforcement_unimplemented`;
- `csv_parser_semantics_unbound`; and
- `result_schema_unbound`.

The byte-identical embedded M7e metadata remains unchanged. M7f retains the
runtime package/symbol, planned-handler, attempt identity and ledger, response
origin, provider transport, timeout, non-CSV request, serialization, execution,
and replay gates.

M7f loads no optional parser package. Its parser opens no supplied path or body
file and performs no DNS lookup, network request, redirect, cache access,
package inspection, handler call, transport, clock read, or file write.
Revalidating the nested M7a plan may reread its bounded bundled handler assets.

M7f adds no public constructor, parser API, fetch API, transport path, runtime
handler readiness, serialization contract, execution path, or replay
authority.

## Acceptance criteria

- A pinned M7e known-answer body produces the exact parser policy, character
  schema and data, counts, result fingerprint, parse identity, and metadata;
  the complete M7e-to-M7a chain remains byte-identical.
- UTF-8 BOM/no-BOM, LF/CRLF, quoted commas, doubled quotes, literal leading
  zeroes, literal `NA` and `#`, surrounding spaces, trailing empty fields,
  quoted empty data, header-only data, and valid multibyte UTF-8 follow the
  fixed policy exactly.
- Empty/BOM-only bodies, invalid/overlong/surrogate/out-of-range/truncated
  UTF-8, NUL/control/format characters, misplaced BOMs, bare CR, embedded
  terminators, blank records, malformed quote transitions, empty/duplicate
  headers, and ragged rows fail under typed trace-redacted conditions.
- Selected and implementation row/column ceilings, input bytes, decoded field
  bytes, header bytes, and explicit total fields pass at the exact limit and
  fail at limit plus one before result allocation.
- Result and parse identities rebind exact names, values, dimensions, policy,
  limits, BOM presence, M7e validation, and body digest without changing under
  locale or numeric display options.
- Forged nested contracts, policy fields, schema/data values, attributes,
  counts, hashes, metadata, blockers, or authority flags fail whole-object
  revalidation.
- Conditions and printing expose no body values or full query-bearing target.
- Pinned fixtures say explicitly that parser consistency is not provider,
  attempt, transport, authenticity, budget-consumption, serialization, or
  replay provenance.
- No M7f parser, result, fetch, execution, serialization, or replay API is
  exported.

## Consequences

- M7 now has a deterministic offline result contract for the exact bytes
  admitted by M7e, without relying on host-installed optional parser state.
- The deliberately narrow dialect rejects common CSV extensions such as quoted
  newlines, alternate delimiters/encodings, name repair, and inferred types.
  Supporting them requires a new reviewed contract, not silent parser drift.
- A future runtime adapter must perform the actual package/version/symbol check
  atomically with invocation and demonstrate how its result maps to this
  canonical character table; the planned `readr` metadata does not become
  executable merely because M7f parsed bytes.
- Physical-attempt identity and ledger rows remain coupled to a future
  transport/execution scope. A deterministic attempt slot would not be unique
  across repeated executions and cannot supply DNS, timing, outcome, charging,
  or provider provenance.
