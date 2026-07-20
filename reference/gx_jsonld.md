# Retrieve bounded Geoconnex JSON-LD

Resolves one identifier, negotiates JSON-LD, and falls back to embedded
JSON-LD or one advertised JSON-LD alternate on an HTML landing page.
Every network request uses the bounded package transport. Remote
contexts are replaced only from the bundled allowlist before
standards-based expansion; arbitrary context loading is disabled.

## Usage

``` r
gx_jsonld(uri, as = c("expanded", "raw", "text"), client = gx_client("pid"))
```

## Arguments

- uri:

  One HTTP(S) identifier.

- as:

  Representation exposed in `document`: standards-expanded, parsed
  source JSON, or source JSON text. The object always retains both
  parsed source and expanded forms for the profile parsers.

- client:

  A PID client created by
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

## Value

A `gx_jsonld` list with these fields:

- `contract_version` and `representation` identify the experimental
  output contract and the form selected in `document`.

- `pid_uri`, `landing_url`, and `source_url` preserve identifier,
  redirect, and representation provenance. `source_url` is exact and can
  contain query credentials; do not write it to logs without redaction.

- `media_type`, `retrieval_mode`, `retrieved_at`, `content_sha256`,
  `content_bytes`, and `response_sha256` describe the selected source.

- `document`, `source_document`, and `expanded` contain the selected
  form, parsed source JSON-LD, and standards-expanded JSON-LD,
  respectively.

- `resolution` is the
  [`gx_resolve()`](https://ksonda.github.io/geoconnexr/reference/gx_resolve.md)
  row, `requests` is a request-attempt ledger with redacted URLs, and
  `diagnostics` has fixed columns `severity`, `code`, `path`, `message`,
  and `recoverable`.

## Processing boundary

Parsers receive already-bounded bytes and cannot make network requests.
HTML is parsed with `NONET`; only inline scripts and one safe advertised
alternate is considered. Package-owned retries record every physical
attempt in the same ledger and cumulative budget used by PID resolution,
landing retrieval, and alternate discovery. The `as` argument changes
only `document`, not validation or expansion.

## Safety limits

Options provide fail-closed ceilings. Defaults are 16 request attempts
or cache retrievals; twice the client's per-response limit in cumulative
response bytes; 2 MiB of local JSON; depth 64; 10,000 serialized
members; 512 KiB of contexts; 16 MiB of expanded JSON; and 64 HTML
candidates. Profile parsing additionally limits one identity to 64
defining fragments and dataset output to 10,000 rows. Invalid limit
values fail rather than disabling a ceiling.

All retrieval, parsing, context, and budget failures inherit from
`gx_error_jsonld`. See
[`gx_parse_location()`](https://ksonda.github.io/geoconnexr/reference/gx_parse_location.md)
and
[`gx_parse_datasets()`](https://ksonda.github.io/geoconnexr/reference/gx_parse_datasets.md)
for tolerant profile-level diagnostics.
