# ADR 0006: JSON-LD processing has no parser-controlled network access

- Status: Accepted for the experimental M2 protocol slice
- Date: 2026-07-13
- Owner: ksonda

## Context

Geoconnex landing pages and their JSON-LD are provider controlled. Both HTML
alternate discovery and JSON-LD remote-context loading can otherwise bypass the
package transport, its SSRF checks, cache policy, and byte limits. Compact
profiles can also amplify into large expanded graphs or dataset cross-products.

## Decision

- All PID, redirect, landing-page, and advertised-alternate requests use one L1
  client, request ledger, redirect policy, and cumulative request/byte budget.
- M2 disables transport retries until each physical attempt can be represented
  in that ledger and budget.
- `xml2` receives bounded raw bytes with `NONET`; script `src`, HTML `base`, and
  meta refresh are ignored. `jsonld` receives literal JSON only.
- Exact allowlisted remote contexts are replaced with hash-verified bundled
  assets. The official Schema.org V30.0 context is separately attributed under
  CC BY-SA 3.0. Unknown contexts and every `@import` fail closed.
- Compatibility prefix repairs are scoped, limited to reviewed Geoconnex
  prefixes in property and identity/type positions, and always diagnostic.
- Input bytes, nesting depth, members, context bytes, expanded bytes, HTML
  candidates, repeated node fragments, and dataset output rows are bounded.
  Atomic R vectors count as serialized members, and the cumulative cost of
  bundled context replacement is checked before any replacement is copied.
- Request ledgers and print methods redact credentials, fragments, and every
  query value. Exact source URLs remain provenance fields but are not printed.
- Errors at provider-controlled JSON-LD and transport boundaries omit raw
  parent conditions and captured call traces because either can contain
  credentials or document literals.

## Consequences

- Some otherwise valid documents using unbundled remote contexts are rejected.
- Latin-1/Windows HTML can advertise or embed UTF-8 JSON-LD, but JSON itself
  must be valid UTF-8.
- Boundary error classes and messages remain stable, but their conditions do
  not carry a debugging backtrace; internal contract failures still do.
- The public parser tables and identity contract remain experimental during P0.
  The evidence corpus now has six observed pages from four landing hosts and
  five semantic providers, closing the five-real-pages/three-providers gate.
  Contract freezing still waits for the remaining P0 vertical-slice gates and
  the M10 build-to-parse round-trip suite.

## Follow-up

ADR 0010 closes the temporary retry-accounting gate in this decision. JSON-LD
retries are enabled again, and every physical attempt now consumes the same
shared request and cumulative-byte budgets as PID resolution, redirects,
landing-page retrieval, and alternate discovery.
