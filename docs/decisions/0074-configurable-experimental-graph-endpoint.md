# ADR 0074: Keep the documented graph root configurable and experimental

- Status: Accepted experimental upstream contract
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the SPARQL endpoint decision gate in ADR 0004

## Context

ADR 0004 left the graph endpoint undecided because POST behavior at the root
had been observed but not documented as a supported contract. Current
Geoconnex documentation now names https://graph.geoconnex.us as the SPARQL
endpoint and includes a POST example using the SPARQL query media type. The
same documentation says graph query functionality remains in active
development.

The bounded evidence in
`data-raw/spike/graph-contract-evidence-v1.json` records successful SELECT and
ASK POST requests. Both returned SPARQL Results JSON through one physical
attempt. The named mainstem query returned four rows and included the checked
gage. A separate one-site HUC10 spatial query still failed at the package's
30-second graph boundary.

## Decision

Use https://graph.geoconnex.us/ as the default graph root for the 0.x package,
with the existing `geoconnexr.endpoint_graph` option as the explicit override.
Treat it as a configurable experimental upstream contract, not a stable public
raw-query API.

Package-controlled graph work remains limited to trusted named SELECT or ASK
text sent by POST with `Content-Type: application/sparql-query` and
`Accept: application/sparql-results+json`. Requests follow no redirects,
apply finite request, time, row, and byte limits, and preserve physical-attempt
evidence. Raw user SPARQL, automatic pagination, `gx_sparql()`, and
`gx_query()` remain unexported.

This closes the product decision about POST at the graph root. It does not
close the separate P0 spatial evidence gate. Automatic HUC10 discovery remains
experimental until a bounded `geof:sfIntersects` probe succeeds with recorded
evidence.

## Consequences

- The package has a documented default and a tested override instead of an
  implied stable endpoint.
- Existing bounded catalog discovery can continue without exposing arbitrary
  graph execution.
- A graph timeout remains a visible partial-result condition.
- Stable public graph APIs still require a later ADR and new upstream evidence.
