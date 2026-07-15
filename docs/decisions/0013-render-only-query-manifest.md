# ADR 0013: Pin named queries without authorizing execution or pagination

- Status: Accepted experimental M5b policy
- Date: 2026-07-13
- Owner: ksonda

## Context

The original named-query manifest mixed reviewed syntax with future paging
intent. Its `stable_order` label implied a guarantee the assets did not prove:
SPARQL 1.1 does not define a total ordering over every RDF term, optional
bindings can be absent, blank-node labels are scoped to one result document,
and the graph has no snapshot contract. Several current result keys may contain
blank nodes. A syntactically complete `ORDER BY` therefore cannot make OFFSET
paging complete or repeatable.

The runtime validator also accepted malformed top-level policies, fields, and
parameter relationships, while query files were neither byte-pinned nor tied
to their declared projection. That is too weak for assets that will eventually
produce executable text.

## Decision

- Replace query-manifest v1 with experimental contract v2 (`0.2.0`). Its exact
  root capabilities enable local rendering and disable execution, pagination,
  and URI-list chunking under ADR 0004.
- Pin every `.rq` asset by exact stored byte count and SHA-256. `.rq` files use
  repository-enforced LF endings; validation reads bounded raw bytes before
  UTF-8 decoding and rejects BOMs, disallowed controls, symlinks, undeclared or
  orphan files, and any byte/hash mismatch.
- Parse the bounded YAML with aliases, custom tags, merges, duplicate keys, and
  ambiguous parser outcomes rejected. Runtime validation is the cross-field
  authority, mirrored as far as possible by `query-manifest-v2.json`.
- Require each template to declare ordered projected and required variables,
  the actual bare-variable `ORDER BY`, candidate result-key uniqueness and
  scope, and explicit pagination blockers. Runtime checks these facts against
  the hash-reviewed SELECT text, its canonical slots, and its single terminal
  `LIMIT`/`OFFSET` pair.
- Use names that match encoder behavior. `http_iri` and `http_iri_list` accept
  bounded credential-free HTTP(S) IRIs; lists reject duplicates and sort by
  UTF-8 bytes. `crs84_wkt_literal` accepts finite, valid, non-empty, explicitly
  closed Polygon/MultiPolygon AOIs. Integers are locale-independent. Escaped
  literal and canonical UTC datetime encoders are available for future pinned
  templates. No type permits raw SPARQL.
- `gx_templates()` exposes the integrity, result-shape, ordering, key, blocker,
  and disabled-capability metadata. `gx_render_query()` retains the validated
  final LF and performs no network request, paging, or chunking.

## Consequences

- Template edits require an explicit stored byte/hash review. The manifest does
  not self-hash; package distribution integrity plus per-template pins are the
  trust boundary.
- `order.variables` records syntax, while `order.total` and
  `stable_across_requests` record reviewed limits. Ordinary templates remain
  non-total and result-document-scoped. The fixed provider aggregate is a
  zero-or-one-row query, not a pageable exception.
- M5 acceptance remains open. A superseding ADR must establish canonical
  always-bound non-blank-node keys, total cross-request ordering, snapshot or
  mutation semantics, page/request/byte budgets, duplicate/missing-page
  reconciliation, endpoint support, and per-page provenance before paging can
  be enabled.
- This reviewed SELECT lint and byte pinning do not authorize arbitrary SPARQL.
  The public raw executor remains gated by ADRs 0004 and 0012.

The ordering limitation follows the
[SPARQL 1.1 `ORDER BY` contract](https://www.w3.org/TR/sparql11-query/#modOrderBy).
