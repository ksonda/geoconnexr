# ADR 0012: Bound one-shot graph results behind the SPARQL release gate

- Status: Accepted for the internal experimental M5a spike
- Date: 2026-07-13
- Owner: ksonda

## Context

ADR 0004 leaves the graph endpoint and its public contract unresolved. The
package nevertheless needs fixture-backed SELECT and ASK evidence without
bypassing the M1 request, retry, safety, cache, and accounting boundary. A raw
public query function is not safe yet: response byte and row ceilings do not
bound server-side SPARQL work, and retrying unclassified text could repeat a
mutation. The current named templates also cannot safely paginate because some
declared ordering and result-key variables may be blank nodes whose labels are
scoped to one response document.

## Decision

- M5a provides only the unexported `gx_graph_execute_once()` and
  `gx_graph_parse_results()` substrate. Callers must supply an explicit graph
  client, trusted package-controlled read-only SELECT or ASK text, its expected
  result form, and every finite execution/parser budget.
- One logical execution sends the validated UTF-8 query bytes unchanged in one
  SPARQL Protocol POST. It never adds paging, chunks, follows result links, or
  rewrites text. ADR 0010 may physically retry the identical body, with every
  attempt counted and DNS-revalidated.
- Successful responses must use `application/sparql-results+json`. A semantic
  response validator runs before cache admission and on cache reads. Intrinsic
  payload corruption invalidates a cache entry; caller budget or expected-form
  mismatches do not misclassify an otherwise valid entry as corrupt.
- Parsing targets the SPARQL 1.1 SELECT/ASK JSON shape and the `uri`, `bnode`,
  and `literal` term kinds. Lexical values, datatypes, and language tags remain
  character data and are never coerced. General absolute IRIs and a
  conservative language-tag shape are validated; returned IRIs and `head`
  links are never dereferenced.
- SELECT output is a sparse table ordered by response row and declared variable
  position. It contains `row`, `variable_index`, `variable`, `term_type`,
  `value`, `datatype`, `language`, and `bnode_scope`. `row_count` separately
  preserves zero-width and all-unbound solutions. Blank-node scope is opaque
  and unique to one parsed result, so labels cannot become cross-response
  identifiers. ASK output preserves one logical value.
- Explicit ceilings cover query bytes, physical/cache retrievals, cumulative
  charged bytes, per-response transport bytes, raw parser bytes, nesting,
  structural/parsed members, atomic bytes, variables, rows, bound terms, and
  links. Result construction is proportional to declared variables plus bound
  terms rather than the dense rows-by-variables product.
- Results retain only hashes, redacted endpoint/response metadata, the projected
  request ledger, and rich physical-attempt metadata. Raw query and response
  bytes are not retained in results or conditions.

## Consequences

- Fixture and opt-in live evidence can use the package safety boundary without
  creating a stable endpoint or public result contract.
- `gx_sparql()`, `gx_query()`, discovery helpers, and named pagination remain
  unimplemented and unexported. Export requires a real read-only query-form
  classifier or a trusted typed-query object; `expected=` is only a response
  contract, not an authorization boundary.
- M5 remains partial. A superseding decision must resolve endpoint support,
  upstream rate limits, public budgets/results, stable cross-response paging
  keys, and concurrency before the graph API can freeze.
- The parser validates the supported result shape and key RDF lexical
  boundaries, but does not claim a complete SPARQL variable, blank-node-label,
  IRI, or BCP47 conformance implementation.
