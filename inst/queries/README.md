# Named SPARQL assets

`manifest.yml` is a render-only v2 contract. Its root capability flags keep
query execution, chunking, and pagination disabled under ADR 0004. Declaring a
candidate strategy does not authorize a request loop.

Every `.rq` file is declared exactly once and pinned by its exact stored byte
count and SHA-256, including its final LF. Runtime validation rejects a missing,
orphaned, symlinked, oversized, BOM-prefixed, non-UTF-8, control-bearing, or
hash-mismatched asset. It also verifies canonical `{{slot}}` syntax, the SELECT
projection, one terminal `LIMIT {{limit}}` / `OFFSET {{offset}}` pair, and the
bare-variable `ORDER BY` sequence before rendering. No parameter type permits
raw SPARQL.

Parameter contracts are deliberately narrower than their RDF names suggest:

- `http_iri` accepts one credential-free absolute HTTP(S) IRI with a real host,
  valid percent escapes, SPARQL-safe IRIREF characters, and an 8 KiB ceiling;
- `http_iri_list` validates each term, rejects duplicates, and sorts exact UTF-8
  bytes before rendering a bounded VALUES sequence;
- `crs84_wkt_literal` validates a finite, non-empty, valid, explicitly closed
  Polygon or MultiPolygon and serializes the original lexical WKT as a
  GeoSPARQL literal with implicit OGC CRS84;
- `integer` uses locale-independent ASCII decimal; and
- `literal` and `datetime` provide escaped UTF-8 and canonical UTC encoders for
  future reviewed templates.

The manifest distinguishes syntactic ordering from paging safety. SPARQL 1.1
does not define a total order over every RDF term, optional variables may be
unbound, blank-node labels are scoped to one result document, and the graph has
no snapshot contract. Accordingly, ordinary template order and result keys are
marked non-total/document-scoped and `pagination.enabled` remains `false`.
`provider_coverage` is a fixed zero-or-one-row aggregate, not a pageable query;
when its provider has no matching site, the current `GROUP BY` returns zero
rows rather than a row of zero counts.

`sites_on_mainstem.rq` deliberately unions the observed nested
`HY_IndirectPosition` graph path and the intended direct path. Spatial queries
use the GeoSPARQL function namespace (`geof:sfIntersects`), not the ontology
namespace.

See the [SPARQL 1.1 ordering rules](https://www.w3.org/TR/sparql11-query/#modOrderBy)
and [GeoSPARQL 1.1 geometry literal rules](https://docs.ogc.org/is/22-047r1/22-047r1.html).
