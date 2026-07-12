# Named SPARQL assets

Only templates declared in `manifest.yml` are eligible for automatic paging.
`gx_sparql()` must execute arbitrary user queries exactly once.

The renderer must compare every `{{slot}}` in a query with its manifest entry,
reject undeclared or missing slots, and encode values by type. In particular,
`uri_list` is a whitespace-separated sequence of individually validated RDF IRI
terms (maximum 200), and `wkt` is one escaped RDF literal with datatype
`http://www.opengis.net/ont/geosparql#wktLiteral`. No type permits raw SPARQL.

Pagination is bounded by both `row_budget` and the parameter maximum. Results
are ordered by every selected field after the logical key so that OFFSET pages
have a total order. A repeated result key, missing page, or upstream mutation is
a diagnostic; callers must not silently claim completeness.

`sites_on_mainstem.rq` deliberately unions the observed nested
`HY_IndirectPosition` graph path and the intended direct path. Spatial queries
use the GeoSPARQL function namespace (`geof:sfIntersects`), not the ontology
namespace.
