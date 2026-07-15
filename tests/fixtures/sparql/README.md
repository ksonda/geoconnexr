# SPARQL Results JSON fixtures

These UTF-8 fixtures are synthetic SPARQL 1.1 SELECT and ASK result documents.
They pin RDF lexical values and metadata, variable order, unbound-variable
absence, zero-row versus zero-width cardinality, and boolean result shapes.

`manifest-v1.json` records hashes of the exact stored bytes. Malformed payloads
remain inline in `test-graph.R` so each rejected shape is visible beside its
expected error class.

`query-template-contract-v2.json` independently pins the bundled render-only
query files, byte counts, hashes, slots, ordered result variables, ordering
facts, document-scoped result keys, and explicit pagination blockers. It is a
test oracle for the installed query manifest; it does not authorize execution
or pagination.
