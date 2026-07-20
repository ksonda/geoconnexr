# P0 vertical-spike workspace

Code in `data-raw/` is maintainer-operated evidence code, not package runtime
code and not an automatic test target.

The P0 slice is complete only after it records:

1. one bounded AOI query using `geof:sfIntersects`;
2. five minimized JSON-LD profiles from at least three providers;
3. one HUC10 through catalog, one current USGS or EDR fetch, harmonization, and
   offline package verification;
4. current, superseded, v3, and large-mainstem behavior;
5. explicit EDR and current USGS request plans; and
6. replay metadata, redirect/cache behavior, and encoded/decoded size ceilings.

Recommended live-run ceiling: 20 requests, 60 seconds per request, 2 MiB per
metadata response, 10 MiB total metadata, and no redirect following for graph
POSTs. Store large/raw captures outside the package; commit only minimized,
sanitized fixtures and SHA-256 evidence sidecars.

The M2 JSON-LD corpus closes its module-level profile fixture requirement. M3
now has hash-pinned collection/queryable schemas, checked legacy-item failure
evidence, and deterministic pagination/empty-result tests. Current,
superseded, v3, and full large-geometry evidence remains open, as do the HUC10
fetch/package path, mainstem-vintage decision, graph contract decision, and
measured delivery estimate.

The M4a gage crosswalk has separate hash-pinned queryable and known-answer
fixtures. `m4-upstream-evidence-v1.json` pins the checked upstream commit,
release asset, checksum, and contrasting known answers that invalidated the
earlier COMID→VAA `levelpathi`→mainstem assumption: those identifiers are not
interchangeable. M4b now mirrors that audit in an immutable installed registry
and implements an explicit, integrity-checked download/import/offline lifecycle
plus a local-only forward mapper. Remaining M4 evidence must resolve mainstem
currentness/supersession, HUC and point provenance, and inverse ranking before
their public APIs are exported.
