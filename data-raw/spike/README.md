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
