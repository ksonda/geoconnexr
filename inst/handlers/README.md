# Fetch-handler registry

`registry.yml` contains only portable facts used to classify a distribution.
It is ordered by numeric precedence and uses first-match semantics. It does not
grant permission to fetch a URL: the request layer must still apply SSRF,
redirect, scheme, host, payload, page, and request budgets.

`implementations-r.json` maps classifier results to R implementations and keeps
optional package requirements out of the portable registry. Every implementation
must expose the package protocol `probe -> plan -> fetch -> normalize`. A missing
Suggests package yields `skipped_missing_pkg`; `unknown` is `reference_only`.

Current USGS Water Data API distributions are tested before the generic OGC API
Features classifier. Legacy NWIS IV/DV URLs are compatibility-only and produce a
deprecation warning. EDR plans must record the base URL, collection, query verb,
geometry/location, parameter, datetime, and response format before fetching.
