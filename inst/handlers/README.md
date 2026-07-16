# Fetch-handler registry

`registry.yml` contains only portable facts used to classify a distribution.
It is ordered by numeric precedence and uses first-match semantics.
`implementations-r.json` maps those exact classifier identities to R
implementation and optional-package metadata. The runtime loader reads both as
bounded regular non-symlink UTF-8/LF files, rejects ambiguous YAML features,
validates their one-to-one contract, and retains the SHA-256 of each exact asset.

Neither asset grants permission to fetch a URL: the request layer must still
apply DNS and redirect revalidation plus scheme, host, payload, page, and
request budgets.

Every implementation must expose the package protocol
`probe -> plan -> fetch -> normalize`. A missing Suggests package eventually
yields `skipped_missing_pkg`; `unknown` is `reference_only`.

The internal M7a plan is deliberately narrower. It reclassifies admitted
catalog URLs, emits one row per unique distribution plus ordered parameter
rows, and records every implementation as planned and non-replayable. Its
request list is empty and `execution_ready` is false. M7a does not probe
packages, call implementations, resolve DNS, use the network or cache, or write
files. Request construction, execution, runtime registration, and serialization
remain later M7 work.

The separate internal M7b report checks only host package metadata for selected
handlers. Its built-in probe checks each unique allowlisted package once using a
bounded direct read of installed `DESCRIPTION` identity and version; it ignores
`Meta/package.rds` and never loads a namespace or inspects or calls a symbol.
Missing and old packages become explicit skip statuses, but a present package
or satisfied minimum version still yields `blocked_implementation_planned`.
The report is host-specific, advisory, non-replayable, and never
execution-ready. Future execution must repeat package and symbol checks at the
point of invocation; provider request planning remains fixture- and
transport-contract gated under ADR 0021.

Current USGS Water Data API distributions are tested before the generic OGC API
Features classifier. Legacy NWIS IV/DV URLs are compatibility-only and produce a
deprecation warning. EDR plans must record the base URL, collection, query verb,
geometry/location, parameter, datetime, and response format before fetching.
