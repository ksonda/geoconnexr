# geoconnexr 0.0.0.9000

## Repository foundation

- Established the package and repository identity as `ksonda/geoconnexr` under
  the MIT license.
- Added cross-platform R CMD check, test coverage, and isolated weekly bounded
  live-smoke workflows.
- Recorded CRAN and r-universe as the intended stable and development release
  channels, respectively.
- Documented the unresolved mainstem-vintage and SPARQL endpoint contract gates
  that must close before the 0.1.0 public API is frozen.
- Added offline-tested HUC/COMID validation, deterministic AOI recipes,
  versioned contract fingerprints, and configurable service endpoints.
- Added typed SPARQL template discovery and rendering with injection guards,
  page/query byte budgets, stable ordering, and the correct GeoSPARQL function
  namespace.
- Added portable distribution classification with overlap tests, reviewed
  affine unit-conversion rules, four JSON Schemas, and installed-asset contract
  tests.
- Added three explicitly gated live semantic checks for PID redirects, bounded
  graph discovery, and reference-gage JSON-LD negotiation.
- Added the first runtime protocol slice: bounded endpoint clients,
  representation-specific HTTP caching and offline misses, public-target and
  redirect safety checks, and vectorized PID resolution with recorded redirect
  chains and `HEAD`-to-`GET` fallback.
- Added a fail-closed HTTP safety policy with DNS-to-connection pinning,
  identity-only streamed bodies, fixed cache freshness, credential-aware cache
  exclusion, and package-owned cache clearing.

Public APIs and serialized contracts remain experimental during the P0
architecture spike.
