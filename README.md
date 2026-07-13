# geoconnexr

<!-- badges: start -->
[![R-CMD-check](https://github.com/ksonda/geoconnexr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ksonda/geoconnexr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## About this experiment

This repository is an experiment in autonomous software development. I gave
ChatGPT Codex only an initializing prompt and access to relevant documentation
and related repositories, then let it research, critique, plan, scaffold, and
test the package autonomously. This is what it came up with; the result remains
experimental rather than production-ready.

The initializing prompt was:

> Design a build-ready plan for an R package called `geoconnexr` that helps
> water-data users discover Geoconnex resources, crosswalk among Geoconnex
> PIDs, NHDPlus COMIDs, HUCs, gage identifiers, and coordinates, and assemble
> reproducible watershed data packages. Use the available Geoconnex
> documentation and related repositories—including `nhdplusTools`,
> `dataRetrieval`, and `edr4r`—to ground the design and avoid duplicating
> existing capabilities. Specify the package architecture, public API, data
> contracts, error handling, provenance, security constraints, testing and CI
> strategy, and a phased implementation roadmap. Validate live infrastructure
> assumptions where practical, identify unresolved decisions explicitly, and
> make the plan detailed enough for a developer or coding agent to implement.

`geoconnexr` is an R-first discovery, identifier-crosswalk, and watershed
data-packaging client for the Geoconnex ecosystem.

The repository is currently in its P0 architecture-spike phase. The initial
scaffold provides versioned contracts, safe SPARQL template metadata,
identifier/recipe foundations, recorded infrastructure evidence, and offline
tests. Its first protocol slices add bounded, cache-aware PID resolution plus
fail-closed JSON-LD negotiation, expansion, and tolerant profile parsing.
Broader discovery and data retrieval remain gated on fixture-backed production
evidence.

## Intended workflow

```r
aoi <- gx_aoi("02070010", type = "huc")
catalog <- gx_catalog(aoi)
plan <- gx_fetch_plan(catalog, time = c("2025-01-01", "2025-01-31"))
fetched <- gx_fetch(plan)
harmonized <- gx_harmonize(fetched)
gx_snapshot(harmonized, dir = "potomac-snapshot")
```

This workflow is the target API, not a claim that every function above is
implemented in the P0 scaffold.

## Available in the P0 scaffold

```r
# Deterministic identifier recipe (no network)
aoi <- gx_aoi("02070010")
aoi$recipe

# Resolve a PID while preserving its identity and redirect chain
resolution <- gx_resolve("https://geoconnex.us/ref/gages/1000001")
resolution[c("pid_uri", "landing_url", "problem_code")]

# Retrieve and safely expand its JSON-LD, then extract profile tables
document <- gx_jsonld("https://geoconnex.us/ref/gages/1000001")
location <- gx_parse_location(document)
datasets <- gx_parse_datasets(document)
attr(location, "diagnostics")

# Inspect and safely render a bounded named SPARQL template
gx_templates()
query <- gx_render_query(
  "sites_on_mainstem",
  list(
    mainstem_uri = "https://geoconnex.us/ref/mainstems/1622734",
    limit = 100,
    offset = 0
  )
)

# Inspect portable handler classification and reviewed unit rules
gx_classify_distribution(
  "https://reference.geoconnex.us/collections/gages/items"
)
gx_unit_conversions()
```

`gx_resolve()` and `gx_jsonld()` make bounded network requests, validate every
redirect target, and cache eligible query-free, credential-free
representations. `gx_jsonld()` never lets the HTML or JSON-LD parser fetch a
URL: it replaces exact allowlisted contexts with hash-pinned bundled assets and
rejects unknown contexts. The two profile parsers are offline and return
structured diagnostics instead of silently dropping tolerated production
quirks. Dataset fetch handlers and snapshot writing remain planned interfaces
and are labeled as such in the bundled implementation metadata.

JSON-LD and parser contracts remain experimental. The fixture corpus currently
contains two observed, minimized reference-service pages plus synthetic
conformance/adversarial cases. The P0 gate requiring five real pages from three
providers is still open; synthetic fixtures are explicitly excluded from that
count.

## Design commitments

- Preserve the original `geoconnex.us` PID as the identity key across 303
  redirects.
- Discover live OGC API collections and queryables rather than treating a
  checked inventory as permanent.
- POST SPARQL queries and use typed template parameters; never splice raw user
  strings into queries.
- Keep recipe replay distinct from offline snapshot verification.
- Treat source-specific failures and incomplete discovery as visible data,
  not silent empty results.
- Preserve provider provenance and original values throughout harmonization.
- Keep provider-controlled JSON-LD bounded by request, byte, depth, member,
  expansion, HTML-candidate, node-fragment, and output-row budgets.

## Development

```r
pak::pak(c("devtools", "testthat"))
devtools::test()
devtools::check()
```

Live infrastructure checks are bounded, opt-in, and separate from normal unit
tests. See `data-raw/README.md` after cloning.

## Planning documents

- [Validated build roadmap](geoconnexr-spec-v0.2.md)
- [Review of the original proposal](geoconnexr-plan-review.md)
- [Architecture decisions](docs/decisions/)

## Status

The package is experimental and has no stable API yet. Contract changes during
the 0.x series will be versioned and accompanied by migration notes once
serialized artifacts are released.
