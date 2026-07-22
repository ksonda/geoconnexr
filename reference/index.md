# Package index

## Define an area of interest

Create and validate hydrologic identifiers and spatial areas.

- [`gx_aoi()`](https://ksonda.github.io/geoconnexr/reference/gx_aoi.md)
  : Define an area of interest
- [`gx_validate_huc()`](https://ksonda.github.io/geoconnexr/reference/gx_validate_huc.md)
  : Validate hydrologic unit codes
- [`gx_validate_comid()`](https://ksonda.github.io/geoconnexr/reference/gx_validate_comid.md)
  : Validate NHDPlus COMIDs

## Resolve and understand Geoconnex resources

Resolve persistent identifiers and parse bounded JSON-LD profiles.

- [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md)
  : Create a bounded Geoconnex protocol client
- [`gx_endpoints()`](https://ksonda.github.io/geoconnexr/reference/gx_endpoints.md)
  : Effective Geoconnex service endpoints
- [`gx_resolve()`](https://ksonda.github.io/geoconnexr/reference/gx_resolve.md)
  : Resolve Geoconnex persistent identifiers
- [`gx_jsonld()`](https://ksonda.github.io/geoconnexr/reference/gx_jsonld.md)
  : Retrieve bounded Geoconnex JSON-LD
- [`gx_parse_location()`](https://ksonda.github.io/geoconnexr/reference/gx_parse_location.md)
  : Parse Geoconnex monitoring-location profiles
- [`gx_parse_datasets()`](https://ksonda.github.io/geoconnexr/reference/gx_parse_datasets.md)
  : Parse datasets described by Geoconnex location profiles

## Explore reference features

Discover collections, queryables, and bounded OGC API feature results.

- [`gx_ref_collections()`](https://ksonda.github.io/geoconnexr/reference/gx_ref_collections.md)
  : Discover reference feature collections
- [`gx_ref_queryables()`](https://ksonda.github.io/geoconnexr/reference/gx_ref_queryables.md)
  : Discover queryable collection properties
- [`gx_ref_features()`](https://ksonda.github.io/geoconnexr/reference/gx_ref_features.md)
  : Retrieve bounded reference features
- [`gx_ref_feature()`](https://ksonda.github.io/geoconnexr/reference/gx_ref_feature.md)
  : Retrieve one reference feature with compatible fallbacks

## Crosswalk identifiers

Map provider identifiers and manage the optional NHDPlus lookup.

- [`gx_gage_to_pid()`](https://ksonda.github.io/geoconnexr/reference/gx_gage_to_pid.md)
  : Map provider gage identifiers to Geoconnex reference PIDs
- [`gx_mainstem_lookup_info()`](https://ksonda.github.io/geoconnexr/reference/gx_mainstem_lookup_info.md)
  : Inspect an installed mainstem lookup
- [`gx_mainstem_lookup_install()`](https://ksonda.github.io/geoconnexr/reference/gx_mainstem_lookup_install.md)
  : Explicitly install the pinned mainstem lookup

## Query and distribution tools

Inspect safe query templates, handlers, and reviewed conversion rules.

- [`gx_templates()`](https://ksonda.github.io/geoconnexr/reference/gx_templates.md)
  : List bundled, typed SPARQL templates
- [`gx_render_query()`](https://ksonda.github.io/geoconnexr/reference/gx_render_query.md)
  : Render a bundled SPARQL template safely
- [`gx_handlers()`](https://ksonda.github.io/geoconnexr/reference/gx_handlers.md)
  : List portable distribution classifiers
- [`gx_classify_distribution()`](https://ksonda.github.io/geoconnexr/reference/gx_classify_distribution.md)
  : Classify a described distribution
- [`gx_unit_conversions()`](https://ksonda.github.io/geoconnexr/reference/gx_unit_conversions.md)
  : Read reviewed unit conversion rules

## Plan and fetch data

Build bounded plans and execute the frozen six-handler M7 subset.

- [`gx_catalog()`](https://ksonda.github.io/geoconnexr/reference/gx_catalog.md)
  : Discover a bounded Geoconnex catalog
- [`gx_fetch_plan()`](https://ksonda.github.io/geoconnexr/reference/gx_fetch_plan.md)
  : Build a bounded fetch plan
- [`gx_fetch()`](https://ksonda.github.io/geoconnexr/reference/gx_fetch.md)
  : Fetch the supported M7 data subset

## Infrastructure and package internals

Cache controls and versioned identity primitives for advanced use.

- [`gx_cache_info()`](https://ksonda.github.io/geoconnexr/reference/gx_cache_info.md)
  : Inspect the package HTTP cache
- [`gx_cache_clear()`](https://ksonda.github.io/geoconnexr/reference/gx_cache_clear.md)
  : Clear the package HTTP cache
- [`gx_contract_hash()`](https://ksonda.github.io/geoconnexr/reference/gx_contract_hash.md)
  : Compute a versioned contract fingerprint
- [`geoconnexr`](https://ksonda.github.io/geoconnexr/reference/geoconnexr-package.md)
  [`geoconnexr-package`](https://ksonda.github.io/geoconnexr/reference/geoconnexr-package.md)
  : geoconnexr: Geoconnex discovery and watershed data snapshots
