# Safety and reproducibility

`geoconnexr` treats safety limits and provenance as part of its data
model, not as implementation details. This article explains how to read
package results correctly and how to keep exploratory work reproducible.

## Know when a call uses the network

Some operations are entirely local:

- validating HUC and COMID values;
- creating AOI recipes;
- inspecting and rendering bundled SPARQL templates;
- classifying a described distribution URL;
- inspecting cache state; and
- checking whether the optional mainstem lookup is installed.

PID resolution, JSON-LD retrieval, reference-feature discovery, and live
crosswalks use the package request layer. Their reference pages document
the relevant request, redirect, byte, row, and time limits.

## Preserve identity across redirects and crosswalks

A persistent identifier is the identity of a resource even when it
redirects to another URL.
[`gx_resolve()`](https://ksonda.github.io/geoconnexr/reference/gx_resolve.md)
therefore reports the original `pid_uri` and the eventual `landing_url`
separately.

Hydrologic and provider identifiers are character values. Keep values
such as `"02070010"` and `"USGS-08332622"` as strings; numeric
conversion can destroy meaningful zeroes or provider-specific syntax.

## Treat completeness as data

Networked water-data systems can return partial pages, incompatible
profiles, or provider-specific failures. The package records these
states rather than turning them into silent empty success.

Inspect the metadata attached to returned objects:

``` r

features <- gx_ref_features("mainstems", limit = 10L)
attr(features, "gx_reference")

gages <- gx_gage_to_pid("USGS-08332622")
attr(gages, "gx_crosswalk")

document <- gx_jsonld("https://geoconnex.us/ref/gages/1000001")
location <- gx_parse_location(document)
attr(location, "diagnostics")
```

Before using a result downstream, check flags such as `complete`,
`truncated`, `stop_reason`, request counts, problem codes, and
diagnostics where the class provides them.

## Keep large optional data explicit

The NHDPlus mainstem lookup is intentionally not downloaded as a side
effect of a crosswalk. Inspect its state first:

``` r

gx_mainstem_lookup_info()
```

Only
[`gx_mainstem_lookup_install()`](https://ksonda.github.io/geoconnexr/reference/gx_mainstem_lookup_install.md)
performs the disclosed large transfer, and the downloaded bytes are
checked against a pinned identity. An air-gapped workflow can import the
same verified file explicitly:

``` r

# Network installation from the reviewed release asset:
gx_mainstem_lookup_install(source = "release")

# Or import the identical bytes from a local file:
gx_mainstem_lookup_install(
  source = "file",
  file = "nhdpv2_lookup.csv"
)
```

## Separate consistency from authenticity

An exact hash can prove that bytes match a declared contract or
manifest. It does not, by itself, prove who produced those bytes or that
a historical network request occurred. The package’s internal snapshot
and parser substrates preserve this distinction and do not upgrade
caller-supplied data into provider provenance.

For reproducible work:

1.  Record the package version and relevant object metadata.
2.  Retain original identifiers and source values alongside normalized
    views.
3.  Save diagnostics and incomplete-status fields, not only successful
    rows.
4.  Pin external assets by exact byte identity when the workflow depends
    on them.
5.  Re-run bounded live checks deliberately; do not confuse a cached or
    offline validation with fresh provider observation.

## Experimental boundary

The package remains in an architecture-spike phase. Public functions on
the [reference
page](https://ksonda.github.io/geoconnexr/reference/index.md) are
implemented, but the documented end-to-end catalog, fetch, harmonize,
package, and replay API is still a target. Use the
[roadmap](https://github.com/ksonda/geoconnexr/blob/main/geoconnexr-spec-v0.2.md)
and [architecture
decisions](https://github.com/ksonda/geoconnexr/tree/main/docs/decisions)
to evaluate maturity before operational use.
