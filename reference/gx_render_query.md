# Render a bundled SPARQL template safely

Parameters are encoded according to the exact-byte-pinned bundled
manifest. Raw string interpolation is deliberately unsupported.
Rendering is local: this function does not execute, paginate, or chunk a
query.

## Usage

``` r
gx_render_query(template, params)
```

## Arguments

- template:

  Name returned by
  [`gx_templates()`](https://ksonda.github.io/geoconnexr/reference/gx_templates.md).

- params:

  A named list containing exactly the template parameters.

## Value

A length-one UTF-8 SPARQL query retaining the template's final LF.
