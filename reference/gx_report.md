# Access a verified geoconnexr package report

Loads the fixed HTML report from a report-bearing package as exact
bounded bytes, revalidates its isolated structure and manifest binding,
and returns read-only evidence. When `output` is supplied, the same
bytes are atomically written to one absent file beneath an existing safe
parent.

## Usage

``` r
gx_report(x, output = NULL)
```

## Arguments

- x:

  A report-bearing `gx_package`, `gx_package_loaded`, or package
  directory path.

- output:

  `NULL` for in-memory access, or one absent HTML file path.

## Value

A validated `gx_report` object containing the verified package load,
report manifest descriptor, exact HTML bytes, optional output path,
fixed scope metadata, and report-access identity.

## Details

Access is offline and never invokes Quarto. The report is redacted,
execution-disabled, cache-disabled, unsigned, and non-replayable.
