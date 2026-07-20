# Explicitly install the pinned mainstem lookup

Installs the optional `ref_rivers` NHDPlusV2 COMID lookup into a
separate, package-owned data directory. This is the only geoconnexr
operation that downloads the disclosed 120,422,425-byte v3.2 asset;
crosswalk calls never install or refresh it implicitly.
`source = "file"` imports the same pinned bytes from a local file and is
suitable for air-gapped use.

## Usage

``` r
gx_mainstem_lookup_install(
  source = c("release", "file"),
  file = NULL,
  version = "v3.2",
  force = FALSE,
  confirm = interactive(),
  offline = getOption("geoconnexr.offline", FALSE),
  data_dir = gx_default_data_dir()
)
```

## Arguments

- source:

  Either `"release"` for the pinned upstream release asset or `"file"`
  for a local import.

- file:

  Local CSV path required by `source = "file"`; otherwise `NULL`.

- version:

  Pinned lookup release. Currently only `"v3.2"` is registered.

- force:

  Replace an existing installation after the replacement has fully
  verified.

- confirm:

  Prompt before writing or downloading when `TRUE`. Non-interactive
  callers must pass `FALSE` explicitly if they set this argument to
  `TRUE` through shared configuration.

- offline:

  Whether network access is prohibited. Local imports remain available
  offline.

- data_dir:

  Persistent package data directory, separate from the HTTP cache.

## Value

The verified one-row result from
[`gx_mainstem_lookup_info()`](https://ksonda.github.io/geoconnexr/reference/gx_mainstem_lookup_info.md).

## Details

The transfer is streamed to disk with redirects disabled at the
transport layer and validated hop by hop. The exact byte count, SHA-256
digest, CSV schema, row count, known answers, and provenance receipt
must all verify before an atomic replacement. Included mainstems were
non-superseded when v3.2 was generated; installation does not check
current service state.
