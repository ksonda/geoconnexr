# Resolve Geoconnex persistent identifiers

Resolution preserves each PID as the identity key while recording every
followed redirect. Each redirect target is checked before a request is
dispatched. A rejected `HEAD` request is retried as a minimal `GET`.

## Usage

``` r
gx_resolve(uri, follow = TRUE, client = gx_client("pid"))
```

## Arguments

- uri:

  Character vector of Geoconnex persistent identifiers.

- follow:

  Whether to follow safe redirects.

- client:

  A PID client created by
  [`gx_client()`](https://ksonda.github.io/geoconnexr/reference/gx_client.md).

## Value

A tibble with one row per input PID and a list-column containing the
redirect chain.
