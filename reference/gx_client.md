# Create a bounded Geoconnex protocol client

A client records endpoint-specific request policy. Network calls remain
lazy: constructing a client never contacts a service or writes to its
cache.

## Usage

``` r
gx_client(
  endpoint = c("graph", "reference", "pid"),
  timeout = 30,
  retries = 3L,
  min_interval = getOption("geoconnexr.min_interval", 0.1),
  max_bytes = 2 * 1024^2,
  cache = TRUE,
  offline = getOption("geoconnexr.offline", FALSE),
  cache_dir = gx_default_cache_dir()
)
```

## Arguments

- endpoint:

  One of `"graph"`, `"reference"`, or `"pid"`.

- timeout:

  Per-attempt timeout in seconds.

- retries:

  Number of physical retry attempts after the initial request, from zero
  through 1,000.

- min_interval:

  Minimum seconds between physical attempts reserved for the same
  hostname. Defaults to 0.1 seconds.

- max_bytes:

  Maximum identity-encoded response bytes per physical attempt.

- cache:

  Whether successful and redirect responses may use the package cache.

- offline:

  Whether requests must be satisfied from valid cache entries.

- cache_dir:

  Cache directory.

## Value

An object of class `gx_client`.

## Retries

The package retries HTTP 429, 500, 502, 503, and 504 responses and
transport failures. Every physical attempt revalidates DNS and is
recorded for downstream request/byte budgets. `Retry-After` is honored
up to `geoconnexr.retry_max_delay`, which defaults to 60 seconds; a
larger server minimum stops retrying instead of being shortened.

## Host throttling

Physical attempts across clients in one R process share a hostname
schedule. Retries and manual redirect hops reserve new slots, while
cache hits and offline misses do not. `min_interval = 0` adds no
interval of its own but still honors a preceding positive reservation
from another client. Bounded concurrent dispatch remains future work.
