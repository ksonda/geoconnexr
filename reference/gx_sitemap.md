# Write a bounded XML sitemap

Writes one deterministic `sitemap.xml` through a private sibling staging
directory and publishes it only to an absent destination directory.

## Usage

``` r
gx_sitemap(uris, dir)
```

## Arguments

- uris:

  One to 50,000 unique canonical HTTP(S) URIs.

- dir:

  A new destination directory whose parent already exists.

## Value

A `gx_sitemap` object with the normalized directory and file paths, URL
count, byte size, and SHA-256 digest.
