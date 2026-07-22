catalog_discovery_fixture <- function(name) {
  paste(
    readLines(
      testthat::test_path("..", "fixtures", "jsonld", "observed", name),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

catalog_discovery_clock <- function() {
  as.POSIXct("2026-07-22 12:00:00", tz = "UTC")
}

catalog_discovery_clients <- function() {
  list(
    client = gx_client("pid", cache = FALSE, retries = 0L),
    graph_client = gx_client("graph", cache = FALSE, retries = 0L),
    reference_client = gx_client("reference", cache = FALSE, retries = 0L)
  )
}

test_that("explicit PID discovery adapts a production profile offline", {
  clients <- catalog_discovery_clients()
  touched <- new.env(parent = emptyenv())
  touched$graph <- 0L
  profile <- catalog_discovery_fixture("virginia-wqp-wmpo001.min.json")
  out <- do.call(gx_catalog_impl, c(list(
    aoi = gx_aoi("VA"),
    site_uri = "https://geoconnex.us/iow/wqp/21VASWCB-WMPO001",
    max_sites = 1L,
    profile_fetcher = function(uri, client) profile,
    graph_executor = function(...) {
      touched$graph <- touched$graph + 1L
      stop("graph should not run", call. = FALSE)
    },
    now = catalog_discovery_clock
  ), clients))

  expect_s3_class(out, "gx_catalog")
  expect_identical(touched$graph, 0L)
  expect_identical(nrow(out$sites), 1L)
  expect_identical(nrow(out$datasets), 2L)
  expect_true(all(is.na(out$datasets$distribution_url)))
  expect_true(all(!out$datasets$fetchable))
  expect_contains(out$problems$code, "explicit_site_seed")
  expect_identical(out$metadata$completeness$status, c("complete", "complete"))
  expect_identical(gx_catalog_validate_impl(out), invisible(out))
})

test_that("automatic spatial discovery executes one bounded graph page", {
  clients <- catalog_discovery_clients()
  profile <- catalog_discovery_fixture("virginia-wqp-wmpo001.min.json")
  graph_calls <- 0L
  executor <- function(query, expected, client, max_rows, max_variables,
                       max_bound_terms, max_links, max_requests,
                       max_total_bytes, max_members, max_atomic_bytes,
                       max_depth) {
    graph_calls <<- graph_calls + 1L
    expect_identical(expected, "select")
    expect_match(query, "geof:sfIntersects", fixed = TRUE)
    list(
      row_count = 1L,
      bindings = tibble::tibble(
        row = 1L, variable_index = 1L, variable = "site",
        term_type = "uri",
        value = "https://geoconnex.us/iow/wqp/21VASWCB-WMPO001",
        datatype = NA_character_, language = NA_character_,
        bnode_scope = NA_character_
      ),
      requests = gx_graph_empty_requests()
    )
  }
  ring <- rbind(
    c(-77, 37), c(-76, 37), c(-76, 38), c(-77, 38), c(-77, 37)
  )
  aoi <- gx_aoi(sf::st_sfc(sf::st_polygon(list(ring)), crs = gx_aoi_crs))
  out <- do.call(gx_catalog_impl, c(list(
    aoi = aoi,
    max_sites = 2L,
    profile_fetcher = function(uri, client) profile,
    graph_executor = executor,
    now = catalog_discovery_clock
  ), clients))

  expect_identical(graph_calls, 1L)
  expect_identical(nrow(out$sites), 1L)
  expect_false("explicit_site_seed" %in% out$problems$code)
  expect_false(any(out$metadata$completeness$truncated))
})

test_that("profile failures remain visible as a partial catalog", {
  clients <- catalog_discovery_clients()
  out <- do.call(gx_catalog_impl, c(list(
    aoi = gx_aoi("VA"),
    site_uri = "https://geoconnex.us/iow/wqp/21VASWCB-WMPO001",
    max_sites = 1L,
    profile_fetcher = function(uri, client) stop("fixture failure"),
    graph_executor = function(...) stop("graph should not run"),
    now = catalog_discovery_clock
  ), clients))

  expect_identical(nrow(out$sites), 1L)
  expect_identical(nrow(out$datasets), 0L)
  expect_contains(out$problems$code, "profile_retrieval_failed")
  expect_identical(out$metadata$completeness$status, c("partial", "partial"))
  expect_identical(out$metadata$completeness$failed_count, c(1L, 1L))
})

test_that("gx_catalog is now a public bounded workflow boundary", {
  expect_true("gx_catalog" %in% getNamespaceExports("geoconnexr"))
  expect_error(
    gx_catalog(gx_aoi("VA"), site_uri = character()),
    class = "gx_error_catalog_discovery_input"
  )
  expect_error(
    gx_catalog(
      gx_aoi("VA"),
      profiles = list("https://example.org/site" = list())
    ),
    class = "gx_error_catalog_discovery_input"
  )
})

test_that("caller-supplied profiles build a public offline catalog", {
  uri <- "https://example.org/geoconnexr/icoads-sst"
  path <- system.file(
    "extdata", "icoads-sst-profile.json", package = "geoconnexr"
  )
  expect_true(nzchar(path))
  profile <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  out <- gx_catalog(
    gx_aoi("VA"), site_uri = uri,
    profiles = stats::setNames(list(profile), uri), max_sites = 1L
  )

  expect_s3_class(out, "gx_catalog")
  expect_identical(nrow(out$sites), 1L)
  expect_identical(nrow(out$datasets), 1L)
  expect_identical(out$datasets$handler_id, "edr")
  expect_true(out$datasets$fetchable)
  expect_contains(out$problems$code, "caller_supplied_profiles")
  expect_identical(nrow(out$requests), 0L)
})
