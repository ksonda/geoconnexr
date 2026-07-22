live_guard <- function() {
  testthat::skip_if(Sys.getenv("GEOCONNEXR_RUN_LIVE") != "true")
  testthat::skip_if_not_installed("httr2")
  testthat::skip_if_not_installed("jsonlite")
}

live_request <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent("geoconnexr-live-smoke/0.0.0.9000") |>
    httr2::req_timeout(seconds = 15) |>
    httr2::req_options(
      followlocation = FALSE,
      maxredirs = 0,
      maxfilesize = 5 * 1024 * 1024
    )
}

test_that("live PID preserves a 303 redirect identity", {
  live_guard()
  withr::local_options(geoconnexr.cache_dir = withr::local_tempdir())
  response <- gx_resolve("https://geoconnex.us/ref/mainstems/29559")
  expect_equal(response$initial_status, 303L)
  expect_equal(response$final_status, 200L)
  expect_equal(response$pid_uri, "https://geoconnex.us/ref/mainstems/29559")
  expect_match(
    response$landing_url,
    "^https://reference\\.geoconnex\\.us/"
  )
  expect_true(is.na(response$problem_code))
})

test_that("live bounded mainstem query returns the checked gage", {
  live_guard()
  query <- gx_render_query(
    "sites_on_mainstem",
    list(
      mainstem_uri = "https://geoconnex.us/ref/mainstems/1622734",
      limit = 10,
      offset = 0
    )
  )
  result <- geoconnexr:::gx_graph_execute_once(
    query,
    expected = "select",
    client = gx_client(
      "graph", retries = 0L, max_bytes = 1024L^2, cache = FALSE
    ),
    max_rows = 10L,
    max_variables = 8L,
    max_bound_terms = 80L,
    max_links = 2L,
    max_requests = 1L,
    max_total_bytes = 1024L^2,
    max_members = 1000L,
    max_atomic_bytes = 1024L^2,
    max_depth = 16L
  )
  sites <- result$bindings$value[result$bindings$variable == "site"]
  expect_contains(sites, "https://geoconnex.us/ref/gages/1000001")
})

test_that("live bounded graph ASK recognizes the checked gage", {
  live_guard()
  result <- geoconnexr:::gx_graph_execute_once(
    paste(
      "ASK WHERE {",
      "<https://geoconnex.us/ref/gages/1000001> ?predicate ?object .",
      "}"
    ),
    expected = "ask",
    client = gx_client(
      "graph", retries = 0L, max_bytes = 512L * 1024L, cache = FALSE
    ),
    max_rows = 0L,
    max_variables = 0L,
    max_bound_terms = 0L,
    max_links = 2L,
    max_requests = 1L,
    max_total_bytes = 512L * 1024L,
    max_members = 100L,
    max_atomic_bytes = 512L * 1024L,
    max_depth = 12L
  )
  expect_true(result$value)
})

test_that("live reference gage negotiates its JSON-LD profile", {
  live_guard()
  response <- live_request(
    "https://reference.geoconnex.us/collections/gages/items/1000001"
  ) |>
    httr2::req_headers(Accept = "application/ld+json") |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200L)
  expect_equal(httr2::resp_content_type(response), "application/ld+json")
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  expect_equal(body[["@id"]], "https://geoconnex.us/ref/gages/1000001")
  expect_true("hyf:referencedPosition" %in% names(body))
})

test_that("live package JSON-LD path parses the checked reference gage", {
  live_guard()
  withr::local_options(geoconnexr.cache_dir = withr::local_tempdir())
  document <- gx_jsonld(
    "https://geoconnex.us/ref/gages/1000001",
    client = gx_client("pid", retries = 0L, max_bytes = 2L * 1024L^2)
  )
  location <- gx_parse_location(document)
  datasets <- gx_parse_datasets(document)

  expect_equal(document$pid_uri, "https://geoconnex.us/ref/gages/1000001")
  expect_equal(nrow(location), 1L)
  expect_equal(location$site_uri, document$pid_uri)
  expect_equal(location$provider_uri, "https://waterdata.usgs.gov")
  expect_true(nrow(datasets) >= 1L)
  expect_true(all(nchar(document$content_sha256) == 64L))
})

test_that("live provider-diverse JSON-LD profiles remain compatible", {
  live_guard()
  withr::local_options(geoconnexr.cache_dir = withr::local_tempdir())
  cases <- list(
    list(
      pid = "https://geoconnex.us/iow/wqp/21VASWCB-WMPO001",
      landing_host = "sta.geoconnex.dev",
      minimum_datasets = 2L,
      diagnostic = NA_character_
    ),
    list(
      pid = "https://geoconnex.us/mtdnrc/gages/SR002",
      landing_host = "features.internetofwater.dev",
      minimum_datasets = 0L,
      diagnostic = "generic_place_geometry"
    ),
    list(
      pid = "https://geoconnex.us/wwdh/snotel/301",
      landing_host = "api.wwdh.internetofwater.app",
      minimum_datasets = 1L,
      diagnostic = "literal_location_type"
    )
  )

  for (case in cases) {
    document <- gx_jsonld(
      case$pid,
      client = gx_client("pid", retries = 0L, max_bytes = 2L * 1024L^2)
    )
    location <- gx_parse_location(document)
    datasets <- gx_parse_datasets(document)
    expect_equal(nrow(location), 1L, info = case$pid)
    expect_equal(location$site_uri, case$pid, info = case$pid)
    expect_match(document$landing_url, case$landing_host, fixed = TRUE, info = case$pid)
    expect_true(nrow(datasets) >= case$minimum_datasets, info = case$pid)
    if (!is.na(case$diagnostic)) {
      expect_true(
        case$diagnostic %in% attr(location, "diagnostics")$code,
        info = case$pid
      )
    }
  }
})

test_that("live native reference client preserves identity roles and bounded filters", {
  live_guard()
  withr::local_options(geoconnexr.cache_dir = withr::local_tempdir())
  client <- gx_client(
    "reference", retries = 0L, max_bytes = 1024L^2,
    cache = FALSE
  )

  collections <- gx_ref_collections(refresh = TRUE, client = client)
  expect_true(all(c(
    "gages", "mainstems", "mainstems_v3", "hu12", "counties"
  ) %in% collections$collection_id))

  queryables <- gx_ref_queryables("hu12", client = client)
  roles <- vapply(queryables$schema, function(x) {
    as.character(x[["x-ogc-role"]] %||% NA_character_)
  }, character(1))
  expect_identical(queryables$name[!is.na(roles) & roles == "id"], "huc12")

  mainstem <- gx_ref_features(
    "mainstems", query = list(id = "29559"), limit = 2L,
    client = client
  )
  expect_equal(nrow(mainstem), 1L)
  expect_identical(mainstem$feature_id, "29559")
  expect_s3_class(mainstem, "sf")
})

test_that("live bounded gage crosswalk preserves the checked identity", {
  live_guard()
  withr::local_options(geoconnexr.cache_dir = withr::local_tempdir())
  client <- gx_client(
    "reference", retries = 0L, max_bytes = 1024L^2,
    cache = FALSE
  )

  out <- gx_gage_to_pid("USGS-08332622", client = client)

  expect_identical(out$status, "matched")
  expect_identical(out$gage_id, "1000001")
  expect_identical(out$gage_uri, "https://geoconnex.us/ref/gages/1000001")
  expect_identical(out$mainstem_uri, "https://geoconnex.us/ref/mainstems/1622734")
  expect_identical(out$comid, "17789327")
  expect_true(attr(out, "gx_crosswalk")$complete)
})

test_that("live pinned mainstem asset exposes the checked bounded redirect", {
  live_guard()
  spec <- gx_mainstem_lookup_spec("v3.2")
  path <- tempfile(fileext = ".redirect-body")
  on.exit(if (file.exists(path)) unlink(path, force = TRUE), add = TRUE)
  client <- gx_client(
    "pid", retries = 0L, max_bytes = 1024L^2, cache = FALSE
  )

  response <- gx_http_download_file(
    client,
    spec$source_url,
    path,
    max_bytes = 1024L^2,
    check_status = FALSE
  )

  expect_identical(response$status, 302L)
  expect_identical(response$bytes, 0)
  location <- gx_header(response$headers, "location")
  expect_true(nzchar(location))
  expect_true(tolower(httr2::url_parse(location)$hostname) %in% spec$allowed_hosts)
})

test_that("live public WQP catalog-to-fetch path returns bounded data", {
  live_guard()
  testthat::skip_if_not_installed("dataRetrieval", minimum_version = "2.7.22")

  catalog <- gx_catalog(
    gx_aoi("VA"),
    site_uri = "https://geoconnex.us/iow/wqp/21VASWCB-WMPO001",
    max_sites = 1L,
    client = gx_client("pid", retries = 0L, cache = FALSE)
  )
  time <- as.POSIXct(
    c("2017-01-01 00:00:00", "2017-12-30 23:59:59"), tz = "UTC"
  )
  fetched <- gx_fetch(gx_fetch_plan(
    catalog, time = time, max_datasets = 1L, max_bytes = 1024^2
  ))

  position <- which(fetched$status$handler_id == "wqp" &
                      fetched$status$attempted)
  expect_length(position, 1L)
  expect_true(fetched$status$succeeded[[position]])
  expect_identical(fetched$status$physical_attempts[[position]], 1L)
  expect_identical(fetched$results$handler_id, "wqp")
  expect_true(fetched$results$row_count[[1L]] >= 1L)
})

test_that("live public caller-profile EDR path returns bounded data", {
  live_guard()
  testthat::skip_if_not_installed("edr4r", minimum_version = "0.1.1")

  uri <- "https://example.org/geoconnexr/icoads-sst"
  path <- system.file(
    "extdata", "icoads-sst-profile.json", package = "geoconnexr"
  )
  expect_true(nzchar(path))
  profile <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  catalog <- gx_catalog(
    gx_aoi("VA"), site_uri = uri,
    profiles = stats::setNames(list(profile), uri), max_sites = 1L
  )
  time <- as.POSIXct(
    c("2000-01-16 06:00:00", "2000-02-16 06:00:00"), tz = "UTC"
  )
  fetched <- gx_fetch(gx_fetch_plan(
    catalog, time = time, max_datasets = 1L, max_bytes = 1024^2
  ))

  expect_identical(fetched$status$handler_id, "edr")
  expect_true(fetched$status$succeeded)
  expect_identical(fetched$status$physical_attempts, 1L)
  expect_identical(fetched$results$row_count, 2L)
  expect_identical(fetched$results$column_count, 9L)
})
