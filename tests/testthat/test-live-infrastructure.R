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
  response <- live_request("https://graph.geoconnex.us/") |>
    httr2::req_method("POST") |>
    httr2::req_headers(Accept = "application/sparql-results+json") |>
    httr2::req_body_raw(charToRaw(query), type = "application/sparql-query") |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(response), 200L)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  sites <- vapply(body$results$bindings, function(x) x$site$value, character(1))
  expect_contains(sites, "https://geoconnex.us/ref/gages/1000001")
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
