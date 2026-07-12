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
  response <- live_request("https://geoconnex.us/ref/mainstems/29559") |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(response), 303L)
  expect_match(
    httr2::resp_header(response, "location"),
    "^https://reference\\.geoconnex\\.us/"
  )
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
