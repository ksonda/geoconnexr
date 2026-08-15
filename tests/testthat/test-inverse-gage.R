gx_inverse_gage_test_handler <- function(items) {
  force(items)
  function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_cw_test_response(
        request,
        body = gx_cw_test_fixture("queryables-gages.min.json"),
        content_type = "application/schema+json"
      ))
    }
    parsed <- httr2::url_parse(request$url)
    mainstem_uri <- parsed$query$mainstem_uri %||% ""
    value <- items[[mainstem_uri]]
    if (is.null(value)) value <- gx_cw_test_collection(list(), 0L)
    gx_cw_test_response(
      request,
      body = if (is.raw(value)) value else gx_cw_test_json(value),
      content_type = "application/geo+json"
    )
  }
}

test_that("inverse gage crosswalk returns every deterministic member", {
  mainstem <- "https://geoconnex.us/ref/mainstems/1622734"
  absent <- "https://geoconnex.us/ref/mainstems/999999999"
  collection <- gx_cw_test_collection(list(
    gx_cw_test_feature(1000002L, "USGS-08332620", mainstem_uri = mainstem),
    gx_cw_test_feature(1000001L, "USGS-08332622", mainstem_uri = mainstem),
    gx_cw_test_feature(1028940L, "USGS-08332624", mainstem_uri = mainstem)
  ))
  setup <- gx_cw_test_client(gx_inverse_gage_test_handler(
    setNames(list(collection), mainstem)
  ))

  out <- gx_mainstem_to_gages(
    c(mainstem, absent, mainstem),
    client = setup$client
  )
  metadata <- attr(out, "gx_crosswalk")

  expect_s3_class(out, "gx_inverse_gage_crosswalk")
  expect_s3_class(out, "gx_crosswalk")
  expect_identical(out$input_index, c(1L, 1L, 1L, 2L, 3L, 3L, 3L))
  expect_identical(
    out$status,
    c("matched", "matched", "matched", "not_found", "matched", "matched", "matched")
  )
  expect_identical(
    out$gage_id,
    c("1000001", "1000002", "1028940", NA_character_, "1000001", "1000002", "1028940")
  )
  expect_identical(
    out$provider_id,
    c(
      "USGS-08332622", "USGS-08332620", "USGS-08332624", NA_character_,
      "USGS-08332622", "USGS-08332620", "USGS-08332624"
    )
  )
  expect_identical(out$match_index, c(1L, 2L, 3L, NA_integer_, 1L, 2L, 3L))
  expect_true(all(out$mainstem_status[out$status == "matched"] == "currentness_not_checked"))
  expect_identical(out$diagnostics[[4L]]$code, "not_found")
  expect_contains(out$diagnostics[[1L]]$code, "mainstem_currentness_not_checked")
  expect_identical(metadata$operation, "mainstem_to_gages")
  expect_identical(metadata$currentness_policy, "not_checked")
  expect_identical(metadata$input_count, 3L)
  expect_identical(metadata$unique_input_count, 2L)
  expect_identical(metadata$matched_input_count, 2L)
  expect_identical(metadata$not_found_input_count, 1L)
  expect_identical(metadata$match_count, 6L)
  expect_identical(nrow(metadata$requests), 3L)
  expect_identical(length(setup$state$calls), 3L)
})

test_that("inverse gage crosswalk preserves missing COMIDs", {
  mainstem <- "https://geoconnex.us/ref/mainstems/1622734"
  collection <- gx_cw_test_collection(list(
    gx_cw_test_feature(
      1000001L,
      "USGS-08332622",
      mainstem_uri = mainstem,
      comid = NULL
    )
  ))
  setup <- gx_cw_test_client(gx_inverse_gage_test_handler(
    setNames(list(collection), mainstem)
  ))

  out <- gx_mainstem_to_gages(mainstem, client = setup$client)

  expect_true(is.na(out$comid))
  expect_contains(out$diagnostics[[1L]]$code, "missing_comid")
})

test_that("inverse gage inputs and aggregate limits fail closed", {
  mainstem <- "https://geoconnex.us/ref/mainstems/1622734"
  setup <- gx_cw_test_client(gx_inverse_gage_test_handler(list()))

  for (value in list(
    1622734,
    NA_character_,
    "",
    "https://geoconnex.us/ref/mainstems/0",
    "https://evil.example/ref/mainstems/1622734"
  )) {
    expect_error(
      gx_mainstem_to_gages(value, client = setup$client),
      class = "gx_error_crosswalk_input"
    )
  }
  expect_identical(length(setup$state$calls), 0L)

  empty <- gx_mainstem_to_gages(character(), client = setup$client)
  expect_s3_class(empty, "gx_inverse_gage_crosswalk")
  expect_identical(nrow(empty), 0L)
  expect_identical(length(setup$state$calls), 0L)

  withr::local_options(list(geoconnexr.crosswalk_max_requests = 1L))
  expect_error(
    gx_mainstem_to_gages(mainstem, client = setup$client),
    class = "gx_error_crosswalk_budget"
  )
})

test_that("inverse gage crosswalk rejects contradictory mainstem identity", {
  requested <- "https://geoconnex.us/ref/mainstems/1622734"
  feature <- gx_cw_test_feature(
    1000001L,
    "USGS-08332622",
    mainstem_uri = "https://geoconnex.us/ref/mainstems/147890"
  )
  setup <- gx_cw_test_client(gx_inverse_gage_test_handler(
    setNames(list(gx_cw_test_collection(list(feature))), requested)
  ))

  expect_error(
    gx_mainstem_to_gages(requested, client = setup$client),
    class = "gx_error_crosswalk_identity"
  )
})

test_that("inverse gage crosswalk enforces aggregate matches", {
  mainstem <- "https://geoconnex.us/ref/mainstems/1622734"
  collection <- gx_cw_test_collection(list(
    gx_cw_test_feature(1L, "P1", mainstem_uri = mainstem),
    gx_cw_test_feature(2L, "P2", mainstem_uri = mainstem)
  ))
  setup <- gx_cw_test_client(gx_inverse_gage_test_handler(
    setNames(list(collection), mainstem)
  ))
  withr::local_options(list(geoconnexr.crosswalk_max_matches = 1L))

  expect_error(
    gx_mainstem_to_gages(mainstem, client = setup$client),
    class = "gx_error_crosswalk_budget"
  )
})
