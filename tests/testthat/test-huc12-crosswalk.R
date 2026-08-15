gx_huc12_test_fixture <- function() {
  path <- testthat::test_path(
    "..", "fixtures", "nldi", "huc12pp-010100020101.min.geojson"
  )
  readBin(path, "raw", n = file.info(path)$size)
}

gx_huc12_test_response <- function(request, status = 200L, body = raw(),
                                   media_type = "application/geo+json") {
  list(
    status = as.integer(status),
    headers = list(
      "Content-Type" = media_type,
      "Cache-Control" = "public, max-age=3600"
    ),
    body = if (is.raw(body)) body else charToRaw(body),
    url = request$url
  )
}

gx_huc12_test_client <- function(handler) {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  withr::local_options(list(
    geoconnexr.performer = function(request) {
      state$calls[[length(state$calls) + 1L]] <- request
      handler(request, state)
    },
    geoconnexr.dns_resolver = function(host) rep("93.184.216.34", length(host)),
    geoconnexr.clock = function() as.POSIXct(
      "2026-08-15 23:59:59", tz = "UTC"
    ),
    geoconnexr.cache_dir = withr::local_tempdir()
  ), .local_envir = parent.frame())
  list(
    client = gx_client("nldi", retries = 0L, cache = FALSE),
    state = state
  )
}

test_that("HUC12 fixture remains bound to its evidence record", {
  evidence <- jsonlite::fromJSON(
    testthat::test_path(
      "..", "..", "data-raw", "spike", "huc12pp-contract-evidence-v1.json"
    ),
    simplifyVector = FALSE
  )
  path <- testthat::test_path(
    "..", "fixtures", "nldi", "huc12pp-010100020101.min.geojson"
  )
  expect_identical(as.integer(file.info(path)$size), evidence$stored_fixture$bytes)
  expect_identical(
    digest::digest(file = path, algo = "sha256", serialize = FALSE),
    evidence$stored_fixture$sha256
  )
})

test_that("NLDI position fixture remains bound to its evidence record", {
  evidence <- jsonlite::fromJSON(
    testthat::test_path(
      "..", "..", "data-raw", "spike", "nldi-position-evidence-v1.json"
    ),
    simplifyVector = FALSE
  )
  path <- testthat::test_path(
    "..", "fixtures", "nldi", "comid-position-9406116.min.geojson"
  )
  expect_identical(as.integer(file.info(path)$size), evidence$stored_fixture$bytes)
  expect_identical(
    digest::digest(file = path, algo = "sha256", serialize = FALSE),
    evidence$stored_fixture$sha256
  )
})

test_that("HUC12 outlet crosswalk deduplicates transport and keeps not found", {
  setup <- gx_huc12_test_client(function(request, state) {
    if (grepl("/huc12pp/010100020101[?]f=json$", request$url)) {
      return(gx_huc12_test_response(request, body = gx_huc12_test_fixture()))
    }
    if (grepl("/huc12pp/020600050207[?]f=json$", request$url)) {
      return(gx_huc12_test_response(
        request,
        status = 404L,
        body = '{"type":"about:blank","status":404}',
        media_type = "application/problem+json"
      ))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })
  data_dir <- tempfile("missing-lookup-")
  out <- gx_huc12_to_mainstem(
    c("010100020101", "020600050207", "010100020101"),
    data_dir = data_dir,
    client = setup$client
  )

  expect_s3_class(out, "gx_huc12_crosswalk")
  expect_identical(out$input_index, 1:3)
  expect_identical(out$status, c("matched", "not_found", "matched"))
  expect_identical(
    out$mainstem_uri,
    c(
      "https://geoconnex.us/ref/mainstems/2239149",
      NA_character_,
      "https://geoconnex.us/ref/mainstems/2239149"
    )
  )
  expect_identical(out$comid, c("720026", NA_character_, "720026"))
  expect_identical(length(setup$state$calls), 2L)
  expect_false(dir.exists(data_dir))
  metadata <- attr(out, "gx_crosswalk")
  expect_identical(metadata$currentness_policy, "not_checked")
  expect_identical(nrow(metadata$requests), 2L)
  expect_null(metadata$mapping)
})

test_that("HUC12 COMID fallback uses only an installed pinned lookup", {
  data_dir <- withr::local_tempdir()
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  gx_mainstem_lookup_install(
    source = "file", file = fixture, version = spec$release,
    confirm = FALSE, offline = TRUE, data_dir = data_dir
  )
  payload <- jsonlite::fromJSON(
    rawToChar(gx_huc12_test_fixture()), simplifyVector = FALSE
  )
  payload$features[[1]]$properties$mainstem <- NULL
  payload$features[[1]]$properties$comid <- 17789327
  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  setup <- gx_huc12_test_client(function(request, state) {
    gx_huc12_test_response(request, body = body)
  })

  out <- gx_huc12_to_mainstem(
    "010100020101",
    version = spec$release,
    data_dir = data_dir,
    client = setup$client
  )
  expect_identical(out$status, "matched")
  expect_identical(out$match_source, "pinned_comid_mapping")
  expect_identical(out$comid, "17789327")
  expect_identical(
    out$mainstem_uri,
    "https://geoconnex.us/ref/mainstems/1622734"
  )
  expect_identical(attr(out, "gx_crosswalk")$mapping$release, spec$release)
})

test_that("HUC12 COMID fallback preserves mapping-level not found", {
  data_dir <- withr::local_tempdir()
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  gx_mainstem_lookup_install(
    source = "file", file = fixture, version = spec$release,
    confirm = FALSE, offline = TRUE, data_dir = data_dir
  )
  payload <- jsonlite::fromJSON(
    rawToChar(gx_huc12_test_fixture()), simplifyVector = FALSE
  )
  payload$features[[1]]$properties$mainstem <- NULL
  payload$features[[1]]$properties$comid <- 999999999
  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  setup <- gx_huc12_test_client(function(request, state) {
    gx_huc12_test_response(request, body = body)
  })

  out <- gx_huc12_to_mainstem(
    "010100020101",
    version = spec$release,
    data_dir = data_dir,
    client = setup$client
  )
  expect_identical(out$status, "not_found")
  expect_identical(out$comid, "999999999")
  expect_true(is.na(out$mainstem_uri))
  expect_identical(out$match_source, "pinned_comid_mapping")
  expect_identical(
    out$diagnostics[[1L]]$code,
    "not_found_in_mapping_release"
  )
})

test_that("HUC12 public gates fail before transport", {
  setup <- gx_huc12_test_client(function(request, state) {
    stop("transport forbidden", call. = FALSE)
  })
  expect_error(
    gx_huc12_to_mainstem("010100020101", method = "intersects", client = setup$client),
    class = "gx_error_crosswalk_method_unavailable"
  )
  expect_error(
    gx_huc12_to_mainstem("010100020101", check = TRUE, client = setup$client),
    class = "gx_error_crosswalk_currentness_unavailable"
  )
  for (value in list(10100020101, NA_character_, "01010002010", "01010002010x")) {
    expect_error(
      gx_huc12_to_mainstem(value, client = setup$client),
      class = "gx_error_crosswalk_input"
    )
  }
  expect_identical(length(setup$state$calls), 0L)
})

test_that("HUC12 identity mismatches fail closed", {
  payload <- jsonlite::fromJSON(
    rawToChar(gx_huc12_test_fixture()), simplifyVector = FALSE
  )
  payload$features[[1]]$properties$identifier <- "010100020102"
  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  setup <- gx_huc12_test_client(function(request, state) {
    gx_huc12_test_response(request, body = body)
  })
  expect_error(
    gx_huc12_to_mainstem("010100020101", client = setup$client),
    class = "gx_error_crosswalk_nldi_contract"
  )
})

test_that("zero-length HUC12 crosswalk is typed and inert", {
  setup <- gx_huc12_test_client(function(request, state) {
    stop("transport forbidden", call. = FALSE)
  })
  out <- gx_huc12_to_mainstem(character(), client = setup$client)
  expect_s3_class(out, "gx_huc12_crosswalk")
  expect_identical(names(out), names(gx_empty_huc12_crosswalk()))
  expect_identical(nrow(out), 0L)
  expect_identical(length(setup$state$calls), 0L)
})
