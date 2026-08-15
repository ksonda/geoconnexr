gx_point_test_fixture <- function() {
  path <- testthat::test_path(
    "..", "fixtures", "nldi", "comid-position-9406116.min.geojson"
  )
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

gx_point_test_body <- function(comid = "17789327") {
  payload <- gx_point_test_fixture()
  payload$features[[1L]]$id <- as.numeric(comid)
  payload$features[[1L]]$properties$identifier <- comid
  payload$features[[1L]]$properties$comid <- as.numeric(comid)
  as.character(jsonlite::toJSON(
    payload, auto_unbox = TRUE, null = "null", digits = NA
  ))
}

gx_point_test_response <- function(request, status = 200L, body = raw(),
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

gx_point_test_client <- function(handler) {
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

gx_point_test_geometry <- function(x = -75.732089877, y = 39.049379755,
                                   crs = 4326) {
  sf::st_sfc(sf::st_point(c(x, y)), crs = crs)
}

test_that("point crosswalk deduplicates NLDI and uses the pinned mapping", {
  data_dir <- withr::local_tempdir()
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  gx_mainstem_lookup_install(
    source = "file", file = fixture, version = spec$release,
    confirm = FALSE, offline = TRUE, data_dir = data_dir
  )
  setup <- gx_point_test_client(function(request, state) {
    expect_true(grepl("/comid/position[?]", request$url))
    expect_true(grepl("f=json", request$url, fixed = TRUE))
    expect_true(grepl("coords=POINT", request$url, fixed = TRUE))
    gx_point_test_response(request, body = gx_point_test_body())
  })
  point <- gx_point_test_geometry()
  points <- c(point, point)
  out <- gx_point_to_mainstem(
    points,
    version = spec$release,
    data_dir = data_dir,
    client = setup$client
  )

  expect_s3_class(out, "gx_point_crosswalk")
  expect_identical(out$input_index, 1:2)
  expect_identical(out$status, c("matched", "matched"))
  expect_identical(out$comid, c("17789327", "17789327"))
  expect_identical(
    out$mainstem_uri,
    rep("https://geoconnex.us/ref/mainstems/1622734", 2L)
  )
  expect_identical(out$match_source, rep("pinned_comid_mapping", 2L))
  expect_identical(length(setup$state$calls), 1L)
  metadata <- attr(out, "gx_crosswalk")
  expect_identical(metadata$unique_input_count, 1L)
  expect_identical(metadata$mapping$release, spec$release)
  expect_identical(nrow(metadata$requests), 1L)
})

test_that("point crosswalk transforms declared projected CRS offline", {
  data_dir <- withr::local_tempdir()
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  gx_mainstem_lookup_install(
    source = "file", file = fixture, version = spec$release,
    confirm = FALSE, offline = TRUE, data_dir = data_dir
  )
  setup <- gx_point_test_client(function(request, state) {
    gx_point_test_response(request, body = gx_point_test_body())
  })
  projected <- sf::st_transform(gx_point_test_geometry(), 3857)
  out <- gx_point_to_mainstem(
    projected,
    version = spec$release,
    data_dir = data_dir,
    client = setup$client
  )
  expect_equal(out$longitude, -75.732089877, tolerance = 1e-8)
  expect_equal(out$latitude, 39.049379755, tolerance = 1e-8)
})

test_that("point crosswalk preserves release-level ambiguity", {
  data_dir <- withr::local_tempdir()
  fixture <- gx_lookup_fixture("nhdpv2-lookup-ambiguous.synthetic.csv")
  spec <- gx_lookup_test_spec(
    fixture,
    forward_cardinality = "zero_or_many",
    known_answers = tibble::tibble(
      comid = "600",
      mainstem_uri = "https://geoconnex.us/ref/mainstems/30"
    ),
    known_absent = "999"
  )
  gx_lookup_mock_spec(spec)
  gx_mainstem_lookup_install(
    source = "file", file = fixture, version = spec$release,
    confirm = FALSE, offline = TRUE, data_dir = data_dir
  )
  setup <- gx_point_test_client(function(request, state) {
    gx_point_test_response(request, body = gx_point_test_body("500"))
  })
  out <- gx_point_to_mainstem(
    gx_point_test_geometry(),
    version = spec$release,
    data_dir = data_dir,
    client = setup$client
  )
  expect_identical(out$status, c("ambiguous", "ambiguous"))
  expect_identical(out$match_index, 1:2)
  expect_identical(
    out$mainstem_uri,
    c(
      "https://geoconnex.us/ref/mainstems/10",
      "https://geoconnex.us/ref/mainstems/20"
    )
  )
  metadata <- attr(out, "gx_crosswalk")
  expect_identical(metadata$ambiguous_input_count, 1L)
  expect_identical(metadata$match_count, 2L)
})

test_that("point and mapping not-found states remain distinct", {
  missing_setup <- gx_point_test_client(function(request, state) {
    gx_point_test_response(
      request,
      status = 404L,
      body = '{"type":"about:blank","status":404}',
      media_type = "application/problem+json"
    )
  })
  data_dir <- tempfile("missing-lookup-")
  missing <- gx_point_to_mainstem(
    gx_point_test_geometry(0, 0),
    data_dir = data_dir,
    client = missing_setup$client
  )
  expect_identical(missing$status, "not_found")
  expect_true(is.na(missing$comid))
  expect_true(is.na(missing$match_source))
  expect_identical(missing$diagnostics[[1L]]$code, "point_not_found")
  expect_false(dir.exists(data_dir))

  data_dir <- withr::local_tempdir()
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  gx_mainstem_lookup_install(
    source = "file", file = fixture, version = spec$release,
    confirm = FALSE, offline = TRUE, data_dir = data_dir
  )
  mapped_setup <- gx_point_test_client(function(request, state) {
    gx_point_test_response(request, body = gx_point_test_body("999999999"))
  })
  mapped <- gx_point_to_mainstem(
    gx_point_test_geometry(),
    version = spec$release,
    data_dir = data_dir,
    client = mapped_setup$client
  )
  expect_identical(mapped$status, "not_found")
  expect_identical(mapped$comid, "999999999")
  expect_identical(mapped$match_source, "pinned_comid_mapping")
  expect_identical(
    mapped$diagnostics[[1L]]$code,
    "not_found_in_mapping_release"
  )
  expect_identical(attr(mapped, "gx_crosswalk")$mapping$release, spec$release)
})

test_that("point crosswalk does not install missing lookup data", {
  setup <- gx_point_test_client(function(request, state) {
    gx_point_test_response(request, body = gx_point_test_body())
  })
  data_dir <- tempfile("missing-lookup-")
  expect_error(
    gx_point_to_mainstem(
      gx_point_test_geometry(), data_dir = data_dir, client = setup$client
    ),
    class = "gx_error_crosswalk_lookup_missing"
  )
  expect_false(dir.exists(data_dir))
})

test_that("point gates fail before transport", {
  setup <- gx_point_test_client(function(request, state) {
    stop("transport forbidden", call. = FALSE)
  })
  expect_error(
    gx_point_to_mainstem(gx_point_test_geometry(), check = TRUE, client = setup$client),
    class = "gx_error_crosswalk_currentness_unavailable"
  )
  invalid <- list(
    c(-75, 39),
    sf::st_sfc(sf::st_point(c(-75, 39))),
    sf::st_sfc(sf::st_linestring(matrix(c(-75, 39, -74, 40), ncol = 2)), crs = 4326),
    sf::st_sfc(sf::st_point(c(-75, 39, 1)), crs = 4326),
    sf::st_sfc(sf::st_point(), crs = 4326)
  )
  for (value in invalid) {
    expect_error(
      gx_point_to_mainstem(value, client = setup$client),
      class = "gx_error_crosswalk_input"
    )
  }
  expect_identical(length(setup$state$calls), 0L)
})

test_that("point position identity mismatches fail closed", {
  body <- gx_point_test_body()
  payload <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  payload$features[[1L]]$properties$identifier <- "17789328"
  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  setup <- gx_point_test_client(function(request, state) {
    gx_point_test_response(request, body = body)
  })
  expect_error(
    gx_point_to_mainstem(gx_point_test_geometry(), client = setup$client),
    class = "gx_error_crosswalk_nldi_contract"
  )
})

test_that("zero-length Point crosswalk is typed and inert", {
  setup <- gx_point_test_client(function(request, state) {
    stop("transport forbidden", call. = FALSE)
  })
  points <- sf::st_sfc(crs = 4326)
  out <- gx_point_to_mainstem(points, client = setup$client)
  expect_s3_class(out, "gx_point_crosswalk")
  expect_identical(names(out), names(gx_empty_point_crosswalk()))
  expect_identical(nrow(out), 0L)
  expect_identical(length(setup$state$calls), 0L)
})
