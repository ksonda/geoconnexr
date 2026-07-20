mainstem_inverse_test_uri <- function(id) {
  paste0("https://geoconnex.us/ref/mainstems/", id)
}

mainstem_inverse_test_block_network <- function(
    .env = rlang::caller_env()) {
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  blocked <- function(...) {
    state$calls <- state$calls + 1L
    stop("inverse crosswalk attempted network access", call. = FALSE)
  }
  withr::local_options(
    list(geoconnexr.file_performer = blocked),
    .local_envir = .env
  )
  testthat::local_mocked_bindings(
    gx_http_request = blocked,
    .package = "geoconnexr",
    .env = .env
  )
  state
}

mainstem_inverse_test_install <- function(
    .env = rlang::caller_env()) {
  data_dir <- withr::local_tempdir(.local_envir = .env)
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  spec$release <- "v3.2"
  gx_lookup_mock_spec(spec, .env = .env)
  installed <- gx_mainstem_lookup_install(
    source = "file",
    file = fixture,
    version = spec$release,
    confirm = FALSE,
    offline = TRUE,
    data_dir = data_dir
  )
  list(
    data_dir = data_dir,
    fixture = fixture,
    spec = spec,
    installed = installed
  )
}

test_that("empty inverse results are typed without data or network access", {
  network <- mainstem_inverse_test_block_network()
  data_dir <- tempfile("missing-mainstem-inverse-")

  out <- gx_mainstem_to_comids_impl(character(), data_dir = data_dir)
  metadata <- attr(out, "gx_crosswalk")

  expect_s3_class(out, "gx_mainstem_comid_crosswalk")
  expect_s3_class(out, "gx_crosswalk")
  expect_identical(names(out), names(gx_empty_mainstem_comid_crosswalk()))
  expect_identical(nrow(out), 0L)
  expect_type(out$input_index, "integer")
  expect_type(out$requested_mainstem_uri, "character")
  expect_type(out$match_index, "integer")
  expect_type(out$comid, "character")
  expect_true(is.list(out$diagnostics))
  expect_identical(metadata$operation, "mainstem_to_comids")
  expect_identical(metadata$input_count, 0L)
  expect_identical(metadata$unique_input_count, 0L)
  expect_identical(metadata$matched_input_count, 0L)
  expect_identical(metadata$match_count, 0L)
  expect_identical(metadata$not_found_input_count, 0L)
  expect_identical(metadata$ambiguous_input_count, 0L)
  expect_true(metadata$complete)
  expect_identical(metadata$mapping$release, "v3.2")
  expect_identical(metadata$mapping$cache_origin, "not_loaded")
  expect_identical(metadata$mapping$currentness_policy, "not_checked")
  expect_true(is.na(metadata$retrieved_at))
  expect_false(dir.exists(data_dir))
  expect_identical(network$calls, 0L)
  expect_invisible(geoconnexr:::gx_validate_mainstem_comid_crosswalk(out))
})

test_that("inverse URI validation is canonical, bounded, and offline", {
  network <- mainstem_inverse_test_block_network()
  invalid_bytes <- rawToChar(as.raw(0xff))
  Encoding(invalid_bytes) <- "bytes"
  invalid <- list(
    1826652,
    NA_character_,
    "",
    " ",
    "http://geoconnex.us/ref/mainstems/1826652",
    "HTTPS://geoconnex.us/ref/mainstems/1826652",
    "https://GEOCONNEX.us/ref/mainstems/1826652",
    "https://geoconnex.us.evil.example/ref/mainstems/1826652",
    "https://geoconnex.us/ref/mainstem/1826652",
    "https://geoconnex.us/ref/mainstems/0",
    "https://geoconnex.us/ref/mainstems/01826652",
    "https://geoconnex.us/ref/mainstems/1826652/",
    "https://geoconnex.us/ref/mainstems/1826652?f=json",
    "https://geoconnex.us/ref/mainstems/1826652#fragment",
    "https://geoconnex.us/ref/mainstems/1826652\n",
    "https://geoconnex.us/ref/mainstems/1826652\r",
    "https://geoconnex.us/ref/mainstems/1826652\r\n",
    "https://geoconnex.us/ref/mainstems/1826652\u2028",
    "https://geoconnex.us/ref/mainstems/\u0661",
    paste0(
      "https://geoconnex.us/ref/mainstems/",
      strrep("9", 257L)
    ),
    invalid_bytes
  )

  for (value in invalid) {
    expect_error(
      gx_mainstem_to_comids_impl(value, data_dir = tempfile()),
      class = "gx_error_crosswalk_input"
    )
  }

  expect_error(
    withr::with_options(
      list(geoconnexr.crosswalk_max_inputs = 1L),
      gx_mainstem_to_comids_impl(
        c(mainstem_inverse_test_uri("1"), mainstem_inverse_test_uri("2")),
        data_dir = tempfile()
      )
    ),
    class = "gx_error_crosswalk_budget"
  )
  expect_identical(network$calls, 0L)
})

test_that("missing inverse lookup data fails closed without network", {
  network <- mainstem_inverse_test_block_network()
  data_dir <- tempfile("missing-mainstem-inverse-")

  expect_error(
    gx_mainstem_to_comids_impl(
      mainstem_inverse_test_uri("1826652"),
      data_dir = data_dir
    ),
    class = "gx_error_crosswalk_lookup_missing"
  )
  expect_false(dir.exists(data_dir))
  expect_identical(network$calls, 0L)
})

test_that("tampered inverse lookup bytes fail local verification", {
  network <- mainstem_inverse_test_block_network()
  setup <- mainstem_inverse_test_install()
  path <- setup$installed$path[[1L]]
  connection <- file(path, open = "r+b")
  writeBin(charToRaw("X"), connection)
  close(connection)

  expect_error(
    gx_mainstem_to_comids_impl(
      mainstem_inverse_test_uri("1826652"),
      version = setup$spec$release,
      data_dir = setup$data_dir
    ),
    class = "gx_error_crosswalk_lookup_integrity"
  )
  expect_identical(network$calls, 0L)
})

test_that("fixture inverse is complete only within the mocked fixture release", {
  network <- mainstem_inverse_test_block_network()
  setup <- mainstem_inverse_test_install()
  uri <- mainstem_inverse_test_uri("1826652")

  out <- gx_mainstem_to_comids_impl(
    uri,
    version = setup$spec$release,
    data_dir = setup$data_dir
  )
  metadata <- attr(out, "gx_crosswalk")

  expect_s3_class(out, "gx_mainstem_comid_crosswalk")
  expect_identical(out$input_index, c(1L, 1L))
  expect_identical(out$requested_mainstem_uri, rep(uri, 2L))
  expect_identical(out$status, rep("matched", 2L))
  expect_identical(out$match_index, c(1L, 2L))
  expect_identical(out$mainstem_uri, rep(uri, 2L))
  expect_identical(out$comid, c("2804607", "2804621"))
  expect_identical(out$mapping_release, rep("v3.2", 2L))
  expect_identical(
    out$mainstem_status,
    rep("active_in_mapping_release", 2L)
  )
  expect_identical(
    vapply(out$diagnostics, function(x) x$code[[1L]], character(1)),
    rep("mainstem_currentness_not_checked", 2L)
  )
  expect_identical(metadata$input_count, 1L)
  expect_identical(metadata$unique_input_count, 1L)
  expect_identical(metadata$matched_input_count, 1L)
  expect_identical(metadata$match_count, 2L)
  expect_identical(metadata$not_found_input_count, 0L)
  expect_identical(metadata$mapping$active_state, "non_superseded_at_release")
  expect_identical(metadata$mapping$currentness_policy, "not_checked")
  expect_identical(nrow(metadata$requests), 0L)
  expect_invisible(geoconnexr:::gx_validate_mainstem_comid_crosswalk(out))

  stored_out_of_order <- gx_mainstem_to_comids_impl(
    mainstem_inverse_test_uri("359842"),
    version = setup$spec$release,
    data_dir = setup$data_dir
  )
  expect_identical(
    stored_out_of_order$comid,
    c("13653850", "937070225")
  )
  expect_identical(network$calls, 0L)
})

test_that("absent mainstem URIs produce exactly one explicit sentinel", {
  network <- mainstem_inverse_test_block_network()
  setup <- mainstem_inverse_test_install()
  uri <- mainstem_inverse_test_uri("9999999")

  out <- gx_mainstem_to_comids_impl(
    uri,
    version = setup$spec$release,
    data_dir = setup$data_dir
  )
  metadata <- attr(out, "gx_crosswalk")

  expect_identical(nrow(out), 1L)
  expect_identical(out$input_index, 1L)
  expect_identical(out$requested_mainstem_uri, uri)
  expect_identical(out$status, "not_found")
  expect_true(is.na(out$match_index[[1L]]))
  expect_true(is.na(out$mainstem_uri[[1L]]))
  expect_true(is.na(out$comid[[1L]]))
  expect_identical(out$mapping_release, "v3.2")
  expect_true(is.na(out$mainstem_status[[1L]]))
  expect_identical(
    out$diagnostics[[1L]]$code,
    "not_found_in_mapping_release"
  )
  expect_identical(out$diagnostics[[1L]]$path, "/inputs/0")
  expect_identical(metadata$matched_input_count, 0L)
  expect_identical(metadata$match_count, 0L)
  expect_identical(metadata$not_found_input_count, 1L)

  matched_uri <- mainstem_inverse_test_uri("1826652")
  second_absent <- mainstem_inverse_test_uri("9999998")
  mixed <- gx_mainstem_to_comids_impl(
    c(uri, matched_uri, second_absent),
    version = setup$spec$release,
    data_dir = setup$data_dir
  )
  expect_identical(mixed$input_index, c(1L, 2L, 2L, 3L))
  expect_identical(
    mixed$status,
    c("not_found", "matched", "matched", "not_found")
  )
  expect_identical(
    mixed$requested_mainstem_uri,
    c(uri, matched_uri, matched_uri, second_absent)
  )
  expect_identical(network$calls, 0L)
})

test_that("duplicate inverse inputs expand each occurrence deterministically", {
  network <- mainstem_inverse_test_block_network()
  setup <- mainstem_inverse_test_install()
  uri <- mainstem_inverse_test_uri("1826652")

  out <- gx_mainstem_to_comids_impl(
    c(uri, uri),
    version = setup$spec$release,
    data_dir = setup$data_dir
  )
  metadata <- attr(out, "gx_crosswalk")

  expect_identical(out$input_index, c(1L, 1L, 2L, 2L))
  expect_identical(out$requested_mainstem_uri, rep(uri, 4L))
  expect_identical(out$status, rep("matched", 4L))
  expect_identical(out$match_index, c(1L, 2L, 1L, 2L))
  expect_identical(
    out$comid,
    c("2804607", "2804621", "2804607", "2804621")
  )
  expect_identical(
    vapply(out$diagnostics, function(x) x$path[[1L]], character(1)),
    c("/inputs/0", "/inputs/0", "/inputs/1", "/inputs/1")
  )
  expect_identical(metadata$input_count, 2L)
  expect_identical(metadata$unique_input_count, 1L)
  expect_identical(metadata$matched_input_count, 2L)
  expect_identical(metadata$match_count, 4L)
  expect_identical(metadata$not_found_input_count, 0L)
  expect_identical(network$calls, 0L)
})

test_that("generalized scanner retains COMID mode and supports URI mode", {
  network <- mainstem_inverse_test_block_network()
  setup <- mainstem_inverse_test_install()
  uri <- mainstem_inverse_test_uri("1826652")
  path <- setup$installed$path[[1L]]

  scans <- withr::with_options(
    list(geoconnexr.lookup_chunk_rows = 1L),
    list(
      inverse = gx_mainstem_lookup_scan(
        path,
        setup$spec,
        targets = uri,
        target_field = "uri",
        max_matches = 2L
      ),
      forward = gx_mainstem_lookup_scan(
        path,
        setup$spec,
        targets = "17789327"
      )
    )
  )

  expect_identical(names(scans$inverse), c("uri", "comid"))
  expect_identical(scans$inverse$uri, rep(uri, 2L))
  expect_identical(scans$inverse$comid, c("2804607", "2804621"))
  expect_identical(scans$forward$comid, "17789327")
  expect_identical(
    scans$forward$uri,
    mainstem_inverse_test_uri("1622734")
  )
  expect_error(
    gx_mainstem_lookup_scan(
      path,
      setup$spec,
      targets = uri,
      target_field = "mainstem"
    ),
    class = "gx_error_crosswalk_lookup_contract"
  )
  expect_error(
    gx_mainstem_lookup_scan(
      path,
      setup$spec,
      targets = uri,
      target_field = "uri",
      max_matches = 1L
    ),
    class = "gx_error_crosswalk_budget"
  )

  ambiguous <- gx_lookup_fixture("nhdpv2-lookup-ambiguous.synthetic.csv")
  zero_or_one <- gx_lookup_test_spec(
    ambiguous,
    known_answers = tibble::tibble(
      comid = character(),
      mainstem_uri = character()
    ),
    known_absent = "999"
  )
  expect_error(
    gx_mainstem_lookup_scan(
      ambiguous,
      zero_or_one,
      targets = "500"
    ),
    class = "gx_error_crosswalk_lookup_integrity"
  )
  expect_identical(network$calls, 0L)
})

test_that("inverse match and expansion row budgets fail closed", {
  network <- mainstem_inverse_test_block_network()
  setup <- mainstem_inverse_test_install()
  uri <- mainstem_inverse_test_uri("1826652")

  expect_error(
    withr::with_options(
      list(geoconnexr.crosswalk_max_matches = 1L),
      gx_mainstem_to_comids_impl(
        uri,
        version = setup$spec$release,
        data_dir = setup$data_dir
      )
    ),
    class = "gx_error_crosswalk_budget"
  )
  expect_error(
    withr::with_options(
      list(geoconnexr.crosswalk_max_rows = 3L),
      gx_mainstem_to_comids_impl(
        c(uri, uri),
        version = setup$spec$release,
        data_dir = setup$data_dir
      )
    ),
    class = "gx_error_crosswalk_budget"
  )

  chunked <- withr::with_options(
    list(geoconnexr.lookup_chunk_rows = 1L),
    gx_mainstem_to_comids_impl(
      uri,
      version = setup$spec$release,
      data_dir = setup$data_dir
    )
  )
  expect_identical(chunked$comid, c("2804607", "2804621"))
  expect_identical(network$calls, 0L)
})

test_that("inverse and forward mappings round-trip in one mocked v3.2 release", {
  network <- mainstem_inverse_test_block_network()
  setup <- mainstem_inverse_test_install()
  uri <- mainstem_inverse_test_uri("1826652")

  inverse <- gx_mainstem_to_comids_impl(
    uri,
    version = setup$spec$release,
    data_dir = setup$data_dir
  )
  forward <- gx_comid_to_mainstem_impl(
    inverse$comid,
    version = setup$spec$release,
    data_dir = setup$data_dir
  )

  expect_identical(forward$status, rep("matched", 2L))
  expect_identical(forward$match_index, c(1L, 1L))
  expect_identical(forward$comid, inverse$comid)
  expect_identical(forward$mainstem_uri, rep(uri, 2L))
  expect_identical(forward$mapping_release, inverse$mapping_release)
  expect_identical(
    attr(forward, "gx_crosswalk")$mapping$release,
    attr(inverse, "gx_crosswalk")$mapping$release
  )
  expect_identical(network$calls, 0L)
})

test_that("inverse validators reject row and metadata tampering", {
  network <- mainstem_inverse_test_block_network()
  setup <- mainstem_inverse_test_install()
  uri <- mainstem_inverse_test_uri("1826652")
  out <- gx_mainstem_to_comids_impl(
    uri,
    version = setup$spec$release,
    data_dir = setup$data_dir
  )
  metadata <- attr(out, "gx_crosswalk")
  expect_invisible(
    geoconnexr:::gx_validate_mainstem_comid_crosswalk(out, metadata)
  )

  bad_order <- out[c(2L, 1L), , drop = FALSE]

  bad_type <- out
  bad_type$match_index <- as.double(bad_type$match_index)

  bad_row_release <- out
  bad_row_release$mapping_release[] <- "v0.0"

  bad_row_currentness <- out
  bad_row_currentness$mainstem_status[[1L]] <- "current"

  bad_row_diagnostic <- out
  bad_row_diagnostic$diagnostics[[1L]]$code[[1L]] <- "not_found_in_mapping_release"

  oversized_uri <- mainstem_inverse_test_uri(strrep("9", 257L))
  bad_oversized_identity <- out
  bad_oversized_identity$requested_mainstem_uri[] <- oversized_uri
  bad_oversized_identity$mainstem_uri[] <- oversized_uri

  for (tampered in list(
    bad_order,
    bad_type,
    bad_row_release,
    bad_row_currentness,
    bad_row_diagnostic,
    bad_oversized_identity
  )) {
    expect_error(
      geoconnexr:::gx_validate_mainstem_comid_crosswalk(tampered, metadata),
      class = "gx_error_crosswalk_contract"
    )
  }

  bad_count <- metadata
  bad_count$match_count <- bad_count$match_count + 1L

  bad_mapping_release <- metadata
  bad_mapping_release$mapping$release <- "v0.0"

  bad_mapping_currentness <- metadata
  bad_mapping_currentness$mapping$currentness_policy <- "checked"

  bad_metadata_diagnostic <- metadata
  bad_metadata_diagnostic$diagnostics$code[[1L]] <- "tampered"

  for (tampered in list(
    bad_count,
    bad_mapping_release,
    bad_mapping_currentness,
    bad_metadata_diagnostic
  )) {
    expect_error(
      geoconnexr:::gx_validate_mainstem_comid_crosswalk(out, tampered),
      class = "gx_error_crosswalk_contract"
    )
  }
  expect_identical(network$calls, 0L)
})

test_that("inverse and generalized scanner remain internal", {
  namespace <- asNamespace("geoconnexr")
  exports <- getNamespaceExports("geoconnexr")

  expect_true(exists(
    "gx_mainstem_to_comids_impl",
    envir = namespace,
    inherits = FALSE
  ))
  expect_true(exists(
    "gx_mainstem_lookup_scan",
    envir = namespace,
    inherits = FALSE
  ))
  expect_false("gx_mainstem_to_comids_impl" %in% exports)
  expect_false("gx_mainstem_lookup_scan" %in% exports)
  expect_false("gx_validate_mainstem_comid_crosswalk" %in% exports)
  expect_false("gx_comid_to_mainstem_impl" %in% exports)
})
