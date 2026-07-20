test_that("crosswalk fixtures are hash-pinned minimized evidence", {
  root <- testthat::test_path("..", "fixtures", "crosswalk")
  manifest <- jsonlite::fromJSON(
    file.path(root, "manifest-v1.json"),
    simplifyVector = FALSE
  )
  entries <- manifest$fixtures
  paths <- vapply(entries, `[[`, character(1), "path")
  hashes <- vapply(entries, `[[`, character(1), "stored_sha256")
  fixture_files <- list.files(root, pattern = "[.](json|geojson|csv)$")
  fixture_files <- setdiff(fixture_files, "manifest-v1.json")

  expect_identical(manifest$contract_version, "0.1.0")
  expect_match(manifest$checked_at, "^2026-07-13T")
  expect_setequal(paths, fixture_files)
  actual <- vapply(
    file.path(root, paths),
    digest::digest,
    character(1),
    algo = "sha256",
    serialize = FALSE,
    file = TRUE
  )
  expect_identical(unname(actual), unname(hashes))
  source_urls <- vapply(entries, `[[`, character(1), "source_url")
  expect_true(all(startsWith(source_urls, "https://")))
  evidence_kinds <- vapply(entries, `[[`, character(1), "evidence_kind")
  observed_reference <- evidence_kinds %in% c(
    "observed_minimized_schema", "observed_minimized_feature"
  )
  expect_true(all(startsWith(
    source_urls[observed_reference],
    "https://reference.geoconnex.us/"
  )))
})

test_that("known provider gage crosswalk preserves its typed contract", {
  setup <- gx_cw_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_cw_test_response(
        request,
        body = gx_cw_test_fixture("queryables-gages.min.json"),
        content_type = "application/schema+json"
      ))
    }
    gx_cw_test_response(
      request,
      body = gx_cw_test_fixture("gage-usgs-08332622.min.geojson"),
      content_type = "application/geo+json"
    )
  })

  out <- gx_gage_to_pid("USGS-08332622", client = setup$client)
  metadata <- attr(out, "gx_crosswalk")

  expect_s3_class(out, "gx_gage_crosswalk")
  expect_s3_class(out, "gx_crosswalk")
  expect_identical(names(out), c(
    "contract_version", "input_index", "requested_provider_id", "status",
    "match_index", "provider_id", "gage_id", "gage_uri",
    "mainstem_uri", "comid", "diagnostics"
  ))
  expect_identical(out$contract_version, "0.1.0")
  expect_identical(out$input_index, 1L)
  expect_identical(out$status, "matched")
  expect_identical(out$gage_id, "1000001")
  expect_identical(out$gage_uri, "https://geoconnex.us/ref/gages/1000001")
  expect_identical(out$mainstem_uri, "https://geoconnex.us/ref/mainstems/1622734")
  expect_identical(out$comid, "17789327")
  expect_type(out$provider_id, "character")
  expect_type(out$gage_id, "character")
  expect_type(out$mainstem_uri, "character")
  expect_type(out$comid, "character")
  expect_contains(out$diagnostics[[1]]$code, "mainstem_vintage_unverified")
  expect_identical(names(metadata), c(
    "contract_version", "operation", "input_count", "unique_input_count",
    "matched_input_count", "match_count", "not_found_input_count",
    "ambiguous_input_count", "complete", "retrieved_at", "requests",
    "diagnostics"
  ))
  expect_identical(metadata$operation, "gage_to_pid")
  expect_identical(metadata$input_count, 1L)
  expect_identical(metadata$match_count, 1L)
  expect_true(metadata$complete)
  expect_equal(nrow(metadata$requests), 2L)
  expect_identical(names(metadata$requests), names(geoconnexr:::gx_ref_empty_requests()))
  returned <- NULL
  invisible(capture.output(returned <- print(out)))
  expect_s3_class(returned, "gx_gage_crosswalk")

  broken <- out
  broken$status <- "selected"
  expect_error(
    geoconnexr:::gx_validate_gage_crosswalk(broken, metadata),
    class = "gx_error_crosswalk_contract"
  )
})

test_that("zero provider identifiers return a typed zero-row crosswalk", {
  setup <- gx_cw_test_client(function(request, state) {
    stop("Zero-row crosswalk must not use transport.", call. = FALSE)
  })

  out <- gx_gage_to_pid(character(), client = setup$client)
  metadata <- attr(out, "gx_crosswalk")

  expect_s3_class(out, "gx_gage_crosswalk")
  expect_equal(nrow(out), 0L)
  expect_type(out$input_index, "integer")
  expect_type(out$gage_id, "character")
  expect_true(is.list(out$diagnostics))
  expect_identical(metadata$input_count, 0L)
  expect_identical(metadata$match_count, 0L)
  expect_true(is.na(metadata$retrieved_at))
  expect_length(setup$state$calls, 0L)
})

test_that("invalid provider identifiers fail before network access", {
  setup <- gx_cw_test_client(function(request, state) {
    stop("Invalid input must not use transport.", call. = FALSE)
  })

  invalid_bytes <- rawToChar(as.raw(0xff))
  Encoding(invalid_bytes) <- "bytes"
  for (value in list(832622, NA_character_, "", " ", "bad\nvalue", invalid_bytes)) {
    expect_error(
      gx_gage_to_pid(value, client = setup$client),
      class = "gx_error_crosswalk_input"
    )
  }
  withr::local_options(geoconnexr.crosswalk_max_inputs = 1L)
  expect_error(
    gx_gage_to_pid(c("one", "two"), client = setup$client),
    class = "gx_error_crosswalk_budget"
  )
  withr::local_options(geoconnexr.ref_max_query_value_bytes = 4L)
  expect_error(
    gx_gage_to_pid("12345", client = setup$client),
    class = "gx_error_crosswalk_budget"
  )
  expect_length(setup$state$calls, 0L)
})

test_that("duplicate and leading-zero provider identifiers are round-tripped", {
  item <- gx_cw_test_collection(list(gx_cw_test_feature(7L, "0007")))
  setup <- gx_cw_test_client(gx_cw_test_handler(list(`0007` = item)))

  out <- gx_gage_to_pid(c("0007", "0007"), client = setup$client)

  expect_identical(out$input_index, c(1L, 2L))
  expect_identical(out$requested_provider_id, c("0007", "0007"))
  expect_identical(out$provider_id, c("0007", "0007"))
  expect_identical(out$gage_id, c("7", "7"))
  expect_identical(attr(out, "gx_crosswalk")$unique_input_count, 1L)
  expect_equal(length(setup$state$calls), 2L)
})

test_that("not-found and ambiguous gages remain explicit and deterministic", {
  items <- list(
    B = gx_cw_test_collection(list(gx_cw_test_feature(20L, "B"))),
    A = gx_cw_test_collection(list(
      gx_cw_test_feature(12L, "A"),
      gx_cw_test_feature(11L, "A")
    ))
  )
  setup <- gx_cw_test_client(gx_cw_test_handler(items))

  out <- gx_gage_to_pid(c("B", "A", "Z"), client = setup$client)
  metadata <- attr(out, "gx_crosswalk")

  expect_identical(out$input_index, c(1L, 2L, 2L, 3L))
  expect_identical(out$status, c("matched", "ambiguous", "ambiguous", "not_found"))
  expect_identical(out$gage_id, c("20", "11", "12", NA_character_))
  expect_identical(out$match_index, c(1L, 1L, 2L, NA_integer_))
  expect_contains(out$diagnostics[[2]]$code, "multiple_matches")
  expect_identical(out$diagnostics[[4]]$code, "not_found")
  expect_identical(metadata$matched_input_count, 1L)
  expect_identical(metadata$ambiguous_input_count, 1L)
  expect_identical(metadata$not_found_input_count, 1L)
  expect_identical(metadata$match_count, 3L)
  expect_equal(length(setup$state$calls), 4L)

  reordered <- out[c(2L, 1L, 3L, 4L), , drop = FALSE]
  expect_error(
    geoconnexr:::gx_validate_gage_crosswalk(reordered, metadata),
    class = "gx_error_crosswalk_contract"
  )
  duplicate_identity <- out
  duplicate_identity$gage_id[[3]] <- duplicate_identity$gage_id[[2]]
  duplicate_identity$gage_uri[[3]] <- duplicate_identity$gage_uri[[2]]
  expect_error(
    geoconnexr:::gx_validate_gage_crosswalk(duplicate_identity, metadata),
    class = "gx_error_crosswalk_contract"
  )
  noncontiguous <- out
  noncontiguous$input_index <- noncontiguous$input_index + 1L
  expect_error(
    geoconnexr:::gx_validate_gage_crosswalk(noncontiguous, metadata),
    class = "gx_error_crosswalk_contract"
  )
})

test_that("gage crosswalk independently verifies filter and PID identities", {
  modes <- c("provider", "property_id", "uri", "duplicate")
  for (mode in modes) {
    features <- switch(
      mode,
      provider = list(gx_cw_test_feature(1L, "WRONG")),
      property_id = {
        value <- gx_cw_test_feature(1L, "P")
        value$properties$id <- 2L
        list(value)
      },
      uri = list(gx_cw_test_feature(
        1L,
        "P",
        uri = "https://geoconnex.us/ref/gages/2"
      )),
      duplicate = {
        value <- gx_cw_test_feature(1L, "P")
        list(value, value)
      }
    )
    setup <- gx_cw_test_client(gx_cw_test_handler(list(
      P = gx_cw_test_collection(features)
    )))
    error <- tryCatch(
      gx_gage_to_pid("P", client = setup$client),
      error = identity
    )
    expect_s3_class(error, "gx_error_crosswalk_identity")
    expect_equal(nrow(error$requests), 2L, info = mode)
  }
})

test_that("missing related identifiers are matched with diagnostics", {
  feature <- gx_cw_test_feature(1L, "P", mainstem_uri = NULL, comid = NULL)
  setup <- gx_cw_test_client(gx_cw_test_handler(list(
    P = gx_cw_test_collection(list(feature))
  )))

  out <- gx_gage_to_pid("P", client = setup$client)

  expect_identical(out$status, "matched")
  expect_true(is.na(out$mainstem_uri))
  expect_true(is.na(out$comid))
  expect_contains(out$diagnostics[[1]]$code, c(
    "missing_mainstem_uri", "missing_comid"
  ))
  expect_contains(attr(out, "gx_crosswalk")$diagnostics$code, c(
    "missing_mainstem_uri", "missing_comid"
  ))
})

test_that("invalid advertised related identifiers fail closed", {
  cases <- list(
    mainstem = gx_cw_test_feature(
      1L,
      "P",
      mainstem_uri = "https://evil.example/ref/mainstems/1"
    ),
    comid = gx_cw_test_feature(1L, "P", comid = 0)
  )
  for (name in names(cases)) {
    setup <- gx_cw_test_client(gx_cw_test_handler(list(
      P = gx_cw_test_collection(list(cases[[name]]))
    )))
    error <- tryCatch(
      gx_gage_to_pid("P", client = setup$client),
      error = identity
    )
    expect_s3_class(error, "gx_error_crosswalk_payload")
    expect_equal(nrow(error$requests), 2L, info = name)
  }
})

test_that("required gage queryables are validated before item retrieval", {
  schema <- list(
    type = "object",
    properties = list(
      geometry = list(format = "geometry-any", `x-ogc-role` = "primary-geometry"),
      id = list(type = "integer", `x-ogc-role` = "id"),
      provider_id = list(type = "string")
    )
  )
  setup <- gx_cw_test_client(function(request, state) {
    gx_cw_test_response(request, body = gx_cw_test_json(schema))
  })

  error <- tryCatch(
    gx_gage_to_pid("P", client = setup$client),
    error = identity
  )

  expect_s3_class(error, "gx_error_crosswalk_contract")
  expect_equal(nrow(error$requests), 1L)
  expect_equal(length(setup$state$calls), 1L)
})

test_that("incomplete reference results never become crosswalk rows", {
  feature <- gx_cw_test_feature(1L, "P")
  payload <- gx_cw_test_collection(list(feature), matched = 2L)
  setup <- gx_cw_test_client(gx_cw_test_handler(list(P = payload)))

  error <- tryCatch(
    gx_gage_to_pid("P", client = setup$client),
    error = identity
  )

  expect_s3_class(error, "gx_error_crosswalk_incomplete")
  expect_equal(nrow(error$requests), 2L)
})

test_that("reference diagnostics remain visible in crosswalk metadata", {
  payload <- gx_cw_test_collection(list(gx_cw_test_feature(1L, "P")))
  payload$numberReturned <- 99L
  setup <- gx_cw_test_client(gx_cw_test_handler(list(P = payload)))

  out <- gx_gage_to_pid("P", client = setup$client)
  diagnostics <- attr(out, "gx_crosswalk")$diagnostics

  expect_contains(diagnostics$code, "number_returned_mismatch")
  row <- match("number_returned_mismatch", diagnostics$code)
  expect_match(diagnostics$path[[row]], "^/queries/0")
})

test_that("aggregate crosswalk budgets include schema, queries, and expansion", {
  feature <- gx_cw_test_feature(1L, "P")
  payload <- gx_cw_test_collection(list(feature))

  setup_requests <- gx_cw_test_client(gx_cw_test_handler(list(P = payload)))
  error <- withr::with_options(
    list(geoconnexr.crosswalk_max_requests = 1L),
    tryCatch(
      gx_gage_to_pid("P", client = setup_requests$client),
      error = identity
    )
  )
  expect_s3_class(error, "gx_error_crosswalk_budget")
  expect_equal(nrow(error$requests), 1L)
  expect_equal(length(setup_requests$state$calls), 1L)

  setup_bytes <- gx_cw_test_client(gx_cw_test_handler(list(P = payload)))
  schema_bytes <- length(gx_cw_test_fixture("queryables-gages.min.json"))
  error <- withr::with_options(
    list(geoconnexr.crosswalk_total_bytes = schema_bytes + 10L),
    tryCatch(
      gx_gage_to_pid("P", client = setup_bytes$client),
      error = identity
    )
  )
  expect_s3_class(error, "gx_error_crosswalk_budget")
  expect_equal(nrow(error$requests), 2L)
  expect_equal(length(setup_bytes$state$calls), 2L)

  setup_rows <- gx_cw_test_client(gx_cw_test_handler(list(P = payload)))
  error <- withr::with_options(
    list(geoconnexr.crosswalk_max_rows = 1L),
    tryCatch(
      gx_gage_to_pid(c("P", "P"), client = setup_rows$client),
      error = identity
    )
  )
  expect_s3_class(error, "gx_error_crosswalk_budget")
  expect_equal(nrow(error$requests), 2L)

  setup_matches <- gx_cw_test_client(gx_cw_test_handler(list(
    P = gx_cw_test_collection(list(
      gx_cw_test_feature(1L, "P"),
      gx_cw_test_feature(2L, "P")
    ))
  )))
  error <- withr::with_options(
    list(geoconnexr.crosswalk_max_matches = 1L),
    tryCatch(
      gx_gage_to_pid("P", client = setup_matches$client),
      error = identity
    )
  )
  expect_s3_class(error, "gx_error_crosswalk_budget")
  expect_equal(nrow(error$requests), 2L)

  setup_redirect <- gx_cw_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_cw_test_response(
        request,
        body = gx_cw_test_fixture("queryables-gages.min.json")
      ))
    }
    if (!grepl("redirected=1", request$url, fixed = TRUE)) {
      return(gx_cw_test_response(
        request,
        status = 302L,
        headers = list(Location = paste0(request$url, "&redirected=1"))
      ))
    }
    stop("The redirect target must remain beyond the response budget.", call. = FALSE)
  })
  error <- withr::with_options(
    list(geoconnexr.crosswalk_max_requests = 2L),
    tryCatch(
      gx_gage_to_pid("P", client = setup_redirect$client),
      error = identity
    )
  )
  expect_s3_class(error, "gx_error_crosswalk_budget")
  expect_equal(nrow(error$requests), 2L)
  expect_equal(length(setup_redirect$state$calls), 2L)
  expect_match(conditionMessage(error), "request budget")

  setup_reference_bytes <- gx_cw_test_client(gx_cw_test_handler(list(P = payload)))
  error <- withr::with_options(
    list(
      geoconnexr.ref_total_bytes = 100L,
      geoconnexr.crosswalk_total_bytes = 1024L * 1024L
    ),
    tryCatch(
      gx_gage_to_pid("P", client = setup_reference_bytes$client),
      error = identity
    )
  )
  expect_s3_class(error, "gx_error_reference_budget")
  expect_false(inherits(error, "gx_error_crosswalk_budget"))
})

test_that("crosswalk request budgets count transient retry attempts", {
  queryable_calls <- 0L
  setup <- gx_cw_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      queryable_calls <<- queryable_calls + 1L
      if (queryable_calls == 1L) {
        return(gx_cw_test_response(
          request, status = 503L, body = "wait", content_type = "text/plain"
        ))
      }
      return(gx_cw_test_response(
        request,
        body = gx_cw_test_fixture("queryables-gages.min.json"),
        content_type = "application/schema+json"
      ))
    }
    stop("The feature request must not exceed the attempt budget.", call. = FALSE)
  }, retries = 1L)

  error <- withr::with_options(
    list(
      geoconnexr.crosswalk_max_requests = 2L,
      geoconnexr.retry_jitter = function(max_seconds) 0
    ),
    tryCatch(gx_gage_to_pid("P", client = setup$client), error = identity)
  )

  expect_s3_class(error, "gx_error_crosswalk_budget")
  expect_identical(error$requests$status, c(503L, 200L))
  expect_identical(error$requests$bytes[[1]], 4L)
  expect_identical(length(setup$state$calls), 2L)
})
