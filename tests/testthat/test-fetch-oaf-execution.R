test_that("M7i plans one bounded OGC API Features request without host activity", {
  touched <- new.env(parent = emptyenv())
  touched$count <- 0L
  forbidden <- function(...) {
    touched$count <- touched$count + 1L
    stop("planning touched host state", call. = FALSE)
  }
  withr::local_options(list(
    geoconnexr.performer = forbidden,
    geoconnexr.dns_resolver = forbidden,
    geoconnexr.clock = forbidden,
    geoconnexr.throttle_clock = forbidden,
    geoconnexr.throttle_sleep = forbidden
  ))
  source <- oaf_test_m7d_plan()
  first <- oaf_test_request_plan(source)
  second <- oaf_test_request_plan(source)

  expect_identical(touched$count, 0L)
  expect_identical(first, second)
  expect_identical(class(first), "gx_oaf_request_plan")
  expect_identical(first$request_plan, source)
  expect_identical(first$request$collection_id, "gages")
  expect_identical(first$request$request_status, "oaf_request_planned")
  expect_identical(first$request$max_physical_attempts, 1L)
  expect_identical(
    first$request$canonical_url_redacted,
    "https://reference.geoconnex.us/collections/gages/items?[redacted]"
  )
  expect_false(first$metadata$transport_authorized)
  expect_false(first$metadata$budgets_consumed)
  expect_identical(
    gx_oaf_request_plan_validate_impl(first), invisible(first)
  )
  expect_snapshot(str(list(policy = first$policy, request = first$request)))
  expect_snapshot(print(first))
})

test_that("M7i checks the native symbol immediately before one bounded invocation", {
  events <- new.env(parent = emptyenv())
  events$values <- character()
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  result <- oaf_test_execute(
    performer = oaf_test_performer(calls = calls, events = events),
    symbol_resolver = oaf_test_resolver(events)
  )

  expect_identical(events$values, c("resolve", "invoke", "transport"))
  expect_length(calls$requests, 1L)
  expect_identical(
    calls$requests[[1L]]$url,
    paste0(
      "https://reference.geoconnex.us/collections/gages/items?",
      "f=json&limit=2"
    )
  )
  expect_identical(calls$requests[[1L]]$retries, 0L)
  expect_identical(calls$requests[[1L]]$max_bytes, 20000L)
  expect_false(any(grepl("offset=2", calls$requests[[1L]]$url, fixed = TRUE)))
  expect_identical(class(result), "gx_oaf_execution")
  expect_s3_class(result$result, "gx_oaf_features")
  expect_s3_class(result$result, "sf")
  expect_identical(nrow(result$result), 2L)
  expect_identical(result$result$feature_id, c("gage-001", "gage-002"))
  expect_identical(result$result$provider_id, c("00123", "00456"))
  expect_true(result$execution$truncated)
  expect_identical(result$execution$stop_reason, "single_page_budget")
  expect_identical(result$execution$feature_count, 2L)
  expect_identical(result$metadata$counts$physical_attempts, 1L)
  expect_true(result$metadata$runtime_symbol_checked)
  expect_identical(
    gx_oaf_execution_validate_impl(result), invisible(result)
  )
})

test_that("M7i missing-symbol failure occurs before DNS or transport", {
  touched <- new.env(parent = emptyenv())
  touched$count <- 0L
  forbidden <- function(...) {
    touched$count <- touched$count + 1L
    stop("capability failure touched transport", call. = FALSE)
  }
  oaf_test_options(forbidden)
  withr::local_options(list(geoconnexr.dns_resolver = forbidden))
  error <- oaf_test_error(gx_oaf_execution_impl(
    oaf_test_request_plan(),
    timeout = 15,
    min_interval = 0,
    execution_scope_id = oaf_test_scope("missing-symbol"),
    symbol_resolver = oaf_test_resolver(available = FALSE)
  ))

  expect_s3_class(error, "gx_error_oaf_execution_capability")
  expect_identical(touched$count, 0L)
})

test_that("M7i rejects poisoned redirects and over-limit feature pages", {
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  redirected <- oaf_test_error(oaf_test_execute(
    performer = oaf_test_performer(
      final_url = "https://evil.example.org/collections/gages/items",
      calls = calls
    ),
    scope = oaf_test_scope("redirect")
  ))
  expect_s3_class(redirected, "gx_error_oaf_execution_transport")
  expect_length(calls$requests, 1L)
  expect_identical(nrow(redirected$attempts), 1L)
  expect_false(any(grepl(
    "evil.example.org",
    unlist(redirected, recursive = TRUE, use.names = FALSE),
    fixed = TRUE
  )))

  calls$requests <- list()
  overflow <- oaf_test_error(oaf_test_execute(
    performer = oaf_test_performer(
      body = oaf_test_body("too-many-features.geojson"), calls = calls
    ),
    scope = oaf_test_scope("overflow")
  ))
  expect_s3_class(overflow, "gx_error_oaf_execution_parse")
  expect_length(calls$requests, 1L)
  expect_identical(nrow(overflow$attempts), 1L)
})

test_that("M7i planning rejects ambiguous provider URLs", {
  catalog <- csv_intents_test_fixture_catalog()
  position <- which(catalog$datasets$handler_id == "ogc_api_features")
  catalog <- csv_request_plan_test_replace_distribution_url(
    catalog,
    catalog$datasets$distribution_id[[position]],
    "https://reference.geoconnex.us/collections/gages/items?token=secret"
  )
  plan <- csv_request_plan_test_build(
    csv_request_plan_test_intent_set(
      catalog = catalog,
      max_encoded_bytes = 100000,
      max_decoded_bytes = 100000
    ),
    max_response_bytes = 20000
  )
  error <- oaf_test_error(gx_oaf_request_plan_impl(
    plan, oaf_test_distribution_id(plan), limit = 2L
  ))
  expect_s3_class(error, "gx_error_oaf_plan_url")
  expect_false(grepl("secret", conditionMessage(error), fixed = TRUE))
})

test_that("M7i whole-object validation fails closed on forged facts", {
  value <- oaf_test_execute()
  mutations <- list(
    body = function(x) {
      x$response_body[[1L]] <- as.raw(bitwXor(as.integer(x$response_body[[1L]]), 1L))
      x
    },
    result = function(x) {
      x$result$feature_id[[1L]] <- "forged"
      x
    },
    implementation = function(x) {
      x$implementation$resolution_status <- "unchecked"
      x
    },
    attempt = function(x) {
      x$attempts$encoded_bytes[[1L]] <- x$attempts$encoded_bytes[[1L]] + 1
      x
    },
    execution = function(x) {
      x$execution$truncated <- FALSE
      x
    },
    metadata = function(x) {
      x$metadata$runtime_symbol_checked <- FALSE
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](oaf_test_clone(value))
    expect_error(
      gx_oaf_execution_validate_impl(forged),
      class = "gx_error_oaf",
      info = name
    )
  }
})

test_that("the M7i OGC handler contract remains internal", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_oaf_request_plan_impl", "gx_oaf_execution_impl", "gx_handler_oaf"
  ) %in% exports))
})
