test_that("M7k plans one bounded WQP request without host activity", {
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
  first <- wqp_test_request_plan(source)
  second <- wqp_test_request_plan(source)

  expect_identical(touched$count, 0L)
  expect_identical(first, second)
  expect_identical(class(first), "gx_wqp_request_plan")
  expect_identical(first$request_plan, source)
  expect_identical(first$request$service, "Result")
  expect_identical(first$request$profile, "narrowResult")
  expect_identical(first$request$site_id, "USGS-01234567")
  expect_identical(first$request$characteristic_status, "not_supplied")
  expect_identical(first$request$time_start, "2025-06-01T00:00:00Z")
  expect_identical(first$request$time_end, "2025-06-30T23:59:59Z")
  expect_identical(first$request$max_physical_attempts, 1L)
  expect_match(first$request$canonical_url_redacted, "\\?\\[redacted\\]$")
  expect_false(first$metadata$transport_authorized)
  expect_false(first$metadata$budgets_consumed)
  expect_identical(
    gx_wqp_request_plan_validate_impl(first), invisible(first)
  )
  expect_snapshot(str(list(policy = first$policy, request = first$request)))
  expect_snapshot(print(first))
})

test_that("M7k resolves its parser before one bounded provider request", {
  events <- new.env(parent = emptyenv())
  events$values <- character()
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  result <- wqp_test_execute(
    performer = wqp_test_performer(calls = calls, events = events),
    symbol_resolver = wqp_test_resolver(events = events)
  )

  expect_identical(events$values, c("resolve", "transport", "parse"))
  expect_length(calls$requests, 1L)
  request <- calls$requests[[1L]]
  expect_identical(request$retries, 0L)
  expect_identical(request$max_bytes, 20000L)
  expect_match(request$url, "siteid=USGS-01234567", fixed = TRUE)
  expect_match(request$url, "dataProfile=narrowResult", fixed = TRUE)
  expect_match(request$url, "startDateLo=06-01-2025", fixed = TRUE)
  expect_match(request$url, "startDateHi=06-30-2025", fixed = TRUE)
  expect_false(grepl("characteristicName=", request$url, fixed = TRUE))
  expect_identical(class(result), "gx_wqp_execution")
  expect_identical(nrow(result$data), 2L)
  expect_identical(ncol(result$data), 5L)
  expect_true(all(vapply(result$data, is.character, logical(1))))
  expect_identical(
    names(result$data)[[5L]], "ResultMeasure.MeasureUnitCode"
  )
  expect_identical(result$data$ResultMeasureValue[[2L]], "NA")
  expect_identical(result$metadata$counts$physical_attempts, 1L)
  expect_true(result$metadata$external_parser_matched)
  expect_identical(
    gx_wqp_execution_validate_impl(result), invisible(result)
  )
})

test_that("M7k missing parser failure occurs before DNS or transport", {
  touched <- new.env(parent = emptyenv())
  touched$count <- 0L
  forbidden <- function(...) {
    touched$count <- touched$count + 1L
    stop("capability failure touched transport", call. = FALSE)
  }
  wqp_test_options(forbidden)
  withr::local_options(list(geoconnexr.dns_resolver = forbidden))
  error <- wqp_test_error(gx_wqp_execution_impl(
    wqp_test_request_plan(),
    timeout = 15,
    min_interval = 0,
    execution_scope_id = wqp_test_scope("missing-symbol"),
    symbol_resolver = wqp_test_resolver(available = FALSE)
  ))

  expect_s3_class(error, "gx_error_wqp_execution_capability")
  expect_identical(touched$count, 0L)
})

test_that("M7k rejects redirects and parser disagreement after one attempt", {
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  redirected <- wqp_test_error(wqp_test_execute(
    performer = wqp_test_performer(
      final_url = "https://evil.example.org/data/Result/search", calls = calls
    ),
    scope = wqp_test_scope("redirect")
  ))
  expect_s3_class(redirected, "gx_error_wqp_execution_transport")
  expect_length(calls$requests, 1L)
  expect_identical(nrow(redirected$attempts), 1L)
  expect_false(any(grepl(
    "evil.example.org",
    unlist(redirected, recursive = TRUE, use.names = FALSE),
    fixed = TRUE
  )))

  calls$requests <- list()
  mismatch <- wqp_test_error(wqp_test_execute(
    performer = wqp_test_performer(calls = calls),
    symbol_resolver = wqp_test_resolver(mismatch = TRUE),
    scope = wqp_test_scope("parser-mismatch")
  ))
  expect_s3_class(mismatch, "gx_error_wqp_execution_parse")
  expect_length(calls$requests, 1L)
  expect_identical(nrow(mismatch$attempts), 1L)
  expect_gt(mismatch$attempts$charged_bytes[[1L]], 0)
})

test_that("M7k planning rejects ambiguous queries and records one characteristic", {
  catalog <- csv_intents_test_fixture_catalog()
  position <- which(catalog$datasets$handler_id == "wqp")
  distribution_id <- catalog$datasets$distribution_id[[position]]
  catalog <- csv_request_plan_test_replace_distribution_url(
    catalog,
    distribution_id,
    paste0(
      "https://www.waterqualitydata.us/data/Result/search?",
      "siteid=USGS-01234567&characteristicName=Dissolved%20oxygen&",
      "mimeType=csv"
    )
  )
  plan <- csv_request_plan_test_build(
    csv_request_plan_test_intent_set(
      catalog = catalog,
      max_encoded_bytes = 100000,
      max_decoded_bytes = 100000
    ),
    max_response_bytes = 20000
  )
  planned <- gx_wqp_request_plan_impl(
    plan, wqp_test_distribution_id(plan), max_fields = 1000L
  )
  expect_identical(planned$request$characteristic_name, "Dissolved oxygen")
  expect_identical(
    planned$request$characteristic_status, "source_url_filter"
  )

  poisoned_catalog <- csv_request_plan_test_replace_distribution_url(
    csv_intents_test_fixture_catalog(),
    wqp_test_distribution_id(plan),
    paste0(
      "https://www.waterqualitydata.us/data/Result/search?",
      "siteid=USGS-01234567&mimeType=csv&token=secret"
    )
  )
  poisoned <- csv_request_plan_test_build(
    csv_request_plan_test_intent_set(
      catalog = poisoned_catalog,
      max_encoded_bytes = 100000,
      max_decoded_bytes = 100000
    ),
    max_response_bytes = 20000
  )
  error <- wqp_test_error(gx_wqp_request_plan_impl(
    poisoned, wqp_test_distribution_id(poisoned), max_fields = 1000L
  ))
  expect_s3_class(error, "gx_error_wqp_plan_url")
  expect_false(grepl("secret", conditionMessage(error), fixed = TRUE))
})

test_that("M7k whole-object validation fails closed on forged facts", {
  value <- wqp_test_execute()
  mutations <- list(
    body = function(x) {
      x$response_body[[1L]] <- as.raw(bitwXor(
        as.integer(x$response_body[[1L]]), 1L
      ))
      x
    },
    data = function(x) {
      x$data[[1L]][[1L]] <- "forged"
      x
    },
    schema = function(x) {
      x$schema$column_name[[1L]] <- "forged"
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
      x$execution$row_count <- x$execution$row_count + 1L
      x
    },
    metadata = function(x) {
      x$metadata$external_parser_matched <- FALSE
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](wqp_test_clone(value))
    expect_error(
      gx_wqp_execution_validate_impl(forged),
      class = "gx_error_wqp",
      info = name
    )
  }
})

test_that("M7k accepts the installed official parser without provider work", {
  skip_if_not_installed("dataRetrieval")
  plan <- wqp_test_request_plan()
  result <- gx_wqp_result_impl(
    wqp_test_body(), plan, parser = dataRetrieval::importWQP
  )
  expect_identical(result$parse$row_count, 2L)
  expect_identical(result$parse$column_count, 5L)
})

test_that("the M7k WQP handler contract remains internal", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_wqp_request_plan_impl", "gx_wqp_execution_impl", "gx_handler_wqp"
  ) %in% exports))
})
