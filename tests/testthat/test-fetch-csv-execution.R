test_that("M7g executes one bounded direct-CSV response exactly", {
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  body <- charToRaw("station,value\n00123,4.5\n")
  plan <- csv_execution_test_plan(body = body)
  execution <- csv_execution_test_build(
    plan = plan,
    body = body,
    performer = csv_execution_test_performer(body = body, calls = calls),
    scope = csv_execution_test_scope("known-answer")
  )

  expect_identical(class(execution), "gx_csv_execution")
  expect_identical(names(execution), c(
    "contract_version", "parsed_response", "execution", "attempts", "metadata"
  ))
  expect_identical(execution$contract_version, "0.1.0")
  expect_s3_class(execution$parsed_response, "gx_csv_parsed_response")
  expect_identical(
    execution$parsed_response$validated_response$request_plan,
    plan
  )
  expect_identical(
    execution$parsed_response$validated_response$request_plan$intent_set$plan$requests,
    list()
  )
  expect_identical(execution$parsed_response$data$station, "00123")
  expect_identical(execution$parsed_response$data$value, "4.5")
  expect_equal(length(calls$requests), 1L)

  request <- calls$requests[[1L]]
  selected <- plan$request_plans[1L, , drop = FALSE]
  expect_identical(request$method, "GET")
  expect_identical(
    request$url,
    gx_canonical_url(csv_response_validation_test_url(plan))
  )
  expect_identical(
    request$headers[["accept"]],
    "text/csv, application/csv;q=0.9"
  )
  expect_identical(request$headers[["accept-encoding"]], "identity")
  expect_identical(request$body, raw())
  expect_identical(request$retries, 0L)
  expect_identical(request$timeout, 15)
  expect_identical(request$max_bytes, as.integer(min(
    selected$response_byte_limit,
    selected$max_encoded_bytes,
    selected$max_decoded_bytes
  )))
  expect_identical(request$resolved_host, "data.example.org")
  expect_identical(request$resolved_ip, "8.8.8.8")

  expect_identical(nrow(execution$attempts), 1L)
  expect_identical(execution$attempts$attempt_number, 1L)
  expect_identical(execution$attempts$status, 200L)
  expect_identical(execution$attempts$outcome, "response")
  expect_identical(execution$attempts$encoded_bytes, as.double(length(body)))
  expect_identical(execution$attempts$decoded_bytes, as.double(length(body)))
  expect_match(execution$attempts$canonical_url_redacted, "\\?\\[redacted\\]$")
  expect_false(grepl("fixture-token", execution$attempts$canonical_url_redacted))
  expect_true(execution$metadata$transport_authorized)
  expect_true(execution$metadata$provider_response_observed)
  expect_true(execution$metadata$budgets_consumed)
  expect_true(execution$metadata$attempt_ledger_bound)
  expect_identical(execution$metadata$observation_origin, "provider_transport")
  expect_identical(
    gx_csv_execution_validate_impl(execution),
    invisible(execution)
  )
})

test_that("M7g preserves M7f semantics while upgrading outer provenance", {
  execution <- csv_execution_test_build()
  nested <- execution$parsed_response$metadata

  expect_false(nested$provider_response_observed)
  expect_false(nested$transport_authorized)
  expect_false(nested$budgets_consumed)
  expect_identical(nested$observation_origin, "caller_supplied")
  expect_true(execution$metadata$provider_response_observed)
  expect_true(execution$metadata$transport_authorized)
  expect_true(execution$metadata$budgets_consumed)
  expect_identical(execution$metadata$observation_origin, "provider_transport")

  removed <- c(
    "attempt_identity_unbound", "attempt_ledger_unbound",
    "provider_transport_unauthorized", "response_origin_unbound",
    "timeout_policy_unbound", "transport_adapter_unimplemented"
  )
  expect_false(any(removed %in% execution$metadata$non_replayable_reasons))
  expect_true(all(c(
    "arbitrary_provider_client_unimplemented",
    "handler_implementations_planned",
    "non_csv_request_plans_absent",
    "runtime_package_preflight_required",
    "serialization_unbound"
  ) %in% execution$metadata$non_replayable_reasons))
  expect_false(execution$metadata$replayable)
  expect_false(execution$metadata$execution_ready)
})

test_that("M7g binds scope, policy, target, attempt, and response identities", {
  first <- csv_execution_test_build(scope = csv_execution_test_scope("first"))
  second <- csv_execution_test_build(scope = csv_execution_test_scope("second"))

  expect_false(identical(
    first$execution$execution_id,
    second$execution$execution_id
  ))
  expect_false(identical(
    first$attempts$attempt_id,
    second$attempts$attempt_id
  ))
  expect_identical(
    first$attempts$body_sha256,
    first$parsed_response$validated_response$validation$body_sha256
  )
  expect_identical(
    first$attempts$logical_request_id,
    first$execution$logical_request_id
  )
  expect_identical(
    first$attempts$reservation_id,
    first$execution$reservation_id
  )
})

test_that("M7g rejects invalid execution inputs before transport", {
  plan <- csv_execution_test_plan()
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  performer <- csv_execution_test_performer(calls = calls)

  cases <- list(
    list(scope = NULL, timeout = 15, interval = 0),
    list(scope = "ABC", timeout = 15, interval = 0),
    list(scope = csv_execution_test_scope(), timeout = NULL, interval = 0),
    list(scope = csv_execution_test_scope(), timeout = 0, interval = 0),
    list(scope = csv_execution_test_scope(), timeout = 601, interval = 0),
    list(scope = csv_execution_test_scope(), timeout = 15, interval = NULL),
    list(scope = csv_execution_test_scope(), timeout = 15, interval = -1)
  )
  for (case in cases) {
    csv_execution_test_options(performer)
    expect_error(
      gx_csv_execution_impl(
        plan,
        csv_execution_test_logical_id(plan),
        max_fields = 1000L,
        timeout = case$timeout,
        min_interval = case$interval,
        execution_scope_id = case$scope
      ),
      class = "gx_error_csv_execution"
    )
  }
  csv_execution_test_options(performer)
  expect_error(
    gx_csv_execution_impl(
      plan,
      paste(rep("0", 64L), collapse = ""),
      max_fields = 1000L,
      timeout = 15,
      min_interval = 0,
      execution_scope_id = csv_execution_test_scope()
    ),
    class = "gx_error_csv_execution_input"
  )
  expect_length(calls$requests, 0L)
})

test_that("M7g performs one attempt and never retries transport failures", {
  calls <- 0L
  performer <- function(request) {
    calls <<- calls + 1L
    stop("sensitive transport detail", call. = FALSE)
  }
  csv_execution_test_options(performer)
  plan <- csv_execution_test_plan()
  condition <- csv_execution_test_error(gx_csv_execution_impl(
    plan,
    csv_execution_test_logical_id(plan),
    max_fields = 1000L,
    timeout = 15,
    min_interval = 0,
    execution_scope_id = csv_execution_test_scope()
  ))

  expect_s3_class(condition, "gx_error_csv_execution_transport")
  expect_identical(calls, 1L)
  expect_false(grepl("sensitive transport detail", conditionMessage(condition)))
  expect_s3_class(condition$attempts, "data.frame")
  expect_identical(nrow(condition$attempts), 1L)
  expect_false(any(grepl("fixture-token", unlist(condition$attempts), fixed = TRUE)))
})

test_that("M7g fails closed on unsafe DNS and response policy", {
  plan <- csv_execution_test_plan()
  performer_calls <- 0L
  performer <- function(request) {
    performer_calls <<- performer_calls + 1L
    csv_execution_test_performer()(request)
  }
  csv_execution_test_options(performer, dns = "127.0.0.1")
  unsafe <- csv_execution_test_error(gx_csv_execution_impl(
    plan,
    csv_execution_test_logical_id(plan),
    max_fields = 1000L,
    timeout = 15,
    min_interval = 0,
    execution_scope_id = csv_execution_test_scope()
  ))
  expect_s3_class(unsafe, "gx_error_csv_execution_transport")
  expect_identical(performer_calls, 0L)
  expect_identical(nrow(unsafe$attempts), 1L)

  csv_execution_test_options(csv_execution_test_performer(
    content_type = "text/html"
  ))
  media <- csv_execution_test_error(gx_csv_execution_impl(
    plan,
    csv_execution_test_logical_id(plan),
    max_fields = 1000L,
    timeout = 15,
    min_interval = 0,
    execution_scope_id = csv_execution_test_scope("media")
  ))
  expect_s3_class(media, "gx_error_csv_execution_transport")
  expect_identical(nrow(media$attempts), 1L)
})

test_that("M7g admits only the exact M7d response envelope", {
  plan <- csv_execution_test_plan()
  cases <- list(
    csv_execution_test_performer(status = 503L),
    csv_execution_test_performer(content_encoding = "gzip"),
    csv_execution_test_performer(content_length = "1"),
    csv_execution_test_performer(
      final_url = "https://data.example.org/changed.csv"
    )
  )
  for (index in seq_along(cases)) {
    csv_execution_test_options(cases[[index]])
    condition <- csv_execution_test_error(gx_csv_execution_impl(
      plan,
      csv_execution_test_logical_id(plan),
      max_fields = 1000L,
      timeout = 15,
      min_interval = 0,
      execution_scope_id = csv_execution_test_scope(paste0("envelope-", index))
    ))
    expect_s3_class(condition, "gx_error_csv_execution_transport")
    expect_identical(nrow(condition$attempts), 1L)
    expect_false(any(grepl(
      "fixture-token", unlist(condition$attempts), fixed = TRUE
    )))
  }
})

test_that("M7g reports post-transport CSV parse failure honestly", {
  body <- charToRaw("station,value\n00123,\"unterminated\n")
  plan <- csv_execution_test_plan(body = body)
  csv_execution_test_options(csv_execution_test_performer(body = body))
  condition <- csv_execution_test_error(gx_csv_execution_impl(
    plan,
    csv_execution_test_logical_id(plan),
    max_fields = 1000L,
    timeout = 15,
    min_interval = 0,
    execution_scope_id = csv_execution_test_scope("parse-failure")
  ))

  expect_s3_class(condition, "gx_error_csv_execution_parse")
  expect_identical(nrow(condition$attempts), 1L)
  expect_identical(condition$attempts$status, 200L)
  expect_false(any(grepl("unterminated", unlist(condition$attempts), fixed = TRUE)))
})

test_that("M7g enforces the minimum selected response byte ceiling", {
  body <- charToRaw("station,value\n00123,4.5\n")
  plan <- csv_execution_test_plan(body = body, max_response_bytes = length(body))
  oversized <- c(body, as.raw(0x0a))
  csv_execution_test_options(csv_execution_test_performer(
    body = oversized,
    content_length = as.character(length(oversized))
  ))
  condition <- csv_execution_test_error(gx_csv_execution_impl(
    plan,
    csv_execution_test_logical_id(plan),
    max_fields = 1000L,
    timeout = 15,
    min_interval = 0,
    execution_scope_id = csv_execution_test_scope("oversized")
  ))

  expect_s3_class(condition, "gx_error_csv_execution_transport")
  expect_identical(nrow(condition$attempts), 1L)
  expect_true(condition$attempts$charged_bytes[[1L]] >= length(body))
})

test_that("M7g whole-object validation rejects forged owned and nested facts", {
  execution <- csv_execution_test_build()
  mutations <- list(
    scope = function(x) {
      x$execution$execution_scope_id <- paste(rep("0", 64L), collapse = "")
      x
    },
    timeout = function(x) {
      x$execution$timeout_seconds <- 16
      x
    },
    attempt_id = function(x) {
      x$attempts$attempt_id[[1L]] <- paste(rep("0", 64L), collapse = "")
      x
    },
    resolved_ip = function(x) {
      x$attempts$resolved_ip[[1L]] <- "127.0.0.1"
      x
    },
    bytes = function(x) {
      x$attempts$encoded_bytes[[1L]] <- x$attempts$encoded_bytes[[1L]] + 1
      x
    },
    body = function(x) {
      x$parsed_response$validated_response$body[[1L]] <- as.raw(0x00)
      x
    },
    authority = function(x) {
      x$metadata$provider_response_observed <- FALSE
      x
    },
    blockers = function(x) {
      x$metadata$non_replayable_reasons <- character()
      x
    }
  )
  for (mutation in mutations) {
    expect_error(
      gx_csv_execution_validate_impl(mutation(csv_execution_test_clone(execution))),
      class = "gx_error_csv_execution"
    )
  }
})

test_that("M7g does not access cache or expose sensitive response data", {
  execution <- testthat::with_mocked_bindings(
    csv_execution_test_build(),
    gx_cache_backend = function(...) {
      stop("M7g accessed the cache.", call. = FALSE)
    }
  )
  output <- capture.output(print(execution), type = "message")
  expect_false(any(grepl("fixture-token", output, fixed = TRUE)))
  expect_false(any(grepl("00123", output, fixed = TRUE)))
  expect_false(any(grepl("4.5", output, fixed = TRUE)))
  expect_true(any(grepl("Provider response: observed", output, fixed = TRUE)))
})

test_that("the M7g execution contract remains internal", {
  internal <- c(
    "gx_csv_execution_impl", "gx_csv_execution_validate_impl",
    "gx_csv_execution_attempt_id_impl"
  )
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(internal %in% exports))
  expect_false("gx_csv_execution" %in% exports)
  expect_true("gx_fetch" %in% exports)
})
