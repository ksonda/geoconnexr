test_that("M7h dry-run reconciles every distribution without host activity", {
  touched <- new.env(parent = emptyenv())
  touched$count <- 0L
  forbidden <- function(...) {
    touched$count <- touched$count + 1L
    stop("dry-run host activity", call. = FALSE)
  }
  withr::local_options(list(
    geoconnexr.performer = forbidden,
    geoconnexr.dns_resolver = forbidden,
    geoconnexr.clock = forbidden,
    geoconnexr.throttle_clock = forbidden,
    geoconnexr.throttle_sleep = forbidden
  ))
  plan <- csv_execution_test_plan()
  scope <- csv_orchestration_test_scope("deterministic")
  first <- csv_orchestration_test_build(
    plan = plan, dry_run = TRUE, scope = scope
  )
  second <- csv_orchestration_test_build(
    plan = plan, dry_run = TRUE, scope = scope
  )

  expect_identical(class(first), "gx_csv_orchestration")
  expect_identical(names(first), c(
    "contract_version", "request_plan", "policy", "orchestration", "results",
    "status", "metadata"
  ))
  expect_identical(first$contract_version, "0.1.0")
  expect_identical(first$request_plan, plan)
  expect_identical(first$results, list())
  expect_identical(nrow(first$status), nrow(plan$coverage))
  expect_identical(first$status$orchestration_status, c(
    rep("dry_run_planned", 3L), rep("handler_unimplemented", 2L),
    rep("not_selected", 3L)
  ))
  expect_false(any(first$status$attempted))
  expect_false(any(first$status$succeeded))
  expect_true(all(first$status$physical_attempts == 0L))
  expect_identical(touched$count, 0L)
  expect_identical(first, second)
  expect_identical(
    first$orchestration$orchestration_scope_id,
    "ad2644ba28bbd3545aec8a11fea38b25b198f9a942b39ba080d69730664c646f"
  )
  expect_identical(
    first$orchestration$orchestration_id,
    "f9440007dd57134dfe0760c636a791c6def4eb746f9a12667fbc5deb69aeec9f"
  )
  expect_identical(
    gx_csv_orchestration_validate_impl(first), invisible(first)
  )
  expect_false(first$metadata$host_specific)
  expect_false(first$metadata$transport_authorized)
  expect_false(first$metadata$budgets_consumed)
  expect_identical(first$metadata$observation_origin, "dry_run")
  expect_true("dry_run_no_transport" %in%
    first$metadata$non_replayable_reasons)
})

test_that("M7h executes admitted CSV requests in deterministic global order", {
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  plan <- csv_execution_test_plan()
  result <- csv_orchestration_test_build(
    plan = plan,
    performer = csv_orchestration_test_performer(calls = calls),
    scope = csv_orchestration_test_scope("all-success")
  )

  expect_length(calls$requests, 3L)
  expected_urls <- vapply(seq_len(nrow(plan$request_plans)), function(i) {
    gx_csv_validated_response_target_impl(plan, i)$url
  }, character(1), USE.NAMES = FALSE)
  expect_identical(
    vapply(calls$requests, `[[`, character(1), "url"),
    expected_urls
  )
  expect_length(result$results, 3L)
  expect_true(all(vapply(
    result$results,
    function(value) identical(class(value), "gx_csv_orchestration_result"),
    logical(1)
  )))
  expect_identical(names(result$results[[1L]]), c(
    "result_id", "execution", "attempt", "validation", "parse_policy",
    "schema", "data", "parse"
  ))
  expect_false("request_plan" %in% names(result$results[[1L]]))
  expect_false(any(vapply(
    unclass(result$results[[1L]]), is.raw, logical(1)
  )))
  expect_identical(
    result$status$orchestration_status[1:3],
    rep("provider_response_validated_and_parsed", 3L)
  )
  expect_identical(result$status$result_index[1:3], 1:3)
  expect_true(all(result$status$attempted[1:3]))
  expect_true(all(result$status$succeeded[1:3]))
  expect_identical(result$metadata$counts$physical_attempts, 3L)
  expect_identical(result$metadata$counts$successful_requests, 3L)
  expect_identical(result$metadata$counts$results, 3L)
  expect_true(result$metadata$results_compacted)
  expect_true(result$metadata$status_reconciled)
  expect_identical(
    result$orchestration$orchestration_status, "execution_complete"
  )
  expect_identical(
    result$metadata$non_replayable_reasons,
    c(
      "arbitrary_provider_client_unimplemented",
      "handler_implementations_planned",
      "non_csv_request_plans_absent",
      "runtime_package_preflight_required",
      "serialization_unbound"
    )
  )
  expect_identical(
    gx_csv_orchestration_validate_impl(result), invisible(result)
  )
})

test_that("M7h isolates one request failure and continues unrelated work", {
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  result <- csv_orchestration_test_build(
    performer = csv_orchestration_test_performer(
      fail_on = 2L, calls = calls
    ),
    scope = csv_orchestration_test_scope("continue")
  )

  expect_length(calls$requests, 3L)
  expect_identical(result$status$orchestration_status[1:3], c(
    "provider_response_validated_and_parsed",
    "transport_failed",
    "provider_response_validated_and_parsed"
  ))
  expect_identical(result$status$succeeded[1:3], c(TRUE, FALSE, TRUE))
  expect_identical(result$status$result_index[1:3], c(1L, NA_integer_, 2L))
  expect_identical(
    result$status$error_code[1:3],
    c(NA_character_, "csv_transport_failed", NA_character_)
  )
  expect_length(result$results, 2L)
  expect_identical(result$metadata$counts$attempted_requests, 3L)
  expect_identical(result$metadata$counts$successful_requests, 2L)
  expect_identical(result$metadata$counts$failed_requests, 1L)
  expect_identical(
    result$orchestration$orchestration_status,
    "execution_complete_with_failures"
  )
  expect_true("csv_execution_failures_present" %in%
    result$metadata$non_replayable_reasons)
  expect_false(any(grepl(
    "sensitive orchestration transport detail",
    unlist(result, recursive = TRUE, use.names = FALSE),
    fixed = TRUE
  )))
  expect_identical(
    gx_csv_orchestration_validate_impl(result), invisible(result)
  )
})

test_that("M7h records parse failure and continues later requests", {
  result <- csv_orchestration_test_build(
    performer = csv_orchestration_test_performer(parse_fail_on = 2L),
    scope = csv_orchestration_test_scope("parse-failure")
  )
  expect_identical(result$status$orchestration_status[1:3], c(
    "provider_response_validated_and_parsed",
    "parse_failed",
    "provider_response_validated_and_parsed"
  ))
  expect_identical(result$status$error_code[[2L]], "csv_parse_failed")
  expect_identical(result$status$physical_attempts[1:3], c(1L, 1L, 1L))
  expect_length(result$results, 2L)
})

test_that("M7h admission applies count and aggregate reservation ceilings", {
  plan <- csv_execution_test_plan(max_response_bytes = 100L)
  count_limited <- csv_orchestration_test_build(
    plan = plan, dry_run = TRUE, max_executions = 1L,
    max_total_bytes = 1000
  )
  byte_limited <- csv_orchestration_test_build(
    plan = plan, dry_run = TRUE, max_executions = 3L,
    max_total_bytes = 150
  )

  expect_identical(
    count_limited$status$orchestration_status[1:3],
    c("dry_run_planned", "batch_limit_deferred", "batch_limit_deferred")
  )
  expect_identical(
    byte_limited$status$orchestration_status[1:3],
    c("dry_run_planned", "batch_limit_deferred", "batch_limit_deferred")
  )
  expect_identical(
    byte_limited$metadata$counts$batch_limit_deferred, 2L
  )
  expect_true("csv_batch_limits_deferred" %in%
    byte_limited$metadata$non_replayable_reasons)
})

test_that("M7h accepts an empty plan without touching host state", {
  plan <- csv_request_plan_test_build(
    intent_set = csv_request_plan_test_empty_intent_set(),
    max_response_bytes = 100L,
    max_rows = 10L,
    max_columns = 5L
  )
  touched <- 0L
  performer <- function(request) {
    touched <<- touched + 1L
    stop("must not execute", call. = FALSE)
  }
  csv_execution_test_options(performer)
  result <- csv_orchestration_test_build(
    plan = plan, dry_run = FALSE, performer = performer,
    max_executions = 1L, max_total_bytes = 100
  )
  expect_identical(touched, 0L)
  expect_identical(nrow(result$status), 0L)
  expect_identical(result$results, list())
  expect_identical(result$metadata$counts$distributions, 0L)
  expect_identical(
    result$orchestration$orchestration_status, "execution_complete"
  )
})

test_that("M7h rejects invalid policy before transport", {
  plan <- csv_execution_test_plan()
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  performer <- csv_orchestration_test_performer(calls = calls)
  cases <- list(
    list(dry = NULL, executions = 3L, bytes = 1000, fields = 1000L,
         timeout = 15, interval = 0, scope = csv_orchestration_test_scope()),
    list(dry = FALSE, executions = 0L, bytes = 1000, fields = 1000L,
         timeout = 15, interval = 0, scope = csv_orchestration_test_scope()),
    list(dry = FALSE, executions = 33L, bytes = 1000, fields = 1000L,
         timeout = 15, interval = 0, scope = csv_orchestration_test_scope()),
    list(dry = FALSE, executions = 3L, bytes = 0, fields = 1000L,
         timeout = 15, interval = 0, scope = csv_orchestration_test_scope()),
    list(dry = FALSE, executions = 3L, bytes = 1000, fields = NULL,
         timeout = 15, interval = 0, scope = csv_orchestration_test_scope()),
    list(dry = FALSE, executions = 3L, bytes = 1000, fields = 1000L,
         timeout = 0, interval = 0, scope = csv_orchestration_test_scope()),
    list(dry = FALSE, executions = 3L, bytes = 1000, fields = 1000L,
         timeout = 15, interval = -1, scope = csv_orchestration_test_scope()),
    list(dry = FALSE, executions = 3L, bytes = 1000, fields = 1000L,
         timeout = 15, interval = 0, scope = NULL)
  )
  for (case in cases) {
    csv_execution_test_options(performer)
    expect_error(
      gx_csv_orchestration_impl(
        plan, case$dry, case$executions, case$bytes, case$fields,
        case$timeout, case$interval, case$scope
      ),
      class = "gx_error_csv_orchestration"
    )
  }
  expect_length(calls$requests, 0L)
})

test_that("M7h whole-object validation fails closed on forged facts", {
  result <- csv_orchestration_test_build(
    scope = csv_orchestration_test_scope("forgery")
  )
  cases <- list(
    status = function(x) {
      x$status$succeeded[[1L]] <- FALSE
      x
    },
    bytes = function(x) {
      x$status$encoded_bytes[[1L]] <- x$status$encoded_bytes[[1L]] + 1
      x
    },
    data = function(x) {
      x$results[[1L]]$data[[1L]][[1L]] <- "forged"
      x
    },
    child_scope = function(x) {
      x$results[[1L]]$execution$execution_scope_id <-
        csv_orchestration_test_scope("foreign")
      x
    },
    resolved_host = function(x) {
      x$results[[1L]]$attempt$resolved_host[[1L]] <- "other.example.org"
      x
    },
    parse_type = function(x) {
      x$results[[1L]]$parse$row_count <- as.double(
        x$results[[1L]]$parse$row_count
      )
      x
    },
    plan = function(x) {
      x$request_plan$request_plans$max_rows[[1L]] <- 1L
      x
    },
    metadata = function(x) {
      x$metadata$replayable <- TRUE
      x
    }
  )
  for (mutate in cases) {
    forged <- mutate(csv_orchestration_test_clone(result))
    expect_error(
      gx_csv_orchestration_validate_impl(forged),
      class = "gx_error_csv_orchestration"
    )
  }
})

test_that("the M7h orchestration contract remains internal", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false("gx_csv_orchestration_impl" %in% exports)
  expect_false("gx_csv_orchestration_validate_impl" %in% exports)
  expect_false("gx_fetch" %in% exports)

  result <- csv_orchestration_test_build(dry_run = TRUE)
  output <- capture.output(print(result), type = "message")
  expect_true(any(grepl("dry_run", output, fixed = TRUE)))
  expect_true(any(grepl("CSV requests admitted", output, fixed = TRUE)))
})
