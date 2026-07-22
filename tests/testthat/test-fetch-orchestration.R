test_that("M7l dry run plans CSV and OGC globally without host activity", {
  touched <- new.env(parent = emptyenv())
  touched$count <- 0L
  forbidden <- function(...) {
    touched$count <- touched$count + 1L
    stop("dry run touched host state", call. = FALSE)
  }
  withr::local_options(list(
    geoconnexr.performer = forbidden,
    geoconnexr.dns_resolver = forbidden,
    geoconnexr.clock = forbidden,
    geoconnexr.throttle_clock = forbidden,
    geoconnexr.throttle_sleep = forbidden
  ))
  plan <- oaf_test_m7d_plan(max_response_bytes = 100L)
  scope <- fetch_orchestration_test_scope("deterministic")
  first <- fetch_orchestration_test_build(
    plan = plan, dry_run = TRUE, max_total_bytes = 500, scope = scope
  )
  second <- fetch_orchestration_test_build(
    plan = plan, dry_run = TRUE, max_total_bytes = 500, scope = scope
  )

  expect_identical(touched$count, 0L)
  expect_identical(first, second)
  expect_identical(class(first), "gx_fetch_orchestration")
  expect_identical(first$contract_version, "0.5.0")
  expect_identical(first$policy$slice_id, "cross_handler_orchestration_v5")
  expect_identical(names(first), c(
    "contract_version", "request_plan", "policy", "requests",
    "orchestration", "results", "status", "metadata"
  ))
  expect_identical(first$request_plan, plan)
  expect_identical(first$requests$fetch_order, 1:5)
  expect_identical(first$requests$handler_id, c(
    "csv", "csv", "csv", "wqp", "ogc_api_features"
  ))
  expect_identical(first$status$orchestration_status, c(
    rep("dry_run_planned", 5L), rep("not_selected", 3L)
  ))
  expect_false(any(first$status$attempted))
  expect_identical(first$results, list())
  expect_identical(first$metadata$counts$candidate_requests, 5L)
  expect_identical(first$metadata$counts$wqp_requests, 1L)
  expect_identical(first$metadata$counts$oaf_requests, 1L)
  expect_true(first$metadata$global_order_enforced)
  expect_identical(
    gx_fetch_orchestration_validate_impl(first), invisible(first)
  )
})

test_that("M7l executes mixed handlers once in deterministic global order", {
  calls <- new.env(parent = emptyenv())
  calls$handlers <- character()
  calls$requests <- list()
  result <- fetch_orchestration_test_build(
    performer = fetch_orchestration_test_performer(calls = calls),
    scope = fetch_orchestration_test_scope("all-success")
  )

  expect_identical(calls$handlers, c(
    "csv", "csv", "csv", "wqp", "ogc_api_features"
  ))
  expect_length(calls$requests, 5L)
  expect_match(calls$requests[[4L]]$url, "dataProfile=narrowResult", fixed = TRUE)
  expect_true(endsWith(calls$requests[[5L]]$url, "f=json&limit=2"))
  expect_true(all(vapply(
    calls$requests, function(request) request$retries == 0L, logical(1)
  )))
  expect_identical(result$status$orchestration_status[1:5], rep(
    "provider_response_validated_and_parsed", 5L
  ))
  expect_identical(result$status$result_index[1:5], 1:5)
  expect_identical(vapply(
    result$results, class, character(1), USE.NAMES = FALSE
  ), c(
    rep("gx_csv_orchestration_result", 3L),
    "gx_wqp_orchestration_result",
    "gx_oaf_orchestration_result"
  ))
  wqp <- result$results[[4L]]
  expect_false("request_plan" %in% names(wqp))
  expect_identical(nrow(wqp$data), 2L)
  oaf <- result$results[[5L]]
  expect_false("request_plan" %in% names(oaf))
  expect_true(is.raw(oaf$response_body))
  expect_s3_class(oaf$result, "sf")
  expect_identical(nrow(oaf$result), 2L)
  expect_identical(result$metadata$counts$physical_attempts, 5L)
  expect_identical(result$metadata$counts$csv_results, 3L)
  expect_identical(result$metadata$counts$wqp_results, 1L)
  expect_identical(result$metadata$counts$oaf_results, 1L)
  expect_true(result$metadata$runtime_symbols_checked)
  expect_identical(
    result$orchestration$orchestration_status, "execution_complete"
  )
  expect_identical(
    gx_fetch_orchestration_validate_impl(result), invisible(result)
  )
})

test_that("M7l schedules EDR with CSV, WQP, and OGC in global order", {
  plan <- fetch_orchestration_test_edr_plan()
  dry <- fetch_orchestration_test_build(
    plan = plan,
    dry_run = TRUE,
    max_executions = 6L,
    max_total_bytes = 120000,
    scope = fetch_orchestration_test_scope("edr-dry-run")
  )
  expect_identical(dry$requests$handler_id, c(
    "csv", "csv", "csv", "wqp", "edr", "ogc_api_features"
  ))
  expect_identical(dry$requests$fetch_order, 1:6)
  expect_identical(dry$metadata$counts$edr_requests, 1L)
  expect_false(dry$metadata$edr_semantics_validated)

  calls <- new.env(parent = emptyenv())
  calls$handlers <- character()
  calls$requests <- list()
  live <- fetch_orchestration_test_build(
    plan = plan,
    performer = fetch_orchestration_test_performer(calls = calls),
    max_executions = 6L,
    max_total_bytes = 120000,
    scope = fetch_orchestration_test_scope("edr-live")
  )
  expect_identical(calls$handlers, c(
    "csv", "csv", "csv", "wqp", "edr", "ogc_api_features"
  ))
  expect_identical(vapply(
    live$results, class, character(1), USE.NAMES = FALSE
  ), c(
    rep("gx_csv_orchestration_result", 3L),
    "gx_wqp_orchestration_result", "gx_edr_orchestration_result",
    "gx_oaf_orchestration_result"
  ))
  edr <- live$results[[5L]]
  expect_false("request_plan" %in% names(edr))
  expect_identical(edr$parse$domain_type, "PointSeries")
  expect_identical(edr$parse$parameter_name, "discharge")
  expect_identical(live$metadata$counts$edr_results, 1L)
  expect_true(live$metadata$edr_semantics_validated)
  expect_identical(
    gx_fetch_orchestration_validate_impl(live), invisible(live)
  )
})

test_that("M7l isolates EDR capability and parse failures from later OGC", {
  plan <- fetch_orchestration_test_edr_plan()
  capability_calls <- new.env(parent = emptyenv())
  capability_calls$handlers <- character()
  capability_calls$requests <- list()
  capability <- fetch_orchestration_test_build(
    plan = plan,
    performer = fetch_orchestration_test_performer(calls = capability_calls),
    max_executions = 6L,
    max_total_bytes = 120000,
    edr_symbol_resolver = edr_test_resolver(available = FALSE),
    scope = fetch_orchestration_test_scope("missing-edr-capability")
  )
  expect_identical(capability_calls$handlers, c(
    "csv", "csv", "csv", "wqp", "ogc_api_features"
  ))
  edr_row <- which(capability$status$handler_id == "edr")
  oaf_row <- which(capability$status$handler_id == "ogc_api_features")
  expect_identical(
    capability$status$orchestration_status[[edr_row]], "capability_failed"
  )
  expect_identical(
    capability$status$error_code[[edr_row]], "edr_capability_failed"
  )
  expect_identical(capability$status$physical_attempts[[edr_row]], 0L)
  expect_true(capability$status$succeeded[[oaf_row]])

  parse_calls <- new.env(parent = emptyenv())
  parse_calls$handlers <- character()
  parse_calls$requests <- list()
  parsed <- fetch_orchestration_test_build(
    plan = plan,
    performer = fetch_orchestration_test_performer(calls = parse_calls),
    max_executions = 6L,
    max_total_bytes = 120000,
    edr_symbol_resolver = edr_test_resolver(mismatch = TRUE),
    scope = fetch_orchestration_test_scope("edr-parse-failure")
  )
  expect_identical(parse_calls$handlers, c(
    "csv", "csv", "csv", "wqp", "edr", "ogc_api_features"
  ))
  edr_row <- which(parsed$status$handler_id == "edr")
  oaf_row <- which(parsed$status$handler_id == "ogc_api_features")
  expect_identical(
    parsed$status$orchestration_status[[edr_row]], "parse_failed"
  )
  expect_identical(parsed$status$error_code[[edr_row]], "edr_parse_failed")
  expect_identical(parsed$status$physical_attempts[[edr_row]], 1L)
  expect_true(parsed$status$succeeded[[oaf_row]])

  transport_calls <- new.env(parent = emptyenv())
  transport_calls$handlers <- character()
  transport_calls$requests <- list()
  transported <- fetch_orchestration_test_build(
    plan = plan,
    performer = fetch_orchestration_test_performer(
      fail_edr = TRUE, calls = transport_calls
    ),
    max_executions = 6L,
    max_total_bytes = 120000,
    scope = fetch_orchestration_test_scope("edr-transport-failure")
  )
  edr_row <- which(transported$status$handler_id == "edr")
  oaf_row <- which(transported$status$handler_id == "ogc_api_features")
  expect_identical(
    transported$status$orchestration_status[[edr_row]], "transport_failed"
  )
  expect_identical(
    transported$status$error_code[[edr_row]], "edr_transport_failed"
  )
  expect_identical(transported$status$physical_attempts[[edr_row]], 1L)
  expect_true(transported$status$succeeded[[oaf_row]])
  expect_false(any(grepl(
    "sensitive EDR transport detail",
    unlist(transported, recursive = TRUE, use.names = FALSE),
    fixed = TRUE
  )))
})

test_that("M7l compact EDR evidence fails closed on forgery", {
  result <- fetch_orchestration_test_build(
    plan = fetch_orchestration_test_edr_plan(),
    max_executions = 6L,
    max_total_bytes = 120000,
    scope = fetch_orchestration_test_scope("edr-compact-forgery")
  )
  edr_index <- which(vapply(
    result$results, inherits, logical(1), what = "gx_edr_orchestration_result"
  ))
  mutations <- list(
    body = function(x) {
      x$results[[edr_index]]$response_body[[1L]] <- as.raw(bitwXor(
        as.integer(x$results[[edr_index]]$response_body[[1L]]), 1L
      ))
      x
    },
    data = function(x) {
      x$results[[edr_index]]$data$value[[1L]] <- 999
      x
    },
    attempt = function(x) {
      x$results[[edr_index]]$attempt$encoded_bytes[[1L]] <-
        x$results[[edr_index]]$attempt$encoded_bytes[[1L]] + 1
      x
    },
    status = function(x) {
      row <- which(x$status$handler_id == "edr")
      x$status$encoded_bytes[[row]] <- x$status$encoded_bytes[[row]] + 1
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](fetch_orchestration_test_clone(result))
    expect_error(
      gx_fetch_orchestration_validate_impl(forged),
      class = "gx_error_fetch_orchestration",
      info = name
    )
  }
})

test_that("M7l isolates a CSV failure and still executes OGC", {
  calls <- new.env(parent = emptyenv())
  calls$handlers <- character()
  calls$requests <- list()
  result <- fetch_orchestration_test_build(
    performer = fetch_orchestration_test_performer(
      fail_csv_on = 2L, calls = calls
    ),
    scope = fetch_orchestration_test_scope("csv-failure")
  )

  expect_identical(calls$handlers, c(
    "csv", "csv", "csv", "wqp", "ogc_api_features"
  ))
  expect_identical(result$status$orchestration_status[1:5], c(
    "provider_response_validated_and_parsed", "transport_failed",
    "provider_response_validated_and_parsed",
    "provider_response_validated_and_parsed",
    "provider_response_validated_and_parsed"
  ))
  expect_identical(result$status$error_code[[2L]], "csv_transport_failed")
  expect_true(result$status$succeeded[[5L]])
  expect_length(result$results, 4L)
  expect_identical(result$metadata$counts$failed_requests, 1L)
  expect_false(any(grepl(
    "sensitive cross-handler transport detail",
    unlist(result, recursive = TRUE, use.names = FALSE),
    fixed = TRUE
  )))
  expect_identical(
    result$orchestration$orchestration_status,
    "execution_complete_with_failures"
  )
})

test_that("M7l isolates WQP capability and parse failures from OGC", {
  capability_calls <- new.env(parent = emptyenv())
  capability_calls$handlers <- character()
  capability_calls$requests <- list()
  capability <- fetch_orchestration_test_build(
    performer = fetch_orchestration_test_performer(calls = capability_calls),
    wqp_symbol_resolver = wqp_test_resolver(available = FALSE),
    scope = fetch_orchestration_test_scope("missing-wqp-symbol")
  )
  expect_identical(
    capability_calls$handlers,
    c(rep("csv", 3L), "ogc_api_features")
  )
  expect_identical(
    capability$status$orchestration_status[[4L]], "capability_failed"
  )
  expect_identical(
    capability$status$error_code[[4L]], "wqp_capability_failed"
  )
  expect_identical(capability$status$physical_attempts[[4L]], 0L)
  expect_true(capability$status$succeeded[[5L]])
  expect_identical(capability$metadata$counts$physical_attempts, 4L)

  parse_calls <- new.env(parent = emptyenv())
  parse_calls$handlers <- character()
  parse_calls$requests <- list()
  parsed <- fetch_orchestration_test_build(
    performer = fetch_orchestration_test_performer(calls = parse_calls),
    wqp_symbol_resolver = wqp_test_resolver(mismatch = TRUE),
    scope = fetch_orchestration_test_scope("wqp-parse-failure")
  )
  expect_identical(
    parse_calls$handlers,
    c(rep("csv", 3L), "wqp", "ogc_api_features")
  )
  expect_identical(parsed$status$orchestration_status[[4L]], "parse_failed")
  expect_identical(parsed$status$error_code[[4L]], "wqp_parse_failed")
  expect_identical(parsed$status$physical_attempts[[4L]], 1L)
  expect_true(parsed$status$succeeded[[5L]])
  expect_identical(parsed$metadata$counts$physical_attempts, 5L)
})

test_that("M7l records OGC capability failure without charging transport", {
  calls <- new.env(parent = emptyenv())
  calls$handlers <- character()
  calls$requests <- list()
  result <- fetch_orchestration_test_build(
    performer = fetch_orchestration_test_performer(calls = calls),
    oaf_symbol_resolver = oaf_test_resolver(available = FALSE),
    scope = fetch_orchestration_test_scope("missing-oaf-symbol")
  )

  expect_identical(calls$handlers, c(rep("csv", 3L), "wqp"))
  expect_identical(
    result$status$orchestration_status[[5L]], "capability_failed"
  )
  expect_identical(result$status$error_code[[5L]], "oaf_capability_failed")
  expect_true(result$status$attempted[[5L]])
  expect_false(result$status$succeeded[[5L]])
  expect_identical(result$status$physical_attempts[[5L]], 0L)
  expect_identical(result$status$encoded_bytes[[5L]], 0)
  expect_true(is.na(result$status$execution_id[[5L]]))
  expect_identical(result$metadata$counts$physical_attempts, 4L)
  expect_true(result$metadata$runtime_symbols_checked)
  expect_identical(
    gx_fetch_orchestration_validate_impl(result), invisible(result)
  )
})

test_that("M7l records an OGC parse failure as one charged attempt", {
  calls <- new.env(parent = emptyenv())
  calls$handlers <- character()
  calls$requests <- list()
  result <- fetch_orchestration_test_build(
    performer = fetch_orchestration_test_performer(
      oaf_body = oaf_test_body("too-many-features.geojson"), calls = calls
    ),
    scope = fetch_orchestration_test_scope("oaf-parse-failure")
  )

  expect_length(calls$requests, 5L)
  expect_identical(result$status$orchestration_status[[5L]], "parse_failed")
  expect_identical(result$status$error_code[[5L]], "oaf_parse_failed")
  expect_identical(result$status$physical_attempts[[5L]], 1L)
  expect_gt(result$status$encoded_bytes[[5L]], 0)
  expect_identical(result$metadata$counts$physical_attempts, 5L)
  expect_length(result$results, 4L)
})

test_that("M7l admission shares count and byte limits across handlers", {
  plan <- oaf_test_m7d_plan(max_response_bytes = 100L)
  count_limited <- fetch_orchestration_test_build(
    plan = plan, dry_run = TRUE, max_executions = 4L,
    max_total_bytes = 500
  )
  byte_limited <- fetch_orchestration_test_build(
    plan = plan, dry_run = TRUE, max_executions = 5L,
    max_total_bytes = 350
  )

  expect_identical(
    count_limited$status$orchestration_status[1:5],
    c(rep("dry_run_planned", 4L), "batch_limit_deferred")
  )
  expect_identical(
    byte_limited$status$orchestration_status[1:5],
    c(rep("dry_run_planned", 3L), rep("batch_limit_deferred", 2L))
  )
  expect_identical(
    byte_limited$metadata$counts$batch_limit_deferred, 2L
  )
})

test_that("M7l makes an incompatible OGC URL an explicit terminal row", {
  plan <- csv_execution_test_plan(max_response_bytes = 100L)
  result <- fetch_orchestration_test_build(
    plan = plan, dry_run = TRUE, max_executions = 4L,
    max_total_bytes = 400
  )

  expect_identical(nrow(result$requests), 4L)
  expect_identical(
    result$status$orchestration_status[[5L]], "handler_plan_unsupported"
  )
  expect_identical(result$metadata$counts$handler_plan_unsupported, 1L)
  expect_true("oaf_request_plan_unsupported" %in%
    result$metadata$non_replayable_reasons)
  expect_identical(
    gx_fetch_orchestration_validate_impl(result), invisible(result)
  )
})

test_that("M7l accepts an empty plan without touching host state", {
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
  result <- fetch_orchestration_test_build(
    plan = plan, performer = performer, max_executions = 1L,
    max_total_bytes = 100
  )
  expect_identical(touched, 0L)
  expect_identical(nrow(result$requests), 0L)
  expect_identical(nrow(result$status), 0L)
  expect_identical(result$results, list())
  expect_identical(result$metadata$counts$distributions, 0L)
})

test_that("M7l rejects invalid policy before transport", {
  plan <- oaf_test_m7d_plan()
  touched <- 0L
  performer <- function(request) {
    touched <<- touched + 1L
    stop("must not execute", call. = FALSE)
  }
  cases <- list(
    list(FALSE, 0L, 80000, 1000L, 2L, 15, 0),
    list(FALSE, 33L, 80000, 1000L, 2L, 15, 0),
    list(FALSE, 4L, 0, 1000L, 2L, 15, 0),
    list(FALSE, 4L, 80000, NULL, 2L, 15, 0),
    list(FALSE, 4L, 80000, 1000L, 0L, 15, 0),
    list(FALSE, 4L, 80000, 1000L, 2L, 0, 0),
    list(FALSE, 4L, 80000, 1000L, 2L, 15, -1)
  )
  for (case in cases) {
    expect_error(
      gx_fetch_orchestration_impl(
        plan, case[[1L]], case[[2L]], case[[3L]], case[[4L]],
        case[[5L]], case[[6L]], case[[7L]],
        fetch_orchestration_test_scope("invalid"), oaf_test_resolver()
      ),
      class = "gx_error_fetch_orchestration"
    )
  }
  expect_identical(touched, 0L)
})

test_that("M7l whole-object validation fails closed on forged facts", {
  result <- fetch_orchestration_test_build(
    scope = fetch_orchestration_test_scope("forgery")
  )
  mutations <- list(
    requests = function(x) {
      x$requests$logical_request_id[[1L]] <- paste(rep("0", 64L), collapse = "")
      x
    },
    status = function(x) {
      x$status$succeeded[[1L]] <- FALSE
      x
    },
    csv_data = function(x) {
      x$results[[1L]]$data[[1L]][[1L]] <- "forged"
      x
    },
    oaf_body = function(x) {
      body <- x$results[[5L]]$response_body
      body[[1L]] <- as.raw(bitwXor(as.integer(body[[1L]]), 1L))
      x$results[[5L]]$response_body <- body
      x
    },
    oaf_result = function(x) {
      x$results[[5L]]$result$feature_id[[1L]] <- "forged"
      x
    },
    child_scope = function(x) {
      x$results[[5L]]$execution$execution_scope_id <-
        fetch_orchestration_test_scope("foreign")
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
  for (name in names(mutations)) {
    forged <- mutations[[name]](fetch_orchestration_test_clone(result))
    expect_error(
      gx_fetch_orchestration_validate_impl(forged),
      class = "gx_error_fetch_orchestration",
      info = name
    )
  }
})

test_that("the M7l cross-handler contract remains internal", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_fetch_orchestration_impl", "gx_fetch_orchestration_validate_impl"
  ) %in% exports))
  expect_false("gx_fetch" %in% exports)

  result <- fetch_orchestration_test_build(dry_run = TRUE)
  output <- capture.output(print(result), type = "message")
  expect_true(any(grepl("dry_run", output, fixed = TRUE)))
  expect_true(any(grepl("OGC Features", output, fixed = TRUE)))
  expect_true(any(grepl("WQP", output, fixed = TRUE)))
})
