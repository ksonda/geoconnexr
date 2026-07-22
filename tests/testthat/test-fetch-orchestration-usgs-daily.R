test_that("M7n schedules USGS daily in deterministic global order", {
  plan <- fetch_orchestration_test_usgs_daily_plan()
  dry <- fetch_orchestration_test_build(
    plan = plan,
    dry_run = TRUE,
    max_executions = 7L,
    max_total_bytes = 140000,
    scope = fetch_orchestration_test_scope("daily-dry-run")
  )
  expected <- c(
    "csv", "csv", "csv", "wqp", "edr",
    "usgs_waterdata_daily", "ogc_api_features"
  )
  expect_identical(dry$requests$handler_id, expected)
  expect_identical(dry$requests$fetch_order, 1:7)
  expect_identical(dry$metadata$counts$usgs_daily_requests, 1L)
  expect_false(dry$metadata$usgs_daily_semantics_validated)

  calls <- new.env(parent = emptyenv())
  calls$handlers <- character()
  calls$requests <- list()
  live <- fetch_orchestration_test_build(
    plan = plan,
    performer = fetch_orchestration_test_performer(calls = calls),
    max_executions = 7L,
    max_total_bytes = 140000,
    scope = fetch_orchestration_test_scope("daily-live")
  )
  expect_identical(calls$handlers, expected)
  expect_identical(vapply(
    live$results, class, character(1), USE.NAMES = FALSE
  ), c(
    rep("gx_csv_orchestration_result", 3L),
    "gx_wqp_orchestration_result", "gx_edr_orchestration_result",
    "gx_usgs_daily_orchestration_result",
    "gx_oaf_orchestration_result"
  ))
  daily <- live$results[[6L]]
  expect_false("request_plan" %in% names(daily))
  expect_identical(daily$data$value, c("167.5", "170"))
  expect_s3_class(daily$data$time, "Date")
  expect_true(daily$parse$truncated)
  expect_identical(
    daily$parse$stop_reason, "next_link_not_followed"
  )
  expect_identical(live$metadata$counts$usgs_daily_results, 1L)
  expect_true(live$metadata$usgs_daily_semantics_validated)
  expect_identical(
    gx_fetch_orchestration_validate_impl(live), invisible(live)
  )
})

test_that("M7n isolates USGS daily failures from later OGC", {
  plan <- fetch_orchestration_test_usgs_daily_plan()

  capability_calls <- new.env(parent = emptyenv())
  capability_calls$handlers <- character()
  capability_calls$requests <- list()
  capability <- fetch_orchestration_test_build(
    plan = plan,
    performer = fetch_orchestration_test_performer(calls = capability_calls),
    max_executions = 7L,
    max_total_bytes = 140000,
    usgs_daily_symbol_resolver =
      usgs_daily_test_resolver(available = FALSE),
    scope = fetch_orchestration_test_scope("daily-capability")
  )
  expect_false(any(
    capability_calls$handlers == "usgs_waterdata_daily"
  ))
  daily_row <- which(
    capability$status$handler_id == "usgs_waterdata_daily"
  )
  oaf_row <- which(capability$status$handler_id == "ogc_api_features")
  expect_identical(
    capability$status$orchestration_status[[daily_row]],
    "capability_failed"
  )
  expect_identical(
    capability$status$error_code[[daily_row]],
    "usgs_daily_capability_failed"
  )
  expect_identical(capability$status$physical_attempts[[daily_row]], 0L)
  expect_true(capability$status$succeeded[[oaf_row]])

  bad_body <- usgs_daily_test_body_replace(
    '"parameter_code": "00060"', '"parameter_code": "00065"'
  )
  parsed <- fetch_orchestration_test_build(
    plan = plan,
    performer = fetch_orchestration_test_performer(
      usgs_daily_body = bad_body
    ),
    max_executions = 7L,
    max_total_bytes = 140000,
    scope = fetch_orchestration_test_scope("daily-parse")
  )
  daily_row <- which(
    parsed$status$handler_id == "usgs_waterdata_daily"
  )
  oaf_row <- which(parsed$status$handler_id == "ogc_api_features")
  expect_identical(
    parsed$status$orchestration_status[[daily_row]], "parse_failed"
  )
  expect_identical(
    parsed$status$error_code[[daily_row]],
    "usgs_daily_parse_failed"
  )
  expect_identical(parsed$status$physical_attempts[[daily_row]], 1L)
  expect_true(parsed$status$succeeded[[oaf_row]])

  transported <- fetch_orchestration_test_build(
    plan = plan,
    performer = fetch_orchestration_test_performer(
      fail_usgs_daily = TRUE
    ),
    max_executions = 7L,
    max_total_bytes = 140000,
    scope = fetch_orchestration_test_scope("daily-transport")
  )
  daily_row <- which(
    transported$status$handler_id == "usgs_waterdata_daily"
  )
  oaf_row <- which(transported$status$handler_id == "ogc_api_features")
  expect_identical(
    transported$status$orchestration_status[[daily_row]],
    "transport_failed"
  )
  expect_identical(
    transported$status$error_code[[daily_row]],
    "usgs_daily_transport_failed"
  )
  expect_identical(transported$status$physical_attempts[[daily_row]], 1L)
  expect_true(transported$status$succeeded[[oaf_row]])
  expect_false(any(grepl(
    "sensitive USGS daily transport detail",
    unlist(transported, recursive = TRUE, use.names = FALSE),
    fixed = TRUE
  )))
})

test_that("M7n compact USGS daily evidence fails closed on forgery", {
  result <- fetch_orchestration_test_build(
    plan = fetch_orchestration_test_usgs_daily_plan(),
    max_executions = 7L,
    max_total_bytes = 140000,
    scope = fetch_orchestration_test_scope("daily-compact-forgery")
  )
  index <- which(vapply(
    result$results, inherits, logical(1),
    what = "gx_usgs_daily_orchestration_result"
  ))
  mutations <- list(
    body = function(x) {
      x$results[[index]]$response_body[[1L]] <- as.raw(bitwXor(
        as.integer(x$results[[index]]$response_body[[1L]]), 1L
      ))
      x
    },
    data = function(x) {
      x$results[[index]]$data$value[[1L]] <- "999"
      x
    },
    attempt = function(x) {
      x$results[[index]]$attempt$encoded_bytes[[1L]] <-
        x$results[[index]]$attempt$encoded_bytes[[1L]] + 1
      x
    },
    status = function(x) {
      row <- which(x$status$handler_id == "usgs_waterdata_daily")
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
