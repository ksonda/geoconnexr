fetch_public_test_limits <- function() {
  limits <- gx_fetch_public_limits_impl()
  limits$max_response_bytes <- 20000L
  limits$max_rows <- 10000L
  limits$max_columns <- 100L
  limits$max_fields <- 1000L
  limits$max_executions <- 7L
  limits$max_total_bytes <- 140000
  limits$oaf_limit <- 2L
  limits$timeout <- 15
  limits$min_interval <- 0
  limits
}

fetch_public_test_plan <- function() {
  fetch_orchestration_test_usgs_daily_plan()$intent_set$plan
}

fetch_public_test_live <- function(label = "public-live") {
  performer <- fetch_orchestration_test_performer()
  oaf_test_options(performer)
  gx_fetch_impl(
    plan = fetch_public_test_plan(),
    limits = fetch_public_test_limits(),
    orchestration_scope_id = fetch_orchestration_test_scope(label),
    oaf_symbol_resolver = oaf_test_resolver(),
    wqp_symbol_resolver = wqp_test_resolver(),
    edr_symbol_resolver = edr_test_resolver(),
    usgs_continuous_symbol_resolver = usgs_continuous_test_resolver(),
    usgs_daily_symbol_resolver = usgs_daily_test_resolver()
  )
}

test_that("public fetch planning freezes the built-in registry and budgets", {
  withr::local_options(list(geoconnexr.clock = fetch_plan_test_now))
  catalog <- csv_intents_test_fixture_catalog()
  plan <- gx_fetch_plan(
    catalog,
    time = fetch_plan_test_time(),
    max_datasets = 4L,
    max_bytes = 40000
  )

  expect_s3_class(plan, "gx_fetch_plan")
  expect_identical(plan$budgets$max_datasets, 4L)
  expect_identical(plan$budgets$max_requests, 4L)
  expect_identical(plan$budgets$max_encoded_bytes, 40000)
  expect_identical(plan$budgets$max_decoded_bytes, 40000)

  handlers <- gx_handlers()
  handlers$precedence[[1L]] <- handlers$precedence[[1L]] + 1L
  expect_error(
    gx_fetch_plan(catalog, handlers = handlers),
    class = "gx_error_fetch_plan_handler"
  )
})

test_that("public dry run is deterministic and performs no host work", {
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
  plan <- fetch_public_test_plan()
  first <- gx_fetch(plan, dry_run = TRUE)
  second <- gx_fetch(plan, dry_run = TRUE)

  expect_identical(touched$count, 0L)
  expect_identical(first, second)
  expect_s3_class(first, "gx_fetched")
  expect_identical(first$contract_version, "0.1.0")
  expect_identical(names(first), c(
    "contract_version", "plan", "status", "results", "provenance", "metadata"
  ))
  expect_true(all(first$status$status[!is.na(first$status$fetch_order)] %in% c(
    "dry_run_planned", "handler_plan_unsupported", "batch_limit_deferred"
  )))
  expect_identical(nrow(first$results), 0L)
  expect_true(first$metadata$dry_run)
  expect_identical(first$metadata$scope, "supported_subset_v1")
  expect_identical(
    gx_fetched_validate_impl(first), invisible(first)
  )
})

test_that("public fetch projects handler-native payloads and provenance", {
  fetched <- fetch_public_test_live()

  expect_s3_class(fetched, "gx_fetched")
  expect_identical(fetched$plan, fetch_public_test_plan())
  successful <- which(fetched$status$succeeded)
  expect_identical(fetched$status$result_index[successful], 1:7)
  expect_identical(fetched$status$fetch_order[successful], 1:7)
  expect_identical(fetched$results$result_index, 1:7)
  expect_identical(fetched$results$handler_id, c(
    "csv", "csv", "csv", "wqp", "edr",
    "usgs_waterdata_daily", "ogc_api_features"
  ))
  expect_identical(fetched$results$payload_class, c(
    rep("table", 6L), "sf"
  ))
  expect_identical(fetched$results$raw_body_available, c(
    FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE
  ))
  expect_true(all(vapply(
    fetched$results$data[1:6], inherits, logical(1), what = "data.frame"
  )))
  expect_s3_class(fetched$results$data[[7L]], "sf")
  expect_identical(
    fetched$results$data[[6L]]$value, c("167.5", "170")
  )
  expect_s3_class(fetched$results$data[[6L]]$time, "Date")
  expect_identical(fetched$metadata$counts$results, 7L)
  expect_identical(fetched$metadata$counts$failed, 0L)
  expect_false(fetched$metadata$replayable)
  expect_identical(fetched$metadata$pagination, "single_page_no_follow")
  expect_identical(
    gx_fetched_validate_impl(fetched), invisible(fetched)
  )
})

test_that("public fetched results fail closed on forgery", {
  fetched <- fetch_public_test_live("public-forgery")
  mutations <- list(
    status = function(x) {
      x$status$status[[1L]] <- "forged"
      x
    },
    data = function(x) {
      x$results$data[[1L]][[1L]][[1L]] <- "forged"
      x
    },
    metadata = function(x) {
      x$metadata$pagination <- "follow"
      x
    },
    provenance = function(x) {
      x$provenance$status$encoded_bytes[[1L]] <-
        x$provenance$status$encoded_bytes[[1L]] + 1
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](fetch_orchestration_test_clone(fetched))
    expect_error(
      gx_fetched_validate_impl(forged),
      class = "gx_error_fetched",
      info = name
    )
  }
})

test_that("public fetch rejects scope expansion and keeps internals private", {
  plan <- fetch_public_test_plan()
  expect_error(
    gx_fetch(plan, parallel = 2L, dry_run = TRUE),
    class = "gx_error_fetch_parallel"
  )
  expect_error(
    gx_fetch(plan, dry_run = NA),
    class = "gx_error_fetch_policy"
  )

  exports <- getNamespaceExports("geoconnexr")
  expect_true(all(c("gx_fetch_plan", "gx_fetch") %in% exports))
  expect_false(any(c(
    "gx_fetch_impl", "gx_fetched_new_impl", "gx_fetched_validate_impl",
    "gx_fetch_orchestration_impl"
  ) %in% exports))
})
