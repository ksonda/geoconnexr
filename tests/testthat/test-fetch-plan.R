test_that("fetch-plan empty helpers have exact typed contracts", {
  distributions <- gx_fetch_plan_empty_distributions()
  parameters <- gx_fetch_plan_empty_parameters()
  handlers <- gx_fetch_plan_empty_handlers()

  expect_s3_class(distributions, "tbl_df")
  expect_identical(names(distributions), c(
    "contract_version", "selection_order", "fetch_order", "site_uri",
    "dataset_id", "distribution_id", "provider_uri", "provider_name",
    "distribution_url", "media_type", "conforms_to", "handler_id",
    "fetchable", "temporal_start", "temporal_end", "time_start",
    "time_end", "variable_count", "selected", "decision"
  ))
  expect_type(distributions$selection_order, "integer")
  expect_type(distributions$fetch_order, "integer")
  expect_type(distributions$conforms_to, "list")
  expect_type(distributions$fetchable, "logical")
  expect_s3_class(distributions$temporal_start, "POSIXct")
  expect_s3_class(distributions$temporal_end, "POSIXct")
  expect_s3_class(distributions$time_start, "POSIXct")
  expect_s3_class(distributions$time_end, "POSIXct")
  expect_type(distributions$variable_count, "integer")
  expect_type(distributions$selected, "logical")
  expect_equal(nrow(distributions), 0L)

  expect_s3_class(parameters, "tbl_df")
  expect_identical(names(parameters), c(
    "contract_version", "distribution_id", "parameter_order", "variable_id",
    "variable_uri", "variable_name", "unit_uri", "unit_label",
    "measurement_technique", "parameter_key", "mapping_status"
  ))
  expect_type(parameters$parameter_order, "integer")
  expect_equal(nrow(parameters), 0L)

  expect_s3_class(handlers, "tbl_df")
  expect_identical(names(handlers), c(
    "contract_version", "handler_id", "precedence", "lifecycle", "outcome",
    "implementation_id", "availability", "required_package",
    "minimum_version", "replayable"
  ))
  expect_type(handlers$precedence, "integer")
  expect_type(handlers$replayable, "logical")
  expect_equal(nrow(handlers), 0L)
})

test_that("empty catalogs produce a valid selection-only plan", {
  catalog <- fetch_plan_test_catalog(populated = FALSE)
  plan <- fetch_plan_test_build(catalog, time = NULL)

  expect_identical(class(plan), "gx_fetch_plan")
  expect_identical(names(plan), c(
    "contract_version", "source", "time", "distributions", "parameters",
    "handlers", "requests", "budgets", "metadata"
  ))
  expect_identical(plan$contract_version, "0.1.0")
  expect_identical(gx_fetch_plan_validate_impl(plan), invisible(plan))
  expect_equal(nrow(plan$distributions), 0L)
  expect_equal(nrow(plan$parameters), 0L)
  expect_identical(plan$requests, list())

  expect_identical(names(plan$source), c(
    "catalog_contract_version", "aoi_contract_version", "aoi_type", "aoi_id",
    "datasets_sha256", "datasets_status", "datasets_truncated", "dataset_rows"
  ))
  expect_identical(plan$source$catalog_contract_version, "0.1.0")
  expect_identical(plan$source$aoi_contract_version, "1.0.0")
  expect_identical(plan$source$aoi_type, catalog$aoi$type)
  expect_identical(plan$source$aoi_id, catalog$aoi$id)
  expect_match(plan$source$datasets_sha256, "^[a-f0-9]{64}$")
  expect_identical(plan$source$datasets_status, "complete")
  expect_false(plan$source$datasets_truncated)
  expect_identical(plan$source$dataset_rows, 0L)

  expect_identical(names(plan$time), c("start", "end"))
  expect_s3_class(plan$time$start, "POSIXct")
  expect_s3_class(plan$time$end, "POSIXct")
  expect_true(is.na(plan$time$start))
  expect_true(is.na(plan$time$end))
  expect_identical(attr(plan$time$start, "tzone"), "UTC")
  expect_identical(attr(plan$time$end, "tzone"), "UTC")

  expect_equal(nrow(plan$handlers), 9L)
  expect_identical(plan$handlers$handler_id, c(
    "edr", "usgs_waterdata_continuous", "usgs_waterdata_daily",
    "nwis_legacy_iv", "nwis_legacy_dv", "wqp", "ogc_api_features", "csv",
    "unknown"
  ))
  expect_true(all(diff(plan$handlers$precedence) > 0L))
  expect_identical(plan$handlers$required_package, c(
    "edr4r", rep("dataRetrieval", 5L), NA_character_, "readr", NA_character_
  ))
  expect_identical(plan$handlers$minimum_version, c(
    "0.1.1", "2.7.22", "2.7.22", rep(NA_character_, 6L)
  ))
  expect_identical(plan$handlers$availability, c(
    rep("planned", 8L), "classifier_only"
  ))
  expect_true(all(!plan$handlers$replayable))
  expect_false(plan$metadata$execution_ready)
})

test_that("populated plans expose the exact selection contract and counts", {
  catalog <- fetch_plan_test_catalog()
  plan <- fetch_plan_test_build(catalog)

  expect_identical(gx_fetch_plan_validate_impl(plan), invisible(plan))
  expect_identical(plan$time, list(
    start = fetch_plan_test_time()[[1L]],
    end = fetch_plan_test_time()[[2L]]
  ))
  expect_identical(plan$budgets, list(
    max_datasets = 2L,
    max_requests = 17L,
    max_encoded_bytes = 123456,
    max_decoded_bytes = 654321
  ))
  expect_identical(plan$requests, list())

  expect_identical(plan$distributions$selection_order, seq_len(6L))
  expect_identical(plan$distributions$decision, c(
    "selected_unplanned", "outside_time", "selected_unplanned",
    "reference_only", "skipped_max_datasets", "not_fetchable"
  ))
  expect_identical(plan$distributions$selected, c(
    TRUE, FALSE, TRUE, FALSE, FALSE, FALSE
  ))
  expect_identical(
    plan$distributions$fetch_order[plan$distributions$selected],
    1:2
  )
  expect_true(all(is.na(
    plan$distributions$fetch_order[!plan$distributions$selected]
  )))
  expect_true(is.na(tail(plan$distributions$provider_uri, 1L)))
  expect_identical(tail(plan$distributions$provider_name, 1L), "Provider AARDVARK")

  expect_identical(names(plan$metadata), c(
    "created_at", "registry_contract_version", "registry_sha256",
    "implementation_contract_version", "implementation_sha256", "ordering",
    "counts", "execution_ready", "non_replayable_reasons"
  ))
  expect_identical(plan$metadata$created_at, fetch_plan_test_now())
  expect_identical(plan$metadata$registry_contract_version, "0.1.0")
  expect_identical(plan$metadata$implementation_contract_version, "0.1.0")
  expect_match(plan$metadata$registry_sha256, "^[a-f0-9]{64}$")
  expect_match(plan$metadata$implementation_sha256, "^[a-f0-9]{64}$")
  expect_identical(
    plan$metadata$registry_sha256,
    digest::digest(
      file = system.file("handlers", "registry.yml", package = "geoconnexr"),
      algo = "sha256", serialize = FALSE
    )
  )
  expect_identical(
    plan$metadata$implementation_sha256,
    digest::digest(
      file = system.file(
        "handlers", "implementations-r.json", package = "geoconnexr"
      ),
      algo = "sha256", serialize = FALSE
    )
  )
  expect_identical(plan$metadata$ordering, c(
    "provider_uri_missing", "provider_uri", "provider_name_missing",
    "provider_name", "site_uri", "distribution_id"
  ))
  expect_identical(names(plan$metadata$counts), c(
    "catalog_rows", "unplannable_rows", "distributions", "parameters",
    "handlers", "selected", "reference_only", "not_fetchable",
    "outside_time", "skipped_max_datasets", "requests"
  ))
  expect_identical(plan$metadata$counts, list(
    catalog_rows = 8L,
    unplannable_rows = 1L,
    distributions = 6L,
    parameters = 7L,
    handlers = 9L,
    selected = 2L,
    reference_only = 1L,
    not_fetchable = 1L,
    outside_time = 1L,
    skipped_max_datasets = 1L,
    requests = 0L
  ))
  expect_false(plan$metadata$execution_ready)
  expect_identical(plan$metadata$non_replayable_reasons, c(
    "handler_implementations_planned", "request_plans_absent"
  ))
})

test_that("distribution rows collapse variables without losing parameters", {
  catalog <- fetch_plan_test_catalog()
  plan <- fetch_plan_test_build(catalog)
  alpha_id <- fetch_plan_test_hash("distribution:alpha")
  alpha <- plan$distributions[plan$distributions$distribution_id == alpha_id, ]
  parameters <- plan$parameters[plan$parameters$distribution_id == alpha_id, ]

  expect_equal(nrow(alpha), 1L)
  expect_identical(alpha$variable_count, 2L)
  expect_equal(nrow(parameters), 2L)
  expect_identical(parameters$parameter_order, 1:2)
  expect_identical(
    parameters$variable_id,
    sort(c(
      fetch_plan_test_hash("variable:flow"),
      fetch_plan_test_hash("variable:temperature")
    ), method = "radix")
  )
  expect_true(all(is.na(parameters$parameter_key)))
  expect_true(all(parameters$mapping_status == "unplanned"))
  expect_identical(
    plan$distributions$variable_count,
    as.integer(tabulate(
      match(plan$parameters$distribution_id, plan$distributions$distribution_id),
      nbins = nrow(plan$distributions)
    ))
  )
})

test_that("requested time is intersected and outside rows carry no effective time", {
  plan <- fetch_plan_test_build()
  distribution <- function(key) {
    id <- fetch_plan_test_hash(paste0("distribution:", key))
    plan$distributions[plan$distributions$distribution_id == id, ]
  }

  alpha <- distribution("alpha")
  beta <- distribution("beta")
  outside <- distribution("outside")

  expect_identical(alpha$time_start, fetch_plan_test_time()[[1L]])
  expect_identical(alpha$time_end, fetch_plan_test_time()[[2L]])
  expect_identical(
    beta$time_start,
    as.POSIXct("2025-06-15 00:00:00", tz = "UTC")
  )
  expect_identical(beta$time_end, fetch_plan_test_time()[[2L]])
  expect_identical(outside$decision, "outside_time")
  expect_true(is.na(outside$time_start))
  expect_true(is.na(outside$time_end))
})

test_that("dataset caps apply only to otherwise eligible distributions", {
  zero <- fetch_plan_test_build(max_datasets = 0L)
  one <- fetch_plan_test_build(max_datasets = 1L)
  two <- fetch_plan_test_build(max_datasets = 2L)
  three <- fetch_plan_test_build(max_datasets = 3L)

  expect_identical(zero$metadata$counts$selected, 0L)
  expect_identical(zero$metadata$counts$skipped_max_datasets, 3L)
  expect_identical(one$metadata$counts$selected, 1L)
  expect_identical(one$metadata$counts$skipped_max_datasets, 2L)
  expect_identical(two$metadata$counts$selected, 2L)
  expect_identical(two$metadata$counts$skipped_max_datasets, 1L)
  expect_identical(three$metadata$counts$selected, 3L)
  expect_identical(three$metadata$counts$skipped_max_datasets, 0L)
  for (plan in list(zero, one, two, three)) {
    expect_identical(plan$metadata$counts$reference_only, 1L)
    expect_identical(plan$metadata$counts$not_fetchable, 1L)
    expect_identical(plan$metadata$counts$outside_time, 1L)
  }
})

test_that("plan ordering and source identity are stable under catalog permutation", {
  catalog <- fetch_plan_test_catalog()
  permuted <- fetch_plan_test_clone(catalog)
  permuted$sites <- permuted$sites[rev(seq_len(nrow(permuted$sites))), ]
  permuted$datasets <- permuted$datasets[
    c(8L, 5L, 2L, 7L, 1L, 6L, 4L, 3L),
  ]
  expect_identical(gx_catalog_validate_impl(permuted), invisible(permuted))

  baseline <- fetch_plan_test_build(catalog)
  reordered <- fetch_plan_test_build(permuted)

  expect_identical(reordered, baseline)
  expect_identical(
    reordered$source$datasets_sha256,
    baseline$source$datasets_sha256
  )
})

test_that("unplannable catalog rows are counted and excluded", {
  catalog <- fetch_plan_test_catalog()
  unplannable_variable <- catalog$datasets$variable_id[[8L]]
  plan <- fetch_plan_test_build(catalog)

  expect_identical(plan$source$dataset_rows, 8L)
  expect_identical(plan$metadata$counts$catalog_rows, 8L)
  expect_identical(plan$metadata$counts$unplannable_rows, 1L)
  expect_false(unplannable_variable %in% plan$parameters$variable_id)
  expect_false(anyNA(plan$distributions$dataset_id))
  expect_false(anyNA(plan$distributions$distribution_id))
  expect_false(anyNA(plan$distributions$distribution_url))
})

test_that("conflicting descriptions of one distribution fail closed", {
  catalog <- fetch_plan_test_catalog()
  alpha <- which(
    catalog$datasets$distribution_id ==
      fetch_plan_test_hash("distribution:alpha")
  )
  expect_length(alpha, 2L)

  mutations <- list(
    provider_name = function(x) {
      x$datasets$provider_name[[alpha[[2L]]]] <- "Conflicting Provider"
      x
    },
    media_type = function(x) {
      x$datasets$media_type[[alpha[[2L]]]] <- "application/csv"
      x
    },
    temporal_end = function(x) {
      x$datasets$temporal_end[[alpha[[2L]]]] <- as.POSIXct(
        "2025-11-30 23:59:59", tz = "UTC"
      )
      x
    }
  )
  for (name in names(mutations)) {
    conflicting <- mutations[[name]](fetch_plan_test_clone(catalog))
    expect_identical(
      gx_catalog_validate_impl(conflicting),
      invisible(conflicting),
      info = name
    )
    expect_error(
      fetch_plan_test_build(conflicting),
      class = "gx_error_fetch_plan_conflict",
      info = name
    )
  }
})

test_that("handler membership and classifier agreement are independently bound", {
  catalog <- fetch_plan_test_catalog()
  alpha <- which(
    catalog$datasets$distribution_id ==
      fetch_plan_test_hash("distribution:alpha")
  )

  unknown_id <- fetch_plan_test_clone(catalog)
  unknown_id$datasets$handler_id[alpha] <- "not_registered"
  expect_identical(
    gx_catalog_validate_impl(unknown_id),
    invisible(unknown_id)
  )
  expect_error(
    fetch_plan_test_build(unknown_id),
    class = "gx_error_fetch_plan_handler"
  )

  mismatch <- fetch_plan_test_clone(catalog)
  mismatch$datasets$handler_id[alpha] <- "wqp"
  expect_identical(gx_catalog_validate_impl(mismatch), invisible(mismatch))
  expect_error(
    fetch_plan_test_build(mismatch),
    class = "gx_error_fetch_plan_handler"
  )
})

test_that("syntactically private targets are rejected without DNS", {
  catalog <- fetch_plan_test_catalog()
  alpha <- which(
    catalog$datasets$distribution_id ==
      fetch_plan_test_hash("distribution:alpha")
  )
  dns_calls <- 0L
  withr::local_options(list(
    geoconnexr.dns_resolver = function(...) {
      dns_calls <<- dns_calls + 1L
      stop("DNS must not run while constructing a fetch plan.", call. = FALSE)
    }
  ))
  poisoned <- list(
    fetchable = function(x) {
      x$datasets$distribution_url[alpha] <- "http://127.0.0.1/data.csv"
      x
    },
    reference_only = function(x) {
      row <- which(x$datasets$handler_id == "unknown" &
        !is.na(x$datasets$distribution_url))
      x$datasets$distribution_url[row] <- "http://127.0.0.1/reference.bin"
      x
    }
  )
  for (name in names(poisoned)) {
    unsafe <- poisoned[[name]](fetch_plan_test_clone(catalog))
    expect_identical(
      gx_catalog_validate_impl(unsafe),
      invisible(unsafe),
      info = name
    )
    expect_error(
      fetch_plan_test_build(unsafe),
      class = "gx_error_fetch_plan_security",
      info = name
    )
  }
  expect_identical(dns_calls, 0L)
})

test_that("source incompleteness is visible and non-replayable", {
  catalog <- fetch_plan_test_catalog(status = "partial", truncated = TRUE)
  plan <- fetch_plan_test_build(catalog)

  expect_identical(plan$source$datasets_status, "partial")
  expect_true(plan$source$datasets_truncated)
  expect_false(plan$metadata$execution_ready)
  expect_identical(plan$metadata$non_replayable_reasons, c(
    "handler_implementations_planned", "request_plans_absent",
    "source_catalog_incomplete"
  ))
  expect_identical(gx_fetch_plan_validate_impl(plan), invisible(plan))
})

test_that("time, budget, clock, and catalog inputs fail with typed errors", {
  catalog <- fetch_plan_test_catalog()
  open <- list(
    start = as.POSIXct(NA, tz = "UTC"),
    end = as.POSIXct(NA, tz = "UTC")
  )
  expect_identical(fetch_plan_test_build(catalog, time = open)$time, open)

  bad_times <- list(
    scalar = fetch_plan_test_time()[[1L]],
    reversed = rev(fetch_plan_test_time()),
    non_utc = structure(fetch_plan_test_time(), tzone = "America/New_York"),
    character = c("2025-06-01", "2025-06-30")
  )
  for (name in names(bad_times)) {
    expect_error(
      fetch_plan_test_build(catalog, time = bad_times[[name]]),
      class = "gx_error_fetch_plan_time",
      info = name
    )
  }

  bad_budgets <- list(
    max_datasets = list(max_datasets = -1L),
    fractional_datasets = list(max_datasets = 1.5),
    max_requests = list(max_requests = Inf),
    fractional_requests = list(max_requests = 1.5),
    encoded = list(max_encoded_bytes = -1),
    decoded = list(max_decoded_bytes = NA_real_)
  )
  for (name in names(bad_budgets)) {
    args <- c(
      list(
        catalog = catalog,
        time = fetch_plan_test_time(),
        now = fetch_plan_test_now
      ),
      bad_budgets[[name]]
    )
    expect_error(
      do.call(gx_fetch_plan_impl, args),
      class = "gx_error_fetch_plan_budget",
      info = name
    )
  }

  invalid_catalog <- fetch_plan_test_clone(catalog)
  invalid_catalog$contract_version <- "9.9.9"
  expect_error(
    fetch_plan_test_build(invalid_catalog),
    class = "gx_error_fetch_plan_input"
  )
  expect_error(
    gx_fetch_plan_impl(catalog, now = as.POSIXct("2026-01-01", tz = "UTC")),
    class = "gx_error_fetch_plan_input"
  )
  expect_error(
    gx_fetch_plan_impl(catalog, now = function() as.POSIXct(c(
      "2026-01-01", "2026-01-02"
    ), tz = "UTC")),
    class = "gx_error_fetch_plan_input"
  )
  expect_error(
    gx_fetch_plan_impl(catalog, now = function() {
      as.POSIXct("2026-01-01 00:00:00", tz = "America/New_York")
    }),
    class = "gx_error_fetch_plan_input"
  )
})

test_that("validator rejects forged plan components and reconciliation", {
  plan <- fetch_plan_test_build()
  selected <- which(plan$distributions$selected)[[1L]]
  mutations <- list(
    extra_root = function(x) {
      x$unexpected <- TRUE
      x
    },
    wrong_class = function(x) {
      class(x) <- c("gx_fetch_plan", "list")
      x
    },
    source_rows = function(x) {
      x$source$dataset_rows <- x$source$dataset_rows + 1L
      x
    },
    time_zone = function(x) {
      attr(x$time$start, "tzone") <- "America/New_York"
      x
    },
    distribution_subclass = function(x) {
      class(x$distributions) <- c("forged", class(x$distributions))
      x
    },
    parameter_subclass = function(x) {
      class(x$parameters) <- c("forged", class(x$parameters))
      x
    },
    handler_subclass = function(x) {
      class(x$handlers) <- c("forged", class(x$handlers))
      x
    },
    distribution_decision = function(x) {
      x$distributions$decision[[selected]] <- "fetched"
      x
    },
    private_distribution_url = function(x) {
      x$distributions$distribution_url[[selected]] <-
        "http://127.0.0.1/data.csv"
      x
    },
    selected_reconciliation = function(x) {
      x$distributions$selected[[selected]] <- FALSE
      x
    },
    variable_count = function(x) {
      x$distributions$variable_count[[selected]] <- 999L
      x
    },
    parameter_foreign_key = function(x) {
      x$parameters$distribution_id[[1L]] <- strrep("0", 64L)
      x
    },
    parameter_order = function(x) {
      x$parameters$parameter_order[[1L]] <- 99L
      x
    },
    handler_replayable = function(x) {
      x$handlers$replayable[[1L]] <- TRUE
      x
    },
    requests_present = function(x) {
      x$requests <- list(list(method = "GET"))
      x
    },
    invalid_budget = function(x) {
      x$budgets$max_requests <- -1L
      x
    },
    wrong_count = function(x) {
      x$metadata$counts$selected <- x$metadata$counts$selected + 1L
      x
    },
    execution_ready = function(x) {
      x$metadata$execution_ready <- TRUE
      x
    },
    missing_reason = function(x) {
      x$metadata$non_replayable_reasons <- "request_plans_absent"
      x
    }
  )

  for (name in names(mutations)) {
    forged <- mutations[[name]](fetch_plan_test_clone(plan))
    expect_error(
      gx_fetch_plan_validate_impl(forged),
      class = "gx_error_fetch_plan",
      info = name
    )
  }
})

test_that("validator rebinds distribution handlers to classifier facts", {
  plan <- fetch_plan_test_build()
  row <- which(plan$distributions$handler_id == "csv")[[1L]]
  plan$distributions$handler_id[[row]] <- "ogc_api_features"

  expect_true("ogc_api_features" %in% plan$handlers$handler_id)
  expect_identical(
    plan$handlers$outcome[
      match("ogc_api_features", plan$handlers$handler_id)
    ],
    "fetch"
  )
  expect_error(
    gx_fetch_plan_validate_impl(plan),
    class = "gx_error_fetch_plan_handler"
  )
})

test_that("plan printing and validation errors do not disclose query secrets", {
  catalog <- fetch_plan_test_catalog()
  secret <- "m7a-super-secret-token"
  alpha <- which(
    catalog$datasets$distribution_id ==
      fetch_plan_test_hash("distribution:alpha")
  )
  catalog$datasets$distribution_url[alpha] <- paste0(
    "https://example.org/data/alpha.csv?token=", secret
  )
  expect_identical(gx_catalog_validate_impl(catalog), invisible(catalog))
  plan <- fetch_plan_test_build(catalog)

  printed <- character()
  withCallingHandlers(
    print(plan),
    message = function(cnd) {
      printed <<- c(printed, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  )
  expect_false(any(grepl(secret, printed, fixed = TRUE)))

  forged <- fetch_plan_test_clone(plan)
  forged$distributions$decision[[1L]] <- "fetched"
  condition <- tryCatch(
    gx_fetch_plan_validate_impl(forged),
    error = identity
  )
  expect_s3_class(condition, "gx_error_fetch_plan")
  expect_false(grepl(secret, conditionMessage(condition), fixed = TRUE))
  trace_text <- paste(capture.output(str(condition$trace)), collapse = "\n")
  expect_false(grepl(secret, trace_text, fixed = TRUE))
})

test_that("selection-only planning performs no network or package execution", {
  catalog <- fetch_plan_test_catalog()
  network_calls <- 0L
  dns_calls <- 0L
  suggests <- c("dataRetrieval", "edr4r", "readr")
  loaded_before <- suggests %in% loadedNamespaces()
  withr::local_options(list(
    geoconnexr.dns_resolver = function(...) {
      dns_calls <<- dns_calls + 1L
      stop("Unexpected DNS resolution.", call. = FALSE)
    }
  ))

  plan <- testthat::with_mocked_bindings(
    fetch_plan_test_build(catalog),
    gx_default_performer = function(...) {
      network_calls <<- network_calls + 1L
      stop("Unexpected HTTP execution.", call. = FALSE)
    },
    gx_default_file_performer = function(...) {
      network_calls <<- network_calls + 1L
      stop("Unexpected download execution.", call. = FALSE)
    },
    .package = "geoconnexr"
  )

  expect_identical(network_calls, 0L)
  expect_identical(dns_calls, 0L)
  expect_identical(suggests %in% loadedNamespaces(), loaded_before)
  expect_identical(plan$requests, list())
  expect_true(all(plan$handlers$availability %in% c(
    "planned", "classifier_only"
  )))
  expect_true(all(!plan$handlers$replayable))
  expect_false(plan$metadata$execution_ready)
})

test_that("the M7a fetch-plan contract remains internal", {
  internal <- c(
    "gx_fetch_plan_impl", "gx_fetch_plan_validate_impl",
    "gx_fetch_plan_empty_distributions", "gx_fetch_plan_empty_parameters",
    "gx_fetch_plan_empty_handlers"
  )
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(internal %in% exports))
  expect_false("gx_fetch_plan" %in% exports)
  expect_false("gx_fetch" %in% exports)
})
