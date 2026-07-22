test_that("empty M7c direct-CSV intents have the exact inert contract", {
  plan <- fetch_plan_test_build(
    catalog = fetch_plan_test_catalog(populated = FALSE),
    time = NULL,
    max_requests = 0L,
    max_encoded_bytes = 0,
    max_decoded_bytes = 0
  )
  plan_bytes <- serialize(plan, NULL)
  result <- gx_csv_get_intents_impl(plan)

  expect_identical(class(result), "gx_csv_get_intents")
  expect_identical(names(result), c(
    "contract_version", "plan", "policy", "intents", "coverage", "metadata"
  ))
  expect_identical(result$contract_version, "0.1.0")
  expect_identical(serialize(plan, NULL), plan_bytes)
  expect_identical(serialize(result$plan, NULL), plan_bytes)
  expect_identical(result$plan$requests, list())
  expect_identical(result$plan$metadata$counts$requests, 0L)

  expect_identical(names(result$policy), c(
    "slice_id", "method", "accept", "accept_encoding", "body_bytes",
    "body_sha256", "credential_policy", "redirect_policy", "cache_policy",
    "parser_policy"
  ))
  expect_identical(result$policy, list(
    slice_id = "direct_csv_get_v1",
    method = "GET",
    accept = "text/csv, application/csv;q=0.9",
    accept_encoding = "identity",
    body_bytes = 0L,
    body_sha256 = paste0(
      "e3b0c44298fc1c149afbf4c8996fb924",
      "27ae41e4649b934ca495991b7852b855"
    ),
    credential_policy = "unbound",
    redirect_policy = "unbound",
    cache_policy = "unbound",
    parser_policy = "unbound"
  ))
  expect_identical(
    result$policy$body_sha256,
    digest::digest(raw(), algo = "sha256", serialize = FALSE)
  )

  expect_identical(class(result$intents), c(
    "tbl_df", "tbl", "data.frame"
  ))
  expect_identical(names(result$intents), c(
    "contract_version", "intent_order", "intent_id", "distribution_id",
    "fetch_order", "handler_id", "declared_media_type",
    "canonical_url_redacted", "intent_status"
  ))
  expect_identical(nrow(result$intents), 0L)
  expect_type(result$intents$intent_order, "integer")
  expect_type(result$intents$fetch_order, "integer")

  expect_identical(class(result$coverage), c(
    "tbl_df", "tbl", "data.frame"
  ))
  expect_identical(names(result$coverage), c(
    "contract_version", "selection_order", "fetch_order", "distribution_id",
    "handler_id", "selected", "plan_decision", "intent_id", "intent_status"
  ))
  expect_identical(nrow(result$coverage), 0L)
  expect_type(result$coverage$selection_order, "integer")
  expect_type(result$coverage$fetch_order, "integer")
  expect_type(result$coverage$selected, "logical")

  expect_identical(names(result$metadata), c(
    "host_specific", "replayable", "execution_ready", "transport_authorized",
    "budgets_allocated", "counts", "non_replayable_reasons"
  ))
  expect_false(result$metadata$host_specific)
  expect_false(result$metadata$replayable)
  expect_false(result$metadata$execution_ready)
  expect_false(result$metadata$transport_authorized)
  expect_false(result$metadata$budgets_allocated)
  expect_identical(result$metadata$counts, list(
    distributions = 0L,
    selected = 0L,
    intents = 0L,
    intent_created = 0L,
    deferred_handler = 0L,
    not_selected = 0L,
    reference_only = 0L,
    requests = 0L
  ))
  expect_identical(result$metadata$non_replayable_reasons, c(
    "attempt_ledger_unbound", "cache_policy_unbound",
    "credential_policy_unbound",
    "handler_implementations_planned",
    "parser_limits_unbound", "provider_transport_unauthorized",
    "redirect_policy_unbound", "request_budgets_unallocated",
    "request_plans_absent", "response_contract_unproven"
  ))
  expect_identical(
    gx_csv_get_intents_validate_impl(result), invisible(result)
  )
})

test_that("selected CSV distributions map one-to-one to inert intents", {
  plan <- fetch_plan_test_build()
  plan_bytes <- serialize(plan, NULL)
  result <- csv_intents_test_build(plan)
  selected_csv <- which(
    plan$distributions$selected & plan$distributions$handler_id == "csv"
  )

  expect_identical(serialize(plan, NULL), plan_bytes)
  expect_identical(serialize(result$plan, NULL), plan_bytes)
  expect_identical(result$intents$intent_order, seq_along(selected_csv))
  expect_identical(
    result$intents$distribution_id,
    plan$distributions$distribution_id[selected_csv]
  )
  expect_identical(
    result$intents$fetch_order,
    plan$distributions$fetch_order[selected_csv]
  )
  expect_true(all(result$intents$handler_id == "csv"))
  expect_true(all(result$intents$intent_status == "inert"))
  expect_true(all(grepl("^[a-f0-9]{64}$", result$intents$intent_id)))
  expect_identical(anyDuplicated(result$intents$intent_id), 0L)
  expect_identical(
    result$intents$declared_media_type,
    plan$distributions$media_type[selected_csv]
  )
  expect_identical(
    result$intents$canonical_url_redacted,
    unname(vapply(
      plan$distributions$distribution_url[selected_csv],
      function(url) gx_redact_url(gx_safe_target(
        url, resolve_dns = FALSE
      )$url),
      character(1)
    ))
  )

  expected_status <- ifelse(
    plan$distributions$decision == "reference_only",
    "reference_only",
    ifelse(
      !plan$distributions$selected,
      "not_selected",
      ifelse(
        plan$distributions$handler_id == "csv",
        "intent_created",
        "deferred_handler"
      )
    )
  )
  expect_identical(result$coverage$intent_status, unname(expected_status))
  expect_identical(
    result$coverage$distribution_id, plan$distributions$distribution_id
  )
  expect_identical(result$coverage$plan_decision, plan$distributions$decision)
  expect_identical(result$coverage$fetch_order, plan$distributions$fetch_order)
  expect_identical(
    result$coverage$intent_id[!is.na(result$coverage$intent_id)],
    result$intents$intent_id
  )
  expect_identical(result$metadata$counts, list(
    distributions = 6L,
    selected = 2L,
    intents = 2L,
    intent_created = 2L,
    deferred_handler = 0L,
    not_selected = 3L,
    reference_only = 1L,
    requests = 0L
  ))
  expect_identical(result$plan$budgets, plan$budgets)
  expect_false(result$metadata$budgets_allocated)
  expect_false(result$metadata$transport_authorized)
})

test_that("pinned CSV descriptors prove classifier, coverage, and intent facts", {
  fixture_dir <- csv_intents_test_fixture_dir()
  manifest_path <- file.path(fixture_dir, "manifest-v1.json")
  cases_path <- file.path(fixture_dir, "cases-v1.json")
  expected_path <- file.path(fixture_dir, "expected-v1.json")
  manifest_raw <- readBin(manifest_path, what = "raw", n = 1024L)
  cases_raw <- readBin(cases_path, what = "raw", n = 16384L)
  expected_raw <- readBin(expected_path, what = "raw", n = 8192L)

  expect_identical(length(manifest_raw), 773L)
  expect_identical(
    digest::digest(manifest_raw, algo = "sha256", serialize = FALSE),
    "a440e0603bb968abd853d90ef81a62e98b94a43d79b22c8e56bd153fa27a2320"
  )
  expect_identical(length(cases_raw), 8237L)
  expect_identical(
    digest::digest(cases_raw, algo = "sha256", serialize = FALSE),
    "d444ffc823e05117a027d9ffdbd76f0b6cf29a0168bb496df6d3336d8c2e2a0a"
  )
  expect_identical(length(expected_raw), 3201L)
  expect_identical(
    digest::digest(expected_raw, algo = "sha256", serialize = FALSE),
    "4029d2debe024af0005e744fb9e7703a30dc20f64aa972cb6b18751c938083cd"
  )
  expect_identical(tail(manifest_raw, 1L), as.raw(0x0a))
  expect_identical(tail(cases_raw, 1L), as.raw(0x0a))
  expect_identical(tail(expected_raw, 1L), as.raw(0x0a))
  expect_false(identical(head(cases_raw, 3L), as.raw(c(0xef, 0xbb, 0xbf))))

  manifest <- csv_intents_test_read_json("manifest-v1.json")
  fixture <- csv_intents_test_read_json("cases-v1.json")
  expected <- csv_intents_test_read_json("expected-v1.json")
  expect_identical(manifest$manifest_version, "1.0.0")
  expect_identical(manifest$files[[1L]]$path, "cases-v1.json")
  expect_identical(manifest$files[[1L]]$bytes, 8237L)
  expect_identical(
    manifest$files[[1L]]$sha256,
    digest::digest(cases_raw, algo = "sha256", serialize = FALSE)
  )
  expect_identical(manifest$files[[2L]]$path, "expected-v1.json")
  expect_identical(manifest$files[[2L]]$bytes, 3201L)
  expect_identical(
    manifest$files[[2L]]$sha256,
    digest::digest(expected_raw, algo = "sha256", serialize = FALSE)
  )
  expect_length(fixture$cases, 8L)

  for (case in fixture$cases) {
    descriptor <- case$descriptor
    classified <- gx_classify_distribution(
      descriptor$url,
      media_type = descriptor$media_type,
      conforms_to = as.character(unlist(
        descriptor$conforms_to, use.names = FALSE
      ))
    )
    expect_identical(
      classified, case$expectation$classifier, info = case$case_id
    )
    target <- gx_safe_target(descriptor$url, resolve_dns = FALSE)$url
    expect_identical(
      target,
      case$expectation$canonical_transport_url,
      info = case$case_id
    )
  }

  plan <- csv_intents_test_fixture_plan()
  result <- gx_csv_get_intents_impl(plan)
  expect_identical(plan$distributions$selection_order, 1:8)
  expect_identical(plan$distributions$fetch_order, c(1:5, NA_integer_, NA_integer_, NA_integer_))
  expect_identical(plan$distributions$selected, c(
    TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE
  ))
  expect_identical(plan$distributions$decision, c(
    rep("selected_unplanned", 5L), "outside_time", "not_fetchable",
    "skipped_max_datasets"
  ))
  expect_identical(plan$distributions$variable_count, c(
    1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L
  ))
  expect_identical(result$intents$fetch_order, 1:3)
  expect_identical(result$intents$declared_media_type, c(
    "text/csv", "text/csv", "application/octet-stream"
  ))
  expect_identical(result$coverage$intent_status, c(
    "intent_created", "intent_created", "intent_created",
    "deferred_handler", "deferred_handler",
    "not_selected", "not_selected", "not_selected"
  ))
  expect_identical(result$metadata$counts$intents, 3L)
  expect_identical(result$metadata$counts$deferred_handler, 2L)
  expect_identical(result$metadata$counts$requests, 0L)
  expect_identical(result$contract_version, expected$contract_version)
  for (name in names(expected$policy)) {
    expected_value <- expected$policy[[name]]
    if (identical(name, "body_bytes")) {
      expected_value <- as.integer(expected_value)
    }
    expect_identical(result$policy[[name]], expected_value, info = name)
  }
  for (name in names(expected$intents)) {
    expected_value <- unname(unlist(
      expected$intents[[name]], use.names = FALSE
    ))
    if (name %in% c("intent_order", "fetch_order")) {
      expected_value <- as.integer(expected_value)
    }
    expect_identical(result$intents[[name]], expected_value, info = name)
  }
  expect_identical(
    result$coverage$distribution_id,
    as.character(unlist(
      expected$coverage$distribution_id, use.names = FALSE
    ))
  )
  expected_coverage_intents <- vapply(
    expected$coverage$intent_id,
    function(value) if (is.null(value)) NA_character_ else value,
    character(1)
  )
  expect_identical(result$coverage$intent_id, expected_coverage_intents)
  expect_identical(
    result$coverage$intent_status,
    as.character(unlist(
      expected$coverage$intent_status, use.names = FALSE
    ))
  )
  expect_identical(
    result$metadata$counts,
    lapply(expected$counts, as.integer)
  )
  expect_identical(
    result$metadata$non_replayable_reasons,
    as.character(unlist(
      expected$non_replayable_reasons, use.names = FALSE
    ))
  )
  expect_true(endsWith(
    plan$distributions$distribution_url[[1L]], "#preview"
  ))
  expect_true(endsWith(
    result$intents$canonical_url_redacted[[1L]], "?[redacted]"
  ))
  expect_false(grepl(
    "fixture-token",
    result$intents$canonical_url_redacted[[1L]],
    fixed = TRUE
  ))

  changed_fragment_catalog <- csv_intents_test_fixture_catalog()
  first_distribution <- which(
    changed_fragment_catalog$datasets$distribution_id ==
      plan$distributions$distribution_id[[1L]]
  )
  changed_fragment_catalog$datasets$distribution_url[first_distribution] <-
    sub(
      "#preview", "#alternate-preview",
      changed_fragment_catalog$datasets$distribution_url[first_distribution],
      fixed = TRUE
    )
  changed_fragment <- gx_csv_get_intents_impl(fetch_plan_test_build(
    changed_fragment_catalog,
    time = fetch_plan_test_time(),
    max_datasets = 5L,
    max_requests = 0L,
    max_encoded_bytes = 0,
    max_decoded_bytes = 0
  ))
  expect_identical(
    changed_fragment$intents$intent_id[[1L]],
    result$intents$intent_id[[1L]]
  )
})

test_that("M7c is deterministic under catalog row permutations", {
  catalog <- csv_intents_test_fixture_catalog()
  permuted <- csv_intents_test_clone(catalog)
  permuted$datasets <- tibble::as_tibble(
    permuted$datasets[rev(seq_len(nrow(permuted$datasets))), , drop = FALSE]
  )
  expect_identical(gx_catalog_validate_impl(permuted), invisible(permuted))

  first_plan <- fetch_plan_test_build(
    catalog,
    max_datasets = 5L,
    max_requests = 0L,
    max_encoded_bytes = 0,
    max_decoded_bytes = 0
  )
  second_plan <- fetch_plan_test_build(
    permuted,
    max_datasets = 5L,
    max_requests = 0L,
    max_encoded_bytes = 0,
    max_decoded_bytes = 0
  )
  first <- gx_csv_get_intents_impl(first_plan)
  second <- gx_csv_get_intents_impl(second_plan)

  expect_identical(first_plan, second_plan)
  expect_identical(first, second)
  expect_identical(
    serialize(gx_csv_get_intents_impl(first_plan), NULL),
    serialize(first, NULL)
  )
})

test_that("M7c does not allocate zero request or byte budgets", {
  plan <- csv_intents_test_fixture_plan()
  result <- gx_csv_get_intents_impl(plan)

  expect_identical(plan$budgets$max_requests, 0L)
  expect_identical(plan$budgets$max_encoded_bytes, 0)
  expect_identical(plan$budgets$max_decoded_bytes, 0)
  expect_identical(nrow(result$intents), 3L)
  expect_false(result$metadata$budgets_allocated)
  expect_identical(result$metadata$counts$requests, 0L)
  expect_false(any(grepl(
    "(?:limit|budget|encoded|decoded|row|column)",
    names(result$intents),
    perl = TRUE
  )))
})

test_that("M7b package state cannot authorize or alter M7c intents", {
  plan <- fetch_preflight_test_plan(c(
    "csv", "wqp", "ogc_api_features", "unknown"
  ))
  before <- gx_csv_get_intents_impl(plan)
  missing <- fetch_preflight_test_build(
    plan, versions = c(readr = NA_character_)
  )
  present <- fetch_preflight_test_build(
    plan, versions = c(readr = "2.1.6")
  )
  after <- gx_csv_get_intents_impl(plan)

  expect_identical(before, after)
  expect_identical(nrow(before$intents), 1L)
  expect_identical(before$coverage$intent_status, c(
    "intent_created", "deferred_handler", "deferred_handler", "reference_only"
  ))
  expect_identical(
    missing$distributions$preflight_status[[1L]], "skipped_missing_pkg"
  )
  expect_identical(
    present$distributions$preflight_status[[1L]],
    "blocked_implementation_planned"
  )
  expect_false(before$metadata$execution_ready)
  expect_false(before$metadata$transport_authorized)

  condition <- csv_intents_test_error(gx_csv_get_intents_impl(present))
  expect_s3_class(condition, "gx_error_csv_get_intents_input")
})

test_that("M7c validation fails closed for forged relationships and attributes", {
  result <- gx_csv_get_intents_impl(csv_intents_test_fixture_plan())
  mutations <- list(
    top_level_class = function(x) {
      class(x) <- c("gx_csv_get_intents", "list")
      x
    },
    top_level_attribute = function(x) {
      attr(x, "forged") <- TRUE
      x
    },
    policy_method = function(x) {
      x$policy$method <- "POST"
      x
    },
    policy_accept = function(x) {
      x$policy$accept <- "*/*"
      x
    },
    policy_body_hash = function(x) {
      x$policy$body_sha256 <- strrep("0", 64L)
      x
    },
    policy_redirect = function(x) {
      x$policy$redirect_policy <- "follow"
      x
    },
    intent_id = function(x) {
      x$intents$intent_id[[1L]] <- strrep("0", 64L)
      x
    },
    intent_order = function(x) {
      x$intents$intent_order[[1L]] <- 2L
      x
    },
    intent_handler = function(x) {
      x$intents$handler_id[[1L]] <- "wqp"
      x
    },
    intent_media_type = function(x) {
      x$intents$declared_media_type[[1L]] <- "application/csv"
      x
    },
    intent_redaction = function(x) {
      x$intents$canonical_url_redacted[[1L]] <-
        "https://data.example.org/observations.csv?token=exposed"
      x
    },
    intent_column_attribute = function(x) {
      names(x$intents$intent_id) <- seq_along(x$intents$intent_id)
      x
    },
    coverage_status = function(x) {
      x$coverage$intent_status[[1L]] <- "fetched"
      x
    },
    coverage_foreign_key = function(x) {
      x$coverage$intent_id[[1L]] <- strrep("f", 64L)
      x
    },
    coverage_selection = function(x) {
      x$coverage$selected[[1L]] <- FALSE
      x
    },
    count = function(x) {
      x$metadata$counts$intents <- 0L
      x
    },
    authority = function(x) {
      x$metadata$transport_authorized <- TRUE
      x
    },
    reasons = function(x) {
      x$metadata$non_replayable_reasons <-
        setdiff(x$metadata$non_replayable_reasons, "parser_limits_unbound")
      x
    },
    embedded_requests = function(x) {
      x$plan$requests <- list(list(method = "GET"))
      x
    },
    embedded_distribution = function(x) {
      x$plan$distributions$distribution_url[[1L]] <-
        "https://data.example.org/changed.csv"
      x
    }
  )

  for (name in names(mutations)) {
    forged <- mutations[[name]](csv_intents_test_clone(result))
    condition <- csv_intents_test_error(
      gx_csv_get_intents_validate_impl(forged)
    )
    expect_s3_class(condition, "gx_error")
    expect_true(
      inherits(condition, c(
        "gx_error_csv_get_intents", "gx_error_fetch_plan"
      )),
      info = name
    )
  }
})

test_that("M7c redacts query values from intent, print, error, and trace surfaces", {
  catalog <- fetch_plan_test_catalog()
  secret <- "m7c-do-not-disclose-this-query-value"
  alpha <- which(
    catalog$datasets$distribution_id ==
      fetch_plan_test_hash("distribution:alpha")
  )
  catalog$datasets$distribution_url[alpha] <- paste0(
    "https://example.org/data/alpha.csv?token=", secret
  )
  expect_identical(gx_catalog_validate_impl(catalog), invisible(catalog))
  plan <- fetch_plan_test_build(catalog)
  result <- gx_csv_get_intents_impl(plan)

  alpha_id <- fetch_plan_test_hash("distribution:alpha")
  intent_row <- match(alpha_id, result$intents$distribution_id)
  expect_identical(
    result$intents$canonical_url_redacted[[intent_row]],
    "https://example.org/data/alpha.csv?[redacted]"
  )
  intent_text <- paste(capture.output(str(result$intents)), collapse = "\n")
  expect_false(grepl(secret, intent_text, fixed = TRUE))

  printed <- character()
  output <- capture.output(withCallingHandlers(
    print(result),
    message = function(cnd) {
      printed <<- c(printed, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  ))
  expect_false(any(grepl(secret, c(printed, output), fixed = TRUE)))

  forged <- csv_intents_test_clone(result)
  forged$coverage$intent_status[[1L]] <- "executed"
  condition <- csv_intents_test_error(
    gx_csv_get_intents_validate_impl(forged)
  )
  expect_s3_class(condition, "gx_error_csv_get_intents")
  expect_false(grepl(secret, conditionMessage(condition), fixed = TRUE))
  trace_text <- paste(capture.output(str(condition$trace)), collapse = "\n")
  expect_false(grepl(secret, trace_text, fixed = TRUE))

  changed_catalog <- csv_intents_test_clone(catalog)
  changed_catalog$datasets$distribution_url[alpha] <- paste0(
    "https://example.org/data/alpha.csv?token=", secret, "-changed"
  )
  changed <- gx_csv_get_intents_impl(fetch_plan_test_build(changed_catalog))
  changed_row <- match(alpha_id, changed$intents$distribution_id)
  expect_false(identical(
    result$intents$intent_id[[intent_row]],
    changed$intents$intent_id[[changed_row]]
  ))
})

test_that("M7c target rebinding rejects credentials and nonpublic targets", {
  secret <- "m7c-userinfo-secret"
  unsafe <- c(
    paste0("https://user:", secret, "@example.org/data.csv"),
    "https://localhost/data.csv",
    "http://127.0.0.1/data.csv",
    "file:///tmp/data.csv"
  )
  for (url in unsafe) {
    condition <- csv_intents_test_error(
      gx_csv_get_intents_target_impl(url)
    )
    expect_s3_class(condition, "gx_error_csv_get_intents_url")
    expect_false(grepl(
      secret, conditionMessage(condition), fixed = TRUE
    ), info = url)
    trace_text <- paste(capture.output(str(condition$trace)), collapse = "\n")
    expect_false(grepl(secret, trace_text, fixed = TRUE), info = url)
  }
})

test_that("M7c performs no transport, DNS, cache, package, CSV parser, or write work", {
  plan <- csv_intents_test_fixture_plan()
  plan_bytes <- serialize(plan, NULL)
  optional <- c("dataRetrieval", "edr4r", "readr")
  loaded_before <- optional %in% loadedNamespaces()
  temp_before <- sort(list.files(
    tempdir(), all.files = TRUE, no.. = TRUE, recursive = TRUE
  ))
  network_calls <- 0L
  dns_calls <- 0L
  cache_calls <- 0L
  withr::local_options(list(
    geoconnexr.dns_resolver = function(...) {
      dns_calls <<- dns_calls + 1L
      stop("DNS forbidden", call. = FALSE)
    }
  ))

  result <- testthat::with_mocked_bindings(
    gx_csv_get_intents_impl(plan),
    gx_default_performer = function(...) {
      network_calls <<- network_calls + 1L
      stop("HTTP forbidden", call. = FALSE)
    },
    gx_default_file_performer = function(...) {
      network_calls <<- network_calls + 1L
      stop("download forbidden", call. = FALSE)
    },
    gx_default_dns_resolver = function(...) {
      dns_calls <<- dns_calls + 1L
      stop("DNS forbidden", call. = FALSE)
    },
    gx_cache_backend = function(...) {
      cache_calls <<- cache_calls + 1L
      stop("cache forbidden", call. = FALSE)
    },
    .package = "geoconnexr"
  )

  expect_identical(network_calls, 0L)
  expect_identical(dns_calls, 0L)
  expect_identical(cache_calls, 0L)
  expect_identical(optional %in% loadedNamespaces(), loaded_before)
  expect_identical(serialize(plan, NULL), plan_bytes)
  expect_identical(serialize(result$plan, NULL), plan_bytes)
  expect_identical(
    sort(list.files(tempdir(), all.files = TRUE, no.. = TRUE, recursive = TRUE)),
    temp_before
  )
  expect_false(result$metadata$execution_ready)
  expect_false(result$metadata$transport_authorized)
  source_text <- paste(c(
    deparse(body(gx_csv_get_intents_impl)),
    deparse(body(gx_csv_get_intents_intents_impl)),
    deparse(body(gx_csv_get_intents_validate_impl))
  ), collapse = "\n")
  forbidden <- paste0(
    "\\b(gx_http_request|gx_default_performer|gx_cache_backend|",
    "requireNamespace|loadNamespace|read_csv|read[.]csv|writeLines|",
    "writeBin)\\s*\\("
  )
  expect_false(grepl(forbidden, source_text, perl = TRUE))
})

test_that("M7c owns a separate aggregate text budget", {
  result <- gx_csv_get_intents_impl(csv_intents_test_fixture_plan())
  plan_total <- gx_fetch_plan_text_total(result$plan)
  wrapped_total <- gx_fetch_plan_text_total(result)
  temporary_plan_limit <- as.integer(
    plan_total + max(1, floor((wrapped_total - plan_total) / 2))
  )
  expect_lt(plan_total, temporary_plan_limit)
  expect_gt(wrapped_total, temporary_plan_limit)

  validated <- testthat::with_mocked_bindings(
    gx_csv_get_intents_validate_impl(result),
    .gx_fetch_plan_max_text_bytes = temporary_plan_limit,
    .package = "geoconnexr"
  )
  expect_identical(validated, invisible(result))

  condition <- testthat::with_mocked_bindings(
    csv_intents_test_error(gx_csv_get_intents_validate_impl(result)),
    .gx_csv_get_intents_max_text_bytes = 1L,
    .package = "geoconnexr"
  )
  expect_s3_class(condition, "gx_error_csv_get_intents_budget")
})

test_that("M7c preserves incomplete-source blockers", {
  plan <- fetch_plan_test_build(
    fetch_plan_test_catalog(status = "partial", truncated = TRUE)
  )
  result <- gx_csv_get_intents_impl(plan)

  expect_true("source_catalog_incomplete" %in%
    result$metadata$non_replayable_reasons)
  expect_identical(
    result$metadata$non_replayable_reasons,
    gx_csv_get_intents_reasons_impl(plan)
  )
  expect_false(result$metadata$replayable)
  expect_false(result$metadata$execution_ready)
})

test_that("the M7c direct-CSV intent contract remains internal", {
  internal <- c(
    "gx_csv_get_intents_impl", "gx_csv_get_intents_validate_impl",
    "gx_csv_get_intents_empty_intents", "gx_csv_get_intents_empty_coverage",
    "gx_csv_get_intents_policy_impl", "gx_csv_get_intents_id_impl"
  )
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(internal %in% exports))
  expect_false("gx_csv_get_intents" %in% exports)
  expect_true("gx_fetch" %in% exports)
  expect_false("gx_fetch_request" %in% exports)
})
