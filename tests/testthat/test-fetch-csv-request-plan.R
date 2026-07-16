test_that("empty M7d CSV request plans have the exact inert contract", {
  intent_set <- csv_request_plan_test_empty_intent_set()
  intent_bytes <- serialize(intent_set, NULL)
  result <- csv_request_plan_test_build(
    intent_set,
    max_response_bytes = 8L,
    max_rows = 10L,
    max_columns = 4L
  )

  expect_identical(class(result), "gx_csv_request_plan")
  expect_identical(names(result), c(
    "contract_version", "intent_set", "policy", "budgets", "reservations",
    "request_plans", "coverage", "metadata"
  ))
  expect_identical(result$contract_version, "0.1.0")
  expect_identical(serialize(intent_set, NULL), intent_bytes)
  expect_identical(serialize(result$intent_set, NULL), intent_bytes)
  expect_identical(result$intent_set$plan$requests, list())

  expect_identical(names(result$policy), c(
    "slice_id", "method", "accept", "accept_encoding", "body_bytes",
    "body_sha256", "credential_policy", "redirect_policy", "max_redirects",
    "retry_policy", "max_retries", "max_physical_attempts", "cache_policy",
    "success_status", "response_media_types", "response_content_encoding",
    "allocation_policy", "max_response_bytes", "max_rows", "max_columns",
    "parser_policy"
  ))
  expect_identical(result$policy, list(
    slice_id = "direct_csv_request_plan_v1",
    method = "GET",
    accept = "text/csv, application/csv;q=0.9",
    accept_encoding = "identity",
    body_bytes = 0L,
    body_sha256 = paste0(
      "e3b0c44298fc1c149afbf4c8996fb924",
      "27ae41e4649b934ca495991b7852b855"
    ),
    credential_policy = "source_url_opaque_no_additional_credentials",
    redirect_policy = "reject",
    max_redirects = 0L,
    retry_policy = "none",
    max_retries = 0L,
    max_physical_attempts = 1L,
    cache_policy = "bypass",
    success_status = 200L,
    response_media_types = c("text/csv", "application/csv"),
    response_content_encoding = "identity",
    allocation_policy = "global_selected_fair_share_v1",
    max_response_bytes = 8L,
    max_rows = 10L,
    max_columns = 4L,
    parser_policy = "shape_limits_only"
  ))
  expect_identical(result$policy$accept, intent_set$policy$accept)
  expect_identical(
    result$policy$accept_encoding, intent_set$policy$accept_encoding
  )
  expect_identical(result$policy$body_bytes, intent_set$policy$body_bytes)
  expect_identical(result$policy$body_sha256, intent_set$policy$body_sha256)

  expect_identical(names(result$budgets), c(
    "source_budgets", "reserved_requests", "reserved_encoded_bytes",
    "reserved_decoded_bytes", "remaining_requests",
    "remaining_encoded_bytes", "remaining_decoded_bytes"
  ))
  expect_identical(result$budgets, list(
    source_budgets = intent_set$plan$budgets,
    reserved_requests = 0L,
    reserved_encoded_bytes = 0,
    reserved_decoded_bytes = 0,
    remaining_requests = 7L,
    remaining_encoded_bytes = 70,
    remaining_decoded_bytes = 90
  ))

  expect_identical(class(result$reservations), c(
    "tbl_df", "tbl", "data.frame"
  ))
  expect_identical(names(result$reservations), c(
    "contract_version", "reservation_order", "reservation_id",
    "distribution_id", "fetch_order", "handler_id",
    "max_physical_attempts", "max_encoded_bytes", "max_decoded_bytes",
    "reservation_status"
  ))
  expect_identical(nrow(result$reservations), 0L)
  expect_type(result$reservations$reservation_order, "integer")
  expect_type(result$reservations$fetch_order, "integer")
  expect_type(result$reservations$max_physical_attempts, "integer")
  expect_type(result$reservations$max_encoded_bytes, "double")
  expect_type(result$reservations$max_decoded_bytes, "double")

  expect_identical(class(result$request_plans), c(
    "tbl_df", "tbl", "data.frame"
  ))
  expect_identical(names(result$request_plans), c(
    "contract_version", "request_order", "logical_request_id", "intent_id",
    "reservation_id", "distribution_id", "fetch_order", "handler_id",
    "method", "canonical_url_redacted", "declared_media_type",
    "max_physical_attempts", "max_encoded_bytes", "max_decoded_bytes",
    "response_byte_limit", "max_rows", "max_columns", "request_status"
  ))
  expect_identical(nrow(result$request_plans), 0L)
  expect_type(result$request_plans$request_order, "integer")
  expect_type(result$request_plans$fetch_order, "integer")
  expect_type(result$request_plans$max_physical_attempts, "integer")
  expect_type(result$request_plans$max_encoded_bytes, "double")
  expect_type(result$request_plans$max_decoded_bytes, "double")
  expect_type(result$request_plans$response_byte_limit, "double")
  expect_type(result$request_plans$max_rows, "integer")
  expect_type(result$request_plans$max_columns, "integer")

  expect_identical(class(result$coverage), c(
    "tbl_df", "tbl", "data.frame"
  ))
  expect_identical(names(result$coverage), c(
    "contract_version", "selection_order", "fetch_order", "distribution_id",
    "handler_id", "selected", "plan_decision", "intent_id",
    "reservation_id", "logical_request_id", "request_status"
  ))
  expect_identical(nrow(result$coverage), 0L)
  expect_type(result$coverage$selection_order, "integer")
  expect_type(result$coverage$fetch_order, "integer")
  expect_type(result$coverage$selected, "logical")

  expect_identical(names(result$metadata), c(
    "host_specific", "replayable", "execution_ready", "transport_authorized",
    "budgets_allocated", "budgets_consumed", "allocation_complete", "counts",
    "non_replayable_reasons"
  ))
  expect_false(result$metadata$host_specific)
  expect_false(result$metadata$replayable)
  expect_false(result$metadata$execution_ready)
  expect_false(result$metadata$transport_authorized)
  expect_true(result$metadata$budgets_allocated)
  expect_false(result$metadata$budgets_consumed)
  expect_true(result$metadata$allocation_complete)
  expect_identical(names(result$metadata$counts), c(
    "distributions", "selected", "intents", "reservations", "request_plans",
    "csv_request_planned", "csv_budget_deferred", "handler_reserved",
    "handler_budget_deferred", "not_selected", "reference_only",
    "physical_attempts_reserved", "requests_executed",
    "physical_attempts_executed"
  ))
  expect_identical(result$metadata$counts, list(
    distributions = 0L,
    selected = 0L,
    intents = 0L,
    reservations = 0L,
    request_plans = 0L,
    csv_request_planned = 0L,
    csv_budget_deferred = 0L,
    handler_reserved = 0L,
    handler_budget_deferred = 0L,
    not_selected = 0L,
    reference_only = 0L,
    physical_attempts_reserved = 0L,
    requests_executed = 0L,
    physical_attempts_executed = 0L
  ))
  expect_type(result$metadata$non_replayable_reasons, "character")
  expect_gt(length(result$metadata$non_replayable_reasons), 0L)
  expect_identical(
    gx_csv_request_plan_validate_impl(result), invisible(result)
  )
})

test_that("M7d reserves selected rows globally and plans only CSV requests", {
  intent_set <- csv_request_plan_test_intent_set(
    max_requests = 5L,
    max_encoded_bytes = 500,
    max_decoded_bytes = 500
  )
  intent_bytes <- serialize(intent_set, NULL)
  result <- csv_request_plan_test_build(
    intent_set,
    max_response_bytes = 100L,
    max_rows = 10000L,
    max_columns = 100L
  )
  selected <- which(intent_set$plan$distributions$selected)

  expect_identical(serialize(intent_set, NULL), intent_bytes)
  expect_identical(serialize(result$intent_set, NULL), intent_bytes)
  expect_identical(result$reservations$reservation_order, 1:5)
  expect_identical(
    result$reservations$distribution_id,
    intent_set$plan$distributions$distribution_id[selected]
  )
  expect_identical(result$reservations$fetch_order, 1:5)
  expect_identical(result$reservations$handler_id, c(
    "csv", "csv", "csv", "wqp", "ogc_api_features"
  ))
  expect_identical(result$reservations$max_physical_attempts, rep(1L, 5L))
  expect_identical(result$reservations$max_encoded_bytes, rep(100, 5L))
  expect_identical(result$reservations$max_decoded_bytes, rep(100, 5L))
  expect_identical(result$reservations$reservation_status, c(
    rep("csv_request_planned", 3L), rep("held_deferred_handler", 2L)
  ))
  expect_true(all(grepl(
    "^[a-f0-9]{64}$", result$reservations$reservation_id
  )))
  expect_identical(anyDuplicated(result$reservations$reservation_id), 0L)

  expect_identical(result$request_plans$request_order, 1:3)
  expect_identical(
    result$request_plans$intent_id, intent_set$intents$intent_id
  )
  expect_identical(
    result$request_plans$reservation_id,
    result$reservations$reservation_id[1:3]
  )
  expect_identical(
    result$request_plans$distribution_id,
    intent_set$intents$distribution_id
  )
  expect_identical(result$request_plans$fetch_order, 1:3)
  expect_identical(result$request_plans$handler_id, rep("csv", 3L))
  expect_identical(result$request_plans$method, rep("GET", 3L))
  expect_identical(
    result$request_plans$canonical_url_redacted,
    intent_set$intents$canonical_url_redacted
  )
  expect_identical(
    result$request_plans$declared_media_type,
    intent_set$intents$declared_media_type
  )
  expect_identical(
    result$request_plans$max_physical_attempts, rep(1L, 3L)
  )
  expect_identical(result$request_plans$max_encoded_bytes, rep(100, 3L))
  expect_identical(result$request_plans$max_decoded_bytes, rep(100, 3L))
  expect_identical(result$request_plans$response_byte_limit, rep(100, 3L))
  expect_identical(result$request_plans$max_rows, rep(10000L, 3L))
  expect_identical(result$request_plans$max_columns, rep(100L, 3L))
  expect_identical(
    result$request_plans$request_status,
    rep("planned_non_executable", 3L)
  )
  expect_true(all(grepl(
    "^[a-f0-9]{64}$", result$request_plans$logical_request_id
  )))
  expect_identical(anyDuplicated(result$request_plans$logical_request_id), 0L)
  expect_identical(result$reservations$reservation_id, c(
    "dc887bce38b526fc7a7c213cd040e95b531c38da02a70f6a1d40f84c58cbb900",
    "90328966be67685039aef315c91693889978bea12774aaf1bb9a4bb340a6df2e",
    "f0cadb1282d9a9925e6f9d35e872e88659968f98b6813124903b9ba676a31513",
    "ed5dd4871e76ee9d496468b5c60bea9da8e6b2aac340c25516e1585766d52130",
    "c31c8b005438a50bdb60a25bfedc8a6be3a36661314ae5878297cbd9187fa15e"
  ))
  expect_identical(result$request_plans$logical_request_id, c(
    "0a520972796e6f0559aca2765a741bb6ebe02504975b33e605447920c9493d4f",
    "7f58d41d3768d44b7d88a9971b1244b8e6b91939f5a1e57849607534e5c59b4e",
    "9c118af6d33890c338c33d28c0d47cc3ea03bbbf33ed7fdd0d4af967f9c50190"
  ))

  expect_identical(result$coverage$request_status, c(
    rep("csv_request_planned", 3L), rep("handler_reserved", 2L),
    rep("not_selected", 3L)
  ))
  expect_identical(
    result$coverage$reservation_id[1:5],
    result$reservations$reservation_id
  )
  expect_true(all(is.na(result$coverage$reservation_id[6:8])))
  expect_identical(
    result$coverage$logical_request_id[1:3],
    result$request_plans$logical_request_id
  )
  expect_true(all(is.na(result$coverage$logical_request_id[4:8])))
  expect_identical(result$budgets, list(
    source_budgets = intent_set$plan$budgets,
    reserved_requests = 5L,
    reserved_encoded_bytes = 500,
    reserved_decoded_bytes = 500,
    remaining_requests = 0L,
    remaining_encoded_bytes = 0,
    remaining_decoded_bytes = 0
  ))
  expect_identical(result$metadata$counts, list(
    distributions = 8L,
    selected = 5L,
    intents = 3L,
    reservations = 5L,
    request_plans = 3L,
    csv_request_planned = 3L,
    csv_budget_deferred = 0L,
    handler_reserved = 2L,
    handler_budget_deferred = 0L,
    not_selected = 3L,
    reference_only = 0L,
    physical_attempts_reserved = 5L,
    requests_executed = 0L,
    physical_attempts_executed = 0L
  ))
  expect_false(result$metadata$execution_ready)
  expect_false(result$metadata$transport_authorized)
  expect_false(result$metadata$budgets_consumed)
  expect_identical(result$metadata$non_replayable_reasons, c(
    "arbitrary_provider_client_unimplemented",
    "attempt_identity_unbound",
    "attempt_ledger_unbound",
    "csv_parser_enforcement_unimplemented",
    "csv_parser_semantics_unbound",
    "handler_implementations_planned",
    "non_csv_request_plans_absent",
    "provider_transport_unauthorized",
    "response_validator_unimplemented",
    "result_schema_unbound",
    "runtime_package_preflight_required",
    "serialization_unbound",
    "timeout_policy_unbound",
    "transport_adapter_unimplemented"
  ))
  expect_false(any(c(
    "cache_policy_unbound", "credential_policy_unbound",
    "parser_limits_unbound", "redirect_policy_unbound",
    "request_budgets_unallocated", "request_plans_absent",
    "response_contract_unproven"
  ) %in% result$metadata$non_replayable_reasons))
})

test_that("pinned M7d request-plan known answers rebind the M7c corpus", {
  fixture_dir <- csv_request_plan_test_fixture_dir()
  manifest_path <- file.path(fixture_dir, "manifest-v1.json")
  expected_path <- file.path(fixture_dir, "expected-v1.json")
  manifest_raw <- readBin(manifest_path, what = "raw", n = 2048L)
  expected_raw <- readBin(expected_path, what = "raw", n = 8192L)

  expect_identical(length(manifest_raw), 939L)
  expect_identical(
    digest::digest(manifest_raw, algo = "sha256", serialize = FALSE),
    "dd1a7b5bad781a428e259aba7606de28c849936ae3c192ea647b565dd05985a8"
  )
  expect_identical(length(expected_raw), 6457L)
  expect_identical(
    digest::digest(expected_raw, algo = "sha256", serialize = FALSE),
    "082f6b409d8287c679b315a32b59f2fe075b64d29a07e2ee5e008d096c12eb4f"
  )
  expect_identical(tail(manifest_raw, 1L), as.raw(0x0a))
  expect_identical(tail(expected_raw, 1L), as.raw(0x0a))

  manifest <- csv_request_plan_test_read_json("manifest-v1.json")
  expected <- csv_request_plan_test_read_json("expected-v1.json")
  expect_identical(manifest$manifest_version, "1.0.0")
  expect_identical(manifest$contract, "gx_csv_request_plan")
  expect_identical(manifest$contract_version, "0.1.0")
  expect_identical(manifest$files[[1L]]$path, "expected-v1.json")
  expect_identical(manifest$files[[1L]]$bytes, 6457L)
  expect_identical(
    manifest$files[[1L]]$sha256,
    digest::digest(expected_raw, algo = "sha256", serialize = FALSE)
  )
  dependency_dir <- file.path(fixture_dir, "..", "csv-intent")
  dependency_names <- c("cases-v1.json", "expected-v1.json")
  dependency_bytes <- c(8237L, 3201L)
  dependency_hashes <- c(
    "d444ffc823e05117a027d9ffdbd76f0b6cf29a0168bb496df6d3336d8c2e2a0a",
    "4029d2debe024af0005e744fb9e7703a30dc20f64aa972cb6b18751c938083cd"
  )
  for (position in seq_along(dependency_names)) {
    raw <- readBin(
      file.path(dependency_dir, dependency_names[[position]]),
      what = "raw",
      n = 16384L
    )
    expect_identical(length(raw), dependency_bytes[[position]])
    expect_identical(
      digest::digest(raw, algo = "sha256", serialize = FALSE),
      dependency_hashes[[position]]
    )
    expect_identical(
      manifest$source_dependencies[[position]]$sha256,
      dependency_hashes[[position]]
    )
  }

  result <- csv_request_plan_test_build()
  expect_identical(result$contract_version, expected$contract_version)
  integer_policy <- c(
    "body_bytes", "max_redirects", "max_retries", "max_physical_attempts",
    "success_status", "max_response_bytes", "max_rows", "max_columns"
  )
  for (name in names(expected$policy)) {
    value <- expected$policy[[name]]
    if (name %in% integer_policy) value <- as.integer(value)
    if (identical(name, "response_media_types")) {
      value <- as.character(unlist(value, use.names = FALSE))
    }
    expect_identical(result$policy[[name]], value, info = name)
  }
  expect_identical(
    result$reservations$reservation_id,
    as.character(unlist(
      expected$reservations$reservation_id, use.names = FALSE
    ))
  )
  expect_identical(
    result$request_plans$logical_request_id,
    as.character(unlist(
      expected$request_plans$logical_request_id, use.names = FALSE
    ))
  )
  expect_identical(
    result$request_plans$canonical_url_redacted,
    as.character(unlist(
      expected$request_plans$canonical_url_redacted, use.names = FALSE
    ))
  )
  nullable <- function(values) {
    vapply(
      values,
      function(value) if (is.null(value)) NA_character_ else value,
      character(1)
    )
  }
  expect_identical(
    result$coverage$intent_id, nullable(expected$coverage$intent_id)
  )
  expect_identical(
    result$coverage$reservation_id,
    nullable(expected$coverage$reservation_id)
  )
  expect_identical(
    result$coverage$logical_request_id,
    nullable(expected$coverage$logical_request_id)
  )
  expect_identical(
    result$coverage$request_status,
    as.character(unlist(
      expected$coverage$request_status, use.names = FALSE
    ))
  )
  expect_identical(result$metadata$counts, lapply(expected$counts, as.integer))
  expect_identical(
    result$metadata$non_replayable_reasons,
    as.character(unlist(
      expected$non_replayable_reasons, use.names = FALSE
    ))
  )
})

test_that("M7d fair-shares encoded and decoded budgets independently", {
  intent_set <- csv_request_plan_test_intent_set(
    max_requests = 5L,
    max_encoded_bytes = 17,
    max_decoded_bytes = 13
  )
  result <- csv_request_plan_test_build(
    intent_set,
    max_response_bytes = 4L,
    max_rows = 25L,
    max_columns = 6L
  )

  expect_identical(result$reservations$max_encoded_bytes, c(4, 4, 3, 3, 3))
  expect_identical(result$reservations$max_decoded_bytes, c(3, 3, 3, 2, 2))
  expect_identical(
    result$reservations$max_encoded_bytes,
    csv_request_plan_test_fair_partition(17, 5L, 4L)
  )
  expect_identical(
    result$reservations$max_decoded_bytes,
    csv_request_plan_test_fair_partition(13, 5L, 4L)
  )
  expect_identical(result$request_plans$max_encoded_bytes, c(4, 4, 3))
  expect_identical(result$request_plans$max_decoded_bytes, c(3, 3, 3))
  expect_identical(result$request_plans$response_byte_limit, c(3, 3, 3))
  expect_identical(result$budgets$reserved_requests, 5L)
  expect_identical(result$budgets$reserved_encoded_bytes, 17)
  expect_identical(result$budgets$reserved_decoded_bytes, 13)
  expect_identical(result$budgets$remaining_requests, 0L)
  expect_identical(result$budgets$remaining_encoded_bytes, 0)
  expect_identical(result$budgets$remaining_decoded_bytes, 0)
})

test_that("M7d allocation is prefix-bounded by every source budget", {
  request_tight <- csv_request_plan_test_build(
    csv_request_plan_test_intent_set(
      max_requests = 3L,
      max_encoded_bytes = 100,
      max_decoded_bytes = 100
    ),
    max_response_bytes = 10L,
    max_rows = 20L,
    max_columns = 5L
  )
  expect_identical(request_tight$reservations$fetch_order, 1:3)
  expect_identical(request_tight$coverage$request_status, c(
    rep("csv_request_planned", 3L), rep("handler_budget_deferred", 2L),
    rep("not_selected", 3L)
  ))
  expect_identical(request_tight$budgets$reserved_requests, 3L)
  expect_identical(request_tight$budgets$remaining_requests, 0L)
  expect_identical(request_tight$budgets$reserved_encoded_bytes, 30)
  expect_identical(request_tight$budgets$remaining_encoded_bytes, 70)
  expect_identical(request_tight$budgets$reserved_decoded_bytes, 30)
  expect_identical(request_tight$budgets$remaining_decoded_bytes, 70)

  encoded_tight <- csv_request_plan_test_build(
    csv_request_plan_test_intent_set(
      max_requests = 5L,
      max_encoded_bytes = 2,
      max_decoded_bytes = 50
    ),
    max_response_bytes = 10L,
    max_rows = 20L,
    max_columns = 5L
  )
  expect_identical(encoded_tight$reservations$fetch_order, 1:2)
  expect_identical(encoded_tight$reservations$max_encoded_bytes, c(1, 1))
  expect_identical(encoded_tight$reservations$max_decoded_bytes, c(10, 10))
  expect_identical(encoded_tight$request_plans$response_byte_limit, c(1, 1))
  expect_identical(encoded_tight$budgets$remaining_requests, 3L)
  expect_identical(encoded_tight$budgets$remaining_encoded_bytes, 0)
  expect_identical(encoded_tight$budgets$remaining_decoded_bytes, 30)
  expect_identical(encoded_tight$coverage$request_status, c(
    rep("csv_request_planned", 2L), "csv_budget_deferred",
    rep("handler_budget_deferred", 2L), rep("not_selected", 3L)
  ))

  for (zero_name in c(
    "max_requests", "max_encoded_bytes", "max_decoded_bytes"
  )) {
    args <- list(
      max_requests = 5L,
      max_encoded_bytes = 50,
      max_decoded_bytes = 50
    )
    args[[zero_name]] <- if (identical(zero_name, "max_requests")) 0L else 0
    intent_set <- do.call(csv_request_plan_test_intent_set, args)
    zero <- csv_request_plan_test_build(
      intent_set,
      max_response_bytes = 10L,
      max_rows = 20L,
      max_columns = 5L
    )
    expect_identical(nrow(zero$reservations), 0L, info = zero_name)
    expect_identical(nrow(zero$request_plans), 0L, info = zero_name)
    expect_identical(zero$coverage$request_status, c(
      rep("csv_budget_deferred", 3L),
      rep("handler_budget_deferred", 2L), rep("not_selected", 3L)
    ), info = zero_name)
    expect_identical(zero$metadata$counts$reservations, 0L, info = zero_name)
    expect_identical(
      zero$metadata$counts$physical_attempts_reserved, 0L, info = zero_name
    )
  }
})

test_that("M7d global reservations remain handler-neutral", {
  catalog <- fetch_preflight_test_catalog(c(
    "wqp", "csv", "ogc_api_features", "unknown"
  ))
  intent_set <- csv_request_plan_test_intent_set(
    catalog = catalog,
    max_datasets = 3L,
    max_requests = 3L,
    max_encoded_bytes = 30,
    max_decoded_bytes = 30
  )
  result <- csv_request_plan_test_build(
    intent_set,
    max_response_bytes = 10L,
    max_rows = 20L,
    max_columns = 5L
  )

  expect_identical(result$reservations$fetch_order, 1:3)
  expect_identical(result$reservations$handler_id, c(
    "wqp", "csv", "ogc_api_features"
  ))
  expect_identical(result$reservations$reservation_status, c(
    "held_deferred_handler", "csv_request_planned",
    "held_deferred_handler"
  ))
  expect_identical(result$request_plans$request_order, 1L)
  expect_identical(result$request_plans$fetch_order, 2L)
  expect_identical(
    result$request_plans$reservation_id,
    result$reservations$reservation_id[[2L]]
  )
  expect_identical(result$coverage$request_status, c(
    "handler_reserved", "csv_request_planned", "handler_reserved",
    "reference_only"
  ))
  expect_identical(result$metadata$counts$handler_reserved, 2L)
  expect_identical(result$metadata$counts$csv_request_planned, 1L)
  expect_identical(result$metadata$counts$reference_only, 1L)
  expect_identical(result$metadata$counts$physical_attempts_reserved, 3L)
})

test_that("M7d rejects absent or invalid explicit shape limits", {
  intent_set <- csv_request_plan_test_intent_set()
  invalid <- list(
    NULL, 0L, -1L, NA_integer_, NaN, Inf, 1.5, c(1L, 2L), "10", TRUE
  )
  fields <- c("max_response_bytes", "max_rows", "max_columns")
  valid <- list(
    intent_set = intent_set,
    max_response_bytes = 100L,
    max_rows = 10000L,
    max_columns = 100L
  )

  for (field in fields) {
    for (case_index in seq_along(invalid)) {
      args <- valid
      args[field] <- invalid[case_index]
      condition <- csv_request_plan_test_error(do.call(
        gx_csv_request_plan_impl, args
      ))
      expect_s3_class(condition, "gx_error_csv_request_plan")
    }
  }

  for (field in fields) {
    args <- valid[setdiff(names(valid), field)]
    condition <- csv_request_plan_test_error(do.call(
      gx_csv_request_plan_impl, args
    ))
    expect_s3_class(condition, "gx_error_csv_request_plan_budget")
  }

  condition <- csv_request_plan_test_error(gx_csv_request_plan_impl(
    intent_set$plan,
    max_response_bytes = 100L,
    max_rows = 10000L,
    max_columns = 100L
  ))
  expect_s3_class(condition, "gx_error_csv_request_plan_input")

  forged <- csv_request_plan_test_clone(intent_set)
  forged$metadata$budgets_allocated <- TRUE
  condition <- csv_request_plan_test_error(gx_csv_request_plan_impl(
    forged,
    max_response_bytes = 100L,
    max_rows = 10000L,
    max_columns = 100L
  ))
  expect_s3_class(condition, "gx_error")
})

test_that("M7d allocation is deterministic under catalog row permutations", {
  catalog <- csv_intents_test_fixture_catalog()
  permuted <- csv_request_plan_test_clone(catalog)
  permuted$datasets <- tibble::as_tibble(
    permuted$datasets[rev(seq_len(nrow(permuted$datasets))), , drop = FALSE]
  )
  expect_identical(gx_catalog_validate_impl(permuted), invisible(permuted))

  first_intents <- csv_request_plan_test_intent_set(
    catalog,
    max_requests = 5L,
    max_encoded_bytes = 17,
    max_decoded_bytes = 13
  )
  second_intents <- csv_request_plan_test_intent_set(
    permuted,
    max_requests = 5L,
    max_encoded_bytes = 17,
    max_decoded_bytes = 13
  )
  first <- csv_request_plan_test_build(
    first_intents, max_response_bytes = 4L, max_rows = 25L, max_columns = 6L
  )
  second <- csv_request_plan_test_build(
    second_intents, max_response_bytes = 4L, max_rows = 25L, max_columns = 6L
  )

  expect_identical(first_intents, second_intents)
  expect_identical(first, second)
  expect_identical(
    serialize(csv_request_plan_test_build(
      first_intents,
      max_response_bytes = 4L,
      max_rows = 25L,
      max_columns = 6L
    ), NULL),
    serialize(first, NULL)
  )
})

test_that("M7d byte-bound identities ignore numeric display options", {
  intent_set <- csv_request_plan_test_intent_set(
    max_requests = 5L,
    max_encoded_bytes = 1e10,
    max_decoded_bytes = 1e10
  )
  compact <- withr::with_options(
    list(scipen = 0),
    csv_request_plan_test_build(
      intent_set,
      max_response_bytes = 2000000000L,
      max_rows = 10000L,
      max_columns = 100L
    )
  )
  expanded <- withr::with_options(
    list(scipen = 999),
    csv_request_plan_test_build(
      intent_set,
      max_response_bytes = 2000000000L,
      max_rows = 10000L,
      max_columns = 100L
    )
  )

  expect_identical(compact, expanded)
  expect_identical(
    compact$reservations$reservation_id,
    expanded$reservations$reservation_id
  )
  expect_identical(
    compact$request_plans$logical_request_id,
    expanded$request_plans$logical_request_id
  )
  expect_identical(
    gx_csv_request_plan_byte_hash_value(.gx_fetch_plan_max_safe_integer),
    "9007199254740991"
  )
})

test_that("M7d request identities ignore fragments but bind query values", {
  catalog <- csv_intents_test_fixture_catalog()
  baseline_intents <- csv_request_plan_test_intent_set(catalog)
  baseline <- csv_request_plan_test_build(baseline_intents)
  first_id <- baseline_intents$plan$distributions$distribution_id[[1L]]
  first_request <- match(
    first_id, baseline$request_plans$distribution_id
  )
  original_url <- unique(
    catalog$datasets$distribution_url[
      catalog$datasets$distribution_id == first_id
    ]
  )
  expect_length(original_url, 1L)
  expect_true(endsWith(original_url, "#preview"))
  expect_identical(
    baseline$request_plans$canonical_url_redacted[[first_request]],
    "https://data.example.org/observations.csv?[redacted]"
  )
  expect_false(grepl(
    "fixture-token",
    baseline$request_plans$canonical_url_redacted[[first_request]],
    fixed = TRUE
  ))

  fragment_catalog <- csv_request_plan_test_replace_distribution_url(
    catalog,
    first_id,
    sub("#preview", "#alternate-preview", original_url, fixed = TRUE)
  )
  fragment <- csv_request_plan_test_build(
    csv_request_plan_test_intent_set(fragment_catalog)
  )
  fragment_request <- match(
    first_id, fragment$request_plans$distribution_id
  )
  expect_identical(
    fragment$request_plans$logical_request_id[[fragment_request]],
    baseline$request_plans$logical_request_id[[first_request]]
  )
  expect_identical(
    fragment$request_plans$canonical_url_redacted[[fragment_request]],
    baseline$request_plans$canonical_url_redacted[[first_request]]
  )

  secret <- "m7d-do-not-disclose-this-query-value"
  query_catalog <- csv_request_plan_test_replace_distribution_url(
    catalog,
    first_id,
    paste0(
      "https://data.example.org/observations.csv?station=00123&token=",
      secret, "#preview"
    )
  )
  query <- csv_request_plan_test_build(
    csv_request_plan_test_intent_set(query_catalog)
  )
  query_request <- match(first_id, query$request_plans$distribution_id)
  expect_false(identical(
    query$request_plans$logical_request_id[[query_request]],
    baseline$request_plans$logical_request_id[[first_request]]
  ))
  expect_identical(
    query$request_plans$canonical_url_redacted[[query_request]],
    "https://data.example.org/observations.csv?[redacted]"
  )
  expect_false(any(grepl(
    secret, unlist(query$request_plans, use.names = FALSE), fixed = TRUE
  )))

  printed <- character()
  output <- capture.output(withCallingHandlers(
    print(query),
    message = function(cnd) {
      printed <<- c(printed, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  ))
  expect_false(any(grepl(secret, c(printed, output), fixed = TRUE)))

  forged <- csv_request_plan_test_clone(query)
  forged$coverage$request_status[[1L]] <- "executed"
  condition <- csv_request_plan_test_error(
    gx_csv_request_plan_validate_impl(forged)
  )
  expect_s3_class(condition, "gx_error_csv_request_plan")
  expect_false(grepl(secret, conditionMessage(condition), fixed = TRUE))
  trace_text <- paste(capture.output(str(condition$trace)), collapse = "\n")
  expect_false(grepl(secret, trace_text, fixed = TRUE))
})

test_that("M7d validation fails closed for forged allocations and links", {
  result <- csv_request_plan_test_build()
  mutations <- list(
    top_level_class = function(x) {
      class(x) <- c("gx_csv_request_plan", "list")
      x
    },
    top_level_attribute = function(x) {
      attr(x, "forged") <- TRUE
      x
    },
    contract_version = function(x) {
      x$contract_version <- "9.9.9"
      x
    },
    embedded_intent = function(x) {
      x$intent_set$intents$intent_status[[1L]] <- "executed"
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
    policy_redirect = function(x) {
      x$policy$redirect_policy <- "follow"
      x
    },
    policy_attempts = function(x) {
      x$policy$max_physical_attempts <- 2L
      x
    },
    policy_response_media = function(x) {
      x$policy$response_media_types <- rev(x$policy$response_media_types)
      x
    },
    policy_shape_limit = function(x) {
      x$policy$max_rows <- x$policy$max_rows + 1L
      x
    },
    source_budget = function(x) {
      x$budgets$source_budgets$max_requests <-
        x$budgets$source_budgets$max_requests + 1L
      x
    },
    reserved_requests = function(x) {
      x$budgets$reserved_requests <- x$budgets$reserved_requests - 1L
      x
    },
    reserved_bytes = function(x) {
      x$budgets$reserved_encoded_bytes <-
        x$budgets$reserved_encoded_bytes - 1
      x
    },
    remaining_budget = function(x) {
      x$budgets$remaining_decoded_bytes <-
        x$budgets$remaining_decoded_bytes + 1
      x
    },
    reservation_id = function(x) {
      x$reservations$reservation_id[[1L]] <- strrep("0", 64L)
      x
    },
    reservation_order = function(x) {
      x$reservations$reservation_order[[1L]] <- 2L
      x
    },
    reservation_distribution = function(x) {
      x$reservations$distribution_id[[1L]] <- strrep("f", 64L)
      x
    },
    reservation_handler = function(x) {
      x$reservations$handler_id[[1L]] <- "wqp"
      x
    },
    reservation_attempts = function(x) {
      x$reservations$max_physical_attempts[[1L]] <- 2L
      x
    },
    reservation_bytes = function(x) {
      x$reservations$max_encoded_bytes[[1L]] <- 1
      x
    },
    reservation_status = function(x) {
      x$reservations$reservation_status[[1L]] <- "consumed"
      x
    },
    reservation_column_attribute = function(x) {
      names(x$reservations$reservation_id) <-
        seq_along(x$reservations$reservation_id)
      x
    },
    logical_request_id = function(x) {
      x$request_plans$logical_request_id[[1L]] <- strrep("0", 64L)
      x
    },
    request_intent_fk = function(x) {
      x$request_plans$intent_id[[1L]] <- strrep("f", 64L)
      x
    },
    request_reservation_fk = function(x) {
      x$request_plans$reservation_id[[1L]] <- strrep("f", 64L)
      x
    },
    request_method = function(x) {
      x$request_plans$method[[1L]] <- "POST"
      x
    },
    request_url = function(x) {
      x$request_plans$canonical_url_redacted[[1L]] <-
        "https://data.example.org/file.csv?token=exposed"
      x
    },
    request_byte_limit = function(x) {
      x$request_plans$response_byte_limit[[1L]] <-
        x$request_plans$response_byte_limit[[1L]] + 1
      x
    },
    request_row_limit = function(x) {
      x$request_plans$max_rows[[1L]] <- x$request_plans$max_rows[[1L]] + 1L
      x
    },
    request_status = function(x) {
      x$request_plans$request_status[[1L]] <- "executed"
      x
    },
    coverage_status = function(x) {
      x$coverage$request_status[[1L]] <- "executed"
      x
    },
    coverage_intent_fk = function(x) {
      x$coverage$intent_id[[1L]] <- strrep("f", 64L)
      x
    },
    coverage_reservation_fk = function(x) {
      x$coverage$reservation_id[[1L]] <- strrep("f", 64L)
      x
    },
    coverage_request_fk = function(x) {
      x$coverage$logical_request_id[[1L]] <- strrep("f", 64L)
      x
    },
    coverage_selection = function(x) {
      x$coverage$selected[[1L]] <- FALSE
      x
    },
    count = function(x) {
      x$metadata$counts$reservations <- 0L
      x
    },
    authority = function(x) {
      x$metadata$transport_authorized <- TRUE
      x
    },
    allocation_flag = function(x) {
      x$metadata$budgets_allocated <- FALSE
      x
    },
    consumption_flag = function(x) {
      x$metadata$budgets_consumed <- TRUE
      x
    },
    completion_flag = function(x) {
      x$metadata$allocation_complete <- FALSE
      x
    },
    reasons = function(x) {
      x$metadata$non_replayable_reasons <- c(
        x$metadata$non_replayable_reasons, "forged_reason"
      )
      x
    }
  )

  for (name in names(mutations)) {
    forged <- mutations[[name]](csv_request_plan_test_clone(result))
    condition <- csv_request_plan_test_error(
      gx_csv_request_plan_validate_impl(forged)
    )
    expect_s3_class(condition, "gx_error")
    expect_true(
      inherits(condition, c(
        "gx_error_csv_request_plan", "gx_error_csv_get_intents",
        "gx_error_fetch_plan"
      )),
      info = name
    )
  }
})

test_that("M7d performs no transport, DNS, cache, parser, package, or write work", {
  intent_set <- csv_request_plan_test_intent_set()
  intent_bytes <- serialize(intent_set, NULL)
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
    csv_request_plan_test_build(intent_set),
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
  expect_identical(serialize(intent_set, NULL), intent_bytes)
  expect_identical(serialize(result$intent_set, NULL), intent_bytes)
  expect_identical(
    sort(list.files(tempdir(), all.files = TRUE, no.. = TRUE, recursive = TRUE)),
    temp_before
  )
  expect_false(result$metadata$execution_ready)
  expect_false(result$metadata$transport_authorized)
  expect_false(result$metadata$budgets_consumed)
  expect_identical(result$metadata$counts$requests_executed, 0L)
  expect_identical(result$metadata$counts$physical_attempts_executed, 0L)

  namespace <- getNamespace("geoconnexr")
  internal <- ls(namespace, pattern = "^gx_csv_request_plan_")
  source_text <- paste(unlist(lapply(internal, function(name) {
    value <- get(name, envir = namespace, inherits = FALSE)
    if (is.function(value)) deparse(body(value)) else character()
  }), use.names = FALSE), collapse = "\n")
  forbidden <- paste0(
    "\\b(gx_http_request|gx_default_performer|gx_cache_backend|",
    "requireNamespace|loadNamespace|read_csv|read[.]csv|writeLines|",
    "writeBin)\\s*\\("
  )
  expect_false(grepl(forbidden, source_text, perl = TRUE))
})

test_that("M7d owns a separate aggregate text budget", {
  result <- csv_request_plan_test_build()
  intent_total <- gx_fetch_plan_text_total(result$intent_set)
  wrapped_total <- gx_fetch_plan_text_total(result)
  temporary_intent_limit <- as.integer(
    intent_total + max(1, floor((wrapped_total - intent_total) / 2))
  )
  expect_lt(intent_total, temporary_intent_limit)
  expect_gt(wrapped_total, temporary_intent_limit)

  validated <- testthat::with_mocked_bindings(
    gx_csv_request_plan_validate_impl(result),
    .gx_csv_get_intents_max_text_bytes = temporary_intent_limit,
    .package = "geoconnexr"
  )
  expect_identical(validated, invisible(result))

  condition <- testthat::with_mocked_bindings(
    csv_request_plan_test_error(gx_csv_request_plan_validate_impl(result)),
    .gx_csv_request_plan_max_text_bytes = 1L,
    .package = "geoconnexr"
  )
  expect_s3_class(condition, "gx_error_csv_request_plan_budget")
})

test_that("M7d preserves incomplete-source blockers", {
  intent_set <- csv_request_plan_test_intent_set(
    catalog = fetch_plan_test_catalog(status = "partial", truncated = TRUE),
    max_datasets = 2L,
    max_requests = 2L,
    max_encoded_bytes = 200,
    max_decoded_bytes = 200
  )
  result <- csv_request_plan_test_build(intent_set)

  expect_identical(result$metadata$non_replayable_reasons, c(
    "arbitrary_provider_client_unimplemented",
    "attempt_identity_unbound",
    "attempt_ledger_unbound",
    "csv_parser_enforcement_unimplemented",
    "csv_parser_semantics_unbound",
    "handler_implementations_planned",
    "non_csv_request_plans_absent",
    "provider_transport_unauthorized",
    "response_validator_unimplemented",
    "result_schema_unbound",
    "runtime_package_preflight_required",
    "serialization_unbound",
    "source_catalog_incomplete",
    "timeout_policy_unbound",
    "transport_adapter_unimplemented"
  ))
  expect_false(result$metadata$replayable)
  expect_false(result$metadata$execution_ready)
  expect_false(result$metadata$transport_authorized)
})

test_that("the M7d direct-CSV request-plan contract remains internal", {
  internal <- c(
    "gx_csv_request_plan_impl", "gx_csv_request_plan_validate_impl"
  )
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(internal %in% exports))
  expect_false("gx_csv_request_plan" %in% exports)
  expect_false("gx_fetch" %in% exports)
  expect_false("gx_fetch_request" %in% exports)
})
