.gx_fetch_orchestration_contract_version <- "0.2.0"
.gx_fetch_orchestration_max_executions <- 32L
.gx_fetch_orchestration_max_total_bytes <- 64L * 1024L^2
.gx_fetch_orchestration_max_text_bytes <- 256L * 1024L^2

.gx_fetch_orchestration_fields <- c(
  "contract_version", "request_plan", "policy", "requests",
  "orchestration", "results", "status", "metadata"
)

.gx_fetch_orchestration_policy_fields <- c(
  "slice_id", "execution_mode", "dry_run", "scheduling_policy",
  "parallelism", "failure_policy", "max_executions", "max_total_bytes",
  "max_fields", "oaf_limit", "timeout_seconds", "min_interval_seconds",
  "cache_policy", "redirect_policy", "retry_policy"
)

.gx_fetch_orchestration_request_columns <- c(
  "contract_version", "request_order", "fetch_order", "distribution_id",
  "handler_id", "logical_request_id", "reservation_id",
  "response_byte_limit", "request_status"
)

.gx_fetch_orchestration_owned_fields <- c(
  "orchestration_scope_id", "orchestration_id", "execution_mode",
  "orchestration_status", "planned_requests", "admitted_requests",
  "completed_requests"
)

.gx_fetch_orchestration_status_columns <- c(
  "contract_version", "selection_order", "fetch_order", "distribution_id",
  "handler_id", "logical_request_id", "request_status",
  "orchestration_status", "attempted", "succeeded", "physical_attempts",
  "encoded_bytes", "decoded_bytes", "execution_id", "result_index",
  "error_code"
)

.gx_fetch_orchestration_oaf_result_fields <- c(
  "result_id", "response_body", "result", "implementation", "execution",
  "attempt"
)

.gx_fetch_orchestration_wqp_result_fields <- c(
  "result_id", "response_body", "data", "schema", "parse",
  "implementation", "execution", "attempt"
)

.gx_fetch_orchestration_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "execution_completed", "dry_run", "budgets_consumed",
  "provider_responses_observed", "response_candidates_validated",
  "csv_semantics_validated", "wqp_semantics_validated",
  "oaf_semantics_validated",
  "runtime_symbols_checked", "result_contract_bound", "status_reconciled",
  "continue_on_error", "results_compacted", "global_order_enforced",
  "observation_origin", "counts", "non_replayable_reasons"
)

.gx_fetch_orchestration_count_fields <- c(
  "distributions", "candidate_requests", "csv_requests", "wqp_requests",
  "oaf_requests",
  "admitted_requests", "batch_limit_deferred", "attempted_requests",
  "successful_requests", "failed_requests", "physical_attempts",
  "encoded_bytes", "decoded_bytes", "handler_deferred",
  "handler_plan_unsupported", "not_selected", "reference_only",
  "csv_results", "wqp_results", "oaf_results", "results"
)

gx_fetch_orchestration_abort <- function(
    message,
    class = "gx_error_fetch_orchestration_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_fetch_orchestration", "gx_error_fetch_plan"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_fetch_orchestration_input_plan_impl <- function(request_plan) {
  valid <- tryCatch({
    gx_csv_request_plan_validate_impl(request_plan)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid) {
    gx_fetch_orchestration_abort(
      "M7k construction requires one valid M7d all-handler request plan.",
      "gx_error_fetch_orchestration_input"
    )
  }
  request_plan
}

gx_fetch_orchestration_flag_impl <- function(x) {
  if (!is.logical(x) || length(x) != 1L || is.na(x) ||
      !is.null(attributes(x))) {
    gx_fetch_orchestration_abort(
      "The M7k dry-run choice must be one explicit logical value.",
      "gx_error_fetch_orchestration_policy"
    )
  }
  unname(x)
}

gx_fetch_orchestration_integer_impl <- function(x, maximum, message) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x) || x > maximum || !is.null(attributes(x))) {
    gx_fetch_orchestration_abort(
      message, "gx_error_fetch_orchestration_policy"
    )
  }
  unname(as.integer(x))
}

gx_fetch_orchestration_byte_limit_impl <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x) ||
      x > .gx_fetch_orchestration_max_total_bytes ||
      !is.null(attributes(x))) {
    gx_fetch_orchestration_abort(
      "The M7k aggregate response-byte ceiling must be an explicit bounded whole number.",
      "gx_error_fetch_orchestration_policy"
    )
  }
  unname(as.double(x))
}

gx_fetch_orchestration_policy_impl <- function(
    dry_run, max_executions, max_total_bytes, max_fields, oaf_limit, timeout,
    min_interval) {
  dry_run <- gx_fetch_orchestration_flag_impl(dry_run)
  max_executions <- gx_fetch_orchestration_integer_impl(
    max_executions,
    .gx_fetch_orchestration_max_executions,
    "The M7k execution ceiling must be an explicit bounded whole number."
  )
  max_total_bytes <- gx_fetch_orchestration_byte_limit_impl(max_total_bytes)
  max_fields <- tryCatch(
    gx_csv_parsed_response_field_limit_impl(max_fields),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  oaf_limit <- tryCatch(
    gx_oaf_limit_impl(oaf_limit),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  timeout <- tryCatch(
    gx_csv_execution_number_impl(timeout, "timeout", 0, 600, FALSE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  min_interval <- tryCatch(
    gx_csv_execution_number_impl(
      min_interval, "min_interval", 0, 3600, TRUE
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(max_fields) || is.null(oaf_limit) || is.null(timeout) ||
      is.null(min_interval)) {
    gx_fetch_orchestration_abort(
      "M7k parser, OGC page, and timing limits must be explicit bounded values.",
      "gx_error_fetch_orchestration_policy"
    )
  }
  list(
    slice_id = "cross_handler_orchestration_v2",
    execution_mode = if (dry_run) "dry_run" else "sequential_continue",
    dry_run = dry_run,
    scheduling_policy = "global_fetch_order",
    parallelism = 1L,
    failure_policy = "continue_unrelated_requests",
    max_executions = max_executions,
    max_total_bytes = max_total_bytes,
    max_fields = max_fields,
    oaf_limit = oaf_limit,
    timeout_seconds = timeout,
    min_interval_seconds = min_interval,
    cache_policy = "bypass",
    redirect_policy = "reject",
    retry_policy = "none"
  )
}

gx_fetch_orchestration_empty_requests_impl <- function() {
  tibble::tibble(
    contract_version = character(),
    request_order = integer(),
    fetch_order = integer(),
    distribution_id = character(),
    handler_id = character(),
    logical_request_id = character(),
    reservation_id = character(),
    response_byte_limit = double(),
    request_status = character()
  )
}

gx_fetch_orchestration_planning_impl <- function(request_plan, policy) {
  rows <- list()
  unsupported <- character()
  csv <- request_plan$request_plans
  if (nrow(csv)) {
    for (position in seq_len(nrow(csv))) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_fetch_orchestration_contract_version,
        request_order = 0L,
        fetch_order = csv$fetch_order[[position]],
        distribution_id = csv$distribution_id[[position]],
        handler_id = "csv",
        logical_request_id = csv$logical_request_id[[position]],
        reservation_id = csv$reservation_id[[position]],
        response_byte_limit = csv$response_byte_limit[[position]],
        request_status = "csv_request_planned"
      )
    }
  }
  coverage <- request_plan$coverage
  wqp_positions <- which(
    coverage$selected & coverage$handler_id == "wqp" &
      coverage$request_status == "handler_reserved"
  )
  if (length(wqp_positions)) {
    for (position in wqp_positions) {
      distribution_id <- coverage$distribution_id[[position]]
      wqp_plan <- tryCatch(
        gx_wqp_request_plan_impl(
          request_plan, distribution_id, max_fields = policy$max_fields
        ),
        error = identity
      )
      if (inherits(wqp_plan, c(
        "gx_error_wqp_plan_url", "gx_error_wqp_plan_time"
      ))) {
        unsupported <- c(unsupported, distribution_id)
        next
      }
      if (!inherits(wqp_plan, "gx_wqp_request_plan")) {
        gx_fetch_orchestration_abort(
          "M7k could not bind a WQP request to its held reservation.",
          "gx_error_fetch_orchestration_input"
        )
      }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_fetch_orchestration_contract_version,
        request_order = 0L,
        fetch_order = wqp_plan$request$fetch_order,
        distribution_id = wqp_plan$request$distribution_id,
        handler_id = "wqp",
        logical_request_id = wqp_plan$request$logical_request_id,
        reservation_id = wqp_plan$request$reservation_id,
        response_byte_limit = wqp_plan$request$response_byte_limit,
        request_status = "wqp_request_planned"
      )
    }
  }
  oaf_positions <- which(
    coverage$selected & coverage$handler_id == "ogc_api_features" &
      coverage$request_status == "handler_reserved"
  )
  if (length(oaf_positions)) {
    for (position in oaf_positions) {
      distribution_id <- coverage$distribution_id[[position]]
      oaf_plan <- tryCatch(
        gx_oaf_request_plan_impl(
          request_plan, distribution_id, limit = policy$oaf_limit
        ),
        error = identity
      )
      if (inherits(oaf_plan, "gx_error_oaf_plan_url")) {
        unsupported <- c(unsupported, distribution_id)
        next
      }
      if (!inherits(oaf_plan, "gx_oaf_request_plan")) {
        gx_fetch_orchestration_abort(
          "M7k could not bind an OGC request to its held reservation.",
          "gx_error_fetch_orchestration_input"
        )
      }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_fetch_orchestration_contract_version,
        request_order = 0L,
        fetch_order = oaf_plan$request$fetch_order,
        distribution_id = oaf_plan$request$distribution_id,
        handler_id = "ogc_api_features",
        logical_request_id = oaf_plan$request$logical_request_id,
        reservation_id = oaf_plan$request$reservation_id,
        response_byte_limit = oaf_plan$request$response_byte_limit,
        request_status = "oaf_request_planned"
      )
    }
  }
  requests <- if (length(rows)) {
    output <- do.call(rbind, rows)
    output <- output[order(output$fetch_order), , drop = FALSE]
    rownames(output) <- NULL
    output$request_order <- seq_len(nrow(output))
    tibble::as_tibble(output)
  } else {
    gx_fetch_orchestration_empty_requests_impl()
  }
  list(requests = requests, unsupported = unname(unsupported))
}

gx_fetch_orchestration_admission_impl <- function(requests, policy) {
  admitted <- rep.int(FALSE, nrow(requests))
  count <- 0L
  bytes <- 0
  if (!nrow(requests)) return(admitted)
  for (position in seq_len(nrow(requests))) {
    ceiling <- requests$response_byte_limit[[position]]
    fits_count <- count < policy$max_executions
    fits_bytes <- ceiling <= policy$max_total_bytes - bytes
    if (fits_count && fits_bytes) {
      admitted[[position]] <- TRUE
      count <- count + 1L
      bytes <- bytes + ceiling
    }
  }
  unname(admitted)
}

gx_fetch_orchestration_hash_pairs_impl <- function(prefix, values) {
  output <- vector("list", length(values) * 2L)
  if (!length(values)) return(output)
  for (position in seq_along(values)) {
    output[[position * 2L - 1L]] <- paste0(prefix, "_", position)
    output[[position * 2L]] <- values[[position]]
  }
  unname(output)
}

gx_fetch_orchestration_id_impl <- function(
    orchestration_scope_id, request_plan, policy, requests) {
  gx_contract_hash(
    c(
      list(
        "orchestration_scope_id", orchestration_scope_id,
        "request_count", nrow(requests),
        "coverage_count", nrow(request_plan$coverage),
        "policy_field_count", length(policy)
      ),
      gx_fetch_orchestration_hash_pairs_impl(
        "request_logical_id", requests$logical_request_id
      ),
      gx_fetch_orchestration_hash_pairs_impl(
        "request_handler", requests$handler_id
      ),
      gx_fetch_orchestration_hash_pairs_impl(
        "coverage_distribution_id", request_plan$coverage$distribution_id
      ),
      gx_fetch_orchestration_hash_pairs_impl(
        "coverage_request_status", request_plan$coverage$request_status
      ),
      gx_fetch_orchestration_hash_pairs_impl("policy", policy)
    ),
    namespace = "geoconnexr.fetch-orchestration.v2",
    contract_version = .gx_fetch_orchestration_contract_version
  )
}

gx_fetch_orchestration_child_scope_impl <- function(
    orchestration_id, request_order, handler_id, logical_request_id) {
  gx_contract_hash(
    list(
      "orchestration_id", orchestration_id,
      "request_order", request_order,
      "handler_id", handler_id,
      "logical_request_id", logical_request_id
    ),
    namespace = "geoconnexr.fetch-orchestration-child.v2",
    contract_version = .gx_fetch_orchestration_contract_version
  )
}

gx_fetch_orchestration_status_impl <- function(
    request_plan, policy, planning, admitted) {
  coverage <- request_plan$coverage
  request_position <- match(
    coverage$distribution_id, planning$requests$distribution_id
  )
  logical_request_id <- unname(coverage$logical_request_id)
  request_status <- unname(coverage$request_status)
  planned <- which(!is.na(request_position))
  if (length(planned)) {
    logical_request_id[planned] <- planning$requests$logical_request_id[
      request_position[planned]
    ]
    request_status[planned] <- planning$requests$request_status[
      request_position[planned]
    ]
  }
  statuses <- unname(vapply(seq_len(nrow(coverage)), function(position) {
    request_row <- request_position[[position]]
    if (!is.na(request_row)) {
      if (!admitted[[request_row]]) return("batch_limit_deferred")
      if (policy$dry_run) "dry_run_planned" else "execution_pending"
    } else if (coverage$distribution_id[[position]] %in%
        planning$unsupported) {
      "handler_plan_unsupported"
    } else {
      switch(
        coverage$request_status[[position]],
        csv_budget_deferred = "csv_budget_deferred",
        handler_reserved = "handler_unimplemented",
        handler_budget_deferred = "handler_budget_deferred",
        not_selected = "not_selected",
        reference_only = "reference_only",
        gx_fetch_orchestration_abort(
          "M7k encountered an unknown M7d coverage status."
        )
      )
    }
  }, character(1)))
  tibble::tibble(
    contract_version = rep.int(
      .gx_fetch_orchestration_contract_version, nrow(coverage)
    ),
    selection_order = unname(coverage$selection_order),
    fetch_order = unname(coverage$fetch_order),
    distribution_id = unname(coverage$distribution_id),
    handler_id = unname(coverage$handler_id),
    logical_request_id = logical_request_id,
    request_status = request_status,
    orchestration_status = statuses,
    attempted = rep.int(FALSE, nrow(coverage)),
    succeeded = rep.int(FALSE, nrow(coverage)),
    physical_attempts = rep.int(0L, nrow(coverage)),
    encoded_bytes = rep.int(0, nrow(coverage)),
    decoded_bytes = rep.int(0, nrow(coverage)),
    execution_id = rep.int(NA_character_, nrow(coverage)),
    result_index = rep.int(NA_integer_, nrow(coverage)),
    error_code = rep.int(NA_character_, nrow(coverage))
  )
}

gx_fetch_orchestration_oaf_result_id_impl <- function(result) {
  gx_contract_hash(
    list(
      "execution_id", result$execution$execution_id,
      "attempt_id", result$attempt$attempt_id[[1L]],
      "body_sha256", result$attempt$body_sha256[[1L]],
      "feature_count", result$execution$feature_count,
      "truncated", result$execution$truncated
    ),
    namespace = "geoconnexr.oaf-orchestration-result.v1",
    contract_version = .gx_fetch_orchestration_contract_version
  )
}

gx_fetch_orchestration_compact_oaf_impl <- function(execution) {
  result <- structure(
    list(
      result_id = "",
      response_body = execution$response_body,
      result = execution$result,
      implementation = execution$implementation,
      execution = execution$execution,
      attempt = execution$attempts
    ),
    class = "gx_oaf_orchestration_result"
  )
  result$result_id <- gx_fetch_orchestration_oaf_result_id_impl(result)
  result
}

gx_fetch_orchestration_wqp_result_id_impl <- function(result) {
  gx_contract_hash(
    list(
      "execution_id", result$execution$execution_id,
      "attempt_id", result$attempt$attempt_id[[1L]],
      "body_sha256", result$parse$body_sha256,
      "result_sha256", result$parse$result_sha256,
      "row_count", result$execution$row_count,
      "column_count", result$execution$column_count
    ),
    namespace = "geoconnexr.wqp-orchestration-result.v1",
    contract_version = .gx_fetch_orchestration_contract_version
  )
}

gx_fetch_orchestration_compact_wqp_impl <- function(execution) {
  result <- structure(
    list(
      result_id = "",
      response_body = execution$response_body,
      data = execution$data,
      schema = execution$schema,
      parse = execution$parse,
      implementation = execution$implementation,
      execution = execution$execution,
      attempt = execution$attempts
    ),
    class = "gx_wqp_orchestration_result"
  )
  result$result_id <- gx_fetch_orchestration_wqp_result_id_impl(result)
  result
}

gx_fetch_orchestration_wqp_failure_impl <- function(cnd) {
  capability <- inherits(cnd, "gx_error_wqp_execution_capability")
  transport <- inherits(cnd, "gx_error_wqp_execution_transport")
  parse <- inherits(cnd, "gx_error_wqp_execution_parse")
  if (!capability && !transport && !parse) {
    gx_fetch_orchestration_abort(
      "A WQP request failed outside an isolatable capability, transport, or parse phase.",
      "gx_error_fetch_orchestration_execution"
    )
  }
  attempts <- cnd$attempts
  physical <- if (is.data.frame(attempts) && nrow(attempts) &&
      "physical" %in% names(attempts)) {
    sum(attempts$physical %in% TRUE)
  } else 0L
  charged <- if (is.data.frame(attempts) && nrow(attempts) &&
      "charged_bytes" %in% names(attempts)) {
    sum(as.double(attempts$charged_bytes), na.rm = TRUE)
  } else 0
  execution_id <- cnd$execution_id %||% NA_character_
  valid_execution <- if (capability) {
    is.character(execution_id) && length(execution_id) == 1L &&
      is.na(execution_id)
  } else {
    gx_catalog_is_sha256(execution_id)
  }
  if (!valid_execution || physical > 1L || !is.finite(charged) ||
      charged < 0 || (capability && (physical != 0L || charged != 0))) {
    gx_fetch_orchestration_abort(
      "A failed WQP request returned invalid redacted attempt facts.",
      "gx_error_fetch_orchestration_execution"
    )
  }
  if (capability) {
    status <- "capability_failed"
    error_code <- "wqp_capability_failed"
  } else if (parse) {
    status <- "parse_failed"
    error_code <- "wqp_parse_failed"
  } else {
    status <- "transport_failed"
    error_code <- "wqp_transport_failed"
  }
  list(
    status = status,
    error_code = error_code,
    physical_attempts = unname(as.integer(physical)),
    bytes = unname(as.double(charged)),
    execution_id = unname(execution_id)
  )
}

gx_fetch_orchestration_oaf_failure_impl <- function(cnd) {
  capability <- inherits(cnd, "gx_error_oaf_execution_capability")
  transport <- inherits(cnd, "gx_error_oaf_execution_transport")
  parse <- inherits(cnd, "gx_error_oaf_execution_parse")
  if (!capability && !transport && !parse) {
    gx_fetch_orchestration_abort(
      "An OGC request failed outside an isolatable capability, transport, or parse phase.",
      "gx_error_fetch_orchestration_execution"
    )
  }
  attempts <- cnd$attempts
  physical <- if (is.data.frame(attempts) && nrow(attempts) &&
      "physical" %in% names(attempts)) {
    sum(attempts$physical %in% TRUE)
  } else 0L
  charged <- if (is.data.frame(attempts) && nrow(attempts) &&
      "charged_bytes" %in% names(attempts)) {
    sum(as.double(attempts$charged_bytes), na.rm = TRUE)
  } else 0
  execution_id <- cnd$execution_id %||% NA_character_
  valid_execution <- if (capability) {
    is.character(execution_id) && length(execution_id) == 1L &&
      is.na(execution_id)
  } else {
    gx_catalog_is_sha256(execution_id)
  }
  if (!valid_execution || physical > 1L || !is.finite(charged) ||
      charged < 0 || (capability && (physical != 0L || charged != 0))) {
    gx_fetch_orchestration_abort(
      "A failed OGC request returned invalid redacted attempt facts.",
      "gx_error_fetch_orchestration_execution"
    )
  }
  if (capability) {
    status <- "capability_failed"
    error_code <- "oaf_capability_failed"
  } else if (parse) {
    status <- "parse_failed"
    error_code <- "oaf_parse_failed"
  } else {
    status <- "transport_failed"
    error_code <- "oaf_transport_failed"
  }
  list(
    status = status,
    error_code = error_code,
    physical_attempts = unname(as.integer(physical)),
    bytes = unname(as.double(charged)),
    execution_id = unname(execution_id)
  )
}

gx_fetch_orchestration_run_impl <- function(
    request_plan, policy, requests, orchestration_id, admitted, status,
    oaf_symbol_resolver, wqp_symbol_resolver) {
  if (policy$dry_run || !any(admitted)) {
    return(list(results = list(), status = status))
  }
  results <- list()
  for (position in which(admitted)) {
    request <- requests[position, , drop = FALSE]
    coverage_position <- match(
      request$distribution_id[[1L]], status$distribution_id
    )
    child_scope <- gx_fetch_orchestration_child_scope_impl(
      orchestration_id, request$request_order[[1L]],
      request$handler_id[[1L]], request$logical_request_id[[1L]]
    )
    outcome <- if (request$handler_id[[1L]] == "csv") {
      tryCatch(
        gx_csv_execution_impl(
          request_plan = request_plan,
          logical_request_id = request$logical_request_id[[1L]],
          max_fields = policy$max_fields,
          timeout = policy$timeout_seconds,
          min_interval = policy$min_interval_seconds,
          execution_scope_id = child_scope
        ),
        error = identity
      )
    } else if (request$handler_id[[1L]] == "wqp") {
      wqp_plan <- gx_wqp_request_plan_impl(
        request_plan, request$distribution_id[[1L]],
        max_fields = policy$max_fields
      )
      tryCatch(
        gx_wqp_execution_impl(
          request_plan = wqp_plan,
          timeout = policy$timeout_seconds,
          min_interval = policy$min_interval_seconds,
          execution_scope_id = child_scope,
          symbol_resolver = wqp_symbol_resolver
        ),
        error = identity
      )
    } else {
      oaf_plan <- gx_oaf_request_plan_impl(
        request_plan, request$distribution_id[[1L]], limit = policy$oaf_limit
      )
      tryCatch(
        gx_oaf_execution_impl(
          request_plan = oaf_plan,
          timeout = policy$timeout_seconds,
          min_interval = policy$min_interval_seconds,
          execution_scope_id = child_scope,
          symbol_resolver = oaf_symbol_resolver
        ),
        error = identity
      )
    }
    status$attempted[[coverage_position]] <- TRUE
    if (inherits(outcome, "gx_csv_execution")) {
      result <- gx_csv_orchestration_compact_result_impl(outcome)
    } else if (inherits(outcome, "gx_wqp_execution")) {
      result <- gx_fetch_orchestration_compact_wqp_impl(outcome)
    } else if (inherits(outcome, "gx_oaf_execution")) {
      result <- gx_fetch_orchestration_compact_oaf_impl(outcome)
    } else {
      failure <- if (request$handler_id[[1L]] == "csv") {
        tryCatch(
          gx_csv_orchestration_failure_impl(outcome),
          error = function(cnd) gx_fetch_orchestration_abort(
            "A direct-CSV request failed outside an isolatable phase.",
            "gx_error_fetch_orchestration_execution"
          )
        )
      } else if (request$handler_id[[1L]] == "wqp") {
        gx_fetch_orchestration_wqp_failure_impl(outcome)
      } else {
        gx_fetch_orchestration_oaf_failure_impl(outcome)
      }
      status$orchestration_status[[coverage_position]] <- failure$status
      status$physical_attempts[[coverage_position]] <- failure$physical_attempts
      status$encoded_bytes[[coverage_position]] <- failure$bytes
      status$decoded_bytes[[coverage_position]] <- failure$bytes
      status$execution_id[[coverage_position]] <- failure$execution_id
      status$error_code[[coverage_position]] <- failure$error_code
      next
    }
    results[[length(results) + 1L]] <- result
    status$orchestration_status[[coverage_position]] <-
      "provider_response_validated_and_parsed"
    status$succeeded[[coverage_position]] <- TRUE
    status$physical_attempts[[coverage_position]] <- 1L
    status$encoded_bytes[[coverage_position]] <- result$execution$encoded_bytes
    status$decoded_bytes[[coverage_position]] <- result$execution$decoded_bytes
    status$execution_id[[coverage_position]] <- result$execution$execution_id
    status$result_index[[coverage_position]] <- as.integer(length(results))
  }
  list(results = results, status = status)
}

gx_fetch_orchestration_overall_status_impl <- function(policy, status) {
  if (policy$dry_run) return("dry_run_complete")
  if (any(status$orchestration_status %in% c(
    "capability_failed", "transport_failed", "parse_failed"
  ))) return("execution_complete_with_failures")
  if (any(status$orchestration_status == "batch_limit_deferred")) {
    return("execution_complete_with_batch_deferral")
  }
  "execution_complete"
}

gx_fetch_orchestration_owned_impl <- function(
    orchestration_scope_id, orchestration_id, policy, requests, admitted,
    status) {
  list(
    orchestration_scope_id = orchestration_scope_id,
    orchestration_id = orchestration_id,
    execution_mode = policy$execution_mode,
    orchestration_status = gx_fetch_orchestration_overall_status_impl(
      policy, status
    ),
    planned_requests = unname(as.integer(nrow(requests))),
    admitted_requests = unname(as.integer(sum(admitted))),
    completed_requests = unname(as.integer(sum(status$attempted)))
  )
}

gx_fetch_orchestration_result_handler_impl <- function(result) {
  if (inherits(result, "gx_csv_orchestration_result")) return("csv")
  if (inherits(result, "gx_wqp_orchestration_result")) return("wqp")
  if (inherits(result, "gx_oaf_orchestration_result")) {
    return("ogc_api_features")
  }
  NA_character_
}

gx_fetch_orchestration_counts_impl <- function(
    status, requests, results) {
  handlers <- if (length(results)) vapply(
    results, gx_fetch_orchestration_result_handler_impl, character(1),
    USE.NAMES = FALSE
  ) else character()
  list(
    distributions = unname(as.integer(nrow(status))),
    candidate_requests = unname(as.integer(nrow(requests))),
    csv_requests = unname(as.integer(sum(requests$handler_id == "csv"))),
    wqp_requests = unname(as.integer(sum(requests$handler_id == "wqp"))),
    oaf_requests = unname(as.integer(sum(
      requests$handler_id == "ogc_api_features"
    ))),
    admitted_requests = unname(as.integer(sum(
      status$orchestration_status %in% c(
        "dry_run_planned", "execution_pending",
        "provider_response_validated_and_parsed", "capability_failed",
        "transport_failed", "parse_failed"
      )
    ))),
    batch_limit_deferred = unname(as.integer(sum(
      status$orchestration_status == "batch_limit_deferred"
    ))),
    attempted_requests = unname(as.integer(sum(status$attempted))),
    successful_requests = unname(as.integer(sum(status$succeeded))),
    failed_requests = unname(as.integer(sum(
      status$attempted & !status$succeeded
    ))),
    physical_attempts = unname(as.integer(sum(status$physical_attempts))),
    encoded_bytes = unname(as.double(sum(status$encoded_bytes))),
    decoded_bytes = unname(as.double(sum(status$decoded_bytes))),
    handler_deferred = unname(as.integer(sum(
      status$orchestration_status %in% c(
        "handler_unimplemented", "handler_budget_deferred"
      )
    ))),
    handler_plan_unsupported = unname(as.integer(sum(
      status$orchestration_status == "handler_plan_unsupported"
    ))),
    not_selected = unname(as.integer(sum(
      status$orchestration_status == "not_selected"
    ))),
    reference_only = unname(as.integer(sum(
      status$orchestration_status == "reference_only"
    ))),
    csv_results = unname(as.integer(sum(handlers == "csv"))),
    wqp_results = unname(as.integer(sum(handlers == "wqp"))),
    oaf_results = unname(as.integer(sum(handlers == "ogc_api_features"))),
    results = unname(as.integer(length(results)))
  )
}

gx_fetch_orchestration_reasons_impl <- function(
    request_plan, policy, status) {
  reasons <- request_plan$metadata$non_replayable_reasons
  reasons <- setdiff(reasons, c(
    "timeout_policy_unbound", "non_csv_request_plans_absent"
  ))
  reasons <- c(reasons, "remaining_handlers_unimplemented",
               "pagination_unbound", "serialization_unbound")
  if (policy$dry_run) reasons <- c(reasons, "dry_run_no_transport")
  if (any(status$orchestration_status == "batch_limit_deferred")) {
    reasons <- c(reasons, "cross_handler_batch_limits_deferred")
  }
  if (any(status$orchestration_status == "handler_plan_unsupported")) {
    unsupported <- status$handler_id[
      status$orchestration_status == "handler_plan_unsupported"
    ]
    if (any(unsupported == "wqp")) {
      reasons <- c(reasons, "wqp_request_plan_unsupported")
    }
    if (any(unsupported == "ogc_api_features")) {
      reasons <- c(reasons, "oaf_request_plan_unsupported")
    }
  }
  if (any(status$orchestration_status %in% c(
    "capability_failed", "transport_failed", "parse_failed"
  ))) {
    reasons <- c(reasons, "cross_handler_execution_failures_present")
  }
  reasons <- unique(reasons)
  reasons[gx_catalog_byte_order(reasons)]
}

gx_fetch_orchestration_metadata_impl <- function(
    request_plan, policy, requests, status, results) {
  counts <- gx_fetch_orchestration_counts_impl(status, requests, results)
  successful <- which(status$succeeded)
  csv_success <- any(status$handler_id[successful] == "csv")
  wqp_success <- any(status$handler_id[successful] == "wqp")
  oaf_success <- any(
    status$handler_id[successful] == "ogc_api_features"
  )
  symbol_attempted <- any(
    status$handler_id %in% c("wqp", "ogc_api_features") & status$attempted
  )
  list(
    host_specific = counts$physical_attempts > 0L,
    replayable = FALSE,
    execution_ready = FALSE,
    transport_authorized = !policy$dry_run,
    execution_completed = TRUE,
    dry_run = policy$dry_run,
    budgets_consumed = counts$physical_attempts > 0L,
    provider_responses_observed = counts$successful_requests > 0L,
    response_candidates_validated = counts$successful_requests > 0L,
    csv_semantics_validated = csv_success,
    wqp_semantics_validated = wqp_success,
    oaf_semantics_validated = oaf_success,
    runtime_symbols_checked = symbol_attempted,
    result_contract_bound = TRUE,
    status_reconciled = TRUE,
    continue_on_error = TRUE,
    results_compacted = TRUE,
    global_order_enforced = TRUE,
    observation_origin = if (policy$dry_run) {
      "dry_run"
    } else if (counts$physical_attempts > 0L) {
      "provider_transport"
    } else {
      "orchestrator_no_physical_attempt"
    },
    counts = counts,
    non_replayable_reasons = gx_fetch_orchestration_reasons_impl(
      request_plan, policy, status
    )
  )
}

gx_fetch_orchestration_new_impl <- function(
    request_plan, policy, requests, orchestration, results, status, metadata) {
  object <- structure(
    list(
      contract_version = .gx_fetch_orchestration_contract_version,
      request_plan = request_plan,
      policy = policy,
      requests = requests,
      orchestration = orchestration,
      results = results,
      status = status,
      metadata = metadata
    ),
    class = "gx_fetch_orchestration"
  )
  gx_fetch_orchestration_validate_impl(object)
  object
}

gx_fetch_orchestration_impl <- function(
    request_plan,
    dry_run = NULL,
    max_executions = NULL,
    max_total_bytes = NULL,
    max_fields = NULL,
    oaf_limit = NULL,
    timeout = NULL,
    min_interval = NULL,
    orchestration_scope_id = NULL,
    oaf_symbol_resolver = NULL,
    wqp_symbol_resolver = NULL) {
  request_plan <- gx_fetch_orchestration_input_plan_impl(request_plan)
  policy <- gx_fetch_orchestration_policy_impl(
    dry_run, max_executions, max_total_bytes, max_fields, oaf_limit,
    timeout, min_interval
  )
  orchestration_scope_id <- tryCatch(
    gx_csv_execution_scope_impl(orchestration_scope_id),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(orchestration_scope_id)) {
    gx_fetch_orchestration_abort(
      "M7k requires one explicit opaque orchestration scope identity.",
      "gx_error_fetch_orchestration_policy"
    )
  }
  planning <- gx_fetch_orchestration_planning_impl(request_plan, policy)
  requests <- planning$requests
  orchestration_id <- gx_fetch_orchestration_id_impl(
    orchestration_scope_id, request_plan, policy, requests
  )
  admitted <- gx_fetch_orchestration_admission_impl(requests, policy)
  status <- gx_fetch_orchestration_status_impl(
    request_plan, policy, planning, admitted
  )
  run <- gx_fetch_orchestration_run_impl(
    request_plan, policy, requests, orchestration_id, admitted, status,
    oaf_symbol_resolver, wqp_symbol_resolver
  )
  orchestration <- gx_fetch_orchestration_owned_impl(
    orchestration_scope_id, orchestration_id, policy, requests, admitted,
    run$status
  )
  metadata <- gx_fetch_orchestration_metadata_impl(
    request_plan, policy, requests, run$status, run$results
  )
  gx_fetch_orchestration_new_impl(
    request_plan, policy, requests, orchestration, run$results, run$status,
    metadata
  )
}

gx_fetch_orchestration_validate_policy_impl <- function(policy) {
  valid_shape <- is.list(policy) && identical(
    names(policy), .gx_fetch_orchestration_policy_fields
  ) && gx_csv_orchestration_exact_attributes(policy, "names")
  expected <- if (valid_shape) tryCatch(
    gx_fetch_orchestration_policy_impl(
      policy$dry_run, policy$max_executions, policy$max_total_bytes,
      policy$max_fields, policy$oaf_limit, policy$timeout_seconds,
      policy$min_interval_seconds
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  if (is.null(expected) || !identical(policy, expected) || !all(vapply(
    policy, function(value) is.null(attributes(value)), logical(1)
  ))) {
    gx_fetch_orchestration_abort(
      "M7k policy violates its exact bounded contract."
    )
  }
  invisible(policy)
}

gx_fetch_orchestration_validate_oaf_result_impl <- function(
    result, request_plan, policy) {
  valid_shape <- is.list(result) && identical(
    names(result), .gx_fetch_orchestration_oaf_result_fields
  ) && identical(class(result), "gx_oaf_orchestration_result") &&
    gx_csv_orchestration_exact_attributes(result, c("names", "class")) &&
    is.raw(result$response_body) &&
    is.null(attributes(result$response_body))
  if (!valid_shape) {
    gx_fetch_orchestration_abort(
      "A compact M7k OGC result violates its exact shape."
    )
  }
  distribution_id <- result$execution$distribution_id %||% NA_character_
  oaf_plan <- tryCatch(
    gx_oaf_request_plan_impl(
      request_plan, distribution_id, limit = policy$oaf_limit
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  valid <- if (!is.null(oaf_plan)) tryCatch({
    execution <- structure(
      list(
        contract_version = .gx_oaf_execution_contract_version,
        request_plan = oaf_plan,
        response_body = result$response_body,
        result = result$result,
        implementation = result$implementation,
        execution = result$execution,
        attempts = result$attempt,
        metadata = gx_oaf_execution_metadata_impl(
          oaf_plan, result$result, length(result$response_body)
        )
      ),
      class = "gx_oaf_execution"
    )
    gx_oaf_execution_validate_impl(execution)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE) else FALSE
  if (!valid || !identical(
    result$result_id, gx_fetch_orchestration_oaf_result_id_impl(result)
  )) {
    gx_fetch_orchestration_abort(
      "A compact M7k OGC result no longer rebinds to its retained bytes."
    )
  }
  invisible(result)
}

gx_fetch_orchestration_validate_wqp_result_impl <- function(
    result, request_plan, policy) {
  valid_shape <- is.list(result) && identical(
    names(result), .gx_fetch_orchestration_wqp_result_fields
  ) && identical(class(result), "gx_wqp_orchestration_result") &&
    gx_csv_orchestration_exact_attributes(result, c("names", "class")) &&
    is.raw(result$response_body) && is.null(attributes(result$response_body))
  if (!valid_shape) {
    gx_fetch_orchestration_abort(
      "A compact M7k WQP result violates its exact shape."
    )
  }
  distribution_id <- result$execution$distribution_id %||% NA_character_
  wqp_plan <- tryCatch(
    gx_wqp_request_plan_impl(
      request_plan, distribution_id, max_fields = policy$max_fields
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  valid <- if (!is.null(wqp_plan)) tryCatch({
    rebuilt <- list(
      data = result$data,
      schema = result$schema,
      parse = result$parse
    )
    execution <- structure(
      list(
        contract_version = .gx_wqp_execution_contract_version,
        request_plan = wqp_plan,
        response_body = result$response_body,
        data = result$data,
        schema = result$schema,
        parse = result$parse,
        implementation = result$implementation,
        execution = result$execution,
        attempts = result$attempt,
        metadata = gx_wqp_execution_metadata_impl(
          wqp_plan, rebuilt, length(result$response_body)
        )
      ),
      class = "gx_wqp_execution"
    )
    gx_wqp_execution_validate_impl(execution)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE) else FALSE
  if (!valid || !identical(
    result$result_id, gx_fetch_orchestration_wqp_result_id_impl(result)
  )) {
    gx_fetch_orchestration_abort(
      "A compact M7k WQP result no longer rebinds to its retained bytes."
    )
  }
  invisible(result)
}

gx_fetch_orchestration_validate_result_impl <- function(
    result, request_plan, policy) {
  if (inherits(result, "gx_csv_orchestration_result")) {
    valid <- tryCatch({
      gx_csv_orchestration_validate_result_impl(
        result, request_plan, policy
      )
      TRUE
    }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
    if (!valid) {
      gx_fetch_orchestration_abort(
        "A compact M7k CSV result failed whole-result validation."
      )
    }
  } else if (inherits(result, "gx_wqp_orchestration_result")) {
    gx_fetch_orchestration_validate_wqp_result_impl(
      result, request_plan, policy
    )
  } else if (inherits(result, "gx_oaf_orchestration_result")) {
    gx_fetch_orchestration_validate_oaf_result_impl(
      result, request_plan, policy
    )
  } else {
    gx_fetch_orchestration_abort(
      "M7k results must use one supported handler-specific compact contract."
    )
  }
  invisible(result)
}

gx_fetch_orchestration_validate_status_impl <- function(
    status, request_plan, policy, planning, admitted, results) {
  rows <- gx_catalog_table_rows(status)
  valid_shape <- inherits(status, "tbl_df") &&
    identical(class(status), c("tbl_df", "tbl", "data.frame")) &&
    identical(names(status), .gx_fetch_orchestration_status_columns) &&
    !is.null(rows) && rows == nrow(request_plan$coverage) &&
    gx_csv_request_plan_table_attributes(status, as.integer(rows))
  integer_columns <- c(
    "selection_order", "fetch_order", "physical_attempts", "result_index"
  )
  numeric_columns <- c("encoded_bytes", "decoded_bytes")
  logical_columns <- c("attempted", "succeeded")
  character_columns <- setdiff(
    .gx_fetch_orchestration_status_columns,
    c(integer_columns, numeric_columns, logical_columns)
  )
  valid_types <- valid_shape && all(vapply(
    status[integer_columns], is.integer, logical(1)
  )) && all(vapply(status[numeric_columns], is.double, logical(1))) &&
    all(vapply(status[logical_columns], is.logical, logical(1))) &&
    all(vapply(status[character_columns], is.character, logical(1)))
  if (!valid_types || anyNA(status[c(
    "contract_version", "selection_order", "distribution_id", "handler_id",
    "request_status", "orchestration_status", "attempted", "succeeded",
    "physical_attempts", "encoded_bytes", "decoded_bytes"
  )])) {
    gx_fetch_orchestration_abort(
      "M7k status violates its exact one-row-per-distribution shape."
    )
  }
  base <- gx_fetch_orchestration_status_impl(
    request_plan, policy, planning, admitted
  )
  identity_columns <- c(
    "contract_version", "selection_order", "fetch_order", "distribution_id",
    "handler_id", "logical_request_id", "request_status"
  )
  if (!identical(status[identity_columns], base[identity_columns]) ||
      any(status$physical_attempts < 0L) ||
      any(status$physical_attempts > 1L) || any(status$encoded_bytes < 0) ||
      any(status$decoded_bytes < 0) ||
      any(status$encoded_bytes != status$decoded_bytes) ||
      any(status$succeeded & !status$attempted)) {
    gx_fetch_orchestration_abort(
      "M7k status identities or bounded attempt facts are invalid."
    )
  }
  nonadmitted <- base$orchestration_status != "execution_pending"
  if (policy$dry_run) nonadmitted <- rep.int(TRUE, nrow(base))
  if (any(status$orchestration_status[nonadmitted] !=
      base$orchestration_status[nonadmitted]) ||
      any(status$attempted[nonadmitted]) ||
      any(status$succeeded[nonadmitted]) ||
      any(status$physical_attempts[nonadmitted] != 0L) ||
      any(status$encoded_bytes[nonadmitted] != 0) ||
      any(status$decoded_bytes[nonadmitted] != 0) ||
      any(!is.na(status$execution_id[nonadmitted])) ||
      any(!is.na(status$result_index[nonadmitted])) ||
      any(!is.na(status$error_code[nonadmitted]))) {
    gx_fetch_orchestration_abort(
      "M7k non-attempted rows overstate execution or budget facts."
    )
  }
  if (!policy$dry_run) {
    live <- which(base$orchestration_status == "execution_pending")
    terminal <- c(
      "provider_response_validated_and_parsed", "capability_failed",
      "transport_failed", "parse_failed"
    )
    if (any(!status$orchestration_status[live] %in% terminal) ||
        any(!status$attempted[live])) {
      gx_fetch_orchestration_abort(
        "Every admitted M7k request must reach one terminal status."
      )
    }
    successful <- which(status$succeeded)
    failed <- which(status$attempted & !status$succeeded)
    if (!identical(
      status$result_index[successful], as.integer(seq_along(results))
    ) || length(successful) != length(results) ||
        any(status$orchestration_status[successful] !=
          "provider_response_validated_and_parsed") ||
        any(status$physical_attempts[successful] != 1L) ||
        any(!gx_catalog_is_sha256(status$execution_id[successful])) ||
        any(!is.na(status$error_code[successful])) ||
        any(!is.na(status$result_index[failed]))) {
      gx_fetch_orchestration_abort(
        "M7k terminal rows do not reconcile exactly with compact results."
      )
    }
    if (length(failed)) {
      expected_error <- vapply(failed, function(position) {
        handler <- status$handler_id[[position]]
        terminal_status <- status$orchestration_status[[position]]
        if (handler == "csv" && terminal_status == "transport_failed") {
          "csv_transport_failed"
        } else if (handler == "csv" && terminal_status == "parse_failed") {
          "csv_parse_failed"
        } else if (handler == "wqp" &&
            terminal_status == "capability_failed") {
          "wqp_capability_failed"
        } else if (handler == "wqp" &&
            terminal_status == "transport_failed") {
          "wqp_transport_failed"
        } else if (handler == "wqp" && terminal_status == "parse_failed") {
          "wqp_parse_failed"
        } else if (handler == "ogc_api_features" &&
            terminal_status == "capability_failed") {
          "oaf_capability_failed"
        } else if (handler == "ogc_api_features" &&
            terminal_status == "transport_failed") {
          "oaf_transport_failed"
        } else if (handler == "ogc_api_features" &&
            terminal_status == "parse_failed") {
          "oaf_parse_failed"
        } else {
          NA_character_
        }
      }, character(1))
      capability <- failed[
        status$orchestration_status[failed] == "capability_failed"
      ]
      other_failures <- setdiff(failed, capability)
      if (anyNA(expected_error) ||
          !identical(status$error_code[failed], unname(expected_error)) ||
          any(status$physical_attempts[capability] != 0L) ||
          any(status$encoded_bytes[capability] != 0) ||
          any(!is.na(status$execution_id[capability])) ||
          any(!gx_catalog_is_sha256(
            status$execution_id[other_failures]
          ))) {
        gx_fetch_orchestration_abort(
          "M7k failure rows do not bind their handler-specific phase."
        )
      }
    }
    if (length(results)) {
      for (index in seq_along(results)) {
        row <- successful[[index]]
        result <- results[[index]]
        handler <- gx_fetch_orchestration_result_handler_impl(result)
        if (status$handler_id[[row]] != handler ||
            status$execution_id[[row]] != result$execution$execution_id ||
            status$logical_request_id[[row]] !=
              result$execution$logical_request_id ||
            status$encoded_bytes[[row]] != result$execution$encoded_bytes ||
            status$decoded_bytes[[row]] != result$execution$decoded_bytes) {
          gx_fetch_orchestration_abort(
            "M7k successful status rows do not bind their compact results."
          )
        }
      }
    }
  }
  request_rows <- match(
    status$distribution_id, planning$requests$distribution_id
  )
  planned <- which(!is.na(request_rows))
  if (any(status$encoded_bytes[planned] >
      planning$requests$response_byte_limit[request_rows[planned]]) ||
      any(status$decoded_bytes[planned] >
        planning$requests$response_byte_limit[request_rows[planned]]) ||
      sum(status$encoded_bytes) > request_plan$budgets$reserved_encoded_bytes ||
      sum(status$decoded_bytes) > request_plan$budgets$reserved_decoded_bytes ||
      sum(status$decoded_bytes) > policy$max_total_bytes) {
    gx_fetch_orchestration_abort(
      "M7k status exceeds an allocated request or aggregate byte budget.",
      "gx_error_fetch_orchestration_budget"
    )
  }
  invisible(status)
}

gx_fetch_orchestration_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(
    names(x), .gx_fetch_orchestration_fields
  ) && identical(class(x), "gx_fetch_orchestration") &&
    gx_csv_orchestration_exact_attributes(x, c("names", "class")) &&
    identical(
      x$contract_version, .gx_fetch_orchestration_contract_version
    ) && is.null(attributes(x$contract_version))
  if (!valid_top) {
    gx_fetch_orchestration_abort(
      "M7k orchestration violates its exact top-level contract."
    )
  }
  gx_fetch_orchestration_input_plan_impl(x$request_plan)
  gx_fetch_orchestration_validate_policy_impl(x$policy)
  planning <- gx_fetch_orchestration_planning_impl(
    x$request_plan, x$policy
  )
  if (!identical(x$requests, planning$requests)) {
    gx_fetch_orchestration_abort(
      "M7k candidate requests no longer rebind to the shared M7d plan."
    )
  }
  admitted <- gx_fetch_orchestration_admission_impl(
    x$requests, x$policy
  )
  orchestration <- x$orchestration
  valid_orchestration <- is.list(orchestration) && identical(
    names(orchestration), .gx_fetch_orchestration_owned_fields
  ) && gx_csv_orchestration_exact_attributes(orchestration, "names") &&
    gx_catalog_is_sha256(orchestration$orchestration_scope_id) &&
    gx_catalog_is_sha256(orchestration$orchestration_id)
  if (!valid_orchestration || !identical(
    orchestration$orchestration_id,
    gx_fetch_orchestration_id_impl(
      orchestration$orchestration_scope_id, x$request_plan, x$policy,
      x$requests
    )
  )) {
    gx_fetch_orchestration_abort(
      "M7k orchestration identity no longer binds its plan and policy."
    )
  }
  if (!is.list(x$results) || !is.null(attributes(x$results)) ||
      length(x$results) > x$policy$max_executions) {
    gx_fetch_orchestration_abort(
      "M7k compact results violate their list or cardinality budget."
    )
  }
  if (length(x$results)) {
    for (result in x$results) {
      gx_fetch_orchestration_validate_result_impl(
        result, x$request_plan, x$policy
      )
      request_position <- match(
        result$execution$logical_request_id,
        x$requests$logical_request_id
      )
      if (is.na(request_position)) {
        gx_fetch_orchestration_abort(
          "A compact M7k result has no planned logical request."
        )
      }
      expected_scope <- gx_fetch_orchestration_child_scope_impl(
        orchestration$orchestration_id,
        x$requests$request_order[[request_position]],
        x$requests$handler_id[[request_position]],
        result$execution$logical_request_id
      )
      if (!identical(result$execution$execution_scope_id, expected_scope)) {
        gx_fetch_orchestration_abort(
          "A compact M7k result has a foreign child execution scope."
        )
      }
    }
    execution_ids <- vapply(
      x$results, function(result) result$execution$execution_id,
      character(1), USE.NAMES = FALSE
    )
    logical_ids <- vapply(
      x$results, function(result) result$execution$logical_request_id,
      character(1), USE.NAMES = FALSE
    )
    if (anyDuplicated(execution_ids) || anyDuplicated(logical_ids)) {
      gx_fetch_orchestration_abort(
        "M7k compact result identities must be unique."
      )
    }
  }
  gx_fetch_orchestration_validate_status_impl(
    x$status, x$request_plan, x$policy, planning, admitted, x$results
  )
  expected_orchestration <- gx_fetch_orchestration_owned_impl(
    orchestration$orchestration_scope_id,
    orchestration$orchestration_id,
    x$policy,
    x$requests,
    admitted,
    x$status
  )
  expected_metadata <- gx_fetch_orchestration_metadata_impl(
    x$request_plan, x$policy, x$requests, x$status, x$results
  )
  valid_metadata <- is.list(x$metadata) && identical(
    names(x$metadata), .gx_fetch_orchestration_metadata_fields
  ) && gx_csv_orchestration_exact_attributes(x$metadata, "names") &&
    is.list(x$metadata$counts) && identical(
      names(x$metadata$counts), .gx_fetch_orchestration_count_fields
    ) && gx_csv_orchestration_exact_attributes(x$metadata$counts, "names")
  if (!identical(orchestration, expected_orchestration) || !valid_metadata ||
      !identical(x$metadata, expected_metadata)) {
    gx_fetch_orchestration_abort(
      "M7k orchestration metadata or completion facts are inconsistent."
    )
  }
  owned_text <- gx_fetch_plan_text_total(
    x, limit = .gx_fetch_orchestration_max_text_bytes
  )
  if (!is.finite(owned_text) ||
      owned_text > .gx_fetch_orchestration_max_text_bytes) {
    gx_fetch_orchestration_abort(
      "M7k owned text exceeds its aggregate byte budget.",
      "gx_error_fetch_orchestration_budget"
    )
  }
  invisible(x)
}

#' @export
print.gx_fetch_orchestration <- function(x, ...) {
  gx_fetch_orchestration_validate_impl(x)
  counts <- x$metadata$counts
  cli::cli_inform(c(
    "<gx_fetch_orchestration>",
    paste0(
      "* Mode: ", x$policy$execution_mode, "; requests admitted: ",
      counts$admitted_requests, "/", counts$candidate_requests
    ),
    paste0(
      "* CSV: ", counts$csv_requests, "; WQP: ", counts$wqp_requests,
      "; OGC Features: ",
      counts$oaf_requests, "; successful: ", counts$successful_requests
    ),
    paste0(
      "* Failed: ", counts$failed_requests,
      "; physical attempts: ", counts$physical_attempts
    )
  ))
  invisible(x)
}
