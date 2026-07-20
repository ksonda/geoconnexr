.gx_csv_orchestration_contract_version <- "0.1.0"
.gx_csv_orchestration_max_executions <- 32L
.gx_csv_orchestration_max_total_bytes <- 64L * 1024L^2
.gx_csv_orchestration_max_text_bytes <- 256L * 1024L^2

.gx_csv_orchestration_fields <- c(
  "contract_version", "request_plan", "policy", "orchestration", "results",
  "status", "metadata"
)

.gx_csv_orchestration_policy_fields <- c(
  "slice_id", "execution_mode", "dry_run", "scheduling_policy",
  "parallelism", "failure_policy", "max_executions", "max_total_bytes",
  "max_fields", "timeout_seconds", "min_interval_seconds", "cache_policy",
  "redirect_policy", "retry_policy"
)

.gx_csv_orchestration_fields_owned <- c(
  "orchestration_scope_id", "orchestration_id", "execution_mode",
  "orchestration_status", "planned_requests", "admitted_requests",
  "completed_requests"
)

.gx_csv_orchestration_result_fields <- c(
  "result_id", "execution", "attempt", "validation", "parse_policy",
  "schema", "data", "parse"
)

.gx_csv_orchestration_status_columns <- c(
  "contract_version", "selection_order", "fetch_order", "distribution_id",
  "handler_id", "logical_request_id", "request_status",
  "orchestration_status", "attempted", "succeeded", "physical_attempts",
  "encoded_bytes", "decoded_bytes", "execution_id", "result_index",
  "error_code"
)

.gx_csv_orchestration_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "execution_completed", "dry_run", "budgets_consumed",
  "provider_responses_observed", "response_candidates_validated",
  "csv_semantics_validated", "result_contract_bound", "status_reconciled",
  "continue_on_error", "results_compacted", "observation_origin", "counts",
  "non_replayable_reasons"
)

.gx_csv_orchestration_count_fields <- c(
  "distributions", "csv_requests", "admitted_requests",
  "batch_limit_deferred", "attempted_requests", "successful_requests",
  "failed_requests", "physical_attempts", "encoded_bytes", "decoded_bytes",
  "handler_deferred", "not_selected", "reference_only", "results"
)

gx_csv_orchestration_abort <- function(
    message,
    class = "gx_error_csv_orchestration_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_csv_orchestration", "gx_error_fetch_plan"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_csv_orchestration_exact_attributes <- function(x, expected) {
  observed <- names(attributes(x))
  is.character(observed) && !anyNA(observed) &&
    length(observed) == length(expected) && all(expected %in% observed)
}

gx_csv_orchestration_scalar_text <- function(x, nonempty = TRUE) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    is.null(attributes(x)) && gx_fetch_plan_text_valid(
      x, allow_na = FALSE, nonempty = nonempty
    )
}

gx_csv_orchestration_input_plan_impl <- function(request_plan) {
  valid <- tryCatch({
    gx_csv_request_plan_validate_impl(request_plan)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid) {
    gx_csv_orchestration_abort(
      "M7h construction requires one valid M7d direct-CSV request plan.",
      "gx_error_csv_orchestration_input"
    )
  }
  request_plan
}

gx_csv_orchestration_flag_impl <- function(x) {
  if (!is.logical(x) || length(x) != 1L || is.na(x) ||
      !is.null(attributes(x))) {
    gx_csv_orchestration_abort(
      "The M7h dry-run choice must be one explicit logical value.",
      "gx_error_csv_orchestration_policy"
    )
  }
  unname(x)
}

gx_csv_orchestration_integer_impl <- function(x, maximum, message) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x) || x > maximum || !is.null(attributes(x))) {
    gx_csv_orchestration_abort(
      message,
      "gx_error_csv_orchestration_policy"
    )
  }
  unname(as.integer(x))
}

gx_csv_orchestration_byte_limit_impl <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x) || x > .gx_csv_orchestration_max_total_bytes ||
      !is.null(attributes(x))) {
    gx_csv_orchestration_abort(
      "The M7h aggregate response-byte ceiling must be an explicit bounded whole number.",
      "gx_error_csv_orchestration_policy"
    )
  }
  unname(as.double(x))
}

gx_csv_orchestration_policy_impl <- function(
    dry_run, max_executions, max_total_bytes, max_fields, timeout,
    min_interval) {
  dry_run <- gx_csv_orchestration_flag_impl(dry_run)
  max_executions <- gx_csv_orchestration_integer_impl(
    max_executions,
    .gx_csv_orchestration_max_executions,
    "The M7h execution ceiling must be an explicit bounded whole number."
  )
  max_total_bytes <- gx_csv_orchestration_byte_limit_impl(max_total_bytes)
  max_fields <- tryCatch(
    gx_csv_parsed_response_field_limit_impl(max_fields),
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
  if (is.null(max_fields) || is.null(timeout) || is.null(min_interval)) {
    gx_csv_orchestration_abort(
      "M7h parser and timing limits must be explicit bounded values.",
      "gx_error_csv_orchestration_policy"
    )
  }
  list(
    slice_id = "direct_csv_orchestration_v1",
    execution_mode = if (dry_run) "dry_run" else "sequential_continue",
    dry_run = dry_run,
    scheduling_policy = "global_request_order",
    parallelism = 1L,
    failure_policy = "continue_unrelated_requests",
    max_executions = max_executions,
    max_total_bytes = max_total_bytes,
    max_fields = max_fields,
    timeout_seconds = timeout,
    min_interval_seconds = min_interval,
    cache_policy = "bypass",
    redirect_policy = "reject",
    retry_policy = "none"
  )
}

gx_csv_orchestration_admission_impl <- function(request_plan, policy) {
  requests <- request_plan$request_plans
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

gx_csv_orchestration_hash_pairs_impl <- function(prefix, values) {
  output <- vector("list", length(values) * 2L)
  if (!length(values)) return(output)
  for (position in seq_along(values)) {
    output[[position * 2L - 1L]] <- paste0(prefix, "_", position)
    output[[position * 2L]] <- values[[position]]
  }
  unname(output)
}

gx_csv_orchestration_id_impl <- function(
    orchestration_scope_id, request_plan, policy) {
  gx_contract_hash(
    c(
      list(
      "orchestration_scope_id", orchestration_scope_id,
        "logical_request_count", nrow(request_plan$request_plans),
        "coverage_count", nrow(request_plan$coverage),
        "policy_field_count", length(policy)
      ),
      gx_csv_orchestration_hash_pairs_impl(
        "logical_request_id",
        request_plan$request_plans$logical_request_id
      ),
      gx_csv_orchestration_hash_pairs_impl(
        "coverage_distribution_id",
        request_plan$coverage$distribution_id
      ),
      gx_csv_orchestration_hash_pairs_impl(
        "coverage_request_status",
        request_plan$coverage$request_status
      ),
      gx_csv_orchestration_hash_pairs_impl("policy", policy)
    ),
    namespace = "geoconnexr.csv-orchestration.v1",
    contract_version = .gx_csv_orchestration_contract_version
  )
}

gx_csv_orchestration_child_scope_impl <- function(
    orchestration_id, request_order, logical_request_id) {
  gx_contract_hash(
    list(
      "orchestration_id", orchestration_id,
      "request_order", request_order,
      "logical_request_id", logical_request_id
    ),
    namespace = "geoconnexr.csv-orchestration-child.v1",
    contract_version = .gx_csv_orchestration_contract_version
  )
}

gx_csv_orchestration_status_impl <- function(
    request_plan, policy, admitted) {
  coverage <- request_plan$coverage
  request_position <- match(
    coverage$logical_request_id,
    request_plan$request_plans$logical_request_id
  )
  statuses <- unname(vapply(seq_len(nrow(coverage)), function(position) {
    source <- coverage$request_status[[position]]
    if (identical(source, "csv_request_planned")) {
      request_row <- request_position[[position]]
      if (!admitted[[request_row]]) return("batch_limit_deferred")
      if (policy$dry_run) "dry_run_planned" else "execution_pending"
    } else {
      switch(
        source,
        csv_budget_deferred = "csv_budget_deferred",
        handler_reserved = "handler_unimplemented",
        handler_budget_deferred = "handler_budget_deferred",
        not_selected = "not_selected",
        reference_only = "reference_only",
        gx_csv_orchestration_abort(
          "M7h encountered an unknown M7d coverage status."
        )
      )
    }
  }, character(1)))
  tibble::tibble(
    contract_version = rep.int(
      .gx_csv_orchestration_contract_version, nrow(coverage)
    ),
    selection_order = unname(coverage$selection_order),
    fetch_order = unname(coverage$fetch_order),
    distribution_id = unname(coverage$distribution_id),
    handler_id = unname(coverage$handler_id),
    logical_request_id = unname(coverage$logical_request_id),
    request_status = unname(coverage$request_status),
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

gx_csv_orchestration_result_id_impl <- function(result) {
  gx_contract_hash(
    list(
      "execution_id", result$execution$execution_id,
      "attempt_id", result$attempt$attempt_id[[1L]],
      "validation_id", result$validation$validation_id,
      "parse_id", result$parse$parse_id,
      "result_sha256", result$parse$result_sha256
    ),
    namespace = "geoconnexr.csv-orchestration-result.v1",
    contract_version = .gx_csv_orchestration_contract_version
  )
}

gx_csv_orchestration_compact_result_impl <- function(execution) {
  parsed <- execution$parsed_response
  result <- structure(
    list(
      result_id = "",
      execution = execution$execution,
      attempt = execution$attempts,
      validation = parsed$validated_response$validation,
      parse_policy = parsed$policy,
      schema = parsed$schema,
      data = parsed$data,
      parse = parsed$parse
    ),
    class = "gx_csv_orchestration_result"
  )
  result$result_id <- gx_csv_orchestration_result_id_impl(result)
  result
}

gx_csv_orchestration_failure_impl <- function(cnd) {
  if (!inherits(cnd, c(
    "gx_error_csv_execution_transport", "gx_error_csv_execution_parse"
  ))) {
    gx_csv_orchestration_abort(
      "A direct-CSV request failed outside an isolatable transport or parse phase.",
      "gx_error_csv_orchestration_execution"
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
  if (!gx_catalog_is_sha256(cnd$execution_id) || physical > 1L ||
      !is.finite(charged) || charged < 0) {
    gx_csv_orchestration_abort(
      "A failed direct-CSV request returned invalid redacted attempt facts.",
      "gx_error_csv_orchestration_execution"
    )
  }
  parse <- inherits(cnd, "gx_error_csv_execution_parse")
  list(
    status = if (parse) "parse_failed" else "transport_failed",
    error_code = if (parse) "csv_parse_failed" else "csv_transport_failed",
    physical_attempts = unname(as.integer(physical)),
    bytes = unname(as.double(charged)),
    execution_id = unname(cnd$execution_id)
  )
}

gx_csv_orchestration_run_impl <- function(
    request_plan, policy, orchestration_id, admitted, status) {
  if (policy$dry_run || !any(admitted)) {
    return(list(results = list(), status = status))
  }
  requests <- request_plan$request_plans
  results <- list()
  for (position in which(admitted)) {
    request <- requests[position, , drop = FALSE]
    logical_id <- request$logical_request_id[[1L]]
    coverage_position <- match(
      logical_id, status$logical_request_id
    )
    child_scope <- gx_csv_orchestration_child_scope_impl(
      orchestration_id, request$request_order[[1L]], logical_id
    )
    outcome <- tryCatch(
      gx_csv_execution_impl(
        request_plan = request_plan,
        logical_request_id = logical_id,
        max_fields = policy$max_fields,
        timeout = policy$timeout_seconds,
        min_interval = policy$min_interval_seconds,
        execution_scope_id = child_scope
      ),
      error = identity
    )
    status$attempted[[coverage_position]] <- TRUE
    if (inherits(outcome, "gx_csv_execution")) {
      result <- gx_csv_orchestration_compact_result_impl(outcome)
      results[[length(results) + 1L]] <- result
      status$orchestration_status[[coverage_position]] <-
        "provider_response_validated_and_parsed"
      status$succeeded[[coverage_position]] <- TRUE
      status$physical_attempts[[coverage_position]] <- 1L
      status$encoded_bytes[[coverage_position]] <-
        result$execution$encoded_bytes
      status$decoded_bytes[[coverage_position]] <-
        result$execution$decoded_bytes
      status$execution_id[[coverage_position]] <-
        result$execution$execution_id
      status$result_index[[coverage_position]] <- as.integer(length(results))
    } else {
      failure <- gx_csv_orchestration_failure_impl(outcome)
      status$orchestration_status[[coverage_position]] <- failure$status
      status$physical_attempts[[coverage_position]] <-
        failure$physical_attempts
      status$encoded_bytes[[coverage_position]] <- failure$bytes
      status$decoded_bytes[[coverage_position]] <- failure$bytes
      status$execution_id[[coverage_position]] <- failure$execution_id
      status$error_code[[coverage_position]] <- failure$error_code
    }
  }
  list(results = results, status = status)
}

gx_csv_orchestration_overall_status_impl <- function(policy, status) {
  if (policy$dry_run) return("dry_run_complete")
  if (any(status$orchestration_status %in% c(
    "transport_failed", "parse_failed"
  ))) return("execution_complete_with_failures")
  if (any(status$orchestration_status == "batch_limit_deferred")) {
    return("execution_complete_with_batch_deferral")
  }
  "execution_complete"
}

gx_csv_orchestration_owned_impl <- function(
    orchestration_scope_id, orchestration_id, policy, request_plan, admitted,
    status) {
  list(
    orchestration_scope_id = orchestration_scope_id,
    orchestration_id = orchestration_id,
    execution_mode = policy$execution_mode,
    orchestration_status = gx_csv_orchestration_overall_status_impl(
      policy, status
    ),
    planned_requests = unname(as.integer(nrow(request_plan$request_plans))),
    admitted_requests = unname(as.integer(sum(admitted))),
    completed_requests = unname(as.integer(sum(status$attempted)))
  )
}

gx_csv_orchestration_counts_impl <- function(status, results) {
  list(
    distributions = unname(as.integer(nrow(status))),
    csv_requests = unname(as.integer(sum(
      status$request_status == "csv_request_planned"
    ))),
    admitted_requests = unname(as.integer(sum(
      status$orchestration_status %in% c(
        "dry_run_planned", "provider_response_validated_and_parsed",
        "transport_failed", "parse_failed", "execution_pending"
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
    not_selected = unname(as.integer(sum(
      status$orchestration_status == "not_selected"
    ))),
    reference_only = unname(as.integer(sum(
      status$orchestration_status == "reference_only"
    ))),
    results = unname(as.integer(length(results)))
  )
}

gx_csv_orchestration_reasons_impl <- function(
    request_plan, policy, status) {
  reasons <- request_plan$metadata$non_replayable_reasons
  reasons <- setdiff(reasons, "timeout_policy_unbound")
  complete_success <- !policy$dry_run &&
    any(status$request_status == "csv_request_planned") &&
    !any(status$orchestration_status %in% c(
      "transport_failed", "parse_failed", "batch_limit_deferred",
      "execution_pending"
    ))
  if (complete_success) {
    reasons <- setdiff(reasons, c(
      "attempt_identity_unbound", "attempt_ledger_unbound",
      "csv_parser_enforcement_unimplemented",
      "csv_parser_semantics_unbound",
      "provider_transport_unauthorized", "response_origin_unbound",
      "response_validator_unimplemented", "result_schema_unbound",
      "transport_adapter_unimplemented"
    ))
  }
  if (policy$dry_run) reasons <- c(reasons, "dry_run_no_transport")
  if (any(status$orchestration_status == "batch_limit_deferred")) {
    reasons <- c(reasons, "csv_batch_limits_deferred")
  }
  if (any(status$orchestration_status %in% c(
    "transport_failed", "parse_failed"
  ))) {
    reasons <- c(reasons, "csv_execution_failures_present")
  }
  reasons <- unique(reasons)
  reasons[gx_catalog_byte_order(reasons)]
}

gx_csv_orchestration_metadata_impl <- function(
    request_plan, policy, status, results) {
  counts <- gx_csv_orchestration_counts_impl(status, results)
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
    csv_semantics_validated = counts$successful_requests > 0L,
    result_contract_bound = TRUE,
    status_reconciled = TRUE,
    continue_on_error = TRUE,
    results_compacted = TRUE,
    observation_origin = if (policy$dry_run) {
      "dry_run"
    } else if (counts$physical_attempts > 0L) {
      "provider_transport"
    } else {
      "orchestrator_no_physical_attempt"
    },
    counts = counts,
    non_replayable_reasons = gx_csv_orchestration_reasons_impl(
      request_plan, policy, status
    )
  )
}

gx_csv_orchestration_new_impl <- function(
    request_plan, policy, orchestration, results, status, metadata) {
  object <- structure(
    list(
      contract_version = .gx_csv_orchestration_contract_version,
      request_plan = request_plan,
      policy = policy,
      orchestration = orchestration,
      results = results,
      status = status,
      metadata = metadata
    ),
    class = "gx_csv_orchestration"
  )
  gx_csv_orchestration_validate_impl(object)
  object
}

gx_csv_orchestration_impl <- function(
    request_plan,
    dry_run = NULL,
    max_executions = NULL,
    max_total_bytes = NULL,
    max_fields = NULL,
    timeout = NULL,
    min_interval = NULL,
    orchestration_scope_id = NULL) {
  request_plan <- gx_csv_orchestration_input_plan_impl(request_plan)
  policy <- gx_csv_orchestration_policy_impl(
    dry_run, max_executions, max_total_bytes, max_fields, timeout,
    min_interval
  )
  orchestration_scope_id <- tryCatch(
    gx_csv_execution_scope_impl(orchestration_scope_id),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(orchestration_scope_id)) {
    gx_csv_orchestration_abort(
      "M7h requires one explicit opaque orchestration scope identity.",
      "gx_error_csv_orchestration_policy"
    )
  }
  orchestration_id <- gx_csv_orchestration_id_impl(
    orchestration_scope_id, request_plan, policy
  )
  admitted <- gx_csv_orchestration_admission_impl(request_plan, policy)
  status <- gx_csv_orchestration_status_impl(
    request_plan, policy, admitted
  )
  run <- gx_csv_orchestration_run_impl(
    request_plan, policy, orchestration_id, admitted, status
  )
  orchestration <- gx_csv_orchestration_owned_impl(
    orchestration_scope_id, orchestration_id, policy, request_plan,
    admitted, run$status
  )
  metadata <- gx_csv_orchestration_metadata_impl(
    request_plan, policy, run$status, run$results
  )
  gx_csv_orchestration_new_impl(
    request_plan, policy, orchestration, run$results, run$status, metadata
  )
}

gx_csv_orchestration_validate_policy_impl <- function(policy) {
  valid_shape <- is.list(policy) && identical(
    names(policy), .gx_csv_orchestration_policy_fields
  ) && gx_csv_orchestration_exact_attributes(policy, "names")
  expected <- if (valid_shape) tryCatch(
    gx_csv_orchestration_policy_impl(
      policy$dry_run, policy$max_executions, policy$max_total_bytes,
      policy$max_fields, policy$timeout_seconds,
      policy$min_interval_seconds
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  if (is.null(expected) || !identical(policy, expected) || !all(vapply(
    policy, function(value) is.null(attributes(value)), logical(1)
  ))) {
    gx_csv_orchestration_abort(
      "M7h policy violates its exact bounded contract."
    )
  }
  invisible(policy)
}

gx_csv_orchestration_validate_validation_impl <- function(
    validation, request_plan, request) {
  valid_shape <- is.list(validation) && identical(
    names(validation), .gx_csv_validated_response_validation_fields
  ) && gx_csv_orchestration_exact_attributes(validation, "names")
  target <- tryCatch(
    gx_csv_validated_response_target_impl(
      request_plan,
      match(
        request$logical_request_id[[1L]],
        request_plan$request_plans$logical_request_id
      )
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (!valid_shape || is.null(target)) {
    gx_csv_orchestration_abort(
      "A compact M7h result has invalid response-validation facts."
    )
  }
  scalar_ids <- c(
    "validation_id", "logical_request_id", "intent_id", "reservation_id",
    "distribution_id", "body_sha256"
  )
  valid_types <- all(vapply(
    validation[scalar_ids], gx_csv_orchestration_scalar_text, logical(1)
  )) && is.integer(validation$status) && length(validation$status) == 1L &&
    is.logical(validation$content_length_present) &&
    length(validation$content_length_present) == 1L &&
    is.integer(validation$content_length) &&
    length(validation$content_length) == 1L &&
    is.integer(validation$encoded_bytes) &&
    length(validation$encoded_bytes) == 1L &&
    is.integer(validation$decoded_bytes) &&
    length(validation$decoded_bytes) == 1L &&
    all(vapply(validation[c(
      "media_type", "content_encoding", "validation_status"
    )], gx_csv_orchestration_scalar_text, logical(1))) &&
    !is.na(validation$content_length_present) &&
    !is.na(validation$encoded_bytes) && !is.na(validation$decoded_bytes) &&
    all(vapply(
      validation, function(value) is.null(attributes(value)), logical(1)
    ))
  expected_content_length <- if (isTRUE(validation$content_length_present)) {
    validation$encoded_bytes
  } else {
    NA_integer_
  }
  expected_id <- if (valid_types) gx_csv_validated_response_validation_id_impl(
    request = request,
    canonical_target = target$url,
    status = validation$status,
    media_type = validation$media_type,
    content_encoding = validation$content_encoding,
    content_length_present = validation$content_length_present,
    content_length = validation$content_length,
    encoded_bytes = validation$encoded_bytes,
    decoded_bytes = validation$decoded_bytes,
    body_sha256 = validation$body_sha256
  ) else NA_character_
  values_ok <- valid_types && validation$status == 200L &&
    validation$logical_request_id == request$logical_request_id[[1L]] &&
    validation$intent_id == request$intent_id[[1L]] &&
    validation$reservation_id == request$reservation_id[[1L]] &&
    validation$distribution_id == request$distribution_id[[1L]] &&
    validation$media_type %in% .gx_csv_validated_response_media_types &&
    validation$content_encoding == "identity" &&
    identical(validation$content_length, expected_content_length) &&
    validation$encoded_bytes == validation$decoded_bytes &&
    validation$encoded_bytes >= 0L &&
    validation$encoded_bytes <= request$response_byte_limit[[1L]] &&
    validation$validation_status == "validated_caller_supplied" &&
    gx_catalog_is_sha256(validation$body_sha256) &&
    identical(validation$validation_id, expected_id)
  if (!values_ok) {
    gx_csv_orchestration_abort(
      "A compact M7h result no longer rebinds to its validated response facts."
    )
  }
  invisible(target$url)
}

gx_csv_orchestration_validate_result_impl <- function(
    result, request_plan, policy) {
  valid_top <- is.list(result) && identical(
    names(result), .gx_csv_orchestration_result_fields
  ) && identical(class(result), "gx_csv_orchestration_result") &&
    gx_csv_orchestration_exact_attributes(result, c("names", "class")) &&
    gx_catalog_is_sha256(result$result_id)
  if (!valid_top) {
    gx_csv_orchestration_abort(
      "A compact M7h result violates its exact top-level contract."
    )
  }
  execution <- result$execution
  valid_execution <- is.list(execution) && identical(
    names(execution), .gx_csv_execution_execution_fields
  ) && gx_csv_orchestration_exact_attributes(execution, "names")
  execution_character <- c(
    "execution_scope_id", "execution_id", "logical_request_id",
    "reservation_id", "distribution_id", "started_at", "completed_at",
    "execution_status"
  )
  execution_numeric <- c(
    "timeout_seconds", "min_interval_seconds", "encoded_bytes",
    "decoded_bytes"
  )
  valid_execution <- valid_execution && all(vapply(
    execution[execution_character],
    gx_csv_orchestration_scalar_text,
    logical(1)
  )) && all(vapply(execution[execution_numeric], function(value) {
    is.numeric(value) && length(value) == 1L && !is.na(value) &&
      is.finite(value) && is.null(attributes(value))
  }, logical(1))) && is.integer(execution$physical_attempt_count) &&
    length(execution$physical_attempt_count) == 1L &&
    !is.na(execution$physical_attempt_count) &&
    is.null(attributes(execution$physical_attempt_count))
  if (!valid_execution || !gx_catalog_is_sha256(execution$execution_id) ||
      !gx_catalog_is_sha256(execution$execution_scope_id) ||
      !gx_catalog_is_sha256(execution$logical_request_id)) {
    gx_csv_orchestration_abort(
      "A compact M7h result has invalid execution facts."
    )
  }
  request_position <- match(
    execution$logical_request_id,
    request_plan$request_plans$logical_request_id
  )
  if (is.na(request_position)) {
    gx_csv_orchestration_abort(
      "A compact M7h result references a foreign logical request."
    )
  }
  request <- request_plan$request_plans[request_position, , drop = FALSE]
  target <- gx_csv_orchestration_validate_validation_impl(
    result$validation, request_plan, request
  )
  expected_execution_id <- gx_csv_execution_id_impl(
    execution$execution_scope_id, request, execution$started_at,
    execution$timeout_seconds, execution$min_interval_seconds
  )
  started <- gx_csv_execution_parse_time_impl(execution$started_at)
  completed <- gx_csv_execution_parse_time_impl(execution$completed_at)
  execution_ok <- !is.na(started) && !is.na(completed) && completed >= started &&
    execution$logical_request_id == request$logical_request_id[[1L]] &&
    execution$reservation_id == request$reservation_id[[1L]] &&
    execution$distribution_id == request$distribution_id[[1L]] &&
    execution$timeout_seconds == policy$timeout_seconds &&
    execution$min_interval_seconds == policy$min_interval_seconds &&
    execution$encoded_bytes == result$validation$encoded_bytes &&
    execution$decoded_bytes == result$validation$decoded_bytes &&
    execution$physical_attempt_count == 1L &&
    execution$execution_status == "provider_response_validated_and_parsed" &&
    identical(execution$execution_id, expected_execution_id)
  if (!execution_ok) {
    gx_csv_orchestration_abort(
      "A compact M7h result no longer rebinds to its execution policy."
    )
  }
  attempt <- result$attempt
  valid_attempt <- inherits(attempt, "tbl_df") && nrow(attempt) == 1L &&
    identical(names(attempt), .gx_csv_execution_attempt_columns) &&
    gx_csv_parsed_response_table_attributes(attempt, 1L) && !anyNA(attempt)
  attempt_character <- setdiff(
    .gx_csv_execution_attempt_columns,
    c("attempt_number", "status", "encoded_bytes", "decoded_bytes")
  )
  valid_attempt <- valid_attempt && is.integer(attempt$attempt_number) &&
    is.integer(attempt$status) && is.numeric(attempt$encoded_bytes) &&
    is.numeric(attempt$decoded_bytes) && all(vapply(
      attempt[attempt_character], is.character, logical(1)
    )) && all(vapply(
      attempt, function(value) is.null(attributes(value)), logical(1)
    ))
  attempt_completed <- if (valid_attempt) {
    gx_csv_execution_parse_time_impl(attempt$completed_at[[1L]])
  } else as.POSIXct(NA, tz = "UTC")
  expected_attempt_id <- if (valid_attempt) gx_csv_execution_attempt_id_impl(
    execution$execution_id, request, target, attempt
  ) else NA_character_
  expected_host <- gx_safe_target(target, resolve_dns = FALSE)$host
  attempt_ok <- valid_attempt && !is.na(attempt_completed) &&
    attempt$contract_version[[1L]] == .gx_csv_execution_contract_version &&
    completed >= attempt_completed && attempt$attempt_number[[1L]] == 1L &&
    attempt$execution_id[[1L]] == execution$execution_id &&
    attempt$logical_request_id[[1L]] == request$logical_request_id[[1L]] &&
    attempt$reservation_id[[1L]] == request$reservation_id[[1L]] &&
    attempt$method[[1L]] == "GET" &&
    attempt$canonical_url_redacted[[1L]] == gx_redact_url(target) &&
    attempt$resolved_host[[1L]] == expected_host &&
    gx_is_ipv4(attempt$resolved_ip[[1L]]) &&
    !gx_is_nonpublic_ipv4(attempt$resolved_ip[[1L]]) &&
    attempt$status[[1L]] == 200L && attempt$outcome[[1L]] == "response" &&
    attempt$media_type[[1L]] == result$validation$media_type &&
    attempt$encoded_bytes[[1L]] == result$validation$encoded_bytes &&
    attempt$decoded_bytes[[1L]] == result$validation$decoded_bytes &&
    attempt$body_sha256[[1L]] == result$validation$body_sha256 &&
    identical(attempt$attempt_id[[1L]], expected_attempt_id)
  if (!attempt_ok) {
    gx_csv_orchestration_abort(
      "A compact M7h result has an invalid charged attempt ledger row."
    )
  }
  expected_parse_policy <- tryCatch(
    gx_csv_parsed_response_policy_impl(request, policy$max_fields),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  valid_owned <- tryCatch({
    gx_csv_parsed_response_assert_owned_shape_impl(list(
      schema = result$schema, data = result$data
    ))
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  expected_schema <- if (valid_owned) {
    gx_csv_parsed_response_schema_impl(result$data)
  } else NULL
  expected_hash <- if (valid_owned) {
    gx_csv_parsed_response_result_hash_impl(result$data)
  } else NA_character_
  parse <- result$parse
  valid_parse <- is.list(parse) && identical(
    names(parse), .gx_csv_parsed_response_parse_fields
  ) && gx_csv_orchestration_exact_attributes(parse, "names")
  parse_character <- c(
    "parse_id", "validation_id", "body_sha256", "result_sha256",
    "parse_status"
  )
  parse_integer <- c("row_count", "column_count", "field_count")
  valid_parse <- valid_parse && all(vapply(
    parse[parse_character], gx_csv_orchestration_scalar_text, logical(1)
  )) && all(vapply(parse[parse_integer], function(value) {
    is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L &&
      is.null(attributes(value))
  }, logical(1))) && is.logical(parse$bom_present) &&
    length(parse$bom_present) == 1L && !is.na(parse$bom_present) &&
    is.null(attributes(parse$bom_present)) &&
    gx_catalog_is_sha256(parse$parse_id) &&
    gx_catalog_is_sha256(parse$validation_id) &&
    gx_catalog_is_sha256(parse$body_sha256) &&
    gx_catalog_is_sha256(parse$result_sha256)
  scan <- if (valid_parse) list(
    bom_present = parse$bom_present,
    row_count = parse$row_count,
    column_count = parse$column_count,
    field_count = parse$field_count
  ) else NULL
  expected_parse_id <- if (valid_parse && !is.null(expected_parse_policy)) {
    gx_csv_parsed_response_parse_id_impl(
      list(validation = result$validation), expected_parse_policy, scan,
      expected_hash
    )
  } else NA_character_
  parse_ok <- valid_owned && valid_parse &&
    identical(result$parse_policy, expected_parse_policy) &&
    identical(result$schema, expected_schema) &&
    parse$validation_id == result$validation$validation_id &&
    parse$body_sha256 == result$validation$body_sha256 &&
    identical(parse$result_sha256, expected_hash) &&
    parse$row_count == nrow(result$data) &&
    parse$column_count == ncol(result$data) &&
    parse$field_count == (nrow(result$data) + 1L) * ncol(result$data) &&
    parse$parse_status == "parsed_caller_supplied_validated_response" &&
    identical(parse$parse_id, expected_parse_id)
  if (!parse_ok || !identical(
    result$result_id, gx_csv_orchestration_result_id_impl(result)
  )) {
    gx_csv_orchestration_abort(
      "A compact M7h result no longer rebinds to its parsed character table."
    )
  }
  invisible(result)
}

gx_csv_orchestration_validate_status_impl <- function(
    status, request_plan, policy, admitted, results) {
  rows <- gx_catalog_table_rows(status)
  valid_shape <- inherits(status, "tbl_df") &&
    identical(class(status), c("tbl_df", "tbl", "data.frame")) &&
    identical(names(status), .gx_csv_orchestration_status_columns) &&
    !is.null(rows) && rows == nrow(request_plan$coverage) &&
    gx_csv_request_plan_table_attributes(status, as.integer(rows))
  integer_columns <- c(
    "selection_order", "fetch_order", "physical_attempts", "result_index"
  )
  numeric_columns <- c("encoded_bytes", "decoded_bytes")
  logical_columns <- c("attempted", "succeeded")
  character_columns <- setdiff(
    .gx_csv_orchestration_status_columns,
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
    gx_csv_orchestration_abort(
      "M7h status violates its exact one-row-per-distribution shape."
    )
  }
  base <- gx_csv_orchestration_status_impl(request_plan, policy, admitted)
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
    gx_csv_orchestration_abort(
      "M7h status identities or bounded attempt facts are invalid."
    )
  }
  nonadmitted <- base$orchestration_status != "execution_pending"
  if (policy$dry_run) nonadmitted <- rep.int(TRUE, nrow(base))
  if (any(status$orchestration_status[nonadmitted] !=
      base$orchestration_status[nonadmitted]) ||
      any(status$attempted[nonadmitted]) || any(status$succeeded[nonadmitted]) ||
      any(status$physical_attempts[nonadmitted] != 0L) ||
      any(status$encoded_bytes[nonadmitted] != 0) ||
      any(status$decoded_bytes[nonadmitted] != 0) ||
      any(!is.na(status$execution_id[nonadmitted])) ||
      any(!is.na(status$result_index[nonadmitted])) ||
      any(!is.na(status$error_code[nonadmitted]))) {
    gx_csv_orchestration_abort(
      "M7h non-attempted rows overstate execution or budget facts."
    )
  }
  if (!policy$dry_run) {
    live <- which(base$orchestration_status == "execution_pending")
    admitted_statuses <- c(
      "provider_response_validated_and_parsed", "transport_failed",
      "parse_failed"
    )
    if (any(!status$orchestration_status[live] %in% admitted_statuses) ||
        any(!status$attempted[live]) ||
        any(!gx_catalog_is_sha256(status$execution_id[live]))) {
      gx_csv_orchestration_abort(
        "Every admitted M7h request must reach one terminal status."
      )
    }
    successful <- which(status$succeeded)
    failed <- which(status$attempted & !status$succeeded)
    result_indices <- status$result_index[successful]
    if (!identical(result_indices, as.integer(seq_along(results))) ||
        length(successful) != length(results) ||
        any(status$orchestration_status[successful] !=
          "provider_response_validated_and_parsed") ||
        any(status$physical_attempts[successful] != 1L) ||
        any(!is.na(status$error_code[successful])) ||
        any(status$orchestration_status[failed] == "transport_failed" &
          status$error_code[failed] != "csv_transport_failed") ||
        any(status$orchestration_status[failed] == "parse_failed" &
          status$error_code[failed] != "csv_parse_failed") ||
        any(!is.na(status$result_index[failed]))) {
      gx_csv_orchestration_abort(
        "M7h terminal rows do not reconcile exactly with compact results."
      )
    }
    if (length(results)) {
      for (index in seq_along(results)) {
        row <- successful[[index]]
        result <- results[[index]]
        if (status$execution_id[[row]] != result$execution$execution_id ||
            status$logical_request_id[[row]] !=
              result$execution$logical_request_id ||
            status$encoded_bytes[[row]] != result$execution$encoded_bytes ||
            status$decoded_bytes[[row]] != result$execution$decoded_bytes) {
          gx_csv_orchestration_abort(
            "M7h successful status rows do not bind their compact results."
          )
        }
      }
    }
  }
  request_rows <- match(
    status$logical_request_id,
    request_plan$request_plans$logical_request_id
  )
  planned <- which(status$request_status == "csv_request_planned")
  if (any(status$encoded_bytes[planned] >
      request_plan$request_plans$max_encoded_bytes[request_rows[planned]]) ||
      any(status$decoded_bytes[planned] >
        request_plan$request_plans$max_decoded_bytes[request_rows[planned]]) ||
      sum(status$encoded_bytes) > request_plan$budgets$reserved_encoded_bytes ||
      sum(status$decoded_bytes) > request_plan$budgets$reserved_decoded_bytes ||
      sum(status$decoded_bytes) > policy$max_total_bytes) {
    gx_csv_orchestration_abort(
      "M7h status exceeds an allocated request or aggregate byte budget.",
      "gx_error_csv_orchestration_budget"
    )
  }
  invisible(status)
}

gx_csv_orchestration_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(
    names(x), .gx_csv_orchestration_fields
  ) && identical(class(x), "gx_csv_orchestration") &&
    gx_csv_orchestration_exact_attributes(x, c("names", "class")) &&
    identical(x$contract_version, .gx_csv_orchestration_contract_version) &&
    is.null(attributes(x$contract_version))
  if (!valid_top) {
    gx_csv_orchestration_abort(
      "M7h orchestration violates its exact top-level contract."
    )
  }
  gx_csv_orchestration_input_plan_impl(x$request_plan)
  gx_csv_orchestration_validate_policy_impl(x$policy)
  orchestration <- x$orchestration
  valid_orchestration <- is.list(orchestration) && identical(
    names(orchestration), .gx_csv_orchestration_fields_owned
  ) && gx_csv_orchestration_exact_attributes(orchestration, "names") &&
    gx_catalog_is_sha256(orchestration$orchestration_scope_id) &&
    gx_catalog_is_sha256(orchestration$orchestration_id)
  admitted <- gx_csv_orchestration_admission_impl(
    x$request_plan, x$policy
  )
  if (!valid_orchestration || !identical(
    orchestration$orchestration_id,
    gx_csv_orchestration_id_impl(
      orchestration$orchestration_scope_id, x$request_plan, x$policy
    )
  )) {
    gx_csv_orchestration_abort(
      "M7h orchestration identity no longer binds its plan and policy."
    )
  }
  if (!is.list(x$results) || !is.null(attributes(x$results)) ||
      length(x$results) > x$policy$max_executions) {
    gx_csv_orchestration_abort(
      "M7h compact results violate their list or cardinality budget."
    )
  }
  if (length(x$results)) {
    for (result in x$results) {
      gx_csv_orchestration_validate_result_impl(
        result, x$request_plan, x$policy
      )
      request_position <- match(
        result$execution$logical_request_id,
        x$request_plan$request_plans$logical_request_id
      )
      expected_scope <- gx_csv_orchestration_child_scope_impl(
        orchestration$orchestration_id,
        x$request_plan$request_plans$request_order[[request_position]],
        result$execution$logical_request_id
      )
      if (!identical(result$execution$execution_scope_id, expected_scope)) {
        gx_csv_orchestration_abort(
          "A compact M7h result has a foreign child execution scope."
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
      gx_csv_orchestration_abort(
        "M7h compact result identities must be unique."
      )
    }
  }
  gx_csv_orchestration_validate_status_impl(
    x$status, x$request_plan, x$policy, admitted, x$results
  )
  expected_orchestration <- gx_csv_orchestration_owned_impl(
    orchestration$orchestration_scope_id,
    orchestration$orchestration_id,
    x$policy,
    x$request_plan,
    admitted,
    x$status
  )
  expected_metadata <- gx_csv_orchestration_metadata_impl(
    x$request_plan, x$policy, x$status, x$results
  )
  valid_metadata <- is.list(x$metadata) && identical(
    names(x$metadata), .gx_csv_orchestration_metadata_fields
  ) && gx_csv_orchestration_exact_attributes(x$metadata, "names") &&
    is.list(x$metadata$counts) && identical(
      names(x$metadata$counts), .gx_csv_orchestration_count_fields
    ) && gx_csv_orchestration_exact_attributes(x$metadata$counts, "names")
  if (!identical(orchestration, expected_orchestration) || !valid_metadata ||
      !identical(x$metadata, expected_metadata)) {
    gx_csv_orchestration_abort(
      "M7h orchestration metadata or completion facts are inconsistent."
    )
  }
  owned_text <- gx_fetch_plan_text_total(
    x,
    limit = .gx_csv_orchestration_max_text_bytes
  )
  if (!is.finite(owned_text) ||
      owned_text > .gx_csv_orchestration_max_text_bytes) {
    gx_csv_orchestration_abort(
      "M7h owned text exceeds its aggregate byte budget.",
      "gx_error_csv_orchestration_budget"
    )
  }
  invisible(x)
}

#' @export
print.gx_csv_orchestration <- function(x, ...) {
  gx_csv_orchestration_validate_impl(x)
  counts <- x$metadata$counts
  cli::cli_inform(c(
    "<gx_csv_orchestration>",
    paste0(
      "* Mode: ", x$policy$execution_mode, "; CSV requests admitted: ",
      counts$admitted_requests, "/", counts$csv_requests
    ),
    paste0(
      "* Successful: ", counts$successful_requests,
      "; failed: ", counts$failed_requests,
      "; physical attempts: ", counts$physical_attempts
    ),
    paste0(
      "* Status rows: ", counts$distributions,
      "; compact results: ", counts$results,
      "; replayable: FALSE"
    )
  ))
  invisible(x)
}
