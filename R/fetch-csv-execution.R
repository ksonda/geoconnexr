.gx_csv_execution_contract_version <- "0.1.0"
.gx_csv_execution_max_text_bytes <- 2L * 1024L^2

.gx_csv_execution_fields <- c(
  "contract_version", "parsed_response", "execution", "attempts", "metadata"
)

.gx_csv_execution_execution_fields <- c(
  "execution_scope_id", "execution_id", "logical_request_id",
  "reservation_id", "distribution_id", "started_at", "completed_at",
  "timeout_seconds", "min_interval_seconds", "encoded_bytes",
  "decoded_bytes", "physical_attempt_count", "execution_status"
)

.gx_csv_execution_attempt_columns <- c(
  "contract_version", "attempt_number", "attempt_id", "execution_id",
  "logical_request_id", "reservation_id", "method",
  "canonical_url_redacted", "resolved_host", "resolved_ip", "status",
  "outcome", "media_type", "encoded_bytes", "decoded_bytes",
  "body_sha256", "completed_at"
)

.gx_csv_execution_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "execution_completed", "provider_response_observed", "budgets_consumed",
  "response_candidate_validated", "parser_executed",
  "csv_semantics_validated", "result_contract_bound",
  "attempt_ledger_bound", "observation_origin", "counts",
  "non_replayable_reasons"
)

.gx_csv_execution_count_fields <- c(
  "logical_requests", "physical_attempts", "successful_attempts",
  "encoded_bytes", "decoded_bytes"
)

gx_csv_execution_abort <- function(
    message,
    class = "gx_error_csv_execution_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_csv_execution", "gx_error_fetch_plan"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_csv_execution_exact_attributes <- function(x, expected) {
  observed <- names(attributes(x))
  is.character(observed) && !anyNA(observed) &&
    length(observed) == length(expected) && all(expected %in% observed)
}

gx_csv_execution_scalar_text <- function(x, nonempty = TRUE) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    (!nonempty || nzchar(x)) && is.null(attributes(x))
}

gx_csv_execution_scope_impl <- function(x) {
  if (!gx_csv_execution_scalar_text(x) ||
      !grepl("^[0-9a-f]{64}$", x, perl = TRUE)) {
    gx_csv_execution_abort(
      "An execution scope must be one opaque 64-lowercase-hex identity.",
      "gx_error_csv_execution_scope"
    )
  }
  unname(x)
}

gx_csv_execution_number_impl <- function(
    x, name, minimum, maximum, allow_zero = FALSE) {
  lower <- if (allow_zero) minimum else max(minimum, .Machine$double.eps)
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < lower || x > maximum || !is.null(attributes(x))) {
    gx_csv_execution_abort(
      "Direct-CSV execution timing limits must be explicit finite numbers.",
      "gx_error_csv_execution_policy"
    )
  }
  unname(as.double(x))
}

gx_csv_execution_time_impl <- function(x) {
  value <- tryCatch(
    as.POSIXct(x, tz = "UTC"),
    error = function(cnd) as.POSIXct(NA, tz = "UTC")
  )
  if (length(value) != 1L || is.na(value)) {
    gx_csv_execution_abort(
      "The execution clock returned an invalid UTC instant.",
      "gx_error_csv_execution_clock"
    )
  }
  unname(format(value, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
}

gx_csv_execution_parse_time_impl <- function(x) {
  if (!gx_csv_execution_scalar_text(x) ||
      !grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{6}Z$",
        x,
        perl = TRUE
      )) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  value <- suppressWarnings(as.POSIXct(
    x,
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  ))
  if (length(value) != 1L || is.na(value) ||
      !identical(gx_csv_execution_time_impl(value), x)) {
    as.POSIXct(NA, tz = "UTC")
  } else {
    value
  }
}

gx_csv_execution_request_impl <- function(request_plan, logical_request_id) {
  valid <- tryCatch({
    gx_csv_request_plan_validate_impl(request_plan)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid) {
    gx_csv_execution_abort(
      "M7g construction requires one valid M7d direct-CSV request plan.",
      "gx_error_csv_execution_input"
    )
  }
  position <- tryCatch(
    gx_csv_validated_response_request_row_impl(
      request_plan, logical_request_id
    ),
    error = function(cnd) NA_integer_,
    warning = function(cnd) NA_integer_
  )
  if (length(position) != 1L || is.na(position)) {
    gx_csv_execution_abort(
      "M7g must select one existing direct-CSV logical request.",
      "gx_error_csv_execution_input"
    )
  }
  request <- request_plan$request_plans[position, , drop = FALSE]
  target <- tryCatch(
    gx_csv_validated_response_target_impl(request_plan, position),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(target)) {
    gx_csv_execution_abort(
      "The direct-CSV execution target no longer rebinds to its request plan.",
      "gx_error_csv_execution_input"
    )
  }
  list(
    position = unname(as.integer(position)),
    request = request,
    target = target$url
  )
}

gx_csv_execution_id_impl <- function(
    execution_scope_id, request, started_at, timeout_seconds,
    min_interval_seconds) {
  gx_contract_hash(
    list(
      "execution_scope_id", execution_scope_id,
      "logical_request_id", request$logical_request_id[[1L]],
      "reservation_id", request$reservation_id[[1L]],
      "distribution_id", request$distribution_id[[1L]],
      "started_at", started_at,
      "timeout_seconds", sprintf("%.6f", timeout_seconds),
      "min_interval_seconds", sprintf("%.6f", min_interval_seconds)
    ),
    namespace = "geoconnexr.csv-execution.v1",
    contract_version = .gx_csv_execution_contract_version
  )
}

gx_csv_execution_attempt_id_impl <- function(
    execution_id, request, target, attempt) {
  gx_contract_hash(
    list(
      "execution_id", execution_id,
      "attempt_number", attempt$attempt_number[[1L]],
      "logical_request_id", request$logical_request_id[[1L]],
      "reservation_id", request$reservation_id[[1L]],
      "canonical_target", target,
      "resolved_host", attempt$resolved_host[[1L]],
      "resolved_ip", attempt$resolved_ip[[1L]],
      "method", attempt$method[[1L]],
      "status", attempt$status[[1L]],
      "outcome", attempt$outcome[[1L]],
      "media_type", attempt$media_type[[1L]],
      "encoded_bytes", gx_csv_request_plan_byte_hash_value(
        attempt$encoded_bytes[[1L]]
      ),
      "decoded_bytes", gx_csv_request_plan_byte_hash_value(
        attempt$decoded_bytes[[1L]]
      ),
      "body_sha256", attempt$body_sha256[[1L]],
      "completed_at", attempt$completed_at[[1L]]
    ),
    namespace = "geoconnexr.csv-attempt.v1",
    contract_version = .gx_csv_execution_contract_version
  )
}

gx_csv_execution_reasons_impl <- function(parsed_response) {
  remove <- c(
    "attempt_identity_unbound",
    "attempt_ledger_unbound",
    "provider_transport_unauthorized",
    "response_origin_unbound",
    "timeout_policy_unbound",
    "transport_adapter_unimplemented"
  )
  reasons <- setdiff(
    parsed_response$metadata$non_replayable_reasons, remove
  )
  reasons[gx_catalog_byte_order(reasons)]
}

gx_csv_execution_counts_impl <- function(bytes) {
  list(
    logical_requests = 1L,
    physical_attempts = 1L,
    successful_attempts = 1L,
    encoded_bytes = unname(as.double(bytes)),
    decoded_bytes = unname(as.double(bytes))
  )
}

gx_csv_execution_metadata_impl <- function(parsed_response, bytes) {
  list(
    host_specific = TRUE,
    replayable = FALSE,
    execution_ready = FALSE,
    transport_authorized = TRUE,
    execution_completed = TRUE,
    provider_response_observed = TRUE,
    budgets_consumed = TRUE,
    response_candidate_validated = TRUE,
    parser_executed = TRUE,
    csv_semantics_validated = TRUE,
    result_contract_bound = TRUE,
    attempt_ledger_bound = TRUE,
    observation_origin = "provider_transport",
    counts = gx_csv_execution_counts_impl(bytes),
    non_replayable_reasons = gx_csv_execution_reasons_impl(parsed_response)
  )
}

gx_csv_execution_attempts_impl <- function(
    response, parsed_response, execution_id, request, target) {
  source <- response$attempts
  validation <- parsed_response$validated_response$validation
  bytes <- unname(as.double(length(parsed_response$validated_response$body)))
  valid_source <- is.data.frame(source) && nrow(source) == 1L &&
    isTRUE(source$physical[[1L]]) && source$attempt[[1L]] == 1L &&
    identical(source$outcome[[1L]], "response") &&
    identical(source$cache_origin[[1L]], "network") &&
    identical(source$method[[1L]], request$method[[1L]]) &&
    identical(source$url[[1L]], gx_redact_url(target)) &&
    identical(source$status[[1L]], 200L) &&
    identical(source$media_type[[1L]], validation$media_type) &&
    identical(source$bytes[[1L]], bytes) &&
    identical(source$charged_bytes[[1L]], bytes) &&
    identical(source$body_sha256[[1L]], validation$body_sha256) &&
    identical(source$resolved_host[[1L]],
              gx_safe_target(target, resolve_dns = FALSE)$host) &&
    gx_is_ipv4(source$resolved_ip[[1L]]) &&
    !gx_is_nonpublic_ipv4(source$resolved_ip[[1L]])
  if (!valid_source) {
    gx_csv_execution_abort(
      "The package transport did not return exactly one physical response attempt.",
      "gx_error_csv_execution_attempt"
    )
  }
  attempt <- tibble::tibble(
    contract_version = .gx_csv_execution_contract_version,
    attempt_number = 1L,
    attempt_id = "",
    execution_id = execution_id,
    logical_request_id = request$logical_request_id[[1L]],
    reservation_id = request$reservation_id[[1L]],
    method = source$method[[1L]],
    canonical_url_redacted = gx_redact_url(target),
    resolved_host = source$resolved_host[[1L]],
    resolved_ip = source$resolved_ip[[1L]],
    status = source$status[[1L]],
    outcome = source$outcome[[1L]],
    media_type = parsed_response$validated_response$validation$media_type,
    encoded_bytes = bytes,
    decoded_bytes = bytes,
    body_sha256 = parsed_response$validated_response$validation$body_sha256,
    completed_at = gx_csv_execution_time_impl(source$retrieved_at[[1L]])
  )
  attempt$attempt_id[[1L]] <- gx_csv_execution_attempt_id_impl(
    execution_id, request, target, attempt
  )
  attempt
}

gx_csv_execution_execution_impl <- function(
    execution_scope_id, execution_id, request, started_at, completed_at,
    timeout_seconds, min_interval_seconds, bytes) {
  list(
    execution_scope_id = execution_scope_id,
    execution_id = execution_id,
    logical_request_id = request$logical_request_id[[1L]],
    reservation_id = request$reservation_id[[1L]],
    distribution_id = request$distribution_id[[1L]],
    started_at = started_at,
    completed_at = completed_at,
    timeout_seconds = timeout_seconds,
    min_interval_seconds = min_interval_seconds,
    encoded_bytes = unname(as.double(bytes)),
    decoded_bytes = unname(as.double(bytes)),
    physical_attempt_count = 1L,
    execution_status = "provider_response_validated_and_parsed"
  )
}

gx_csv_execution_new_impl <- function(
    parsed_response, execution, attempts, metadata) {
  object <- structure(
    list(
      contract_version = .gx_csv_execution_contract_version,
      parsed_response = parsed_response,
      execution = execution,
      attempts = attempts,
      metadata = metadata
    ),
    class = "gx_csv_execution"
  )
  gx_csv_execution_validate_impl(object)
  object
}

gx_csv_execution_redacted_failure_attempts <- function(x) {
  if (!is.data.frame(x) || !nrow(x)) return(NULL)
  keep <- intersect(
    c(
      "attempt", "method", "url", "resolved_host", "resolved_ip", "status",
      "outcome", "physical", "error_code", "charged_bytes", "retrieved_at"
    ),
    names(x)
  )
  x[, keep, drop = FALSE]
}

gx_csv_execution_fail_impl <- function(cnd, phase, execution_id) {
  attempts <- gx_csv_execution_redacted_failure_attempts(cnd$attempts)
  gx_csv_execution_abort(
    "Direct-CSV execution failed during its bounded {phase} phase; underlying details were withheld.",
    paste0("gx_error_csv_execution_", phase),
    execution_id = execution_id,
    attempts = attempts
  )
}

gx_csv_execution_impl <- function(
    request_plan,
    logical_request_id,
    max_fields = NULL,
    timeout = NULL,
    min_interval = NULL,
    execution_scope_id = NULL) {
  selected <- gx_csv_execution_request_impl(
    request_plan, logical_request_id
  )
  request <- selected$request
  target <- selected$target
  execution_scope_id <- gx_csv_execution_scope_impl(execution_scope_id)
  timeout <- gx_csv_execution_number_impl(
    timeout, "timeout", 0, 600, allow_zero = FALSE
  )
  min_interval <- gx_csv_execution_number_impl(
    min_interval, "min_interval", 0, 3600, allow_zero = TRUE
  )
  max_fields <- gx_csv_parsed_response_field_limit_impl(max_fields)
  started_at <- gx_csv_execution_time_impl(gx_now())
  execution_id <- gx_csv_execution_id_impl(
    execution_scope_id,
    request,
    started_at,
    timeout,
    min_interval
  )
  max_bytes <- as.integer(min(
    request$response_byte_limit[[1L]],
    request$max_encoded_bytes[[1L]],
    request$max_decoded_bytes[[1L]]
  ))
  client <- gx_client(
    endpoint = "pid",
    timeout = timeout,
    retries = 0L,
    min_interval = min_interval,
    max_bytes = max_bytes,
    cache = FALSE,
    offline = FALSE,
    cache_dir = tempdir()
  )
  validated <- NULL
  response_validator <- function(response) {
    candidate <- list(
      status = response$status,
      headers = response$headers,
      body = response$body,
      url = response$url
    )
    validated <<- gx_csv_validated_response_impl(
      request_plan,
      logical_request_id,
      candidate
    )
    invisible(NULL)
  }
  response <- tryCatch(
    gx_http_request(
      client,
      method = request$method[[1L]],
      url = target,
      headers = list(
        Accept = request_plan$policy$accept,
        `Accept-Encoding` = request_plan$policy$accept_encoding
      ),
      body = raw(),
      check_status = FALSE,
      .response_validator = response_validator
    ),
    error = function(cnd) gx_csv_execution_fail_impl(
      cnd, "transport", execution_id
    )
  )
  if (is.null(validated)) {
    gx_csv_execution_abort(
      "The provider response was not bound by the M7e validator.",
      "gx_error_csv_execution_response",
      execution_id = execution_id
    )
  }
  parsed <- tryCatch(
    gx_csv_parsed_response_impl(validated, max_fields = max_fields),
    error = function(cnd) {
      cnd$attempts <- response$attempts
      gx_csv_execution_fail_impl(cnd, "parse", execution_id)
    }
  )
  completed_at <- gx_csv_execution_time_impl(gx_now())
  attempts <- gx_csv_execution_attempts_impl(
    response, parsed, execution_id, request, target
  )
  bytes <- length(parsed$validated_response$body)
  execution <- gx_csv_execution_execution_impl(
    execution_scope_id,
    execution_id,
    request,
    started_at,
    completed_at,
    timeout,
    min_interval,
    bytes
  )
  gx_csv_execution_new_impl(
    parsed_response = parsed,
    execution = execution,
    attempts = attempts,
    metadata = gx_csv_execution_metadata_impl(parsed, bytes)
  )
}

gx_csv_execution_validate_attempts_impl <- function(
    attempts, execution, parsed_response, request, target) {
  valid_shape <- inherits(attempts, "tbl_df") && nrow(attempts) == 1L &&
    identical(names(attempts), .gx_csv_execution_attempt_columns) &&
    gx_csv_parsed_response_table_attributes(attempts, 1L) &&
    all(vapply(attempts, function(column) {
      is.null(attributes(column))
    }, logical(1)))
  character_columns <- setdiff(
    .gx_csv_execution_attempt_columns,
    c("attempt_number", "status", "encoded_bytes", "decoded_bytes")
  )
  valid_types <- valid_shape &&
    is.integer(attempts$attempt_number) && is.integer(attempts$status) &&
    is.numeric(attempts$encoded_bytes) && is.numeric(attempts$decoded_bytes) &&
    all(vapply(attempts[character_columns], is.character, logical(1))) &&
    !anyNA(attempts)
  if (!valid_types) {
    gx_csv_execution_abort(
      "The direct-CSV attempt ledger violates its exact bounded shape.",
      "gx_error_csv_execution_attempt"
    )
  }
  body <- parsed_response$validated_response$body
  validation <- parsed_response$validated_response$validation
  expected_bytes <- unname(as.double(length(body)))
  completed <- gx_csv_execution_parse_time_impl(attempts$completed_at[[1L]])
  host_ok <- identical(attempts$resolved_host[[1L]],
                       gx_safe_target(target, resolve_dns = FALSE)$host)
  ip <- attempts$resolved_ip[[1L]]
  ip_ok <- gx_is_ipv4(ip) && !gx_is_nonpublic_ipv4(ip)
  values_ok <- attempts$contract_version[[1L]] ==
      .gx_csv_execution_contract_version &&
    attempts$attempt_number[[1L]] == 1L &&
    attempts$execution_id[[1L]] == execution$execution_id &&
    attempts$logical_request_id[[1L]] == request$logical_request_id[[1L]] &&
    attempts$reservation_id[[1L]] == request$reservation_id[[1L]] &&
    attempts$method[[1L]] == request$method[[1L]] &&
    attempts$canonical_url_redacted[[1L]] == gx_redact_url(target) &&
    host_ok && ip_ok && attempts$status[[1L]] == 200L &&
    attempts$outcome[[1L]] == "response" &&
    attempts$media_type[[1L]] == validation$media_type &&
    identical(attempts$encoded_bytes[[1L]], expected_bytes) &&
    identical(attempts$decoded_bytes[[1L]], expected_bytes) &&
    attempts$body_sha256[[1L]] == validation$body_sha256 &&
    !is.na(completed)
  expected_attempt_id <- if (values_ok) {
    gx_csv_execution_attempt_id_impl(
      execution$execution_id, request, target, attempts
    )
  } else {
    NA_character_
  }
  if (!values_ok ||
      !identical(attempts$attempt_id[[1L]], expected_attempt_id)) {
    gx_csv_execution_abort(
      "The direct-CSV attempt ledger no longer rebinds to its transport facts.",
      "gx_error_csv_execution_attempt"
    )
  }
  invisible(completed)
}

gx_csv_execution_validate_execution_impl <- function(
    execution, parsed_response, request, target, attempts) {
  valid_shape <- is.list(execution) && identical(
    names(execution), .gx_csv_execution_execution_fields
  ) && gx_csv_execution_exact_attributes(execution, "names")
  character_fields <- c(
    "execution_scope_id", "execution_id", "logical_request_id",
    "reservation_id", "distribution_id", "started_at", "completed_at",
    "execution_status"
  )
  numeric_fields <- c(
    "timeout_seconds", "min_interval_seconds", "encoded_bytes",
    "decoded_bytes"
  )
  valid_types <- valid_shape && all(vapply(
    execution[character_fields], gx_csv_execution_scalar_text, logical(1)
  )) && all(vapply(execution[numeric_fields], function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
      is.null(attributes(x))
  }, logical(1))) && is.integer(execution$physical_attempt_count) &&
    length(execution$physical_attempt_count) == 1L &&
    is.null(attributes(execution$physical_attempt_count))
  if (!valid_types) {
    gx_csv_execution_abort(
      "Direct-CSV execution facts violate their exact shape.",
      "gx_error_csv_execution_contract"
    )
  }
  scope <- tryCatch(
    gx_csv_execution_scope_impl(execution$execution_scope_id),
    error = function(cnd) NULL
  )
  timeout <- tryCatch(
    gx_csv_execution_number_impl(
      execution$timeout_seconds, "timeout", 0, 600, FALSE
    ),
    error = function(cnd) NULL
  )
  interval <- tryCatch(
    gx_csv_execution_number_impl(
      execution$min_interval_seconds, "min_interval", 0, 3600, TRUE
    ),
    error = function(cnd) NULL
  )
  started <- gx_csv_execution_parse_time_impl(execution$started_at)
  completed <- gx_csv_execution_parse_time_impl(execution$completed_at)
  attempt_completed <- gx_csv_execution_validate_attempts_impl(
    attempts, execution, parsed_response, request, target
  )
  bytes <- unname(as.double(length(parsed_response$validated_response$body)))
  expected_id <- if (!is.null(scope) && !is.null(timeout) &&
      !is.null(interval) && !is.na(started)) {
    gx_csv_execution_id_impl(
      scope, request, execution$started_at, timeout, interval
    )
  } else {
    NA_character_
  }
  values_ok <- !is.na(started) && !is.na(completed) &&
    !is.na(attempt_completed) && completed >= started &&
    completed >= attempt_completed &&
    execution$logical_request_id == request$logical_request_id[[1L]] &&
    execution$reservation_id == request$reservation_id[[1L]] &&
    execution$distribution_id == request$distribution_id[[1L]] &&
    identical(execution$execution_id, expected_id) &&
    identical(execution$encoded_bytes, bytes) &&
    identical(execution$decoded_bytes, bytes) &&
    execution$physical_attempt_count == 1L &&
    execution$execution_status == "provider_response_validated_and_parsed"
  if (!values_ok) {
    gx_csv_execution_abort(
      "Direct-CSV execution facts no longer rebind to the request and attempt.",
      "gx_error_csv_execution_contract"
    )
  }
  invisible(NULL)
}

gx_csv_execution_validate_metadata_impl <- function(
    metadata, parsed_response) {
  valid_shape <- is.list(metadata) && identical(
    names(metadata), .gx_csv_execution_metadata_fields
  ) && gx_csv_execution_exact_attributes(metadata, "names") &&
    is.list(metadata$counts) && identical(
      names(metadata$counts), .gx_csv_execution_count_fields
    ) && gx_csv_execution_exact_attributes(metadata$counts, "names")
  bytes <- unname(as.double(length(parsed_response$validated_response$body)))
  expected <- gx_csv_execution_metadata_impl(parsed_response, bytes)
  if (!valid_shape || !identical(metadata, expected)) {
    gx_csv_execution_abort(
      "Direct-CSV execution metadata violates its exact authority contract.",
      "gx_error_csv_execution_contract"
    )
  }
  invisible(NULL)
}

gx_csv_execution_assert_text_budget_impl <- function(x) {
  owned <- list(
    contract_version = x$contract_version,
    execution = x$execution,
    attempts = x$attempts,
    metadata = x$metadata
  )
  total <- gx_fetch_plan_text_total(
    owned, limit = .gx_csv_execution_max_text_bytes
  )
  if (!is.finite(total) || total > .gx_csv_execution_max_text_bytes) {
    gx_csv_execution_abort(
      "Direct-CSV execution text exceeds its aggregate byte budget.",
      "gx_error_csv_execution_budget"
    )
  }
  invisible(NULL)
}

gx_csv_execution_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(names(x), .gx_csv_execution_fields) &&
    identical(class(x), "gx_csv_execution") &&
    gx_csv_execution_exact_attributes(x, c("names", "class")) &&
    identical(x$contract_version, .gx_csv_execution_contract_version) &&
    is.null(attributes(x$contract_version))
  if (!valid_top) {
    gx_csv_execution_abort(
      "Direct-CSV execution violates its exact top-level contract."
    )
  }
  valid_parsed <- tryCatch({
    gx_csv_parsed_response_validate_impl(x$parsed_response)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid_parsed) {
    gx_csv_execution_abort(
      "Direct-CSV execution embeds an invalid M7f parsed response."
    )
  }
  validated <- x$parsed_response$validated_response
  request_plan <- validated$request_plan
  selected <- gx_csv_execution_request_impl(
    request_plan, validated$validation$logical_request_id
  )
  gx_csv_execution_validate_execution_impl(
    x$execution,
    x$parsed_response,
    selected$request,
    selected$target,
    x$attempts
  )
  gx_csv_execution_validate_metadata_impl(x$metadata, x$parsed_response)
  gx_csv_execution_assert_text_budget_impl(x)
  invisible(x)
}

#' @export
print.gx_csv_execution <- function(x, ...) {
  gx_csv_execution_validate_impl(x)
  cli::cli_inform(c(
    "<gx_csv_execution>",
    paste0(
      "* Provider response: observed and validated; bytes: ",
      format(x$execution$encoded_bytes, big.mark = ",")
    ),
    paste0(
      "* Physical attempts: ", x$execution$physical_attempt_count,
      "; parser: strict CSV v1"
    ),
    paste0(
      "* Execution completed: TRUE; replayable: FALSE; remaining blockers: ",
      length(x$metadata$non_replayable_reasons)
    )
  ))
  invisible(x)
}
