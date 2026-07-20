.gx_oaf_request_plan_contract_version <- "0.1.0"
.gx_oaf_execution_contract_version <- "0.1.0"

.gx_oaf_request_plan_fields <- c(
  "contract_version", "request_plan", "policy", "request", "metadata"
)

.gx_oaf_policy_fields <- c(
  "slice_id", "handler_id", "implementation_id", "implementation_package",
  "implementation_symbol", "method", "accept", "accept_encoding",
  "body_bytes", "body_sha256", "credential_policy", "redirect_policy",
  "max_redirects", "retry_policy", "max_retries",
  "max_physical_attempts", "cache_policy", "success_status",
  "response_media_types", "response_content_encoding", "pagination_policy",
  "limit"
)

.gx_oaf_request_fields <- c(
  "logical_request_id", "reservation_id", "distribution_id", "fetch_order",
  "collection_id", "source_url_redacted", "canonical_url_redacted",
  "max_physical_attempts", "max_encoded_bytes", "max_decoded_bytes",
  "response_byte_limit", "request_status"
)

.gx_oaf_plan_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "budgets_allocated", "budgets_consumed", "counts",
  "non_replayable_reasons"
)

.gx_oaf_execution_fields <- c(
  "contract_version", "request_plan", "response_body", "result",
  "implementation", "execution", "attempts", "metadata"
)

.gx_oaf_implementation_fields <- c(
  "implementation_id", "package", "symbol", "resolution_status",
  "invocation_status"
)

.gx_oaf_execution_fact_fields <- c(
  "execution_scope_id", "execution_id", "logical_request_id",
  "reservation_id", "distribution_id", "started_at", "completed_at",
  "timeout_seconds", "min_interval_seconds", "encoded_bytes",
  "decoded_bytes", "feature_count", "number_matched", "truncated",
  "stop_reason", "execution_status"
)

.gx_oaf_attempt_columns <- c(
  "contract_version", "attempt_number", "attempt_id", "execution_id",
  "logical_request_id", "reservation_id", "method",
  "canonical_url_redacted", "resolved_host", "resolved_ip", "status",
  "outcome", "media_type", "encoded_bytes", "decoded_bytes",
  "body_sha256", "completed_at"
)

.gx_oaf_execution_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "execution_completed", "provider_response_observed", "budgets_consumed",
  "response_validated", "geojson_parsed", "result_contract_bound",
  "attempt_ledger_bound", "runtime_symbol_checked", "observation_origin",
  "counts", "non_replayable_reasons"
)

gx_oaf_abort <- function(
    message,
    class = "gx_error_oaf_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_oaf", "gx_error_fetch_plan")),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_oaf_scalar_text <- function(x, nonempty = TRUE) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    (!nonempty || nzchar(x)) && is.null(attributes(x))
}

gx_oaf_exact_names <- function(x, expected) {
  is.list(x) && identical(names(x), expected) &&
    identical(names(attributes(x)), "names")
}

gx_oaf_limit_impl <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x > 100000L || x != floor(x) || !is.null(attributes(x))) {
    gx_oaf_abort(
      "The OGC API Features row limit must be one explicit bounded whole number.",
      "gx_error_oaf_plan_policy"
    )
  }
  unname(as.integer(x))
}

gx_oaf_policy_impl <- function(limit) {
  list(
    slice_id = "ogc_api_features_single_page_v1",
    handler_id = "ogc_api_features",
    implementation_id = "geoconnexr:native-oaf",
    implementation_package = "geoconnexr",
    implementation_symbol = "gx_handler_oaf",
    method = "GET",
    accept = "application/geo+json, application/json;q=0.9",
    accept_encoding = "identity",
    body_bytes = 0L,
    body_sha256 = digest::digest(raw(), algo = "sha256", serialize = FALSE),
    credential_policy = "source_url_opaque_no_additional_credentials",
    redirect_policy = "reject",
    max_redirects = 0L,
    retry_policy = "none",
    max_retries = 0L,
    max_physical_attempts = 1L,
    cache_policy = "bypass",
    success_status = 200L,
    response_media_types = c("application/geo+json", "application/json"),
    response_content_encoding = "identity",
    pagination_policy = "single_page_no_follow",
    limit = gx_oaf_limit_impl(limit)
  )
}

gx_oaf_source_row_impl <- function(request_plan, distribution_id) {
  valid <- tryCatch({
    gx_csv_request_plan_validate_impl(request_plan)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid || !gx_oaf_scalar_text(distribution_id) ||
      !gx_catalog_is_sha256(distribution_id)) {
    gx_oaf_abort(
      "M7i planning requires a valid M7d plan and one distribution identity.",
      "gx_error_oaf_plan_input"
    )
  }
  coverage_position <- match(distribution_id, request_plan$coverage$distribution_id)
  distribution_position <- match(
    distribution_id, request_plan$intent_set$plan$distributions$distribution_id
  )
  reservation_position <- match(
    distribution_id, request_plan$reservations$distribution_id
  )
  positions_valid <- !is.na(coverage_position) && !is.na(distribution_position) &&
    !is.na(reservation_position)
  if (!positions_valid) {
    gx_oaf_abort(
      "The selected OGC API Features distribution has no complete M7d reservation.",
      "gx_error_oaf_plan_input"
    )
  }
  coverage <- request_plan$coverage[coverage_position, , drop = FALSE]
  distribution <- request_plan$intent_set$plan$distributions[
    distribution_position, , drop = FALSE
  ]
  reservation <- request_plan$reservations[reservation_position, , drop = FALSE]
  valid_binding <- coverage$selected[[1L]] &&
    coverage$handler_id[[1L]] == "ogc_api_features" &&
    coverage$request_status[[1L]] == "handler_reserved" &&
    coverage$reservation_id[[1L]] == reservation$reservation_id[[1L]] &&
    reservation$handler_id[[1L]] == "ogc_api_features" &&
    reservation$reservation_status[[1L]] == "held_deferred_handler" &&
    reservation$max_physical_attempts[[1L]] == 1L &&
    distribution$selected[[1L]] &&
    distribution$handler_id[[1L]] == "ogc_api_features" &&
    distribution$fetch_order[[1L]] == coverage$fetch_order[[1L]]
  if (!valid_binding) {
    gx_oaf_abort(
      "The selected distribution is not an admitted OGC API Features reservation.",
      "gx_error_oaf_plan_input"
    )
  }
  list(
    coverage = coverage,
    distribution = distribution,
    reservation = reservation
  )
}

gx_oaf_target_impl <- function(source_url, limit) {
  if (!gx_oaf_scalar_text(source_url)) {
    gx_oaf_abort("The OGC API Features source URL is invalid.", "gx_error_oaf_plan_url")
  }
  original <- tryCatch(httr2::url_parse(source_url), error = function(cnd) NULL)
  if (is.null(original) || length(original$query %||% list()) ||
      !is.null(original$fragment)) {
    gx_oaf_abort(
      "M7i accepts only query-free and fragment-free OGC item URLs.",
      "gx_error_oaf_plan_url"
    )
  }
  canonical <- tryCatch(
    gx_safe_target(source_url, resolve_dns = FALSE)$url,
    error = function(cnd) NULL
  )
  if (is.null(canonical)) {
    gx_oaf_abort("The OGC API Features source URL is unsafe.", "gx_error_oaf_plan_url")
  }
  parsed <- httr2::url_parse(canonical)
  matched <- regexec(
    "^(.*)/collections/([^/]+)/items/?$", parsed$path %||% "", perl = TRUE
  )
  parts <- regmatches(parsed$path %||% "", matched)[[1L]]
  if (length(parts) != 3L || !nzchar(parts[[3L]])) {
    gx_oaf_abort(
      "The OGC API Features source URL must end in /collections/{id}/items.",
      "gx_error_oaf_plan_url"
    )
  }
  collection_id <- tryCatch(
    utils::URLdecode(parts[[3L]]),
    error = function(cnd) NA_character_
  )
  encoded <- tryCatch(
    gx_ref_path_segment(collection_id, "collection"),
    error = function(cnd) NA_character_
  )
  if (is.na(encoded) || !identical(encoded, parts[[3L]])) {
    gx_oaf_abort(
      "The OGC collection identifier is not canonically encoded.",
      "gx_error_oaf_plan_url"
    )
  }
  target <- do.call(
    httr2::url_modify_query,
    list(.url = canonical, f = "json", limit = as.character(limit))
  )
  list(
    source = canonical,
    target = gx_canonical_url(target),
    collection_id = unname(enc2utf8(collection_id))
  )
}

gx_oaf_request_id_impl <- function(source, policy, row) {
  gx_contract_hash(
    list(
      "distribution_id", row$distribution$distribution_id[[1L]],
      "reservation_id", row$reservation$reservation_id[[1L]],
      "fetch_order", row$distribution$fetch_order[[1L]],
      "canonical_source", source$source,
      "canonical_target", source$target,
      "collection_id", source$collection_id,
      "implementation_id", policy$implementation_id,
      "implementation_symbol", policy$implementation_symbol,
      "method", policy$method,
      "accept", policy$accept,
      "accept_encoding", policy$accept_encoding,
      "pagination_policy", policy$pagination_policy,
      "limit", policy$limit,
      "max_physical_attempts", row$reservation$max_physical_attempts[[1L]],
      "max_encoded_bytes", gx_csv_request_plan_byte_hash_value(
        row$reservation$max_encoded_bytes[[1L]]
      ),
      "max_decoded_bytes", gx_csv_request_plan_byte_hash_value(
        row$reservation$max_decoded_bytes[[1L]]
      )
    ),
    namespace = "geoconnexr.oaf-request.v1",
    contract_version = .gx_oaf_request_plan_contract_version
  )
}

gx_oaf_request_impl <- function(row, source, policy) {
  byte_limit <- unname(as.double(min(
    row$reservation$max_encoded_bytes[[1L]],
    row$reservation$max_decoded_bytes[[1L]]
  )))
  list(
    logical_request_id = gx_oaf_request_id_impl(source, policy, row),
    reservation_id = unname(row$reservation$reservation_id[[1L]]),
    distribution_id = unname(row$distribution$distribution_id[[1L]]),
    fetch_order = unname(row$distribution$fetch_order[[1L]]),
    collection_id = source$collection_id,
    source_url_redacted = gx_redact_url(source$source),
    canonical_url_redacted = gx_redact_url(source$target),
    max_physical_attempts = 1L,
    max_encoded_bytes = unname(as.double(row$reservation$max_encoded_bytes[[1L]])),
    max_decoded_bytes = unname(as.double(row$reservation$max_decoded_bytes[[1L]])),
    response_byte_limit = byte_limit,
    request_status = "oaf_request_planned"
  )
}

gx_oaf_plan_reasons_impl <- function(request_plan) {
  reasons <- unique(c(
    setdiff(
      request_plan$metadata$non_replayable_reasons,
      c("arbitrary_provider_client_unimplemented", "non_csv_request_plans_absent")
    ),
    "oaf_provider_transport_unauthorized",
    "oaf_result_unbound",
    "runtime_symbol_check_pending",
    "single_page_only"
  ))
  reasons[gx_catalog_byte_order(reasons)]
}

gx_oaf_plan_metadata_impl <- function(request_plan) {
  list(
    host_specific = FALSE,
    replayable = FALSE,
    execution_ready = FALSE,
    transport_authorized = FALSE,
    budgets_allocated = TRUE,
    budgets_consumed = FALSE,
    counts = list(
      logical_requests = 1L,
      physical_attempts_reserved = 1L,
      requests_executed = 0L,
      physical_attempts_executed = 0L
    ),
    non_replayable_reasons = gx_oaf_plan_reasons_impl(request_plan)
  )
}

gx_oaf_request_plan_new_impl <- function(
    request_plan, policy, request, metadata) {
  object <- structure(
    list(
      contract_version = .gx_oaf_request_plan_contract_version,
      request_plan = request_plan,
      policy = policy,
      request = request,
      metadata = metadata
    ),
    class = "gx_oaf_request_plan"
  )
  gx_oaf_request_plan_validate_impl(object)
  object
}

gx_oaf_request_plan_impl <- function(request_plan, distribution_id, limit = NULL) {
  policy <- gx_oaf_policy_impl(limit)
  row <- gx_oaf_source_row_impl(request_plan, distribution_id)
  source <- gx_oaf_target_impl(row$distribution$distribution_url[[1L]], policy$limit)
  gx_oaf_request_plan_new_impl(
    request_plan = request_plan,
    policy = policy,
    request = gx_oaf_request_impl(row, source, policy),
    metadata = gx_oaf_plan_metadata_impl(request_plan)
  )
}

gx_oaf_request_plan_validate_body <- function(x) {
  if (!identical(class(x), "gx_oaf_request_plan") ||
      !identical(names(x), .gx_oaf_request_plan_fields) ||
      !identical(names(attributes(x)), c("names", "class")) ||
      !identical(x$contract_version, .gx_oaf_request_plan_contract_version)) {
    gx_oaf_abort("The OGC request plan violates its exact top-level contract.")
  }
  row <- gx_oaf_source_row_impl(
    x$request_plan, x$request$distribution_id %||% NA_character_
  )
  if (!gx_oaf_exact_names(x$policy, .gx_oaf_policy_fields) ||
      !gx_oaf_exact_names(x$request, .gx_oaf_request_fields) ||
      !gx_oaf_exact_names(x$metadata, .gx_oaf_plan_metadata_fields)) {
    gx_oaf_abort("The OGC request plan contains malformed nested facts.")
  }
  expected_policy <- gx_oaf_policy_impl(x$policy$limit)
  source <- gx_oaf_target_impl(
    row$distribution$distribution_url[[1L]], expected_policy$limit
  )
  expected_request <- gx_oaf_request_impl(row, source, expected_policy)
  expected_metadata <- gx_oaf_plan_metadata_impl(x$request_plan)
  if (!identical(x$policy, expected_policy) ||
      !identical(x$request, expected_request) ||
      !identical(x$metadata, expected_metadata)) {
    gx_oaf_abort(
      "The OGC request plan no longer rebinds to its reservation and policy.",
      "gx_error_oaf_plan_binding"
    )
  }
  invisible(x)
}

gx_oaf_request_plan_validate_impl <- function(x) {
  tryCatch(
    gx_oaf_request_plan_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_oaf")) stop(cnd)
      gx_oaf_abort("OGC request-plan validation rejected a malformed object.")
    },
    warning = function(cnd) {
      gx_oaf_abort("OGC request-plan validation rejected a warning-producing object.")
    }
  )
}

#' @export
print.gx_oaf_request_plan <- function(x, ...) {
  gx_oaf_request_plan_validate_impl(x)
  cli::cli_inform(c(
    "<gx_oaf_request_plan>",
    "* Collection: {x$request$collection_id}; limit: {x$policy$limit}",
    "* Reserved attempts: 1; requests executed: 0",
    "* Transport authorized: FALSE; runtime symbol check: pending"
  ))
  invisible(x)
}

gx_oaf_empty_queryables_impl <- function(collection_id, source_url) {
  out <- tibble::tibble(
    contract_version = character(), collection_id = character(),
    name = character(), json_types = list(), format = character(),
    title = character(), description = character(), enum = list(),
    schema = list()
  )
  metadata <- gx_ref_metadata(
    gx_ref_empty_requests(),
    source_url,
    collection = collection_id,
    additional_properties = NA
  )
  gx_ref_new_table(out, "gx_ref_queryables", metadata)
}

gx_oaf_result_impl <- function(body, media_type, request, target) {
  if (!is.raw(body) || !gx_oaf_scalar_text(media_type) ||
      !media_type %in% c("application/geo+json", "application/json")) {
    gx_oaf_abort("The OGC response candidate is not bounded GeoJSON.", "gx_error_oaf_payload")
  }
  response <- list(
    headers = list(`Content-Type` = media_type),
    body = body
  )
  parsed <- tryCatch({
    payload <- gx_ref_json(response, "features")
    features <- gx_ref_feature_array(payload)
    if (length(features) > request$policy$limit) {
      gx_oaf_abort(
        "The OGC response exceeded its planned feature limit.",
        "gx_error_oaf_payload_limit"
      )
    }
    queryables <- gx_oaf_empty_queryables_impl(
      request$request$collection_id, target
    )
    value <- gx_ref_features_sf(features, queryables)
    diagnostics <- attr(value, "gx_reference_diagnostics") %||%
      gx_empty_diagnostics()
    attr(value, "gx_reference_diagnostics") <- NULL
    number_matched <- gx_ref_count(payload$numberMatched)
    number_returned <- gx_ref_count(payload$numberReturned)
    if (!is.na(number_returned) && number_returned != length(features)) {
      gx_oaf_abort(
        "The OGC response numberReturned does not match its feature array.",
        "gx_error_oaf_payload_count"
      )
    }
    has_next <- !is.null(gx_ref_next_href(payload))
    truncated <- has_next ||
      (!is.na(number_matched) && number_matched > length(features)) ||
      length(features) == request$policy$limit
    stop_reason <- if (has_next) {
      "single_page_budget"
    } else if (!is.na(number_matched) && number_matched > length(features)) {
      "missing_next_link"
    } else if (length(features) == request$policy$limit) {
      "limit"
    } else {
      "no_next"
    }
    metadata <- list(
      contract_version = .gx_oaf_execution_contract_version,
      collection_id = request$request$collection_id,
      source_url = gx_redact_url(target),
      body_sha256 = digest::digest(body, algo = "sha256", serialize = FALSE),
      feature_count = unname(as.integer(length(features))),
      number_matched = unname(as.integer(number_matched)),
      truncated = unname(as.logical(truncated)),
      stop_reason = stop_reason,
      diagnostics = diagnostics
    )
    attr(value, "gx_oaf") <- metadata
    class(value) <- c("gx_oaf_features", class(value))
    value
  }, error = function(cnd) {
    if (inherits(cnd, "gx_error_oaf")) stop(cnd)
    gx_oaf_abort(
      "The OGC response did not satisfy the bounded GeoJSON contract.",
      "gx_error_oaf_payload"
    )
  })
  parsed
}

gx_oaf_candidate_impl <- function(response, request_plan, target) {
  policy <- request_plan$policy
  request <- request_plan$request
  media_type <- gx_media_type(response$headers)
  content_encoding <- tolower(
    trimws(gx_header(response$headers, "content-encoding") %||% "identity")
  )
  content_length_text <- gx_header(response$headers, "content-length")
  content_length <- suppressWarnings(as.numeric(content_length_text %||% NA))
  body_bytes <- length(response$body)
  valid_length <- is.null(content_length_text) ||
    (length(content_length) == 1L && !is.na(content_length) &&
       is.finite(content_length) && content_length >= 0 &&
       content_length == floor(content_length) && content_length == body_bytes)
  valid <- identical(response$status, policy$success_status) &&
    media_type %in% policy$response_media_types &&
    content_encoding %in% c("", policy$response_content_encoding) &&
    valid_length && body_bytes <= request$response_byte_limit &&
    body_bytes <= request$max_encoded_bytes &&
    body_bytes <= request$max_decoded_bytes &&
    identical(gx_canonical_url(response$url), target)
  if (!valid) {
    gx_oaf_abort(
      "The provider response violated the planned OGC response envelope.",
      "gx_error_oaf_response"
    )
  }
  gx_oaf_result_impl(response$body, media_type, request_plan, target)
}

gx_oaf_symbol_resolver_impl <- function(package, symbol) {
  namespace <- tryCatch(asNamespace(package), error = function(cnd) NULL)
  if (is.null(namespace)) return(NULL)
  get0(symbol, envir = namespace, inherits = FALSE, ifnotfound = NULL)
}

gx_handler_oaf <- function(request_plan, timeout, min_interval) {
  gx_oaf_request_plan_validate_impl(request_plan)
  source <- gx_oaf_source_row_impl(
    request_plan$request_plan, request_plan$request$distribution_id
  )
  target <- gx_oaf_target_impl(
    source$distribution$distribution_url[[1L]], request_plan$policy$limit
  )$target
  max_bytes <- as.integer(request_plan$request$response_byte_limit)
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
  result <- NULL
  validator <- function(response) {
    result <<- gx_oaf_candidate_impl(response, request_plan, target)
    invisible(NULL)
  }
  response <- gx_http_request(
    client,
    method = request_plan$policy$method,
    url = target,
    headers = list(
      Accept = request_plan$policy$accept,
      `Accept-Encoding` = request_plan$policy$accept_encoding
    ),
    body = raw(),
    check_status = FALSE,
    .response_validator = validator
  )
  if (is.null(result)) {
    gx_oaf_abort(
      "The OGC provider response bypassed its result validator.",
      "gx_error_oaf_response"
    )
  }
  list(response = response, result = result, target = target)
}

gx_oaf_invoke_impl <- function(
    request_plan, timeout, min_interval, symbol_resolver) {
  resolver <- symbol_resolver %||% gx_oaf_symbol_resolver_impl
  if (!is.function(resolver)) {
    gx_oaf_abort(
      "The runtime implementation resolver is invalid.",
      "gx_error_oaf_execution_capability"
    )
  }
  handler <- tryCatch(
    resolver(
      request_plan$policy$implementation_package,
      request_plan$policy$implementation_symbol
    ),
    error = function(cnd) NULL
  )
  if (!is.function(handler)) {
    gx_oaf_abort(
      "The OGC handler symbol is unavailable at invocation time.",
      "gx_error_oaf_execution_capability"
    )
  }
  do.call(handler, list(
    request_plan = request_plan,
    timeout = timeout,
    min_interval = min_interval
  ))
}

gx_oaf_execution_id_impl <- function(
    scope, request_plan, started_at, timeout, min_interval) {
  gx_contract_hash(
    list(
      "execution_scope_id", scope,
      "logical_request_id", request_plan$request$logical_request_id,
      "reservation_id", request_plan$request$reservation_id,
      "started_at", started_at,
      "timeout_seconds", sprintf("%.6f", timeout),
      "min_interval_seconds", sprintf("%.6f", min_interval),
      "implementation_id", request_plan$policy$implementation_id,
      "implementation_symbol", request_plan$policy$implementation_symbol
    ),
    namespace = "geoconnexr.oaf-execution.v1",
    contract_version = .gx_oaf_execution_contract_version
  )
}

gx_oaf_attempt_id_impl <- function(execution_id, request_plan, target, attempt) {
  gx_contract_hash(
    list(
      "execution_id", execution_id,
      "logical_request_id", request_plan$request$logical_request_id,
      "reservation_id", request_plan$request$reservation_id,
      "canonical_target", target,
      "resolved_host", attempt$resolved_host[[1L]],
      "resolved_ip", attempt$resolved_ip[[1L]],
      "status", attempt$status[[1L]],
      "media_type", attempt$media_type[[1L]],
      "encoded_bytes", gx_csv_request_plan_byte_hash_value(
        attempt$encoded_bytes[[1L]]
      ),
      "body_sha256", attempt$body_sha256[[1L]],
      "completed_at", attempt$completed_at[[1L]]
    ),
    namespace = "geoconnexr.oaf-attempt.v1",
    contract_version = .gx_oaf_execution_contract_version
  )
}

gx_oaf_attempts_impl <- function(
    response, request_plan, execution_id, target, result) {
  source <- response$attempts
  metadata <- attr(result, "gx_oaf")
  bytes <- unname(as.double(length(response$body)))
  valid <- is.data.frame(source) && nrow(source) == 1L &&
    isTRUE(source$physical[[1L]]) && source$attempt[[1L]] == 1L &&
    source$outcome[[1L]] == "response" &&
    source$cache_origin[[1L]] == "network" &&
    source$method[[1L]] == request_plan$policy$method &&
    source$url[[1L]] == gx_redact_url(target) && source$status[[1L]] == 200L &&
    source$media_type[[1L]] %in% request_plan$policy$response_media_types &&
    identical(source$bytes[[1L]], bytes) &&
    identical(source$charged_bytes[[1L]], bytes) &&
    source$body_sha256[[1L]] == metadata$body_sha256 &&
    source$resolved_host[[1L]] == gx_safe_target(target, FALSE)$host &&
    gx_is_ipv4(source$resolved_ip[[1L]]) &&
    !gx_is_nonpublic_ipv4(source$resolved_ip[[1L]])
  if (!valid) {
    gx_oaf_abort(
      "The OGC handler did not return one charged physical response attempt.",
      "gx_error_oaf_execution_attempt"
    )
  }
  attempt <- tibble::tibble(
    contract_version = .gx_oaf_execution_contract_version,
    attempt_number = 1L,
    attempt_id = "",
    execution_id = execution_id,
    logical_request_id = request_plan$request$logical_request_id,
    reservation_id = request_plan$request$reservation_id,
    method = source$method[[1L]],
    canonical_url_redacted = gx_redact_url(target),
    resolved_host = source$resolved_host[[1L]],
    resolved_ip = source$resolved_ip[[1L]],
    status = source$status[[1L]],
    outcome = source$outcome[[1L]],
    media_type = source$media_type[[1L]],
    encoded_bytes = bytes,
    decoded_bytes = bytes,
    body_sha256 = metadata$body_sha256,
    completed_at = gx_csv_execution_time_impl(source$retrieved_at[[1L]])
  )
  attempt$attempt_id[[1L]] <- gx_oaf_attempt_id_impl(
    execution_id, request_plan, target, attempt
  )
  attempt
}

gx_oaf_implementation_impl <- function(request_plan) {
  list(
    implementation_id = request_plan$policy$implementation_id,
    package = request_plan$policy$implementation_package,
    symbol = request_plan$policy$implementation_symbol,
    resolution_status = "verified_at_invocation",
    invocation_status = "invoked"
  )
}

gx_oaf_execution_facts_impl <- function(
    scope, execution_id, request_plan, started_at, completed_at, timeout,
    min_interval, result, bytes) {
  metadata <- attr(result, "gx_oaf")
  list(
    execution_scope_id = scope,
    execution_id = execution_id,
    logical_request_id = request_plan$request$logical_request_id,
    reservation_id = request_plan$request$reservation_id,
    distribution_id = request_plan$request$distribution_id,
    started_at = started_at,
    completed_at = completed_at,
    timeout_seconds = timeout,
    min_interval_seconds = min_interval,
    encoded_bytes = unname(as.double(bytes)),
    decoded_bytes = unname(as.double(bytes)),
    feature_count = metadata$feature_count,
    number_matched = metadata$number_matched,
    truncated = metadata$truncated,
    stop_reason = metadata$stop_reason,
    execution_status = "provider_response_validated_and_parsed"
  )
}

gx_oaf_execution_metadata_impl <- function(request_plan, result, bytes) {
  result_metadata <- attr(result, "gx_oaf")
  reasons <- unique(c(
    setdiff(
      request_plan$metadata$non_replayable_reasons,
      c(
        "oaf_provider_transport_unauthorized", "oaf_result_unbound",
        "runtime_symbol_check_pending", "transport_adapter_unimplemented",
        "attempt_identity_unbound", "attempt_ledger_unbound",
        "provider_transport_unauthorized", "result_schema_unbound",
        "timeout_policy_unbound"
      )
    ),
    "single_page_only",
    "serialization_unbound"
  ))
  reasons <- reasons[gx_catalog_byte_order(reasons)]
  list(
    host_specific = TRUE,
    replayable = FALSE,
    execution_ready = FALSE,
    transport_authorized = TRUE,
    execution_completed = TRUE,
    provider_response_observed = TRUE,
    budgets_consumed = TRUE,
    response_validated = TRUE,
    geojson_parsed = TRUE,
    result_contract_bound = TRUE,
    attempt_ledger_bound = TRUE,
    runtime_symbol_checked = TRUE,
    observation_origin = "provider_transport",
    counts = list(
      logical_requests = 1L,
      physical_attempts = 1L,
      successful_attempts = 1L,
      encoded_bytes = unname(as.double(bytes)),
      decoded_bytes = unname(as.double(bytes)),
      features = result_metadata$feature_count
    ),
    non_replayable_reasons = reasons
  )
}

gx_oaf_redacted_attempts_impl <- function(cnd) {
  attempts <- cnd$attempts
  if (!is.data.frame(attempts) || !nrow(attempts)) return(NULL)
  keep <- intersect(
    c(
      "attempt", "method", "url", "resolved_host", "resolved_ip", "status",
      "outcome", "physical", "error_code", "charged_bytes", "retrieved_at"
    ),
    names(attempts)
  )
  attempts[, keep, drop = FALSE]
}

gx_oaf_execution_impl <- function(
    request_plan,
    timeout = NULL,
    min_interval = NULL,
    execution_scope_id = NULL,
    symbol_resolver = NULL) {
  gx_oaf_request_plan_validate_impl(request_plan)
  scope <- gx_csv_execution_scope_impl(execution_scope_id)
  timeout <- gx_csv_execution_number_impl(
    timeout, "timeout", 0, 600, allow_zero = FALSE
  )
  min_interval <- gx_csv_execution_number_impl(
    min_interval, "min_interval", 0, 3600, allow_zero = TRUE
  )
  started_at <- gx_csv_execution_time_impl(gx_now())
  execution_id <- gx_oaf_execution_id_impl(
    scope, request_plan, started_at, timeout, min_interval
  )
  invoked <- tryCatch(
    gx_oaf_invoke_impl(
      request_plan, timeout, min_interval, symbol_resolver
    ),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_oaf_execution_capability")) stop(cnd)
      phase <- if (inherits(cnd, c(
        "gx_error_oaf_payload", "gx_error_oaf_payload_limit",
        "gx_error_oaf_payload_count", "gx_error_reference_payload",
        "gx_error_reference_geometry", "gx_error_reference_budget"
      ))) "parse" else "transport"
      gx_oaf_abort(
        "OGC execution failed during its bounded {phase} phase; underlying details were withheld.",
        paste0("gx_error_oaf_execution_", phase),
        execution_id = execution_id,
        attempts = gx_oaf_redacted_attempts_impl(cnd)
      )
    }
  )
  completed_at <- gx_csv_execution_time_impl(gx_now())
  attempts <- gx_oaf_attempts_impl(
    invoked$response, request_plan, execution_id, invoked$target, invoked$result
  )
  bytes <- unname(as.double(length(invoked$response$body)))
  execution <- gx_oaf_execution_facts_impl(
    scope, execution_id, request_plan, started_at, completed_at, timeout,
    min_interval, invoked$result, bytes
  )
  object <- structure(
    list(
      contract_version = .gx_oaf_execution_contract_version,
      request_plan = request_plan,
      response_body = invoked$response$body,
      result = invoked$result,
      implementation = gx_oaf_implementation_impl(request_plan),
      execution = execution,
      attempts = attempts,
      metadata = gx_oaf_execution_metadata_impl(
        request_plan, invoked$result, bytes
      )
    ),
    class = "gx_oaf_execution"
  )
  gx_oaf_execution_validate_impl(object)
  object
}

gx_oaf_execution_validate_body <- function(x) {
  if (!identical(class(x), "gx_oaf_execution") ||
      !identical(names(x), .gx_oaf_execution_fields) ||
      !identical(names(attributes(x)), c("names", "class")) ||
      !identical(x$contract_version, .gx_oaf_execution_contract_version)) {
    gx_oaf_abort("The OGC execution violates its exact top-level contract.")
  }
  gx_oaf_request_plan_validate_impl(x$request_plan)
  if (!is.raw(x$response_body) || !is.null(attributes(x$response_body)) ||
      !gx_oaf_exact_names(x$implementation, .gx_oaf_implementation_fields) ||
      !gx_oaf_exact_names(x$execution, .gx_oaf_execution_fact_fields) ||
      !gx_oaf_exact_names(x$metadata, .gx_oaf_execution_metadata_fields)) {
    gx_oaf_abort("The OGC execution contains malformed nested facts.")
  }
  source <- gx_oaf_source_row_impl(
    x$request_plan$request_plan, x$request_plan$request$distribution_id
  )
  target <- gx_oaf_target_impl(
    source$distribution$distribution_url[[1L]], x$request_plan$policy$limit
  )$target
  attempts <- x$attempts
  valid_attempt_shape <- inherits(attempts, "tbl_df") && nrow(attempts) == 1L &&
    identical(names(attempts), .gx_oaf_attempt_columns) &&
    !anyNA(attempts) && all(vapply(attempts, function(column) {
      is.null(attributes(column))
    }, logical(1)))
  if (!valid_attempt_shape) {
    gx_oaf_abort("The OGC attempt ledger violates its exact shape.")
  }
  rebuilt <- gx_oaf_result_impl(
    x$response_body, attempts$media_type[[1L]], x$request_plan, target
  )
  if (!identical(x$result, rebuilt)) {
    gx_oaf_abort(
      "The OGC simple-feature result no longer rebinds to its retained bytes.",
      "gx_error_oaf_execution_result"
    )
  }
  result_metadata <- attr(rebuilt, "gx_oaf")
  bytes <- unname(as.double(length(x$response_body)))
  expected_implementation <- gx_oaf_implementation_impl(x$request_plan)
  scope <- tryCatch(
    gx_csv_execution_scope_impl(x$execution$execution_scope_id),
    error = function(cnd) NULL
  )
  timeout <- tryCatch(
    gx_csv_execution_number_impl(
      x$execution$timeout_seconds, "timeout", 0, 600, FALSE
    ),
    error = function(cnd) NULL
  )
  interval <- tryCatch(
    gx_csv_execution_number_impl(
      x$execution$min_interval_seconds, "min_interval", 0, 3600, TRUE
    ),
    error = function(cnd) NULL
  )
  started <- gx_csv_execution_parse_time_impl(x$execution$started_at)
  completed <- gx_csv_execution_parse_time_impl(x$execution$completed_at)
  expected_execution_id <- if (!is.null(scope) && !is.null(timeout) &&
      !is.null(interval) && !is.na(started)) {
    gx_oaf_execution_id_impl(
      scope, x$request_plan, x$execution$started_at, timeout, interval
    )
  } else {
    NA_character_
  }
  attempt_time <- gx_csv_execution_parse_time_impl(
    attempts$completed_at[[1L]]
  )
  attempt_values <- attempts$contract_version[[1L]] ==
      .gx_oaf_execution_contract_version && attempts$attempt_number[[1L]] == 1L &&
    attempts$execution_id[[1L]] == expected_execution_id &&
    attempts$logical_request_id[[1L]] == x$request_plan$request$logical_request_id &&
    attempts$reservation_id[[1L]] == x$request_plan$request$reservation_id &&
    attempts$method[[1L]] == x$request_plan$policy$method &&
    attempts$canonical_url_redacted[[1L]] == gx_redact_url(target) &&
    attempts$resolved_host[[1L]] == gx_safe_target(target, FALSE)$host &&
    gx_is_ipv4(attempts$resolved_ip[[1L]]) &&
    !gx_is_nonpublic_ipv4(attempts$resolved_ip[[1L]]) &&
    attempts$status[[1L]] == 200L && attempts$outcome[[1L]] == "response" &&
    attempts$media_type[[1L]] %in% x$request_plan$policy$response_media_types &&
    identical(attempts$encoded_bytes[[1L]], bytes) &&
    identical(attempts$decoded_bytes[[1L]], bytes) &&
    attempts$body_sha256[[1L]] == result_metadata$body_sha256 &&
    !is.na(attempt_time)
  expected_attempt_id <- if (attempt_values) {
    gx_oaf_attempt_id_impl(expected_execution_id, x$request_plan, target, attempts)
  } else {
    NA_character_
  }
  execution_values <- !is.na(started) && !is.na(completed) &&
    !is.na(attempt_time) && completed >= started && completed >= attempt_time &&
    x$execution$execution_id == expected_execution_id &&
    x$execution$logical_request_id == x$request_plan$request$logical_request_id &&
    x$execution$reservation_id == x$request_plan$request$reservation_id &&
    x$execution$distribution_id == x$request_plan$request$distribution_id &&
    identical(x$execution$encoded_bytes, bytes) &&
    identical(x$execution$decoded_bytes, bytes) &&
    x$execution$feature_count == result_metadata$feature_count &&
    identical(x$execution$number_matched, result_metadata$number_matched) &&
    identical(x$execution$truncated, result_metadata$truncated) &&
    x$execution$stop_reason == result_metadata$stop_reason &&
    x$execution$execution_status == "provider_response_validated_and_parsed"
  expected_metadata <- gx_oaf_execution_metadata_impl(
    x$request_plan, rebuilt, bytes
  )
  if (!identical(x$implementation, expected_implementation) ||
      !attempt_values || attempts$attempt_id[[1L]] != expected_attempt_id ||
      !execution_values || !identical(x$metadata, expected_metadata)) {
    gx_oaf_abort(
      "The OGC execution facts no longer rebind to its plan and response.",
      "gx_error_oaf_execution_binding"
    )
  }
  invisible(x)
}

gx_oaf_execution_validate_impl <- function(x) {
  tryCatch(
    gx_oaf_execution_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_oaf")) stop(cnd)
      gx_oaf_abort("OGC execution validation rejected a malformed object.")
    },
    warning = function(cnd) {
      gx_oaf_abort("OGC execution validation rejected a warning-producing object.")
    }
  )
}

#' @export
print.gx_oaf_execution <- function(x, ...) {
  gx_oaf_execution_validate_impl(x)
  cli::cli_inform(c(
    "<gx_oaf_execution>",
    "* Collection: {x$request_plan$request$collection_id}",
    "* Features: {x$execution$feature_count}; attempts: 1",
    "* Truncated: {x$execution$truncated} ({x$execution$stop_reason})",
    "* Runtime symbol: verified and invoked"
  ))
  invisible(x)
}
