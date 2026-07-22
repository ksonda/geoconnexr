.gx_usgs_daily_request_plan_contract_version <- "0.1.0"
.gx_usgs_daily_execution_contract_version <- "0.1.0"
.gx_usgs_daily_max_text_bytes <- 64L * 1024L^2
.gx_usgs_daily_max_json_depth <- 32L
.gx_usgs_daily_properties <- c(
  "time_series_id", "monitoring_location_id", "parameter_code",
  "statistic_id", "time", "value", "unit_of_measure", "approval_status",
  "qualifier", "last_modified"
)

.gx_usgs_daily_request_plan_fields <- c(
  "contract_version", "request_plan", "policy", "request", "metadata"
)

.gx_usgs_daily_policy_fields <- c(
  "slice_id", "handler_id", "implementation_id", "implementation_package",
  "minimum_version", "query_symbol", "normalization_owner", "method",
  "query_type", "response_format", "accept",
  "accept_encoding", "body_bytes", "body_sha256", "credential_policy",
  "redirect_policy", "max_redirects", "retry_policy", "max_retries",
  "max_physical_attempts", "cache_policy", "success_status",
  "response_media_types", "response_content_encoding", "parser_encoding",
  "type_inference", "attribute_policy", "pagination_policy", "max_fields",
  "max_json_depth", "max_json_members", "properties"
)

.gx_usgs_daily_request_fields <- c(
  "logical_request_id", "reservation_id", "distribution_id", "fetch_order",
  "base_url_redacted", "api_version", "collection_id", "query_type",
  "monitoring_location_id", "parameter_code", "statistic_id", "time_start",
  "time_end", "time", "properties", "skip_geometry", "response_format",
  "language", "limit", "source_url_redacted", "canonical_url_redacted",
  "max_physical_attempts", "max_encoded_bytes", "max_decoded_bytes",
  "response_byte_limit", "max_rows", "max_columns", "request_status"
)

.gx_usgs_daily_plan_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "budgets_allocated", "budgets_consumed", "counts",
  "non_replayable_reasons"
)

.gx_usgs_daily_execution_fields <- c(
  "contract_version", "request_plan", "response_body", "data", "schema",
  "parse", "implementation", "execution", "attempts", "metadata"
)

.gx_usgs_daily_parse_fields <- c(
  "body_sha256", "result_sha256", "row_count", "column_count",
  "field_count", "number_returned", "number_matched", "truncated",
  "stop_reason", "parser_validation"
)

.gx_usgs_daily_implementation_fields <- c(
  "implementation_id", "package", "minimum_version", "package_version",
  "query_symbol", "normalization_owner", "resolution_status", "invocation_status"
)

.gx_usgs_daily_execution_fact_fields <- c(
  "execution_scope_id", "execution_id", "logical_request_id",
  "reservation_id", "distribution_id", "started_at", "completed_at",
  "timeout_seconds", "min_interval_seconds", "encoded_bytes",
  "decoded_bytes", "row_count", "column_count", "execution_status"
)

.gx_usgs_daily_attempt_columns <- c(
  "contract_version", "attempt_number", "attempt_id", "execution_id",
  "logical_request_id", "reservation_id", "method",
  "canonical_url_redacted", "resolved_host", "resolved_ip", "status",
  "outcome", "media_type", "encoded_bytes", "decoded_bytes",
  "body_sha256", "completed_at"
)

.gx_usgs_daily_execution_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "execution_completed", "provider_response_observed", "budgets_consumed",
  "response_validated", "dataretrieval_symbol_checked",
  "native_geojson_parsed", "result_contract_bound",
  "attempt_ledger_bound", "runtime_symbol_checked", "observation_origin",
  "counts", "non_replayable_reasons"
)

gx_usgs_daily_abort <- function(
    message,
    class = "gx_error_usgs_daily_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_usgs_daily", "gx_error_fetch_plan")),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_usgs_daily_scalar_text <- function(x, nonempty = TRUE) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    (!nonempty || nzchar(x)) && is.null(attributes(x))
}

gx_usgs_daily_exact_names <- function(x, expected) {
  is.list(x) && identical(names(x), expected) &&
    identical(names(attributes(x)), "names")
}

gx_usgs_daily_bounded_text_impl <- function(
    x, label, allow_empty = FALSE, maximum = 2048L) {
  valid <- gx_usgs_daily_scalar_text(x, nonempty = !allow_empty) &&
    nchar(x, type = "bytes") <= maximum &&
    identical(tryCatch(stringi::stri_enc_isutf8(x), error = function(cnd) FALSE), TRUE) &&
    identical(tryCatch(
      stringi::stri_detect_regex(x, "[\\p{Cc}\\p{Cf}\\p{Cs}]"),
      error = function(cnd) TRUE
    ), FALSE)
  if (!valid) {
    gx_usgs_daily_abort(
      "The USGS daily {label} is not one bounded control-safe UTF-8 value.",
      "gx_error_usgs_daily_plan_url"
    )
  }
  unname(enc2utf8(x))
}

gx_usgs_daily_policy_impl <- function(max_fields) {
  max_fields <- tryCatch(
    gx_csv_parsed_response_field_limit_impl(max_fields),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(max_fields)) {
    gx_usgs_daily_abort(
      "The USGS daily parser field limit must be one explicit bounded whole number.",
      "gx_error_usgs_daily_plan_policy"
    )
  }
  list(
    slice_id = "usgs_daily_single_page_v1",
    handler_id = "usgs_waterdata_daily",
    implementation_id = "geoconnexr:dataRetrieval-daily",
    implementation_package = "dataRetrieval",
    minimum_version = "2.7.22",
    query_symbol = "read_waterdata_daily",
    normalization_owner = "geoconnexr",
    method = "GET",
    query_type = "daily",
    response_format = "GeoJSON",
    accept = "application/geo+json, application/json;q=0.9",
    accept_encoding = "identity",
    body_bytes = 0L,
    body_sha256 = digest::digest(raw(), algo = "sha256", serialize = FALSE),
    credential_policy = "source_query_allowlist_no_additional_credentials",
    redirect_policy = "reject",
    max_redirects = 0L,
    retry_policy = "none",
    max_retries = 0L,
    max_physical_attempts = 1L,
    cache_policy = "bypass",
    success_status = 200L,
    response_media_types = c("application/geo+json", "application/json"),
    response_content_encoding = "identity",
    parser_encoding = "UTF-8",
    type_inference = "fixed_geojson_property_contract",
    attribute_policy = "disabled",
    pagination_policy = "single_page_no_follow",
    max_fields = max_fields,
    max_json_depth = .gx_usgs_daily_max_json_depth,
    max_json_members = unname(as.integer(3L * max_fields + 1024L)),
    properties = .gx_usgs_daily_properties
  )
}

gx_usgs_daily_source_row_impl <- function(request_plan, distribution_id) {
  valid <- tryCatch({
    gx_csv_request_plan_validate_impl(request_plan)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid || !gx_usgs_daily_scalar_text(distribution_id) ||
      !gx_catalog_is_sha256(distribution_id)) {
    gx_usgs_daily_abort(
      "M7n planning requires a valid M7d plan and one distribution identity.",
      "gx_error_usgs_daily_plan_input"
    )
  }
  coverage_position <- match(
    distribution_id, request_plan$coverage$distribution_id
  )
  distribution_position <- match(
    distribution_id,
    request_plan$intent_set$plan$distributions$distribution_id
  )
  reservation_position <- match(
    distribution_id, request_plan$reservations$distribution_id
  )
  positions_valid <- !is.na(coverage_position) &&
    !is.na(distribution_position) && !is.na(reservation_position)
  if (!positions_valid) {
    gx_usgs_daily_abort(
      "The selected USGS daily distribution has no complete M7d reservation.",
      "gx_error_usgs_daily_plan_input"
    )
  }
  coverage <- request_plan$coverage[coverage_position, , drop = FALSE]
  distribution <- request_plan$intent_set$plan$distributions[
    distribution_position, , drop = FALSE
  ]
  reservation <- request_plan$reservations[
    reservation_position, , drop = FALSE
  ]
  valid_binding <- coverage$selected[[1L]] &&
    coverage$handler_id[[1L]] == "usgs_waterdata_daily" &&
    coverage$request_status[[1L]] == "handler_reserved" &&
    coverage$reservation_id[[1L]] == reservation$reservation_id[[1L]] &&
    reservation$handler_id[[1L]] == "usgs_waterdata_daily" &&
    reservation$reservation_status[[1L]] == "held_deferred_handler" &&
    reservation$max_physical_attempts[[1L]] == 1L &&
    distribution$selected[[1L]] &&
      distribution$handler_id[[1L]] == "usgs_waterdata_daily" &&
    distribution$fetch_order[[1L]] == coverage$fetch_order[[1L]]
  if (!valid_binding) {
    gx_usgs_daily_abort(
      "The selected distribution is not an admitted USGS daily reservation.",
      "gx_error_usgs_daily_plan_input"
    )
  }
  list(
    coverage = coverage,
    distribution = distribution,
    reservation = reservation,
    max_rows = unname(as.integer(request_plan$policy$max_rows)),
    max_columns = unname(as.integer(request_plan$policy$max_columns))
  )
}

gx_usgs_daily_time_impl <- function(distribution) {
  start <- distribution$time_start[[1L]]
  end <- distribution$time_end[[1L]]
  valid <- inherits(start, "POSIXct") && length(start) == 1L &&
    !is.na(start) && inherits(end, "POSIXct") && length(end) == 1L &&
    !is.na(end) && as.double(start) <= as.double(end)
  if (!valid) {
    gx_usgs_daily_abort(
      "The USGS daily distribution requires one finite planned time interval.",
      "gx_error_usgs_daily_plan_time"
    )
  }
  time_start <- unname(format(start, "%Y-%m-%d", tz = "UTC"))
  time_end <- unname(format(end, "%Y-%m-%d", tz = "UTC"))
  list(
    time_start = time_start,
    time_end = time_end,
    time = paste0(time_start, "/", time_end)
  )
}

gx_usgs_daily_query_value_impl <- function(query, names, name) {
  position <- match(name, names)
  if (is.na(position)) NULL else query[[position]]
}

gx_usgs_daily_target_impl <- function(
    source_url, distribution, max_rows, max_fields) {
  if (!gx_usgs_daily_scalar_text(source_url)) {
    gx_usgs_daily_abort(
      "The USGS daily source URL is invalid.",
      "gx_error_usgs_daily_plan_url"
    )
  }
  canonical <- tryCatch(
    gx_safe_target(source_url, resolve_dns = FALSE)$url,
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(canonical)) {
    gx_usgs_daily_abort("The USGS daily source URL is unsafe.", "gx_error_usgs_daily_plan_url")
  }
  parsed <- tryCatch(httr2::url_parse(canonical), error = function(cnd) NULL)
  path_match <- if (is.null(parsed)) integer() else regexec(
    "^/ogcapi/([A-Za-z0-9._~-]+)/collections/(daily)/items/?$",
    parsed$path %||% "",
    perl = TRUE
  )
  path_parts <- if (!length(path_match)) character() else {
    regmatches(parsed$path %||% "", path_match)[[1L]]
  }
  valid_port <- is.null(parsed$port) || identical(as.character(parsed$port), "443")
  if (is.null(parsed) || !identical(tolower(parsed$scheme %||% ""), "https") ||
      !valid_port || !identical(tolower(parsed$host %||% ""),
      "api.waterdata.usgs.gov") || !is.null(parsed$fragment) ||
      length(path_parts) != 3L) {
    gx_usgs_daily_abort(
      "M7n accepts only the reviewed USGS Water Data daily items endpoint.",
      "gx_error_usgs_daily_plan_url"
    )
  }
  query <- parsed$query %||% list()
  query_names <- names(query) %||% character()
  allowed <- c(
    "monitoring_location_id", "parameter_code", "statistic_id", "time",
    "properties", "skipGeometry", "f", "lang", "limit"
  )
  if (!length(query) || any(!query_names %in% allowed) ||
      anyDuplicated(query_names) ||
      sum(query_names == "monitoring_location_id") != 1L ||
      sum(query_names == "parameter_code") != 1L ||
      sum(query_names == "statistic_id") != 1L) {
    gx_usgs_daily_abort(
      "The inherited USGS daily query must contain one site, one parameter code, one statistic code, and no unreviewed filters.",
      "gx_error_usgs_daily_plan_url"
    )
  }
  site <- gx_usgs_daily_bounded_text_impl(
    gx_usgs_daily_query_value_impl(
      query, query_names, "monitoring_location_id"
    ),
    "monitoring-location identity", maximum = 128L
  )
  parameter <- gx_usgs_daily_bounded_text_impl(
    gx_usgs_daily_query_value_impl(query, query_names, "parameter_code"),
    "parameter code", maximum = 5L
  )
  statistic <- gx_usgs_daily_bounded_text_impl(
    gx_usgs_daily_query_value_impl(query, query_names, "statistic_id"),
    "statistic code", maximum = 5L
  )
  if (!grepl("^USGS-[A-Za-z0-9]+$", site, perl = TRUE) ||
      !grepl("^[0-9]{5}$", parameter, perl = TRUE) ||
      !grepl("^[0-9]{5}$", statistic, perl = TRUE)) {
    gx_usgs_daily_abort(
      "M7n requires one USGS monitoring-location identity, one five-digit parameter code, and one five-digit statistic code.",
      "gx_error_usgs_daily_plan_url"
    )
  }
  time <- gx_usgs_daily_time_impl(distribution)
  limit <- unname(as.integer(min(
    50000L, max_rows, floor(max_fields / 11)
  )))
  if (limit < 1L) {
    gx_usgs_daily_abort(
      "The M7n result limits do not admit one daily observation.",
      "gx_error_usgs_daily_plan_policy"
    )
  }
  expected_optional <- list(
    time = time$time,
    properties = paste(.gx_usgs_daily_properties, collapse = ","),
    skipGeometry = "true",
    f = "json",
    lang = "en-US",
    limit = as.character(limit)
  )
  for (name in names(expected_optional)) {
    inherited <- gx_usgs_daily_query_value_impl(query, query_names, name)
    if (!is.null(inherited) && !identical(inherited, expected_optional[[name]])) {
      gx_usgs_daily_abort(
        "An inherited USGS daily query option does not match the bounded M7n request.",
        "gx_error_usgs_daily_plan_url"
      )
    }
  }
  api_version <- gx_usgs_daily_bounded_text_impl(
    path_parts[[2L]], "API version", maximum = 64L
  )
  collection <- path_parts[[3L]]
  endpoint_path <- sub("/$", "", parsed$path)
  base_path <- sub("/collections/daily/items$", "", endpoint_path)
  parsed$query <- list()
  parsed$fragment <- NULL
  endpoint <- httr2::url_build(parsed)
  parsed$path <- if (nzchar(base_path)) base_path else "/"
  base <- httr2::url_build(parsed)
  arguments <- list(
    .url = endpoint,
    f = "json",
    lang = "en-US",
    properties = expected_optional$properties,
    monitoring_location_id = site,
    parameter_code = parameter,
    statistic_id = statistic,
    time = time$time,
    skipGeometry = "true",
    limit = as.character(limit)
  )
  target <- do.call(httr2::url_modify_query, arguments)
  list(
    source = canonical,
    target = gx_canonical_url(target),
    base = gx_canonical_url(base),
    api_version = api_version,
    collection = collection,
    site = site,
    parameter = parameter,
    statistic = statistic,
    time = time,
    limit = limit
  )
}

gx_usgs_daily_request_id_impl <- function(source, policy, row) {
  gx_contract_hash(
    list(
      "distribution_id", row$distribution$distribution_id[[1L]],
      "reservation_id", row$reservation$reservation_id[[1L]],
      "fetch_order", row$distribution$fetch_order[[1L]],
      "canonical_source", source$source,
      "canonical_target", source$target,
      "base_url", source$base,
      "api_version", source$api_version,
      "collection", source$collection,
      "query_type", policy$query_type,
      "monitoring_location_id", source$site,
      "parameter", source$parameter,
      "statistic", source$statistic,
      "time_start", source$time$time_start,
      "time_end", source$time$time_end,
      "time", source$time$time,
      "properties", paste(policy$properties, collapse = ","),
      "skip_geometry", "true",
      "language", "en-US",
      "limit", source$limit,
      "response_format", policy$response_format,
      "implementation_id", policy$implementation_id,
      "minimum_version", policy$minimum_version,
      "query_symbol", policy$query_symbol,
      "normalization_owner", policy$normalization_owner,
      "max_fields", policy$max_fields,
      "max_json_depth", policy$max_json_depth,
      "max_json_members", policy$max_json_members,
      "max_physical_attempts", row$reservation$max_physical_attempts[[1L]],
      "max_encoded_bytes", gx_csv_request_plan_byte_hash_value(
        row$reservation$max_encoded_bytes[[1L]]
      ),
      "max_decoded_bytes", gx_csv_request_plan_byte_hash_value(
        row$reservation$max_decoded_bytes[[1L]]
      ),
      "max_rows", row$max_rows,
      "max_columns", row$max_columns
    ),
    namespace = "geoconnexr.usgs-daily-request.v1",
    contract_version = .gx_usgs_daily_request_plan_contract_version
  )
}

gx_usgs_daily_request_impl <- function(row, source, policy) {
  byte_limit <- unname(as.double(min(
    row$reservation$max_encoded_bytes[[1L]],
    row$reservation$max_decoded_bytes[[1L]]
  )))
  list(
    logical_request_id = gx_usgs_daily_request_id_impl(source, policy, row),
    reservation_id = unname(row$reservation$reservation_id[[1L]]),
    distribution_id = unname(row$distribution$distribution_id[[1L]]),
    fetch_order = unname(row$distribution$fetch_order[[1L]]),
    base_url_redacted = gx_redact_url(source$base),
    api_version = source$api_version,
    collection_id = source$collection,
    query_type = policy$query_type,
    monitoring_location_id = source$site,
    parameter_code = source$parameter,
    statistic_id = source$statistic,
    time_start = source$time$time_start,
    time_end = source$time$time_end,
    time = source$time$time,
    properties = paste(policy$properties, collapse = ","),
    skip_geometry = TRUE,
    response_format = policy$response_format,
    language = "en-US",
    limit = source$limit,
    source_url_redacted = gx_redact_url(source$source),
    canonical_url_redacted = gx_redact_url(source$target),
    max_physical_attempts = 1L,
    max_encoded_bytes = unname(as.double(
      row$reservation$max_encoded_bytes[[1L]]
    )),
    max_decoded_bytes = unname(as.double(
      row$reservation$max_decoded_bytes[[1L]]
    )),
    response_byte_limit = byte_limit,
    max_rows = row$max_rows,
    max_columns = row$max_columns,
    request_status = "usgs_daily_request_planned"
  )
}

gx_usgs_daily_plan_reasons_impl <- function(request_plan, request) {
  reasons <- unique(c(
    setdiff(
      request_plan$metadata$non_replayable_reasons,
      c(
        "arbitrary_provider_client_unimplemented",
        "non_csv_request_plans_absent"
      )
    ),
    "usgs_daily_provider_transport_unauthorized",
    "usgs_daily_result_unbound",
    "runtime_symbol_check_pending",
    "daily_collection_only",
    "single_page_no_follow"
  ))
  reasons[gx_catalog_byte_order(reasons)]
}

gx_usgs_daily_plan_metadata_impl <- function(request_plan, request) {
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
    non_replayable_reasons = gx_usgs_daily_plan_reasons_impl(request_plan, request)
  )
}

gx_usgs_daily_request_plan_new_impl <- function(
    request_plan, policy, request, metadata) {
  object <- structure(
    list(
      contract_version = .gx_usgs_daily_request_plan_contract_version,
      request_plan = request_plan,
      policy = policy,
      request = request,
      metadata = metadata
    ),
    class = "gx_usgs_daily_request_plan"
  )
  gx_usgs_daily_request_plan_validate_impl(object)
  object
}

gx_usgs_daily_request_plan_impl <- function(
    request_plan, distribution_id, max_fields = NULL) {
  policy <- gx_usgs_daily_policy_impl(max_fields)
  row <- gx_usgs_daily_source_row_impl(request_plan, distribution_id)
  source <- gx_usgs_daily_target_impl(
    row$distribution$distribution_url[[1L]], row$distribution,
    row$max_rows, policy$max_fields
  )
  request <- gx_usgs_daily_request_impl(row, source, policy)
  gx_usgs_daily_request_plan_new_impl(
    request_plan = request_plan,
    policy = policy,
    request = request,
    metadata = gx_usgs_daily_plan_metadata_impl(request_plan, request)
  )
}

gx_usgs_daily_request_plan_validate_body <- function(x) {
  valid_top <- identical(class(x), "gx_usgs_daily_request_plan") &&
    identical(names(x), .gx_usgs_daily_request_plan_fields) &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(x$contract_version, .gx_usgs_daily_request_plan_contract_version)
  if (!valid_top) {
    gx_usgs_daily_abort("The USGS daily request plan violates its exact top-level contract.")
  }
  row <- gx_usgs_daily_source_row_impl(
    x$request_plan, x$request$distribution_id %||% NA_character_
  )
  if (!gx_usgs_daily_exact_names(x$policy, .gx_usgs_daily_policy_fields) ||
      !gx_usgs_daily_exact_names(x$request, .gx_usgs_daily_request_fields) ||
      !gx_usgs_daily_exact_names(x$metadata, .gx_usgs_daily_plan_metadata_fields)) {
    gx_usgs_daily_abort("The USGS daily request plan contains malformed nested facts.")
  }
  policy <- gx_usgs_daily_policy_impl(x$policy$max_fields)
  source <- gx_usgs_daily_target_impl(
    row$distribution$distribution_url[[1L]], row$distribution,
    row$max_rows, policy$max_fields
  )
  request <- gx_usgs_daily_request_impl(row, source, policy)
  metadata <- gx_usgs_daily_plan_metadata_impl(x$request_plan, request)
  if (!identical(x$policy, policy) || !identical(x$request, request) ||
      !identical(x$metadata, metadata)) {
    gx_usgs_daily_abort(
      "The USGS daily request plan no longer rebinds to its source and reservation.",
      "gx_error_usgs_daily_plan_binding"
    )
  }
  invisible(x)
}

gx_usgs_daily_request_plan_validate_impl <- function(x) {
  tryCatch(
    gx_usgs_daily_request_plan_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_usgs_daily")) stop(cnd)
      gx_usgs_daily_abort("USGS daily request-plan validation rejected a malformed object.")
    },
    warning = function(cnd) {
      gx_usgs_daily_abort(
        "USGS daily request-plan validation rejected a warning-producing object."
      )
    }
  )
}

#' @export
print.gx_usgs_daily_request_plan <- function(x, ...) {
  gx_usgs_daily_request_plan_validate_impl(x)
  cli::cli_inform(c(
    "<gx_usgs_daily_request_plan>",
    "* Collection: {x$request$collection_id}; site: {x$request$monitoring_location_id}",
    "* Parameter: {x$request$parameter_code}; statistic: {x$request$statistic_id}",
    "* Local dates: {x$request$time}; page limit: {x$request$limit}",
    "* Reserved attempts: 1; requests executed: 0"
  ))
  invisible(x)
}

gx_usgs_daily_json_text_impl <- function(body) {
  if (!is.raw(body) || !is.null(attributes(body)) || !length(body)) {
    gx_usgs_daily_abort(
      "The USGS daily response body must contain bounded raw GeoJSON bytes.",
      "gx_error_usgs_daily_payload"
    )
  }
  text <- tryCatch(rawToChar(body), error = function(cnd) NULL)
  valid <- !is.null(text) && !startsWith(text, "\ufeff") &&
    identical(tryCatch(
      stringi::stri_enc_isutf8(text), error = function(cnd) FALSE
    ), TRUE)
  if (!valid) {
    gx_usgs_daily_abort(
      "The USGS daily response is not one BOM-free UTF-8 JSON document.",
      "gx_error_usgs_daily_payload"
    )
  }
  Encoding(text) <- "UTF-8"
  text
}

gx_usgs_daily_assert_unique_members_impl <- function(value) {
  stack <- list(value)
  while (length(stack)) {
    current <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    if (!is.list(current)) next
    object_names <- names(current)
    if (!is.null(object_names) &&
        (anyNA(object_names) || any(!nzchar(object_names)) ||
           anyDuplicated(object_names))) {
      gx_usgs_daily_abort(
      "The USGS daily GeoJSON contains duplicate or invalid object members.",
        "gx_error_usgs_daily_payload"
      )
    }
    children <- current[vapply(current, is.list, logical(1))]
    if (length(children)) stack <- c(stack, unname(children))
  }
  invisible(value)
}

gx_usgs_daily_json_impl <- function(body, policy) {
  text <- gx_usgs_daily_json_text_impl(body)
  preflight <- tryCatch({
    gx_graph_json_preflight(
      text,
      max_depth = policy$max_json_depth,
      max_members = policy$max_json_members
    )
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!preflight) {
    gx_usgs_daily_abort(
      "The USGS daily GeoJSON exceeds its structural parsing limits.",
      "gx_error_usgs_daily_payload"
    )
  }
  document <- tryCatch(
    withCallingHandlers(
      jsonlite::fromJSON(text, simplifyVector = FALSE),
      warning = function(cnd) stop(cnd)
    ),
    error = function(cnd) NULL
  )
  if (!is.list(document) || is.null(names(document))) {
    gx_usgs_daily_abort(
      "The USGS daily response is not one GeoJSON object.",
      "gx_error_usgs_daily_payload"
    )
  }
  gx_usgs_daily_assert_unique_members_impl(document)
  document
}

gx_usgs_daily_json_array_impl <- function(x, label, allow_null = FALSE) {
  if (!is.list(x) || (!allow_null && any(vapply(x, is.null, logical(1))))) {
    gx_usgs_daily_abort(
      "The USGS daily GeoJSON {label} array is malformed.",
      "gx_error_usgs_daily_payload"
    )
  }
  x
}

gx_usgs_daily_json_object_impl <- function(x, label) {
  valid <- is.list(x) && !is.null(names(x)) && length(x) > 0L &&
    !anyNA(names(x)) && all(nzchar(names(x))) && !anyDuplicated(names(x))
  if (!valid) {
    gx_usgs_daily_abort(
      "The USGS daily GeoJSON {label} object is malformed.",
      "gx_error_usgs_daily_payload"
    )
  }
  x
}

gx_usgs_daily_datetime_impl <- function(value) {
  if (!gx_usgs_daily_scalar_text(value)) return(NA_real_)
  normalized <- sub("Z$", "+0000", value)
  normalized <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", normalized)
  parsed <- suppressWarnings(as.numeric(as.POSIXct(
    normalized,
    format = "%Y-%m-%dT%H:%M:%OS%z",
    tz = "UTC"
  )))
  if (!is.finite(parsed)) return(NA_real_)
  parsed
}

gx_usgs_daily_date_impl <- function(value) {
  if (!gx_usgs_daily_scalar_text(value) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value, perl = TRUE)) {
    return(NA_real_)
  }
  parsed <- suppressWarnings(as.numeric(as.Date(value, format = "%Y-%m-%d")))
  if (!is.finite(parsed) ||
      !identical(format(as.Date(parsed, origin = "1970-01-01"), "%Y-%m-%d"),
                 value)) {
    return(NA_real_)
  }
  parsed
}

gx_usgs_daily_schema_impl <- function(data) {
  storage <- vapply(data, function(column) {
    if (inherits(column, "POSIXct")) {
      "POSIXct[UTC]"
    } else if (inherits(column, "Date")) {
      "Date"
    } else {
      typeof(column)
    }
  }, character(1))
  tibble::tibble(
    column_index = seq_along(data),
    column_name = names(data),
    storage_type = unname(storage)
  )
}

gx_usgs_daily_whole_impl <- function(x, label, allow_null = FALSE) {
  if (allow_null && is.null(x)) return(NA_integer_)
  valid <- is.numeric(x) && !is.logical(x) && length(x) == 1L &&
    !is.na(x) && is.finite(x) && x >= 0 && x == floor(x) &&
    x <= .Machine$integer.max && is.null(attributes(x))
  if (!valid) {
    gx_usgs_daily_abort(
      "The USGS daily GeoJSON {label} is not one bounded whole number.",
      "gx_error_usgs_daily_payload"
    )
  }
  unname(as.integer(x))
}

gx_usgs_daily_property_text_impl <- function(
    value, label, allow_null = FALSE) {
  if (allow_null && is.null(value)) return(NA_character_)
  valid <- gx_usgs_daily_scalar_text(value) &&
    nchar(value, type = "bytes") <= 4096L &&
    identical(tryCatch(
      stringi::stri_enc_isutf8(value), error = function(cnd) FALSE
    ), TRUE) && identical(tryCatch(
      stringi::stri_detect_regex(value, "[\\p{Cc}\\p{Cf}\\p{Cs}]"),
      error = function(cnd) TRUE
    ), FALSE)
  if (!valid) {
    gx_usgs_daily_abort(
      "The USGS daily GeoJSON {label} is not one bounded control-safe UTF-8 string.",
      "gx_error_usgs_daily_payload"
    )
  }
  unname(enc2utf8(value))
}

gx_usgs_daily_feature_impl <- function(feature, request_plan) {
  request <- request_plan$request
  feature <- gx_usgs_daily_json_object_impl(feature, "feature")
  if (!setequal(names(feature), c("type", "id", "geometry", "properties")) ||
      !identical(feature$type, "Feature") ||
      !gx_usgs_daily_scalar_text(feature$id) ||
      !is.null(feature$geometry)) {
    gx_usgs_daily_abort(
      "M7n requires identified GeoJSON Features with omitted geometry.",
      "gx_error_usgs_daily_payload"
    )
  }
  properties <- gx_usgs_daily_json_object_impl(
    feature$properties, "feature properties"
  )
  if (length(properties) != length(.gx_usgs_daily_properties) ||
      !setequal(names(properties), .gx_usgs_daily_properties)) {
    gx_usgs_daily_abort(
      "A USGS daily feature does not have the exact requested property set.",
      "gx_error_usgs_daily_payload"
    )
  }
  values <- lapply(.gx_usgs_daily_properties, function(name) {
    gx_usgs_daily_property_text_impl(
      properties[[name]], name, allow_null = identical(name, "qualifier")
    )
  })
  names(values) <- .gx_usgs_daily_properties
  if (!identical(values$monitoring_location_id, request$monitoring_location_id) ||
      !identical(values$parameter_code, request$parameter_code) ||
      !identical(values$statistic_id, request$statistic_id)) {
    gx_usgs_daily_abort(
      "A USGS daily feature does not match the planned site, parameter, and statistic.",
      "gx_error_usgs_daily_payload"
    )
  }
  observed_time <- gx_usgs_daily_date_impl(values$time)
  modified_time <- gx_usgs_daily_datetime_impl(values$last_modified)
  start <- gx_usgs_daily_date_impl(request$time_start)
  end <- gx_usgs_daily_date_impl(request$time_end)
  if (anyNA(c(observed_time, modified_time, start, end)) ||
      observed_time < start || observed_time > end) {
    gx_usgs_daily_abort(
      "A USGS daily feature has an invalid or out-of-window local date or last-modified timestamp.",
      "gx_error_usgs_daily_payload"
    )
  }
  list(
    daily_id = gx_usgs_daily_property_text_impl(
      feature$id, "feature identity"
    ),
    time_series_id = values$time_series_id,
    monitoring_location_id = values$monitoring_location_id,
    parameter_code = values$parameter_code,
    statistic_id = values$statistic_id,
    time = as.Date(observed_time, origin = "1970-01-01"),
    value = values$value,
    unit_of_measure = values$unit_of_measure,
    approval_status = values$approval_status,
    qualifier = values$qualifier,
    last_modified = as.POSIXct(modified_time, origin = "1970-01-01", tz = "UTC")
  )
}

gx_usgs_daily_data_impl <- function(features, request_plan) {
  features <- gx_usgs_daily_json_array_impl(
    features, "features", allow_null = FALSE
  )
  if (length(features) > request_plan$request$limit ||
      length(features) > request_plan$request$max_rows) {
    gx_usgs_daily_abort(
      "The USGS daily page exceeds its planned row limit.",
      "gx_error_usgs_daily_payload"
    )
  }
  rows <- lapply(features, gx_usgs_daily_feature_impl,
                 request_plan = request_plan)
  if (!length(rows)) {
    return(tibble::tibble(
      daily_id = character(), time_series_id = character(),
      monitoring_location_id = character(), parameter_code = character(),
      statistic_id = character(),
      time = as.Date(character()), value = character(),
      unit_of_measure = character(), approval_status = character(),
      qualifier = character(),
      last_modified = as.POSIXct(character(), tz = "UTC")
    ))
  }
  tibble::tibble(
    daily_id = vapply(rows, `[[`, character(1), "daily_id"),
    time_series_id = vapply(rows, `[[`, character(1), "time_series_id"),
    monitoring_location_id = vapply(
      rows, `[[`, character(1), "monitoring_location_id"
    ),
    parameter_code = vapply(rows, `[[`, character(1), "parameter_code"),
    statistic_id = vapply(rows, `[[`, character(1), "statistic_id"),
    time = as.Date(vapply(rows, function(row) as.numeric(row$time), numeric(1)),
                   origin = "1970-01-01"),
    value = vapply(rows, `[[`, character(1), "value"),
    unit_of_measure = vapply(rows, `[[`, character(1), "unit_of_measure"),
    approval_status = vapply(rows, `[[`, character(1), "approval_status"),
    qualifier = vapply(rows, `[[`, character(1), "qualifier"),
    last_modified = as.POSIXct(
      vapply(rows, function(row) as.numeric(row$last_modified), numeric(1)),
      origin = "1970-01-01", tz = "UTC"
    )
  )
}

gx_usgs_daily_next_link_impl <- function(links) {
  links <- gx_usgs_daily_json_array_impl(links, "links")
  rels <- vapply(links, function(link) {
    link <- gx_usgs_daily_json_object_impl(link, "link")
    rel <- gx_usgs_daily_property_text_impl(link$rel, "link relation")
    if (identical(rel, "next")) {
      gx_usgs_daily_property_text_impl(link$href, "next-link target")
    }
    rel
  }, character(1))
  if (sum(rels == "next") > 1L) {
    gx_usgs_daily_abort(
      "The USGS daily page contains multiple next links.",
      "gx_error_usgs_daily_payload"
    )
  }
  any(rels == "next")
}

gx_usgs_daily_strict_result_impl <- function(body, request_plan) {
  document <- gx_usgs_daily_json_impl(body, request_plan$policy)
  required <- c("type", "features", "numberReturned", "links")
  if (!all(required %in% names(document)) ||
      !identical(document$type, "FeatureCollection") ||
      is.null(document$features) || is.null(document$links)) {
    gx_usgs_daily_abort(
      "M7n requires one USGS GeoJSON FeatureCollection page.",
      "gx_error_usgs_daily_payload"
    )
  }
  data <- gx_usgs_daily_data_impl(document$features, request_plan)
  number_returned <- gx_usgs_daily_whole_impl(
    document$numberReturned, "numberReturned"
  )
  number_matched <- if ("numberMatched" %in% names(document)) {
    gx_usgs_daily_whole_impl(
      document$numberMatched, "numberMatched", allow_null = TRUE
    )
  } else {
    NA_integer_
  }
  if (number_returned != nrow(data) ||
      (!is.na(number_matched) && number_matched < number_returned)) {
    gx_usgs_daily_abort(
      "The USGS daily page counts do not match its feature data.",
      "gx_error_usgs_daily_payload"
    )
  }
  next_link <- gx_usgs_daily_next_link_impl(document$links)
  truncated <- next_link ||
    (!is.na(number_matched) && number_matched > number_returned) ||
    number_returned == request_plan$request$limit
  stop_reason <- if (next_link) {
    "next_link_not_followed"
  } else if (!is.na(number_matched) && number_matched > number_returned) {
    "number_matched_exceeds_page"
  } else if (number_returned == request_plan$request$limit) {
    "page_limit_reached"
  } else {
    "complete_single_page"
  }
  field_count <- unname(as.double(nrow(data) * ncol(data)))
  if (ncol(data) > request_plan$request$max_columns ||
      field_count > request_plan$policy$max_fields) {
    gx_usgs_daily_abort(
      "The normalized USGS daily result exceeds its planned shape limits.",
      "gx_error_usgs_daily_payload"
    )
  }
  schema <- gx_usgs_daily_schema_impl(data)
  parse <- list(
    body_sha256 = digest::digest(body, algo = "sha256", serialize = FALSE),
    result_sha256 = gx_csv_parsed_response_result_hash_impl(data),
    row_count = nrow(data),
    column_count = ncol(data),
    field_count = field_count,
    number_returned = number_returned,
    number_matched = number_matched,
    truncated = truncated,
    stop_reason = stop_reason,
    parser_validation = "native_geojson_exact_daily_subset"
  )
  list(data = data, schema = schema, parse = parse)
}

gx_usgs_daily_result_impl <- function(body, request_plan) {
  gx_usgs_daily_strict_result_impl(body, request_plan)
}

gx_usgs_daily_candidate_impl <- function(response, request_plan, target) {
  policy <- request_plan$policy
  request <- request_plan$request
  media_type <- gx_media_type(response$headers)
  content_encoding <- tolower(trimws(
    gx_header(response$headers, "content-encoding") %||% "identity"
  ))
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
    gx_usgs_daily_abort(
      "The provider response violated the planned USGS daily response envelope.",
      "gx_error_usgs_daily_response"
    )
  }
  gx_usgs_daily_result_impl(response$body, request_plan)
}

gx_usgs_daily_symbol_resolver_impl <- function(
    package, query_symbol, minimum_version) {
  namespace <- tryCatch(asNamespace(package), error = function(cnd) NULL)
  if (is.null(namespace)) return(NULL)
  package_version <- tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(cnd) NULL
  )
  query <- get0(
    query_symbol, envir = namespace, inherits = FALSE, ifnotfound = NULL
  )
  sufficient <- !is.null(package_version) &&
    tryCatch(
      utils::compareVersion(package_version, minimum_version) >= 0L,
      error = function(cnd) FALSE
    )
  exported <- query_symbol %in% tryCatch(
    getNamespaceExports(package), error = function(cnd) character()
  )
  required_formals <- c(
    "monitoring_location_id", "parameter_code", "statistic_id", "properties",
    "time", "limit", "no_paging"
  )
  if (!sufficient || !exported || !is.function(query) ||
      !all(required_formals %in% names(formals(query)))) {
    return(NULL)
  }
  list(
    package_version = unname(package_version),
    query = query
  )
}

gx_handler_usgs_waterdata_daily <- function(
    request_plan, timeout, min_interval, capability) {
  gx_usgs_daily_request_plan_validate_impl(request_plan)
  source <- gx_usgs_daily_source_row_impl(
    request_plan$request_plan, request_plan$request$distribution_id
  )
  target <- gx_usgs_daily_target_impl(
    source$distribution$distribution_url[[1L]], source$distribution,
    source$max_rows, request_plan$policy$max_fields
  )$target
  client <- gx_client(
    endpoint = "pid",
    timeout = timeout,
    retries = 0L,
    min_interval = min_interval,
    max_bytes = as.integer(request_plan$request$response_byte_limit),
    cache = FALSE,
    offline = FALSE,
    cache_dir = tempdir()
  )
  result <- NULL
  validator <- function(response) {
    result <<- gx_usgs_daily_candidate_impl(response, request_plan, target)
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
    gx_usgs_daily_abort(
      "The USGS daily provider response bypassed its result validator.",
      "gx_error_usgs_daily_response"
    )
  }
  list(
    response = response,
    result = result,
    target = target,
    package_version = capability$package_version
  )
}

gx_usgs_daily_invoke_impl <- function(
    request_plan, timeout, min_interval, symbol_resolver) {
  resolver <- symbol_resolver %||% gx_usgs_daily_symbol_resolver_impl
  if (!is.function(resolver)) {
    gx_usgs_daily_abort(
      "The USGS daily runtime implementation resolver is invalid.",
      "gx_error_usgs_daily_execution_capability"
    )
  }
  capability <- tryCatch(
    resolver(
      request_plan$policy$implementation_package,
      request_plan$policy$query_symbol,
      request_plan$policy$minimum_version
    ),
    error = function(cnd) NULL
  )
  valid <- is.list(capability) &&
    identical(names(capability), c("package_version", "query")) &&
    gx_usgs_daily_scalar_text(capability$package_version) &&
    is.function(capability$query) &&
    tryCatch(
      utils::compareVersion(
        capability$package_version, request_plan$policy$minimum_version
      ) >= 0L,
      error = function(cnd) FALSE
    )
  if (!valid) {
    gx_usgs_daily_abort(
      "The required dataRetrieval version and daily query symbol are unavailable at invocation time.",
      "gx_error_usgs_daily_execution_capability"
    )
  }
  gx_handler_usgs_waterdata_daily(
    request_plan, timeout, min_interval, capability
  )
}

gx_usgs_daily_execution_id_impl <- function(
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
      "minimum_version", request_plan$policy$minimum_version,
      "query_symbol", request_plan$policy$query_symbol,
      "normalization_owner", request_plan$policy$normalization_owner
    ),
    namespace = "geoconnexr.usgs-daily-execution.v1",
    contract_version = .gx_usgs_daily_execution_contract_version
  )
}

gx_usgs_daily_attempt_id_impl <- function(execution_id, request_plan, target, attempt) {
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
    namespace = "geoconnexr.usgs-daily-attempt.v1",
    contract_version = .gx_usgs_daily_execution_contract_version
  )
}

gx_usgs_daily_attempts_impl <- function(
    response, request_plan, execution_id, target, result) {
  source <- response$attempts
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
    source$body_sha256[[1L]] == result$parse$body_sha256 &&
    source$resolved_host[[1L]] == gx_safe_target(target, FALSE)$host &&
    gx_is_ipv4(source$resolved_ip[[1L]]) &&
    !gx_is_nonpublic_ipv4(source$resolved_ip[[1L]])
  if (!valid) {
    gx_usgs_daily_abort(
      "The USGS daily handler did not return one charged physical response attempt.",
      "gx_error_usgs_daily_execution_attempt"
    )
  }
  attempt <- tibble::tibble(
    contract_version = .gx_usgs_daily_execution_contract_version,
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
    body_sha256 = result$parse$body_sha256,
    completed_at = gx_csv_execution_time_impl(source$retrieved_at[[1L]])
  )
  attempt$attempt_id[[1L]] <- gx_usgs_daily_attempt_id_impl(
    execution_id, request_plan, target, attempt
  )
  attempt
}

gx_usgs_daily_implementation_impl <- function(request_plan, package_version) {
  if (!gx_usgs_daily_scalar_text(package_version) ||
      !tryCatch(
        utils::compareVersion(
          package_version, request_plan$policy$minimum_version
        ) >= 0L,
        error = function(cnd) FALSE
      )) {
    gx_usgs_daily_abort(
      "The recorded USGS daily package version does not satisfy the request plan.",
      "gx_error_usgs_daily_execution_capability"
    )
  }
  list(
    implementation_id = request_plan$policy$implementation_id,
    package = request_plan$policy$implementation_package,
    minimum_version = request_plan$policy$minimum_version,
    package_version = package_version,
    query_symbol = request_plan$policy$query_symbol,
    normalization_owner = request_plan$policy$normalization_owner,
    resolution_status = "verified_at_invocation",
    invocation_status = "capability_checked_transport_and_normalization_package_owned"
  )
}

gx_usgs_daily_execution_facts_impl <- function(
    scope, execution_id, request_plan, started_at, completed_at, timeout,
    min_interval, result, bytes) {
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
    row_count = result$parse$row_count,
    column_count = result$parse$column_count,
    execution_status = "provider_response_validated_and_parsed"
  )
}

gx_usgs_daily_execution_reasons_impl <- function(request_plan) {
  reasons <- unique(c(
    setdiff(
      request_plan$metadata$non_replayable_reasons,
      c(
        "usgs_daily_provider_transport_unauthorized",
        "usgs_daily_result_unbound",
        "runtime_symbol_check_pending", "transport_adapter_unimplemented",
        "attempt_identity_unbound", "attempt_ledger_unbound",
        "provider_transport_unauthorized", "result_schema_unbound",
        "timeout_policy_unbound"
      )
    ),
    "daily_collection_only",
    "single_page_no_follow",
    "serialization_unbound"
  ))
  reasons[gx_catalog_byte_order(reasons)]
}

gx_usgs_daily_execution_metadata_impl <- function(request_plan, result, bytes) {
  list(
    host_specific = TRUE,
    replayable = FALSE,
    execution_ready = FALSE,
    transport_authorized = TRUE,
    execution_completed = TRUE,
    provider_response_observed = TRUE,
    budgets_consumed = TRUE,
    response_validated = TRUE,
    dataretrieval_symbol_checked = TRUE,
    native_geojson_parsed = TRUE,
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
      rows = result$parse$row_count,
      columns = result$parse$column_count
    ),
    non_replayable_reasons = gx_usgs_daily_execution_reasons_impl(request_plan)
  )
}

gx_usgs_daily_redacted_attempts_impl <- function(cnd) {
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

gx_usgs_daily_execution_impl <- function(
    request_plan,
    timeout = NULL,
    min_interval = NULL,
    execution_scope_id = NULL,
    symbol_resolver = NULL) {
  gx_usgs_daily_request_plan_validate_impl(request_plan)
  scope <- gx_csv_execution_scope_impl(execution_scope_id)
  timeout <- gx_csv_execution_number_impl(
    timeout, "timeout", 0, 600, allow_zero = FALSE
  )
  min_interval <- gx_csv_execution_number_impl(
    min_interval, "min_interval", 0, 3600, allow_zero = TRUE
  )
  started_at <- gx_csv_execution_time_impl(gx_now())
  execution_id <- gx_usgs_daily_execution_id_impl(
    scope, request_plan, started_at, timeout, min_interval
  )
  invoked <- tryCatch(
    gx_usgs_daily_invoke_impl(
      request_plan, timeout, min_interval, symbol_resolver
    ),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_usgs_daily_execution_capability")) stop(cnd)
      phase <- if (inherits(cnd, c(
        "gx_error_usgs_daily_payload", "gx_error_csv_parse"
      ))) "parse" else "transport"
      gx_usgs_daily_abort(
        "USGS daily execution failed during its bounded {phase} phase; underlying details were withheld.",
        paste0("gx_error_usgs_daily_execution_", phase),
        execution_id = execution_id,
        attempts = gx_usgs_daily_redacted_attempts_impl(cnd)
      )
    }
  )
  completed_at <- gx_csv_execution_time_impl(gx_now())
  attempts <- gx_usgs_daily_attempts_impl(
    invoked$response, request_plan, execution_id, invoked$target,
    invoked$result
  )
  bytes <- unname(as.double(length(invoked$response$body)))
  execution <- gx_usgs_daily_execution_facts_impl(
    scope, execution_id, request_plan, started_at, completed_at, timeout,
    min_interval, invoked$result, bytes
  )
  object <- structure(
    list(
      contract_version = .gx_usgs_daily_execution_contract_version,
      request_plan = request_plan,
      response_body = invoked$response$body,
      data = invoked$result$data,
      schema = invoked$result$schema,
      parse = invoked$result$parse,
      implementation = gx_usgs_daily_implementation_impl(
        request_plan, invoked$package_version
      ),
      execution = execution,
      attempts = attempts,
      metadata = gx_usgs_daily_execution_metadata_impl(
        request_plan, invoked$result, bytes
      )
    ),
    class = "gx_usgs_daily_execution"
  )
  gx_usgs_daily_execution_validate_impl(object)
  object
}

gx_usgs_daily_execution_validate_body <- function(x) {
  valid_top <- identical(class(x), "gx_usgs_daily_execution") &&
    identical(names(x), .gx_usgs_daily_execution_fields) &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(x$contract_version, .gx_usgs_daily_execution_contract_version)
  if (!valid_top) {
    gx_usgs_daily_abort("The USGS daily execution violates its exact top-level contract.")
  }
  gx_usgs_daily_request_plan_validate_impl(x$request_plan)
  if (!is.raw(x$response_body) || !is.null(attributes(x$response_body)) ||
      !inherits(x$data, "tbl_df") || !inherits(x$schema, "tbl_df") ||
      !gx_usgs_daily_exact_names(x$parse, .gx_usgs_daily_parse_fields) ||
      !gx_usgs_daily_exact_names(
        x$implementation, .gx_usgs_daily_implementation_fields
      ) || !gx_usgs_daily_exact_names(x$execution, .gx_usgs_daily_execution_fact_fields) ||
      !gx_usgs_daily_exact_names(x$metadata, .gx_usgs_daily_execution_metadata_fields)) {
    gx_usgs_daily_abort("The USGS daily execution contains malformed nested facts.")
  }
  rebuilt <- gx_usgs_daily_strict_result_impl(x$response_body, x$request_plan)
  if (!identical(x$data, rebuilt$data) ||
      !identical(x$schema, rebuilt$schema) ||
      !identical(x$parse, rebuilt$parse)) {
    gx_usgs_daily_abort(
      "The USGS daily result no longer rebinds to its retained response bytes.",
      "gx_error_usgs_daily_execution_result"
    )
  }
  source <- gx_usgs_daily_source_row_impl(
    x$request_plan$request_plan, x$request_plan$request$distribution_id
  )
  target <- gx_usgs_daily_target_impl(
    source$distribution$distribution_url[[1L]], source$distribution,
    source$max_rows, x$request_plan$policy$max_fields
  )$target
  attempts <- x$attempts
  valid_attempt_shape <- inherits(attempts, "tbl_df") &&
    nrow(attempts) == 1L && identical(names(attempts), .gx_usgs_daily_attempt_columns) &&
    !anyNA(attempts) && all(vapply(attempts, function(column) {
      is.null(attributes(column))
    }, logical(1)))
  if (!valid_attempt_shape) {
    gx_usgs_daily_abort("The USGS daily attempt ledger violates its exact shape.")
  }
  bytes <- unname(as.double(length(x$response_body)))
  expected_implementation <- gx_usgs_daily_implementation_impl(
    x$request_plan, x$implementation$package_version
  )
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
    gx_usgs_daily_execution_id_impl(
      scope, x$request_plan, x$execution$started_at, timeout, interval
    )
  } else {
    NA_character_
  }
  attempt_time <- gx_csv_execution_parse_time_impl(
    attempts$completed_at[[1L]]
  )
  attempt_values <- attempts$contract_version[[1L]] ==
      .gx_usgs_daily_execution_contract_version &&
    attempts$attempt_number[[1L]] == 1L &&
    attempts$execution_id[[1L]] == expected_execution_id &&
    attempts$logical_request_id[[1L]] ==
      x$request_plan$request$logical_request_id &&
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
    attempts$body_sha256[[1L]] == rebuilt$parse$body_sha256 &&
    !is.na(attempt_time)
  expected_attempt_id <- if (attempt_values) {
    gx_usgs_daily_attempt_id_impl(
      expected_execution_id, x$request_plan, target, attempts
    )
  } else {
    NA_character_
  }
  execution_values <- !is.na(started) && !is.na(completed) &&
    !is.na(attempt_time) && completed >= started && completed >= attempt_time &&
    x$execution$execution_id == expected_execution_id &&
    x$execution$logical_request_id ==
      x$request_plan$request$logical_request_id &&
    x$execution$reservation_id == x$request_plan$request$reservation_id &&
    x$execution$distribution_id == x$request_plan$request$distribution_id &&
    identical(x$execution$encoded_bytes, bytes) &&
    identical(x$execution$decoded_bytes, bytes) &&
    x$execution$row_count == rebuilt$parse$row_count &&
    x$execution$column_count == rebuilt$parse$column_count &&
    x$execution$execution_status ==
      "provider_response_validated_and_parsed"
  expected_metadata <- gx_usgs_daily_execution_metadata_impl(
    x$request_plan, rebuilt, bytes
  )
  within_budget <- bytes <= x$request_plan$request$response_byte_limit &&
    bytes <= x$request_plan$request$max_encoded_bytes &&
    bytes <= x$request_plan$request$max_decoded_bytes
  if (!identical(x$implementation, expected_implementation) ||
      !attempt_values || attempts$attempt_id[[1L]] != expected_attempt_id ||
      !execution_values || !within_budget ||
      !identical(x$metadata, expected_metadata)) {
    gx_usgs_daily_abort(
      "The USGS daily execution facts no longer rebind to its plan and response.",
      "gx_error_usgs_daily_execution_binding"
    )
  }
  owned_text <- gx_fetch_plan_text_total(
    x, limit = .gx_usgs_daily_max_text_bytes
  )
  if (!is.finite(owned_text) || owned_text > .gx_usgs_daily_max_text_bytes) {
    gx_usgs_daily_abort(
      "The USGS daily execution exceeds its aggregate text budget.",
      "gx_error_usgs_daily_execution_budget"
    )
  }
  invisible(x)
}

gx_usgs_daily_execution_validate_impl <- function(x) {
  tryCatch(
    gx_usgs_daily_execution_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_usgs_daily")) stop(cnd)
      gx_usgs_daily_abort("USGS daily execution validation rejected a malformed object.")
    },
    warning = function(cnd) {
      gx_usgs_daily_abort(
        "USGS daily execution validation rejected a warning-producing object."
      )
    }
  )
}

#' @export
print.gx_usgs_daily_execution <- function(x, ...) {
  gx_usgs_daily_execution_validate_impl(x)
  cli::cli_inform(c(
    "<gx_usgs_daily_execution>",
    "* Site: {x$request_plan$request$monitoring_location_id}; parameter: {x$request_plan$request$parameter_code}",
    "* Rows: {x$execution$row_count}; columns: {x$execution$column_count}",
    "* Attempts: 1; next page followed: no"
  ))
  invisible(x)
}
