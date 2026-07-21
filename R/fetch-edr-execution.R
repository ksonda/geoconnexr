.gx_edr_request_plan_contract_version <- "0.1.0"
.gx_edr_execution_contract_version <- "0.1.0"
.gx_edr_max_text_bytes <- 64L * 1024L^2
.gx_edr_max_json_depth <- 32L

.gx_edr_request_plan_fields <- c(
  "contract_version", "request_plan", "policy", "request", "metadata"
)

.gx_edr_policy_fields <- c(
  "slice_id", "handler_id", "implementation_id", "implementation_package",
  "minimum_version", "query_symbol", "normalizer_symbol", "method",
  "query_type", "response_format", "accept",
  "accept_encoding", "body_bytes", "body_sha256", "credential_policy",
  "redirect_policy", "max_redirects", "retry_policy", "max_retries",
  "max_physical_attempts", "cache_policy", "success_status",
  "response_media_types", "response_content_encoding", "parser_encoding",
  "type_inference", "attribute_policy", "pagination_policy", "max_fields",
  "max_json_depth", "max_json_members"
)

.gx_edr_request_fields <- c(
  "logical_request_id", "reservation_id", "distribution_id", "fetch_order",
  "base_url_redacted", "collection_id", "query_type", "coords_wkt",
  "longitude", "latitude", "parameter_name", "time_start", "time_end",
  "datetime", "crs", "response_format", "source_url_redacted",
  "canonical_url_redacted",
  "max_physical_attempts", "max_encoded_bytes", "max_decoded_bytes",
  "response_byte_limit", "max_rows", "max_columns", "request_status"
)

.gx_edr_plan_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "budgets_allocated", "budgets_consumed", "counts",
  "non_replayable_reasons"
)

.gx_edr_execution_fields <- c(
  "contract_version", "request_plan", "response_body", "data", "schema",
  "parse", "implementation", "execution", "attempts", "metadata"
)

.gx_edr_parse_fields <- c(
  "body_sha256", "result_sha256", "row_count", "column_count",
  "field_count", "coverage_id", "domain_type", "parameter_name",
  "parser_validation"
)

.gx_edr_implementation_fields <- c(
  "implementation_id", "package", "minimum_version", "package_version",
  "query_symbol", "normalizer_symbol", "resolution_status",
  "invocation_status"
)

.gx_edr_execution_fact_fields <- c(
  "execution_scope_id", "execution_id", "logical_request_id",
  "reservation_id", "distribution_id", "started_at", "completed_at",
  "timeout_seconds", "min_interval_seconds", "encoded_bytes",
  "decoded_bytes", "row_count", "column_count", "execution_status"
)

.gx_edr_attempt_columns <- c(
  "contract_version", "attempt_number", "attempt_id", "execution_id",
  "logical_request_id", "reservation_id", "method",
  "canonical_url_redacted", "resolved_host", "resolved_ip", "status",
  "outcome", "media_type", "encoded_bytes", "decoded_bytes",
  "body_sha256", "completed_at"
)

.gx_edr_execution_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "execution_completed", "provider_response_observed", "budgets_consumed",
  "response_validated", "external_normalizer_invoked",
  "external_normalizer_matched", "coveragejson_parsed", "result_contract_bound",
  "attempt_ledger_bound", "runtime_symbol_checked", "observation_origin",
  "counts", "non_replayable_reasons"
)

gx_edr_abort <- function(
    message,
    class = "gx_error_edr_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_edr", "gx_error_fetch_plan")),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_edr_scalar_text <- function(x, nonempty = TRUE) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    (!nonempty || nzchar(x)) && is.null(attributes(x))
}

gx_edr_exact_names <- function(x, expected) {
  is.list(x) && identical(names(x), expected) &&
    identical(names(attributes(x)), "names")
}

gx_edr_bounded_text_impl <- function(
    x, label, allow_empty = FALSE, maximum = 2048L) {
  valid <- gx_edr_scalar_text(x, nonempty = !allow_empty) &&
    nchar(x, type = "bytes") <= maximum &&
    identical(tryCatch(stringi::stri_enc_isutf8(x), error = function(cnd) FALSE), TRUE) &&
    identical(tryCatch(
      stringi::stri_detect_regex(x, "[\\p{Cc}\\p{Cf}\\p{Cs}]"),
      error = function(cnd) TRUE
    ), FALSE)
  if (!valid) {
    gx_edr_abort(
      "The EDR {label} is not one bounded control-safe UTF-8 value.",
      "gx_error_edr_plan_url"
    )
  }
  unname(enc2utf8(x))
}

gx_edr_policy_impl <- function(max_fields) {
  max_fields <- tryCatch(
    gx_csv_parsed_response_field_limit_impl(max_fields),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(max_fields)) {
    gx_edr_abort(
      "The EDR parser field limit must be one explicit bounded whole number.",
      "gx_error_edr_plan_policy"
    )
  }
  list(
    slice_id = "edr_position_single_response_v1",
    handler_id = "edr",
    implementation_id = "geoconnexr:edr4r",
    implementation_package = "edr4r",
    minimum_version = "0.1.1",
    query_symbol = "edr_position",
    normalizer_symbol = "covjson_to_tibble",
    method = "GET",
    query_type = "position",
    response_format = "CoverageJSON",
    accept = "application/prs.coverage+json, application/json;q=0.9",
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
    response_media_types = c(
      "application/prs.coverage+json", "application/json"
    ),
    response_content_encoding = "identity",
    parser_encoding = "UTF-8",
    type_inference = "bounded_position_subset",
    attribute_policy = "disabled",
    pagination_policy = "single_response_no_follow",
    max_fields = max_fields,
    max_json_depth = .gx_edr_max_json_depth,
    max_json_members = unname(as.integer(2L * max_fields + 1024L))
  )
}

gx_edr_source_row_impl <- function(request_plan, distribution_id) {
  valid <- tryCatch({
    gx_csv_request_plan_validate_impl(request_plan)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid || !gx_edr_scalar_text(distribution_id) ||
      !gx_catalog_is_sha256(distribution_id)) {
    gx_edr_abort(
      "M7l planning requires a valid M7d plan and one distribution identity.",
      "gx_error_edr_plan_input"
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
    gx_edr_abort(
      "The selected EDR distribution has no complete M7d reservation.",
      "gx_error_edr_plan_input"
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
    coverage$handler_id[[1L]] == "edr" &&
    coverage$request_status[[1L]] == "handler_reserved" &&
    coverage$reservation_id[[1L]] == reservation$reservation_id[[1L]] &&
    reservation$handler_id[[1L]] == "edr" &&
    reservation$reservation_status[[1L]] == "held_deferred_handler" &&
    reservation$max_physical_attempts[[1L]] == 1L &&
    distribution$selected[[1L]] && distribution$handler_id[[1L]] == "edr" &&
    distribution$fetch_order[[1L]] == coverage$fetch_order[[1L]]
  if (!valid_binding) {
    gx_edr_abort(
      "The selected distribution is not an admitted EDR reservation.",
      "gx_error_edr_plan_input"
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

gx_edr_time_impl <- function(distribution) {
  start <- distribution$time_start[[1L]]
  end <- distribution$time_end[[1L]]
  valid <- inherits(start, "POSIXct") && length(start) == 1L &&
    !is.na(start) && inherits(end, "POSIXct") && length(end) == 1L &&
    !is.na(end) && as.double(start) <= as.double(end)
  if (!valid) {
    gx_edr_abort(
      "The EDR distribution requires one finite planned time interval.",
      "gx_error_edr_plan_time"
    )
  }
  time_start <- unname(format(start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  time_end <- unname(format(end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  list(
    time_start = time_start,
    time_end = time_end,
    datetime = paste0(time_start, "/", time_end)
  )
}

gx_edr_number_text_impl <- function(x) {
  unname(format(x, scientific = FALSE, trim = TRUE, digits = 15L))
}

gx_edr_point_impl <- function(value) {
  value <- gx_edr_bounded_text_impl(value, "position coordinates")
  match <- regexec(
    paste0(
      "^POINT[[:space:]]*\\([[:space:]]*",
      "([-+]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+))[[:space:]]+",
      "([-+]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+))",
      "[[:space:]]*\\)$"
    ),
    value,
    perl = TRUE
  )
  captures <- regmatches(value, match)[[1L]]
  if (length(captures) != 3L) {
    gx_edr_abort(
      "M7l accepts one two-dimensional WKT POINT position.",
      "gx_error_edr_plan_url"
    )
  }
  longitude <- suppressWarnings(as.numeric(captures[[2L]]))
  latitude <- suppressWarnings(as.numeric(captures[[3L]]))
  if (!is.finite(longitude) || !is.finite(latitude) ||
      longitude < -180 || longitude > 180 || latitude < -90 || latitude > 90) {
    gx_edr_abort(
      "The EDR position is outside finite longitude/latitude bounds.",
      "gx_error_edr_plan_url"
    )
  }
  list(
    wkt = paste0(
      "POINT(", gx_edr_number_text_impl(longitude), " ",
      gx_edr_number_text_impl(latitude), ")"
    ),
    longitude = unname(longitude),
    latitude = unname(latitude)
  )
}

gx_edr_target_impl <- function(source_url, distribution) {
  if (!gx_edr_scalar_text(source_url)) {
    gx_edr_abort("The EDR source URL is invalid.", "gx_error_edr_plan_url")
  }
  canonical <- tryCatch(
    gx_safe_target(source_url, resolve_dns = FALSE)$url,
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(canonical)) {
    gx_edr_abort("The EDR source URL is unsafe.", "gx_error_edr_plan_url")
  }
  parsed <- tryCatch(httr2::url_parse(canonical), error = function(cnd) NULL)
  path_match <- if (is.null(parsed)) integer() else regexec(
    "^(.*/collections/)([A-Za-z0-9._~-]+)/position/?$",
    parsed$path %||% "",
    perl = TRUE
  )
  path_parts <- if (!length(path_match)) character() else {
    regmatches(parsed$path %||% "", path_match)[[1L]]
  }
  if (is.null(parsed) || !is.null(parsed$fragment) ||
      length(path_parts) != 3L) {
    gx_edr_abort(
      "M7l accepts only one EDR collection position endpoint.",
      "gx_error_edr_plan_url"
    )
  }
  query <- parsed$query %||% list()
  query_names <- names(query) %||% character()
  allowed <- c("coords", "parameter-name", "datetime", "crs", "f")
  if (!length(query) || any(!query_names %in% allowed) ||
      anyDuplicated(query_names) || sum(query_names == "coords") != 1L ||
      sum(query_names == "parameter-name") != 1L) {
    gx_edr_abort(
      "The inherited EDR query must contain one coords and parameter-name value and no unreviewed filters.",
      "gx_error_edr_plan_url"
    )
  }
  point <- gx_edr_point_impl(query[[match("coords", query_names)]])
  parameter <- gx_edr_bounded_text_impl(
    query[[match("parameter-name", query_names)]], "parameter name"
  )
  if (grepl("[,;&#=]", parameter, perl = TRUE)) {
    gx_edr_abort(
      "M7l accepts exactly one unambiguous EDR parameter name.",
      "gx_error_edr_plan_url"
    )
  }
  time <- gx_edr_time_impl(distribution)
  datetime_position <- match("datetime", query_names)
  if (!is.na(datetime_position) &&
      !identical(query[[datetime_position]], time$datetime)) {
    gx_edr_abort(
      "An inherited EDR datetime must exactly match the planned interval.",
      "gx_error_edr_plan_time"
    )
  }
  crs_position <- match("crs", query_names)
  if (!is.na(crs_position) && !identical(query[[crs_position]], "CRS84")) {
    gx_edr_abort(
      "M7l accepts only the reviewed CRS84 position coordinate reference system.",
      "gx_error_edr_plan_url"
    )
  }
  format_position <- match("f", query_names)
  if (!is.na(format_position) &&
      !query[[format_position]] %in% c("CoverageJSON", "json")) {
    gx_edr_abort(
      "M7l accepts only a CoverageJSON EDR response format.",
      "gx_error_edr_plan_url"
    )
  }
  collection <- gx_edr_bounded_text_impl(path_parts[[3L]], "collection")
  endpoint_path <- sub("/$", "", parsed$path)
  base_path <- sub("/collections/[A-Za-z0-9._~-]+/position$", "", endpoint_path)
  parsed$query <- list()
  parsed$fragment <- NULL
  endpoint <- httr2::url_build(parsed)
  parsed$path <- if (nzchar(base_path)) base_path else "/"
  base <- httr2::url_build(parsed)
  arguments <- list(
    .url = endpoint,
    coords = point$wkt,
    `parameter-name` = parameter,
    datetime = time$datetime,
    crs = "CRS84",
    f = "CoverageJSON"
  )
  target <- do.call(httr2::url_modify_query, arguments)
  list(
    source = canonical,
    target = gx_canonical_url(target),
    base = gx_canonical_url(base),
    collection = collection,
    point = point,
    parameter = parameter,
    time = time
  )
}

gx_edr_request_id_impl <- function(source, policy, row) {
  gx_contract_hash(
    list(
      "distribution_id", row$distribution$distribution_id[[1L]],
      "reservation_id", row$reservation$reservation_id[[1L]],
      "fetch_order", row$distribution$fetch_order[[1L]],
      "canonical_source", source$source,
      "canonical_target", source$target,
      "base_url", source$base,
      "collection", source$collection,
      "query_type", policy$query_type,
      "coords", source$point$wkt,
      "longitude", gx_edr_number_text_impl(source$point$longitude),
      "latitude", gx_edr_number_text_impl(source$point$latitude),
      "parameter", source$parameter,
      "time_start", source$time$time_start,
      "time_end", source$time$time_end,
      "datetime", source$time$datetime,
      "crs", "CRS84",
      "response_format", policy$response_format,
      "implementation_id", policy$implementation_id,
      "minimum_version", policy$minimum_version,
      "query_symbol", policy$query_symbol,
      "normalizer_symbol", policy$normalizer_symbol,
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
    namespace = "geoconnexr.edr-request.v1",
    contract_version = .gx_edr_request_plan_contract_version
  )
}

gx_edr_request_impl <- function(row, source, policy) {
  byte_limit <- unname(as.double(min(
    row$reservation$max_encoded_bytes[[1L]],
    row$reservation$max_decoded_bytes[[1L]]
  )))
  list(
    logical_request_id = gx_edr_request_id_impl(source, policy, row),
    reservation_id = unname(row$reservation$reservation_id[[1L]]),
    distribution_id = unname(row$distribution$distribution_id[[1L]]),
    fetch_order = unname(row$distribution$fetch_order[[1L]]),
    base_url_redacted = gx_redact_url(source$base),
    collection_id = source$collection,
    query_type = policy$query_type,
    coords_wkt = source$point$wkt,
    longitude = source$point$longitude,
    latitude = source$point$latitude,
    parameter_name = source$parameter,
    time_start = source$time$time_start,
    time_end = source$time$time_end,
    datetime = source$time$datetime,
    crs = "CRS84",
    response_format = policy$response_format,
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
    request_status = "edr_request_planned"
  )
}

gx_edr_plan_reasons_impl <- function(request_plan, request) {
  reasons <- unique(c(
    setdiff(
      request_plan$metadata$non_replayable_reasons,
      c(
        "arbitrary_provider_client_unimplemented",
        "non_csv_request_plans_absent"
      )
    ),
    "edr_provider_transport_unauthorized",
    "edr_result_unbound",
    "runtime_symbol_check_pending",
    "single_response_position_only"
  ))
  reasons[gx_catalog_byte_order(reasons)]
}

gx_edr_plan_metadata_impl <- function(request_plan, request) {
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
    non_replayable_reasons = gx_edr_plan_reasons_impl(request_plan, request)
  )
}

gx_edr_request_plan_new_impl <- function(
    request_plan, policy, request, metadata) {
  object <- structure(
    list(
      contract_version = .gx_edr_request_plan_contract_version,
      request_plan = request_plan,
      policy = policy,
      request = request,
      metadata = metadata
    ),
    class = "gx_edr_request_plan"
  )
  gx_edr_request_plan_validate_impl(object)
  object
}

gx_edr_request_plan_impl <- function(
    request_plan, distribution_id, max_fields = NULL) {
  policy <- gx_edr_policy_impl(max_fields)
  row <- gx_edr_source_row_impl(request_plan, distribution_id)
  source <- gx_edr_target_impl(
    row$distribution$distribution_url[[1L]], row$distribution
  )
  request <- gx_edr_request_impl(row, source, policy)
  gx_edr_request_plan_new_impl(
    request_plan = request_plan,
    policy = policy,
    request = request,
    metadata = gx_edr_plan_metadata_impl(request_plan, request)
  )
}

gx_edr_request_plan_validate_body <- function(x) {
  valid_top <- identical(class(x), "gx_edr_request_plan") &&
    identical(names(x), .gx_edr_request_plan_fields) &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(x$contract_version, .gx_edr_request_plan_contract_version)
  if (!valid_top) {
    gx_edr_abort("The EDR request plan violates its exact top-level contract.")
  }
  row <- gx_edr_source_row_impl(
    x$request_plan, x$request$distribution_id %||% NA_character_
  )
  if (!gx_edr_exact_names(x$policy, .gx_edr_policy_fields) ||
      !gx_edr_exact_names(x$request, .gx_edr_request_fields) ||
      !gx_edr_exact_names(x$metadata, .gx_edr_plan_metadata_fields)) {
    gx_edr_abort("The EDR request plan contains malformed nested facts.")
  }
  policy <- gx_edr_policy_impl(x$policy$max_fields)
  source <- gx_edr_target_impl(
    row$distribution$distribution_url[[1L]], row$distribution
  )
  request <- gx_edr_request_impl(row, source, policy)
  metadata <- gx_edr_plan_metadata_impl(x$request_plan, request)
  if (!identical(x$policy, policy) || !identical(x$request, request) ||
      !identical(x$metadata, metadata)) {
    gx_edr_abort(
      "The EDR request plan no longer rebinds to its source and reservation.",
      "gx_error_edr_plan_binding"
    )
  }
  invisible(x)
}

gx_edr_request_plan_validate_impl <- function(x) {
  tryCatch(
    gx_edr_request_plan_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_edr")) stop(cnd)
      gx_edr_abort("EDR request-plan validation rejected a malformed object.")
    },
    warning = function(cnd) {
      gx_edr_abort(
        "EDR request-plan validation rejected a warning-producing object."
      )
    }
  )
}

#' @export
print.gx_edr_request_plan <- function(x, ...) {
  gx_edr_request_plan_validate_impl(x)
  cli::cli_inform(c(
    "<gx_edr_request_plan>",
    "* Query: {x$request$query_type}; collection: {x$request$collection_id}",
    "* Parameter: {x$request$parameter_name}; format: {x$request$response_format}",
    "* Reserved attempts: 1; requests executed: 0"
  ))
  invisible(x)
}

gx_edr_json_text_impl <- function(body) {
  if (!is.raw(body) || !is.null(attributes(body)) || !length(body)) {
    gx_edr_abort(
      "The EDR response body must contain bounded raw CoverageJSON bytes.",
      "gx_error_edr_payload"
    )
  }
  text <- tryCatch(rawToChar(body), error = function(cnd) NULL)
  valid <- !is.null(text) && !startsWith(text, "\ufeff") &&
    identical(tryCatch(
      stringi::stri_enc_isutf8(text), error = function(cnd) FALSE
    ), TRUE)
  if (!valid) {
    gx_edr_abort(
      "The EDR response is not one BOM-free UTF-8 JSON document.",
      "gx_error_edr_payload"
    )
  }
  Encoding(text) <- "UTF-8"
  text
}

gx_edr_assert_unique_members_impl <- function(value) {
  stack <- list(value)
  while (length(stack)) {
    current <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    if (!is.list(current)) next
    object_names <- names(current)
    if (!is.null(object_names) &&
        (anyNA(object_names) || any(!nzchar(object_names)) ||
           anyDuplicated(object_names))) {
      gx_edr_abort(
        "The EDR CoverageJSON contains duplicate or invalid object members.",
        "gx_error_edr_payload"
      )
    }
    children <- current[vapply(current, is.list, logical(1))]
    if (length(children)) stack <- c(stack, unname(children))
  }
  invisible(value)
}

gx_edr_json_impl <- function(body, policy) {
  text <- gx_edr_json_text_impl(body)
  preflight <- tryCatch({
    gx_graph_json_preflight(
      text,
      max_depth = policy$max_json_depth,
      max_members = policy$max_json_members
    )
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!preflight) {
    gx_edr_abort(
      "The EDR CoverageJSON exceeds its structural parsing limits.",
      "gx_error_edr_payload"
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
    gx_edr_abort(
      "The EDR response is not one CoverageJSON object.",
      "gx_error_edr_payload"
    )
  }
  gx_edr_assert_unique_members_impl(document)
  document
}

gx_edr_json_array_impl <- function(x, label, allow_null = FALSE) {
  if (!is.list(x) || (!allow_null && any(vapply(x, is.null, logical(1))))) {
    gx_edr_abort(
      "The EDR CoverageJSON {label} array is malformed.",
      "gx_error_edr_payload"
    )
  }
  x
}

gx_edr_json_object_impl <- function(x, label) {
  valid <- is.list(x) && !is.null(names(x)) && length(x) > 0L &&
    !anyNA(names(x)) && all(nzchar(names(x))) && !anyDuplicated(names(x))
  if (!valid) {
    gx_edr_abort(
      "The EDR CoverageJSON {label} object is malformed.",
      "gx_error_edr_payload"
    )
  }
  x
}

gx_edr_localized_impl <- function(x, fallback, label) {
  value <- if (gx_edr_scalar_text(x)) {
    x
  } else if (is.list(x) && gx_edr_scalar_text(x$en)) {
    x$en
  } else if (is.list(x) && gx_edr_scalar_text(x$value)) {
    x$value
  } else if (is.null(x)) {
    fallback
  } else {
    NULL
  }
  if (is.null(value)) {
    gx_edr_abort(
      "The EDR CoverageJSON {label} is not one supported localized value.",
      "gx_error_edr_payload"
    )
  }
  gx_edr_bounded_text_impl(value, label, allow_empty = FALSE)
}

gx_edr_axis_values_impl <- function(axis, name) {
  gx_edr_json_object_impl(axis, paste0(name, " axis"))
  values <- gx_edr_json_array_impl(axis$values, paste0(name, " values"))
  if (!length(values)) {
    gx_edr_abort(
      "The EDR CoverageJSON {name} axis is empty.",
      "gx_error_edr_payload"
    )
  }
  values
}

gx_edr_datetime_impl <- function(value) {
  if (!gx_edr_scalar_text(value)) return(NA_real_)
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

gx_edr_schema_impl <- function(data) {
  storage <- vapply(data, function(column) {
    if (inherits(column, "POSIXct")) "POSIXct[UTC]" else typeof(column)
  }, character(1))
  tibble::tibble(
    column_index = seq_along(data),
    column_name = names(data),
    storage_type = unname(storage)
  )
}

gx_edr_position_result_impl <- function(document, request_plan) {
  request <- request_plan$request
  if (!identical(document$type, "Coverage") ||
      !gx_edr_scalar_text(document$id)) {
    gx_edr_abort(
      "M7l requires one explicitly identified CoverageJSON Coverage.",
      "gx_error_edr_payload"
    )
  }
  parameters <- gx_edr_json_object_impl(document$parameters, "parameters")
  ranges <- gx_edr_json_object_impl(document$ranges, "ranges")
  if (!identical(names(parameters), request$parameter_name) ||
      !identical(names(ranges), request$parameter_name)) {
    gx_edr_abort(
      "The EDR CoverageJSON parameter does not match the planned parameter.",
      "gx_error_edr_payload"
    )
  }
  parameter <- gx_edr_json_object_impl(
    parameters[[request$parameter_name]], "parameter metadata"
  )
  observed <- gx_edr_json_object_impl(
    parameter$observedProperty, "observed property"
  )
  label <- gx_edr_localized_impl(
    observed$label, request$parameter_name, "parameter label"
  )
  unit <- gx_edr_json_object_impl(parameter$unit, "parameter unit")
  unit_value <- if (!is.null(unit$symbol)) unit$symbol else unit$label
  unit_label <- gx_edr_localized_impl(unit_value, NA_character_, "unit")

  domain <- gx_edr_json_object_impl(document$domain, "domain")
  if (!identical(domain$type, "Domain") ||
      !identical(domain$domainType, "PointSeries")) {
    gx_edr_abort(
      "M7l accepts only an inline CoverageJSON PointSeries domain.",
      "gx_error_edr_payload"
    )
  }
  axes <- gx_edr_json_object_impl(domain$axes, "domain axes")
  if (!identical(names(axes), c("x", "y", "t"))) {
    gx_edr_abort(
      "The EDR PointSeries must declare exact x, y, and t axes.",
      "gx_error_edr_payload"
    )
  }
  x_values <- gx_edr_axis_values_impl(axes$x, "x")
  y_values <- gx_edr_axis_values_impl(axes$y, "y")
  time_values <- gx_edr_axis_values_impl(axes$t, "t")
  valid_xy <- length(x_values) == 1L && length(y_values) == 1L &&
    is.numeric(x_values[[1L]]) && length(x_values[[1L]]) == 1L &&
    is.finite(x_values[[1L]]) && is.numeric(y_values[[1L]]) &&
    length(y_values[[1L]]) == 1L && is.finite(y_values[[1L]]) &&
    identical(as.numeric(x_values[[1L]]), request$longitude) &&
    identical(as.numeric(y_values[[1L]]), request$latitude)
  if (!valid_xy) {
    gx_edr_abort(
      "The EDR PointSeries coordinates do not match the planned position.",
      "gx_error_edr_payload"
    )
  }
  time_numeric <- vapply(time_values, gx_edr_datetime_impl, numeric(1))
  if (anyNA(time_numeric)) {
    gx_edr_abort(
      "The EDR PointSeries time axis is not exact RFC 3339 UTC data.",
      "gx_error_edr_payload"
    )
  }
  datetime <- as.POSIXct(time_numeric, origin = "1970-01-01", tz = "UTC")

  range <- gx_edr_json_object_impl(
    ranges[[request$parameter_name]], "parameter range"
  )
  shape <- unlist(gx_edr_json_array_impl(range$shape, "range shape"),
                  use.names = FALSE)
  axis_names <- unlist(
    gx_edr_json_array_impl(range$axisNames, "range axis names"),
    use.names = FALSE
  )
  values <- gx_edr_json_array_impl(
    range$values, "range values", allow_null = TRUE
  )
  valid_range <- identical(range$type, "NdArray") &&
    gx_edr_scalar_text(range$dataType) &&
    range$dataType %in% c("float", "integer") &&
    identical(axis_names, "t") && length(shape) == 1L &&
    is.numeric(shape) && is.finite(shape) && shape == length(time_values) &&
    length(values) == length(time_values)
  if (!valid_range) {
    gx_edr_abort(
      "The EDR parameter range is not one bounded t-axis NdArray.",
      "gx_error_edr_payload"
    )
  }
  value <- vapply(values, function(item) {
    if (is.null(item)) return(NA_real_)
    if (!is.numeric(item) || is.logical(item) || length(item) != 1L ||
        !is.finite(item)) return(NaN)
    as.numeric(item)
  }, numeric(1))
  if (any(is.nan(value)) ||
      (identical(range$dataType, "integer") &&
         any(!is.na(value) & value != floor(value)))) {
    gx_edr_abort(
      "The EDR parameter range contains values inconsistent with its type.",
      "gx_error_edr_payload"
    )
  }
  data <- tibble::tibble(
    coverage_id = rep(unname(document$id), length(value)),
    parameter = rep(request$parameter_name, length(value)),
    parameter_label = rep(label, length(value)),
    unit = rep(unit_label, length(value)),
    datetime = datetime,
    x = rep(request$longitude, length(value)),
    y = rep(request$latitude, length(value)),
    z = rep(NA_real_, length(value)),
    value = value
  )
  field_count <- unname(as.double(nrow(data) * ncol(data)))
  if (nrow(data) < 1L || nrow(data) > request$max_rows ||
      ncol(data) > request$max_columns ||
      field_count > request_plan$policy$max_fields) {
    gx_edr_abort(
      "The normalized EDR result exceeds its planned shape limits.",
      "gx_error_edr_payload"
    )
  }
  list(
    data = data,
    coverage_id = unname(document$id),
    domain_type = "PointSeries",
    field_count = field_count
  )
}

gx_edr_external_data_impl <- function(parser, document, request_plan) {
  if (!is.function(parser)) {
    gx_edr_abort(
      "The EDR normalizer symbol is unavailable at invocation time.",
      "gx_error_edr_execution_capability"
    )
  }
  parsed <- tryCatch(
    suppressMessages(withCallingHandlers(
      parser(document, datetime_as_posix = TRUE),
      warning = function(cnd) stop(cnd)
    )),
    error = function(cnd) NULL
  )
  expected_names <- c(
    "coverage_id", "parameter", "parameter_label", "unit", "datetime",
    "x", "y", "z", "value"
  )
  if (!inherits(parsed, "tbl_df") || !identical(names(parsed), expected_names) ||
      nrow(parsed) > request_plan$request$max_rows ||
      ncol(parsed) > request_plan$request$max_columns ||
      ncol(parsed) * nrow(parsed) > request_plan$policy$max_fields) {
    gx_edr_abort(
      "The optional EDR normalizer did not return one bounded position table.",
      "gx_error_edr_payload"
    )
  }
  parsed
}

gx_edr_strict_result_impl <- function(body, request_plan) {
  document <- gx_edr_json_impl(body, request_plan$policy)
  normalized <- gx_edr_position_result_impl(document, request_plan)
  data <- normalized$data
  schema <- gx_edr_schema_impl(data)
  parse <- list(
    body_sha256 = digest::digest(body, algo = "sha256", serialize = FALSE),
    result_sha256 = gx_csv_parsed_response_result_hash_impl(data),
    row_count = nrow(data),
    column_count = ncol(data),
    field_count = normalized$field_count,
    coverage_id = normalized$coverage_id,
    domain_type = normalized$domain_type,
    parameter_name = request_plan$request$parameter_name,
    parser_validation = "external_normalizer_matched_strict_position_subset"
  )
  list(data = data, schema = schema, parse = parse, document = document)
}

gx_edr_result_impl <- function(body, request_plan, parser = NULL) {
  strict <- gx_edr_strict_result_impl(body, request_plan)
  if (!is.null(parser)) {
    external <- gx_edr_external_data_impl(
      parser, strict$document, request_plan
    )
    if (!identical(external, strict$data)) {
      gx_edr_abort(
        "The optional EDR normalizer disagreed with the strict position result.",
        "gx_error_edr_payload"
      )
    }
  }
  strict$document <- NULL
  strict
}

gx_edr_candidate_impl <- function(response, request_plan, target, parser) {
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
    gx_edr_abort(
      "The provider response violated the planned EDR response envelope.",
      "gx_error_edr_response"
    )
  }
  gx_edr_result_impl(response$body, request_plan, parser = parser)
}

gx_edr_symbol_resolver_impl <- function(
    package, query_symbol, normalizer_symbol, minimum_version) {
  namespace <- tryCatch(asNamespace(package), error = function(cnd) NULL)
  if (is.null(namespace)) return(NULL)
  package_version <- tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(cnd) NULL
  )
  query <- get0(
    query_symbol, envir = namespace, inherits = FALSE, ifnotfound = NULL
  )
  normalizer <- get0(
    normalizer_symbol, envir = namespace, inherits = FALSE, ifnotfound = NULL
  )
  sufficient <- !is.null(package_version) &&
    tryCatch(
      utils::compareVersion(package_version, minimum_version) >= 0L,
      error = function(cnd) FALSE
    )
  if (!sufficient || !is.function(query) || !is.function(normalizer)) {
    return(NULL)
  }
  list(
    package_version = unname(package_version),
    query = query,
    normalizer = normalizer
  )
}

gx_handler_edr <- function(request_plan, timeout, min_interval, capability) {
  gx_edr_request_plan_validate_impl(request_plan)
  source <- gx_edr_source_row_impl(
    request_plan$request_plan, request_plan$request$distribution_id
  )
  target <- gx_edr_target_impl(
    source$distribution$distribution_url[[1L]], source$distribution
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
    result <<- gx_edr_candidate_impl(
      response, request_plan, target, capability$normalizer
    )
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
    gx_edr_abort(
      "The EDR provider response bypassed its result validator.",
      "gx_error_edr_response"
    )
  }
  list(
    response = response,
    result = result,
    target = target,
    package_version = capability$package_version
  )
}

gx_edr_invoke_impl <- function(
    request_plan, timeout, min_interval, symbol_resolver) {
  resolver <- symbol_resolver %||% gx_edr_symbol_resolver_impl
  if (!is.function(resolver)) {
    gx_edr_abort(
      "The EDR runtime implementation resolver is invalid.",
      "gx_error_edr_execution_capability"
    )
  }
  capability <- tryCatch(
    resolver(
      request_plan$policy$implementation_package,
      request_plan$policy$query_symbol,
      request_plan$policy$normalizer_symbol,
      request_plan$policy$minimum_version
    ),
    error = function(cnd) NULL
  )
  valid <- is.list(capability) &&
    identical(names(capability), c(
      "package_version", "query", "normalizer"
    )) && gx_edr_scalar_text(capability$package_version) &&
    is.function(capability$query) && is.function(capability$normalizer) &&
    tryCatch(
      utils::compareVersion(
        capability$package_version, request_plan$policy$minimum_version
      ) >= 0L,
      error = function(cnd) FALSE
    )
  if (!valid) {
    gx_edr_abort(
      "The required EDR package version and symbols are unavailable at invocation time.",
      "gx_error_edr_execution_capability"
    )
  }
  gx_handler_edr(request_plan, timeout, min_interval, capability)
}

gx_edr_execution_id_impl <- function(
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
      "normalizer_symbol", request_plan$policy$normalizer_symbol
    ),
    namespace = "geoconnexr.edr-execution.v1",
    contract_version = .gx_edr_execution_contract_version
  )
}

gx_edr_attempt_id_impl <- function(execution_id, request_plan, target, attempt) {
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
    namespace = "geoconnexr.edr-attempt.v1",
    contract_version = .gx_edr_execution_contract_version
  )
}

gx_edr_attempts_impl <- function(
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
    gx_edr_abort(
      "The EDR handler did not return one charged physical response attempt.",
      "gx_error_edr_execution_attempt"
    )
  }
  attempt <- tibble::tibble(
    contract_version = .gx_edr_execution_contract_version,
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
  attempt$attempt_id[[1L]] <- gx_edr_attempt_id_impl(
    execution_id, request_plan, target, attempt
  )
  attempt
}

gx_edr_implementation_impl <- function(request_plan, package_version) {
  if (!gx_edr_scalar_text(package_version) ||
      !tryCatch(
        utils::compareVersion(
          package_version, request_plan$policy$minimum_version
        ) >= 0L,
        error = function(cnd) FALSE
      )) {
    gx_edr_abort(
      "The recorded EDR package version does not satisfy the request plan.",
      "gx_error_edr_execution_capability"
    )
  }
  list(
    implementation_id = request_plan$policy$implementation_id,
    package = request_plan$policy$implementation_package,
    minimum_version = request_plan$policy$minimum_version,
    package_version = package_version,
    query_symbol = request_plan$policy$query_symbol,
    normalizer_symbol = request_plan$policy$normalizer_symbol,
    resolution_status = "verified_at_invocation",
    invocation_status = "normalizer_invoked_offline_transport_package_owned"
  )
}

gx_edr_execution_facts_impl <- function(
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

gx_edr_execution_reasons_impl <- function(request_plan) {
  reasons <- unique(c(
    setdiff(
      request_plan$metadata$non_replayable_reasons,
      c(
        "edr_provider_transport_unauthorized", "edr_result_unbound",
        "runtime_symbol_check_pending", "transport_adapter_unimplemented",
        "attempt_identity_unbound", "attempt_ledger_unbound",
        "provider_transport_unauthorized", "result_schema_unbound",
        "timeout_policy_unbound"
      )
    ),
    "single_response_position_only",
    "serialization_unbound"
  ))
  reasons[gx_catalog_byte_order(reasons)]
}

gx_edr_execution_metadata_impl <- function(request_plan, result, bytes) {
  list(
    host_specific = TRUE,
    replayable = FALSE,
    execution_ready = FALSE,
    transport_authorized = TRUE,
    execution_completed = TRUE,
    provider_response_observed = TRUE,
    budgets_consumed = TRUE,
    response_validated = TRUE,
    external_normalizer_invoked = TRUE,
    external_normalizer_matched = TRUE,
    coveragejson_parsed = TRUE,
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
    non_replayable_reasons = gx_edr_execution_reasons_impl(request_plan)
  )
}

gx_edr_redacted_attempts_impl <- function(cnd) {
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

gx_edr_execution_impl <- function(
    request_plan,
    timeout = NULL,
    min_interval = NULL,
    execution_scope_id = NULL,
    symbol_resolver = NULL) {
  gx_edr_request_plan_validate_impl(request_plan)
  scope <- gx_csv_execution_scope_impl(execution_scope_id)
  timeout <- gx_csv_execution_number_impl(
    timeout, "timeout", 0, 600, allow_zero = FALSE
  )
  min_interval <- gx_csv_execution_number_impl(
    min_interval, "min_interval", 0, 3600, allow_zero = TRUE
  )
  started_at <- gx_csv_execution_time_impl(gx_now())
  execution_id <- gx_edr_execution_id_impl(
    scope, request_plan, started_at, timeout, min_interval
  )
  invoked <- tryCatch(
    gx_edr_invoke_impl(
      request_plan, timeout, min_interval, symbol_resolver
    ),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_edr_execution_capability")) stop(cnd)
      phase <- if (inherits(cnd, c(
        "gx_error_edr_payload", "gx_error_csv_parse"
      ))) "parse" else "transport"
      gx_edr_abort(
        "EDR execution failed during its bounded {phase} phase; underlying details were withheld.",
        paste0("gx_error_edr_execution_", phase),
        execution_id = execution_id,
        attempts = gx_edr_redacted_attempts_impl(cnd)
      )
    }
  )
  completed_at <- gx_csv_execution_time_impl(gx_now())
  attempts <- gx_edr_attempts_impl(
    invoked$response, request_plan, execution_id, invoked$target,
    invoked$result
  )
  bytes <- unname(as.double(length(invoked$response$body)))
  execution <- gx_edr_execution_facts_impl(
    scope, execution_id, request_plan, started_at, completed_at, timeout,
    min_interval, invoked$result, bytes
  )
  object <- structure(
    list(
      contract_version = .gx_edr_execution_contract_version,
      request_plan = request_plan,
      response_body = invoked$response$body,
      data = invoked$result$data,
      schema = invoked$result$schema,
      parse = invoked$result$parse,
      implementation = gx_edr_implementation_impl(
        request_plan, invoked$package_version
      ),
      execution = execution,
      attempts = attempts,
      metadata = gx_edr_execution_metadata_impl(
        request_plan, invoked$result, bytes
      )
    ),
    class = "gx_edr_execution"
  )
  gx_edr_execution_validate_impl(object)
  object
}

gx_edr_execution_validate_body <- function(x) {
  valid_top <- identical(class(x), "gx_edr_execution") &&
    identical(names(x), .gx_edr_execution_fields) &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(x$contract_version, .gx_edr_execution_contract_version)
  if (!valid_top) {
    gx_edr_abort("The EDR execution violates its exact top-level contract.")
  }
  gx_edr_request_plan_validate_impl(x$request_plan)
  if (!is.raw(x$response_body) || !is.null(attributes(x$response_body)) ||
      !inherits(x$data, "tbl_df") || !inherits(x$schema, "tbl_df") ||
      !gx_edr_exact_names(x$parse, .gx_edr_parse_fields) ||
      !gx_edr_exact_names(
        x$implementation, .gx_edr_implementation_fields
      ) || !gx_edr_exact_names(x$execution, .gx_edr_execution_fact_fields) ||
      !gx_edr_exact_names(x$metadata, .gx_edr_execution_metadata_fields)) {
    gx_edr_abort("The EDR execution contains malformed nested facts.")
  }
  rebuilt <- gx_edr_strict_result_impl(x$response_body, x$request_plan)
  if (!identical(x$data, rebuilt$data) ||
      !identical(x$schema, rebuilt$schema) ||
      !identical(x$parse, rebuilt$parse)) {
    gx_edr_abort(
      "The EDR result no longer rebinds to its retained response bytes.",
      "gx_error_edr_execution_result"
    )
  }
  source <- gx_edr_source_row_impl(
    x$request_plan$request_plan, x$request_plan$request$distribution_id
  )
  target <- gx_edr_target_impl(
    source$distribution$distribution_url[[1L]], source$distribution
  )$target
  attempts <- x$attempts
  valid_attempt_shape <- inherits(attempts, "tbl_df") &&
    nrow(attempts) == 1L && identical(names(attempts), .gx_edr_attempt_columns) &&
    !anyNA(attempts) && all(vapply(attempts, function(column) {
      is.null(attributes(column))
    }, logical(1)))
  if (!valid_attempt_shape) {
    gx_edr_abort("The EDR attempt ledger violates its exact shape.")
  }
  bytes <- unname(as.double(length(x$response_body)))
  expected_implementation <- gx_edr_implementation_impl(
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
    gx_edr_execution_id_impl(
      scope, x$request_plan, x$execution$started_at, timeout, interval
    )
  } else {
    NA_character_
  }
  attempt_time <- gx_csv_execution_parse_time_impl(
    attempts$completed_at[[1L]]
  )
  attempt_values <- attempts$contract_version[[1L]] ==
      .gx_edr_execution_contract_version &&
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
    gx_edr_attempt_id_impl(
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
  expected_metadata <- gx_edr_execution_metadata_impl(
    x$request_plan, rebuilt, bytes
  )
  within_budget <- bytes <= x$request_plan$request$response_byte_limit &&
    bytes <= x$request_plan$request$max_encoded_bytes &&
    bytes <= x$request_plan$request$max_decoded_bytes
  if (!identical(x$implementation, expected_implementation) ||
      !attempt_values || attempts$attempt_id[[1L]] != expected_attempt_id ||
      !execution_values || !within_budget ||
      !identical(x$metadata, expected_metadata)) {
    gx_edr_abort(
      "The EDR execution facts no longer rebind to its plan and response.",
      "gx_error_edr_execution_binding"
    )
  }
  owned_text <- gx_fetch_plan_text_total(
    x, limit = .gx_edr_max_text_bytes
  )
  if (!is.finite(owned_text) || owned_text > .gx_edr_max_text_bytes) {
    gx_edr_abort(
      "The EDR execution exceeds its aggregate text budget.",
      "gx_error_edr_execution_budget"
    )
  }
  invisible(x)
}

gx_edr_execution_validate_impl <- function(x) {
  tryCatch(
    gx_edr_execution_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_edr")) stop(cnd)
      gx_edr_abort("EDR execution validation rejected a malformed object.")
    },
    warning = function(cnd) {
      gx_edr_abort(
        "EDR execution validation rejected a warning-producing object."
      )
    }
  )
}

#' @export
print.gx_edr_execution <- function(x, ...) {
  gx_edr_execution_validate_impl(x)
  cli::cli_inform(c(
    "<gx_edr_execution>",
    "* Query: {x$request_plan$request$query_type}; collection: {x$request_plan$request$collection_id}",
    "* Rows: {x$execution$row_count}; columns: {x$execution$column_count}",
    "* Attempts: 1; external normalizer: verified and matched"
  ))
  invisible(x)
}
