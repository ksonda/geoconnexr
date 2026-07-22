.gx_wqp_request_plan_contract_version <- "0.1.0"
.gx_wqp_execution_contract_version <- "0.1.0"
.gx_wqp_max_text_bytes <- 64L * 1024L^2

.gx_wqp_request_plan_fields <- c(
  "contract_version", "request_plan", "policy", "request", "metadata"
)

.gx_wqp_policy_fields <- c(
  "slice_id", "handler_id", "implementation_id", "implementation_package",
  "implementation_symbol", "method", "service", "profile", "accept",
  "accept_encoding", "body_bytes", "body_sha256", "credential_policy",
  "redirect_policy", "max_redirects", "retry_policy", "max_retries",
  "max_physical_attempts", "cache_policy", "success_status",
  "response_media_types", "response_content_encoding", "parser_encoding",
  "type_inference", "attribute_policy", "pagination_policy", "max_fields"
)

.gx_wqp_request_fields <- c(
  "logical_request_id", "reservation_id", "distribution_id", "fetch_order",
  "service", "profile", "site_id", "characteristic_name",
  "characteristic_status", "time_start", "time_end", "start_date",
  "end_date", "source_url_redacted", "canonical_url_redacted",
  "max_physical_attempts", "max_encoded_bytes", "max_decoded_bytes",
  "response_byte_limit", "max_rows", "max_columns", "request_status"
)

.gx_wqp_plan_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "budgets_allocated", "budgets_consumed", "counts",
  "non_replayable_reasons"
)

.gx_wqp_execution_fields <- c(
  "contract_version", "request_plan", "response_body", "data", "schema",
  "parse", "implementation", "execution", "attempts", "metadata"
)

.gx_wqp_parse_fields <- c(
  "body_sha256", "result_sha256", "row_count", "column_count",
  "field_count", "bom_present", "parser_validation",
  "characteristic_status"
)

.gx_wqp_implementation_fields <- c(
  "implementation_id", "package", "symbol", "resolution_status",
  "invocation_status"
)

.gx_wqp_execution_fact_fields <- c(
  "execution_scope_id", "execution_id", "logical_request_id",
  "reservation_id", "distribution_id", "started_at", "completed_at",
  "timeout_seconds", "min_interval_seconds", "encoded_bytes",
  "decoded_bytes", "row_count", "column_count", "execution_status"
)

.gx_wqp_attempt_columns <- c(
  "contract_version", "attempt_number", "attempt_id", "execution_id",
  "logical_request_id", "reservation_id", "method",
  "canonical_url_redacted", "resolved_host", "resolved_ip", "status",
  "outcome", "media_type", "encoded_bytes", "decoded_bytes",
  "body_sha256", "completed_at"
)

.gx_wqp_execution_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "execution_completed", "provider_response_observed", "budgets_consumed",
  "response_validated", "external_parser_invoked",
  "external_parser_matched", "strict_csv_parsed", "result_contract_bound",
  "attempt_ledger_bound", "runtime_symbol_checked", "observation_origin",
  "counts", "non_replayable_reasons"
)

gx_wqp_abort <- function(
    message,
    class = "gx_error_wqp_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_wqp", "gx_error_fetch_plan")),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_wqp_scalar_text <- function(x, nonempty = TRUE) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    (!nonempty || nzchar(x)) && is.null(attributes(x))
}

gx_wqp_exact_names <- function(x, expected) {
  is.list(x) && identical(names(x), expected) &&
    identical(names(attributes(x)), "names")
}

gx_wqp_bounded_text_impl <- function(
    x, label, allow_empty = FALSE, maximum = 2048L) {
  valid <- gx_wqp_scalar_text(x, nonempty = !allow_empty) &&
    nchar(x, type = "bytes") <= maximum &&
    identical(tryCatch(stringi::stri_enc_isutf8(x), error = function(cnd) FALSE), TRUE) &&
    identical(tryCatch(
      stringi::stri_detect_regex(x, "[\\p{Cc}\\p{Cf}\\p{Cs}]"),
      error = function(cnd) TRUE
    ), FALSE)
  if (!valid) {
    gx_wqp_abort(
      "The WQP {label} is not one bounded control-safe UTF-8 value.",
      "gx_error_wqp_plan_url"
    )
  }
  unname(enc2utf8(x))
}

gx_wqp_policy_impl <- function(max_fields) {
  max_fields <- tryCatch(
    gx_csv_parsed_response_field_limit_impl(max_fields),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(max_fields)) {
    gx_wqp_abort(
      "The WQP parser field limit must be one explicit bounded whole number.",
      "gx_error_wqp_plan_policy"
    )
  }
  list(
    slice_id = "wqp_single_response_v1",
    handler_id = "wqp",
    implementation_id = "geoconnexr:dataRetrieval-wqp",
    implementation_package = "dataRetrieval",
    implementation_symbol = "importWQP",
    method = "GET",
    service = "Result",
    profile = "narrowResult",
    accept = "text/csv",
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
    response_media_types = c("text/csv", "text/plain"),
    response_content_encoding = "identity",
    parser_encoding = "UTF-8",
    type_inference = "disabled",
    attribute_policy = "disabled",
    pagination_policy = "single_response_no_follow",
    max_fields = max_fields
  )
}

gx_wqp_source_row_impl <- function(request_plan, distribution_id) {
  valid <- tryCatch({
    gx_csv_request_plan_validate_impl(request_plan)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid || !gx_wqp_scalar_text(distribution_id) ||
      !gx_catalog_is_sha256(distribution_id)) {
    gx_wqp_abort(
      "M7k planning requires a valid M7d plan and one distribution identity.",
      "gx_error_wqp_plan_input"
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
    gx_wqp_abort(
      "The selected WQP distribution has no complete M7d reservation.",
      "gx_error_wqp_plan_input"
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
    coverage$handler_id[[1L]] == "wqp" &&
    coverage$request_status[[1L]] == "handler_reserved" &&
    coverage$reservation_id[[1L]] == reservation$reservation_id[[1L]] &&
    reservation$handler_id[[1L]] == "wqp" &&
    reservation$reservation_status[[1L]] == "held_deferred_handler" &&
    reservation$max_physical_attempts[[1L]] == 1L &&
    distribution$selected[[1L]] && distribution$handler_id[[1L]] == "wqp" &&
    distribution$fetch_order[[1L]] == coverage$fetch_order[[1L]]
  if (!valid_binding) {
    gx_wqp_abort(
      "The selected distribution is not an admitted WQP reservation.",
      "gx_error_wqp_plan_input"
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

gx_wqp_time_impl <- function(distribution) {
  start <- distribution$time_start[[1L]]
  end <- distribution$time_end[[1L]]
  valid <- inherits(start, "POSIXct") && length(start) == 1L &&
    !is.na(start) && inherits(end, "POSIXct") && length(end) == 1L &&
    !is.na(end) && as.double(start) <= as.double(end)
  if (!valid) {
    gx_wqp_abort(
      "The WQP distribution requires one finite planned time interval.",
      "gx_error_wqp_plan_time"
    )
  }
  start_clock <- format(start, "%H:%M:%S", tz = "UTC")
  end_clock <- format(end, "%H:%M:%S", tz = "UTC")
  if (start_clock != "00:00:00" || end_clock != "23:59:59") {
    gx_wqp_abort(
      "WQP date filters cannot represent a partial UTC day without overfetching.",
      "gx_error_wqp_plan_time"
    )
  }
  list(
    time_start = unname(format(start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    time_end = unname(format(end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    start_date = unname(format(start, "%m-%d-%Y", tz = "UTC")),
    end_date = unname(format(end, "%m-%d-%Y", tz = "UTC"))
  )
}

gx_wqp_target_impl <- function(source_url, distribution) {
  if (!gx_wqp_scalar_text(source_url)) {
    gx_wqp_abort("The WQP source URL is invalid.", "gx_error_wqp_plan_url")
  }
  canonical <- tryCatch(
    gx_safe_target(source_url, resolve_dns = FALSE)$url,
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(canonical)) {
    gx_wqp_abort("The WQP source URL is unsafe.", "gx_error_wqp_plan_url")
  }
  parsed <- tryCatch(httr2::url_parse(canonical), error = function(cnd) NULL)
  if (is.null(parsed) || !is.null(parsed$fragment) ||
      !parsed$hostname %in% c("waterqualitydata.us", "www.waterqualitydata.us") ||
      !parsed$path %in% c("/data/Result/search", "/wqx3/Result/search")) {
    gx_wqp_abort(
      "M7k accepts only the WQP Result search endpoint.",
      "gx_error_wqp_plan_url"
    )
  }
  query <- parsed$query %||% list()
  query_names <- names(query) %||% character()
  allowed <- c("siteid", "characteristicName", "mimeType", "dataProfile")
  if (!length(query) || any(!query_names %in% allowed) ||
      anyDuplicated(query_names) || sum(query_names == "siteid") != 1L ||
      sum(query_names == "mimeType") != 1L ||
      !identical(query[[match("mimeType", query_names)]], "csv")) {
    gx_wqp_abort(
      "The inherited WQP query must contain one siteid and mimeType=csv and no unreviewed filters.",
      "gx_error_wqp_plan_url"
    )
  }
  profile_position <- match("dataProfile", query_names)
  if (!is.na(profile_position) &&
      !query[[profile_position]] %in% c("fullPhysChem", "narrowResult")) {
    gx_wqp_abort(
      "The inherited WQP data profile is not a reviewed Result profile.",
      "gx_error_wqp_plan_url"
    )
  }
  site <- gx_wqp_bounded_text_impl(
    query[[match("siteid", query_names)]], "site identifier"
  )
  if (grepl("[;&=#]", site, perl = TRUE)) {
    gx_wqp_abort(
      "M7k accepts exactly one unambiguous WQP site identifier.",
      "gx_error_wqp_plan_url"
    )
  }
  characteristic_position <- match("characteristicName", query_names)
  characteristic <- if (is.na(characteristic_position)) "" else {
    gx_wqp_bounded_text_impl(
      query[[characteristic_position]], "characteristic", allow_empty = FALSE
    )
  }
  if (nzchar(characteristic) && grepl("[;&#]", characteristic, perl = TRUE)) {
    gx_wqp_abort(
      "M7k accepts at most one unambiguous WQP characteristic.",
      "gx_error_wqp_plan_url"
    )
  }
  time <- gx_wqp_time_impl(distribution)
  # Current Geoconnex WQP profiles still advertise the WQX3 beta route. The
  # reviewed narrowResult request contract is served by the stable /data route,
  # so retain only the allowlisted source facts and build the owned request
  # against that route.
  parsed$path <- "/data/Result/search"
  parsed$query <- list()
  parsed$fragment <- NULL
  base <- httr2::url_build(parsed)
  arguments <- list(
    .url = base,
    siteid = site,
    startDateLo = time$start_date,
    startDateHi = time$end_date,
    mimeType = "csv",
    dataProfile = "narrowResult",
    count = "no",
    sorted = "no"
  )
  if (nzchar(characteristic)) {
    arguments <- append(
      arguments,
      list(characteristicName = characteristic),
      after = 1L
    )
  }
  target <- do.call(httr2::url_modify_query, arguments)
  list(
    source = canonical,
    target = gx_canonical_url(target),
    site = site,
    characteristic = characteristic,
    characteristic_status = if (nzchar(characteristic)) {
      "source_url_filter"
    } else {
      "not_supplied"
    },
    time = time
  )
}

gx_wqp_request_id_impl <- function(source, policy, row) {
  gx_contract_hash(
    list(
      "distribution_id", row$distribution$distribution_id[[1L]],
      "reservation_id", row$reservation$reservation_id[[1L]],
      "fetch_order", row$distribution$fetch_order[[1L]],
      "canonical_source", source$source,
      "canonical_target", source$target,
      "service", policy$service,
      "profile", policy$profile,
      "site", source$site,
      "characteristic", source$characteristic,
      "time_start", source$time$time_start,
      "time_end", source$time$time_end,
      "implementation_id", policy$implementation_id,
      "implementation_symbol", policy$implementation_symbol,
      "max_fields", policy$max_fields,
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
    namespace = "geoconnexr.wqp-request.v1",
    contract_version = .gx_wqp_request_plan_contract_version
  )
}

gx_wqp_request_impl <- function(row, source, policy) {
  byte_limit <- unname(as.double(min(
    row$reservation$max_encoded_bytes[[1L]],
    row$reservation$max_decoded_bytes[[1L]]
  )))
  list(
    logical_request_id = gx_wqp_request_id_impl(source, policy, row),
    reservation_id = unname(row$reservation$reservation_id[[1L]]),
    distribution_id = unname(row$distribution$distribution_id[[1L]]),
    fetch_order = unname(row$distribution$fetch_order[[1L]]),
    service = policy$service,
    profile = policy$profile,
    site_id = source$site,
    characteristic_name = source$characteristic,
    characteristic_status = source$characteristic_status,
    time_start = source$time$time_start,
    time_end = source$time$time_end,
    start_date = source$time$start_date,
    end_date = source$time$end_date,
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
    request_status = "wqp_request_planned"
  )
}

gx_wqp_plan_reasons_impl <- function(request_plan, request) {
  reasons <- unique(c(
    setdiff(
      request_plan$metadata$non_replayable_reasons,
      c(
        "arbitrary_provider_client_unimplemented",
        "non_csv_request_plans_absent"
      )
    ),
    "wqp_provider_transport_unauthorized",
    "wqp_result_unbound",
    "runtime_symbol_check_pending",
    "single_response_only",
    if (request$characteristic_status == "not_supplied") {
      "wqp_characteristic_filter_absent"
    }
  ))
  reasons[gx_catalog_byte_order(reasons)]
}

gx_wqp_plan_metadata_impl <- function(request_plan, request) {
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
    non_replayable_reasons = gx_wqp_plan_reasons_impl(request_plan, request)
  )
}

gx_wqp_request_plan_new_impl <- function(
    request_plan, policy, request, metadata) {
  object <- structure(
    list(
      contract_version = .gx_wqp_request_plan_contract_version,
      request_plan = request_plan,
      policy = policy,
      request = request,
      metadata = metadata
    ),
    class = "gx_wqp_request_plan"
  )
  gx_wqp_request_plan_validate_impl(object)
  object
}

gx_wqp_request_plan_impl <- function(
    request_plan, distribution_id, max_fields = NULL) {
  policy <- gx_wqp_policy_impl(max_fields)
  row <- gx_wqp_source_row_impl(request_plan, distribution_id)
  source <- gx_wqp_target_impl(
    row$distribution$distribution_url[[1L]], row$distribution
  )
  request <- gx_wqp_request_impl(row, source, policy)
  gx_wqp_request_plan_new_impl(
    request_plan = request_plan,
    policy = policy,
    request = request,
    metadata = gx_wqp_plan_metadata_impl(request_plan, request)
  )
}

gx_wqp_request_plan_validate_body <- function(x) {
  valid_top <- identical(class(x), "gx_wqp_request_plan") &&
    identical(names(x), .gx_wqp_request_plan_fields) &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(x$contract_version, .gx_wqp_request_plan_contract_version)
  if (!valid_top) {
    gx_wqp_abort("The WQP request plan violates its exact top-level contract.")
  }
  row <- gx_wqp_source_row_impl(
    x$request_plan, x$request$distribution_id %||% NA_character_
  )
  if (!gx_wqp_exact_names(x$policy, .gx_wqp_policy_fields) ||
      !gx_wqp_exact_names(x$request, .gx_wqp_request_fields) ||
      !gx_wqp_exact_names(x$metadata, .gx_wqp_plan_metadata_fields)) {
    gx_wqp_abort("The WQP request plan contains malformed nested facts.")
  }
  policy <- gx_wqp_policy_impl(x$policy$max_fields)
  source <- gx_wqp_target_impl(
    row$distribution$distribution_url[[1L]], row$distribution
  )
  request <- gx_wqp_request_impl(row, source, policy)
  metadata <- gx_wqp_plan_metadata_impl(x$request_plan, request)
  if (!identical(x$policy, policy) || !identical(x$request, request) ||
      !identical(x$metadata, metadata)) {
    gx_wqp_abort(
      "The WQP request plan no longer rebinds to its source and reservation.",
      "gx_error_wqp_plan_binding"
    )
  }
  invisible(x)
}

gx_wqp_request_plan_validate_impl <- function(x) {
  tryCatch(
    gx_wqp_request_plan_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_wqp")) stop(cnd)
      gx_wqp_abort("WQP request-plan validation rejected a malformed object.")
    },
    warning = function(cnd) {
      gx_wqp_abort(
        "WQP request-plan validation rejected a warning-producing object."
      )
    }
  )
}

#' @export
print.gx_wqp_request_plan <- function(x, ...) {
  gx_wqp_request_plan_validate_impl(x)
  characteristic <- if (x$request$characteristic_status == "not_supplied") {
    "all characteristics"
  } else {
    x$request$characteristic_name
  }
  cli::cli_inform(c(
    "<gx_wqp_request_plan>",
    "* Service/profile: {x$request$service}/{x$request$profile}",
    "* Site: {x$request$site_id}; characteristic: {characteristic}",
    "* Reserved attempts: 1; requests executed: 0"
  ))
  invisible(x)
}

gx_wqp_strict_policy_impl <- function(request_plan) {
  request <- request_plan$request
  shape <- tibble::tibble(
    max_rows = request$max_rows,
    max_columns = request$max_columns
  )
  gx_csv_parsed_response_policy_impl(shape, request_plan$policy$max_fields)
}

gx_wqp_normalize_names_impl <- function(data) {
  mapped <- gsub("/", ".", names(data), fixed = TRUE)
  if (anyNA(mapped) || any(!nzchar(mapped)) || anyDuplicated(mapped)) {
    gx_wqp_abort(
      "The WQP parser produced empty or colliding normalized column names.",
      "gx_error_wqp_payload"
    )
  }
  names(data) <- unname(mapped)
  data
}

gx_wqp_external_mask_impl <- function(text) {
  sentinels <- paste0("__GEOCONNEXR_WQP_HTTPS_", 0:63, "__")
  occupied <- vapply(
    sentinels, function(value) grepl(value, text, fixed = TRUE), logical(1)
  )
  position <- which(!occupied)[1L]
  if (is.na(position)) {
    gx_wqp_abort(
      "The WQP response exhausted the bounded parser-mask namespace.",
      "gx_error_wqp_payload"
    )
  }
  sentinel <- sentinels[[position]]
  list(
    text = gsub("https://", sentinel, text, fixed = TRUE),
    sentinel = sentinel
  )
}

gx_wqp_external_data_impl <- function(parser, body, request_plan) {
  if (!is.function(parser)) {
    gx_wqp_abort(
      "The WQP parser symbol is unavailable at invocation time.",
      "gx_error_wqp_execution_capability"
    )
  }
  text <- tryCatch(rawToChar(body), error = function(cnd) NULL)
  if (is.null(text)) {
    gx_wqp_abort(
      "The WQP response could not be passed to its offline parser.",
      "gx_error_wqp_payload"
    )
  }
  Encoding(text) <- "UTF-8"
  masked <- gx_wqp_external_mask_impl(text)
  parsed <- tryCatch(
    suppressMessages(withCallingHandlers(
      do.call(parser, list(
        # importWQP() treats any character input containing an HTTPS substring
        # as a URL. Temporarily mask only that exact scheme token, parse the
        # single literal CSV document, and restore the token in every returned
        # character cell before comparing with the exact strict parse.
        obs_url = masked$text,
        tz = "UTC",
        csv = TRUE,
        convertType = FALSE
      )),
      warning = function(cnd) stop(cnd)
    )),
    error = function(cnd) NULL
  )
  if (!is.data.frame(parsed) || nrow(parsed) > request_plan$request$max_rows ||
      ncol(parsed) > request_plan$request$max_columns || ncol(parsed) < 1L ||
      ncol(parsed) * (nrow(parsed) + 1L) > request_plan$policy$max_fields ||
      any(!vapply(parsed, is.character, logical(1)))) {
    gx_wqp_abort(
      "The optional WQP parser did not return one bounded character table.",
      "gx_error_wqp_payload"
    )
  }
  columns <- lapply(parsed, function(column) {
    column[is.na(column)] <- ""
    unname(gsub(masked$sentinel, "https://", column, fixed = TRUE))
  })
  names(columns) <- names(parsed)
  gx_wqp_normalize_names_impl(
    tibble::as_tibble(columns, .name_repair = "minimal")
  )
}

gx_wqp_strict_result_impl <- function(body, request_plan) {
  policy <- gx_wqp_strict_policy_impl(request_plan)
  scan <- gx_csv_parsed_response_scan_impl(body, policy)
  data <- gx_csv_parsed_response_data_impl(body, scan, policy)
  data <- gx_wqp_normalize_names_impl(data)
  schema <- gx_csv_parsed_response_schema_impl(data)
  parse <- list(
    body_sha256 = digest::digest(body, algo = "sha256", serialize = FALSE),
    result_sha256 = gx_csv_parsed_response_result_hash_impl(data),
    row_count = scan$row_count,
    column_count = scan$column_count,
    field_count = scan$field_count,
    bom_present = unname(scan$bom_present),
    parser_validation = "external_parser_matched_strict_csv",
    characteristic_status = request_plan$request$characteristic_status
  )
  list(data = data, schema = schema, parse = parse)
}

gx_wqp_result_impl <- function(body, request_plan, parser = NULL) {
  strict <- gx_wqp_strict_result_impl(body, request_plan)
  if (!is.null(parser)) {
    external <- gx_wqp_external_data_impl(parser, body, request_plan)
    comparison <- strict$data
    comparison[] <- lapply(comparison, function(column) {
      column[column %in% c("", "NA")] <- ""
      column
    })
    if (!identical(external, comparison)) {
      gx_wqp_abort(
        "The optional WQP parser disagreed with the strict bounded CSV result.",
        "gx_error_wqp_payload"
      )
    }
  }
  strict
}

gx_wqp_candidate_impl <- function(response, request_plan, target, parser) {
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
    gx_wqp_abort(
      "The provider response violated the planned WQP response envelope.",
      "gx_error_wqp_response"
    )
  }
  gx_wqp_result_impl(response$body, request_plan, parser = parser)
}

gx_wqp_symbol_resolver_impl <- function(package, symbol) {
  namespace <- tryCatch(asNamespace(package), error = function(cnd) NULL)
  if (is.null(namespace)) return(NULL)
  get0(symbol, envir = namespace, inherits = FALSE, ifnotfound = NULL)
}

gx_handler_wqp <- function(request_plan, timeout, min_interval, parser) {
  gx_wqp_request_plan_validate_impl(request_plan)
  source <- gx_wqp_source_row_impl(
    request_plan$request_plan, request_plan$request$distribution_id
  )
  target <- gx_wqp_target_impl(
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
    result <<- gx_wqp_candidate_impl(
      response, request_plan, target, parser
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
    gx_wqp_abort(
      "The WQP provider response bypassed its result validator.",
      "gx_error_wqp_response"
    )
  }
  list(response = response, result = result, target = target)
}

gx_wqp_invoke_impl <- function(
    request_plan, timeout, min_interval, symbol_resolver) {
  resolver <- symbol_resolver %||% gx_wqp_symbol_resolver_impl
  if (!is.function(resolver)) {
    gx_wqp_abort(
      "The WQP runtime implementation resolver is invalid.",
      "gx_error_wqp_execution_capability"
    )
  }
  parser <- tryCatch(
    resolver(
      request_plan$policy$implementation_package,
      request_plan$policy$implementation_symbol
    ),
    error = function(cnd) NULL
  )
  if (!is.function(parser)) {
    gx_wqp_abort(
      "The WQP parser symbol is unavailable at invocation time.",
      "gx_error_wqp_execution_capability"
    )
  }
  gx_handler_wqp(request_plan, timeout, min_interval, parser)
}

gx_wqp_execution_id_impl <- function(
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
    namespace = "geoconnexr.wqp-execution.v1",
    contract_version = .gx_wqp_execution_contract_version
  )
}

gx_wqp_attempt_id_impl <- function(execution_id, request_plan, target, attempt) {
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
    namespace = "geoconnexr.wqp-attempt.v1",
    contract_version = .gx_wqp_execution_contract_version
  )
}

gx_wqp_attempts_impl <- function(
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
    gx_wqp_abort(
      "The WQP handler did not return one charged physical response attempt.",
      "gx_error_wqp_execution_attempt"
    )
  }
  attempt <- tibble::tibble(
    contract_version = .gx_wqp_execution_contract_version,
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
  attempt$attempt_id[[1L]] <- gx_wqp_attempt_id_impl(
    execution_id, request_plan, target, attempt
  )
  attempt
}

gx_wqp_implementation_impl <- function(request_plan) {
  list(
    implementation_id = request_plan$policy$implementation_id,
    package = request_plan$policy$implementation_package,
    symbol = request_plan$policy$implementation_symbol,
    resolution_status = "verified_at_invocation",
    invocation_status = "invoked_offline_parser"
  )
}

gx_wqp_execution_facts_impl <- function(
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

gx_wqp_execution_reasons_impl <- function(request_plan) {
  reasons <- unique(c(
    setdiff(
      request_plan$metadata$non_replayable_reasons,
      c(
        "wqp_provider_transport_unauthorized", "wqp_result_unbound",
        "runtime_symbol_check_pending", "transport_adapter_unimplemented",
        "attempt_identity_unbound", "attempt_ledger_unbound",
        "provider_transport_unauthorized", "result_schema_unbound",
        "timeout_policy_unbound"
      )
    ),
    "single_response_only",
    "serialization_unbound"
  ))
  reasons[gx_catalog_byte_order(reasons)]
}

gx_wqp_execution_metadata_impl <- function(request_plan, result, bytes) {
  list(
    host_specific = TRUE,
    replayable = FALSE,
    execution_ready = FALSE,
    transport_authorized = TRUE,
    execution_completed = TRUE,
    provider_response_observed = TRUE,
    budgets_consumed = TRUE,
    response_validated = TRUE,
    external_parser_invoked = TRUE,
    external_parser_matched = TRUE,
    strict_csv_parsed = TRUE,
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
    non_replayable_reasons = gx_wqp_execution_reasons_impl(request_plan)
  )
}

gx_wqp_redacted_attempts_impl <- function(cnd) {
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

gx_wqp_execution_impl <- function(
    request_plan,
    timeout = NULL,
    min_interval = NULL,
    execution_scope_id = NULL,
    symbol_resolver = NULL) {
  gx_wqp_request_plan_validate_impl(request_plan)
  scope <- gx_csv_execution_scope_impl(execution_scope_id)
  timeout <- gx_csv_execution_number_impl(
    timeout, "timeout", 0, 600, allow_zero = FALSE
  )
  min_interval <- gx_csv_execution_number_impl(
    min_interval, "min_interval", 0, 3600, allow_zero = TRUE
  )
  started_at <- gx_csv_execution_time_impl(gx_now())
  execution_id <- gx_wqp_execution_id_impl(
    scope, request_plan, started_at, timeout, min_interval
  )
  invoked <- tryCatch(
    gx_wqp_invoke_impl(
      request_plan, timeout, min_interval, symbol_resolver
    ),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_wqp_execution_capability")) stop(cnd)
      phase <- if (inherits(cnd, c(
        "gx_error_wqp_payload", "gx_error_csv_parse"
      ))) "parse" else "transport"
      gx_wqp_abort(
        "WQP execution failed during its bounded {phase} phase; underlying details were withheld.",
        paste0("gx_error_wqp_execution_", phase),
        execution_id = execution_id,
        attempts = gx_wqp_redacted_attempts_impl(cnd)
      )
    }
  )
  completed_at <- gx_csv_execution_time_impl(gx_now())
  attempts <- gx_wqp_attempts_impl(
    invoked$response, request_plan, execution_id, invoked$target,
    invoked$result
  )
  bytes <- unname(as.double(length(invoked$response$body)))
  execution <- gx_wqp_execution_facts_impl(
    scope, execution_id, request_plan, started_at, completed_at, timeout,
    min_interval, invoked$result, bytes
  )
  object <- structure(
    list(
      contract_version = .gx_wqp_execution_contract_version,
      request_plan = request_plan,
      response_body = invoked$response$body,
      data = invoked$result$data,
      schema = invoked$result$schema,
      parse = invoked$result$parse,
      implementation = gx_wqp_implementation_impl(request_plan),
      execution = execution,
      attempts = attempts,
      metadata = gx_wqp_execution_metadata_impl(
        request_plan, invoked$result, bytes
      )
    ),
    class = "gx_wqp_execution"
  )
  gx_wqp_execution_validate_impl(object)
  object
}

gx_wqp_execution_validate_body <- function(x) {
  valid_top <- identical(class(x), "gx_wqp_execution") &&
    identical(names(x), .gx_wqp_execution_fields) &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(x$contract_version, .gx_wqp_execution_contract_version)
  if (!valid_top) {
    gx_wqp_abort("The WQP execution violates its exact top-level contract.")
  }
  gx_wqp_request_plan_validate_impl(x$request_plan)
  if (!is.raw(x$response_body) || !is.null(attributes(x$response_body)) ||
      !inherits(x$data, "tbl_df") || !inherits(x$schema, "tbl_df") ||
      !gx_wqp_exact_names(x$parse, .gx_wqp_parse_fields) ||
      !gx_wqp_exact_names(
        x$implementation, .gx_wqp_implementation_fields
      ) || !gx_wqp_exact_names(x$execution, .gx_wqp_execution_fact_fields) ||
      !gx_wqp_exact_names(x$metadata, .gx_wqp_execution_metadata_fields)) {
    gx_wqp_abort("The WQP execution contains malformed nested facts.")
  }
  rebuilt <- gx_wqp_strict_result_impl(x$response_body, x$request_plan)
  if (!identical(x$data, rebuilt$data) ||
      !identical(x$schema, rebuilt$schema) ||
      !identical(x$parse, rebuilt$parse)) {
    gx_wqp_abort(
      "The WQP result no longer rebinds to its retained response bytes.",
      "gx_error_wqp_execution_result"
    )
  }
  source <- gx_wqp_source_row_impl(
    x$request_plan$request_plan, x$request_plan$request$distribution_id
  )
  target <- gx_wqp_target_impl(
    source$distribution$distribution_url[[1L]], source$distribution
  )$target
  attempts <- x$attempts
  valid_attempt_shape <- inherits(attempts, "tbl_df") &&
    nrow(attempts) == 1L && identical(names(attempts), .gx_wqp_attempt_columns) &&
    !anyNA(attempts) && all(vapply(attempts, function(column) {
      is.null(attributes(column))
    }, logical(1)))
  if (!valid_attempt_shape) {
    gx_wqp_abort("The WQP attempt ledger violates its exact shape.")
  }
  bytes <- unname(as.double(length(x$response_body)))
  expected_implementation <- gx_wqp_implementation_impl(x$request_plan)
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
    gx_wqp_execution_id_impl(
      scope, x$request_plan, x$execution$started_at, timeout, interval
    )
  } else {
    NA_character_
  }
  attempt_time <- gx_csv_execution_parse_time_impl(
    attempts$completed_at[[1L]]
  )
  attempt_values <- attempts$contract_version[[1L]] ==
      .gx_wqp_execution_contract_version &&
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
    gx_wqp_attempt_id_impl(
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
  expected_metadata <- gx_wqp_execution_metadata_impl(
    x$request_plan, rebuilt, bytes
  )
  within_budget <- bytes <= x$request_plan$request$response_byte_limit &&
    bytes <= x$request_plan$request$max_encoded_bytes &&
    bytes <= x$request_plan$request$max_decoded_bytes
  if (!identical(x$implementation, expected_implementation) ||
      !attempt_values || attempts$attempt_id[[1L]] != expected_attempt_id ||
      !execution_values || !within_budget ||
      !identical(x$metadata, expected_metadata)) {
    gx_wqp_abort(
      "The WQP execution facts no longer rebind to its plan and response.",
      "gx_error_wqp_execution_binding"
    )
  }
  owned_text <- gx_fetch_plan_text_total(
    x, limit = .gx_wqp_max_text_bytes
  )
  if (!is.finite(owned_text) || owned_text > .gx_wqp_max_text_bytes) {
    gx_wqp_abort(
      "The WQP execution exceeds its aggregate text budget.",
      "gx_error_wqp_execution_budget"
    )
  }
  invisible(x)
}

gx_wqp_execution_validate_impl <- function(x) {
  tryCatch(
    gx_wqp_execution_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_wqp")) stop(cnd)
      gx_wqp_abort("WQP execution validation rejected a malformed object.")
    },
    warning = function(cnd) {
      gx_wqp_abort(
        "WQP execution validation rejected a warning-producing object."
      )
    }
  )
}

#' @export
print.gx_wqp_execution <- function(x, ...) {
  gx_wqp_execution_validate_impl(x)
  cli::cli_inform(c(
    "<gx_wqp_execution>",
    "* Service/profile: {x$request_plan$request$service}/{x$request_plan$request$profile}",
    "* Rows: {x$execution$row_count}; columns: {x$execution$column_count}",
    "* Attempts: 1; external parser: verified and matched"
  ))
  invisible(x)
}
