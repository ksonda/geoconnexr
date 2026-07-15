.gx_crosswalk_contract_version <- "0.1.0"

gx_crosswalk_empty_requests <- function() {
  gx_ref_empty_requests()
}

gx_crosswalk_max_inputs <- function() {
  gx_scalar_number(
    getOption("geoconnexr.crosswalk_max_inputs", 100L),
    "geoconnexr.crosswalk_max_inputs",
    minimum = 1,
    maximum = 10000,
    integer = TRUE
  )
}

gx_crosswalk_max_matches <- function() {
  gx_scalar_number(
    getOption("geoconnexr.crosswalk_max_matches", 100L),
    "geoconnexr.crosswalk_max_matches",
    minimum = 1,
    maximum = 99999,
    integer = TRUE
  )
}

gx_crosswalk_max_rows <- function() {
  gx_scalar_number(
    getOption("geoconnexr.crosswalk_max_rows", 1000L),
    "geoconnexr.crosswalk_max_rows",
    minimum = 1,
    maximum = 1000000,
    integer = TRUE
  )
}

gx_crosswalk_max_requests <- function() {
  gx_scalar_number(
    getOption("geoconnexr.crosswalk_max_requests", 256L),
    "geoconnexr.crosswalk_max_requests",
    minimum = 1,
    maximum = 100000,
    integer = TRUE
  )
}

gx_crosswalk_total_bytes <- function(client) {
  default <- min(as.double(.Machine$integer.max), 8 * as.double(client$max_bytes))
  gx_scalar_number(
    getOption("geoconnexr.crosswalk_total_bytes", default),
    "geoconnexr.crosswalk_total_bytes",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
}

gx_crosswalk_provider_ids <- function(provider_id) {
  if (!is.character(provider_id)) {
    gx_abort(
      "{.arg provider_id} must be character so provider identifiers are not coerced.",
      c("gx_error_crosswalk_input", "gx_error_identifier")
    )
  }
  if (length(provider_id) > gx_crosswalk_max_inputs()) {
    gx_abort(
      "{.arg provider_id} exceeds the configured crosswalk input ceiling.",
      "gx_error_crosswalk_budget"
    )
  }
  valid_utf8 <- tryCatch(
    stringi::stri_enc_isutf8(provider_id),
    error = function(cnd) rep(FALSE, length(provider_id))
  )
  controls <- rep(TRUE, length(provider_id))
  check_controls <- !is.na(provider_id) & !is.na(valid_utf8) & valid_utf8
  controls[check_controls] <- grepl(
    "[[:cntrl:]]",
    provider_id[check_controls]
  )
  blank <- rep(TRUE, length(provider_id))
  blank[check_controls] <- !nzchar(stringi::stri_trim_both(
    provider_id[check_controls]
  ))
  invalid <- is.na(provider_id) | !nzchar(provider_id) |
    is.na(valid_utf8) | !valid_utf8 | controls | blank
  if (any(invalid)) {
    gx_abort(
      "{.arg provider_id} values must be non-missing, non-empty UTF-8 strings without control characters.",
      c("gx_error_crosswalk_input", "gx_error_identifier")
    )
  }
  provider_id
}

gx_crosswalk_gage_uri_id <- function(uri) {
  if (!is.character(uri) || length(uri) != 1L || is.na(uri)) return(NA_character_)
  matched <- regexec(
    "^https://geoconnex[.]us/ref/gages/([^/?#]+)\\z",
    uri,
    perl = TRUE
  )
  pieces <- regmatches(uri, matched)[[1]]
  if (length(pieces) != 2L) return(NA_character_)
  id <- pieces[[2]]
  valid <- tryCatch({
    gx_ref_path_segment(id, "gage id")
    TRUE
  }, error = function(cnd) FALSE)
  if (valid) id else NA_character_
}

gx_crosswalk_valid_mainstem_uri <- function(uri, allow_na = TRUE) {
  if (!is.character(uri) || length(uri) != 1L) return(FALSE)
  if (is.na(uri)) return(isTRUE(allow_na))
  valid_utf8 <- tryCatch(
    stringi::stri_enc_isutf8(uri),
    error = function(cnd) FALSE
  )
  bytes <- tryCatch(nchar(enc2utf8(uri), type = "bytes"), error = function(cnd) Inf)
  isTRUE(valid_utf8) && is.finite(bytes) && bytes <= 256L &&
    isTRUE(stringi::stri_detect_regex(
      uri,
      "^https://geoconnex[.]us/ref/mainstems/[1-9][0-9]*\\z"
    ))
}

gx_crosswalk_valid_comid <- function(comid) {
  is.na(comid) || (is.character(comid) && length(comid) == 1L &&
    grepl("^[1-9][0-9]{0,9}\\z", comid, perl = TRUE))
}

gx_crosswalk_queryable_types <- function(queryables, name) {
  row <- match(name, queryables$name)
  if (is.na(row)) character() else setdiff(queryables$json_types[[row]], "null")
}

gx_crosswalk_validate_gage_queryables <- function(queryables, requests) {
  identity <- gx_ref_identity_queryables(queryables)
  required <- c(provider_id = "string", uri = "string")
  valid_required <- vapply(names(required), function(name) {
    identical(gx_crosswalk_queryable_types(queryables, name), required[[name]])
  }, logical(1))
  if (!identical(identity, "id") || !all(valid_required)) {
    gx_abort(
      "The gages collection does not advertise the identity, provider, and URI queryables required by this crosswalk.",
      "gx_error_crosswalk_contract",
      requests = requests
    )
  }
  for (name in intersect(c("mainstem_uri", "nhdpv2_comid"), queryables$name)) {
    types <- gx_crosswalk_queryable_types(queryables, name)
    expected <- if (identical(name, "mainstem_uri")) "string" else c("integer", "number", "string")
    if (!length(types) || !all(types %in% expected)) {
      gx_abort(
        "The gages collection advertises an incompatible type for {.field {name}}.",
        "gx_error_crosswalk_contract",
        requests = requests
      )
    }
  }
  invisible(queryables)
}

gx_crosswalk_reference_requests <- function(x) {
  metadata <- attr(x, "gx_reference")
  requests <- metadata$requests
  required <- names(gx_crosswalk_empty_requests())
  if (!is.data.frame(requests) || !identical(names(requests), required)) {
    gx_abort(
      "Reference output omitted its request ledger.",
      "gx_error_crosswalk_contract"
    )
  }
  tibble::as_tibble(requests)
}

gx_crosswalk_merge_requests <- function(x, y) {
  out <- rbind(x, y)
  row.names(out) <- NULL
  tibble::as_tibble(out)
}

gx_crosswalk_condition_requests <- function(cnd) {
  requests <- cnd$requests
  required <- names(gx_crosswalk_empty_requests())
  if (!is.data.frame(requests) || !all(required %in% names(requests))) {
    return(gx_crosswalk_empty_requests())
  }
  tibble::as_tibble(requests[required])
}

gx_crosswalk_rethrow <- function(cnd, requests, budget_limited = FALSE) {
  prior <- gx_crosswalk_condition_requests(cnd)
  if (nrow(prior)) requests <- gx_crosswalk_merge_requests(requests, prior)
  budget_kind <- as.character(cnd$budget_kind %||% NA_character_)
  budget_scope <- as.character(cnd$budget_scope %||% NA_character_)
  if (identical(budget_kind, "requests") &&
      identical(budget_scope, "injected")) {
    gx_abort(
      "The gage crosswalk exhausted its aggregate request budget.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }
  if ((identical(budget_kind, "bytes") &&
      identical(budget_scope, "injected")) ||
      (budget_limited && inherits(cnd, "gx_error_payload_too_large"))) {
    gx_abort(
      "The gage crosswalk exhausted its aggregate byte budget.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }
  if (inherits(cnd, "gx_error")) {
    cnd$requests <- requests
  }
  stop(cnd)
}

gx_crosswalk_assert_budgets <- function(requests, rows = 0L,
                                        max_requests, max_rows,
                                        max_total_bytes) {
  if (nrow(requests) > max_requests) {
    gx_abort(
      "The gage crosswalk exceeded its aggregate request ceiling.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }
  bytes <- sum(as.double(requests$bytes), na.rm = TRUE)
  if (bytes > max_total_bytes) {
    gx_abort(
      "The gage crosswalk exceeded its aggregate response-byte ceiling.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }
  if (rows > max_rows) {
    gx_abort(
      "The gage crosswalk exceeded its aggregate output-row ceiling.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }
  invisible(bytes)
}

gx_crosswalk_remaining_limits <- function(requests, client,
                                          max_requests,
                                          max_total_bytes) {
  remaining_requests <- max_requests - nrow(requests)
  used_bytes <- sum(as.double(requests$bytes), na.rm = TRUE)
  remaining_bytes <- floor(max_total_bytes - used_bytes)
  if (remaining_requests < 1L) {
    gx_abort(
      "The gage crosswalk has no request budget remaining.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }
  if (!is.finite(remaining_bytes) || remaining_bytes < 1) {
    gx_abort(
      "The gage crosswalk has no response-byte budget remaining.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }
  call_client <- client
  normal_response_budget <- min(
    as.double(client$max_bytes),
    as.double(gx_ref_total_byte_limit(client))
  )
  call_client$max_bytes <- as.integer(min(
    as.double(client$max_bytes),
    remaining_bytes
  ))
  list(
    client = call_client,
    max_pages = as.integer(remaining_requests),
    max_requests = as.integer(remaining_requests),
    max_total_bytes = as.integer(remaining_bytes),
    byte_limited = remaining_bytes <= normal_response_budget
  )
}

gx_crosswalk_gage_queryables <- function(client, max_requests,
                                         max_total_bytes) {
  gx_ref_queryables_impl(
    "gages",
    client = client,
    .max_requests = max_requests,
    .max_total_bytes = max_total_bytes
  )
}

gx_crosswalk_gage_features <- function(provider_id, queryables, limit, client,
                                       max_pages, max_total_bytes,
                                       max_requests) {
  gx_ref_features_impl(
    "gages",
    query = list(provider_id = provider_id),
    limit = limit,
    client = client,
    .queryables = queryables,
    .max_pages = max_pages,
    .max_total_bytes = max_total_bytes,
    .max_requests = max_requests
  )
}

gx_crosswalk_validate_gage_matches <- function(x, requested, requests) {
  metadata <- attr(x, "gx_reference")
  if (!is.list(metadata) || !identical(metadata$complete, TRUE) ||
      !identical(metadata$truncated, FALSE)) {
    gx_abort(
      "The reference result was incomplete; no gage crosswalk was returned.",
      "gx_error_crosswalk_incomplete",
      requests = requests
    )
  }
  if (!nrow(x)) return(invisible(x))
  required <- c("feature_id", "id", "uri", "provider_id")
  if (!all(required %in% names(x))) {
    gx_abort(
      "The gage result omitted required identity properties.",
      "gx_error_crosswalk_payload",
      requests = requests
    )
  }
  if (!is.character(x$feature_id) || !is.character(x$id) ||
      !is.character(x$uri) || !is.character(x$provider_id) ||
      anyNA(x$feature_id) || anyNA(x$id) || anyNA(x$uri) ||
      anyNA(x$provider_id) || any(x$provider_id != requested) ||
      any(x$feature_id != x$id)) {
    gx_abort(
      "The gage result did not honor the requested provider or contained contradictory identities.",
      "gx_error_crosswalk_identity",
      requests = requests
    )
  }
  uri_ids <- vapply(x$uri, gx_crosswalk_gage_uri_id, character(1))
  if (anyNA(uri_ids) || any(uri_ids != x$feature_id) || anyDuplicated(x$uri)) {
    gx_abort(
      "The gage result contained an invalid, contradictory, or duplicate PID identity.",
      "gx_error_crosswalk_identity",
      requests = requests
    )
  }
  if ("mainstem_uri" %in% names(x)) {
    values <- as.character(x$mainstem_uri)
    valid <- vapply(values, gx_crosswalk_valid_mainstem_uri, logical(1))
    if (!all(valid)) {
      gx_abort(
        "The gage result contained an invalid mainstem URI.",
        "gx_error_crosswalk_payload",
        requests = requests
      )
    }
  }
  if ("nhdpv2_comid" %in% names(x)) {
    values <- as.character(x$nhdpv2_comid)
    valid <- vapply(values, gx_crosswalk_valid_comid, logical(1))
    if (!all(valid)) {
      gx_abort(
        "The gage result contained an invalid NHDPlus COMID.",
        "gx_error_crosswalk_payload",
        requests = requests
      )
    }
  }
  invisible(x)
}

gx_crosswalk_row_diagnostics <- function(status, input_index,
                                         mainstem_uri = NA_character_,
                                         comid = NA_character_) {
  path <- paste0("/inputs/", input_index - 1L)
  diagnostics <- gx_empty_diagnostics()
  if (identical(status, "not_found")) {
    return(gx_diagnostic(
      "warning", "not_found", path,
      "No reference gage matched the provider identifier."
    ))
  }
  if (identical(status, "ambiguous")) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic(
        "warning", "multiple_matches", path,
        "The provider identifier matched multiple distinct reference gages."
      )
    )
  }
  if (is.na(mainstem_uri)) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic(
        "info", "missing_mainstem_uri", path,
        "The matched gage did not advertise a mainstem URI."
      )
    )
  } else {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic(
        "info", "mainstem_vintage_unverified", path,
        "The advertised mainstem URI is retained without asserting a current vintage."
      )
    )
  }
  if (is.na(comid)) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic(
        "info", "missing_comid", path,
        "The matched gage did not advertise an NHDPlus COMID."
      )
    )
  }
  diagnostics
}

gx_empty_gage_crosswalk <- function() {
  tibble::tibble(
    contract_version = character(),
    input_index = integer(),
    requested_provider_id = character(),
    status = character(),
    match_index = integer(),
    provider_id = character(),
    gage_id = character(),
    gage_uri = character(),
    mainstem_uri = character(),
    comid = character(),
    diagnostics = list()
  )
}

gx_crosswalk_prefix_diagnostics <- function(diagnostics, prefix) {
  if (!nrow(diagnostics)) return(diagnostics)
  out <- tibble::as_tibble(diagnostics)
  suffix <- ifelse(
    is.na(out$path) | !nzchar(out$path),
    "",
    ifelse(startsWith(out$path, "/"), out$path, paste0("/", out$path))
  )
  out$path <- paste0(prefix, suffix)
  out
}

gx_crosswalk_metadata <- function(x, provider_id, requests,
                                  reference_diagnostics = gx_empty_diagnostics()) {
  statuses <- if (nrow(x)) {
    vapply(seq_along(provider_id), function(index) {
      unique(x$status[x$input_index == index])[[1]]
    }, character(1))
  } else {
    character()
  }
  row_diagnostics <- if (nrow(x)) {
    do.call(gx_bind_diagnostics, c(list(gx_empty_diagnostics()), x$diagnostics))
  } else {
    gx_empty_diagnostics()
  }
  diagnostics <- gx_bind_diagnostics(reference_diagnostics, row_diagnostics)
  retrieved_at <- if (nrow(requests)) {
    max(as.POSIXct(requests$retrieved_at, tz = "UTC"))
  } else {
    as.POSIXct(NA, tz = "UTC")
  }
  list(
    contract_version = .gx_crosswalk_contract_version,
    operation = "gage_to_pid",
    input_count = as.integer(length(provider_id)),
    unique_input_count = as.integer(length(unique(provider_id))),
    matched_input_count = as.integer(sum(statuses == "matched")),
    match_count = as.integer(sum(x$status != "not_found")),
    not_found_input_count = as.integer(sum(statuses == "not_found")),
    ambiguous_input_count = as.integer(sum(statuses == "ambiguous")),
    complete = TRUE,
    retrieved_at = retrieved_at,
    requests = requests,
    diagnostics = diagnostics
  )
}

gx_validate_crosswalk_metadata <- function(metadata, x) {
  expected <- c(
    "contract_version", "operation", "input_count", "unique_input_count",
    "matched_input_count", "match_count", "not_found_input_count",
    "ambiguous_input_count", "complete", "retrieved_at", "requests",
    "diagnostics"
  )
  counts <- expected[grepl("(_count|^input_count$|^match_count$)", expected)]
  request_names <- names(gx_crosswalk_empty_requests())
  diagnostic_names <- names(gx_empty_diagnostics())
  valid <- is.list(metadata) && identical(names(metadata), expected) &&
    identical(metadata$contract_version, .gx_crosswalk_contract_version) &&
    identical(metadata$operation, "gage_to_pid") &&
    all(vapply(metadata[counts], function(value) {
      is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
    }, logical(1))) &&
    is.logical(metadata$complete) && length(metadata$complete) == 1L &&
    !is.na(metadata$complete) && metadata$complete &&
    inherits(metadata$retrieved_at, "POSIXct") && length(metadata$retrieved_at) == 1L &&
    is.data.frame(metadata$requests) && identical(names(metadata$requests), request_names) &&
    is.data.frame(metadata$diagnostics) && identical(names(metadata$diagnostics), diagnostic_names)
  if (!valid) {
    gx_abort(
      "Gage crosswalk metadata does not satisfy its contract.",
      "gx_error_crosswalk_contract"
    )
  }
  input_status <- if (nrow(x)) {
    vapply(seq_len(metadata$input_count), function(index) {
      values <- unique(x$status[x$input_index == index])
      if (length(values) == 1L) values else NA_character_
    }, character(1))
  } else {
    character()
  }
  reconciled <- metadata$input_count == length(unique(x$input_index)) &&
    identical(unique(x$input_index), seq_len(metadata$input_count)) &&
    metadata$unique_input_count == length(unique(x$requested_provider_id)) &&
    metadata$matched_input_count == sum(input_status == "matched") &&
    metadata$not_found_input_count == sum(input_status == "not_found") &&
    metadata$ambiguous_input_count == sum(input_status == "ambiguous") &&
    metadata$match_count == sum(x$status != "not_found")
  if (!isTRUE(reconciled)) {
    gx_abort(
      "Gage crosswalk metadata does not reconcile with its rows.",
      "gx_error_crosswalk_contract"
    )
  }
  invisible(metadata)
}

gx_validate_gage_crosswalk <- function(x, metadata = attr(x, "gx_crosswalk")) {
  expected <- names(gx_empty_gage_crosswalk())
  diagnostic_names <- names(gx_empty_diagnostics())
  valid <- is.data.frame(x) && identical(names(x), expected) &&
    is.character(x$contract_version) && is.integer(x$input_index) &&
    is.character(x$requested_provider_id) && is.character(x$status) &&
    is.integer(x$match_index) && is.character(x$provider_id) &&
    is.character(x$gage_id) && is.character(x$gage_uri) &&
    is.character(x$mainstem_uri) && is.character(x$comid) &&
    is.list(x$diagnostics) &&
    all(vapply(x$diagnostics, function(value) {
      is.data.frame(value) && identical(names(value), diagnostic_names)
    }, logical(1))) &&
    !anyNA(x$contract_version) &&
    all(x$contract_version == .gx_crosswalk_contract_version) &&
    !anyNA(x$input_index) && all(x$input_index >= 1L) &&
    !anyNA(x$requested_provider_id) &&
    all(x$status %in% c("matched", "not_found", "ambiguous"))
  if (!valid) {
    gx_abort(
      "Gage crosswalk rows do not satisfy their contract.",
      "gx_error_crosswalk_contract"
    )
  }
  if (nrow(x)) {
    missing <- x$status == "not_found"
    matched <- !missing
    missing_ok <- all(is.na(x$match_index[missing])) &&
      all(is.na(x$provider_id[missing])) && all(is.na(x$gage_id[missing])) &&
      all(is.na(x$gage_uri[missing])) && all(is.na(x$mainstem_uri[missing])) &&
      all(is.na(x$comid[missing]))
    matched_ok <- all(!is.na(x$match_index[matched]) & x$match_index[matched] >= 1L) &&
      all(!is.na(x$provider_id[matched]) & x$provider_id[matched] == x$requested_provider_id[matched]) &&
      all(!is.na(x$gage_id[matched])) && all(!is.na(x$gage_uri[matched])) &&
      all(vapply(seq_len(nrow(x))[matched], function(row) {
        identical(gx_crosswalk_gage_uri_id(x$gage_uri[[row]]), x$gage_id[[row]])
      }, logical(1))) &&
      all(vapply(x$mainstem_uri, gx_crosswalk_valid_mainstem_uri, logical(1))) &&
      all(vapply(x$comid, gx_crosswalk_valid_comid, logical(1)))
    groups_ok <- all(vapply(split(seq_len(nrow(x)), x$input_index), function(rows) {
      status <- unique(x$status[rows])
      if (length(status) != 1L) return(FALSE)
      if (length(unique(x$requested_provider_id[rows])) != 1L) return(FALSE)
      if (identical(status, "not_found")) return(length(rows) == 1L)
      unique_identity <- !anyDuplicated(x$gage_uri[rows]) &&
        !anyDuplicated(x$gage_id[rows])
      if (identical(status, "matched")) {
        return(length(rows) == 1L && x$match_index[rows] == 1L && unique_identity)
      }
      deterministic <- identical(
        order(x$gage_uri[rows], x$gage_id[rows], method = "radix"),
        seq_along(rows)
      )
      identical(x$match_index[rows], seq_along(rows)) &&
        length(rows) > 1L && deterministic && unique_identity
    }, logical(1)))
    ordered_inputs <- identical(
      order(x$input_index, method = "radix"),
      seq_len(nrow(x))
    )
    if (!missing_ok || !matched_ok || !groups_ok || !ordered_inputs) {
      gx_abort(
        "Gage crosswalk identities or statuses do not satisfy their contract.",
        "gx_error_crosswalk_contract"
      )
    }
  }
  gx_validate_crosswalk_metadata(metadata, x)
  invisible(x)
}

gx_new_gage_crosswalk <- function(x, metadata) {
  gx_validate_gage_crosswalk(x, metadata)
  attr(x, "gx_crosswalk") <- metadata
  class(x) <- unique(c("gx_gage_crosswalk", "gx_crosswalk", class(x)))
  x
}

#' Map provider gage identifiers to Geoconnex reference PIDs
#'
#' Queries the native reference client using the advertised `provider_id`
#' property and verifies that every returned feature has consistent provider,
#' feature, property, and PID identities. Inputs are deduplicated for transport
#' and expanded back into their original order.
#'
#' This experimental M4 slice retains advertised `mainstem_uri` values without
#' asserting that they belong to a selected current mainstem vintage. Query-
#' bearing reference responses are intentionally non-cacheable, so offline
#' crosswalk retrieval is not promised.
#'
#' @param provider_id A character vector of opaque provider identifiers.
#'   Numeric values, missing values, blanks, invalid UTF-8, and control
#'   characters are rejected without network access.
#' @param client A reference client created by [gx_client()].
#'
#' @return A `gx_gage_crosswalk` tibble with one row for a unique match, one
#'   explicit sentinel row for no match, or every distinct match when a
#'   provider identifier is ambiguous. Identifier columns are character. The
#'   `gx_crosswalk` attribute contains aggregate counts, diagnostics, retrieval
#'   time, completeness, and the redacted reference request ledger.
#'
#' @section Budgets:
#' `geoconnexr.crosswalk_max_inputs` defaults to 100,
#' `geoconnexr.crosswalk_max_matches` to 100,
#' `geoconnexr.crosswalk_max_rows` to 1,000,
#' `geoconnexr.crosswalk_max_requests` to 256, and
#' `geoconnexr.crosswalk_total_bytes` to eight times the client response
#' ceiling. Invalid limits or incomplete reference pagination fail closed.
#'
#' @export
gx_gage_to_pid <- function(provider_id, client = gx_client("reference")) {
  gx_ref_client(client)
  provider_id <- gx_crosswalk_provider_ids(provider_id)
  if (!length(provider_id)) {
    out <- gx_empty_gage_crosswalk()
    metadata <- gx_crosswalk_metadata(out, provider_id, gx_crosswalk_empty_requests())
    return(gx_new_gage_crosswalk(out, metadata))
  }
  for (id in unique(provider_id)) {
    tryCatch(
      gx_ref_preflight_query(list(provider_id = id)),
      error = function(cnd) {
        if (inherits(cnd, "gx_error_reference_budget")) {
          gx_abort(
            "A {.arg provider_id} value exceeds the configured query budget.",
            "gx_error_crosswalk_budget"
          )
        }
        stop(cnd)
      }
    )
  }

  max_matches <- gx_crosswalk_max_matches()
  max_rows <- gx_crosswalk_max_rows()
  max_requests <- gx_crosswalk_max_requests()
  max_total_bytes <- gx_crosswalk_total_bytes(client)
  requests <- gx_crosswalk_empty_requests()
  reference_diagnostics <- gx_empty_diagnostics()
  queryables <- tryCatch(
    gx_crosswalk_gage_queryables(
      client,
      max_requests = max_requests,
      max_total_bytes = max_total_bytes
    ),
    error = function(cnd) gx_crosswalk_rethrow(
      cnd,
      requests,
      budget_limited = max_total_bytes < client$max_bytes
    )
  )
  requests <- gx_crosswalk_merge_requests(
    requests,
    gx_crosswalk_reference_requests(queryables)
  )
  queryable_metadata <- attr(queryables, "gx_reference")
  reference_diagnostics <- gx_bind_diagnostics(
    reference_diagnostics,
    gx_crosswalk_prefix_diagnostics(
      queryable_metadata$diagnostics,
      "/queryables"
    )
  )
  gx_crosswalk_assert_budgets(
    requests,
    max_requests = max_requests,
    max_rows = max_rows,
    max_total_bytes = max_total_bytes
  )
  gx_crosswalk_validate_gage_queryables(queryables, requests)

  unique_ids <- unique(provider_id)
  found <- vector("list", length(unique_ids))
  names(found) <- unique_ids
  total_matches <- 0L
  per_query_limit <- max_matches + 1L
  for (query_index in seq_along(unique_ids)) {
    id <- unique_ids[[query_index]]
    call_limits <- gx_crosswalk_remaining_limits(
      requests,
      client,
      max_requests,
      max_total_bytes
    )
    result <- tryCatch(
      gx_crosswalk_gage_features(
        id,
        queryables,
        per_query_limit,
        call_limits$client,
        call_limits$max_pages,
        call_limits$max_total_bytes,
        call_limits$max_requests
      ),
      error = function(cnd) gx_crosswalk_rethrow(
        cnd,
        requests,
        budget_limited = call_limits$byte_limited
      )
    )
    result_requests <- gx_crosswalk_reference_requests(result)
    requests <- gx_crosswalk_merge_requests(requests, result_requests)
    result_metadata <- attr(result, "gx_reference")
    reference_diagnostics <- gx_bind_diagnostics(
      reference_diagnostics,
      gx_crosswalk_prefix_diagnostics(
        result_metadata$diagnostics,
        paste0("/queries/", query_index - 1L)
      )
    )
    gx_crosswalk_assert_budgets(
      requests,
      rows = total_matches + nrow(result),
      max_requests = max_requests,
      max_rows = max_rows,
      max_total_bytes = max_total_bytes
    )
    if (isTRUE(result_metadata$truncated) &&
        identical(result_metadata$stop_reason, "page_budget") &&
        nrow(requests) >= max_requests) {
      gx_abort(
        "The gage crosswalk exhausted its aggregate request ceiling during pagination.",
        "gx_error_crosswalk_budget",
        requests = requests
      )
    }
    gx_crosswalk_validate_gage_matches(result, id, requests)
    total_matches <- total_matches + nrow(result)
    if (total_matches > max_matches) {
      gx_abort(
        "The gage crosswalk exceeded its aggregate match ceiling.",
        "gx_error_crosswalk_budget",
        requests = requests
      )
    }
    found[[id]] <- result
  }

  input_frequencies <- tabulate(
    match(provider_id, unique_ids),
    nbins = length(unique_ids)
  )
  rows_per_unique_input <- pmax(1, vapply(found, nrow, integer(1)))
  projected_rows <- sum(
    as.double(input_frequencies) * as.double(rows_per_unique_input)
  )
  if (!is.finite(projected_rows) || projected_rows > max_rows) {
    gx_abort(
      "The gage crosswalk would exceed its aggregate output-row ceiling during input expansion.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }

  rows <- list()
  for (input_index in seq_along(provider_id)) {
    requested <- provider_id[[input_index]]
    result <- found[[requested]]
    if (!nrow(result)) {
      diagnostics <- gx_crosswalk_row_diagnostics("not_found", input_index)
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_provider_id = requested,
        status = "not_found",
        match_index = NA_integer_,
        provider_id = NA_character_,
        gage_id = NA_character_,
        gage_uri = NA_character_,
        mainstem_uri = NA_character_,
        comid = NA_character_,
        diagnostics = list(diagnostics)
      )
      next
    }
    order_rows <- order(result$uri, result$feature_id, method = "radix")
    result <- result[order_rows, , drop = FALSE]
    status <- if (nrow(result) == 1L) "matched" else "ambiguous"
    for (match_index in seq_len(nrow(result))) {
      mainstem_uri <- if ("mainstem_uri" %in% names(result)) {
        as.character(result$mainstem_uri[[match_index]])
      } else {
        NA_character_
      }
      comid <- if ("nhdpv2_comid" %in% names(result)) {
        as.character(result$nhdpv2_comid[[match_index]])
      } else {
        NA_character_
      }
      diagnostics <- gx_crosswalk_row_diagnostics(
        status, input_index, mainstem_uri, comid
      )
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_provider_id = requested,
        status = status,
        match_index = as.integer(match_index),
        provider_id = as.character(result$provider_id[[match_index]]),
        gage_id = as.character(result$feature_id[[match_index]]),
        gage_uri = as.character(result$uri[[match_index]]),
        mainstem_uri = mainstem_uri,
        comid = comid,
        diagnostics = list(diagnostics)
      )
    }
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)
  gx_crosswalk_assert_budgets(
    requests,
    rows = nrow(out),
    max_requests = max_requests,
    max_rows = max_rows,
    max_total_bytes = max_total_bytes
  )
  metadata <- gx_crosswalk_metadata(
    out,
    provider_id,
    requests,
    reference_diagnostics = reference_diagnostics
  )
  gx_new_gage_crosswalk(out, metadata)
}

#' @export
print.gx_crosswalk <- function(x, ...) {
  original <- x
  metadata <- attr(x, "gx_crosswalk")
  cli::cli_inform(c(
    "<gx_crosswalk>",
    "* Operation: {metadata$operation}",
    "* Inputs: {metadata$input_count}; matches: {metadata$match_count}",
    "* Not found: {metadata$not_found_input_count}; ambiguous: {metadata$ambiguous_input_count}",
    "* Complete: {metadata$complete}; requests: {nrow(metadata$requests)}"
  ))
  class(x) <- setdiff(class(x), c("gx_gage_crosswalk", "gx_crosswalk"))
  print(x, ...)
  invisible(original)
}
