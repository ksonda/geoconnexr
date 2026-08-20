gx_inverse_gage_validate_queryables <- function(queryables, requests) {
  identity <- gx_ref_identity_queryables(queryables)
  required <- c(
    uri = "string",
    provider_id = "string",
    mainstem_uri = "string"
  )
  valid <- identical(identity, "id") && all(vapply(
    names(required),
    function(name) {
      identical(gx_crosswalk_queryable_types(queryables, name), required[[name]])
    },
    logical(1)
  ))
  if (!valid) {
    gx_abort(
      paste(
        "The gages collection does not advertise the identity, provider, URI,",
        "and mainstem queryables required by the inverse crosswalk."
      ),
      "gx_error_crosswalk_contract",
      requests = requests
    )
  }
  if ("nhdpv2_comid" %in% queryables$name) {
    types <- gx_crosswalk_queryable_types(queryables, "nhdpv2_comid")
    if (!length(types) || !all(types %in% c("integer", "number", "string"))) {
      gx_abort(
        "The gages collection advertises an incompatible COMID type.",
        "gx_error_crosswalk_contract",
        requests = requests
      )
    }
  }
  invisible(queryables)
}

gx_inverse_gage_features <- function(mainstem_uri, queryables, limit, client,
                                     max_pages, max_total_bytes,
                                     max_requests) {
  gx_ref_features_impl(
    "gages",
    query = list(mainstem_uri = mainstem_uri),
    limit = limit,
    client = client,
    .queryables = queryables,
    .max_pages = max_pages,
    .max_total_bytes = max_total_bytes,
    .max_requests = max_requests
  )
}

gx_inverse_gage_valid_provider <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    return(FALSE)
  }
  valid_utf8 <- tryCatch(stringi::stri_enc_isutf8(value), error = function(cnd) FALSE)
  isTRUE(valid_utf8) && !grepl("[[:cntrl:]]", value) &&
    nzchar(stringi::stri_trim_both(value))
}

gx_inverse_gage_validate_matches <- function(x, requested, requests) {
  metadata <- attr(x, "gx_reference")
  if (!is.list(metadata) || !identical(metadata$complete, TRUE) ||
      !identical(metadata$truncated, FALSE)) {
    gx_abort(
      "The reference result was incomplete; no inverse gage crosswalk was returned.",
      "gx_error_crosswalk_incomplete",
      requests = requests
    )
  }
  if (!nrow(x)) return(invisible(x))
  required <- c(
    "feature_id", "id", "uri", "provider_id", "mainstem_uri"
  )
  if (!all(required %in% names(x))) {
    gx_abort(
      "The inverse gage result omitted required identity properties.",
      "gx_error_crosswalk_payload",
      requests = requests
    )
  }
  valid <- is.character(x$feature_id) && is.character(x$id) &&
    is.character(x$uri) && is.character(x$provider_id) &&
    is.character(x$mainstem_uri) && !anyNA(x$feature_id) &&
    !anyNA(x$id) && !anyNA(x$uri) && !anyNA(x$provider_id) &&
    !anyNA(x$mainstem_uri) && all(x$feature_id == x$id) &&
    all(x$mainstem_uri == requested) && all(vapply(
      x$provider_id, gx_inverse_gage_valid_provider, logical(1)
    ))
  if (!valid) {
    gx_abort(
      paste(
        "The inverse gage result did not honor the requested mainstem or",
        "contained contradictory identities."
      ),
      "gx_error_crosswalk_identity",
      requests = requests
    )
  }
  uri_ids <- vapply(x$uri, gx_crosswalk_gage_uri_id, character(1))
  if (anyNA(uri_ids) || any(uri_ids != x$feature_id) ||
      anyDuplicated(x$uri) || anyDuplicated(x$feature_id)) {
    gx_abort(
      "The inverse gage result contained invalid or duplicate gage identities.",
      "gx_error_crosswalk_identity",
      requests = requests
    )
  }
  if ("nhdpv2_comid" %in% names(x)) {
    comids <- as.character(x$nhdpv2_comid)
    if (!all(vapply(comids, gx_crosswalk_valid_comid, logical(1)))) {
      gx_abort(
        "The inverse gage result contained an invalid NHDPlus COMID.",
        "gx_error_crosswalk_payload",
        requests = requests
      )
    }
  }
  invisible(x)
}

gx_empty_inverse_gage_crosswalk <- function() {
  tibble::tibble(
    contract_version = character(),
    input_index = integer(),
    requested_mainstem_uri = character(),
    status = character(),
    match_index = integer(),
    mainstem_uri = character(),
    gage_id = character(),
    gage_uri = character(),
    provider_id = character(),
    comid = character(),
    mainstem_status = character(),
    diagnostics = list()
  )
}

gx_inverse_gage_diagnostics <- function(status, input_index,
                                        comid = NA_character_) {
  path <- paste0("/inputs/", input_index - 1L)
  if (identical(status, "not_found")) {
    return(gx_diagnostic(
      "warning",
      "not_found",
      path,
      "No reference gage advertised the requested mainstem PID."
    ))
  }
  diagnostics <- gx_diagnostic(
    "info",
    "mainstem_currentness_not_checked",
    path,
    paste(
      "The gage advertised the requested mainstem PID; its live",
      "mainstems_v3 state was not checked."
    )
  )
  if (is.na(comid)) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic(
        "info",
        "missing_comid",
        path,
        "The matched gage did not advertise an NHDPlus COMID."
      )
    )
  }
  diagnostics
}

gx_inverse_gage_metadata <- function(
    x,
    mainstem_uri,
    requests,
    reference_diagnostics = gx_empty_diagnostics()) {
  statuses <- if (length(mainstem_uri)) {
    vapply(seq_along(mainstem_uri), function(index) {
      values <- unique(x$status[x$input_index == index])
      if (length(values) == 1L) values else NA_character_
    }, character(1))
  } else {
    character()
  }
  row_diagnostics <- if (nrow(x)) {
    do.call(gx_bind_diagnostics, c(list(gx_empty_diagnostics()), x$diagnostics))
  } else {
    gx_empty_diagnostics()
  }
  list(
    contract_version = .gx_crosswalk_contract_version,
    operation = "mainstem_to_gages",
    currentness_policy = "not_checked",
    input_count = as.integer(length(mainstem_uri)),
    unique_input_count = as.integer(length(unique(mainstem_uri))),
    matched_input_count = as.integer(sum(statuses == "matched")),
    match_count = as.integer(sum(x$status == "matched")),
    not_found_input_count = as.integer(sum(statuses == "not_found")),
    complete = TRUE,
    retrieved_at = if (nrow(requests)) {
      max(as.POSIXct(requests$retrieved_at, tz = "UTC"))
    } else {
      as.POSIXct(NA, tz = "UTC")
    },
    requests = requests,
    diagnostics = gx_bind_diagnostics(reference_diagnostics, row_diagnostics)
  )
}

gx_validate_inverse_gage_metadata <- function(metadata, x) {
  expected <- c(
    "contract_version", "operation", "currentness_policy", "input_count",
    "unique_input_count", "matched_input_count", "match_count",
    "not_found_input_count", "complete", "retrieved_at", "requests",
    "diagnostics"
  )
  counts <- c(
    "input_count", "unique_input_count", "matched_input_count",
    "match_count", "not_found_input_count"
  )
  valid <- is.list(metadata) && identical(names(metadata), expected) &&
    identical(metadata$contract_version, .gx_crosswalk_contract_version) &&
    identical(metadata$operation, "mainstem_to_gages") &&
    identical(metadata$currentness_policy, "not_checked") &&
    all(vapply(metadata[counts], function(value) {
      is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
    }, logical(1))) && isTRUE(metadata$complete) &&
    inherits(metadata$retrieved_at, "POSIXct") &&
    length(metadata$retrieved_at) == 1L && is.data.frame(metadata$requests) &&
    identical(names(metadata$requests), names(gx_crosswalk_empty_requests())) &&
    is.data.frame(metadata$diagnostics) &&
    identical(names(metadata$diagnostics), names(gx_empty_diagnostics()))
  if (!valid) {
    gx_abort(
      "Inverse gage metadata does not satisfy its contract.",
      "gx_error_crosswalk_contract"
    )
  }
  statuses <- if (metadata$input_count) {
    vapply(seq_len(metadata$input_count), function(index) {
      values <- unique(x$status[x$input_index == index])
      if (length(values) == 1L) values else NA_character_
    }, character(1))
  } else {
    character()
  }
  reconciled <- metadata$input_count == length(unique(x$input_index)) &&
    identical(unique(x$input_index), seq_len(metadata$input_count)) &&
    metadata$unique_input_count == length(unique(x$requested_mainstem_uri)) &&
    metadata$matched_input_count == sum(statuses == "matched") &&
    metadata$not_found_input_count == sum(statuses == "not_found") &&
    metadata$match_count == sum(x$status == "matched")
  if (!isTRUE(reconciled)) {
    gx_abort(
      "Inverse gage metadata does not reconcile with its rows.",
      "gx_error_crosswalk_contract"
    )
  }
  invisible(metadata)
}

gx_validate_inverse_gage_crosswalk <- function(
    x,
    metadata = attr(x, "gx_crosswalk")) {
  expected <- names(gx_empty_inverse_gage_crosswalk())
  valid <- is.data.frame(x) && identical(names(x), expected) &&
    is.character(x$contract_version) &&
    all(x$contract_version == .gx_crosswalk_contract_version) &&
    is.integer(x$input_index) && !anyNA(x$input_index) &&
    all(x$input_index >= 1L) && is.character(x$requested_mainstem_uri) &&
    all(vapply(
      x$requested_mainstem_uri,
      gx_crosswalk_valid_mainstem_uri,
      logical(1),
      allow_na = FALSE
    )) && is.character(x$status) &&
    all(x$status %in% c("matched", "not_found")) &&
    is.integer(x$match_index) && is.character(x$mainstem_uri) &&
    is.character(x$gage_id) && is.character(x$gage_uri) &&
    is.character(x$provider_id) && is.character(x$comid) &&
    is.character(x$mainstem_status) && is.list(x$diagnostics) &&
    all(vapply(x$diagnostics, function(value) {
      is.data.frame(value) && identical(names(value), names(gx_empty_diagnostics()))
    }, logical(1)))
  if (!valid) {
    gx_abort(
      "Inverse gage rows do not satisfy their contract.",
      "gx_error_crosswalk_contract"
    )
  }
  if (nrow(x)) {
    missing <- x$status == "not_found"
    matched <- !missing
    missing_ok <- all(is.na(x$match_index[missing])) &&
      all(is.na(x$mainstem_uri[missing])) && all(is.na(x$gage_id[missing])) &&
      all(is.na(x$gage_uri[missing])) && all(is.na(x$provider_id[missing])) &&
      all(is.na(x$comid[missing])) && all(is.na(x$mainstem_status[missing]))
    matched_ok <- all(!is.na(x$match_index[matched])) &&
      all(x$match_index[matched] >= 1L) &&
      all(x$mainstem_uri[matched] == x$requested_mainstem_uri[matched]) &&
      all(!is.na(x$gage_id[matched])) && all(!is.na(x$gage_uri[matched])) &&
      all(!is.na(x$provider_id[matched])) &&
      all(x$mainstem_status[matched] == "currentness_not_checked") &&
      all(vapply(x$comid[matched], gx_crosswalk_valid_comid, logical(1))) &&
      all(vapply(seq_len(nrow(x))[matched], function(row) {
        identical(gx_crosswalk_gage_uri_id(x$gage_uri[[row]]), x$gage_id[[row]])
      }, logical(1)))
    groups_ok <- all(vapply(split(seq_len(nrow(x)), x$input_index), function(rows) {
      stable <- length(unique(x$status[rows])) == 1L &&
        length(unique(x$requested_mainstem_uri[rows])) == 1L
      if (!stable) return(FALSE)
      if (identical(x$status[[rows[[1L]]]], "not_found")) {
        return(length(rows) == 1L)
      }
      deterministic <- identical(
        order(x$gage_uri[rows], x$gage_id[rows], method = "radix"),
        seq_along(rows)
      )
      identical(x$match_index[rows], seq_along(rows)) && deterministic &&
        !anyDuplicated(x$gage_uri[rows]) && !anyDuplicated(x$gage_id[rows])
    }, logical(1)))
    diagnostics_ok <- all(vapply(seq_len(nrow(x)), function(row) {
      identical(
        x$diagnostics[[row]],
        gx_inverse_gage_diagnostics(
          x$status[[row]], x$input_index[[row]], x$comid[[row]]
        )
      )
    }, logical(1)))
    ordered <- identical(
      order(x$input_index, method = "radix"),
      seq_len(nrow(x))
    )
    if (!missing_ok || !matched_ok || !groups_ok || !diagnostics_ok ||
        !ordered) {
      gx_abort(
        "Inverse gage identities or statuses do not satisfy their contract.",
        "gx_error_crosswalk_contract"
      )
    }
  }
  gx_validate_inverse_gage_metadata(metadata, x)
  invisible(x)
}

gx_new_inverse_gage_crosswalk <- function(x, metadata) {
  gx_validate_inverse_gage_crosswalk(x, metadata)
  attr(x, "gx_crosswalk") <- metadata
  class(x) <- unique(c("gx_inverse_gage_crosswalk", "gx_crosswalk", class(x)))
  x
}

#' Map mainstem PIDs to reference gages
#'
#' Queries the reference service using its advertised `mainstem_uri` property,
#' validates every gage, provider, and mainstem identity, and returns all
#' matching gages. Repeated mainstem PIDs share transport and are expanded in
#' the caller's input order.
#'
#' This release does not compose the result with [gx_mainstem()]. Advertised
#' mainstem membership therefore has `currentness_policy = "not_checked"` and
#' no superseded PID is followed automatically.
#'
#' @param mainstem_uri Character vector of canonical Geoconnex mainstem PIDs.
#' @param client A reference client created by [gx_client()].
#'
#' @return A `gx_inverse_gage_crosswalk` tibble with every matching reference
#'   gage or one explicit not-found row. Its `gx_crosswalk` attribute contains
#'   aggregate counts, diagnostics, and the redacted request ledger.
#' @export
gx_mainstem_to_gages <- function(
    mainstem_uri,
    client = gx_client("reference")) {
  gx_ref_client(client)
  mainstem_uri <- gx_crosswalk_mainstem_uris(mainstem_uri)
  if (!length(mainstem_uri)) {
    out <- gx_empty_inverse_gage_crosswalk()
    metadata <- gx_inverse_gage_metadata(
      out, mainstem_uri, gx_crosswalk_empty_requests()
    )
    return(gx_new_inverse_gage_crosswalk(out, metadata))
  }
  for (uri in unique(mainstem_uri)) {
    tryCatch(
      gx_ref_preflight_query(list(mainstem_uri = uri)),
      error = function(cnd) {
        if (inherits(cnd, "gx_error_reference_budget")) {
          gx_abort(
            "A {.arg mainstem_uri} value exceeds the configured query budget.",
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
  gx_inverse_gage_validate_queryables(queryables, requests)

  unique_uri <- unique(mainstem_uri)
  found <- vector("list", length(unique_uri))
  names(found) <- unique_uri
  total_matches <- 0L
  per_query_limit <- max_matches + 1L
  for (query_index in seq_along(unique_uri)) {
    uri <- unique_uri[[query_index]]
    call_limits <- gx_crosswalk_remaining_limits(
      requests,
      client,
      max_requests,
      max_total_bytes
    )
    result <- tryCatch(
      gx_inverse_gage_features(
        uri,
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
    requests <- gx_crosswalk_merge_requests(
      requests,
      gx_crosswalk_reference_requests(result)
    )
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
        paste(
          "The inverse gage crosswalk exhausted its aggregate request ceiling",
          "during pagination."
        ),
        "gx_error_crosswalk_budget",
        requests = requests
      )
    }
    gx_inverse_gage_validate_matches(result, uri, requests)
    total_matches <- total_matches + nrow(result)
    if (total_matches > max_matches) {
      gx_abort(
        "The inverse gage crosswalk exceeded its aggregate match ceiling.",
        "gx_error_crosswalk_budget",
        requests = requests
      )
    }
    if (nrow(result)) {
      result <- result[order(result$uri, result$feature_id, method = "radix"), , drop = FALSE]
    }
    found[[uri]] <- result
  }

  frequencies <- tabulate(match(mainstem_uri, unique_uri), nbins = length(unique_uri))
  rows_per_unique <- pmax(1, vapply(found, nrow, integer(1)))
  projected_rows <- sum(as.double(frequencies) * as.double(rows_per_unique))
  if (!is.finite(projected_rows) || projected_rows > max_rows) {
    gx_abort(
      paste(
        "The inverse gage crosswalk would exceed its aggregate output-row",
        "ceiling during input expansion."
      ),
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }

  rows <- list()
  for (input_index in seq_along(mainstem_uri)) {
    requested <- mainstem_uri[[input_index]]
    result <- found[[requested]]
    if (!nrow(result)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_mainstem_uri = requested,
        status = "not_found",
        match_index = NA_integer_,
        mainstem_uri = NA_character_,
        gage_id = NA_character_,
        gage_uri = NA_character_,
        provider_id = NA_character_,
        comid = NA_character_,
        mainstem_status = NA_character_,
        diagnostics = list(gx_inverse_gage_diagnostics(
          "not_found", input_index
        ))
      )
      next
    }
    comids <- if ("nhdpv2_comid" %in% names(result)) {
      as.character(result$nhdpv2_comid)
    } else {
      rep(NA_character_, nrow(result))
    }
    for (row_index in seq_len(nrow(result))) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_mainstem_uri = requested,
        status = "matched",
        match_index = as.integer(row_index),
        mainstem_uri = as.character(result$mainstem_uri[[row_index]]),
        gage_id = as.character(result$feature_id[[row_index]]),
        gage_uri = as.character(result$uri[[row_index]]),
        provider_id = as.character(result$provider_id[[row_index]]),
        comid = comids[[row_index]],
        mainstem_status = "currentness_not_checked",
        diagnostics = list(gx_inverse_gage_diagnostics(
          "matched", input_index, comids[[row_index]]
        ))
      )
    }
  }
  out <- tibble::as_tibble(do.call(rbind, rows))
  metadata <- gx_inverse_gage_metadata(
    out,
    mainstem_uri,
    requests,
    reference_diagnostics
  )
  gx_new_inverse_gage_crosswalk(out, metadata)
}
