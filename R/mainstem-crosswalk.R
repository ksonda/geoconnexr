gx_crosswalk_mainstem_uris <- function(mainstem_uri) {
  if (!is.character(mainstem_uri)) {
    gx_abort(
      "{.arg mainstem_uri} must be character so PID identities are not coerced.",
      c("gx_error_crosswalk_input", "gx_error_identifier")
    )
  }
  if (length(mainstem_uri) > gx_crosswalk_max_inputs()) {
    gx_abort(
      "{.arg mainstem_uri} exceeds the configured crosswalk input ceiling.",
      "gx_error_crosswalk_budget"
    )
  }
  valid <- vapply(
    mainstem_uri,
    gx_crosswalk_valid_mainstem_uri,
    logical(1),
    allow_na = FALSE
  )
  if (any(!valid)) {
    gx_abort(
      paste(
        "{.arg mainstem_uri} values must be canonical Geoconnex mainstem",
        "HTTPS PIDs with a positive ASCII-decimal identifier."
      ),
      c("gx_error_crosswalk_input", "gx_error_identifier")
    )
  }
  mainstem_uri
}

gx_empty_mainstem_comid_crosswalk <- function() {
  tibble::tibble(
    contract_version = character(),
    input_index = integer(),
    requested_mainstem_uri = character(),
    status = character(),
    match_index = integer(),
    mainstem_uri = character(),
    comid = character(),
    mapping_release = character(),
    mainstem_status = character(),
    diagnostics = list()
  )
}

gx_mainstem_comid_row_diagnostics <- function(status, input_index) {
  path <- paste0("/inputs/", input_index - 1L)
  if (identical(status, "not_found")) {
    return(gx_diagnostic(
      "warning",
      "not_found_in_mapping_release",
      path,
      "The mainstem PID is absent from the pinned mapping release."
    ))
  }
  gx_diagnostic(
    "info",
    "mainstem_currentness_not_checked",
    path,
    paste(
      "The mainstem is active in the pinned mapping release;",
      "current service state was not checked."
    )
  )
}

gx_mainstem_comid_metadata <- function(
    x,
    mainstem_uri,
    spec,
    verification = NULL) {
  statuses <- if (length(mainstem_uri)) {
    vapply(seq_along(mainstem_uri), function(index) {
      values <- unique(x$status[x$input_index == index])
      if (length(values) == 1L) values else NA_character_
    }, character(1))
  } else {
    character()
  }
  diagnostics <- if (nrow(x)) {
    do.call(gx_bind_diagnostics, c(list(gx_empty_diagnostics()), x$diagnostics))
  } else {
    gx_empty_diagnostics()
  }
  list(
    contract_version = .gx_crosswalk_contract_version,
    operation = "mainstem_to_comids",
    input_count = as.integer(length(mainstem_uri)),
    unique_input_count = as.integer(length(unique(mainstem_uri))),
    matched_input_count = as.integer(sum(statuses == "matched")),
    match_count = as.integer(sum(x$status == "matched")),
    not_found_input_count = as.integer(sum(statuses == "not_found")),
    ambiguous_input_count = 0L,
    complete = TRUE,
    retrieved_at = verification$verified_at %||% as.POSIXct(NA, tz = "UTC"),
    requests = gx_crosswalk_empty_requests(),
    diagnostics = diagnostics,
    mapping = gx_comid_mapping_metadata(spec, verification)
  )
}

gx_validate_mainstem_comid_metadata <- function(metadata, x) {
  expected <- c(
    "contract_version", "operation", "input_count", "unique_input_count",
    "matched_input_count", "match_count", "not_found_input_count",
    "ambiguous_input_count", "complete", "retrieved_at", "requests",
    "diagnostics", "mapping"
  )
  counts <- c(
    "input_count", "unique_input_count", "matched_input_count", "match_count",
    "not_found_input_count", "ambiguous_input_count"
  )
  valid <- is.list(metadata) && identical(names(metadata), expected) &&
    identical(metadata$contract_version, .gx_crosswalk_contract_version) &&
    identical(metadata$operation, "mainstem_to_comids") &&
    all(vapply(metadata[counts], function(value) {
      is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
    }, logical(1))) &&
    identical(metadata$ambiguous_input_count, 0L) &&
    is.logical(metadata$complete) && length(metadata$complete) == 1L &&
    isTRUE(metadata$complete) &&
    inherits(metadata$retrieved_at, "POSIXct") && length(metadata$retrieved_at) == 1L &&
    identical(metadata$requests, gx_crosswalk_empty_requests()) &&
    is.data.frame(metadata$diagnostics) &&
    identical(names(metadata$diagnostics), names(gx_empty_diagnostics()))
  if (!isTRUE(valid)) {
    gx_abort(
      "Mainstem inverse crosswalk metadata does not satisfy its contract.",
      "gx_error_crosswalk_contract"
    )
  }

  input_status <- if (metadata$input_count) {
    vapply(seq_len(metadata$input_count), function(index) {
      values <- unique(x$status[x$input_index == index])
      if (length(values) == 1L) values else NA_character_
    }, character(1))
  } else {
    character()
  }
  expected_diagnostics <- if (nrow(x)) {
    do.call(gx_bind_diagnostics, c(list(gx_empty_diagnostics()), x$diagnostics))
  } else {
    gx_empty_diagnostics()
  }
  reconciled <- metadata$input_count == length(unique(x$input_index)) &&
    identical(unique(x$input_index), seq_len(metadata$input_count)) &&
    metadata$unique_input_count == length(unique(x$requested_mainstem_uri)) &&
    metadata$matched_input_count == sum(input_status == "matched") &&
    metadata$not_found_input_count == sum(input_status == "not_found") &&
    metadata$match_count == sum(x$status == "matched") &&
    identical(metadata$diagnostics, expected_diagnostics)
  if (!isTRUE(reconciled)) {
    gx_abort(
      "Mainstem inverse crosswalk metadata does not reconcile with its rows.",
      "gx_error_crosswalk_contract"
    )
  }

  gx_validate_comid_mapping_metadata(metadata$mapping)
  lifecycle_ok <- if (metadata$input_count == 0L) {
    identical(metadata$mapping$cache_origin, "not_loaded") &&
      is.na(metadata$mapping$installed_at) &&
      is.na(metadata$mapping$verified_at) &&
      is.na(metadata$retrieved_at)
  } else {
    metadata$mapping$cache_origin %in% c("local_import", "release_download") &&
      !is.na(metadata$mapping$installed_at) &&
      !is.na(metadata$mapping$verified_at) &&
      !is.na(metadata$retrieved_at) &&
      identical(metadata$retrieved_at, metadata$mapping$verified_at) &&
      metadata$mapping$installed_at <= metadata$mapping$verified_at
  }
  if (!isTRUE(lifecycle_ok)) {
    gx_abort(
      "Mainstem inverse crosswalk lookup lifecycle does not reconcile with its rows.",
      "gx_error_crosswalk_contract"
    )
  }
  invisible(metadata)
}

gx_validate_mainstem_comid_crosswalk <- function(
    x,
    metadata = attr(x, "gx_crosswalk")) {
  expected <- names(gx_empty_mainstem_comid_crosswalk())
  diagnostic_names <- names(gx_empty_diagnostics())
  valid <- is.data.frame(x) && identical(names(x), expected) &&
    is.character(x$contract_version) && is.integer(x$input_index) &&
    is.character(x$requested_mainstem_uri) && is.character(x$status) &&
    is.integer(x$match_index) && is.character(x$mainstem_uri) &&
    is.character(x$comid) && is.character(x$mapping_release) &&
    is.character(x$mainstem_status) && is.list(x$diagnostics) &&
    all(vapply(x$diagnostics, function(value) {
      is.data.frame(value) && identical(names(value), diagnostic_names)
    }, logical(1))) &&
    !anyNA(x$contract_version) &&
    all(x$contract_version == .gx_crosswalk_contract_version) &&
    !anyNA(x$input_index) && all(x$input_index >= 1L) &&
    !anyNA(x$requested_mainstem_uri) &&
    all(vapply(
      x$requested_mainstem_uri,
      gx_crosswalk_valid_mainstem_uri,
      logical(1),
      allow_na = FALSE
    )) &&
    !anyNA(x$mapping_release) &&
    all(x$status %in% c("matched", "not_found"))
  if (!isTRUE(valid)) {
    gx_abort(
      "Mainstem inverse crosswalk rows do not satisfy their contract.",
      "gx_error_crosswalk_contract"
    )
  }

  if (nrow(x)) {
    missing <- x$status == "not_found"
    matched <- !missing
    missing_ok <- all(is.na(x$match_index[missing])) &&
      all(is.na(x$mainstem_uri[missing])) && all(is.na(x$comid[missing])) &&
      all(is.na(x$mainstem_status[missing]))
    matched_ok <- all(!is.na(x$match_index[matched]) & x$match_index[matched] >= 1L) &&
      all(!is.na(x$mainstem_uri[matched])) &&
      all(x$mainstem_uri[matched] == x$requested_mainstem_uri[matched]) &&
      all(!is.na(x$comid[matched])) &&
      all(grepl("^[1-9][0-9]{0,9}\\z", x$comid[matched], perl = TRUE)) &&
      all(!is.na(x$mainstem_status[matched])) &&
      all(x$mainstem_status[matched] == "active_in_mapping_release")
    diagnostics_ok <- all(vapply(seq_len(nrow(x)), function(row) {
      identical(
        x$diagnostics[[row]],
        gx_mainstem_comid_row_diagnostics(x$status[[row]], x$input_index[[row]])
      )
    }, logical(1)))
    groups_ok <- all(vapply(split(seq_len(nrow(x)), x$input_index), function(rows) {
      status <- unique(x$status[rows])
      if (length(status) != 1L ||
          length(unique(x$requested_mainstem_uri[rows])) != 1L ||
          length(unique(x$mapping_release[rows])) != 1L) {
        return(FALSE)
      }
      if (identical(status, "not_found")) return(length(rows) == 1L)
      identical(x$match_index[rows], seq_along(rows)) &&
        !anyDuplicated(x$comid[rows]) &&
        identical(order(x$comid[rows], method = "radix"), seq_along(rows))
    }, logical(1)))
    ordered_inputs <- identical(
      order(x$input_index, method = "radix"),
      seq_len(nrow(x))
    )
    if (!missing_ok || !matched_ok || !diagnostics_ok ||
        !groups_ok || !ordered_inputs) {
      gx_abort(
        "Mainstem inverse crosswalk identities or statuses do not satisfy their contract.",
        "gx_error_crosswalk_contract"
      )
    }
  }

  gx_validate_mainstem_comid_metadata(metadata, x)
  if (nrow(x) && any(x$mapping_release != metadata$mapping$release)) {
    gx_abort(
      "Mainstem inverse rows do not match their registered mapping release.",
      "gx_error_crosswalk_contract"
    )
  }
  invisible(x)
}

gx_new_mainstem_comid_crosswalk <- function(x, metadata) {
  gx_validate_mainstem_comid_crosswalk(x, metadata)
  attr(x, "gx_crosswalk") <- metadata
  class(x) <- unique(c("gx_mainstem_comid_crosswalk", "gx_crosswalk", class(x)))
  x
}

# Internal M4c substrate. It reports membership in one verified mapping release;
# it does not select or check the current mainstem collection or service state.
gx_mainstem_to_comids_impl <- function(
    mainstem_uri,
    version = "v3.2",
    data_dir = gx_default_data_dir()) {
  mainstem_uri <- gx_crosswalk_mainstem_uris(mainstem_uri)
  spec <- gx_mainstem_lookup_spec(version)
  if (!length(mainstem_uri)) {
    out <- gx_empty_mainstem_comid_crosswalk()
    metadata <- gx_mainstem_comid_metadata(out, mainstem_uri, spec)
    return(gx_new_mainstem_comid_crosswalk(out, metadata))
  }

  lookup <- gx_mainstem_lookup_require(version, data_dir)
  unique_ids <- unique(mainstem_uri)
  max_matches <- gx_crosswalk_max_matches()
  max_rows <- gx_crosswalk_max_rows()
  matches <- gx_mainstem_lookup_scan(
    lookup$verification$path,
    lookup$spec,
    targets = unique_ids,
    target_field = "uri",
    max_matches = max_matches
  )
  if (nrow(matches) > max_matches) {
    gx_abort(
      "The mainstem inverse crosswalk exceeded its aggregate match ceiling.",
      "gx_error_crosswalk_budget"
    )
  }

  found <- split(matches, factor(matches$uri, levels = unique_ids), drop = FALSE)
  frequencies <- tabulate(match(mainstem_uri, unique_ids), nbins = length(unique_ids))
  rows_per_id <- pmax(1L, vapply(found, nrow, integer(1)))
  projected_rows <- sum(as.double(frequencies) * as.double(rows_per_id))
  if (!is.finite(projected_rows) || projected_rows > max_rows) {
    gx_abort(
      "The mainstem inverse crosswalk would exceed its output-row ceiling during input expansion.",
      "gx_error_crosswalk_budget"
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
        comid = NA_character_,
        mapping_release = lookup$spec$release,
        mainstem_status = NA_character_,
        diagnostics = list(gx_mainstem_comid_row_diagnostics(
          "not_found", input_index
        ))
      )
      next
    }

    result <- result[order(result$comid, method = "radix"), , drop = FALSE]
    for (match_index in seq_len(nrow(result))) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_mainstem_uri = requested,
        status = "matched",
        match_index = as.integer(match_index),
        mainstem_uri = as.character(result$uri[[match_index]]),
        comid = as.character(result$comid[[match_index]]),
        mapping_release = lookup$spec$release,
        mainstem_status = "active_in_mapping_release",
        diagnostics = list(gx_mainstem_comid_row_diagnostics(
          "matched", input_index
        ))
      )
    }
  }

  out <- tibble::as_tibble(do.call(rbind, rows))
  metadata <- gx_mainstem_comid_metadata(
    out,
    mainstem_uri,
    lookup$spec,
    lookup$verification
  )
  gx_new_mainstem_comid_crosswalk(out, metadata)
}
