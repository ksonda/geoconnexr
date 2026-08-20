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
    replacement_uris = list(),
    mainstem_observed_at = as.POSIXct(character(), tz = "UTC"),
    mainstem_retrieval_mode = character(),
    diagnostics = list()
  )
}

gx_mainstem_comid_row_diagnostics <- function(
    status,
    input_index,
    mainstem_status = if (identical(status, "not_found")) {
      NA_character_
    } else {
      "active_in_mapping_release"
    }) {
  path <- paste0("/inputs/", input_index - 1L)
  diagnostics <- gx_empty_diagnostics()
  if (identical(status, "not_found")) {
    diagnostics <- gx_diagnostic(
      "warning",
      "not_found_in_mapping_release",
      path,
      "The mainstem PID is absent from the pinned mapping release."
    )
  }
  if (!is.na(mainstem_status)) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_crosswalk_currentness_diagnostic(
        mainstem_status,
        path,
        paste(
      "The mainstem is active in the pinned mapping release;",
      "current service state was not checked."
        )
      )
    )
  }
  diagnostics
}

gx_mainstem_comid_metadata <- function(
    x,
    mainstem_uri,
    spec,
    verification = NULL,
    requests = gx_crosswalk_empty_requests(),
    currentness_policy = "not_checked",
    currentness_collection = NA_character_,
    currentness_dataset_vintage = NA_character_) {
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
    currentness_policy = currentness_policy,
    currentness_collection = currentness_collection,
    currentness_dataset_vintage = currentness_dataset_vintage,
    input_count = as.integer(length(mainstem_uri)),
    unique_input_count = as.integer(length(unique(mainstem_uri))),
    matched_input_count = as.integer(sum(statuses == "matched")),
    match_count = as.integer(sum(x$status == "matched")),
    not_found_input_count = as.integer(sum(statuses == "not_found")),
    ambiguous_input_count = 0L,
    complete = TRUE,
    retrieved_at = gx_crosswalk_retrieved_at(
      requests,
      verification$verified_at %||% as.POSIXct(NA, tz = "UTC")
    ),
    requests = requests,
    diagnostics = diagnostics,
    mapping = gx_comid_mapping_metadata(spec, verification)
  )
}

gx_validate_mainstem_comid_metadata <- function(metadata, x) {
  expected <- c(
    "contract_version", "operation", "currentness_policy",
    "currentness_collection", "currentness_dataset_vintage",
    "input_count", "unique_input_count",
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
    metadata$currentness_policy %in% c("not_checked", "live_v3_observed") &&
    is.character(metadata$currentness_collection) &&
    length(metadata$currentness_collection) == 1L &&
    is.character(metadata$currentness_dataset_vintage) &&
    length(metadata$currentness_dataset_vintage) == 1L &&
    all(vapply(metadata[counts], function(value) {
      is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
    }, logical(1))) &&
    identical(metadata$ambiguous_input_count, 0L) &&
    is.logical(metadata$complete) && length(metadata$complete) == 1L &&
    isTRUE(metadata$complete) &&
    inherits(metadata$retrieved_at, "POSIXct") && length(metadata$retrieved_at) == 1L &&
    is.data.frame(metadata$requests) &&
    identical(names(metadata$requests), names(gx_crosswalk_empty_requests())) &&
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
  currentness_metadata_ok <- if (identical(
    metadata$currentness_policy,
    "not_checked"
  )) {
    is.na(metadata$currentness_collection) &&
      is.na(metadata$currentness_dataset_vintage) &&
      nrow(metadata$requests) == 0L
  } else {
    identical(metadata$currentness_collection, .gx_mainstem_collection) &&
      identical(
        metadata$currentness_dataset_vintage,
        .gx_mainstem_dataset_vintage
      )
  }
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
      metadata$retrieved_at >= metadata$mapping$verified_at &&
      metadata$mapping$installed_at <= metadata$mapping$verified_at
  }
  if (!isTRUE(lifecycle_ok) || !isTRUE(currentness_metadata_ok)) {
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
    is.character(x$mainstem_status) && is.list(x$replacement_uris) &&
    inherits(x$mainstem_observed_at, "POSIXct") &&
    is.character(x$mainstem_retrieval_mode) && is.list(x$diagnostics) &&
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
      if (identical(metadata$currentness_policy, "not_checked")) {
        all(is.na(x$mainstem_status[missing]))
      } else {
        all(x$mainstem_status[missing] %in% c(
          "current", "superseded", "superseded_unresolved"
        ))
      }
    allowed_mainstem_status <- if (identical(
      metadata$currentness_policy,
      "not_checked"
    )) {
      "active_in_mapping_release"
    } else {
      c("current", "superseded", "superseded_unresolved")
    }
    matched_ok <- all(!is.na(x$match_index[matched]) & x$match_index[matched] >= 1L) &&
      all(!is.na(x$mainstem_uri[matched])) &&
      all(x$mainstem_uri[matched] == x$requested_mainstem_uri[matched]) &&
      all(!is.na(x$comid[matched])) &&
      all(grepl("^[1-9][0-9]{0,9}\\z", x$comid[matched], perl = TRUE)) &&
      all(!is.na(x$mainstem_status[matched])) &&
      all(x$mainstem_status[matched] %in% allowed_mainstem_status)
    currentness_rows_ok <- all(vapply(seq_len(nrow(x)), function(row) {
      replacements <- x$replacement_uris[[row]]
      valid_replacements <- all(vapply(
        replacements,
        gx_crosswalk_valid_mainstem_uri,
        logical(1),
        allow_na = FALSE
      )) && !anyDuplicated(replacements)
      if (!valid_replacements) return(FALSE)
      if (identical(metadata$currentness_policy, "not_checked")) {
        return(length(replacements) == 0L &&
          is.na(x$mainstem_observed_at[[row]]) &&
          is.na(x$mainstem_retrieval_mode[[row]]))
      }
      state_ok <- if (identical(x$mainstem_status[[row]], "current")) {
        length(replacements) == 0L
      } else if (identical(x$mainstem_status[[row]], "superseded")) {
        length(replacements) > 0L
      } else {
        length(replacements) == 0L
      }
      state_ok && !is.na(x$mainstem_observed_at[[row]]) &&
        x$mainstem_retrieval_mode[[row]] %in% c("item", "filter")
    }, logical(1)))
    diagnostics_ok <- all(vapply(seq_len(nrow(x)), function(row) {
      identical(
        x$diagnostics[[row]],
        gx_mainstem_comid_row_diagnostics(
          x$status[[row]], x$input_index[[row]], x$mainstem_status[[row]]
        )
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
    if (!missing_ok || !matched_ok || !currentness_rows_ok || !diagnostics_ok ||
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

# Internal release-scoped inverse. It does not check live service state.
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
        replacement_uris = list(character()),
        mainstem_observed_at = as.POSIXct(NA, tz = "UTC"),
        mainstem_retrieval_mode = NA_character_,
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
        replacement_uris = list(character()),
        mainstem_observed_at = as.POSIXct(NA, tz = "UTC"),
        mainstem_retrieval_mode = NA_character_,
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

#' Map mainstem PIDs to NHDPlus COMIDs in a pinned release
#'
#' Returns every NHDPlus COMID associated with each canonical mainstem PID in
#' an explicitly installed, checksum-pinned `ref_rivers` lookup. The function
#' never downloads or refreshes lookup data. With `check = TRUE`, the requested
#' PIDs are also checked against `mainstems_v3`, including PIDs with no COMID in
#' the selected mapping release.
#'
#' @param mainstem_uri Character vector of canonical Geoconnex mainstem PIDs.
#' @param check Whether to compose release membership with bounded live
#'   `mainstems_v3` currentness.
#' @param version Registered mapping release.
#' @param data_dir Package data directory containing an explicitly installed
#'   lookup. See [gx_mainstem_lookup_install()].
#' @param currentness_client A reference client used only when `check = TRUE`,
#'   or `NULL` to construct the default.
#'
#' @return A `gx_mainstem_comid_crosswalk` tibble. Its `gx_crosswalk` attribute
#'   records mapping release, checksum provenance, counts, currentness policy,
#'   and the redacted live request ledger.
#' @export
gx_mainstem_to_comids <- function(
    mainstem_uri,
    check = FALSE,
    version = "v3.2",
    data_dir = gx_default_data_dir(),
    currentness_client = NULL) {
  check <- gx_crosswalk_check(check)
  out <- gx_mainstem_to_comids_impl(
    mainstem_uri,
    version = version,
    data_dir = data_dir
  )
  if (!check) return(out)
  if (is.null(currentness_client)) currentness_client <- gx_client("reference")
  metadata <- attr(out, "gx_crosswalk")
  composed <- gx_crosswalk_apply_currentness(
    out,
    currentness_client,
    requests = metadata$requests,
    max_requests = gx_crosswalk_max_requests(),
    max_total_bytes = gx_crosswalk_total_bytes(currentness_client),
    uri_column = "requested_mainstem_uri"
  )
  out <- composed$rows
  out$diagnostics <- lapply(seq_len(nrow(out)), function(row) {
    gx_mainstem_comid_row_diagnostics(
      out$status[[row]], out$input_index[[row]], out$mainstem_status[[row]]
    )
  })
  metadata$currentness_policy <- composed$currentness_policy
  metadata$currentness_collection <- composed$currentness_collection
  metadata$currentness_dataset_vintage <-
    composed$currentness_dataset_vintage
  metadata$retrieved_at <- gx_crosswalk_retrieved_at(
    composed$requests,
    metadata$mapping$verified_at
  )
  metadata$requests <- composed$requests
  metadata$diagnostics <- if (nrow(out)) {
    do.call(
      gx_bind_diagnostics,
      c(list(gx_empty_diagnostics()), out$diagnostics)
    )
  } else {
    gx_empty_diagnostics()
  }
  attr(out, "gx_crosswalk") <- NULL
  class(out) <- intersect(class(out), c("tbl_df", "tbl", "data.frame"))
  gx_new_mainstem_comid_crosswalk(out, metadata)
}
