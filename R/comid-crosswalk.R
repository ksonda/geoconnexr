gx_crosswalk_comids <- function(comid) {
  if (!is.character(comid)) {
    gx_abort(
      "{.arg comid} must be character so NHDPlus identifiers are not coerced.",
      c("gx_error_crosswalk_input", "gx_error_identifier")
    )
  }
  if (length(comid) > gx_crosswalk_max_inputs()) {
    gx_abort(
      "{.arg comid} exceeds the configured crosswalk input ceiling.",
      "gx_error_crosswalk_budget"
    )
  }
  valid_utf8 <- tryCatch(
    stringi::stri_enc_isutf8(comid),
    error = function(cnd) rep(FALSE, length(comid))
  )
  valid <- !is.na(comid) & !is.na(valid_utf8) & valid_utf8
  valid[valid] <- stringi::stri_detect_regex(
    comid[valid],
    "^[1-9][0-9]{0,9}\\z"
  )
  if (any(!valid)) {
    gx_abort(
      "{.arg comid} values must be non-missing character identifiers containing 1 to 10 ASCII digits without a leading zero.",
      c("gx_error_crosswalk_input", "gx_error_identifier")
    )
  }
  comid
}

gx_empty_comid_crosswalk <- function() {
  tibble::tibble(
    contract_version = character(),
    input_index = integer(),
    requested_comid = character(),
    status = character(),
    match_index = integer(),
    comid = character(),
    mainstem_uri = character(),
    mapping_release = character(),
    mainstem_status = character(),
    diagnostics = list()
  )
}

gx_comid_crosswalk_row_diagnostics <- function(status, input_index) {
  path <- paste0("/inputs/", input_index - 1L)
  if (identical(status, "not_found")) {
    return(gx_diagnostic(
      "warning",
      "not_found_in_mapping_release",
      path,
      "The COMID is absent from the pinned mainstem mapping release."
    ))
  }
  diagnostics <- gx_diagnostic(
    "info",
    "mainstem_currentness_not_checked",
    path,
    "The mainstem is active in the pinned mapping release; current service state was not checked."
  )
  if (identical(status, "ambiguous")) {
    diagnostics <- gx_bind_diagnostics(
      gx_diagnostic(
        "warning",
        "multiple_mapping_matches",
        path,
        "The mapping release associated the COMID with multiple distinct mainstem URIs."
      ),
      diagnostics
    )
  }
  diagnostics
}

gx_comid_mapping_metadata <- function(spec, verification = NULL) {
  receipt <- verification$receipt %||% NULL
  list(
    lookup_id = spec$lookup_id,
    registry_version = spec$registry_version,
    release = spec$release,
    tag_commit = spec$tag_commit,
    asset_id = spec$asset_id,
    asset_name = spec$asset_name,
    source_url = spec$source_url,
    asset_bytes = spec$bytes,
    asset_sha256 = spec$sha256,
    asset_rows = spec$rows,
    license = spec$license,
    installed_at = if (is.null(receipt)) {
      as.POSIXct(NA, tz = "UTC")
    } else {
      receipt$installed_at
    },
    verified_at = verification$verified_at %||% as.POSIXct(NA, tz = "UTC"),
    cache_origin = receipt$source %||% "not_loaded",
    active_state = spec$active_state,
    currentness_policy = "not_checked"
  )
}

gx_comid_crosswalk_metadata <- function(x, comid, spec, verification = NULL) {
  statuses <- if (length(comid)) {
    vapply(seq_along(comid), function(index) {
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
    operation = "comid_to_mainstem",
    input_count = as.integer(length(comid)),
    unique_input_count = as.integer(length(unique(comid))),
    matched_input_count = as.integer(sum(statuses == "matched")),
    match_count = as.integer(sum(x$status != "not_found")),
    not_found_input_count = as.integer(sum(statuses == "not_found")),
    ambiguous_input_count = as.integer(sum(statuses == "ambiguous")),
    complete = TRUE,
    retrieved_at = verification$verified_at %||% as.POSIXct(NA, tz = "UTC"),
    requests = gx_crosswalk_empty_requests(),
    diagnostics = diagnostics,
    mapping = gx_comid_mapping_metadata(spec, verification)
  )
}

gx_validate_comid_mapping_metadata <- function(mapping) {
  expected <- c(
    "lookup_id", "registry_version", "release", "tag_commit", "asset_id",
    "asset_name", "source_url", "asset_bytes", "asset_sha256", "asset_rows",
    "license", "installed_at", "verified_at", "cache_origin", "active_state",
    "currentness_policy"
  )
  valid <- is.list(mapping) && identical(names(mapping), expected) &&
    all(vapply(mapping[c(
      "lookup_id", "release", "tag_commit", "asset_id", "asset_name",
      "source_url", "asset_sha256", "license", "cache_origin",
      "active_state", "currentness_policy"
    )], function(value) {
      is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
    }, logical(1))) &&
    is.integer(mapping$registry_version) && length(mapping$registry_version) == 1L &&
    !is.na(mapping$registry_version) && mapping$registry_version >= 1L &&
    is.integer(mapping$asset_bytes) && length(mapping$asset_bytes) == 1L &&
    !is.na(mapping$asset_bytes) &&
    mapping$asset_bytes >= 1L &&
    is.integer(mapping$asset_rows) && length(mapping$asset_rows) == 1L &&
    !is.na(mapping$asset_rows) &&
    mapping$asset_rows >= 1L &&
    inherits(mapping$installed_at, "POSIXct") && length(mapping$installed_at) == 1L &&
    inherits(mapping$verified_at, "POSIXct") && length(mapping$verified_at) == 1L &&
    grepl("^[0-9a-f]{40}$", mapping$tag_commit) &&
    grepl("^[0-9]+$", mapping$asset_id) &&
    grepl("^[0-9a-f]{64}$", mapping$asset_sha256) &&
    gx_lookup_https_url(mapping$source_url) &&
    identical(mapping$license, "CC0-1.0") &&
    mapping$cache_origin %in% c("not_loaded", "local_import", "release_download") &&
    identical(mapping$currentness_policy, "not_checked")
  if (!isTRUE(valid)) {
    gx_abort(
      "COMID crosswalk mapping provenance does not satisfy its contract.",
      "gx_error_crosswalk_contract"
    )
  }
  spec <- tryCatch(
    gx_mainstem_lookup_spec(mapping$release),
    error = function(cnd) NULL
  )
  pinned <- !is.null(spec) &&
    identical(mapping$lookup_id, spec$lookup_id) &&
    identical(mapping$registry_version, spec$registry_version) &&
    identical(mapping$release, spec$release) &&
    identical(mapping$tag_commit, spec$tag_commit) &&
    identical(mapping$asset_id, spec$asset_id) &&
    identical(mapping$asset_name, spec$asset_name) &&
    identical(mapping$source_url, spec$source_url) &&
    identical(mapping$asset_bytes, spec$bytes) &&
    identical(mapping$asset_sha256, spec$sha256) &&
    identical(mapping$asset_rows, spec$rows) &&
    identical(mapping$license, spec$license) &&
    identical(mapping$active_state, spec$active_state)
  if (!isTRUE(pinned)) {
    gx_abort(
      "COMID crosswalk mapping provenance does not match its registered release.",
      "gx_error_crosswalk_contract"
    )
  }
  invisible(mapping)
}

gx_validate_comid_crosswalk_metadata <- function(metadata, x) {
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
    identical(metadata$operation, "comid_to_mainstem") &&
    all(vapply(metadata[counts], function(value) {
      is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
    }, logical(1))) &&
    is.logical(metadata$complete) && length(metadata$complete) == 1L &&
    isTRUE(metadata$complete) &&
    inherits(metadata$retrieved_at, "POSIXct") && length(metadata$retrieved_at) == 1L &&
    is.data.frame(metadata$requests) &&
    identical(names(metadata$requests), names(gx_crosswalk_empty_requests())) &&
    is.data.frame(metadata$diagnostics) &&
    identical(names(metadata$diagnostics), names(gx_empty_diagnostics()))
  if (!isTRUE(valid)) {
    gx_abort(
      "COMID crosswalk metadata does not satisfy its contract.",
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
  reconciled <- metadata$input_count == length(unique(x$input_index)) &&
    identical(unique(x$input_index), seq_len(metadata$input_count)) &&
    metadata$unique_input_count == length(unique(x$requested_comid)) &&
    metadata$matched_input_count == sum(input_status == "matched") &&
    metadata$not_found_input_count == sum(input_status == "not_found") &&
    metadata$ambiguous_input_count == sum(input_status == "ambiguous") &&
    metadata$match_count == sum(x$status != "not_found")
  if (!isTRUE(reconciled)) {
    gx_abort(
      "COMID crosswalk metadata does not reconcile with its rows.",
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
      "COMID crosswalk lookup lifecycle does not reconcile with its rows.",
      "gx_error_crosswalk_contract"
    )
  }
  invisible(metadata)
}

gx_validate_comid_crosswalk <- function(x, metadata = attr(x, "gx_crosswalk")) {
  expected <- names(gx_empty_comid_crosswalk())
  diagnostic_names <- names(gx_empty_diagnostics())
  valid <- is.data.frame(x) && identical(names(x), expected) &&
    is.character(x$contract_version) && is.integer(x$input_index) &&
    is.character(x$requested_comid) && is.character(x$status) &&
    is.integer(x$match_index) && is.character(x$comid) &&
    is.character(x$mainstem_uri) && is.character(x$mapping_release) &&
    is.character(x$mainstem_status) && is.list(x$diagnostics) &&
    all(vapply(x$diagnostics, function(value) {
      is.data.frame(value) && identical(names(value), diagnostic_names)
    }, logical(1))) &&
    !anyNA(x$contract_version) &&
    all(x$contract_version == .gx_crosswalk_contract_version) &&
    !anyNA(x$input_index) && all(x$input_index >= 1L) &&
    !anyNA(x$requested_comid) &&
    all(grepl("^[1-9][0-9]{0,9}\\z", x$requested_comid, perl = TRUE)) &&
    !anyNA(x$mapping_release) &&
    all(x$status %in% c("matched", "not_found", "ambiguous"))
  if (!valid) {
    gx_abort(
      "COMID crosswalk rows do not satisfy their contract.",
      "gx_error_crosswalk_contract"
    )
  }
  if (nrow(x)) {
    missing <- x$status == "not_found"
    matched <- !missing
    missing_ok <- all(is.na(x$match_index[missing])) &&
      all(is.na(x$comid[missing])) && all(is.na(x$mainstem_uri[missing])) &&
      all(is.na(x$mainstem_status[missing]))
    matched_ok <- all(!is.na(x$match_index[matched]) & x$match_index[matched] >= 1L) &&
      all(!is.na(x$comid[matched]) & x$comid[matched] == x$requested_comid[matched]) &&
      all(grepl("^[1-9][0-9]{0,9}\\z", x$comid[matched], perl = TRUE)) &&
      all(!is.na(x$mainstem_uri[matched])) &&
      all(vapply(
        x$mainstem_uri[matched],
        gx_crosswalk_valid_mainstem_uri,
        logical(1),
        allow_na = FALSE
      )) &&
      all(x$mainstem_status[matched] == "active_in_mapping_release")
    groups_ok <- all(vapply(split(seq_len(nrow(x)), x$input_index), function(rows) {
      status <- unique(x$status[rows])
      if (length(status) != 1L ||
          length(unique(x$requested_comid[rows])) != 1L ||
          length(unique(x$mapping_release[rows])) != 1L) return(FALSE)
      if (identical(status, "not_found")) return(length(rows) == 1L)
      unique_identity <- !anyDuplicated(x$mainstem_uri[rows])
      if (identical(status, "matched")) {
        return(length(rows) == 1L && x$match_index[rows] == 1L && unique_identity)
      }
      identical(x$match_index[rows], seq_along(rows)) && length(rows) > 1L &&
        identical(order(x$mainstem_uri[rows], method = "radix"), seq_along(rows)) &&
        unique_identity
    }, logical(1)))
    ordered_inputs <- identical(
      order(x$input_index, method = "radix"),
      seq_len(nrow(x))
    )
    if (!missing_ok || !matched_ok || !groups_ok || !ordered_inputs) {
      gx_abort(
        "COMID crosswalk identities or statuses do not satisfy their contract.",
        "gx_error_crosswalk_contract"
      )
    }
  }
  gx_validate_comid_crosswalk_metadata(metadata, x)
  if (nrow(x) && any(x$mapping_release != metadata$mapping$release)) {
    gx_abort(
      "COMID crosswalk rows do not match their registered mapping release.",
      "gx_error_crosswalk_contract"
    )
  }
  invisible(x)
}

gx_new_comid_crosswalk <- function(x, metadata) {
  gx_validate_comid_crosswalk(x, metadata)
  attr(x, "gx_crosswalk") <- metadata
  class(x) <- unique(c("gx_comid_crosswalk", "gx_crosswalk", class(x)))
  x
}

# Internal M4b substrate. The public gx_comid_to_mainstem() wrapper remains
# gated by ADRs 0004 and 0008 until current/superseded semantics are selected.
gx_comid_to_mainstem_impl <- function(
    comid,
    version = "v3.2",
    data_dir = gx_default_data_dir()) {
  comid <- gx_crosswalk_comids(comid)
  spec <- gx_mainstem_lookup_spec(version)
  if (!length(comid)) {
    out <- gx_empty_comid_crosswalk()
    metadata <- gx_comid_crosswalk_metadata(out, comid, spec)
    return(gx_new_comid_crosswalk(out, metadata))
  }
  lookup <- gx_mainstem_lookup_require(version, data_dir)
  unique_ids <- unique(comid)
  max_matches <- gx_crosswalk_max_matches()
  max_rows <- gx_crosswalk_max_rows()
  matches <- gx_mainstem_lookup_scan(
    lookup$verification$path,
    lookup$spec,
    targets = unique_ids,
    max_matches = max_matches
  )
  if (nrow(matches) > max_matches) {
    gx_abort(
      "The COMID crosswalk exceeded its aggregate match ceiling.",
      "gx_error_crosswalk_budget"
    )
  }
  found <- split(matches, factor(matches$comid, levels = unique_ids), drop = FALSE)
  frequencies <- tabulate(match(comid, unique_ids), nbins = length(unique_ids))
  rows_per_id <- pmax(1L, vapply(found, nrow, integer(1)))
  projected_rows <- sum(as.double(frequencies) * as.double(rows_per_id))
  if (!is.finite(projected_rows) || projected_rows > max_rows) {
    gx_abort(
      "The COMID crosswalk would exceed its output-row ceiling during input expansion.",
      "gx_error_crosswalk_budget"
    )
  }
  rows <- list()
  for (input_index in seq_along(comid)) {
    requested <- comid[[input_index]]
    result <- found[[requested]]
    if (!nrow(result)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_comid = requested,
        status = "not_found",
        match_index = NA_integer_,
        comid = NA_character_,
        mainstem_uri = NA_character_,
        mapping_release = lookup$spec$release,
        mainstem_status = NA_character_,
        diagnostics = list(gx_comid_crosswalk_row_diagnostics(
          "not_found", input_index
        ))
      )
      next
    }
    result <- result[order(result$uri, method = "radix"), , drop = FALSE]
    status <- if (nrow(result) == 1L) "matched" else "ambiguous"
    for (match_index in seq_len(nrow(result))) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_comid = requested,
        status = status,
        match_index = as.integer(match_index),
        comid = as.character(result$comid[[match_index]]),
        mainstem_uri = as.character(result$uri[[match_index]]),
        mapping_release = lookup$spec$release,
        mainstem_status = "active_in_mapping_release",
        diagnostics = list(gx_comid_crosswalk_row_diagnostics(
          status, input_index
        ))
      )
    }
  }
  out <- tibble::as_tibble(do.call(rbind, rows))
  metadata <- gx_comid_crosswalk_metadata(
    out,
    comid,
    lookup$spec,
    lookup$verification
  )
  gx_new_comid_crosswalk(out, metadata)
}
