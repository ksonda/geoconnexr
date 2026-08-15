.gx_mainstem_collection <- "mainstems_v3"
.gx_mainstem_dataset_vintage <- "3.0"

gx_mainstem_uri_id <- function(uri) {
  sub(
    "^https://geoconnex[.]us/ref/mainstems/",
    "",
    uri,
    perl = TRUE
  )
}

gx_mainstem_replacements <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    gx_abort(
      "Mainstem replacement metadata must be one string.",
      "gx_error_crosswalk_currentness_contract"
    )
  }
  if (!nzchar(value)) return(character())
  valid_shape <- grepl(
    paste0(
      "^\\['https://geoconnex[.]us/ref/mainstems/[1-9][0-9]*'",
      "(?:, 'https://geoconnex[.]us/ref/mainstems/[1-9][0-9]*')*\\]$"
    ),
    value,
    perl = TRUE
  )
  if (!valid_shape) {
    gx_abort(
      "Mainstem replacements do not match the checked upstream list encoding.",
      "gx_error_crosswalk_currentness_contract"
    )
  }
  inner <- substring(value, 3L, nchar(value) - 2L)
  replacements <- strsplit(inner, "', '", fixed = TRUE)[[1L]]
  if (anyDuplicated(replacements) || !all(vapply(
    replacements,
    gx_crosswalk_valid_mainstem_uri,
    logical(1),
    allow_na = FALSE
  ))) {
    gx_abort(
      "Mainstem replacement PIDs are invalid or duplicated.",
      "gx_error_crosswalk_currentness_contract"
    )
  }
  replacements
}

gx_empty_mainstem_currentness <- function() {
  tibble::tibble(
    contract_version = character(),
    input_index = integer(),
    requested_mainstem_uri = character(),
    status = character(),
    replacement_index = integer(),
    replacement_uri = character(),
    collection = character(),
    dataset_vintage = character(),
    observed_at = as.POSIXct(character(), tz = "UTC"),
    retrieval_mode = character(),
    diagnostics = list()
  )
}

gx_mainstem_currentness_diagnostics <- function(status, input_index) {
  if (!identical(status, "superseded_unresolved")) {
    return(gx_empty_diagnostics())
  }
  gx_diagnostic(
    "warning",
    "superseded_without_replacement",
    paste0("/inputs/", input_index - 1L),
    "The service marks the mainstem superseded but advertises no replacement."
  )
}

gx_mainstem_feature_record <- function(feature, requested_uri) {
  metadata <- attr(feature, "gx_reference")
  id <- gx_mainstem_uri_id(requested_uri)
  valid <- nrow(feature) == 1L &&
    identical(as.character(feature$feature_id[[1L]]), id) &&
    "id" %in% names(feature) && identical(as.character(feature$id[[1L]]), id) &&
    "uri" %in% names(feature) &&
    identical(as.character(feature$uri[[1L]]), requested_uri) &&
    "superseded" %in% names(feature) && is.logical(feature$superseded) &&
    length(feature$superseded) == 1L && !is.na(feature$superseded[[1L]]) &&
    "new_mainstemid" %in% names(feature) &&
    is.character(feature$new_mainstemid) &&
    length(feature$new_mainstemid) == 1L &&
    !is.na(feature$new_mainstemid[[1L]]) && is.list(metadata) &&
    identical(metadata$collection, .gx_mainstem_collection) &&
    identical(metadata$id, id) && isTRUE(metadata$complete) &&
    inherits(metadata$retrieved_at, "POSIXct") &&
    length(metadata$retrieved_at) == 1L && !is.na(metadata$retrieved_at) &&
    metadata$retrieval_mode %in% c("item", "filter")
  if (!isTRUE(valid)) {
    gx_abort(
      "The mainstems_v3 feature does not satisfy the currentness contract.",
      "gx_error_crosswalk_currentness_contract"
    )
  }
  replacements <- gx_mainstem_replacements(feature$new_mainstemid[[1L]])
  superseded <- isTRUE(feature$superseded[[1L]])
  if (!superseded && length(replacements)) {
    gx_abort(
      "A current mainstem unexpectedly advertises replacement PIDs.",
      "gx_error_crosswalk_currentness_contract"
    )
  }
  list(
    superseded = superseded,
    replacements = replacements,
    observed_at = metadata$retrieved_at,
    retrieval_mode = metadata$retrieval_mode,
    requests = gx_crosswalk_reference_requests(feature)
  )
}

gx_mainstem_currentness_fetch <- function(uri, client, requests,
                                          max_requests, max_total_bytes) {
  remaining_requests <- max_requests - nrow(requests)
  remaining_bytes <- max_total_bytes - sum(as.double(requests$bytes))
  if (remaining_requests < 4L || !is.finite(remaining_bytes) ||
      remaining_bytes < 4) {
    gx_abort(
      "The mainstem currentness workflow has insufficient aggregate budget.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }
  call_client <- client
  call_client$retries <- 0L
  call_client$max_bytes <- as.integer(min(
    as.double(client$max_bytes),
    floor(remaining_bytes / 4)
  ))
  id <- gx_mainstem_uri_id(uri)
  feature <- tryCatch(
    gx_ref_feature(.gx_mainstem_collection, id, client = call_client),
    error = function(cnd) {
      prior <- gx_crosswalk_condition_requests(cnd)
      if (nrow(prior)) requests <- gx_crosswalk_merge_requests(requests, prior)
      if (inherits(cnd, "gx_error")) cnd$requests <- requests
      stop(cnd)
    }
  )
  record <- tryCatch(
    gx_mainstem_feature_record(feature, uri),
    error = function(cnd) {
      feature_requests <- gx_crosswalk_reference_requests(feature)
      merged <- gx_crosswalk_merge_requests(requests, feature_requests)
      if (inherits(cnd, "gx_error")) cnd$requests <- merged
      stop(cnd)
    }
  )
  merged <- gx_crosswalk_merge_requests(requests, record$requests)
  if (nrow(merged) > max_requests ||
      sum(as.double(merged$bytes)) > max_total_bytes) {
    gx_abort(
      "The mainstem currentness workflow exceeded its aggregate budget.",
      "gx_error_crosswalk_budget",
      requests = merged
    )
  }
  list(record = record, requests = merged)
}

gx_mainstem_currentness_metadata <- function(x, input, requests) {
  statuses <- if (length(input)) {
    vapply(seq_along(input), function(index) {
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
    operation = "mainstem_currentness",
    collection = .gx_mainstem_collection,
    dataset_vintage = .gx_mainstem_dataset_vintage,
    input_count = as.integer(length(input)),
    unique_input_count = as.integer(length(unique(input))),
    current_input_count = as.integer(sum(statuses == "current")),
    superseded_input_count = as.integer(sum(
      statuses %in% c("superseded", "superseded_unresolved")
    )),
    replacement_count = as.integer(sum(!is.na(x$replacement_uri))),
    complete = TRUE,
    retrieved_at = if (nrow(requests)) max(requests$retrieved_at) else
      as.POSIXct(NA, tz = "UTC"),
    requests = requests,
    diagnostics = diagnostics
  )
}

gx_validate_mainstem_currentness <- function(
    x,
    metadata = attr(x, "gx_crosswalk")) {
  expected <- names(gx_empty_mainstem_currentness())
  diagnostic_names <- names(gx_empty_diagnostics())
  valid <- is.data.frame(x) && identical(names(x), expected) &&
    is.character(x$contract_version) &&
    all(x$contract_version == .gx_crosswalk_contract_version) &&
    is.integer(x$input_index) && !anyNA(x$input_index) &&
    all(x$input_index >= 1L) && is.character(x$requested_mainstem_uri) &&
    all(vapply(
      x$requested_mainstem_uri, gx_crosswalk_valid_mainstem_uri,
      logical(1), allow_na = FALSE
    )) && is.character(x$status) &&
    all(x$status %in% c("current", "superseded", "superseded_unresolved")) &&
    is.integer(x$replacement_index) && is.character(x$replacement_uri) &&
    is.character(x$collection) &&
    all(x$collection == .gx_mainstem_collection) &&
    is.character(x$dataset_vintage) &&
    all(x$dataset_vintage == .gx_mainstem_dataset_vintage) &&
    inherits(x$observed_at, "POSIXct") && !anyNA(x$observed_at) &&
    is.character(x$retrieval_mode) &&
    all(x$retrieval_mode %in% c("item", "filter")) &&
    is.list(x$diagnostics) && all(vapply(x$diagnostics, function(item) {
      is.data.frame(item) && identical(names(item), diagnostic_names)
    }, logical(1))) && is.list(metadata) &&
    identical(metadata$operation, "mainstem_currentness") &&
    identical(metadata$collection, .gx_mainstem_collection) &&
    identical(metadata$dataset_vintage, .gx_mainstem_dataset_vintage) &&
    isTRUE(metadata$complete) && is.data.frame(metadata$requests) &&
    identical(names(metadata$requests), names(gx_crosswalk_empty_requests()))
  if (!isTRUE(valid)) {
    gx_abort(
      "Mainstem currentness output does not satisfy its contract.",
      "gx_error_crosswalk_currentness_contract"
    )
  }
  if (nrow(x)) {
    groups_ok <- all(vapply(split(seq_len(nrow(x)), x$input_index), function(rows) {
      status <- unique(x$status[rows])
      stable <- length(status) == 1L &&
        length(unique(x$requested_mainstem_uri[rows])) == 1L &&
        length(unique(x$observed_at[rows])) == 1L &&
        length(unique(x$retrieval_mode[rows])) == 1L
      if (!stable) return(FALSE)
      if (identical(status, "current")) {
        return(length(rows) == 1L && is.na(x$replacement_index[rows]) &&
          is.na(x$replacement_uri[rows]))
      }
      if (identical(status, "superseded_unresolved")) {
        return(length(rows) == 1L && is.na(x$replacement_index[rows]) &&
          is.na(x$replacement_uri[rows]))
      }
      identical(x$replacement_index[rows], seq_along(rows)) &&
        all(vapply(
          x$replacement_uri[rows], gx_crosswalk_valid_mainstem_uri,
          logical(1), allow_na = FALSE
        )) && !anyDuplicated(x$replacement_uri[rows])
    }, logical(1)))
    diagnostics_ok <- all(vapply(seq_len(nrow(x)), function(row) {
      identical(
        x$diagnostics[[row]],
        gx_mainstem_currentness_diagnostics(
          x$status[[row]], x$input_index[[row]]
        )
      )
    }, logical(1)))
    if (!groups_ok || !diagnostics_ok ||
        !identical(order(x$input_index, method = "radix"), seq_len(nrow(x)))) {
      gx_abort(
        "Mainstem currentness rows do not preserve migration identity.",
        "gx_error_crosswalk_currentness_contract"
      )
    }
  }
  invisible(x)
}

gx_new_mainstem_currentness <- function(x, metadata) {
  gx_validate_mainstem_currentness(x, metadata)
  attr(x, "gx_crosswalk") <- metadata
  class(x) <- unique(c("gx_mainstem_currentness", "gx_crosswalk", class(x)))
  x
}

gx_mainstem_impl <- function(
    mainstem_uri,
    client,
    max_requests,
    max_total_bytes) {
  mainstem_uri <- gx_crosswalk_mainstem_uris(mainstem_uri)
  gx_ref_client(client)
  if (!length(mainstem_uri)) {
    out <- gx_empty_mainstem_currentness()
    metadata <- gx_mainstem_currentness_metadata(
      out, mainstem_uri, gx_crosswalk_empty_requests()
    )
    return(gx_new_mainstem_currentness(out, metadata))
  }
  unique_uri <- unique(mainstem_uri)
  if (length(unique_uri) * 4 > max_requests) {
    gx_abort(
      "Mainstem currentness inputs exceed the conservative request allocation.",
      "gx_error_crosswalk_budget"
    )
  }
  requests <- gx_crosswalk_empty_requests()
  records <- list()
  for (uri in unique_uri) {
    fetched <- gx_mainstem_currentness_fetch(
      uri, client, requests, max_requests, max_total_bytes
    )
    records[[uri]] <- fetched$record
    requests <- fetched$requests
  }
  rows <- list()
  for (input_index in seq_along(mainstem_uri)) {
    requested <- mainstem_uri[[input_index]]
    record <- records[[requested]]
    status <- if (!record$superseded) {
      "current"
    } else if (length(record$replacements)) {
      "superseded"
    } else {
      "superseded_unresolved"
    }
    replacements <- if (length(record$replacements)) {
      record$replacements
    } else {
      NA_character_
    }
    for (row_index in seq_along(replacements)) {
      has_replacement <- !is.na(replacements[[row_index]])
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_mainstem_uri = requested,
        status = status,
        replacement_index = if (has_replacement) {
          as.integer(row_index)
        } else {
          NA_integer_
        },
        replacement_uri = replacements[[row_index]],
        collection = .gx_mainstem_collection,
        dataset_vintage = .gx_mainstem_dataset_vintage,
        observed_at = record$observed_at,
        retrieval_mode = record$retrieval_mode,
        diagnostics = list(gx_mainstem_currentness_diagnostics(
          status, input_index
        ))
      )
    }
  }
  out <- tibble::as_tibble(do.call(rbind, rows))
  metadata <- gx_mainstem_currentness_metadata(out, mainstem_uri, requests)
  gx_new_mainstem_currentness(out, metadata)
}

#' Check current and superseded mainstem PIDs
#'
#' Retrieves each unique PID from the `mainstems_v3` reference collection and
#' preserves the requested PID, superseded state, every advertised replacement,
#' collection, dataset vintage, observation time, retrieval mode, and request
#' ledger. Replacement PIDs are never followed or ranked automatically.
#'
#' @param mainstem_uri Character vector of canonical Geoconnex mainstem PIDs.
#' @param client A reference client created by [gx_client()].
#'
#' @return A `gx_mainstem_currentness` tibble. Superseded one-to-many mappings
#'   occupy one row per replacement. Its `gx_crosswalk` attribute contains
#'   aggregate counts, diagnostics, and the redacted request ledger.
#' @export
gx_mainstem <- function(
    mainstem_uri,
    client = gx_client("reference")) {
  gx_mainstem_impl(
    mainstem_uri,
    client = client,
    max_requests = gx_crosswalk_max_requests(),
    max_total_bytes = gx_crosswalk_total_bytes(client)
  )
}
