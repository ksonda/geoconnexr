.gx_huc12_source <- "huc12pp"

gx_crosswalk_huc12s <- function(huc12) {
  if (!is.character(huc12)) {
    gx_abort(
      "{.arg huc12} must be character so leading zeroes are preserved.",
      c("gx_error_crosswalk_input", "gx_error_identifier")
    )
  }
  if (length(huc12) > gx_crosswalk_max_inputs()) {
    gx_abort(
      "{.arg huc12} exceeds the configured crosswalk input ceiling.",
      "gx_error_crosswalk_budget"
    )
  }
  valid <- !is.na(huc12) & grepl("^[0-9]{12}\\z", huc12, perl = TRUE)
  if (any(!valid)) {
    gx_abort(
      "{.arg huc12} values must contain exactly 12 ASCII digits.",
      c("gx_error_crosswalk_input", "gx_error_identifier")
    )
  }
  huc12
}

gx_huc12_method <- function(method) {
  tryCatch(
    match.arg(method, c("outlet", "intersects")),
    error = function(cnd) {
      gx_abort(
        "{.arg method} must be 'outlet' or 'intersects'.",
        "gx_error_crosswalk_input"
      )
    }
  )
}

gx_huc12_client <- function(client) {
  if (!inherits(client, "gx_client") || !identical(client$endpoint, "nldi")) {
    gx_abort(
      "{.arg client} must be an NLDI client created by {.fn gx_client}.",
      "gx_error_crosswalk_input"
    )
  }
  client
}

gx_empty_huc12_crosswalk <- function() {
  tibble::tibble(
    contract_version = character(),
    input_index = integer(),
    requested_huc12 = character(),
    status = character(),
    match_index = integer(),
    huc12 = character(),
    comid = character(),
    mainstem_uri = character(),
    match_source = character(),
    mainstem_status = character(),
    replacement_uris = list(),
    mainstem_observed_at = as.POSIXct(character(), tz = "UTC"),
    mainstem_retrieval_mode = character(),
    diagnostics = list()
  )
}

gx_huc12_row_diagnostics <- function(
    status,
    input_index,
    source = NA_character_,
    mainstem_status = if (identical(status, "not_found")) {
      NA_character_
    } else if (identical(source, "nldi_mainstem")) {
      "currentness_not_checked"
    } else {
      "active_in_mapping_release"
    }) {
  path <- paste0("/inputs/", input_index - 1L)
  if (identical(status, "not_found")) {
    message <- if (identical(source, "pinned_comid_mapping")) {
      "The NLDI outlet COMID is absent from the pinned mainstem mapping release."
    } else {
      "The HUC12 is absent from the NLDI huc12pp source."
    }
    code <- if (identical(source, "pinned_comid_mapping")) {
      "not_found_in_mapping_release"
    } else {
      "huc12_not_found"
    }
    return(gx_diagnostic("warning", code, path, message))
  }
  diagnostics <- gx_crosswalk_currentness_diagnostic(
    mainstem_status,
    path,
    "The HUC12 outlet match does not assert current mainstem service state."
  )
  if (identical(status, "ambiguous")) {
    diagnostics <- gx_bind_diagnostics(
      gx_diagnostic(
        "warning",
        "multiple_mapping_matches",
        path,
        "The outlet COMID has multiple mainstem matches in the mapping release."
      ),
      diagnostics
    )
  }
  diagnostics
}

gx_huc12_scalar <- function(x) {
  if (is.null(x) || is.list(x) || length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  as.character(x)
}

gx_huc12_parse_feature <- function(response, huc12) {
  value <- tryCatch(
    gx_ref_json(response, "NLDI HUC12 feature"),
    error = function(cnd) {
      gx_abort(
        "NLDI returned an invalid HUC12 GeoJSON payload.",
        "gx_error_crosswalk_nldi_payload"
      )
    }
  )
  if (!identical(value$type, "FeatureCollection") ||
      !is.list(value$features) || length(value$features) != 1L) {
    gx_abort(
      "NLDI must return exactly one HUC12 outlet feature.",
      "gx_error_crosswalk_nldi_payload"
    )
  }
  feature <- value$features[[1]]
  if (!is.list(feature)) {
    gx_abort(
      "NLDI HUC12 outlet feature must be a GeoJSON object.",
      "gx_error_crosswalk_nldi_payload"
    )
  }
  properties <- feature$properties
  geometry <- feature$geometry
  if (!is.list(properties) || !is.list(geometry)) {
    gx_abort(
      "NLDI HUC12 outlet properties and geometry must be objects.",
      "gx_error_crosswalk_nldi_payload"
    )
  }
  coordinates <- geometry$coordinates %||% list()
  coordinates <- unlist(coordinates, recursive = TRUE, use.names = FALSE)
  finite_point <- length(coordinates) == 2L && is.numeric(coordinates) &&
    all(is.finite(coordinates)) && abs(coordinates[[1]]) <= 180 &&
    abs(coordinates[[2]]) <= 90
  identities <- c(
    gx_huc12_scalar(feature$id),
    gx_huc12_scalar(properties$identifier),
    gx_huc12_scalar(properties$uri)
  )
  if (!identical(feature$type, "Feature") ||
      !identical(geometry$type, "Point") || !finite_point ||
      anyNA(identities) || any(identities != huc12) ||
      !identical(gx_huc12_scalar(properties$source), .gx_huc12_source)) {
    gx_abort(
      "NLDI HUC12 outlet identity or geometry did not match the request.",
      "gx_error_crosswalk_nldi_contract"
    )
  }
  comid <- gx_huc12_scalar(properties$comid)
  if (!is.na(comid)) {
    numeric_comid <- suppressWarnings(as.numeric(comid))
    if (!grepl("^[1-9][0-9]{0,9}\\z", comid, perl = TRUE) ||
        !is.finite(numeric_comid) || numeric_comid != trunc(numeric_comid)) {
      gx_abort(
        "NLDI returned an invalid outlet COMID.",
        "gx_error_crosswalk_nldi_contract"
      )
    }
  }
  mainstem_uri <- gx_huc12_scalar(properties$mainstem)
  if (!is.na(mainstem_uri) &&
      !gx_crosswalk_valid_mainstem_uri(mainstem_uri, allow_na = FALSE)) {
    gx_abort(
      "NLDI returned an invalid mainstem PID.",
      "gx_error_crosswalk_nldi_contract"
    )
  }
  if (is.na(mainstem_uri) && is.na(comid)) {
    gx_abort(
      "NLDI HUC12 outlet omitted both mainstem and COMID identifiers.",
      "gx_error_crosswalk_nldi_contract"
    )
  }
  list(huc12 = huc12, comid = comid, mainstem_uri = mainstem_uri)
}

gx_huc12_attempt_control <- function(state, max_requests, max_total_bytes) {
  list(
    before = function(request, physical) {
      if (nrow(state$requests) >= max_requests) {
        gx_abort(
          "The HUC12 crosswalk exhausted its request budget.",
          "gx_error_crosswalk_budget",
          requests = state$requests
        )
      }
      remaining <- max_total_bytes - sum(as.double(state$requests$bytes))
      if (!is.finite(remaining) || remaining < 1) {
        gx_abort(
          "The HUC12 crosswalk exhausted its response-byte budget.",
          "gx_error_crosswalk_budget",
          requests = state$requests
        )
      }
      as.integer(min(as.double(request$max_bytes), floor(remaining)))
    },
    after = function(attempt) {
      state$requests <- rbind(
        state$requests,
        gx_http_attempt_request_row(attempt)
      )
      invisible(NULL)
    }
  )
}

gx_huc12_fetch_one <- function(huc12, client, state, max_requests,
                               max_total_bytes) {
  path <- utils::URLencode(huc12, reserved = TRUE, repeated = TRUE)
  url <- paste0(
    sub("/+$", "", client$base_url),
    "/", .gx_huc12_source, "/", path, "?f=json"
  )
  response <- tryCatch(
    gx_http_request(
      client,
      method = "GET",
      url = url,
      headers = list(Accept = "application/geo+json, application/json;q=0.9"),
      check_status = FALSE,
      .attempt_control = gx_huc12_attempt_control(
        state, max_requests, max_total_bytes
      )
    ),
    error = function(cnd) {
      if (inherits(cnd, "gx_error")) cnd$requests <- state$requests
      stop(cnd)
    }
  )
  if (identical(response$status, 404L)) return(NULL)
  if (response$status < 200L || response$status >= 300L) {
    gx_abort(
      "NLDI HUC12 lookup failed with HTTP status {response$status}.",
      "gx_error_crosswalk_nldi_http",
      status = response$status,
      requests = state$requests
    )
  }
  tryCatch(
    gx_huc12_parse_feature(response, huc12),
    error = function(cnd) {
      if (inherits(cnd, "gx_error")) cnd$requests <- state$requests
      stop(cnd)
    }
  )
}

gx_huc12_metadata <- function(
    x,
    huc12,
    requests,
    client,
    mapping = NULL,
    currentness_policy = "not_checked",
    currentness_collection = NA_character_,
    currentness_dataset_vintage = NA_character_) {
  statuses <- if (length(huc12)) {
    vapply(seq_along(huc12), function(index) {
      unique(x$status[x$input_index == index])[[1]]
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
    operation = "huc12_to_mainstem",
    method = "outlet",
    currentness_policy = currentness_policy,
    currentness_collection = currentness_collection,
    currentness_dataset_vintage = currentness_dataset_vintage,
    input_count = as.integer(length(huc12)),
    unique_input_count = as.integer(length(unique(huc12))),
    matched_input_count = as.integer(sum(statuses == "matched")),
    match_count = as.integer(sum(x$status != "not_found")),
    not_found_input_count = as.integer(sum(statuses == "not_found")),
    ambiguous_input_count = as.integer(sum(statuses == "ambiguous")),
    complete = TRUE,
    retrieved_at = if (nrow(requests)) max(requests$retrieved_at) else
      as.POSIXct(NA, tz = "UTC"),
    requests = requests,
    diagnostics = diagnostics,
    nldi = list(source = .gx_huc12_source, endpoint = client$base_url),
    mapping = mapping
  )
}

gx_validate_huc12_crosswalk <- function(x, metadata = attr(x, "gx_crosswalk")) {
  expected <- names(gx_empty_huc12_crosswalk())
  metadata_expected <- c(
    "contract_version", "operation", "method", "currentness_policy",
    "currentness_collection", "currentness_dataset_vintage",
    "input_count", "unique_input_count", "matched_input_count",
    "match_count", "not_found_input_count", "ambiguous_input_count",
    "complete", "retrieved_at", "requests", "diagnostics", "nldi", "mapping"
  )
  count_names <- c(
    "input_count", "unique_input_count", "matched_input_count", "match_count",
    "not_found_input_count", "ambiguous_input_count"
  )
  diagnostic_names <- names(gx_empty_diagnostics())
  valid <- is.data.frame(x) && identical(names(x), expected) &&
    is.character(x$contract_version) &&
    all(x$contract_version == .gx_crosswalk_contract_version) &&
    is.integer(x$input_index) && is.character(x$requested_huc12) &&
    is.character(x$status) && is.integer(x$match_index) &&
    is.character(x$huc12) && is.character(x$comid) &&
    is.character(x$mainstem_uri) && is.character(x$match_source) &&
    is.character(x$mainstem_status) && is.list(x$replacement_uris) &&
    inherits(x$mainstem_observed_at, "POSIXct") &&
    is.character(x$mainstem_retrieval_mode) && is.list(x$diagnostics) &&
    all(vapply(x$diagnostics, function(item) {
      is.data.frame(item) && identical(names(item), diagnostic_names)
    }, logical(1))) &&
    !anyNA(x$input_index) && all(x$input_index >= 1L) &&
    !anyNA(x$requested_huc12) &&
    all(x$status %in% c("matched", "ambiguous", "not_found")) &&
    all(grepl("^[0-9]{12}\\z", x$requested_huc12, perl = TRUE)) &&
    is.list(metadata) && identical(names(metadata), metadata_expected) &&
    identical(metadata$contract_version, .gx_crosswalk_contract_version) &&
    identical(metadata$operation, "huc12_to_mainstem") &&
    identical(metadata$method, "outlet") &&
    metadata$currentness_policy %in% c("not_checked", "live_v3_observed") &&
    is.character(metadata$currentness_collection) &&
    length(metadata$currentness_collection) == 1L &&
    is.character(metadata$currentness_dataset_vintage) &&
    length(metadata$currentness_dataset_vintage) == 1L &&
    all(vapply(metadata[count_names], function(value) {
      is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
    }, logical(1))) &&
    isTRUE(metadata$complete) && inherits(metadata$retrieved_at, "POSIXct") &&
    length(metadata$retrieved_at) == 1L && is.data.frame(metadata$requests) &&
    identical(names(metadata$requests), names(gx_crosswalk_empty_requests())) &&
    is.data.frame(metadata$diagnostics) &&
    identical(names(metadata$diagnostics), diagnostic_names) &&
    is.list(metadata$nldi) &&
    identical(names(metadata$nldi), c("source", "endpoint")) &&
    identical(metadata$nldi$source, .gx_huc12_source) &&
    is.character(metadata$nldi$endpoint) && length(metadata$nldi$endpoint) == 1L
  if (!isTRUE(valid)) {
    gx_abort(
      "HUC12 crosswalk output does not satisfy its contract.",
      "gx_error_crosswalk_contract"
    )
  }
  if (nrow(x)) {
    found <- x$status != "not_found"
    direct <- !is.na(x$match_source) & x$match_source == "nldi_mainstem"
    mapped <- !is.na(x$match_source) &
      x$match_source == "pinned_comid_mapping"
    not_found <- !found
    allowed_mainstem_status <- if (identical(
      metadata$currentness_policy,
      "not_checked"
    )) {
      c("currentness_not_checked", "active_in_mapping_release")
    } else {
      c("current", "superseded", "superseded_unresolved")
    }
    identities_ok <- all(is.na(x$huc12[!found]) | x$huc12[!found] ==
      x$requested_huc12[!found]) &&
      all(x$huc12[found] == x$requested_huc12[found]) &&
      all(!is.na(x$match_index[found]) & x$match_index[found] >= 1L) &&
      all(!is.na(x$mainstem_uri[found])) &&
      all(vapply(
        x$mainstem_uri[found],
        gx_crosswalk_valid_mainstem_uri,
        logical(1),
        allow_na = FALSE
      )) &&
      all(direct | mapped | not_found) &&
      all(!is.na(x$comid[direct | mapped])) &&
      all(grepl("^[1-9][0-9]{0,9}\\z", x$comid[direct | mapped], perl = TRUE)) &&
      all(x$mainstem_status[found] %in% allowed_mainstem_status) &&
      all(is.na(x$mainstem_uri[not_found])) &&
      all(is.na(x$match_index[not_found]))
    currentness_rows_ok <- all(vapply(seq_len(nrow(x)), function(row) {
      replacements <- x$replacement_uris[[row]]
      valid_replacements <- all(vapply(
        replacements,
        gx_crosswalk_valid_mainstem_uri,
        logical(1),
        allow_na = FALSE
      )) && !anyDuplicated(replacements)
      if (!valid_replacements) return(FALSE)
      if (!found[[row]]) {
        return(length(replacements) == 0L &&
          is.na(x$mainstem_observed_at[[row]]) &&
          is.na(x$mainstem_retrieval_mode[[row]]))
      }
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
        gx_huc12_row_diagnostics(
          x$status[[row]], x$input_index[[row]], x$match_source[[row]],
          x$mainstem_status[[row]]
        )
      )
    }, logical(1)))
    if (!identities_ok || !currentness_rows_ok || !diagnostics_ok) {
      gx_abort(
        "HUC12 crosswalk identities do not satisfy their contract.",
        "gx_error_crosswalk_contract"
      )
    }
  }
  input_status <- if (metadata$input_count) {
    vapply(seq_len(metadata$input_count), function(index) {
      statuses <- unique(x$status[x$input_index == index])
      if (length(statuses) == 1L) statuses else NA_character_
    }, character(1))
  } else {
    character()
  }
  reconciled <- metadata$input_count == length(unique(x$input_index)) &&
    identical(unique(x$input_index), seq_len(metadata$input_count)) &&
    metadata$unique_input_count == length(unique(x$requested_huc12)) &&
    metadata$matched_input_count == sum(input_status == "matched") &&
    metadata$not_found_input_count == sum(input_status == "not_found") &&
    metadata$ambiguous_input_count == sum(input_status == "ambiguous") &&
    metadata$match_count == sum(x$status != "not_found")
  if (!isTRUE(reconciled)) {
    gx_abort(
      "HUC12 crosswalk metadata does not reconcile with its rows.",
      "gx_error_crosswalk_contract"
    )
  }
  if (is.null(metadata$mapping)) {
    if (any(
      !is.na(x$match_source) & x$match_source == "pinned_comid_mapping"
    )) {
      gx_abort(
        "HUC12 COMID fallback omitted mapping provenance.",
        "gx_error_crosswalk_contract"
      )
    }
  } else {
    gx_validate_comid_mapping_metadata(metadata$mapping)
  }
  currentness_metadata_ok <- if (identical(
    metadata$currentness_policy,
    "not_checked"
  )) {
    is.na(metadata$currentness_collection) &&
      is.na(metadata$currentness_dataset_vintage)
  } else {
    identical(metadata$currentness_collection, .gx_mainstem_collection) &&
      identical(
        metadata$currentness_dataset_vintage,
        .gx_mainstem_dataset_vintage
      )
  }
  time_ok <- if (nrow(metadata$requests)) {
    !is.na(metadata$retrieved_at) &&
      identical(metadata$retrieved_at, max(metadata$requests$retrieved_at))
  } else {
    is.na(metadata$retrieved_at)
  }
  if (!time_ok || !isTRUE(currentness_metadata_ok)) {
    gx_abort(
      "HUC12 crosswalk retrieval time does not reconcile with its ledger.",
      "gx_error_crosswalk_contract"
    )
  }
  invisible(x)
}

gx_new_huc12_crosswalk <- function(x, metadata) {
  gx_validate_huc12_crosswalk(x, metadata)
  attr(x, "gx_crosswalk") <- metadata
  class(x) <- unique(c("gx_huc12_crosswalk", "gx_crosswalk", class(x)))
  x
}

gx_huc12_compose_currentness <- function(
    out,
    metadata,
    client,
    max_requests,
    max_total_bytes) {
  composed <- gx_crosswalk_apply_currentness(
    out,
    client,
    requests = metadata$requests,
    max_requests = max_requests,
    max_total_bytes = max_total_bytes
  )
  out <- composed$rows
  out$diagnostics <- lapply(seq_len(nrow(out)), function(row) {
    gx_huc12_row_diagnostics(
      out$status[[row]], out$input_index[[row]], out$match_source[[row]],
      out$mainstem_status[[row]]
    )
  })
  metadata$currentness_policy <- composed$currentness_policy
  metadata$currentness_collection <- composed$currentness_collection
  metadata$currentness_dataset_vintage <-
    composed$currentness_dataset_vintage
  metadata$retrieved_at <- gx_crosswalk_retrieved_at(composed$requests)
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
  gx_new_huc12_crosswalk(out, metadata)
}

#' Map HUC12 outlets to mainstem PIDs
#'
#' Retrieves one HUC12 pour point from the USGS NLDI `huc12pp` source. An
#' advertised mainstem PID is preferred. If the response has only a COMID, the
#' function resolves it through the explicitly installed checksum-pinned
#' mapping release. No lookup data is downloaded or refreshed implicitly. In
#' outlet mode, `check = TRUE` composes every match with live `mainstems_v3`
#' currentness without following or ranking replacements.
#'
#' @param huc12 Character vector containing exact 12-digit hydrologic unit
#'   codes.
#' @param method `"outlet"` uses the NLDI pour point. `"intersects"` retrieves
#'   the reference HUC12 polygon and returns every locally intersecting
#'   `mainstems_v3` geometry with disclosed ranking metrics.
#' @param check Whether outlet matches should include bounded live currentness.
#'   Intersection matches already carry observed `mainstems_v3` state.
#' @param version Registered mapping release used only when an NLDI response
#'   omits its mainstem PID.
#' @param data_dir Package data directory containing an explicitly installed
#'   lookup when COMID fallback is needed.
#' @param client An NLDI client for `method = "outlet"`, a reference client for
#'   `method = "intersects"`, or `NULL` to construct the matching default.
#' @param currentness_client A reference client used for checked outlet matches,
#'   or `NULL` to construct the default.
#'
#' @return For `method = "outlet"`, a `gx_huc12_crosswalk` tibble with NLDI and
#'   optional mapping provenance. For `method = "intersects"`, a
#'   `gx_huc12_intersection_crosswalk` tibble containing every ranked geometry
#'   match, its metrics, observed currentness, replacements, and reference
#'   request ledger.
#' @export
gx_huc12_to_mainstem <- function(
    huc12,
    method = c("outlet", "intersects"),
    check = FALSE,
    version = "v3.2",
    data_dir = gx_default_data_dir(),
    client = NULL,
    currentness_client = NULL) {
  method <- gx_huc12_method(method)
  check <- gx_crosswalk_check(check)
  huc12 <- gx_crosswalk_huc12s(huc12)
  if (is.null(client)) {
    client <- gx_client(if (identical(method, "outlet")) "nldi" else "reference")
  }
  if (identical(method, "intersects")) {
    return(gx_huc12_to_mainstem_intersects(huc12, client))
  }
  gx_huc12_client(client)
  max_requests <- gx_crosswalk_max_requests()
  max_total_bytes <- gx_crosswalk_total_bytes(client)
  if (check && is.null(currentness_client)) {
    currentness_client <- gx_client("reference")
  }
  if (!length(huc12)) {
    out <- gx_empty_huc12_crosswalk()
    metadata <- gx_huc12_metadata(
      out, huc12, gx_crosswalk_empty_requests(), client
    )
    out <- gx_new_huc12_crosswalk(out, metadata)
    if (!check) return(out)
    return(gx_huc12_compose_currentness(
      out, metadata, currentness_client, max_requests, max_total_bytes
    ))
  }

  state <- new.env(parent = emptyenv())
  state$requests <- gx_crosswalk_empty_requests()
  unique_huc12 <- unique(huc12)
  fetched <- stats::setNames(lapply(unique_huc12, function(id) {
    gx_huc12_fetch_one(
      id, client, state, max_requests = max_requests,
      max_total_bytes = max_total_bytes
    )
  }), unique_huc12)

  fallback_comids <- unique(vapply(fetched, function(item) {
    if (is.null(item) || !is.na(item$mainstem_uri)) NA_character_ else item$comid
  }, character(1)))
  fallback_comids <- fallback_comids[!is.na(fallback_comids)]
  fallback <- NULL
  mapping <- NULL
  if (length(fallback_comids)) {
    fallback <- gx_comid_to_mainstem_impl(
      fallback_comids, version = version, data_dir = data_dir
    )
    mapping <- attr(fallback, "gx_crosswalk")$mapping
  }

  rows <- list()
  for (input_index in seq_along(huc12)) {
    requested <- huc12[[input_index]]
    item <- fetched[[requested]]
    if (is.null(item)) {
      status <- "not_found"
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_huc12 = requested,
        status = status,
        match_index = NA_integer_,
        huc12 = NA_character_,
        comid = NA_character_,
        mainstem_uri = NA_character_,
        match_source = NA_character_,
        mainstem_status = NA_character_,
        replacement_uris = list(character()),
        mainstem_observed_at = as.POSIXct(NA, tz = "UTC"),
        mainstem_retrieval_mode = NA_character_,
        diagnostics = list(gx_huc12_row_diagnostics(status, input_index))
      )
      next
    }
    if (!is.na(item$mainstem_uri)) {
      status <- "matched"
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_huc12 = requested,
        status = status,
        match_index = 1L,
        huc12 = requested,
        comid = item$comid,
        mainstem_uri = item$mainstem_uri,
        match_source = "nldi_mainstem",
        mainstem_status = "currentness_not_checked",
        replacement_uris = list(character()),
        mainstem_observed_at = as.POSIXct(NA, tz = "UTC"),
        mainstem_retrieval_mode = NA_character_,
        diagnostics = list(gx_huc12_row_diagnostics(
          status, input_index, "nldi_mainstem"
        ))
      )
      next
    }
    mapped <- fallback[fallback$requested_comid == item$comid, , drop = FALSE]
    for (row_index in seq_len(nrow(mapped))) {
      status <- mapped$status[[row_index]]
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_huc12 = requested,
        status = status,
        match_index = mapped$match_index[[row_index]],
        huc12 = requested,
        comid = item$comid,
        mainstem_uri = mapped$mainstem_uri[[row_index]],
        match_source = "pinned_comid_mapping",
        mainstem_status = if (status == "not_found") NA_character_ else
          "active_in_mapping_release",
        replacement_uris = list(character()),
        mainstem_observed_at = as.POSIXct(NA, tz = "UTC"),
        mainstem_retrieval_mode = NA_character_,
        diagnostics = list(gx_huc12_row_diagnostics(
          status, input_index, "pinned_comid_mapping"
        ))
      )
    }
  }
  out <- tibble::as_tibble(do.call(rbind, rows))
  metadata <- gx_huc12_metadata(
    out, huc12, state$requests, client, mapping = mapping
  )
  out <- gx_new_huc12_crosswalk(out, metadata)
  if (!check) return(out)
  gx_huc12_compose_currentness(
    out, metadata, currentness_client, max_requests, max_total_bytes
  )
}
