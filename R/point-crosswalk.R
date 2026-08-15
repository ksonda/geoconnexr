gx_crosswalk_points <- function(points) {
  geometry <- if (inherits(points, "sf")) {
    tryCatch(sf::st_geometry(points), error = function(cnd) NULL)
  } else if (inherits(points, "sfc")) {
    points
  } else {
    NULL
  }
  if (is.null(geometry)) {
    gx_abort(
      "{.arg points} must be an {.cls sf} or {.cls sfc} Point object.",
      "gx_error_crosswalk_input"
    )
  }
  if (length(geometry) > gx_crosswalk_max_inputs()) {
    gx_abort(
      "{.arg points} exceeds the configured crosswalk input ceiling.",
      "gx_error_crosswalk_budget"
    )
  }
  crs <- tryCatch(sf::st_crs(geometry), error = function(cnd) NULL)
  if (is.null(crs) || isTRUE(is.na(crs))) {
    gx_abort(
      "{.arg points} must declare a coordinate reference system.",
      "gx_error_crosswalk_input"
    )
  }
  if (!length(geometry)) {
    return(list(
      geometry = sf::st_sfc(crs = gx_aoi_crs),
      longitude = numeric(), latitude = numeric(), wkt = character()
    ))
  }
  types <- tryCatch(
    as.character(sf::st_geometry_type(geometry, by_geometry = TRUE)),
    error = function(cnd) character()
  )
  dimensions <- vapply(geometry, function(item) class(item)[[1L]], character(1))
  empty <- tryCatch(sf::st_is_empty(geometry), error = function(cnd) rep(TRUE, length(geometry)))
  if (length(types) != length(geometry) || any(types != "POINT") ||
      any(dimensions != "XY") || length(empty) != length(geometry) || any(empty)) {
    gx_abort(
      "{.arg points} must contain only nonempty two-dimensional Point geometries.",
      "gx_error_crosswalk_input"
    )
  }
  transformed <- tryCatch(
    gx_aoi_transform_crs84_offline(geometry),
    error = function(cnd) NULL
  )
  transformed_types <- tryCatch(
    as.character(sf::st_geometry_type(transformed, by_geometry = TRUE)),
    error = function(cnd) character()
  )
  if (is.null(transformed) || length(transformed_types) != length(geometry) ||
      any(transformed_types != "POINT")) {
    gx_abort(
      "{.arg points} could not be transformed to OGC CRS84 Points offline.",
      "gx_error_crosswalk_input"
    )
  }
  coordinates <- t(vapply(transformed, function(item) {
    values <- unclass(item)
    if (!is.numeric(values) || length(values) != 2L) c(NA_real_, NA_real_) else
      as.double(values)
  }, numeric(2)))
  longitude <- unname(coordinates[, 1L])
  latitude <- unname(coordinates[, 2L])
  if (any(!is.finite(longitude)) || any(!is.finite(latitude)) ||
      any(longitude < -180 | longitude > 180) ||
      any(latitude < -90 | latitude > 90)) {
    gx_abort(
      "Transformed points must remain inside finite longitude and latitude bounds.",
      "gx_error_crosswalk_input"
    )
  }
  wkt <- paste0(
    "POINT(",
    vapply(longitude, gx_edr_number_text_impl, character(1)),
    " ",
    vapply(latitude, gx_edr_number_text_impl, character(1)),
    ")"
  )
  list(
    geometry = transformed,
    longitude = longitude,
    latitude = latitude,
    wkt = wkt
  )
}

gx_empty_point_crosswalk <- function() {
  tibble::tibble(
    contract_version = character(),
    input_index = integer(),
    status = character(),
    match_index = integer(),
    longitude = double(),
    latitude = double(),
    comid = character(),
    mainstem_uri = character(),
    mapping_release = character(),
    match_source = character(),
    mainstem_status = character(),
    replacement_uris = list(),
    mainstem_observed_at = as.POSIXct(character(), tz = "UTC"),
    mainstem_retrieval_mode = character(),
    diagnostics = list()
  )
}

gx_point_row_diagnostics <- function(status, input_index,
                                     source = NA_character_,
                                     mainstem_status = if (
                                       identical(status, "not_found")
                                     ) {
                                       NA_character_
                                     } else {
                                       "active_in_mapping_release"
                                     }) {
  path <- paste0("/inputs/", input_index - 1L)
  if (identical(status, "not_found")) {
    mapped <- identical(source, "pinned_comid_mapping")
    return(gx_diagnostic(
      "warning",
      if (mapped) "not_found_in_mapping_release" else "point_not_found",
      path,
      if (mapped) {
        "The NLDI COMID is absent from the pinned mainstem mapping release."
      } else {
        "NLDI did not find an indexed NHDPlusV2 catchment for the point."
      }
    ))
  }
  diagnostics <- gx_crosswalk_currentness_diagnostic(
    mainstem_status,
    path,
    "The point match is scoped to the pinned mapping release; current service state was not checked."
  )
  if (identical(status, "ambiguous")) {
    diagnostics <- gx_bind_diagnostics(
      gx_diagnostic(
        "warning",
        "multiple_mapping_matches",
        path,
        "The NLDI COMID has multiple mainstem matches in the mapping release."
      ),
      diagnostics
    )
  }
  diagnostics
}

gx_point_parse_feature <- function(response) {
  value <- tryCatch(
    gx_ref_json(response, "NLDI position feature"),
    error = function(cnd) {
      gx_abort(
        "NLDI returned an invalid position GeoJSON payload.",
        "gx_error_crosswalk_nldi_payload"
      )
    }
  )
  if (!identical(value$type, "FeatureCollection") ||
      !is.list(value$features) || length(value$features) != 1L ||
      !is.list(value$features[[1L]])) {
    gx_abort(
      "NLDI position lookup must return exactly one feature.",
      "gx_error_crosswalk_nldi_payload"
    )
  }
  feature <- value$features[[1L]]
  properties <- feature$properties
  geometry <- feature$geometry
  if (!is.list(properties) || !is.list(geometry) ||
      !identical(feature$type, "Feature") ||
      !identical(geometry$type, "LineString") ||
      !identical(gx_huc12_scalar(properties$source), "comid")) {
    gx_abort(
      "NLDI position source or feature geometry changed.",
      "gx_error_crosswalk_nldi_contract"
    )
  }
  identities <- c(
    gx_huc12_scalar(feature$id),
    gx_huc12_scalar(properties$identifier),
    gx_huc12_scalar(properties$comid)
  )
  if (anyNA(identities) || length(unique(identities)) != 1L ||
      !grepl("^[1-9][0-9]{0,9}\\z", identities[[1L]], perl = TRUE)) {
    gx_abort(
      "NLDI position COMID identities did not agree.",
      "gx_error_crosswalk_nldi_contract"
    )
  }
  line <- geometry$coordinates
  valid_line <- is.list(line) && length(line) >= 2L && all(vapply(line, function(item) {
    coordinates <- unlist(item, recursive = TRUE, use.names = FALSE)
    is.numeric(coordinates) && length(coordinates) == 2L &&
      all(is.finite(coordinates)) && abs(coordinates[[1L]]) <= 180 &&
      abs(coordinates[[2L]]) <= 90
  }, logical(1)))
  if (!valid_line) {
    gx_abort(
      "NLDI position flowline coordinates are invalid.",
      "gx_error_crosswalk_nldi_contract"
    )
  }
  identities[[1L]]
}

gx_point_attempt_control <- function(state, max_requests, max_total_bytes) {
  list(
    before = function(request, physical) {
      if (nrow(state$requests) >= max_requests) {
        gx_abort(
          "The point crosswalk exhausted its request budget.",
          "gx_error_crosswalk_budget",
          requests = state$requests
        )
      }
      remaining <- max_total_bytes - sum(as.double(state$requests$bytes))
      if (!is.finite(remaining) || remaining < 1) {
        gx_abort(
          "The point crosswalk exhausted its response-byte budget.",
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

gx_point_fetch_one <- function(wkt, client, state, max_requests,
                               max_total_bytes) {
  url <- httr2::url_modify_query(
    paste0(sub("/+$", "", client$base_url), "/comid/position"),
    f = "json",
    coords = wkt
  )
  response <- tryCatch(
    gx_http_request(
      client,
      method = "GET",
      url = url,
      headers = list(Accept = "application/geo+json, application/json;q=0.9"),
      check_status = FALSE,
      .attempt_control = gx_point_attempt_control(
        state, max_requests, max_total_bytes
      )
    ),
    error = function(cnd) {
      if (inherits(cnd, "gx_error")) cnd$requests <- state$requests
      stop(cnd)
    }
  )
  if (identical(response$status, 404L)) return(NA_character_)
  if (response$status < 200L || response$status >= 300L) {
    gx_abort(
      "NLDI position lookup failed with HTTP status {response$status}.",
      "gx_error_crosswalk_nldi_http",
      status = response$status,
      requests = state$requests
    )
  }
  tryCatch(
    gx_point_parse_feature(response),
    error = function(cnd) {
      if (inherits(cnd, "gx_error")) cnd$requests <- state$requests
      stop(cnd)
    }
  )
}

gx_point_metadata <- function(
    x,
    point_count,
    unique_count,
    requests,
    client,
    mapping = NULL,
    currentness_policy = "not_checked",
    currentness_collection = NA_character_,
    currentness_dataset_vintage = NA_character_) {
  statuses <- if (point_count) {
    vapply(seq_len(point_count), function(index) {
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
    operation = "point_to_mainstem",
    currentness_policy = currentness_policy,
    currentness_collection = currentness_collection,
    currentness_dataset_vintage = currentness_dataset_vintage,
    input_count = as.integer(point_count),
    unique_input_count = as.integer(unique_count),
    matched_input_count = as.integer(sum(statuses == "matched")),
    match_count = as.integer(sum(x$status != "not_found")),
    not_found_input_count = as.integer(sum(statuses == "not_found")),
    ambiguous_input_count = as.integer(sum(statuses == "ambiguous")),
    complete = TRUE,
    retrieved_at = if (nrow(requests)) max(requests$retrieved_at) else
      as.POSIXct(NA, tz = "UTC"),
    requests = requests,
    diagnostics = diagnostics,
    nldi = list(source = "comid/position", endpoint = client$base_url),
    mapping = mapping
  )
}

gx_validate_point_crosswalk <- function(x, metadata = attr(x, "gx_crosswalk")) {
  expected <- names(gx_empty_point_crosswalk())
  diagnostic_names <- names(gx_empty_diagnostics())
  metadata_expected <- c(
    "contract_version", "operation", "currentness_policy",
    "currentness_collection", "currentness_dataset_vintage", "input_count",
    "unique_input_count", "matched_input_count", "match_count",
    "not_found_input_count", "ambiguous_input_count", "complete",
    "retrieved_at", "requests", "diagnostics", "nldi", "mapping"
  )
  count_names <- c(
    "input_count", "unique_input_count", "matched_input_count", "match_count",
    "not_found_input_count", "ambiguous_input_count"
  )
  valid <- is.data.frame(x) && identical(names(x), expected) &&
    is.character(x$contract_version) &&
    all(x$contract_version == .gx_crosswalk_contract_version) &&
    is.integer(x$input_index) && !anyNA(x$input_index) &&
    all(x$input_index >= 1L) && is.character(x$status) &&
    all(x$status %in% c("matched", "ambiguous", "not_found")) &&
    is.integer(x$match_index) && is.double(x$longitude) &&
    is.double(x$latitude) && is.character(x$comid) &&
    is.character(x$mainstem_uri) && is.character(x$mapping_release) &&
    is.character(x$match_source) && is.character(x$mainstem_status) &&
    is.list(x$replacement_uris) &&
    inherits(x$mainstem_observed_at, "POSIXct") &&
    is.character(x$mainstem_retrieval_mode) &&
    is.list(x$diagnostics) &&
    all(vapply(x$diagnostics, function(item) {
      is.data.frame(item) && identical(names(item), diagnostic_names)
    }, logical(1))) &&
    is.list(metadata) && identical(names(metadata), metadata_expected) &&
    identical(metadata$contract_version, .gx_crosswalk_contract_version) &&
    identical(metadata$operation, "point_to_mainstem") &&
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
    identical(metadata$nldi$source, "comid/position") &&
    is.character(metadata$nldi$endpoint) && length(metadata$nldi$endpoint) == 1L
  if (!isTRUE(valid)) {
    gx_abort(
      "Point crosswalk output does not satisfy its contract.",
      "gx_error_crosswalk_contract"
    )
  }
  if (nrow(x)) {
    found <- x$status != "not_found"
    mapped <- !is.na(x$match_source) &
      x$match_source == "pinned_comid_mapping"
    nldi_missing <- !mapped & !found
    allowed_mainstem_status <- if (identical(
      metadata$currentness_policy,
      "not_checked"
    )) {
      "active_in_mapping_release"
    } else {
      c("current", "superseded", "superseded_unresolved")
    }
    rows_ok <- all(is.finite(x$longitude)) && all(is.finite(x$latitude)) &&
      all(x$longitude >= -180 & x$longitude <= 180) &&
      all(x$latitude >= -90 & x$latitude <= 90) &&
      all(mapped | nldi_missing) &&
      all(is.na(x$match_index[!found])) && all(is.na(x$comid[nldi_missing])) &&
      all(is.na(x$mapping_release[nldi_missing])) &&
      all(grepl("^[1-9][0-9]{0,9}\\z", x$comid[mapped], perl = TRUE)) &&
      all(!is.na(x$mapping_release[mapped])) &&
      all(is.na(x$mainstem_uri[!found])) &&
      all(!is.na(x$match_index[found]) & x$match_index[found] >= 1L) &&
      all(grepl("^[1-9][0-9]{0,9}\\z", x$comid[found], perl = TRUE)) &&
      all(vapply(
        x$mainstem_uri[found], gx_crosswalk_valid_mainstem_uri,
        logical(1), allow_na = FALSE
      )) && all(x$mainstem_status[found] %in% allowed_mainstem_status)
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
        gx_point_row_diagnostics(
          x$status[[row]], x$input_index[[row]], x$match_source[[row]],
          x$mainstem_status[[row]]
        )
      )
    }, logical(1)))
    if (!rows_ok || !currentness_rows_ok || !diagnostics_ok) {
      gx_abort(
        "Point crosswalk rows do not satisfy their identity contract.",
        "gx_error_crosswalk_contract"
      )
    }
  }
  input_status <- if (metadata$input_count) {
    vapply(seq_len(metadata$input_count), function(index) {
      values <- unique(x$status[x$input_index == index])
      if (length(values) == 1L) values else NA_character_
    }, character(1))
  } else {
    character()
  }
  coordinate_keys <- if (nrow(x)) {
    paste(
      vapply(x$longitude, gx_edr_number_text_impl, character(1)),
      vapply(x$latitude, gx_edr_number_text_impl, character(1)),
      sep = ","
    )
  } else {
    character()
  }
  reconciled <- metadata$input_count == length(unique(x$input_index)) &&
    identical(unique(x$input_index), seq_len(metadata$input_count)) &&
    metadata$unique_input_count == length(unique(coordinate_keys)) &&
    metadata$matched_input_count == sum(input_status == "matched") &&
    metadata$not_found_input_count == sum(input_status == "not_found") &&
    metadata$ambiguous_input_count == sum(input_status == "ambiguous") &&
    metadata$match_count == sum(x$status != "not_found")
  if (!isTRUE(reconciled)) {
    gx_abort(
      "Point crosswalk metadata does not reconcile with its rows.",
      "gx_error_crosswalk_contract"
    )
  }
  expected_diagnostics <- if (nrow(x)) {
    do.call(gx_bind_diagnostics, c(list(gx_empty_diagnostics()), x$diagnostics))
  } else {
    gx_empty_diagnostics()
  }
  if (!identical(metadata$diagnostics, expected_diagnostics)) {
    gx_abort(
      "Point crosswalk metadata diagnostics do not match its rows.",
      "gx_error_crosswalk_contract"
    )
  }
  used_mapping <- any(
    !is.na(x$match_source) & x$match_source == "pinned_comid_mapping"
  )
  if (used_mapping) {
    gx_validate_comid_mapping_metadata(metadata$mapping)
    mapped_rows <- !is.na(x$match_source) &
      x$match_source == "pinned_comid_mapping"
    if (any(x$mapping_release[mapped_rows] != metadata$mapping$release)) {
      gx_abort(
        "Point crosswalk rows do not match their mapping release.",
        "gx_error_crosswalk_contract"
      )
    }
  } else if (!is.null(metadata$mapping)) {
    gx_abort(
      "Point not-found results must not claim mapping provenance.",
      "gx_error_crosswalk_contract"
    )
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
      "Point crosswalk retrieval time does not reconcile with its ledger.",
      "gx_error_crosswalk_contract"
    )
  }
  invisible(x)
}

gx_new_point_crosswalk <- function(x, metadata) {
  gx_validate_point_crosswalk(x, metadata)
  attr(x, "gx_crosswalk") <- metadata
  class(x) <- unique(c("gx_point_crosswalk", "gx_crosswalk", class(x)))
  x
}

gx_point_compose_currentness <- function(
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
    gx_point_row_diagnostics(
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
  gx_new_point_crosswalk(out, metadata)
}

#' Map Points to release-scoped mainstem PIDs
#'
#' Transforms Point geometries to OGC CRS84 with PROJ networking disabled,
#' retrieves containing NHDPlusV2 COMIDs from the USGS NLDI position route,
#' then maps those COMIDs through the explicitly installed checksum-pinned
#' lookup. Lookup data is never installed or refreshed implicitly. With
#' `check = TRUE`, matched PIDs are checked against live `mainstems_v3` while
#' retaining every advertised replacement.
#'
#' @param points An `sf` or `sfc` object containing nonempty two-dimensional
#'   Point geometries with a declared CRS.
#' @param check Whether to compose mapping matches with bounded live
#'   `mainstems_v3` currentness.
#' @param version Registered mapping release.
#' @param data_dir Package data directory containing an explicitly installed
#'   lookup.
#' @param client An NLDI client created by [gx_client()].
#' @param currentness_client A reference client used when `check = TRUE`, or
#'   `NULL` to construct the default.
#'
#' @return A `gx_point_crosswalk` tibble. Its `gx_crosswalk` attribute records
#'   NLDI requests, mapping provenance, counts, and diagnostics.
#' @export
gx_point_to_mainstem <- function(
    points,
    check = FALSE,
    version = "v3.2",
    data_dir = gx_default_data_dir(),
    client = gx_client("nldi"),
    currentness_client = NULL) {
  check <- gx_crosswalk_check(check)
  gx_huc12_client(client)
  normalized <- gx_crosswalk_points(points)
  max_requests <- gx_crosswalk_max_requests()
  max_total_bytes <- gx_crosswalk_total_bytes(client)
  if (check && is.null(currentness_client)) {
    currentness_client <- gx_client("reference")
  }
  if (!length(normalized$wkt)) {
    out <- gx_empty_point_crosswalk()
    metadata <- gx_point_metadata(
      out, 0L, 0L, gx_crosswalk_empty_requests(), client
    )
    out <- gx_new_point_crosswalk(out, metadata)
    if (!check) return(out)
    return(gx_point_compose_currentness(
      out, metadata, currentness_client, max_requests, max_total_bytes
    ))
  }
  state <- new.env(parent = emptyenv())
  state$requests <- gx_crosswalk_empty_requests()
  unique_wkt <- unique(normalized$wkt)
  comids <- stats::setNames(vapply(unique_wkt, function(wkt) {
    gx_point_fetch_one(
      wkt, client, state, max_requests = max_requests,
      max_total_bytes = max_total_bytes
    )
  }, character(1)), unique_wkt)
  found_comids <- unique(unname(comids[!is.na(comids)]))
  mapped <- NULL
  mapping <- NULL
  if (length(found_comids)) {
    mapped <- gx_comid_to_mainstem_impl(
      found_comids, version = version, data_dir = data_dir
    )
    mapping <- attr(mapped, "gx_crosswalk")$mapping
  }
  rows <- list()
  for (input_index in seq_along(normalized$wkt)) {
    comid <- unname(comids[[normalized$wkt[[input_index]]]])
    if (is.na(comid)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        status = "not_found",
        match_index = NA_integer_,
        longitude = normalized$longitude[[input_index]],
        latitude = normalized$latitude[[input_index]],
        comid = NA_character_,
        mainstem_uri = NA_character_,
        mapping_release = NA_character_,
        match_source = NA_character_,
        mainstem_status = NA_character_,
        replacement_uris = list(character()),
        mainstem_observed_at = as.POSIXct(NA, tz = "UTC"),
        mainstem_retrieval_mode = NA_character_,
        diagnostics = list(gx_point_row_diagnostics(
          "not_found", input_index
        ))
      )
      next
    }
    matches <- mapped[mapped$requested_comid == comid, , drop = FALSE]
    for (row_index in seq_len(nrow(matches))) {
      status <- matches$status[[row_index]]
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        status = status,
        match_index = matches$match_index[[row_index]],
        longitude = normalized$longitude[[input_index]],
        latitude = normalized$latitude[[input_index]],
        comid = comid,
        mainstem_uri = matches$mainstem_uri[[row_index]],
        mapping_release = matches$mapping_release[[row_index]],
        match_source = "pinned_comid_mapping",
        mainstem_status = matches$mainstem_status[[row_index]],
        replacement_uris = list(character()),
        mainstem_observed_at = as.POSIXct(NA, tz = "UTC"),
        mainstem_retrieval_mode = NA_character_,
        diagnostics = list(gx_point_row_diagnostics(
          status, input_index, "pinned_comid_mapping"
        ))
      )
    }
  }
  out <- tibble::as_tibble(do.call(rbind, rows))
  metadata <- gx_point_metadata(
    out,
    length(normalized$wkt),
    length(unique_wkt),
    state$requests,
    client,
    mapping = mapping
  )
  out <- gx_new_point_crosswalk(out, metadata)
  if (!check) return(out)
  gx_point_compose_currentness(
    out, metadata, currentness_client, max_requests, max_total_bytes
  )
}
