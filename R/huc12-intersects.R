.gx_huc12_reference_collection <- "hu12"

gx_huc12_intersection_client <- function(client) {
  if (!inherits(client, "gx_client") ||
      !identical(client$endpoint, "reference")) {
    gx_abort(
      paste(
        "{.arg client} must be a reference client created by",
        "{.fn gx_client} for {.code method = \"intersects\"}."
      ),
      "gx_error_crosswalk_input"
    )
  }
  client
}

gx_huc12_intersection_assert_budgets <- function(
    requests,
    candidates = 0L,
    matches = 0L,
    rows = 0L,
    max_requests,
    max_candidates,
    max_matches,
    max_rows,
    max_total_bytes) {
  exceeded <- nrow(requests) > max_requests ||
    sum(as.double(requests$bytes)) > max_total_bytes ||
    candidates > max_candidates || matches > max_matches || rows > max_rows
  if (isTRUE(exceeded)) {
    gx_abort(
      "The HUC12 intersection workflow exceeded an aggregate budget.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }
  invisible(requests)
}

gx_huc12_intersection_limits <- function(
    requests,
    client,
    max_requests,
    max_total_bytes) {
  remaining_requests <- max_requests - nrow(requests)
  remaining_bytes <- floor(
    max_total_bytes - sum(as.double(requests$bytes))
  )
  if (remaining_requests < 1L || !is.finite(remaining_bytes) ||
      remaining_bytes < 1) {
    gx_abort(
      "The HUC12 intersection workflow exhausted its aggregate budget.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }
  call_client <- client
  call_client$retries <- 0L
  call_client$max_bytes <- as.integer(min(
    as.double(client$max_bytes),
    remaining_bytes
  ))
  list(
    client = call_client,
    max_pages = as.integer(remaining_requests),
    max_requests = as.integer(remaining_requests),
    max_total_bytes = as.integer(remaining_bytes)
  )
}

gx_huc12_intersection_rethrow <- function(cnd, requests) {
  prior <- gx_crosswalk_condition_requests(cnd)
  if (nrow(prior)) requests <- gx_crosswalk_merge_requests(requests, prior)
  budget_kind <- as.character(cnd$budget_kind %||% NA_character_)
  if (inherits(cnd, c(
    "gx_error_payload_too_large",
    "gx_error_reference_budget"
  )) || budget_kind %in% c("requests", "bytes")) {
    gx_abort(
      "The HUC12 intersection workflow exhausted its aggregate budget.",
      "gx_error_crosswalk_budget",
      requests = requests
    )
  }
  if (inherits(cnd, "gx_error")) cnd$requests <- requests
  stop(cnd)
}

gx_huc12_intersection_queryables <- function(collection, client, requests,
                                              max_requests,
                                              max_total_bytes) {
  limits <- gx_huc12_intersection_limits(
    requests, client, max_requests, max_total_bytes
  )
  tryCatch(
    gx_ref_queryables_impl(
      collection,
      client = limits$client,
      .max_requests = limits$max_requests,
      .max_total_bytes = limits$max_total_bytes
    ),
    error = function(cnd) gx_huc12_intersection_rethrow(cnd, requests)
  )
}

gx_huc12_intersection_validate_queryables <- function(
    huc_queryables,
    mainstem_queryables,
    requests) {
  huc_required <- c(uri = "string", huc12 = "string")
  huc_ok <- identical(gx_ref_identity_queryables(huc_queryables), "huc12") &&
    all(vapply(names(huc_required), function(name) {
      identical(
        gx_crosswalk_queryable_types(huc_queryables, name),
        huc_required[[name]]
      )
    }, logical(1)))
  mainstem_required <- c(
    uri = "string",
    outlet_drainagearea_sqkm = "number",
    outlet_nhdpv2huc12 = "string",
    superseded = "boolean",
    new_mainstemid = "string"
  )
  mainstem_ok <- identical(
    gx_ref_identity_queryables(mainstem_queryables), "id"
  ) && all(vapply(names(mainstem_required), function(name) {
    identical(
      gx_crosswalk_queryable_types(mainstem_queryables, name),
      mainstem_required[[name]]
    )
  }, logical(1)))
  if (!huc_ok || !mainstem_ok) {
    gx_abort(
      paste(
        "The reference collections do not advertise the identity, migration,",
        "and ranking properties required for HUC12 intersections."
      ),
      "gx_error_crosswalk_contract",
      requests = requests
    )
  }
  invisible(TRUE)
}

gx_huc12_intersection_fetch_huc <- function(
    huc12,
    queryables,
    client,
    requests,
    max_requests,
    max_total_bytes) {
  limits <- gx_huc12_intersection_limits(
    requests, client, max_requests, max_total_bytes
  )
  tryCatch(
    gx_ref_features_impl(
      .gx_huc12_reference_collection,
      query = list(huc12 = huc12),
      limit = 2L,
      client = limits$client,
      .queryables = queryables,
      .max_pages = limits$max_pages,
      .max_requests = limits$max_requests,
      .max_total_bytes = limits$max_total_bytes
    ),
    error = function(cnd) gx_huc12_intersection_rethrow(cnd, requests)
  )
}

gx_huc12_intersection_fetch_candidates <- function(
    bbox,
    queryables,
    limit,
    client,
    requests,
    max_requests,
    max_total_bytes) {
  limits <- gx_huc12_intersection_limits(
    requests, client, max_requests, max_total_bytes
  )
  tryCatch(
    gx_ref_features_impl(
      .gx_mainstem_collection,
      bbox = bbox,
      limit = limit,
      client = limits$client,
      .queryables = queryables,
      .max_pages = limits$max_pages,
      .max_requests = limits$max_requests,
      .max_total_bytes = limits$max_total_bytes
    ),
    error = function(cnd) gx_huc12_intersection_rethrow(cnd, requests)
  )
}

gx_huc12_reference_uri <- function(huc12) {
  paste0("https://geoconnex.us/ref/huc12/", huc12)
}

gx_huc12_intersection_validate_huc <- function(x, requested, requests) {
  metadata <- attr(x, "gx_reference")
  complete <- is.list(metadata) && isTRUE(metadata$complete) &&
    !isTRUE(metadata$truncated)
  if (!complete || nrow(x) > 1L) {
    gx_abort(
      "The reference HUC12 lookup was incomplete or ambiguous.",
      "gx_error_crosswalk_incomplete",
      requests = requests
    )
  }
  if (!nrow(x)) return(invisible(x))
  required <- c("feature_id", "huc12", "uri")
  geometry <- sf::st_geometry(x)
  valid <- all(required %in% names(x)) && is.character(x$feature_id) &&
    is.character(x$huc12) && is.character(x$uri) &&
    identical(x$feature_id[[1L]], requested) &&
    identical(x$huc12[[1L]], requested) &&
    identical(x$uri[[1L]], gx_huc12_reference_uri(requested)) &&
    inherits(x, "sf") && !sf::st_is_empty(geometry)[[1L]] &&
    isTRUE(sf::st_is_valid(geometry)[[1L]]) &&
    as.character(sf::st_geometry_type(geometry)[[1L]]) %in%
      c("POLYGON", "MULTIPOLYGON") && isTRUE(sf::st_is_longlat(x))
  if (!isTRUE(valid)) {
    gx_abort(
      "The reference HUC12 identity or polygon did not match the request.",
      "gx_error_crosswalk_identity",
      requests = requests
    )
  }
  invisible(x)
}

gx_huc12_outlet_id <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return(NA_character_)
  }
  if (!nzchar(value)) return("")
  matched <- regexec(
    "^https://geoconnex[.]us/nhdplusv2/huc12/([0-9]{12})\\z",
    value,
    perl = TRUE
  )
  pieces <- regmatches(value, matched)[[1L]]
  if (length(pieces) == 2L) pieces[[2L]] else NA_character_
}

gx_huc12_intersection_validate_candidates <- function(x, requests) {
  metadata <- attr(x, "gx_reference")
  if (!is.list(metadata) || !isTRUE(metadata$complete) ||
      isTRUE(metadata$truncated)) {
    gx_abort(
      "The mainstem bounding-box candidate set was incomplete.",
      "gx_error_crosswalk_incomplete",
      requests = requests
    )
  }
  if (!nrow(x)) return(invisible(x))
  required <- c(
    "feature_id", "id", "uri", "outlet_drainagearea_sqkm",
    "outlet_nhdpv2huc12", "superseded", "new_mainstemid"
  )
  geometry <- sf::st_geometry(x)
  types <- as.character(sf::st_geometry_type(geometry))
  valid <- all(required %in% names(x)) && is.character(x$feature_id) &&
    is.character(x$id) && is.character(x$uri) &&
    is.numeric(x$outlet_drainagearea_sqkm) &&
    is.character(x$outlet_nhdpv2huc12) && is.logical(x$superseded) &&
    is.character(x$new_mainstemid) && !anyNA(x$feature_id) &&
    !anyNA(x$id) && !anyNA(x$uri) &&
    !anyNA(x$outlet_drainagearea_sqkm) &&
    all(is.finite(x$outlet_drainagearea_sqkm)) &&
    all(x$outlet_drainagearea_sqkm >= 0) &&
    !anyNA(x$outlet_nhdpv2huc12) && !anyNA(x$superseded) &&
    !anyNA(x$new_mainstemid) && all(x$feature_id == x$id) &&
    !anyDuplicated(x$feature_id) && !anyDuplicated(x$uri) &&
    all(vapply(x$uri, gx_crosswalk_valid_mainstem_uri, logical(1),
      allow_na = FALSE)) &&
    all(vapply(seq_len(nrow(x)), function(index) {
      identical(
        gx_mainstem_uri_id(x$uri[[index]]),
        x$feature_id[[index]]
      )
    }, logical(1))) && all(!sf::st_is_empty(geometry)) &&
    all(sf::st_is_valid(geometry)) &&
    all(types %in% c("LINESTRING", "MULTILINESTRING")) &&
    isTRUE(sf::st_is_longlat(x))
  if (!isTRUE(valid)) {
    gx_abort(
      "A mainstem candidate violated the intersection identity contract.",
      "gx_error_crosswalk_identity",
      requests = requests
    )
  }
  outlet_ids <- vapply(
    x$outlet_nhdpv2huc12, gx_huc12_outlet_id, character(1)
  )
  if (anyNA(outlet_ids)) {
    gx_abort(
      "A mainstem candidate advertised an invalid outlet HUC12 URI.",
      "gx_error_crosswalk_contract",
      requests = requests
    )
  }
  replacements <- lapply(x$new_mainstemid, gx_mainstem_replacements)
  inconsistent <- vapply(seq_len(nrow(x)), function(index) {
    !isTRUE(x$superseded[[index]]) && length(replacements[[index]]) > 0L
  }, logical(1))
  if (any(inconsistent)) {
    gx_abort(
      "A current mainstem candidate unexpectedly advertised replacements.",
      "gx_error_crosswalk_contract",
      requests = requests
    )
  }
  invisible(x)
}

gx_huc12_intersection_status <- function(superseded, replacements) {
  if (!isTRUE(superseded)) return("current")
  if (length(replacements)) "superseded" else "superseded_unresolved"
}

gx_huc12_intersection_rank <- function(huc, candidates, requested, requests) {
  if (!nrow(candidates)) return(tibble::tibble())
  if (!identical(sf::st_crs(huc), sf::st_crs(candidates))) {
    gx_abort(
      "HUC12 and mainstem candidate CRS values do not match.",
      "gx_error_crosswalk_contract",
      requests = requests
    )
  }
  prior_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(prior_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(TRUE))
  hits <- which(lengths(sf::st_intersects(candidates, huc)) > 0L)
  if (!length(hits)) return(tibble::tibble())
  intersection_km <- vapply(hits, function(index) {
    clipped <- suppressWarnings(sf::st_intersection(
      sf::st_geometry(candidates[index, , drop = FALSE]),
      sf::st_geometry(huc)
    ))
    sum(as.numeric(sf::st_length(clipped))) / 1000
  }, numeric(1))
  if (any(!is.finite(intersection_km)) || any(intersection_km < 0)) {
    gx_abort(
      "A mainstem intersection produced an invalid geodesic length.",
      "gx_error_crosswalk_contract",
      requests = requests
    )
  }
  replacements <- lapply(
    candidates$new_mainstemid[hits], gx_mainstem_replacements
  )
  statuses <- vapply(seq_along(hits), function(index) {
    gx_huc12_intersection_status(
      candidates$superseded[[hits[[index]]]],
      replacements[[index]]
    )
  }, character(1))
  outlet_ids <- vapply(
    candidates$outlet_nhdpv2huc12[hits],
    gx_huc12_outlet_id,
    character(1)
  )
  out <- tibble::tibble(
    mainstem_uri = as.character(candidates$uri[hits]),
    mainstem_status = statuses,
    replacement_uris = replacements,
    outlet_huc12_match = outlet_ids == requested,
    intersection_length_km = intersection_km,
    outlet_drainagearea_sqkm = as.double(
      candidates$outlet_drainagearea_sqkm[hits]
    )
  )
  current_tier <- ifelse(out$mainstem_status == "current", 0L, 1L)
  order_index <- order(
    current_tier,
    !out$outlet_huc12_match,
    -out$intersection_length_km,
    -out$outlet_drainagearea_sqkm,
    out$mainstem_uri,
    method = "radix"
  )
  out[order_index, , drop = FALSE]
}

gx_empty_huc12_intersections <- function() {
  tibble::tibble(
    contract_version = character(),
    input_index = integer(),
    requested_huc12 = character(),
    status = character(),
    match_index = integer(),
    huc12 = character(),
    mainstem_uri = character(),
    mainstem_status = character(),
    replacement_uris = list(),
    outlet_huc12_match = logical(),
    intersection_length_km = double(),
    outlet_drainagearea_sqkm = double(),
    match_source = character(),
    diagnostics = list()
  )
}

gx_huc12_intersection_diagnostics <- function(
    status,
    input_index,
    mainstem_status = NA_character_,
    huc_found = TRUE) {
  path <- paste0("/inputs/", input_index - 1L)
  if (identical(status, "not_found")) {
    return(gx_diagnostic(
      "warning",
      if (isTRUE(huc_found)) "no_mainstem_intersections" else "huc12_not_found",
      path,
      if (isTRUE(huc_found)) {
        "No complete mainstems_v3 geometry intersected the reference HUC12 polygon."
      } else {
        "The HUC12 is absent from the reference hu12 collection."
      }
    ))
  }
  diagnostics <- gx_diagnostic(
    "info",
    "ranked_intersection_not_selected",
    path,
    "The row is ranked by disclosed metrics; no mainstem was selected."
  )
  if (mainstem_status %in% c("superseded", "superseded_unresolved")) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic(
        "warning",
        "superseded_mainstem",
        path,
        "The intersecting mainstem is marked superseded in mainstems_v3."
      )
    )
  }
  diagnostics
}

gx_huc12_intersection_metadata <- function(
    x,
    huc12,
    requests,
    candidate_count,
    unique_intersection_count,
    reference_diagnostics = gx_empty_diagnostics()) {
  statuses <- if (length(huc12)) {
    vapply(seq_along(huc12), function(index) {
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
    operation = "huc12_to_mainstem",
    method = "intersects",
    currentness_policy = "live_v3_observed",
    input_count = as.integer(length(huc12)),
    unique_input_count = as.integer(length(unique(huc12))),
    matched_input_count = as.integer(sum(statuses == "matched")),
    match_count = as.integer(sum(x$status == "matched")),
    not_found_input_count = as.integer(sum(statuses == "not_found")),
    ambiguous_input_count = 0L,
    candidate_count = as.integer(candidate_count),
    unique_intersection_count = as.integer(unique_intersection_count),
    complete = TRUE,
    retrieved_at = if (nrow(requests)) {
      max(requests$retrieved_at)
    } else {
      as.POSIXct(NA, tz = "UTC")
    },
    requests = requests,
    diagnostics = gx_bind_diagnostics(reference_diagnostics, row_diagnostics),
    collections = list(
      huc12 = .gx_huc12_reference_collection,
      mainstem = .gx_mainstem_collection,
      dataset_vintage = .gx_mainstem_dataset_vintage
    ),
    ranking = c(
      "current_before_superseded",
      "outlet_huc12_match_desc",
      "intersection_length_km_desc",
      "outlet_drainagearea_sqkm_desc",
      "mainstem_uri_bytewise"
    ),
    geometry_runtime = list(
      engine = "s2",
      enabled = TRUE,
      length_unit = "km",
      sf_version = as.character(utils::packageVersion("sf")),
      s2_version = as.character(utils::packageVersion("s2"))
    )
  )
}

gx_validate_huc12_intersections <- function(
    x,
    metadata = attr(x, "gx_crosswalk")) {
  expected <- names(gx_empty_huc12_intersections())
  metadata_expected <- c(
    "contract_version", "operation", "method", "currentness_policy",
    "input_count", "unique_input_count", "matched_input_count",
    "match_count", "not_found_input_count", "ambiguous_input_count",
    "candidate_count", "unique_intersection_count", "complete",
    "retrieved_at", "requests", "diagnostics", "collections", "ranking",
    "geometry_runtime"
  )
  diagnostic_names <- names(gx_empty_diagnostics())
  valid <- is.data.frame(x) && identical(names(x), expected) &&
    is.character(x$contract_version) &&
    all(x$contract_version == .gx_crosswalk_contract_version) &&
    is.integer(x$input_index) && !anyNA(x$input_index) &&
    all(x$input_index >= 1L) && is.character(x$requested_huc12) &&
    all(grepl("^[0-9]{12}\\z", x$requested_huc12, perl = TRUE)) &&
    is.character(x$status) && all(x$status %in% c("matched", "not_found")) &&
    is.integer(x$match_index) && is.character(x$huc12) &&
    is.character(x$mainstem_uri) && is.character(x$mainstem_status) &&
    is.list(x$replacement_uris) && is.logical(x$outlet_huc12_match) &&
    is.double(x$intersection_length_km) &&
    is.double(x$outlet_drainagearea_sqkm) &&
    is.character(x$match_source) && is.list(x$diagnostics) &&
    all(vapply(x$diagnostics, function(item) {
      is.data.frame(item) && identical(names(item), diagnostic_names)
    }, logical(1))) && is.list(metadata) &&
    identical(names(metadata), metadata_expected) &&
    identical(metadata$contract_version, .gx_crosswalk_contract_version) &&
    identical(metadata$operation, "huc12_to_mainstem") &&
    identical(metadata$method, "intersects") &&
    identical(metadata$currentness_policy, "live_v3_observed") &&
    isTRUE(metadata$complete) && is.data.frame(metadata$requests) &&
    identical(names(metadata$requests), names(gx_crosswalk_empty_requests()))
  if (!isTRUE(valid)) {
    gx_abort(
      "HUC12 intersection output does not satisfy its contract.",
      "gx_error_crosswalk_contract"
    )
  }
  if (nrow(x)) {
    found <- x$status == "matched"
    missing <- !found
    found_ok <- all(!is.na(x$match_index[found])) &&
      all(x$match_index[found] >= 1L) &&
      all(x$huc12[found] == x$requested_huc12[found]) &&
      all(vapply(x$mainstem_uri[found], gx_crosswalk_valid_mainstem_uri,
        logical(1), allow_na = FALSE)) &&
      all(x$mainstem_status[found] %in% c(
        "current", "superseded", "superseded_unresolved"
      )) && all(!is.na(x$outlet_huc12_match[found])) &&
      all(is.finite(x$intersection_length_km[found])) &&
      all(x$intersection_length_km[found] >= 0) &&
      all(is.finite(x$outlet_drainagearea_sqkm[found])) &&
      all(x$outlet_drainagearea_sqkm[found] >= 0) &&
      all(x$match_source[found] == "reference_geometry_intersection")
    missing_ok <- all(is.na(x$match_index[missing])) &&
      all(is.na(x$mainstem_uri[missing])) &&
      all(is.na(x$mainstem_status[missing])) &&
      all(vapply(x$replacement_uris[missing], length, integer(1)) == 0L) &&
      all(is.na(x$outlet_huc12_match[missing])) &&
      all(is.na(x$intersection_length_km[missing])) &&
      all(is.na(x$outlet_drainagearea_sqkm[missing]))
    replacements_ok <- all(vapply(seq_len(nrow(x))[found], function(row) {
      values <- x$replacement_uris[[row]]
      all(vapply(values, gx_crosswalk_valid_mainstem_uri, logical(1),
        allow_na = FALSE)) && !anyDuplicated(values) &&
        if (x$mainstem_status[[row]] == "current") {
          length(values) == 0L
        } else if (x$mainstem_status[[row]] == "superseded") {
          length(values) > 0L
        } else {
          length(values) == 0L
        }
    }, logical(1)))
    groups_ok <- all(vapply(split(seq_len(nrow(x)), x$input_index), function(rows) {
      stable <- length(unique(x$status[rows])) == 1L &&
        length(unique(x$requested_huc12[rows])) == 1L
      if (!stable) return(FALSE)
      if (identical(x$status[[rows[[1L]]]], "not_found")) {
        return(length(rows) == 1L)
      }
      current_tier <- ifelse(
        x$mainstem_status[rows] == "current", 0L, 1L
      )
      ranked <- order(
        current_tier,
        !x$outlet_huc12_match[rows],
        -x$intersection_length_km[rows],
        -x$outlet_drainagearea_sqkm[rows],
        x$mainstem_uri[rows],
        method = "radix"
      )
      identical(x$match_index[rows], seq_along(rows)) &&
        identical(ranked, seq_along(rows)) &&
        !anyDuplicated(x$mainstem_uri[rows])
    }, logical(1)))
    diagnostics_ok <- all(vapply(seq_len(nrow(x)), function(row) {
      identical(
        x$diagnostics[[row]],
        gx_huc12_intersection_diagnostics(
          x$status[[row]],
          x$input_index[[row]],
          x$mainstem_status[[row]],
          !is.na(x$huc12[[row]])
        )
      )
    }, logical(1)))
    if (!found_ok || !missing_ok || !replacements_ok || !groups_ok ||
        !diagnostics_ok || !identical(
          order(x$input_index, method = "radix"), seq_len(nrow(x))
        )) {
      gx_abort(
        "HUC12 intersection rows do not preserve ranking identity.",
        "gx_error_crosswalk_contract"
      )
    }
  }
  count_names <- c(
    "input_count", "unique_input_count", "matched_input_count",
    "match_count", "not_found_input_count", "ambiguous_input_count",
    "candidate_count", "unique_intersection_count"
  )
  metadata_ok <- all(vapply(metadata[count_names], function(value) {
    is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
  }, logical(1))) && identical(metadata$ambiguous_input_count, 0L) &&
    inherits(metadata$retrieved_at, "POSIXct") &&
    length(metadata$retrieved_at) == 1L &&
    is.data.frame(metadata$diagnostics) &&
    identical(names(metadata$diagnostics), diagnostic_names) &&
    identical(metadata$collections, list(
      huc12 = .gx_huc12_reference_collection,
      mainstem = .gx_mainstem_collection,
      dataset_vintage = .gx_mainstem_dataset_vintage
    )) && identical(metadata$ranking, c(
      "current_before_superseded", "outlet_huc12_match_desc",
      "intersection_length_km_desc", "outlet_drainagearea_sqkm_desc",
      "mainstem_uri_bytewise"
    )) && is.list(metadata$geometry_runtime) &&
    identical(metadata$geometry_runtime$engine, "s2") &&
    isTRUE(metadata$geometry_runtime$enabled) &&
    identical(metadata$geometry_runtime$length_unit, "km") &&
    is.character(metadata$geometry_runtime$sf_version) &&
    length(metadata$geometry_runtime$sf_version) == 1L &&
    is.character(metadata$geometry_runtime$s2_version) &&
    length(metadata$geometry_runtime$s2_version) == 1L
  statuses <- if (metadata$input_count) {
    vapply(seq_len(metadata$input_count), function(index) {
      values <- unique(x$status[x$input_index == index])
      if (length(values) == 1L) values else NA_character_
    }, character(1))
  } else {
    character()
  }
  unique_intersections <- if (metadata$input_count) {
    sum(vapply(match(unique(x$requested_huc12), x$requested_huc12), function(index) {
      sum(x$status[x$input_index == x$input_index[[index]]] == "matched")
    }, integer(1)))
  } else {
    0L
  }
  time_ok <- if (nrow(metadata$requests)) {
    !is.na(metadata$retrieved_at) &&
      identical(metadata$retrieved_at, max(metadata$requests$retrieved_at))
  } else {
    is.na(metadata$retrieved_at)
  }
  reconciled <- metadata$input_count == length(unique(x$input_index)) &&
    identical(unique(x$input_index), seq_len(metadata$input_count)) &&
    metadata$unique_input_count == length(unique(x$requested_huc12)) &&
    metadata$matched_input_count == sum(statuses == "matched") &&
    metadata$not_found_input_count == sum(statuses == "not_found") &&
    metadata$match_count == sum(x$status == "matched") &&
    metadata$unique_intersection_count == unique_intersections &&
    metadata$candidate_count >= metadata$unique_intersection_count
  if (!isTRUE(metadata_ok) || !isTRUE(reconciled) || !isTRUE(time_ok)) {
    gx_abort(
      "HUC12 intersection metadata does not reconcile with its rows.",
      "gx_error_crosswalk_contract"
    )
  }
  invisible(x)
}

gx_new_huc12_intersections <- function(x, metadata) {
  gx_validate_huc12_intersections(x, metadata)
  attr(x, "gx_crosswalk") <- metadata
  class(x) <- unique(c(
    "gx_huc12_intersection_crosswalk", "gx_crosswalk", class(x)
  ))
  x
}

gx_huc12_to_mainstem_intersects <- function(huc12, client) {
  gx_huc12_intersection_client(client)
  if (!length(huc12)) {
    out <- gx_empty_huc12_intersections()
    metadata <- gx_huc12_intersection_metadata(
      out,
      huc12,
      gx_crosswalk_empty_requests(),
      candidate_count = 0L,
      unique_intersection_count = 0L
    )
    return(gx_new_huc12_intersections(out, metadata))
  }
  max_requests <- gx_crosswalk_max_requests()
  max_candidates <- gx_crosswalk_max_matches()
  max_matches <- gx_crosswalk_max_matches()
  max_rows <- gx_crosswalk_max_rows()
  max_total_bytes <- gx_crosswalk_total_bytes(client)
  requests <- gx_crosswalk_empty_requests()
  reference_diagnostics <- gx_empty_diagnostics()

  huc_queryables <- gx_huc12_intersection_queryables(
    .gx_huc12_reference_collection,
    client,
    requests,
    max_requests,
    max_total_bytes
  )
  requests <- gx_crosswalk_merge_requests(
    requests, gx_crosswalk_reference_requests(huc_queryables)
  )
  reference_diagnostics <- gx_bind_diagnostics(
    reference_diagnostics,
    gx_crosswalk_prefix_diagnostics(
      attr(huc_queryables, "gx_reference")$diagnostics,
      "/queryables/hu12"
    )
  )
  mainstem_queryables <- gx_huc12_intersection_queryables(
    .gx_mainstem_collection,
    client,
    requests,
    max_requests,
    max_total_bytes
  )
  requests <- gx_crosswalk_merge_requests(
    requests, gx_crosswalk_reference_requests(mainstem_queryables)
  )
  reference_diagnostics <- gx_bind_diagnostics(
    reference_diagnostics,
    gx_crosswalk_prefix_diagnostics(
      attr(mainstem_queryables, "gx_reference")$diagnostics,
      "/queryables/mainstems_v3"
    )
  )
  gx_huc12_intersection_validate_queryables(
    huc_queryables, mainstem_queryables, requests
  )

  unique_huc12 <- unique(huc12)
  found <- vector("list", length(unique_huc12))
  huc_found <- stats::setNames(rep(FALSE, length(unique_huc12)), unique_huc12)
  names(found) <- unique_huc12
  total_candidates <- 0L
  total_matches <- 0L
  for (query_index in seq_along(unique_huc12)) {
    requested <- unique_huc12[[query_index]]
    huc <- gx_huc12_intersection_fetch_huc(
      requested,
      huc_queryables,
      client,
      requests,
      max_requests,
      max_total_bytes
    )
    requests <- gx_crosswalk_merge_requests(
      requests, gx_crosswalk_reference_requests(huc)
    )
    reference_diagnostics <- gx_bind_diagnostics(
      reference_diagnostics,
      gx_crosswalk_prefix_diagnostics(
        attr(huc, "gx_reference")$diagnostics,
        paste0("/queries/", query_index - 1L, "/huc12")
      )
    )
    gx_huc12_intersection_validate_huc(huc, requested, requests)
    if (!nrow(huc)) {
      found[[requested]] <- tibble::tibble()
      next
    }
    huc_found[[requested]] <- TRUE
    candidates <- gx_huc12_intersection_fetch_candidates(
      as.numeric(sf::st_bbox(huc)),
      mainstem_queryables,
      max_candidates + 1L,
      client,
      requests,
      max_requests,
      max_total_bytes
    )
    requests <- gx_crosswalk_merge_requests(
      requests, gx_crosswalk_reference_requests(candidates)
    )
    reference_diagnostics <- gx_bind_diagnostics(
      reference_diagnostics,
      gx_crosswalk_prefix_diagnostics(
        attr(candidates, "gx_reference")$diagnostics,
        paste0("/queries/", query_index - 1L, "/mainstems_v3")
      )
    )
    total_candidates <- total_candidates + nrow(candidates)
    gx_huc12_intersection_assert_budgets(
      requests,
      candidates = total_candidates,
      matches = total_matches,
      max_requests = max_requests,
      max_candidates = max_candidates,
      max_matches = max_matches,
      max_rows = max_rows,
      max_total_bytes = max_total_bytes
    )
    gx_huc12_intersection_validate_candidates(candidates, requests)
    ranked <- gx_huc12_intersection_rank(
      huc, candidates, requested, requests
    )
    total_matches <- total_matches + nrow(ranked)
    gx_huc12_intersection_assert_budgets(
      requests,
      candidates = total_candidates,
      matches = total_matches,
      max_requests = max_requests,
      max_candidates = max_candidates,
      max_matches = max_matches,
      max_rows = max_rows,
      max_total_bytes = max_total_bytes
    )
    found[[requested]] <- ranked
  }

  frequencies <- tabulate(
    match(huc12, unique_huc12), nbins = length(unique_huc12)
  )
  rows_per_unique <- pmax(1L, vapply(found, nrow, integer(1)))
  projected_rows <- sum(as.double(frequencies) * as.double(rows_per_unique))
  gx_huc12_intersection_assert_budgets(
    requests,
    candidates = total_candidates,
    matches = total_matches,
    rows = projected_rows,
    max_requests = max_requests,
    max_candidates = max_candidates,
    max_matches = max_matches,
    max_rows = max_rows,
    max_total_bytes = max_total_bytes
  )

  rows <- list()
  for (input_index in seq_along(huc12)) {
    requested <- huc12[[input_index]]
    ranked <- found[[requested]]
    if (!nrow(ranked)) {
      has_huc <- isTRUE(huc_found[[requested]])
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_huc12 = requested,
        status = "not_found",
        match_index = NA_integer_,
        huc12 = if (has_huc) requested else NA_character_,
        mainstem_uri = NA_character_,
        mainstem_status = NA_character_,
        replacement_uris = list(character()),
        outlet_huc12_match = NA,
        intersection_length_km = NA_real_,
        outlet_drainagearea_sqkm = NA_real_,
        match_source = if (has_huc) {
          "reference_geometry_intersection"
        } else {
          NA_character_
        },
        diagnostics = list(gx_huc12_intersection_diagnostics(
          "not_found", input_index, huc_found = has_huc
        ))
      )
      next
    }
    for (row_index in seq_len(nrow(ranked))) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        contract_version = .gx_crosswalk_contract_version,
        input_index = as.integer(input_index),
        requested_huc12 = requested,
        status = "matched",
        match_index = as.integer(row_index),
        huc12 = requested,
        mainstem_uri = ranked$mainstem_uri[[row_index]],
        mainstem_status = ranked$mainstem_status[[row_index]],
        replacement_uris = list(ranked$replacement_uris[[row_index]]),
        outlet_huc12_match = ranked$outlet_huc12_match[[row_index]],
        intersection_length_km = ranked$intersection_length_km[[row_index]],
        outlet_drainagearea_sqkm = ranked$outlet_drainagearea_sqkm[[row_index]],
        match_source = "reference_geometry_intersection",
        diagnostics = list(gx_huc12_intersection_diagnostics(
          "matched", input_index, ranked$mainstem_status[[row_index]]
        ))
      )
    }
  }
  out <- tibble::as_tibble(do.call(rbind, rows))
  metadata <- gx_huc12_intersection_metadata(
    out,
    huc12,
    requests,
    candidate_count = total_candidates,
    unique_intersection_count = total_matches,
    reference_diagnostics = reference_diagnostics
  )
  gx_new_huc12_intersections(out, metadata)
}
