.gx_catalog_discovery_max_sites <- 100L

gx_catalog_discovery_abort <- function(
    message, class = "gx_error_catalog_discovery", ...,
    call = rlang::caller_env()) {
  gx_catalog_abort(
    message,
    class = unique(c(class, "gx_error_catalog_discovery")),
    ...,
    call = call
  )
}

gx_catalog_discovery_count <- function(x, name) {
  valid <- is.numeric(x) && !is.logical(x) && length(x) == 1L &&
    !is.na(x) && is.finite(x) && x == trunc(x) && x >= 1L &&
    x <= .gx_catalog_discovery_max_sites
  if (!valid) {
    gx_catalog_discovery_abort(
      "{.arg {name}} must be one whole number from 1 through {.val {.gx_catalog_discovery_max_sites}}.",
      "gx_error_catalog_discovery_input"
    )
  }
  as.integer(x)
}

gx_catalog_discovery_client <- function(client, endpoint, name) {
  if (!inherits(client, "gx_client") || !identical(client$endpoint, endpoint)) {
    gx_catalog_discovery_abort(
      "{.arg {name}} must be an explicit {.val {endpoint}} client.",
      "gx_error_catalog_discovery_input"
    )
  }
  client
}

gx_catalog_discovery_site_uris <- function(site_uri, max_sites) {
  if (is.null(site_uri)) return(NULL)
  valid <- is.character(site_uri) && !is.object(site_uri) &&
    is.null(attributes(site_uri)) && length(site_uri) >= 1L &&
    length(site_uri) <= max_sites && !anyNA(site_uri) &&
    !anyDuplicated(site_uri)
  if (!valid) {
    gx_catalog_discovery_abort(
      "{.arg site_uri} must contain one through {.arg max_sites} unique canonical HTTP(S) PID URIs.",
      "gx_error_catalog_discovery_input"
    )
  }
  canonical <- vapply(site_uri, gx_identity_iri, character(1))
  valid <- !anyNA(canonical) && identical(unname(canonical), unname(site_uri)) &&
    all(grepl("^https?://", canonical, ignore.case = TRUE))
  if (!valid) {
    gx_catalog_discovery_abort(
      "{.arg site_uri} contains a noncanonical or unsafe PID URI.",
      "gx_error_catalog_discovery_input"
    )
  }
  canonical[gx_catalog_byte_order(canonical)]
}

gx_catalog_discovery_profiles <- function(profiles, site_uri) {
  if (is.null(profiles)) return(NULL)
  valid <- !is.null(site_uri) && is.list(profiles) && !is.data.frame(profiles) &&
    !is.null(names(profiles)) && length(profiles) == length(site_uri) &&
    !anyNA(names(profiles)) && all(nzchar(names(profiles))) &&
    !anyDuplicated(names(profiles)) && setequal(names(profiles), site_uri)
  if (!valid) {
    gx_catalog_discovery_abort(
      paste0(
        "{.arg profiles} must be a named list containing exactly one local ",
        "JSON-LD profile for each {.arg site_uri}."
      ),
      "gx_error_catalog_discovery_input"
    )
  }
  unname(profiles[site_uri]) |>
    stats::setNames(site_uri)
}

gx_catalog_discovery_reference_geometry <- function(aoi, client) {
  if (identical(aoi$type, "sf")) return(aoi$geometry)
  spec <- switch(
    aoi$type,
    huc = list(
      collection = paste0("hu", sprintf("%02d", nchar(aoi$id))),
      field = paste0("huc", nchar(aoi$id)), value = aoi$id
    ),
    county = list(collection = "counties", field = "geoid", value = aoi$id),
    state = list(collection = "states", field = "stusps", value = aoi$id),
    NULL
  )
  if (is.null(spec)) {
    gx_catalog_discovery_abort(
      "The AOI type has no reviewed reference-geometry mapping.",
      "gx_error_catalog_discovery_aoi"
    )
  }
  query <- stats::setNames(list(spec$value), spec$field)
  feature <- tryCatch(
    gx_ref_features(
      spec$collection, query = query, limit = 2L, client = client
    ),
    error = function(cnd) {
      gx_catalog_discovery_abort(
        "Catalog discovery could not resolve the AOI reference geometry.",
        "gx_error_catalog_discovery_reference"
      )
    }
  )
  if (nrow(feature) != 1L) {
    gx_catalog_discovery_abort(
      "The AOI identifier did not resolve to exactly one reference geometry.",
      "gx_error_catalog_discovery_reference"
    )
  }
  geometry <- tryCatch(
    gx_aoi_transform_crs84_offline(sf::st_geometry(feature)),
    error = function(cnd) NULL
  )
  if (is.null(geometry) || length(geometry) != 1L ||
      isTRUE(sf::st_is_empty(geometry)[[1L]])) {
    gx_catalog_discovery_abort(
      "The AOI reference geometry could not be represented in CRS84.",
      "gx_error_catalog_discovery_reference"
    )
  }
  geometry
}

gx_catalog_discovery_graph_uris <- function(aoi, max_sites, graph_client,
                                             reference_client,
                                             graph_executor) {
  geometry <- gx_catalog_discovery_reference_geometry(aoi, reference_client)
  wkt <- tryCatch(
    unname(sf::st_as_text(geometry, digits = 17)[[1L]]),
    error = function(cnd) NULL
  )
  if (!is.character(wkt) || length(wkt) != 1L || is.na(wkt)) {
    gx_catalog_discovery_abort(
      "The AOI could not be encoded for bounded graph discovery.",
      "gx_error_catalog_discovery_aoi"
    )
  }
  query <- gx_render_query(
    "sites_in_aoi",
    list(aoi_wkt = wkt, limit = max_sites, offset = 0L)
  )
  result <- tryCatch(
    graph_executor(
      query,
      expected = "select",
      client = graph_client,
      max_rows = max_sites,
      max_variables = 8L,
      max_bound_terms = as.integer(max_sites * 4L),
      max_links = 2L,
      max_requests = 1L,
      max_total_bytes = min(as.integer(4L * 1024L^2), graph_client$max_bytes),
      max_members = as.integer(1024L + max_sites * 64L),
      max_atomic_bytes = min(
        as.integer(4L * 1024L^2), graph_client$max_bytes
      ),
      max_depth = 16L
    ),
    error = function(cnd) {
      gx_catalog_discovery_abort(
        "The bounded Geoconnex graph site query failed.",
        "gx_error_catalog_discovery_graph"
      )
    }
  )
  bindings <- result$bindings
  sites <- bindings[
    bindings$variable == "site" & bindings$term_type == "uri",
    c("row", "value"), drop = FALSE
  ]
  canonical <- if (nrow(sites)) {
    vapply(sites$value, gx_identity_iri, character(1))
  } else {
    character()
  }
  if (anyNA(canonical) || any(canonical != sites$value)) {
    gx_catalog_discovery_abort(
      "The graph returned a noncanonical site identity.",
      "gx_error_catalog_discovery_graph"
    )
  }
  canonical <- unique(canonical)
  canonical <- canonical[gx_catalog_byte_order(canonical)]
  list(
    site_uri = canonical,
    truncated = result$row_count >= max_sites,
    requests = result$requests %||% gx_graph_empty_requests()
  )
}

gx_catalog_discovery_clean_iri <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(NA_character_)
  canonical <- gx_identity_iri(x)
  if (is.na(canonical) || !identical(canonical, x)) NA_character_ else canonical
}

gx_catalog_discovery_clean_url <- function(x) {
  value <- gx_catalog_discovery_clean_iri(x)
  if (is.na(value) || !grepl("^https?://", value, ignore.case = TRUE)) {
    return(NA_character_)
  }
  safe <- tryCatch({
    gx_assert_safe_url(value, resolve_dns = FALSE)
    TRUE
  }, error = function(cnd) FALSE)
  if (safe) value else NA_character_
}

gx_catalog_discovery_point <- function(wkt) {
  if (!is.character(wkt) || length(wkt) != 1L || is.na(wkt)) {
    return(sf::st_point())
  }
  geometry <- tryCatch(
    sf::st_as_sfc(wkt, crs = gx_aoi_crs),
    error = function(cnd) NULL
  )
  if (is.null(geometry) || length(geometry) != 1L ||
      !inherits(geometry, "sfc_POINT") || isTRUE(sf::st_is_empty(geometry))) {
    return(sf::st_point())
  }
  coordinates <- unclass(geometry[[1L]])
  if (!is.numeric(coordinates) || length(coordinates) != 2L ||
      any(!is.finite(coordinates)) || coordinates[[1L]] < -180 ||
      coordinates[[1L]] > 180 || coordinates[[2L]] < -90 ||
      coordinates[[2L]] > 90) {
    return(sf::st_point())
  }
  sf::st_point(as.double(coordinates))
}

gx_catalog_discovery_problem <- function(stage, source_uri, code, severity,
                                          message, occurred_at) {
  out <- tibble::tibble(
    stage = stage,
    source_uri = if (is.na(source_uri)) NA_character_ else
      gx_redact_url(source_uri),
    path = "",
    code = code,
    severity = severity,
    message = message,
    recoverable = TRUE,
    occurred_at = as.POSIXct(occurred_at, tz = "UTC")
  )
  gx_catalog_validate_problems(out)
  out
}

gx_catalog_discovery_diagnostic_problems <- function(
    diagnostics, stage, source_uri, occurred_at) {
  if (!gx_is_diagnostics(diagnostics) || !nrow(diagnostics)) {
    return(gx_catalog_empty_problems())
  }
  diagnostics$recoverable[] <- TRUE
  gx_catalog_problems_from_diagnostics_impl(
    diagnostics, stage = stage, source_uri = gx_redact_url(source_uri),
    occurred_at = occurred_at
  )
}

gx_catalog_discovery_request_rows <- function(rows, stage, scope) {
  if (!is.data.frame(rows) || !nrow(rows)) return(gx_catalog_empty_requests())
  required <- c(
    "request_id", "method", "url", "status", "media_type", "bytes",
    "body_sha256", "retrieved_at", "cache_origin"
  )
  if (!all(required %in% names(rows))) return(gx_catalog_empty_requests())
  request_id <- vapply(seq_len(nrow(rows)), function(index) {
    gx_contract_hash(
      list(scope, rows$request_id[[index]], index),
      namespace = "geoconnexr.catalog-request.v1",
      contract_version = .gx_catalog_contract_version
    )
  }, character(1))
  url <- vapply(rows$url, gx_redact_url, character(1))
  bytes <- as.integer(rows$bytes)
  out <- tibble::tibble(
    request_id = request_id,
    stage = rep(stage, nrow(rows)),
    method = as.character(rows$method),
    canonical_url_redacted = url,
    request_hash = vapply(seq_len(nrow(rows)), function(index) {
      gx_contract_hash(
        list(rows$method[[index]], url[[index]], rows$request_id[[index]]),
        namespace = "geoconnexr.catalog-request-source.v1",
        contract_version = .gx_catalog_contract_version
      )
    }, character(1)),
    body_hash = rep(NA_character_, nrow(rows)),
    final_url = url,
    response_status = as.integer(rows$status),
    response_media_type = as.character(rows$media_type),
    encoded_bytes = bytes,
    decoded_bytes = bytes,
    content_hash = as.character(rows$body_sha256),
    etag = rep(NA_character_, nrow(rows)),
    last_modified = rep(NA_character_, nrow(rows)),
    retrieved_at = as.POSIXct(rows$retrieved_at, tz = "UTC"),
    elapsed_ms = rep(NA_real_, nrow(rows)),
    cache_origin = as.character(rows$cache_origin),
    error_code = rep(NA_character_, nrow(rows))
  )
  gx_catalog_validate_requests(out)
  out
}

gx_catalog_discovery_site_row <- function(location, site_uri, source_url) {
  position <- match(site_uri, location$site_uri)
  if (is.na(position)) {
    gx_catalog_discovery_abort(
      "A retrieved profile did not describe its requested site identity.",
      "gx_error_catalog_discovery_profile"
    )
  }
  row <- location[position, , drop = FALSE]
  sf::st_sf(
    tibble::tibble(
      contract_version = .gx_catalog_contract_version,
      site_uri = site_uri,
      name = row$name[[1L]],
      description = row$description[[1L]],
      site_type = row$site_type[[1L]],
      provider_id = NA_character_,
      provider_uri = gx_catalog_discovery_clean_iri(row$provider_uri[[1L]]),
      provider_name = row$provider_name[[1L]],
      provider_url = gx_catalog_discovery_clean_url(row$provider_url[[1L]]),
      mainstem_uri = gx_catalog_discovery_clean_url(row$mainstem_uri[[1L]]),
      landing_url = gx_catalog_discovery_clean_url(row$landing_url[[1L]]),
      source_url = source_url
    ),
    geometry = sf::st_sfc(
      gx_catalog_discovery_point(row$geometry_wkt[[1L]]), crs = gx_aoi_crs
    )
  )
}

gx_catalog_discovery_missing_site_row <- function(site_uri) {
  sf::st_sf(
    tibble::tibble(
      contract_version = .gx_catalog_contract_version,
      site_uri = site_uri, name = NA_character_, description = NA_character_,
      site_type = NA_character_, provider_id = NA_character_,
      provider_uri = NA_character_, provider_name = NA_character_,
      provider_url = NA_character_, mainstem_uri = NA_character_,
      landing_url = NA_character_, source_url = site_uri
    ),
    geometry = sf::st_sfc(sf::st_point(), crs = gx_aoi_crs)
  )
}

gx_catalog_discovery_dataset_rows <- function(data, site_uri, source_url) {
  if (!nrow(data)) return(gx_catalog_empty_datasets())
  out <- tibble::as_tibble(data[setdiff(names(data), "diagnostics")])
  out$contract_version[] <- .gx_catalog_contract_version
  out$site_uri[] <- site_uri
  out$source_url[] <- source_url
  iri_fields <- c("dataset_uri", "variable_uri", "unit_uri", "provider_uri")
  for (field in iri_fields) {
    out[[field]] <- vapply(
      out[[field]], gx_catalog_discovery_clean_iri, character(1)
    )
  }
  url_fields <- c("distribution_url", "provider_url", "source_url")
  for (field in url_fields) {
    out[[field]] <- vapply(
      out[[field]], gx_catalog_discovery_clean_url, character(1)
    )
  }
  out$source_url[is.na(out$source_url)] <- site_uri
  out$conforms_to <- lapply(out$conforms_to, function(value) {
    value <- vapply(value, gx_catalog_discovery_clean_iri, character(1))
    value <- unique(value[!is.na(value)])
    value[gx_catalog_byte_order(value)]
  })
  for (index in seq_len(nrow(out))) {
    if (is.na(out$distribution_url[[index]])) {
      out$distribution_id[[index]] <- NA_character_
      out$handler_id[[index]] <- "unknown"
      out$fetchable[[index]] <- FALSE
      next
    }
    handler <- tryCatch(
      gx_classify_distribution(
        out$distribution_url[[index]], out$media_type[[index]],
        out$conforms_to[[index]]
      ),
      error = function(cnd) "unknown"
    )
    out$handler_id[[index]] <- handler
    out$fetchable[[index]] <- !identical(handler, "unknown") &&
      !is.na(out$dataset_id[[index]]) &&
      !is.na(out$distribution_id[[index]])
  }
  key <- paste(
    ifelse(is.na(out$dataset_id), "<NA>", out$dataset_id),
    ifelse(is.na(out$distribution_id), "<NA>", out$distribution_id),
    ifelse(is.na(out$variable_id), "<NA>", out$variable_id),
    sep = "\r"
  )
  out <- out[!duplicated(key), , drop = FALSE]
  out
}

gx_catalog_discovery_bind <- function(rows, empty) {
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (!length(rows)) return(empty)
  tibble::as_tibble(do.call(rbind, rows))
}

gx_catalog_discovery_completeness <- function(
    sites, datasets, inputs, succeeded, failed, truncated) {
  partial <- failed > 0L || isTRUE(truncated)
  status <- if (partial) "partial" else "complete"
  reason <- if (partial) {
    paste0(
      if (failed > 0L) "One or more site profiles failed. " else "",
      if (truncated) "The single graph page reached max_sites." else ""
    ) |> trimws()
  } else {
    NA_character_
  }
  tibble::tibble(
    stage = c("sites", "datasets"),
    status = rep(status, 2L),
    truncated = rep(isTRUE(truncated), 2L),
    input_count = rep(as.integer(inputs), 2L),
    attempted_count = rep(as.integer(inputs), 2L),
    succeeded_count = rep(as.integer(succeeded), 2L),
    failed_count = rep(as.integer(failed), 2L),
    skipped_count = rep(0L, 2L),
    output_count = as.integer(c(nrow(sites), nrow(datasets))),
    reason = rep(reason, 2L)
  )
}

gx_catalog_impl <- function(
    aoi,
    site_uri = NULL,
    profiles = NULL,
    max_sites = 25L,
    client = gx_client("pid"),
    graph_client = gx_client("graph"),
    reference_client = gx_client("reference"),
    profile_fetcher = gx_jsonld,
    graph_executor = gx_graph_execute_once,
    now = gx_now) {
  tryCatch(gx_validate_aoi(aoi), error = function(cnd) {
    gx_catalog_discovery_abort(
      "{.arg aoi} must be a valid {.cls gx_aoi} object.",
      "gx_error_catalog_discovery_input"
    )
  })
  max_sites <- gx_catalog_discovery_count(max_sites, "max_sites")
  client <- gx_catalog_discovery_client(client, "pid", "client")
  graph_client <- gx_catalog_discovery_client(
    graph_client, "graph", "graph_client"
  )
  reference_client <- gx_catalog_discovery_client(
    reference_client, "reference", "reference_client"
  )
  if (!is.function(profile_fetcher) || !is.function(graph_executor) ||
      !is.function(now)) {
    gx_catalog_discovery_abort(
      "Catalog discovery runtime adapters must be functions.",
      "gx_error_catalog_discovery_input"
    )
  }
  created_at <- as.POSIXct(now(), tz = "UTC")
  if (length(created_at) != 1L || is.na(created_at)) {
    gx_catalog_discovery_abort(
      "Catalog discovery received an invalid clock value.",
      "gx_error_catalog_discovery_input"
    )
  }
  seeds <- gx_catalog_discovery_site_uris(site_uri, max_sites)
  profiles <- gx_catalog_discovery_profiles(profiles, seeds)
  seeded <- !is.null(seeds)
  discovery <- if (seeded) {
    list(
      site_uri = seeds, truncated = FALSE,
      requests = gx_graph_empty_requests()
    )
  } else {
    gx_catalog_discovery_graph_uris(
      aoi, max_sites, graph_client, reference_client, graph_executor
    )
  }

  sites <- list()
  datasets <- list()
  problems <- list()
  requests <- list()
  if (nrow(discovery$requests)) {
    requests[[length(requests) + 1L]] <-
      gx_catalog_discovery_request_rows(
        discovery$requests, "sites", "graph-discovery"
      )
  }
  if (seeded) {
    problems[[length(problems) + 1L]] <- gx_catalog_discovery_problem(
      "sites", NA_character_, "explicit_site_seed", "info",
      "Site PIDs were supplied explicitly; AOI membership was not rechecked.",
      created_at
    )
  }
  if (!is.null(profiles)) {
    problems[[length(problems) + 1L]] <- gx_catalog_discovery_problem(
      "datasets", NA_character_, "caller_supplied_profiles", "info",
      paste0(
        "Catalog rows were adapted from caller-supplied local JSON-LD; ",
        "no PID profile request was made."
      ),
      created_at
    )
  }
  if (discovery$truncated) {
    problems[[length(problems) + 1L]] <- gx_catalog_discovery_problem(
      "sites", NA_character_, "site_page_truncated", "warning",
      "The single bounded graph page reached max_sites; no next page was followed.",
      created_at
    )
  }

  succeeded <- 0L
  failed <- 0L
  for (uri in discovery$site_uri) {
    profile <- if (is.null(profiles)) {
      tryCatch(
        profile_fetcher(uri, client = client),
        error = function(cnd) cnd
      )
    } else {
      profiles[[uri]]
    }
    if (inherits(profile, "error")) {
      failed <- failed + 1L
      sites[[length(sites) + 1L]] <-
        gx_catalog_discovery_missing_site_row(uri)
      problems[[length(problems) + 1L]] <- gx_catalog_discovery_problem(
        "sites", uri, "profile_retrieval_failed", "error",
        "A bounded site profile could not be retrieved or decoded.", created_at
      )
      next
    }
    source_url <- if (inherits(profile, "gx_jsonld")) {
      gx_catalog_discovery_clean_url(profile$source_url)
    } else {
      uri
    }
    if (is.na(source_url)) source_url <- uri
    parsed <- tryCatch(
      list(
        location = gx_parse_location(profile),
        datasets = gx_parse_datasets(profile)
      ),
      error = function(cnd) cnd
    )
    if (inherits(parsed, "error")) {
      failed <- failed + 1L
      sites[[length(sites) + 1L]] <-
        gx_catalog_discovery_missing_site_row(uri)
      problems[[length(problems) + 1L]] <- gx_catalog_discovery_problem(
        "sites", source_url, "profile_parse_failed", "error",
        "A bounded site profile could not be adapted to catalog tables.",
        created_at
      )
      next
    }
    site <- tryCatch(
      gx_catalog_discovery_site_row(
        parsed$location, uri, source_url
      ),
      error = function(cnd) cnd
    )
    if (inherits(site, "error")) {
      failed <- failed + 1L
      sites[[length(sites) + 1L]] <-
        gx_catalog_discovery_missing_site_row(uri)
      problems[[length(problems) + 1L]] <- gx_catalog_discovery_problem(
        "sites", source_url, "site_adaptation_failed", "error",
        "A profile did not contain one usable requested site identity.",
        created_at
      )
      next
    }
    data <- gx_catalog_discovery_dataset_rows(
      parsed$datasets, uri, source_url
    )
    sites[[length(sites) + 1L]] <- site
    datasets[[length(datasets) + 1L]] <- data
    succeeded <- succeeded + 1L
    problems[[length(problems) + 1L]] <-
      gx_catalog_discovery_diagnostic_problems(
        attr(parsed$location, "diagnostics"), "sites", source_url, created_at
      )
    problems[[length(problems) + 1L]] <-
      gx_catalog_discovery_diagnostic_problems(
        attr(parsed$datasets, "diagnostics"), "datasets", source_url,
        created_at
      )
    if (inherits(profile, "gx_jsonld")) {
      requests[[length(requests) + 1L]] <-
        gx_catalog_discovery_request_rows(
          profile$requests, "datasets", uri
        )
    }
  }

  site_table <- if (length(sites)) {
    out <- if (length(sites) == 1L) sites[[1L]] else do.call(rbind, sites)
    out[gx_catalog_byte_order(out$site_uri), , drop = FALSE]
  } else {
    gx_catalog_empty_sites()
  }
  dataset_table <- gx_catalog_discovery_bind(
    datasets, gx_catalog_empty_datasets()
  )
  if (nrow(dataset_table)) {
    order <- gx_catalog_byte_order(
      dataset_table$site_uri, dataset_table$dataset_id,
      dataset_table$distribution_id, dataset_table$variable_id
    )
    dataset_table <- dataset_table[order, , drop = FALSE]
  }
  problem_table <- gx_catalog_discovery_bind(
    problems, gx_catalog_empty_problems()
  )
  request_table <- gx_catalog_discovery_bind(
    requests, gx_catalog_empty_requests()
  )
  completeness <- gx_catalog_discovery_completeness(
    site_table, dataset_table, length(discovery$site_uri), succeeded, failed,
    discovery$truncated
  )
  metadata <- list(
    created_at = created_at,
    selection = list(
      include = c("sites", "datasets"),
      providers = character(), variables = character()
    ),
    completeness = completeness,
    counts = list(
      sites = as.integer(nrow(site_table)),
      datasets = as.integer(nrow(dataset_table)),
      reference_layers = 0L,
      reference_features = 0L,
      problems = as.integer(nrow(problem_table)),
      requests = as.integer(nrow(request_table))
    ),
    endpoints = c(
      graph = gx_identity_iri(graph_client$base_url),
      pid = gx_identity_iri(client$base_url),
      reference = gx_identity_iri(reference_client$base_url)
    ),
    hydrologic_vintage = list(
      reference_collection = NA_character_, vintage = NA_character_,
      migration_policy = "live_reference_geometry_not_snapshotted"
    ),
    source_contracts = c(
      aoi = gx_aoi_contract_version,
      catalog = .gx_catalog_contract_version,
      jsonld = .gx_jsonld_contract_version
    )
  )
  gx_catalog_new_impl(
    aoi = aoi, sites = site_table, datasets = dataset_table,
    reference = list(), problems = problem_table, requests = request_table,
    metadata = metadata
  )
}

#' Discover a bounded Geoconnex catalog
#'
#' Builds the public catalog value object used by [gx_fetch_plan()]. With no
#' explicit site PIDs, the function resolves identifier AOIs through the
#' Geoconnex reference service, executes one bounded `sites_in_aoi` graph page,
#' then retrieves each selected PID's JSON-LD profile. Supplying `site_uri`
#' skips graph discovery and retrieves exactly those profiles; this is useful
#' for reproducible examples, but does not assert that the PIDs fall inside the
#' AOI.
#' Alternatively, `profiles` can provide those exact profiles as local JSON-LD
#' inputs accepted by [gx_parse_location()]. This performs no PID profile
#' request and is intended for offline catalog construction or for providers
#' whose distribution description is not yet published through a PID.
#'
#' Discovery is deliberately single-page and sequential. Reaching `max_sites`
#' records a partial, truncated catalog rather than following a graph page.
#' Individual profile failures remain visible in `catalog$problems` and
#' completeness metadata while unrelated sites continue.
#'
#' @param aoi A validated object returned by [gx_aoi()].
#' @param site_uri Optional unique canonical HTTP(S) site PID URIs. When
#'   supplied, automatic AOI membership discovery is skipped.
#' @param profiles Optional named list of local JSON-LD inputs, with names
#'   exactly matching `site_uri`. Each value may be a decoded JSON-LD list, a
#'   JSON string, raw JSON bytes, or a `gx_jsonld` object. Supplying profiles
#'   also requires `site_uri` and performs no PID profile requests.
#' @param max_sites Maximum graph rows or explicit site PIDs, from 1 through
#'   100.
#' @param client A PID client from [gx_client()].
#' @param graph_client A graph client from [gx_client()].
#' @param reference_client A reference client from [gx_client()].
#'
#' @return A validated `gx_catalog` with typed sites, flattened datasets,
#'   recoverable problems, request evidence, and procedural completeness.
#' @export
gx_catalog <- function(
    aoi,
    site_uri = NULL,
    profiles = NULL,
    max_sites = 25L,
    client = gx_client("pid"),
    graph_client = gx_client("graph"),
    reference_client = gx_client("reference")) {
  gx_catalog_impl(
    aoi = aoi,
    site_uri = site_uri,
    profiles = profiles,
    max_sites = max_sites,
    client = client,
    graph_client = graph_client,
    reference_client = reference_client
  )
}
