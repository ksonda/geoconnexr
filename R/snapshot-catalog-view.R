gx_snapshot_catalog_view_abort <- function(
    message,
    subclass = "gx_error_snapshot_catalog_view_contract",
    call = rlang::caller_env()) {
  gx_abort(
    message,
    c(subclass, "gx_error_snapshot_catalog_view", "gx_error_snapshot"),
    call = call,
    .redact_trace = TRUE
  )
}

gx_snapshot_catalog_view_logical_impl <- function(x, label) {
  valid <- is.character(x) && !anyNA(x) &&
    all(x %in% c("true", "false"))
  if (!valid) {
    gx_snapshot_catalog_view_abort(
      "The redacted catalog view has an invalid logical field in {label}.",
      "gx_error_snapshot_catalog_view_type"
    )
  }
  identical_value <- x == "true"
  unname(identical_value)
}

gx_snapshot_catalog_view_time_impl <- function(
    x,
    label,
    allow_blank = TRUE) {
  valid_input <- is.character(x) && !anyNA(x) &&
    (allow_blank || all(nzchar(x)))
  present <- nzchar(x)
  pattern <- paste0(
    "^[0-9]{4}-(?:0[1-9]|1[0-2])-(?:0[1-9]|[12][0-9]|3[01])",
    "T(?:[01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]",
    "\\.[0-9]{6}Z\\z"
  )
  if (!valid_input || any(present & !grepl(pattern, x, perl = TRUE))) {
    gx_snapshot_catalog_view_abort(
      "The redacted catalog view has an invalid UTC timestamp in {label}.",
      "gx_error_snapshot_catalog_view_type"
    )
  }
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  if (any(present)) {
    parsed <- as.POSIXct(
      x[present],
      format = "%Y-%m-%dT%H:%M:%OSZ",
      tz = "UTC"
    )
    rebound <- tryCatch(
      vapply(
        as.list(parsed),
        gx_snapshot_time_text_impl,
        character(1),
        USE.NAMES = FALSE
      ),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
    if (anyNA(parsed) || is.null(rebound) ||
        !identical(unname(rebound), unname(x[present]))) {
      gx_snapshot_catalog_view_abort(
        "The redacted catalog view timestamp is not an exact UTC instant in {label}.",
        "gx_error_snapshot_catalog_view_type"
      )
    }
    out[present] <- parsed
  }
  attr(out, "tzone") <- "UTC"
  unname(out)
}

gx_snapshot_catalog_view_conforms_impl <- function(x) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    gx_snapshot_catalog_view_abort(
      "The redacted catalog conforms-to field must contain canonical JSON arrays.",
      "gx_error_snapshot_catalog_view_type"
    )
  }
  lapply(x, function(value) {
    decoded <- tryCatch(
      gx_snapshot_parse_json(charToRaw(enc2utf8(value))),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
    array <- tryCatch(
      gx_snapshot_plain_array(
        decoded,
        maximum = .gx_catalog_max_conforms_per_row
      ),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
    values <- if (is.null(array)) {
      NULL
    } else {
      tryCatch(
        vapply(
          array,
          gx_snapshot_plain_string,
          character(1),
          nonempty = TRUE
        ),
        error = function(cnd) NULL,
        warning = function(cnd) NULL
      )
    }
    valid <- !is.null(values) &&
      !anyNA(values) && !anyDuplicated(values) &&
      gx_catalog_text_valid(values, allow_na = FALSE, nonempty = TRUE) &&
      gx_catalog_byte_sorted(values) &&
      identical(gx_catalog_encode_conforms(unname(values)), value)
    if (!valid) {
      gx_snapshot_catalog_view_abort(
        "The redacted catalog conforms-to field is not its canonical JSON array.",
        "gx_error_snapshot_catalog_view_type"
      )
    }
    unname(values)
  })
}

gx_snapshot_catalog_view_geometry_impl <- function(wkt) {
  if (!is.character(wkt) || anyNA(wkt)) {
    gx_snapshot_catalog_view_abort(
      "The redacted catalog site geometry must contain exact WKT text.",
      "gx_error_snapshot_catalog_view_geometry"
    )
  }
  if (!length(wkt)) {
    return(sf::st_geometry(gx_catalog_empty_sites()))
  }
  geometry <- tryCatch(
    sf::st_as_sfc(wkt, crs = gx_aoi_crs),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  rebound <- if (is.null(geometry)) {
    NULL
  } else {
    tryCatch(
      unname(sf::st_as_text(geometry, digits = 17)),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
  }
  empty <- if (is.null(geometry)) {
    NULL
  } else {
    tryCatch(sf::st_is_empty(geometry), error = function(cnd) NULL)
  }
  valid_coordinates <- if (is.null(geometry) || is.null(empty)) {
    FALSE
  } else {
    vapply(seq_along(geometry), function(index) {
      if (isTRUE(empty[[index]])) return(TRUE)
      point <- unclass(geometry[[index]])
      is.numeric(point) && length(point) == 2L &&
        all(is.finite(point)) &&
        point[[1L]] >= -180 && point[[1L]] <= 180 &&
        point[[2L]] >= -90 && point[[2L]] <= 90
    }, logical(1))
  }
  if (is.null(geometry) || !inherits(geometry, "sfc_POINT") ||
      !identical(sf::st_crs(geometry)$input, gx_aoi_crs) ||
      !identical(rebound, unname(wkt)) || !all(valid_coordinates)) {
    gx_snapshot_catalog_view_abort(
      "The redacted catalog site WKT is not an exact bounded CRS84 point.",
      "gx_error_snapshot_catalog_view_geometry"
    )
  }
  geometry
}

gx_snapshot_catalog_view_sites_impl <- function(raw) {
  expected <- gx_snapshot_catalog_csv_columns$sites
  if (!inherits(raw, "tbl_df") || !identical(names(raw), expected)) {
    gx_snapshot_catalog_view_abort(
      "The redacted sites table has an invalid exact schema.",
      "gx_error_snapshot_catalog_view_shape"
    )
  }
  geometry <- gx_snapshot_catalog_view_geometry_impl(raw$geometry_wkt)
  data <- raw[setdiff(names(raw), "geometry_wkt")]
  out <- sf::st_sf(data, geometry = geometry)
  if (!identical(
    names(out),
    c(setdiff(expected, "geometry_wkt"), "geometry")
  )) {
    gx_snapshot_catalog_view_abort(
      "The typed redacted sites view has an invalid exact schema.",
      "gx_error_snapshot_catalog_view_shape"
    )
  }
  out
}

gx_snapshot_catalog_view_datasets_impl <- function(raw) {
  expected <- gx_snapshot_catalog_csv_columns$datasets
  if (!inherits(raw, "tbl_df") || !identical(names(raw), expected)) {
    gx_snapshot_catalog_view_abort(
      "The redacted datasets table has an invalid exact schema.",
      "gx_error_snapshot_catalog_view_shape"
    )
  }
  out <- raw
  out$temporal_start <- gx_snapshot_catalog_view_time_impl(
    raw$temporal_start,
    "datasets$temporal_start"
  )
  out$temporal_end <- gx_snapshot_catalog_view_time_impl(
    raw$temporal_end,
    "datasets$temporal_end"
  )
  out$conforms_to <- gx_snapshot_catalog_view_conforms_impl(raw$conforms_to)
  out$fetchable <- gx_snapshot_catalog_view_logical_impl(
    raw$fetchable,
    "datasets$fetchable"
  )
  tibble::as_tibble(out)
}

gx_snapshot_catalog_view_problems_impl <- function(raw) {
  expected <- gx_snapshot_catalog_csv_columns$problems
  if (!inherits(raw, "tbl_df") || !identical(names(raw), expected)) {
    gx_snapshot_catalog_view_abort(
      "The redacted problems table has an invalid exact schema.",
      "gx_error_snapshot_catalog_view_shape"
    )
  }
  out <- raw
  out$recoverable <- gx_snapshot_catalog_view_logical_impl(
    raw$recoverable,
    "problems$recoverable"
  )
  out$occurred_at <- gx_snapshot_catalog_view_time_impl(
    raw$occurred_at,
    "problems$occurred_at",
    allow_blank = FALSE
  )
  tibble::as_tibble(out)
}

gx_snapshot_catalog_view_metadata_impl <- function(
    sites,
    datasets,
    problems,
    requests) {
  list(
    scope = "redacted_catalog_view_v1",
    blank_cells = "preserved_as_empty_strings",
    identity_policy = "redacted_values_not_reconstructed",
    typed_fields = list(
      sites = "geometry_wkt_to_crs84_point",
      datasets = c(
        "temporal_start", "temporal_end", "conforms_to", "fetchable"
      ),
      problems = c("recoverable", "occurred_at"),
      requests = "authoritative_manifest_ledger"
    ),
    counts = list(
      sites = as.integer(nrow(sites)),
      datasets = as.integer(nrow(datasets)),
      problems = as.integer(nrow(problems)),
      requests = as.integer(nrow(requests))
    ),
    replayable = FALSE
  )
}

gx_snapshot_catalog_view_id_impl <- function(
    path,
    verification,
    resource_evidence,
    request_evidence,
    metadata) {
  gx_contract_hash(
    list(
      "mode", "redacted_catalog_view",
      "path", path,
      "manifest_sha256", verification$manifest_sha256,
      "sites_table_id", resource_evidence$sites$table_id,
      "datasets_table_id", resource_evidence$datasets$table_id,
      "problems_table_id", resource_evidence$problems$table_id,
      "request_export_id", request_evidence$export_id,
      "sites", metadata$counts$sites,
      "datasets", metadata$counts$datasets,
      "problems", metadata$counts$problems,
      "requests", metadata$counts$requests
    ),
    namespace = "geoconnexr.snapshot-catalog-view.v1",
    contract_version = "0.1.0"
  )
}

gx_snapshot_catalog_view_validate_impl <- function(x) {
  fields <- c(
    "contract_version", "mode", "status", "path", "verification", "sites",
    "datasets", "problems", "requests", "resource_evidence",
    "request_evidence", "metadata", "view_id"
  )
  valid_top <- is.list(x) &&
    identical(class(x), "gx_snapshot_catalog_view") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), fields) &&
    identical(x$contract_version, "0.1.0") &&
    identical(x$mode, "redacted_catalog_view") &&
    identical(x$status, "loaded_and_verified") &&
    is.character(x$path) && length(x$path) == 1L && !is.na(x$path) &&
    isTRUE(gx_catalog_is_sha256(x$view_id)) &&
    is.list(x$resource_evidence) &&
    identical(names(x$resource_evidence), c("sites", "datasets", "problems"))
  valid_verification <- valid_top && tryCatch({
    gx_snapshot_verification_validate_impl(x$verification)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  root <- if (valid_top) {
    tryCatch(
      gx_snapshot_root(x$path),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
  } else {
    NULL
  }
  valid_resources <- valid_top && all(vapply(
    names(x$resource_evidence),
    function(resource) {
      evidence <- x$resource_evidence[[resource]]
      tryCatch({
        gx_snapshot_catalog_csv_validate_impl(evidence)
        identical(evidence$resource, resource)
      }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
    },
    logical(1)
  ))
  valid_requests <- valid_top && tryCatch({
    gx_snapshot_request_export_validate_impl(x$request_evidence)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  rebound <- if (valid_resources && valid_requests) {
    tryCatch(
      list(
        sites = gx_snapshot_catalog_view_sites_impl(
          x$resource_evidence$sites$table
        ),
        datasets = gx_snapshot_catalog_view_datasets_impl(
          x$resource_evidence$datasets$table
        ),
        problems = gx_snapshot_catalog_view_problems_impl(
          x$resource_evidence$problems$table
        ),
        requests = x$request_evidence$requests
      ),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
  } else {
    NULL
  }
  expected_metadata <- if (!is.null(rebound)) {
    gx_snapshot_catalog_view_metadata_impl(
      rebound$sites,
      rebound$datasets,
      rebound$problems,
      rebound$requests
    )
  } else {
    NULL
  }
  evidence_paths <- if (valid_resources) {
    vapply(
      x$resource_evidence,
      `[[`,
      character(1),
      "path"
    )
  } else {
    character()
  }
  evidence_manifests <- if (valid_resources) {
    vapply(
      x$resource_evidence,
      `[[`,
      character(1),
      "manifest_sha256"
    )
  } else {
    character()
  }
  expected_id <- if (valid_verification && valid_resources &&
      valid_requests && !is.null(expected_metadata)) {
    gx_snapshot_catalog_view_id_impl(
      x$path,
      x$verification,
      x$resource_evidence,
      x$request_evidence,
      expected_metadata
    )
  } else {
    NULL
  }
  valid <- valid_top && valid_verification && valid_resources &&
    valid_requests && !is.null(root) && identical(root$path, x$path) &&
    !is.null(rebound) &&
    identical(x$verification$status, "verified") &&
    all(evidence_paths == x$path) &&
    identical(x$request_evidence$path, x$path) &&
    all(evidence_manifests == x$verification$manifest_sha256) &&
    identical(
      x$request_evidence$manifest_sha256,
      x$verification$manifest_sha256
    ) &&
    identical(x$sites, rebound$sites) &&
    identical(x$datasets, rebound$datasets) &&
    identical(x$problems, rebound$problems) &&
    identical(x$requests, rebound$requests) &&
    identical(x$metadata, expected_metadata) &&
    !is.null(expected_id) && identical(x$view_id, expected_id)
  if (!valid) {
    gx_snapshot_catalog_view_abort(
      "Redacted catalog-view evidence violates its exact binding contract.",
      "gx_error_snapshot_catalog_view_evidence"
    )
  }
  invisible(x)
}

# Internal M9h boundary. It derives typed fields only from the fixed M9g
# character tables, preserves blank/redacted strings, retains all source
# evidence, and does not construct gx_catalog or authorize replay.
gx_snapshot_load_catalog_view_impl <- function(dir) {
  tryCatch(
    {
      verification <- gx_snapshot_verify(dir)
      resource_evidence <- list(
        sites = gx_snapshot_load_catalog_csv_impl(dir, "sites"),
        datasets = gx_snapshot_load_catalog_csv_impl(dir, "datasets"),
        problems = gx_snapshot_load_catalog_csv_impl(dir, "problems")
      )
      request_evidence <- gx_snapshot_requests(dir)
      path <- resource_evidence$sites$path
      sites <- gx_snapshot_catalog_view_sites_impl(
        resource_evidence$sites$table
      )
      datasets <- gx_snapshot_catalog_view_datasets_impl(
        resource_evidence$datasets$table
      )
      problems <- gx_snapshot_catalog_view_problems_impl(
        resource_evidence$problems$table
      )
      requests <- request_evidence$requests
      metadata <- gx_snapshot_catalog_view_metadata_impl(
        sites,
        datasets,
        problems,
        requests
      )
      object <- structure(
        list(
          contract_version = "0.1.0",
          mode = "redacted_catalog_view",
          status = "loaded_and_verified",
          path = path,
          verification = verification,
          sites = sites,
          datasets = datasets,
          problems = problems,
          requests = requests,
          resource_evidence = resource_evidence,
          request_evidence = request_evidence,
          metadata = metadata,
          view_id = gx_snapshot_catalog_view_id_impl(
            path,
            verification,
            resource_evidence,
            request_evidence,
            metadata
          )
        ),
        class = "gx_snapshot_catalog_view"
      )
      gx_snapshot_catalog_view_validate_impl(object)
      object
    },
    error = function(cnd) {
      if (inherits(cnd, "gx_error_snapshot_catalog_view")) stop(cnd)
      if (inherits(cnd, "gx_error_snapshot")) stop(cnd)
      gx_snapshot_catalog_view_abort(
        "Snapshot redacted catalog-view loading failed closed.",
        "gx_error_snapshot_catalog_view_contract"
      )
    }
  )
}
