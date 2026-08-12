.gx_package_hydrated_contract_version <- "0.2.0"

.gx_package_hydrated_fields <- c(
  "contract_version", "mode", "status", "stage", "path", "table_view",
  "catalog", "fetch", "harmonized", "native_tables", "metadata",
  "hydration_id"
)

.gx_package_hydrated_metadata_fields <- c(
  "scope", "offline", "read_only", "typed_package_tables", "redacted",
  "catalog_tables", "fetch_tables", "harmonized_tables", "native_tables",
  "observations_format", "arrow", "native_payloads_typed",
  "reconstructed_objects", "authenticity",
  "frictionless", "replayable"
)

gx_package_hydrate_abort <- function(
    message,
    class = "gx_error_package_hydrate_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_hydrate", "gx_error_package_tables",
      "gx_error_package_load", "gx_error_package"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_hydrate_schema_impl <- function(table, columns, label) {
  if (!inherits(table, "tbl_df") || !identical(names(table), columns) ||
      !all(vapply(table, is.character, logical(1)))) {
    gx_package_hydrate_abort(
      "The canonical package table for {label} has an invalid exact schema.",
      "gx_error_package_hydrate_shape"
    )
  }
  table
}

gx_package_hydrate_integer_impl <- function(x, label) {
  present <- nzchar(x)
  valid <- is.character(x) && !anyNA(x) && all(
    !present | grepl("^-?(?:0|[1-9][0-9]*)$", x, perl = TRUE)
  )
  parsed <- suppressWarnings(as.double(x[present]))
  valid <- valid && all(is.finite(parsed)) &&
    all(parsed == trunc(parsed)) &&
    all(parsed >= -.Machine$integer.max) &&
    all(parsed <= .Machine$integer.max) &&
    identical(as.character(parsed), x[present])
  if (!valid) {
    gx_package_hydrate_abort(
      "The canonical package table has an invalid integer field in {label}.",
      "gx_error_package_hydrate_type"
    )
  }
  out <- rep.int(NA_integer_, length(x))
  out[present] <- as.integer(parsed)
  unname(out)
}

gx_package_hydrate_double_impl <- function(x, label) {
  present <- nzchar(x)
  parsed <- suppressWarnings(as.numeric(x[present]))
  rebound <- if (length(parsed)) sprintf("%.17g", parsed) else character()
  valid <- is.character(x) && !anyNA(x) &&
    all(is.finite(parsed)) && identical(rebound, x[present])
  if (!valid) {
    gx_package_hydrate_abort(
      "The canonical package table has an invalid numeric field in {label}.",
      "gx_error_package_hydrate_type"
    )
  }
  out <- rep.int(NA_real_, length(x))
  out[present] <- parsed
  unname(out)
}

gx_package_hydrate_logical_impl <- function(x, label) {
  valid <- is.character(x) && !anyNA(x) &&
    all(x %in% c("", "true", "false"))
  if (!valid) {
    gx_package_hydrate_abort(
      "The canonical package table has an invalid logical field in {label}.",
      "gx_error_package_hydrate_type"
    )
  }
  out <- rep.int(NA, length(x))
  out[x == "true"] <- TRUE
  out[x == "false"] <- FALSE
  unname(out)
}

gx_package_hydrate_fetch_status_impl <- function(raw) {
  raw <- gx_package_hydrate_schema_impl(
    raw, .gx_fetched_status_columns, "catalog/fetch_status.csv"
  )
  out <- raw
  for (field in c(
    "selection_order", "fetch_order", "physical_attempts", "result_index"
  )) {
    out[[field]] <- gx_package_hydrate_integer_impl(
      raw[[field]], paste0("fetch_status$", field)
    )
  }
  for (field in c("attempted", "succeeded")) {
    out[[field]] <- gx_package_hydrate_logical_impl(
      raw[[field]], paste0("fetch_status$", field)
    )
  }
  for (field in c("encoded_bytes", "decoded_bytes")) {
    out[[field]] <- gx_package_hydrate_double_impl(
      raw[[field]], paste0("fetch_status$", field)
    )
  }
  tibble::as_tibble(out)
}

gx_package_hydrate_native_index_impl <- function(raw) {
  columns <- c(
    "contract_version", "result_index", "result_id", "distribution_id",
    "handler_id", "payload_class", "storage", "path", "media_type",
    "row_count", "column_count", "raw_body_available", "bytes", "sha256"
  )
  raw <- gx_package_hydrate_schema_impl(
    raw, columns, "data/native/index.csv"
  )
  out <- raw
  for (field in c("result_index", "row_count", "column_count")) {
    out[[field]] <- gx_package_hydrate_integer_impl(
      raw[[field]], paste0("native_index$", field)
    )
  }
  out$raw_body_available <- gx_package_hydrate_logical_impl(
    raw$raw_body_available, "native_index$raw_body_available"
  )
  out$bytes <- gx_package_hydrate_double_impl(
    raw$bytes, "native_index$bytes"
  )
  tibble::as_tibble(out)
}

gx_package_hydrate_observations_impl <- function(raw) {
  raw <- gx_package_hydrate_schema_impl(
    raw, .gx_harmonized_observation_columns, "data/observations.csv"
  )
  out <- raw
  for (field in c("observation_index", "native_row")) {
    out[[field]] <- gx_package_hydrate_integer_impl(
      raw[[field]], paste0("observations$", field)
    )
  }
  out$datetime <- gx_snapshot_catalog_view_time_impl(
    raw$datetime, "observations$datetime"
  )
  out$value <- gx_package_hydrate_double_impl(raw$value, "observations$value")
  out$harmonized <- gx_package_hydrate_logical_impl(
    raw$harmonized, "observations$harmonized"
  )
  tibble::as_tibble(out)
}

gx_package_hydrate_parquet_observations_impl <- function(view) {
  path <- "data/observations.parquet"
  position <- match(path, view$loaded$resources$path)
  valid <- !is.na(position) &&
    identical(view$loaded$resources$role[[position]], "observations") &&
    identical(view$loaded$resources$format[[position]], "parquet") &&
    identical(
      view$loaded$resources$media_type[[position]],
      "application/vnd.apache.parquet"
    ) && !is.null(view$loaded$contents[[path]])
  if (!valid) {
    gx_package_hydrate_abort(
      "The package is missing its fixed Parquet observation resource.",
      "gx_error_package_hydrate_shape"
    )
  }
  value <- tryCatch(
    gx_package_parquet_read_impl(view$loaded$contents[[path]]),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(value)) {
    gx_package_hydrate_abort(
      "The package Parquet observations violate the fixed typed profile.",
      "gx_error_package_hydrate_type"
    )
  }
  tibble::as_tibble(value)
}

gx_package_hydrate_harmonized_index_impl <- function(raw) {
  raw <- gx_package_hydrate_schema_impl(
    raw, .gx_harmonized_resource_columns, "data/harmonized_resources.csv"
  )
  out <- raw
  for (field in c(
    "result_index", "native_rows", "native_columns", "observation_count"
  )) {
    out[[field]] <- gx_package_hydrate_integer_impl(
      raw[[field]], paste0("harmonized_resources$", field)
    )
  }
  out$timeseries <- gx_package_hydrate_logical_impl(
    raw$timeseries, "harmonized_resources$timeseries"
  )
  tibble::as_tibble(out)
}

gx_package_hydrate_requests_impl <- function(view) {
  requests <- tryCatch(
    gx_snapshot_requests_from_manifest_impl(
      view$loaded$verification$manifest$requests
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  bytes <- view$loaded$contents[["requests.csv"]]
  rebound <- if (is.null(requests)) NULL else tryCatch(
    gx_snapshot_request_export_bytes_impl(
      gx_snapshot_writer_requests(requests)
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(requests) || is.null(bytes) || !identical(rebound, bytes)) {
    gx_package_hydrate_abort(
      "The package request table does not rebind its manifest ledger.",
      "gx_error_package_hydrate_binding"
    )
  }
  requests
}

gx_package_hydrate_projection_impl <- function(view) {
  gx_package_tables_validate_impl(view)
  tables <- view$tables
  required <- c(
    "catalog/sites.csv", "catalog/datasets.csv", "catalog/problems.csv",
    "requests.csv"
  )
  if (!all(required %in% names(tables))) {
    gx_package_hydrate_abort(
      "The package is missing a required catalog table.",
      "gx_error_package_hydrate_shape"
    )
  }
  catalog <- list(
    sites = gx_snapshot_catalog_view_sites_impl(
      tables[["catalog/sites.csv"]]
    ),
    datasets = gx_snapshot_catalog_view_datasets_impl(
      tables[["catalog/datasets.csv"]]
    ),
    problems = gx_snapshot_catalog_view_problems_impl(
      tables[["catalog/problems.csv"]]
    ),
    requests = gx_package_hydrate_requests_impl(view)
  )
  fetch <- if (view$stage == "catalog") {
    NULL
  } else {
    list(
      status = gx_package_hydrate_fetch_status_impl(
        tables[["catalog/fetch_status.csv"]]
      ),
      resources = gx_package_hydrate_native_index_impl(
        tables[["data/native/index.csv"]]
      )
    )
  }
  harmonized <- if (view$stage != "harmonized") {
    NULL
  } else {
    parquet <- identical(
      view$loaded$verification$manifest$recipe$output$timeseries,
      "parquet"
    )
    list(
      observations = if (parquet) {
        gx_package_hydrate_parquet_observations_impl(view)
      } else {
        gx_package_hydrate_observations_impl(
          tables[["data/observations.csv"]]
        )
      },
      resources = gx_package_hydrate_harmonized_index_impl(
        tables[["data/harmonized_resources.csv"]]
      )
    )
  }
  roles <- view$loaded$resources$role
  native_paths <- view$loaded$resources$path[roles == "native_table"]
  native_tables <- tables[native_paths]
  list(
    catalog = catalog,
    fetch = fetch,
    harmonized = harmonized,
    native_tables = native_tables,
    observations_format = if (view$stage != "harmonized") {
      "not_applicable"
    } else {
      view$loaded$verification$manifest$recipe$output$timeseries
    }
  )
}

gx_package_hydrate_metadata_impl <- function(projection, frictionless = FALSE) {
  list(
    scope = "typed_package_tables_v1",
    offline = TRUE,
    read_only = TRUE,
    typed_package_tables = TRUE,
    redacted = TRUE,
    catalog_tables = unname(as.integer(length(projection$catalog))),
    fetch_tables = unname(as.integer(length(projection$fetch))),
    harmonized_tables = unname(as.integer(length(projection$harmonized))),
    native_tables = unname(as.integer(length(projection$native_tables))),
    observations_format = projection$observations_format,
    arrow = identical(projection$observations_format, "parquet"),
    native_payloads_typed = FALSE,
    reconstructed_objects = FALSE,
    authenticity = FALSE,
    frictionless = frictionless,
    replayable = FALSE
  )
}

gx_package_hydrate_id_impl <- function(view, metadata) {
  gx_contract_hash(
    list(
      "mode", "typed_package_hydration",
      "status", "hydrated_and_verified",
      "path", view$path,
      "stage", view$stage,
      "table_view_id", view$view_id,
      "manifest_sha256", view$loaded$verification$manifest_sha256,
      "catalog_tables", metadata$catalog_tables,
      "fetch_tables", metadata$fetch_tables,
      "harmonized_tables", metadata$harmonized_tables,
      "native_tables", metadata$native_tables,
      "observations_format", metadata$observations_format
    ),
    namespace = "geoconnexr.package-hydrate.v1",
    contract_version = .gx_package_hydrated_contract_version
  )
}

gx_package_hydrated_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_package_hydrated") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_hydrated_fields) &&
    identical(x$contract_version, .gx_package_hydrated_contract_version) &&
    identical(x$mode, "typed_package_hydration") &&
    identical(x$status, "hydrated_and_verified") &&
    is.character(x$stage) && length(x$stage) == 1L &&
    is.character(x$path) && length(x$path) == 1L && nzchar(x$path) &&
    is.character(x$hydration_id) && length(x$hydration_id) == 1L &&
    gx_catalog_is_sha256(x$hydration_id)
  if (!valid_top) {
    gx_package_hydrate_abort(
      "Typed package hydration violates its exact top-level contract."
    )
  }
  view_valid <- tryCatch({
    gx_package_tables_validate_impl(x$table_view)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  projection <- if (view_valid) tryCatch(
    gx_package_hydrate_projection_impl(x$table_view),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  metadata <- if (is.null(projection)) NULL else
    gx_package_hydrate_metadata_impl(
      projection, x$table_view$loaded$metadata$frictionless
    )
  hydration_id <- if (is.null(metadata)) NULL else
    gx_package_hydrate_id_impl(x$table_view, metadata)
  valid <- view_valid && !is.null(projection) &&
    identical(x$stage, x$table_view$stage) &&
    identical(x$path, x$table_view$path) &&
    identical(x$catalog, projection$catalog) &&
    identical(x$fetch, projection$fetch) &&
    identical(x$harmonized, projection$harmonized) &&
    identical(x$native_tables, projection$native_tables) &&
    is.list(x$metadata) &&
    identical(names(x$metadata), .gx_package_hydrated_metadata_fields) &&
    identical(x$metadata, metadata) && identical(x$hydration_id, hydration_id)
  if (!valid) {
    gx_package_hydrate_abort(
      "Typed package hydration no longer binds its canonical table evidence."
    )
  }
  invisible(x)
}

gx_package_hydrate_impl <- function(dir) {
  tryCatch(
    {
      view <- gx_package_tables(dir)
      projection <- gx_package_hydrate_projection_impl(view)
      metadata <- gx_package_hydrate_metadata_impl(
        projection, view$loaded$metadata$frictionless
      )
      object <- structure(
        list(
          contract_version = .gx_package_hydrated_contract_version,
          mode = "typed_package_hydration",
          status = "hydrated_and_verified",
          stage = view$stage,
          path = view$path,
          table_view = view,
          catalog = projection$catalog,
          fetch = projection$fetch,
          harmonized = projection$harmonized,
          native_tables = projection$native_tables,
          metadata = metadata,
          hydration_id = gx_package_hydrate_id_impl(view, metadata)
        ),
        class = "gx_package_hydrated"
      )
      gx_package_hydrated_validate_impl(object)
      object
    },
    error = function(cnd) {
      if (inherits(cnd, "gx_error_package_hydrate")) stop(cnd)
      gx_package_hydrate_abort(
        "Typed package hydration failed closed.",
        "gx_error_package_hydrate_contract"
      )
    }
  )
}

#' Hydrate verified package-owned tables with fixed storage types
#'
#' Loads a package through [gx_package_tables()] and applies only the exact
#' storage types declared by the fixed package profile. Catalog tables receive
#' the same redacted geometry, UTC timestamp, logical, and JSON-array typing as
#' [gx_snapshot_catalog_view()]. Package-owned fetch indexes and harmonization
#' tables receive explicit integer, double, logical, and UTC timestamp types.
#' Parquet observations are read in memory through the reviewed Arrow
#' capability and must reproduce the exact fixed typed schema.
#'
#' Provider-native CSV tables remain character-only and raw provider resources
#' remain opaque in the embedded loading evidence. The result is a redacted,
#' read-only inspection view; it does not reconstruct a live `gx_catalog`,
#' `gx_fetched`, or `gx_harmonized` object, authenticate the unsigned manifest,
#' write, refresh, or replay the package.
#'
#' @param dir Existing package directory created with the fixed public
#'   [gx_package()] profile.
#'
#' @return A validated `gx_package_hydrated` object containing typed
#'   package-owned tables, character-only provider-native tables, the complete
#'   canonical table evidence, fixed scope metadata, and deterministic
#'   hydration identity.
#' @export
gx_package_hydrate <- function(dir) {
  value <- gx_package_hydrate_impl(dir)
  gx_package_hydrated_validate_impl(value)
  value
}

#' @export
print.gx_package_hydrated <- function(x, ...) {
  gx_package_hydrated_validate_impl(x)
  cli::cli_inform(c(
    "<gx_package_hydrated>",
    "* Status: {x$status}",
    "* Source stage: {x$stage}",
    paste0(
      "* Typed package tables: ",
      x$metadata$catalog_tables + x$metadata$fetch_tables +
        x$metadata$harmonized_tables,
      "; character-only native tables: ", x$metadata$native_tables
    ),
    "* Observations format: {x$metadata$observations_format}",
    "* Scope: redacted; read-only; non-replayable",
    "* Reconstruction: no live workflow objects"
  ))
  invisible(x)
}
