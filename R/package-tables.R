.gx_package_tables_contract_version <- "0.2.0"

.gx_package_tables_fields <- c(
  "contract_version", "mode", "status", "stage", "path", "loaded",
  "tables", "metadata", "view_id"
)

.gx_package_tables_metadata_fields <- c(
  "scope", "read_only", "canonical", "character_only", "csv_tables",
  "parquet_resources", "raw_resources", "rows", "columns", "reconstructed_objects",
  "authenticity", "frictionless", "replayable"
)

gx_package_tables_abort <- function(
    message,
    class = "gx_error_package_tables_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_tables", "gx_error_package",
      "gx_error_snapshot"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_table_parse_impl <- function(bytes) {
  valid_input <- is.raw(bytes) && !is.object(bytes) &&
    is.null(attributes(bytes)) && length(bytes) >= 1L &&
    length(bytes) <= .gx_package_resources_max_resource_bytes
  if (!valid_input) {
    gx_package_tables_abort(
      "Package CSV parsing requires one exact bounded raw vector.",
      "gx_error_package_tables_input"
    )
  }
  text <- tryCatch(
    rawToChar(bytes),
    error = function(cnd) NULL
  )
  utf8_bom <- length(bytes) >= 3L &&
    identical(as.integer(bytes[seq_len(3L)]), c(239L, 187L, 191L))
  if (is.null(text) || !isTRUE(stringi::stri_enc_isutf8(text)) || utf8_bom ||
      !endsWith(text, "\n") || grepl("\r", text, fixed = TRUE)) {
    gx_package_tables_abort(
      "Package CSV bytes must be unmarked UTF-8 with LF line endings.",
      "gx_error_package_tables_encoding"
    )
  }
  policy <- list(
    max_input_bytes = .gx_package_resources_max_resource_bytes,
    max_field_bytes = .gx_package_resources_max_resource_bytes,
    max_header_name_bytes = .gx_package_resources_max_resource_bytes,
    max_header_bytes = .gx_package_resources_max_resource_bytes,
    max_fields = .gx_package_resources_max_fields,
    request_max_rows = .gx_package_resources_max_fields,
    request_max_columns = gx_snapshot_csv_load_max_columns,
    implementation_max_rows = .gx_package_resources_max_fields,
    implementation_max_columns = gx_snapshot_csv_load_max_columns
  )
  parsed <- tryCatch(
    {
      scan <- gx_csv_parsed_response_scan_impl(bytes, policy)
      gx_csv_parsed_response_data_impl(bytes, scan, policy)
    },
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  fields <- if (is.null(parsed)) {
    Inf
  } else {
    as.double(nrow(parsed)) * as.double(ncol(parsed))
  }
  if (is.null(parsed) || ncol(parsed) < 1L ||
      ncol(parsed) > gx_snapshot_csv_load_max_columns ||
      fields > .gx_package_resources_max_fields ||
      !all(vapply(parsed, is.character, logical(1)))) {
    gx_package_tables_abort(
      "Package CSV bytes violate the bounded character-table profile.",
      "gx_error_package_tables_shape"
    )
  }
  parsed <- tibble::as_tibble(parsed)
  canonical <- tryCatch(
    gx_snapshot_csv_bytes_impl(
      parsed,
      max_bytes = .gx_package_resources_max_resource_bytes,
      max_columns = gx_snapshot_csv_load_max_columns,
      max_fields = .gx_package_resources_max_fields
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(canonical) || !identical(canonical, bytes)) {
    gx_package_tables_abort(
      "Package CSV bytes are not their canonical parsed-table projection.",
      "gx_error_package_tables_canonical"
    )
  }
  parsed
}

gx_package_tables_build_impl <- function(loaded) {
  gx_package_loaded_validate_impl(loaded)
  positions <- which(loaded$resources$format == "csv")
  tables <- lapply(
    positions,
    function(position) {
      gx_package_table_parse_impl(loaded$contents[[position]])
    }
  )
  names(tables) <- loaded$resources$path[positions]
  tables
}

gx_package_tables_metadata_impl <- function(loaded, tables) {
  list(
    scope = "canonical_package_tables_v1",
    read_only = TRUE,
    canonical = TRUE,
    character_only = TRUE,
    csv_tables = unname(as.integer(length(tables))),
    parquet_resources = unname(as.integer(sum(
      loaded$resources$format == "parquet"
    ))),
    raw_resources = unname(as.integer(sum(
      loaded$resources$format == "raw"
    ))),
    rows = unname(as.double(sum(vapply(tables, nrow, integer(1))))),
    columns = unname(as.double(sum(vapply(tables, ncol, integer(1))))),
    reconstructed_objects = FALSE,
    authenticity = FALSE,
    frictionless = loaded$metadata$frictionless,
    replayable = FALSE
  )
}

gx_package_tables_id_impl <- function(loaded, tables, metadata) {
  positions <- match(names(tables), loaded$resources$path)
  evidence <- vapply(seq_along(tables), function(index) {
    paste(
      names(tables)[[index]],
      loaded$resources$sha256[[positions[[index]]]],
      nrow(tables[[index]]),
      ncol(tables[[index]]),
      sep = "="
    )
  }, character(1), USE.NAMES = FALSE)
  gx_contract_hash(
    list(
      "mode", "canonical_package_tables",
      "status", "parsed_and_verified",
      "path", loaded$path,
      "stage", loaded$stage,
      "load_id", loaded$load_id,
      "manifest_sha256", loaded$verification$manifest_sha256,
      "csv_tables", metadata$csv_tables,
      "parquet_resources", metadata$parquet_resources,
      "raw_resources", metadata$raw_resources,
      "table_evidence_sha256", gx_package_input_vector_hash_impl(
        evidence,
        "geoconnexr.package-tables.evidence.v1"
      )
    ),
    namespace = "geoconnexr.package-tables.v1",
    contract_version = .gx_package_tables_contract_version
  )
}

gx_package_tables_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_package_tables") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_tables_fields) &&
    identical(
      x$contract_version,
      .gx_package_tables_contract_version
    ) &&
    identical(x$mode, "canonical_package_tables") &&
    identical(x$status, "parsed_and_verified") &&
    is.character(x$stage) && length(x$stage) == 1L &&
    !is.na(x$stage) && is.null(attributes(x$stage)) &&
    is.character(x$path) && length(x$path) == 1L &&
    !is.na(x$path) && is.null(attributes(x$path)) && nzchar(x$path) &&
    is.character(x$view_id) && length(x$view_id) == 1L &&
    !is.na(x$view_id) && is.null(attributes(x$view_id)) &&
    gx_catalog_is_sha256(x$view_id)
  if (!valid_top) {
    gx_package_tables_abort(
      "Package-table evidence violates its exact top-level contract."
    )
  }
  loaded_valid <- tryCatch({
    gx_package_loaded_validate_impl(x$loaded)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  expected_tables <- if (loaded_valid) {
    tryCatch(
      gx_package_tables_build_impl(x$loaded),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
  } else {
    NULL
  }
  valid_tables <- is.list(x$tables) && !is.null(expected_tables) &&
    identical(x$tables, expected_tables)
  expected_metadata <- if (loaded_valid && valid_tables) {
    gx_package_tables_metadata_impl(x$loaded, x$tables)
  } else {
    NULL
  }
  expected_id <- if (!is.null(expected_metadata)) {
    gx_package_tables_id_impl(x$loaded, x$tables, expected_metadata)
  } else {
    NULL
  }
  valid <- loaded_valid &&
    identical(x$stage, x$loaded$stage) &&
    identical(x$path, x$loaded$path) &&
    valid_tables &&
    is.list(x$metadata) &&
    identical(names(x$metadata), .gx_package_tables_metadata_fields) &&
    identical(x$metadata, expected_metadata) &&
    !is.null(expected_id) && identical(x$view_id, expected_id)
  if (!valid) {
    gx_package_tables_abort(
      "Package-table evidence no longer binds its verified canonical bytes."
    )
  }
  invisible(x)
}

#' Parse verified package CSV resources as canonical character tables
#'
#' Loads a fixed-profile package with [gx_package_load()] and parses every CSV
#' resource from its already verified in-memory bytes. Parsing uses the same
#' strict bytewise CSV implementation as snapshot loading and admits only
#' quote-all canonical UTF-8/LF tables under the package resource, column, and
#' field ceilings.
#'
#' All CSV columns remain character vectors. Parquet and native raw resources
#' remain available only through the embedded `gx_package_loaded` object. This
#' function does not reconstruct a live catalog, fetched result, or harmonized
#' result, infer types or semantics, authenticate the manifest, write, refresh,
#' or replay.
#'
#' @param dir Existing package directory created with the fixed public
#'   [gx_package()] profile.
#'
#' @return A validated `gx_package_tables` object containing the embedded
#'   byte-preserving load evidence, path-named canonical character tables,
#'   fixed scope metadata, and deterministic table-view identity.
#' @export
gx_package_tables <- function(dir) {
  tryCatch(
    {
      loaded <- gx_package_load(dir)
      tables <- gx_package_tables_build_impl(loaded)
      metadata <- gx_package_tables_metadata_impl(loaded, tables)
      object <- structure(
        list(
          contract_version = .gx_package_tables_contract_version,
          mode = "canonical_package_tables",
          status = "parsed_and_verified",
          stage = loaded$stage,
          path = loaded$path,
          loaded = loaded,
          tables = tables,
          metadata = metadata,
          view_id = gx_package_tables_id_impl(loaded, tables, metadata)
        ),
        class = "gx_package_tables"
      )
      gx_package_tables_validate_impl(object)
      object
    },
    error = function(cnd) {
      if (inherits(cnd, "gx_error_package_tables")) stop(cnd)
      gx_package_tables_abort(
        "Package table loading failed closed.",
        "gx_error_package_tables_contract"
      )
    }
  )
}

#' @export
print.gx_package_tables <- function(x, ...) {
  gx_package_tables_validate_impl(x)
  cli::cli_inform(c(
    "<gx_package_tables>",
    "* Status: {x$status}",
    "* Source stage: {x$stage}",
    paste0(
      "* Canonical CSV tables: ", x$metadata$csv_tables,
      "; Parquet resources: ", x$metadata$parquet_resources,
      "; opaque raw resources: ", x$metadata$raw_resources
    ),
    "* Scope: character-only; read-only; non-replayable",
    "* Assurance: manifest-bound bytes; unsigned"
  ))
  invisible(x)
}
