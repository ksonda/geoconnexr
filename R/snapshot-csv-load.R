gx_snapshot_csv_load_max_bytes <- 64 * 1024^2
gx_snapshot_csv_load_max_columns <- 128L
gx_snapshot_csv_load_max_fields <- 1000000

gx_snapshot_csv_load_abort <- function(
    message,
    subclass = "gx_error_snapshot_csv_load_contract",
    call = rlang::caller_env()) {
  gx_abort(
    message,
    c(subclass, "gx_error_snapshot_csv_load", "gx_error_snapshot"),
    call = call,
    .redact_trace = TRUE
  )
}

gx_snapshot_csv_bytes_impl <- function(
    x,
    max_bytes = gx_snapshot_csv_load_max_bytes,
    max_columns = gx_snapshot_csv_load_max_columns,
    max_fields = gx_snapshot_csv_load_max_fields) {
  valid_limits <- is.numeric(max_bytes) && length(max_bytes) == 1L &&
    !is.na(max_bytes) && is.finite(max_bytes) && max_bytes >= 1 &&
    is.numeric(max_columns) && length(max_columns) == 1L &&
    !is.na(max_columns) && is.finite(max_columns) &&
    max_columns >= 1 && max_columns == floor(max_columns) &&
    is.numeric(max_fields) && length(max_fields) == 1L &&
    !is.na(max_fields) && is.finite(max_fields) &&
    max_fields >= 0 && max_fields == floor(max_fields)
  valid <- valid_limits && inherits(x, "data.frame") &&
    !is.null(names(x)) && !anyNA(names(x)) &&
    all(nzchar(names(x))) && !anyDuplicated(names(x)) &&
    ncol(x) > 0L && ncol(x) <= max_columns &&
    nrow(x) >= 0L &&
    as.double(nrow(x)) * as.double(ncol(x)) <= max_fields &&
    all(vapply(x, function(column) {
      is.character(column) && length(column) == nrow(x)
    }, logical(1)))
  if (!valid) {
    gx_snapshot_csv_load_abort(
      "A snapshot CSV table violates its exact character-table contract.",
      "gx_error_snapshot_csv_load_shape"
    )
  }
  gx_snapshot_writer_validate_text(x)
  lines <- vector("character", nrow(x) + 1L)
  lines[[1L]] <- paste(gx_snapshot_writer_quote(names(x)), collapse = ",")
  if (nrow(x)) {
    for (index in seq_len(nrow(x))) {
      values <- vapply(x[index, , drop = FALSE], `[[`, character(1), 1L)
      lines[[index + 1L]] <- paste(
        gx_snapshot_writer_quote(values),
        collapse = ","
      )
    }
  }
  text <- paste0(paste(lines, collapse = "\n"), "\n")
  size <- nchar(enc2utf8(text), type = "bytes")
  if (!is.finite(size) || size > max_bytes) {
    gx_snapshot_csv_load_abort(
      "A canonical snapshot CSV exceeds its loading byte ceiling.",
      "gx_error_snapshot_csv_load_budget"
    )
  }
  charToRaw(enc2utf8(text))
}

gx_snapshot_csv_parse_impl <- function(
    bytes,
    expected_names,
    max_rows) {
  valid_input <- is.raw(bytes) && !is.object(bytes) &&
    is.null(attributes(bytes)) && length(bytes) >= 1L &&
    length(bytes) <= gx_snapshot_csv_load_max_bytes &&
    is.character(expected_names) && length(expected_names) >= 1L &&
    length(expected_names) <= gx_snapshot_csv_load_max_columns &&
    !anyNA(expected_names) && all(nzchar(expected_names)) &&
    !anyDuplicated(expected_names) &&
    is.numeric(max_rows) && length(max_rows) == 1L &&
    !is.na(max_rows) && is.finite(max_rows) &&
    max_rows >= 0 && max_rows == floor(max_rows)
  if (!valid_input) {
    gx_snapshot_csv_load_abort(
      "Snapshot CSV parsing requires exact bounded bytes and schema limits.",
      "gx_error_snapshot_csv_load_input"
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
    gx_snapshot_csv_load_abort(
      "Snapshot CSV bytes must be unmarked UTF-8 with LF line endings.",
      "gx_error_snapshot_csv_load_encoding"
    )
  }
  policy <- list(
    max_input_bytes = gx_snapshot_csv_load_max_bytes,
    max_field_bytes = gx_snapshot_csv_load_max_bytes,
    max_header_name_bytes = gx_snapshot_csv_load_max_bytes,
    max_header_bytes = gx_snapshot_csv_load_max_bytes,
    max_fields = gx_snapshot_csv_load_max_fields,
    request_max_rows = max_rows,
    request_max_columns = length(expected_names),
    implementation_max_rows = max_rows,
    implementation_max_columns = length(expected_names)
  )
  parsed <- tryCatch(
    {
      scan <- gx_csv_parsed_response_scan_impl(bytes, policy)
      gx_csv_parsed_response_data_impl(bytes, scan, policy)
    },
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(parsed) || !identical(names(parsed), expected_names) ||
      nrow(parsed) > max_rows ||
      as.double(nrow(parsed)) * as.double(ncol(parsed)) >
        gx_snapshot_csv_load_max_fields ||
      !all(vapply(parsed, is.character, logical(1)))) {
    gx_snapshot_csv_load_abort(
      "Snapshot CSV bytes do not satisfy their declared table schema.",
      "gx_error_snapshot_csv_load_shape"
    )
  }
  parsed <- tibble::as_tibble(parsed)
  canonical <- gx_snapshot_csv_bytes_impl(parsed)
  if (!identical(canonical, bytes)) {
    gx_snapshot_csv_load_abort(
      "Snapshot CSV bytes are not their canonical parsed-table projection.",
      "gx_error_snapshot_csv_load_canonical"
    )
  }
  parsed
}

gx_snapshot_catalog_csv_columns <- list(
  sites = append(
    setdiff(.gx_catalog_site_columns, "geometry"),
    "site_uri_sha256",
    after = 2L
  ) |>
    append("geometry_wkt"),
  datasets = append(
    .gx_catalog_dataset_columns,
    "site_uri_sha256",
    after = 2L
  ) |>
    append("variable_id_sha256", after = 6L),
  problems = .gx_catalog_problem_columns
)

gx_snapshot_catalog_csv_roles <- list(
  sites = c("catalog", "sites"),
  datasets = c("catalog", "datasets"),
  problems = c("catalog", "diagnostics")
)

gx_snapshot_catalog_csv_max_rows <- c(
  sites = .gx_catalog_max_sites,
  datasets = .gx_catalog_max_datasets,
  problems = .gx_catalog_max_problems
)

gx_snapshot_csv_read_raw_impl <- function(root, resource) {
  size <- as.numeric(resource$bytes)
  if (!is.finite(size) || size < 1 ||
      size > gx_snapshot_csv_load_max_bytes ||
      size > .Machine$integer.max - 1) {
    gx_snapshot_csv_load_abort(
      "The snapshot CSV exceeds its loading byte ceiling.",
      "gx_error_snapshot_csv_load_budget"
    )
  }
  path <- file.path(root, resource$path)
  before <- gx_snapshot_assert_fs_type(path, "file")
  if (!identical(as.numeric(before$size[[1L]]), size)) {
    gx_snapshot_csv_load_abort(
      "The snapshot CSV size changed before loading.",
      "gx_error_snapshot_csv_load_mutation"
    )
  }
  bytes <- tryCatch(
    readBin(path, what = "raw", n = as.integer(size + 1)),
    warning = function(cnd) NULL,
    error = function(cnd) NULL
  )
  after <- gx_snapshot_assert_fs_type(path, "file")
  if (is.null(bytes) || length(bytes) != size) {
    gx_snapshot_csv_load_abort(
      "The snapshot CSV could not be read as exact bounded bytes.",
      "gx_error_snapshot_csv_load_io"
    )
  }
  tryCatch(
    gx_snapshot_assert_same_info(before, after),
    error = function(cnd) {
      gx_snapshot_csv_load_abort(
        "The snapshot CSV changed while it was being loaded.",
        "gx_error_snapshot_csv_load_mutation"
      )
    }
  )
  bytes
}

gx_snapshot_catalog_csv_profile_impl <- function(verification, resource) {
  gx_snapshot_request_profile_impl(verification)
  paths <- vapply(
    verification$manifest$resources,
    `[[`,
    character(1),
    "path"
  )
  position <- match(gx_snapshot_writer_paths[[resource]], paths)
  if (is.na(position)) {
    gx_snapshot_csv_load_abort(
      "The catalog CSV resource is absent from the fixed writer profile.",
      "gx_error_snapshot_csv_load_profile"
    )
  }
  declaration <- verification$manifest$resources[[position]]
  valid <- identical(declaration$media_type, "text/csv") &&
    identical(declaration$required, TRUE) &&
    identical(
      unname(unlist(declaration$roles, use.names = FALSE)),
      gx_snapshot_catalog_csv_roles[[resource]]
    )
  if (!valid) {
    gx_snapshot_csv_load_abort(
      "The catalog CSV resource has an unsupported declaration.",
      "gx_error_snapshot_csv_load_profile"
    )
  }
  declaration
}

gx_snapshot_catalog_csv_id_impl <- function(
    resource,
    path,
    table,
    manifest_sha256,
    resource_sha256) {
  bytes <- gx_snapshot_csv_bytes_impl(table)
  gx_contract_hash(
    list(
      "mode", "redacted_catalog_csv",
      "resource", resource,
      "path", path,
      "manifest_sha256", manifest_sha256,
      "resource_sha256", resource_sha256,
      "canonical_bytes_sha256",
      digest::digest(bytes, algo = "sha256", serialize = FALSE),
      "rows", as.integer(nrow(table)),
      "columns", as.integer(ncol(table))
    ),
    namespace = "geoconnexr.snapshot-catalog-csv.v1",
    contract_version = "0.1.0"
  )
}

gx_snapshot_catalog_csv_validate_impl <- function(x) {
  valid_top <- is.list(x) &&
    identical(class(x), "gx_snapshot_catalog_csv") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(
      names(x),
      c(
        "contract_version", "mode", "status", "resource", "path",
        "table", "rows", "columns", "manifest_sha256", "resource_sha256",
        "table_id"
      )
    ) &&
    identical(x$contract_version, "0.1.0") &&
    identical(x$mode, "redacted_catalog_csv") &&
    identical(x$status, "loaded_and_bound") &&
    is.character(x$resource) && length(x$resource) == 1L &&
    !is.na(x$resource) &&
    x$resource %in% names(gx_snapshot_catalog_csv_columns) &&
    is.character(x$path) && length(x$path) == 1L && !is.na(x$path) &&
    is.integer(x$rows) && length(x$rows) == 1L && !is.na(x$rows) &&
    is.integer(x$columns) && length(x$columns) == 1L && !is.na(x$columns) &&
    isTRUE(gx_catalog_is_sha256(x$manifest_sha256)) &&
    isTRUE(gx_catalog_is_sha256(x$resource_sha256)) &&
    isTRUE(gx_catalog_is_sha256(x$table_id))
  root <- if (valid_top) {
    tryCatch(
      gx_snapshot_root(x$path),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
  } else {
    NULL
  }
  valid_table <- valid_top &&
    inherits(x$table, "tbl_df") &&
    identical(class(x$table), c("tbl_df", "tbl", "data.frame")) &&
    identical(names(x$table), gx_snapshot_catalog_csv_columns[[x$resource]]) &&
    nrow(x$table) <= gx_snapshot_catalog_csv_max_rows[[x$resource]] &&
    all(vapply(x$table, is.character, logical(1)))
  expected_id <- if (valid_table) {
    tryCatch(
      gx_snapshot_catalog_csv_id_impl(
        x$resource,
        x$path,
        x$table,
        x$manifest_sha256,
        x$resource_sha256
      ),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
  } else {
    NULL
  }
  if (!valid_top || is.null(root) || !identical(root$path, x$path) ||
      !valid_table || !identical(x$rows, as.integer(nrow(x$table))) ||
      !identical(x$columns, as.integer(ncol(x$table))) ||
      is.null(expected_id) || !identical(x$table_id, expected_id) ||
      !identical(
        digest::digest(
          gx_snapshot_csv_bytes_impl(x$table),
          algo = "sha256",
          serialize = FALSE
        ),
        x$resource_sha256
      )) {
    gx_snapshot_csv_load_abort(
      "Catalog CSV evidence violates its exact binding contract.",
      "gx_error_snapshot_csv_load_evidence"
    )
  }
  invisible(x)
}

# Internal M9g boundary. It loads one fixed redacted catalog CSV as an exact
# character table. It does not restore missing values, parse WKT/JSON/time
# fields, reconstruct gx_catalog identities, or authorize replay.
gx_snapshot_load_catalog_csv_impl <- function(dir, resource) {
  tryCatch(
    {
      valid_resource <- is.character(resource) && length(resource) == 1L &&
        is.null(attributes(resource)) && !is.na(resource) &&
        resource %in% names(gx_snapshot_catalog_csv_columns)
      if (!valid_resource) {
        gx_snapshot_csv_load_abort(
          "Catalog CSV loading requires one fixed resource name.",
          "gx_error_snapshot_csv_load_input"
        )
      }
      before <- gx_snapshot_verify_impl(dir)
      declaration <- gx_snapshot_catalog_csv_profile_impl(
        before,
        resource
      )
      path <- normalizePath(dir, winslash = "/", mustWork = TRUE)
      bytes <- gx_snapshot_csv_read_raw_impl(path, declaration)
      if (!identical(
        digest::digest(bytes, algo = "sha256", serialize = FALSE),
        declaration$sha256
      )) {
        gx_snapshot_csv_load_abort(
          "The catalog CSV no longer matches its verified resource hash.",
          "gx_error_snapshot_csv_load_mutation"
        )
      }
      table <- gx_snapshot_csv_parse_impl(
        bytes,
        gx_snapshot_catalog_csv_columns[[resource]],
        gx_snapshot_catalog_csv_max_rows[[resource]]
      )
      after <- gx_snapshot_verify_impl(dir)
      if (!identical(before$manifest_sha256, after$manifest_sha256) ||
          !identical(before$resources, after$resources)) {
        gx_snapshot_csv_load_abort(
          "The snapshot changed while its catalog CSV was being loaded.",
          "gx_error_snapshot_csv_load_mutation"
        )
      }
      object <- structure(
        list(
          contract_version = "0.1.0",
          mode = "redacted_catalog_csv",
          status = "loaded_and_bound",
          resource = resource,
          path = path,
          table = table,
          rows = as.integer(nrow(table)),
          columns = as.integer(ncol(table)),
          manifest_sha256 = before$manifest_sha256,
          resource_sha256 = declaration$sha256,
          table_id = gx_snapshot_catalog_csv_id_impl(
            resource,
            path,
            table,
            before$manifest_sha256,
            declaration$sha256
          )
        ),
        class = "gx_snapshot_catalog_csv"
      )
      gx_snapshot_catalog_csv_validate_impl(object)
      object
    },
    error = function(cnd) {
      if (inherits(cnd, "gx_error_snapshot_csv_load")) stop(cnd)
      if (inherits(cnd, "gx_error_snapshot")) stop(cnd)
      gx_snapshot_csv_load_abort(
        "Snapshot catalog CSV loading failed closed.",
        "gx_error_snapshot_csv_load_contract"
      )
    }
  )
}
