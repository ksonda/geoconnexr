gx_snapshot_request_load_max_bytes <- 64 * 1024^2

gx_snapshot_request_load_abort <- function(
    message,
    subclass = "gx_error_snapshot_request_load_contract",
    call = rlang::caller_env()) {
  gx_abort(
    message,
    c(subclass, "gx_error_snapshot_request_load", "gx_error_snapshot"),
    call = call,
    .redact_trace = TRUE
  )
}

gx_snapshot_request_export_bytes_impl <- function(requests) {
  requests <- gx_snapshot_validate_requests(requests)
  view <- gx_snapshot_writer_requests_view(requests)
  view <- gx_snapshot_writer_redact_view(
    gx_snapshot_writer_character_view(view)
  )
  tryCatch(
    gx_snapshot_csv_bytes_impl(
      view,
      max_bytes = gx_snapshot_request_load_max_bytes
    ),
    gx_error_snapshot_csv_load = function(cnd) {
      gx_snapshot_request_load_abort(
        "The canonical request export exceeds or violates its loading contract.",
        if (inherits(cnd, "gx_error_snapshot_csv_load_budget")) {
          "gx_error_snapshot_request_load_budget"
        } else {
          "gx_error_snapshot_request_load_contract"
        }
      )
    }
  )
}

gx_snapshot_request_profile_impl <- function(verification) {
  manifest <- verification$manifest
  options <- manifest$effective_options
  serialization <- if (is.list(options)) options$serialization else NULL
  expected_serialization <- list(
    csv = gx_snapshot_writer_csv_profile,
    request_export = "manifest-requests-csv-v1",
    writer = gx_snapshot_writer_profile
  )
  paths <- vapply(
    manifest$resources,
    `[[`,
    character(1),
    "path"
  )
  request_position <- match(
    gx_snapshot_writer_paths[["requests"]],
    paths
  )
  expected_paths <- sort(
    unname(gx_snapshot_writer_paths),
    method = "radix"
  )
  valid <- identical(verification$status, "verified") &&
    identical(manifest$package$name, "geoconnexr") &&
    identical(manifest$recipe$pipeline$end_stage, "catalog") &&
    identical(manifest$replay$replayable, FALSE) &&
    identical(
      unname(unlist(
        manifest$replay$non_replayable_reasons,
        use.names = FALSE
      )),
      "catalog_only_writer_v0_1"
    ) &&
    is.list(options) &&
    identical(options$catalog_contract_version, .gx_catalog_contract_version) &&
    identical(serialization, expected_serialization) &&
    identical(sort(paths, method = "radix"), expected_paths) &&
    !is.na(request_position)
  if (!valid) {
    gx_snapshot_request_load_abort(
      "The snapshot does not declare the exact catalog request-export profile.",
      "gx_error_snapshot_request_load_profile"
    )
  }
  resource <- manifest$resources[[request_position]]
  expected_roles <- c(
    "request-ledger-export",
    "request-ledger-export-v1"
  )
  valid_resource <- identical(resource$media_type, "text/csv") &&
    identical(resource$required, TRUE) &&
    identical(
      unname(unlist(resource$roles, use.names = FALSE)),
      expected_roles
    )
  if (!valid_resource) {
    gx_snapshot_request_load_abort(
      "The request export has an unsupported resource declaration.",
      "gx_error_snapshot_request_load_profile"
    )
  }
  list(position = request_position, resource = resource)
}

gx_snapshot_request_read_raw_impl <- function(root, resource) {
  size <- as.numeric(resource$bytes)
  if (!is.finite(size) || size < 1 ||
      size > gx_snapshot_request_load_max_bytes ||
      size > .Machine$integer.max - 1) {
    gx_snapshot_request_load_abort(
      "The request export exceeds its loading byte ceiling.",
      "gx_error_snapshot_request_load_budget"
    )
  }
  path <- file.path(root, resource$path)
  before <- gx_snapshot_assert_fs_type(path, "file")
  if (!identical(as.numeric(before$size[[1L]]), size)) {
    gx_snapshot_request_load_abort(
      "The request export size changed before loading.",
      "gx_error_snapshot_request_load_mutation"
    )
  }
  bytes <- tryCatch(
    readBin(path, what = "raw", n = as.integer(size + 1)),
    warning = function(cnd) NULL,
    error = function(cnd) NULL
  )
  after <- gx_snapshot_assert_fs_type(path, "file")
  if (is.null(bytes) || length(bytes) != size) {
    gx_snapshot_request_load_abort(
      "The request export could not be read as exact bounded bytes.",
      "gx_error_snapshot_request_load_io"
    )
  }
  tryCatch(
    gx_snapshot_assert_same_info(before, after),
    error = function(cnd) {
      gx_snapshot_request_load_abort(
        "The request export changed while it was being loaded.",
        "gx_error_snapshot_request_load_mutation"
      )
    }
  )
  bytes
}

gx_snapshot_request_column_impl <- function(requests, field, type) {
  extract <- function(item) {
    value <- item[[field]]
    if (is.null(value)) {
      return(switch(
        type,
        character = NA_character_,
        integer = NA_integer_,
        double = NA_real_
      ))
    }
    switch(
      type,
      character = as.character(value),
      integer = as.integer(value),
      double = as.numeric(value)
    )
  }
  vapply(
    requests,
    extract,
    switch(
      type,
      character = character(1),
      integer = integer(1),
      double = numeric(1)
    )
  )
}

gx_snapshot_request_time_impl <- function(x) {
  value <- as.POSIXct(
    x,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )
  if (anyNA(value) || !identical(attr(value, "tzone"), "UTC")) {
    gx_snapshot_request_load_abort(
      "A request-export timestamp could not be loaded exactly.",
      "gx_error_snapshot_request_load_parse"
    )
  }
  value
}

gx_snapshot_requests_from_manifest_impl <- function(requests) {
  requests <- gx_snapshot_validate_requests(requests)
  if (!length(requests)) return(gx_catalog_empty_requests())
  character_fields <- setdiff(
    gx_snapshot_writer_request_fields,
    c(
      "response_status", "encoded_bytes", "decoded_bytes",
      "retrieved_at", "elapsed_ms"
    )
  )
  columns <- lapply(character_fields, function(field) {
    gx_snapshot_request_column_impl(requests, field, "character")
  })
  names(columns) <- character_fields
  columns$response_status <- gx_snapshot_request_column_impl(
    requests, "response_status", "integer"
  )
  columns$encoded_bytes <- gx_snapshot_request_column_impl(
    requests, "encoded_bytes", "integer"
  )
  columns$decoded_bytes <- gx_snapshot_request_column_impl(
    requests, "decoded_bytes", "integer"
  )
  columns$retrieved_at <- gx_snapshot_request_time_impl(vapply(
    requests,
    `[[`,
    character(1),
    "retrieved_at"
  ))
  columns$elapsed_ms <- gx_snapshot_request_column_impl(
    requests, "elapsed_ms", "double"
  )
  columns <- columns[gx_snapshot_writer_request_fields]
  out <- tibble::as_tibble(columns)
  gx_catalog_validate_requests(out)
  rebound <- gx_snapshot_writer_requests(out)
  if (!identical(rebound, requests)) {
    gx_snapshot_request_load_abort(
      "The typed request export does not rebind its manifest ledger.",
      "gx_error_snapshot_request_load_binding"
    )
  }
  out
}

gx_snapshot_request_export_id_impl <- function(
    requests,
    manifest_sha256,
    resource_sha256,
    path) {
  normalized <- gx_snapshot_writer_requests(requests)
  bytes <- gx_snapshot_request_export_bytes_impl(normalized)
  bytes_sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  gx_contract_hash(
    list(
      "mode", "catalog_request_export",
      "path", path,
      "manifest_sha256", manifest_sha256,
      "resource_sha256", resource_sha256,
      "canonical_bytes_sha256", bytes_sha256,
      "requests", as.integer(nrow(requests))
    ),
    namespace = "geoconnexr.snapshot-request-export.v1",
    contract_version = "0.1.0"
  )
}

gx_snapshot_request_export_validate_impl <- function(x) {
  valid_top <- is.list(x) &&
    identical(class(x), "gx_snapshot_request_export") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(
      names(x),
      c(
        "contract_version", "mode", "status", "path", "requests",
        "request_count", "manifest_sha256", "resource_sha256", "export_id"
      )
    ) &&
    identical(x$contract_version, "0.1.0") &&
    identical(x$mode, "catalog_request_export") &&
    identical(x$status, "loaded_and_bound") &&
    is.character(x$path) && length(x$path) == 1L && !is.na(x$path) &&
    is.integer(x$request_count) && length(x$request_count) == 1L &&
    !is.na(x$request_count) && x$request_count >= 0L &&
    isTRUE(gx_catalog_is_sha256(x$manifest_sha256)) &&
    isTRUE(gx_catalog_is_sha256(x$resource_sha256)) &&
    isTRUE(gx_catalog_is_sha256(x$export_id))
  valid_requests <- tryCatch({
    gx_catalog_validate_requests(x$requests)
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
  expected_id <- if (valid_requests) {
    tryCatch(
      gx_snapshot_request_export_id_impl(
        x$requests,
        x$manifest_sha256,
        x$resource_sha256,
        x$path
      ),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
  } else {
    NULL
  }
  if (!valid_top || is.null(root) || !identical(root$path, x$path) ||
      !valid_requests ||
      !identical(x$request_count, as.integer(nrow(x$requests))) ||
      is.null(expected_id) || !identical(x$export_id, expected_id)) {
    gx_snapshot_request_load_abort(
      "Request-export evidence violates its exact binding contract.",
      "gx_error_snapshot_request_load_evidence"
    )
  }
  invisible(x)
}

# Internal M9e boundary. It recognizes only the M9b catalog writer profile,
# proves requests.csv is the canonical byte projection of the manifest ledger,
# and returns the ledger as the exact typed catalog request table.
gx_snapshot_load_requests_impl <- function(dir) {
  tryCatch(
    {
      before <- gx_snapshot_verify_impl(dir)
      profile <- gx_snapshot_request_profile_impl(before)
      expected <- gx_snapshot_request_export_bytes_impl(
        before$manifest$requests
      )
      actual <- gx_snapshot_request_read_raw_impl(
        normalizePath(dir, winslash = "/", mustWork = TRUE),
        profile$resource
      )
      if (!identical(actual, expected) ||
          !identical(
            digest::digest(actual, algo = "sha256", serialize = FALSE),
            profile$resource$sha256
          )) {
        gx_snapshot_request_load_abort(
          "The request export is not the canonical projection of its manifest ledger.",
          "gx_error_snapshot_request_load_binding"
        )
      }
      requests <- gx_snapshot_requests_from_manifest_impl(
        before$manifest$requests
      )
      after <- gx_snapshot_verify_impl(dir)
      if (!identical(before$manifest_sha256, after$manifest_sha256) ||
          !identical(before$resources, after$resources)) {
        gx_snapshot_request_load_abort(
          "The snapshot changed while its request export was being loaded.",
          "gx_error_snapshot_request_load_mutation"
        )
      }
      path <- normalizePath(dir, winslash = "/", mustWork = TRUE)
      object <- structure(
        list(
          contract_version = "0.1.0",
          mode = "catalog_request_export",
          status = "loaded_and_bound",
          path = path,
          requests = requests,
          request_count = as.integer(nrow(requests)),
          manifest_sha256 = before$manifest_sha256,
          resource_sha256 = profile$resource$sha256,
          export_id = gx_snapshot_request_export_id_impl(
            requests,
            before$manifest_sha256,
            profile$resource$sha256,
            path
          )
        ),
        class = "gx_snapshot_request_export"
      )
      gx_snapshot_request_export_validate_impl(object)
      object
    },
    error = function(cnd) {
      if (inherits(cnd, "gx_error_snapshot_request_load")) stop(cnd)
      if (inherits(cnd, "gx_error_snapshot")) stop(cnd)
      gx_snapshot_request_load_abort(
        "Snapshot request-export loading failed closed.",
        "gx_error_snapshot_request_load_contract"
      )
    }
  )
}
