gx_snapshot_writer_profile <- "catalog-only-writer-v0.1"
gx_snapshot_writer_csv_profile <- "quote-all-utf8-lf-v1"
gx_snapshot_writer_paths <- c(
  sites = "catalog/sites.csv",
  datasets = "catalog/datasets.csv",
  problems = "catalog/problems.csv",
  requests = "requests.csv"
)

gx_snapshot_writer_abort <- function(
    message,
    subclass = "gx_error_snapshot_write_contract",
    call = rlang::caller_env()) {
  gx_abort(
    message,
    c(subclass, "gx_error_snapshot_write", "gx_error_snapshot"),
    call = call,
    .redact_trace = TRUE
  )
}

gx_snapshot_writer_muffle_warnings <- function(code) {
  withCallingHandlers(
    code,
    warning = function(cnd) {
      invokeRestart("muffleWarning")
    }
  )
}

gx_snapshot_writer_scalar_path <- function(dir) {
  valid <- is.character(dir) && !is.object(dir) && length(dir) == 1L &&
    is.null(attributes(dir)) && !is.na(dir) && nzchar(dir) &&
    isTRUE(stringi::stri_enc_isutf8(dir)) &&
    !isTRUE(stringi::stri_detect_regex(dir, "\\p{Cc}"))
  if (!isTRUE(valid)) {
    gx_snapshot_writer_abort(
      "Snapshot writing requires one literal destination path.",
      "gx_error_snapshot_write_input"
    )
  }

  expanded <- path.expand(dir)
  leaf <- basename(expanded)
  parent_input <- dirname(expanded)
  if (!nzchar(leaf) || leaf %in% c(".", "..")) {
    gx_snapshot_writer_abort(
      "The snapshot destination does not have a safe leaf name.",
      "gx_error_snapshot_write_input"
    )
  }
  parent_absolute <- tryCatch(
    as.character(fs::path_abs(parent_input)),
    warning = function(cnd) NULL,
    error = function(cnd) NULL
  )
  if (is.null(parent_absolute) || length(parent_absolute) != 1L) {
    gx_snapshot_writer_abort(
      "The snapshot destination parent could not be resolved.",
      "gx_error_snapshot_write_input"
    )
  }
  parent_info <- gx_snapshot_assert_fs_type(parent_absolute, "directory")
  parent <- tryCatch(
    normalizePath(parent_absolute, winslash = "/", mustWork = TRUE),
    warning = function(cnd) NA_character_,
    error = function(cnd) NA_character_
  )
  if (length(parent) != 1L || is.na(parent) || !nzchar(parent)) {
    gx_snapshot_writer_abort(
      "The snapshot destination parent could not be normalized.",
      "gx_error_snapshot_write_input"
    )
  }
  normalized_info <- gx_snapshot_assert_fs_type(parent, "directory")
  gx_snapshot_assert_same_info(parent_info, normalized_info)
  list(
    parent = parent,
    parent_info = normalized_info,
    target = file.path(parent, leaf)
  )
}

gx_snapshot_writer_entry_exists <- function(path) {
  link <- tryCatch(
    Sys.readlink(path),
    warning = function(cnd) NA_character_,
    error = function(cnd) NA_character_
  )
  if (length(link) != 1L) {
    gx_snapshot_writer_abort(
      "The snapshot destination could not be inspected safely.",
      "gx_error_snapshot_write_io"
    )
  }
  if (is.na(link)) return(file.exists(path) || dir.exists(path))
  nzchar(link) || file.exists(path) || dir.exists(path)
}

gx_snapshot_writer_time <- function(x) {
  value <- tryCatch(
    as.POSIXct(x, tz = "UTC"),
    warning = function(cnd) as.POSIXct(NA, tz = "UTC"),
    error = function(cnd) as.POSIXct(NA, tz = "UTC")
  )
  if (length(value) != 1L || is.na(value) || !is.finite(as.numeric(value))) {
    gx_snapshot_writer_abort(
      "Snapshot metadata contains an invalid timestamp.",
      "gx_error_snapshot_write_catalog"
    )
  }
  format(value, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC", usetz = FALSE)
}

gx_snapshot_writer_character_column <- function(x) {
  if (inherits(x, "POSIXct")) {
    out <- rep.int(NA_character_, length(x))
    present <- !is.na(x)
    out[present] <- vapply(
      as.list(x[present]),
      gx_snapshot_writer_time,
      character(1)
    )
    return(out)
  }
  if (is.character(x) && !is.object(x)) return(enc2utf8(x))
  if (is.logical(x) && !is.object(x)) {
    return(ifelse(is.na(x), NA_character_, ifelse(x, "true", "false")))
  }
  if (is.integer(x) && !is.object(x)) return(as.character(x))
  if (is.numeric(x) && !is.object(x)) {
    if (any(!is.na(x) & !is.finite(x))) {
      gx_snapshot_writer_abort(
        "A catalog export contains a non-finite numeric value.",
        "gx_error_snapshot_write_catalog"
      )
    }
    out <- rep.int(NA_character_, length(x))
    present <- !is.na(x)
    out[present] <- sprintf("%.17g", x[present])
    return(out)
  }
  gx_snapshot_writer_abort(
    "A catalog export did not project to scalar atomic columns.",
    "gx_error_snapshot_write_catalog"
  )
}

gx_snapshot_writer_character_view <- function(x) {
  if (!inherits(x, "data.frame") || is.null(names(x)) || anyNA(names(x)) ||
      any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    gx_snapshot_writer_abort(
      "A catalog export view has an invalid tabular contract.",
      "gx_error_snapshot_write_catalog"
    )
  }
  columns <- lapply(x, gx_snapshot_writer_character_column)
  if (length(columns) && any(lengths(columns) != nrow(x))) {
    gx_snapshot_writer_abort(
      "A catalog export view has inconsistent column lengths.",
      "gx_error_snapshot_write_catalog"
    )
  }
  names(columns) <- names(x)
  out <- as.data.frame(
    columns,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    optional = TRUE
  )
  if (!length(columns)) row.names(out) <- seq_len(nrow(x))
  out
}

gx_snapshot_writer_redact_view <- function(x) {
  sensitive <- grepl("(?:^|_)(?:uri|url)$", names(x), perl = TRUE) |
    names(x) %in% c(
      "canonical_url_redacted", "final_url"
    )
  for (column in names(x)[sensitive]) {
    present <- !is.na(x[[column]]) & nzchar(x[[column]])
    if (any(present)) {
      x[[column]][present] <- vapply(
        x[[column]][present],
        gx_redact_url,
        character(1)
      )
    }
  }
  x
}

gx_snapshot_writer_validate_text <- function(x) {
  values <- unlist(x, use.names = FALSE)
  values <- values[!is.na(values)]
  valid <- !length(values) || (
    all(stringi::stri_enc_isutf8(values)) &&
      !any(stringi::stri_detect_regex(values, "\\p{Cc}"))
  )
  if (!isTRUE(valid)) {
    gx_snapshot_writer_abort(
      "A catalog export contains unsafe or invalid text.",
      "gx_error_snapshot_write_catalog"
    )
  }
  invisible(x)
}

gx_snapshot_writer_quote <- function(x) {
  x[is.na(x)] <- ""
  paste0('"', gsub('"', '""', enc2utf8(x), fixed = TRUE), '"')
}

gx_snapshot_writer_write_csv <- function(x, path) {
  x <- gx_snapshot_writer_character_view(x)
  gx_snapshot_writer_validate_text(x)
  connection <- tryCatch(
    gx_snapshot_writer_muffle_warnings(file(path, open = "wb")),
    error = function(cnd) NULL
  )
  if (is.null(connection)) {
    gx_snapshot_writer_abort(
      "A snapshot resource could not be opened for exclusive staging.",
      "gx_error_snapshot_write_io"
    )
  }
  on.exit(try(
    gx_snapshot_writer_muffle_warnings(close(connection)),
    silent = TRUE
  ), add = TRUE)
  write_line <- function(fields) {
    line <- paste0(paste(gx_snapshot_writer_quote(fields), collapse = ","), "\n")
    tryCatch(
      gx_snapshot_writer_muffle_warnings(
        writeBin(charToRaw(enc2utf8(line)), connection)
      ),
      error = function(cnd) {
        gx_snapshot_writer_abort(
          "A snapshot resource could not be written completely.",
          "gx_error_snapshot_write_io"
        )
      }
    )
  }
  write_line(names(x))
  if (nrow(x)) {
    for (index in seq_len(nrow(x))) {
      write_line(vapply(x[index, , drop = FALSE], `[[`, character(1), 1L))
    }
  }
  tryCatch(
    gx_snapshot_writer_muffle_warnings(close(connection)),
    error = function(cnd) {
      gx_snapshot_writer_abort(
        "A snapshot resource could not be closed safely.",
        "gx_error_snapshot_write_io"
      )
    }
  )
  on.exit(NULL, add = FALSE)
  info <- gx_snapshot_assert_fs_type(path, "file")
  bytes <- as.numeric(info$size[[1L]])
  if (!is.finite(bytes) || bytes < 1 || bytes > gx_snapshot_max_resource_bytes) {
    gx_snapshot_writer_abort(
      "A staged snapshot resource exceeds its byte ceiling.",
      "gx_error_snapshot_write_budget"
    )
  }
  list(
    bytes = bytes,
    sha256 = gx_snapshot_hash_file(path),
    info = info
  )
}

gx_snapshot_writer_request_fields <- c(
  "request_id", "stage", "method", "canonical_url_redacted",
  "request_hash", "body_hash", "final_url", "response_status",
  "response_media_type", "encoded_bytes", "decoded_bytes", "content_hash",
  "etag", "last_modified", "retrieved_at", "elapsed_ms", "cache_origin",
  "error_code"
)

gx_snapshot_writer_request_value <- function(x, index, field) {
  value <- x[[field]][[index]]
  if (length(value) != 1L || is.na(value)) return(NULL)
  if (inherits(x[[field]], "POSIXct")) return(gx_snapshot_writer_time(value))
  if (is.integer(value)) return(as.integer(value))
  if (is.numeric(value)) return(as.numeric(value))
  if (is.logical(value)) return(isTRUE(value))
  enc2utf8(as.character(value))
}

gx_snapshot_writer_requests <- function(requests) {
  if (!inherits(requests, "data.frame") ||
      !identical(names(requests), gx_snapshot_writer_request_fields)) {
    gx_snapshot_writer_abort(
      "Catalog requests do not satisfy the snapshot-ledger projection.",
      "gx_error_snapshot_write_catalog"
    )
  }
  out <- vector("list", nrow(requests))
  for (index in seq_len(nrow(requests))) {
    item <- stats::setNames(
      vector("list", length(gx_snapshot_writer_request_fields)),
      gx_snapshot_writer_request_fields
    )
    for (field in gx_snapshot_writer_request_fields) {
      item[field] <- list(gx_snapshot_writer_request_value(
        requests,
        index,
        field
      ))
    }
    item$canonical_url_redacted <- gx_redact_url(
      as.character(requests$canonical_url_redacted[[index]])
    )
    final <- gx_snapshot_writer_request_value(requests, index, "final_url")
    if (!is.null(final)) {
      redacted <- gx_redact_url(final)
      placeholder <- grepl("[?#]\\[redacted\\]", final, perl = TRUE)
      valid_uri <- tryCatch({
        gx_snapshot_uri(final)
        TRUE
      }, error = function(cnd) FALSE)
      item["final_url"] <- list(
        if (identical(final, redacted) && !placeholder && valid_uri) final else NULL
      )
    }
    out[[index]] <- item
  }
  validated <- gx_snapshot_validate_requests(out)
  order_key <- vapply(validated, `[[`, character(1), "retrieved_at")
  ids <- vapply(validated, `[[`, character(1), "request_id")
  validated[order(order_key, ids, method = "radix")]
}

gx_snapshot_writer_requests_view <- function(requests) {
  rows <- lapply(requests, function(item) {
    values <- lapply(gx_snapshot_writer_request_fields, function(field) {
      value <- item[[field]]
      if (is.null(value)) return(NA_character_)
      if (is.logical(value)) return(if (value) "true" else "false")
      if (is.numeric(value)) return(sprintf("%.17g", value))
      as.character(value)
    })
    names(values) <- gx_snapshot_writer_request_fields
    as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
  })
  if (!length(rows)) {
    empty <- stats::setNames(
      rep(list(character()), length(gx_snapshot_writer_request_fields)),
      gx_snapshot_writer_request_fields
    )
    return(as.data.frame(empty, stringsAsFactors = FALSE, check.names = FALSE))
  }
  do.call(rbind, rows)
}

gx_snapshot_writer_asset_hash <- function(directory, file) {
  path <- file.path(gx_asset_dir(directory), file)
  before <- gx_snapshot_assert_fs_type(path, "file")
  value <- gx_snapshot_hash_file(path)
  after <- gx_snapshot_assert_fs_type(path, "file")
  gx_snapshot_assert_same_info(before, after)
  value
}

gx_snapshot_writer_named_object <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.atomic(x) && !is.null(names(x))) x <- as.list(x)
  if (!is.list(x) || is.null(names(x)) || anyNA(names(x)) ||
      any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    gx_snapshot_writer_abort(
      "Catalog metadata does not satisfy its named-object contract.",
      "gx_error_snapshot_write_catalog"
    )
  }
  x
}

gx_snapshot_writer_records <- function(x) {
  if (!inherits(x, "data.frame")) {
    gx_snapshot_writer_abort(
      "Catalog metadata records are not tabular.",
      "gx_error_snapshot_write_catalog"
    )
  }
  rows <- vector("list", nrow(x))
  for (index in seq_len(nrow(x))) {
    item <- stats::setNames(vector("list", ncol(x)), names(x))
    for (field in names(x)) {
      value <- x[[field]][[index]]
      if (length(value) != 1L || is.na(value)) {
        item[field] <- list(NULL)
      } else if (inherits(x[[field]], "POSIXct")) {
        item[field] <- list(gx_snapshot_writer_time(value))
      } else if (is.logical(value)) {
        item[field] <- list(isTRUE(value))
      } else if (is.integer(value)) {
        item[field] <- list(as.integer(value))
      } else if (is.numeric(value)) {
        item[field] <- list(as.numeric(value))
      } else {
        item[field] <- list(enc2utf8(as.character(value)))
      }
    }
    rows[[index]] <- item
  }
  rows
}

gx_snapshot_writer_selection <- function(selection) {
  selection <- gx_snapshot_writer_named_object(selection)
  out <- list()
  for (field in intersect(c("include", "providers", "variables"), names(selection))) {
    value <- selection[[field]]
    if (!is.character(value) || anyNA(value)) {
      gx_snapshot_writer_abort(
        "Catalog selection metadata is invalid.",
        "gx_error_snapshot_write_catalog"
      )
    }
    value <- gx_catalog_redact_uri(enc2utf8(value))
    out[field] <- list(as.list(value))
  }
  out
}

gx_snapshot_writer_completeness <- function(x) {
  required <- c("stage", "status", "truncated", "reason")
  if (!inherits(x, "data.frame") || !all(required %in% names(x))) {
    gx_snapshot_writer_abort(
      "Catalog completeness metadata is invalid.",
      "gx_error_snapshot_write_catalog"
    )
  }
  rows <- vector("list", nrow(x))
  for (index in seq_len(nrow(x))) {
    item <- list(
      stage = as.character(x$stage[[index]]),
      status = as.character(x$status[[index]]),
      truncated = isTRUE(x$truncated[[index]])
    )
    if (!is.na(x$reason[[index]])) item$reason <- as.character(x$reason[[index]])
    rows[[index]] <- item
  }
  rows
}

gx_snapshot_writer_session <- function() {
  packages <- sort(c("digest", "fs", "geoconnexr", "jsonlite"))
  package_rows <- lapply(packages, function(package) {
    version <- tryCatch(
      as.character(utils::packageVersion(package)),
      error = function(cnd) "unknown"
    )
    list(package = package, version = version)
  })
  locale <- tryCatch(Sys.getlocale("LC_CTYPE"), error = function(cnd) "unknown")
  if (!is.character(locale) || length(locale) != 1L || is.na(locale) ||
      nchar(locale, type = "bytes") > 1024L) locale <- "unknown"
  list(
    r_version = R.version$version.string,
    platform = R.version$platform,
    locale = locale,
    packages = package_rows
  )
}

gx_snapshot_writer_resource <- function(path, state, roles) {
  list(
    path = path,
    media_type = "text/csv",
    bytes = state$bytes,
    sha256 = state$sha256,
    required = TRUE,
    roles = as.list(roles),
    source_uri = NULL,
    license_uri = NULL
  )
}

gx_snapshot_writer_manifest <- function(catalog, states, requests) {
  metadata <- catalog$metadata
  selection <- gx_snapshot_writer_selection(metadata$selection)
  recipe <- catalog$aoi$recipe
  recipe["catalog"] <- list(selection)
  endpoints <- gx_snapshot_writer_named_object(metadata$endpoints)
  endpoints <- lapply(endpoints, function(endpoint) {
    endpoint <- as.character(endpoint)
    if (length(endpoint) != 1L || is.na(endpoint) ||
        !identical(endpoint, gx_redact_url(endpoint))) {
      gx_snapshot_writer_abort(
        "Catalog endpoint metadata contains a sensitive or invalid URL.",
        "gx_error_snapshot_write_catalog"
      )
    }
    endpoint
  })
  resources <- list(
    gx_snapshot_writer_resource(
      gx_snapshot_writer_paths[["sites"]], states$sites, c("catalog", "sites")
    ),
    gx_snapshot_writer_resource(
      gx_snapshot_writer_paths[["datasets"]], states$datasets,
      c("catalog", "datasets")
    ),
    gx_snapshot_writer_resource(
      gx_snapshot_writer_paths[["problems"]], states$problems,
      c("catalog", "diagnostics")
    ),
    gx_snapshot_writer_resource(
      gx_snapshot_writer_paths[["requests"]], states$requests,
      c("request-ledger-export", "request-ledger-export-v1")
    )
  )
  resources <- resources[order(
    vapply(resources, `[[`, character(1), "path"),
    method = "radix"
  )]
  package_version <- tryCatch(
    as.character(utils::packageVersion("geoconnexr")),
    error = function(cnd) "0.0.0.9000"
  )
  manifest <- list(
    contract_version = "1.0.0",
    manifest_version = "1.0.0",
    package = list(name = "geoconnexr", version = package_version),
    created_at = gx_snapshot_writer_time(gx_now()),
    recipe = recipe,
    replay = list(
      replayable = FALSE,
      non_replayable_reasons = list("catalog_only_writer_v0_1"),
      handler_versions = list()
    ),
    effective_options = list(
      catalog_contract_version = as.character(catalog$contract_version),
      catalog_created_at = gx_snapshot_writer_time(metadata$created_at),
      catalog_counts = gx_snapshot_writer_named_object(metadata$counts),
      catalog_completeness = gx_snapshot_writer_records(metadata$completeness),
      source_contracts = gx_snapshot_writer_named_object(
        metadata$source_contracts
      ),
      serialization = list(
        writer = gx_snapshot_writer_profile,
        csv = gx_snapshot_writer_csv_profile,
        request_export = "manifest-requests-csv-v1"
      )
    ),
    endpoints = endpoints,
    asset_hashes = list(
      queries = gx_snapshot_writer_asset_hash("queries", "manifest.yml"),
      handler_registry = gx_snapshot_writer_asset_hash("handlers", "registry.yml"),
      vocabulary = gx_snapshot_writer_asset_hash(
        "vocab", "unit-conversions-v1.csv"
      )
    ),
    requests = requests,
    resources = resources,
    completeness = gx_snapshot_writer_completeness(metadata$completeness),
    session = gx_snapshot_writer_session()
  )
  vintage <- metadata$hydrologic_vintage
  if (!is.null(vintage) && length(vintage)) {
    vintage <- gx_snapshot_writer_named_object(vintage)
    present <- vapply(vintage, function(value) {
      length(value) == 1L && !is.na(value)
    }, logical(1))
    vintage <- vintage[present]
    if (length(vintage)) manifest["hydrologic_vintage"] <- list(vintage)
  }
  manifest
}

gx_snapshot_writer_json_bytes <- function(manifest) {
  text <- tryCatch(
    jsonlite::toJSON(
      manifest,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA,
      POSIXt = "ISO8601",
      pretty = TRUE,
      force = TRUE
    ),
    error = function(cnd) NULL
  )
  if (is.null(text) || length(text) != 1L) {
    gx_snapshot_writer_abort(
      "The snapshot manifest could not be serialized.",
      "gx_error_snapshot_write_manifest"
    )
  }
  text <- sub("[\\r\\n]+\\z", "", enc2utf8(as.character(text)), perl = TRUE)
  bytes <- charToRaw(paste0(text, "\n"))
  decoded <- gx_snapshot_parse_json(bytes)
  gx_snapshot_validate_manifest(decoded)
  bytes
}

gx_snapshot_writer_write_raw <- function(path, bytes) {
  if (!is.raw(bytes) || file.exists(path) || dir.exists(path)) {
    gx_snapshot_writer_abort(
      "A staged snapshot path is not available for writing.",
      "gx_error_snapshot_write_io"
    )
  }
  tryCatch(
    gx_snapshot_writer_muffle_warnings(writeBin(bytes, path)),
    error = function(cnd) {
      gx_snapshot_writer_abort(
        "A staged snapshot file could not be written completely.",
        "gx_error_snapshot_write_io"
      )
    }
  )
  invisible(gx_snapshot_assert_fs_type(path, "file"))
}

gx_snapshot_writer_file_rename <- function(from, to) file.rename(from, to)

gx_snapshot_writer_rename <- function(from, to) {
  result <- tryCatch(
    gx_snapshot_writer_muffle_warnings(
      gx_snapshot_writer_file_rename(from, to)
    ),
    error = function(cnd) FALSE
  )
  isTRUE(result)
}

gx_snapshot_writer_unlink <- function(path) {
  unlink(path, recursive = TRUE, force = TRUE)
}

gx_snapshot_writer_cleanup_stage <- function(path) {
  result <- tryCatch(
    gx_snapshot_writer_muffle_warnings(gx_snapshot_writer_unlink(path)),
    error = function(cnd) NULL
  )
  if (is.null(result) || length(result) != 1L || is.na(result) || result != 0L) {
    return(FALSE)
  }
  absent <- tryCatch(
    gx_snapshot_writer_muffle_warnings({
      link <- Sys.readlink(path)
      !file.exists(path) && !dir.exists(path) &&
        (length(link) == 1L && (is.na(link) || !nzchar(link)))
    }),
    error = function(cnd) FALSE
  )
  isTRUE(absent)
}

# Internal M9b creation boundary. It packages an already validated offline
# catalog; it never performs catalog discovery, network work, or overwrite.
gx_snapshot_write_catalog_impl <- function(catalog, dir) {
  cleanup <- new.env(parent = emptyenv())
  cleanup$stage <- NULL
  cleanup$owned <- FALSE
  on.exit({
    if (isTRUE(cleanup$owned) && is.character(cleanup$stage)) {
      try(gx_snapshot_writer_cleanup_stage(cleanup$stage), silent = TRUE)
    }
  }, add = TRUE)
  tryCatch(
    {
      gx_catalog_validate_impl(catalog)
      destination <- gx_snapshot_writer_scalar_path(dir)
      if (gx_snapshot_writer_entry_exists(destination$target)) {
        gx_snapshot_writer_abort(
          "The snapshot destination already exists; overwrite is not supported.",
          "gx_error_snapshot_write_exists"
        )
      }

      stage <- tempfile(pattern = ".gx-snapshot-stage-", tmpdir = destination$parent)
      if (!dir.create(stage, mode = "0700", showWarnings = FALSE)) {
        gx_snapshot_writer_abort(
          "The snapshot staging directory could not be created.",
          "gx_error_snapshot_write_io"
        )
      }
      cleanup$stage <- stage
      cleanup$owned <- TRUE
      destination$parent_info <- gx_snapshot_assert_fs_type(
        destination$parent,
        "directory"
      )
      if (!dir.create(file.path(stage, "catalog"), mode = "0700",
                      showWarnings = FALSE)) {
        gx_snapshot_writer_abort(
          "The snapshot catalog staging directory could not be created.",
          "gx_error_snapshot_write_io"
        )
      }

      # Revalidate immediately before deriving any serialized projection.
      gx_catalog_validate_impl(catalog)
      views <- gx_catalog_export_views_impl(catalog)
      if (!is.list(views) || !identical(
        names(views),
        c(
          "sites", "datasets", "reference", "problems", "requests",
          "completeness"
        )
      )) {
        gx_snapshot_writer_abort(
          "The catalog export views do not satisfy their exact contract.",
          "gx_error_snapshot_write_catalog"
        )
      }
      if (!identical(views$reference, list())) {
        gx_snapshot_writer_abort(
          "Reference-layer snapshot serialization remains gated.",
          "gx_error_snapshot_write_catalog"
        )
      }
      requests <- gx_snapshot_writer_requests(views$requests)
      views$requests <- gx_snapshot_writer_requests_view(requests)
      views <- views[c("sites", "datasets", "problems", "requests")]
      views <- lapply(views, function(view) {
        gx_snapshot_writer_redact_view(gx_snapshot_writer_character_view(view))
      })

      states <- list()
      for (name in names(gx_snapshot_writer_paths)) {
        path <- file.path(stage, gx_snapshot_writer_paths[[name]])
        states[[name]] <- gx_snapshot_writer_write_csv(views[[name]], path)
      }
      total <- sum(vapply(states, `[[`, numeric(1), "bytes"))
      if (!is.finite(total) || total > gx_snapshot_max_resource_bytes) {
        gx_snapshot_writer_abort(
          "The staged snapshot exceeds its aggregate resource-byte ceiling.",
          "gx_error_snapshot_write_budget"
        )
      }

      manifest <- gx_snapshot_writer_manifest(catalog, states, requests)
      manifest_bytes <- gx_snapshot_writer_json_bytes(manifest)
      gx_snapshot_writer_write_raw(
        file.path(stage, gx_snapshot_manifest_name),
        manifest_bytes
      )
      staged_verification <- gx_snapshot_verify_impl(stage)

      gx_snapshot_assert_same_info(
        destination$parent_info,
        gx_snapshot_assert_fs_type(destination$parent, "directory")
      )
      if (gx_snapshot_writer_entry_exists(destination$target)) {
        gx_snapshot_writer_abort(
          "The snapshot destination changed before atomic exposure.",
          "gx_error_snapshot_write_race"
        )
      }
      if (!isTRUE(gx_snapshot_writer_rename(stage, destination$target))) {
        gx_snapshot_writer_abort(
          "The verified snapshot could not be exposed atomically.",
          "gx_error_snapshot_write_io"
        )
      }
      cleanup$owned <- FALSE
      final_verification <- gx_snapshot_verify_impl(destination$target)
      if (!identical(
        staged_verification$manifest_sha256,
        final_verification$manifest_sha256
      )) {
        gx_snapshot_writer_abort(
          "The snapshot changed during atomic exposure.",
          "gx_error_snapshot_write_race"
        )
      }
      list(
        contract_version = "1.0.0",
        mode = "catalog_snapshot_write",
        status = "written",
        path = normalizePath(destination$target, winslash = "/", mustWork = TRUE),
        verification = final_verification
      )
    },
    error = function(cnd) {
      if (isTRUE(cleanup$owned)) {
        cleaned <- gx_snapshot_writer_cleanup_stage(cleanup$stage)
        if (isTRUE(cleaned)) {
          cleanup$owned <- FALSE
        } else {
          gx_snapshot_writer_abort(
            "Snapshot staging cleanup failed; no destination was published.",
            "gx_error_snapshot_write_cleanup"
          )
        }
      }
      if (inherits(cnd, "gx_error_snapshot_write")) stop(cnd)
      gx_snapshot_writer_abort(
        "Catalog snapshot writing failed closed.",
        "gx_error_snapshot_write_contract"
      )
    }
  )
}
