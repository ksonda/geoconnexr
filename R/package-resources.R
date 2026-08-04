.gx_package_resources_contract_version <- "0.2.0"
.gx_package_resources_max_resource_bytes <- 128 * 1024^2
.gx_package_resources_max_total_bytes <- 256 * 1024^2
.gx_package_resources_max_fields <- 5000000
.gx_package_resources_max_resources <- 10000L

.gx_package_resources_fields <- c(
  "contract_version", "stage", "timeseries", "input", "resources", "contents",
  "metadata", "bundle_id"
)

.gx_package_resources_columns <- c(
  "contract_version", "path", "role", "format", "media_type", "bytes",
  "sha256", "rows", "columns", "result_id", "distribution_id", "handler_id"
)

.gx_package_resources_metadata_fields <- c(
  "scope", "in_memory", "deterministic", "serializes", "writes",
  "publishes", "arrow", "arrow_package_version", "quarto", "frictionless", "replayable",
  "counts", "limitations"
)

.gx_package_resources_count_fields <- c(
  "resources", "catalog_resources", "fetch_resources", "native_resources",
  "harmonized_resources", "csv_resources", "parquet_resources",
  "raw_resources", "stored_bytes"
)

gx_package_resources_abort <- function(
    message,
    class = "gx_error_package_resources_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_resources", "gx_error_package_input",
      "gx_error_package"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_resources_csv_impl <- function(x, redact = FALSE) {
  table <- gx_snapshot_writer_character_view(x)
  if (redact) table <- gx_snapshot_writer_redact_view(table)
  gx_snapshot_csv_bytes_impl(
    table,
    max_bytes = .gx_package_resources_max_resource_bytes,
    max_columns = gx_snapshot_csv_load_max_columns,
    max_fields = .gx_package_resources_max_fields
  )
}

gx_package_resources_entry_impl <- function(
    path,
    role,
    format,
    media_type,
    content,
    rows = NA_integer_,
    columns = NA_integer_,
    result_id = NA_character_,
    distribution_id = NA_character_,
    handler_id = NA_character_) {
  path <- tryCatch(
    gx_snapshot_path(path),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  valid_text <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) &&
      is.null(attributes(x)) && nzchar(x)
  }
  valid_optional_text <- function(x) {
    is.character(x) && length(x) == 1L && is.null(attributes(x)) &&
      (is.na(x) || nzchar(x))
  }
  valid_count <- function(x) {
    is.integer(x) && length(x) == 1L && (is.na(x) || x >= 0L)
  }
  valid <- !is.null(path) && valid_text(role) &&
    format %in% c("csv", "parquet", "raw") && valid_text(format) &&
    valid_text(media_type) && is.raw(content) && !is.object(content) &&
    is.null(attributes(content)) &&
    length(content) <= .gx_package_resources_max_resource_bytes &&
    valid_count(rows) && valid_count(columns) &&
    valid_optional_text(result_id) &&
    valid_optional_text(distribution_id) &&
    valid_optional_text(handler_id)
  if (!valid) {
    gx_package_resources_abort(
      "A package resource violates its bounded exact entry contract."
    )
  }
  list(
    contract_version = .gx_package_resources_contract_version,
    path = path,
    role = role,
    format = format,
    media_type = media_type,
    bytes = unname(as.double(length(content))),
    sha256 = digest::digest(content, algo = "sha256", serialize = FALSE),
    rows = rows,
    columns = columns,
    result_id = result_id,
    distribution_id = distribution_id,
    handler_id = handler_id,
    content = content
  )
}

gx_package_resources_catalog_impl <- function(input) {
  tables <- gx_package_input_catalog_tables_impl(input$catalog)
  paths <- c(
    sites = "catalog/sites.csv",
    datasets = "catalog/datasets.csv",
    problems = "catalog/problems.csv",
    requests = "requests.csv"
  )
  lapply(names(paths), function(resource) {
    content <- gx_snapshot_csv_bytes_impl(tables[[resource]])
    sha256 <- digest::digest(content, algo = "sha256", serialize = FALSE)
    if (!identical(
      unname(sha256),
      unname(input$evidence$catalog_resources[[resource]])
    )) {
      gx_package_resources_abort(
        "Catalog resource bytes no longer match M9j input evidence.",
        "gx_error_package_resources_lineage"
      )
    }
    gx_package_resources_entry_impl(
      path = unname(paths[[resource]]),
      role = paste0("catalog_", resource),
      format = "csv",
      media_type = "text/csv; charset=utf-8",
      content = content,
      rows = unname(as.integer(nrow(tables[[resource]]))),
      columns = unname(as.integer(ncol(tables[[resource]])))
    )
  })
}

gx_package_resources_result_media_type_impl <- function(fetched, result) {
  position <- which(
    fetched$plan$distributions$distribution_id == result$distribution_id
  )
  if (length(position) != 1L) {
    gx_package_resources_abort(
      "A fetched result no longer has one exact planned distribution.",
      "gx_error_package_resources_lineage"
    )
  }
  media_type <- fetched$plan$distributions$media_type[[position]]
  if (is.na(media_type) || !nzchar(media_type)) {
    "application/octet-stream"
  } else {
    unname(media_type)
  }
}

gx_package_resources_native_impl <- function(fetched) {
  entries <- vector("list", nrow(fetched$results))
  index <- vector("list", nrow(fetched$results))
  if (nrow(fetched$results)) {
    for (position in seq_len(nrow(fetched$results))) {
      result <- fetched$results[position, , drop = FALSE]
      stem <- paste0(
        sprintf("%06d", result$result_index[[1L]]),
        "-", result$result_id[[1L]]
      )
      if (isTRUE(result$raw_body_available[[1L]])) {
        content <- result$raw_body[[1L]]
        path <- paste0("data/raw/", stem, ".bin")
        storage <- "raw_body"
        format <- "raw"
        media_type <- gx_package_resources_result_media_type_impl(
          fetched, result
        )
      } else {
        valid_table <- identical(result$handler_id[[1L]], "csv") &&
          identical(result$payload_class[[1L]], "table") &&
          inherits(result$data[[1L]], "data.frame") &&
          !inherits(result$data[[1L]], "sf")
        if (!valid_table) {
          gx_package_resources_abort(
            paste0(
              "A result without retained raw bytes is outside the fixed ",
              "canonical direct-CSV serialization profile."
            ),
            "gx_error_package_resources_profile"
          )
        }
        content <- gx_package_resources_csv_impl(result$data[[1L]])
        path <- paste0("data/native/", stem, ".csv")
        storage <- "canonical_csv"
        format <- "csv"
        media_type <- "text/csv; charset=utf-8"
      }
      entry <- gx_package_resources_entry_impl(
        path = path,
        role = if (identical(storage, "raw_body")) {
          "native_raw"
        } else {
          "native_table"
        },
        format = format,
        media_type = media_type,
        content = content,
        rows = result$row_count[[1L]],
        columns = result$column_count[[1L]],
        result_id = result$result_id[[1L]],
        distribution_id = result$distribution_id[[1L]],
        handler_id = result$handler_id[[1L]]
      )
      entries[[position]] <- entry
      index[[position]] <- list(
        contract_version = .gx_package_resources_contract_version,
        result_index = result$result_index[[1L]],
        result_id = result$result_id[[1L]],
        distribution_id = result$distribution_id[[1L]],
        handler_id = result$handler_id[[1L]],
        payload_class = result$payload_class[[1L]],
        storage = storage,
        path = path,
        media_type = media_type,
        row_count = result$row_count[[1L]],
        column_count = result$column_count[[1L]],
        raw_body_available = result$raw_body_available[[1L]],
        bytes = entry$bytes,
        sha256 = entry$sha256
      )
    }
  }
  index_table <- if (!length(index)) {
    tibble::tibble(
      contract_version = character(),
      result_index = integer(),
      result_id = character(),
      distribution_id = character(),
      handler_id = character(),
      payload_class = character(),
      storage = character(),
      path = character(),
      media_type = character(),
      row_count = integer(),
      column_count = integer(),
      raw_body_available = logical(),
      bytes = double(),
      sha256 = character()
    )
  } else {
    tibble::as_tibble(do.call(rbind.data.frame, c(
      index,
      list(stringsAsFactors = FALSE, check.names = FALSE)
    )))
  }
  list(entries = entries, index = index_table)
}

gx_package_resources_fetched_impl <- function(fetched) {
  status <- gx_package_resources_entry_impl(
    path = "catalog/fetch_status.csv",
    role = "fetch_status",
    format = "csv",
    media_type = "text/csv; charset=utf-8",
    content = gx_package_resources_csv_impl(fetched$status),
    rows = unname(as.integer(nrow(fetched$status))),
    columns = unname(as.integer(ncol(fetched$status)))
  )
  native <- gx_package_resources_native_impl(fetched)
  native_index <- gx_package_resources_entry_impl(
    path = "data/native/index.csv",
    role = "native_index",
    format = "csv",
    media_type = "text/csv; charset=utf-8",
    content = gx_package_resources_csv_impl(native$index),
    rows = unname(as.integer(nrow(native$index))),
    columns = unname(as.integer(ncol(native$index)))
  )
  c(list(status, native_index), native$entries)
}

gx_package_resources_harmonized_impl <- function(
    harmonized,
    timeseries,
    parquet_content = NULL) {
  observations <- if (identical(timeseries, "parquet")) {
    if (is.null(parquet_content)) {
      parquet_content <- gx_package_parquet_build_impl(
        harmonized,
        gx_package_parquet_capability_impl
      )$content
    } else {
      valid_content <- is.raw(parquet_content) &&
        !is.object(parquet_content) && is.null(attributes(parquet_content)) &&
        length(parquet_content) >= 8L &&
        length(parquet_content) <= .gx_package_parquet_max_bytes &&
        identical(parquet_content[seq_len(4L)], charToRaw("PAR1")) &&
        identical(
          parquet_content[(length(parquet_content) - 3L):length(parquet_content)],
          charToRaw("PAR1")
        )
      if (!valid_content) {
        gx_package_resources_abort(
          "Parquet resource bytes violate their bounded fixed profile.",
          "gx_error_package_resources_lineage"
        )
      }
    }
    gx_package_resources_entry_impl(
      path = "data/observations.parquet",
      role = "observations",
      format = "parquet",
      media_type = "application/vnd.apache.parquet",
      content = parquet_content,
      rows = unname(as.integer(nrow(harmonized$observations))),
      columns = unname(as.integer(ncol(harmonized$observations)))
    )
  } else {
    gx_package_resources_entry_impl(
      path = "data/observations.csv",
      role = "observations",
      format = "csv",
      media_type = "text/csv; charset=utf-8",
      content = gx_package_resources_csv_impl(
        harmonized$observations,
        redact = TRUE
      ),
      rows = unname(as.integer(nrow(harmonized$observations))),
      columns = unname(as.integer(ncol(harmonized$observations)))
    )
  }
  list(
    observations,
    gx_package_resources_entry_impl(
      path = "data/harmonized_resources.csv",
      role = "harmonized_index",
      format = "csv",
      media_type = "text/csv; charset=utf-8",
      content = gx_package_resources_csv_impl(harmonized$resources),
      rows = unname(as.integer(nrow(harmonized$resources))),
      columns = unname(as.integer(ncol(harmonized$resources)))
    )
  )
}

gx_package_resources_table_impl <- function(entries) {
  table <- tibble::tibble(
    contract_version = vapply(
      entries, `[[`, character(1), "contract_version"
    ),
    path = vapply(entries, `[[`, character(1), "path"),
    role = vapply(entries, `[[`, character(1), "role"),
    format = vapply(entries, `[[`, character(1), "format"),
    media_type = vapply(entries, `[[`, character(1), "media_type"),
    bytes = vapply(entries, `[[`, numeric(1), "bytes"),
    sha256 = vapply(entries, `[[`, character(1), "sha256"),
    rows = vapply(entries, `[[`, integer(1), "rows"),
    columns = vapply(entries, `[[`, integer(1), "columns"),
    result_id = vapply(entries, `[[`, character(1), "result_id"),
    distribution_id = vapply(
      entries, `[[`, character(1), "distribution_id"
    ),
    handler_id = vapply(entries, `[[`, character(1), "handler_id")
  )
  table
}

gx_package_resources_metadata_impl <- function(resources, timeseries) {
  roles <- resources$role
  limitations <- c(
    if (identical(timeseries, "csv")) "arrow_deferred",
    "frictionless_deferred", "quarto_deferred", "replay_deferred"
  )
  limitations <- limitations[gx_catalog_byte_order(limitations)]
  list(
    scope = "fixed_in_memory_resources_v1",
    in_memory = TRUE,
    deterministic = TRUE,
    serializes = TRUE,
    writes = FALSE,
    publishes = FALSE,
    arrow = identical(timeseries, "parquet"),
    arrow_package_version = if (identical(timeseries, "parquet")) {
      as.character(utils::packageVersion("arrow"))
    } else {
      NA_character_
    },
    quarto = FALSE,
    frictionless = FALSE,
    replayable = FALSE,
    counts = list(
      resources = unname(as.integer(nrow(resources))),
      catalog_resources = unname(as.integer(sum(grepl(
        "^catalog_", roles
      )))),
      fetch_resources = unname(as.integer(sum(
        roles %in% c("fetch_status", "native_index")
      ))),
      native_resources = unname(as.integer(sum(
        roles %in% c("native_raw", "native_table")
      ))),
      harmonized_resources = unname(as.integer(sum(
        roles %in% c("observations", "harmonized_index")
      ))),
      csv_resources = unname(as.integer(sum(resources$format == "csv"))),
      parquet_resources = unname(as.integer(sum(
        resources$format == "parquet"
      ))),
      raw_resources = unname(as.integer(sum(resources$format == "raw"))),
      stored_bytes = unname(as.double(sum(resources$bytes)))
    ),
    limitations = limitations
  )
}

gx_package_resources_id_impl <- function(input, timeseries, resources, metadata) {
  gx_contract_hash(
    list(
      "stage", input$stage,
      "timeseries", timeseries,
      "arrow_package_version", metadata$arrow_package_version,
      "package_input_id", input$input_id,
      "resource_evidence_sha256", gx_package_input_vector_hash_impl(
        paste(resources$path, resources$bytes, resources$sha256, sep = "="),
        "geoconnexr.package-resources.evidence.v1"
      ),
      "resource_count", metadata$counts$resources,
      "stored_bytes", metadata$counts$stored_bytes
    ),
    namespace = "geoconnexr.package-resources.v1",
    contract_version = .gx_package_resources_contract_version
  )
}

gx_package_resources_build_impl <- function(
    input,
    timeseries = "csv",
    parquet_content = NULL) {
  gx_package_input_validate_impl(input)
  valid_timeseries <- is.character(timeseries) && length(timeseries) == 1L &&
    !is.na(timeseries) && is.null(attributes(timeseries)) &&
    timeseries %in% c("csv", "parquet")
  if (!valid_timeseries ||
      (identical(timeseries, "parquet") && !identical(input$stage, "harmonized"))) {
    gx_package_resources_abort(
      "Parquet package resources require one harmonized package input.",
      "gx_error_package_resources_profile"
    )
  }
  entries <- gx_package_resources_catalog_impl(input)
  if (!is.null(input$fetched)) {
    entries <- c(entries, gx_package_resources_fetched_impl(input$fetched))
  }
  if (!is.null(input$harmonized)) {
    entries <- c(
      entries,
      gx_package_resources_harmonized_impl(
        input$harmonized, timeseries, parquet_content
      )
    )
  }
  if (!length(entries) ||
      length(entries) > .gx_package_resources_max_resources) {
    gx_package_resources_abort(
      "The package resource count exceeds its fixed profile.",
      "gx_error_package_resources_budget"
    )
  }
  paths <- vapply(entries, `[[`, character(1), "path")
  folded <- gx_snapshot_ascii_fold(paths)
  if (anyDuplicated(paths) || anyDuplicated(folded)) {
    gx_package_resources_abort(
      "Package resource paths are duplicated or ASCII aliases.",
      "gx_error_package_resources_path"
    )
  }
  order <- order(paths, method = "radix")
  entries <- entries[order]
  paths <- paths[order]
  resources <- gx_package_resources_table_impl(entries)
  total <- sum(resources$bytes)
  if (!is.finite(total) || total > .gx_package_resources_max_total_bytes) {
    gx_package_resources_abort(
      "Package resources exceed the aggregate in-memory byte ceiling.",
      "gx_error_package_resources_budget"
    )
  }
  contents <- lapply(entries, `[[`, "content")
  names(contents) <- paths
  metadata <- gx_package_resources_metadata_impl(resources, timeseries)
  list(
    resources = resources,
    contents = contents,
    metadata = metadata,
    bundle_id = gx_package_resources_id_impl(
      input, timeseries, resources, metadata
    )
  )
}

gx_package_resources_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_package_resources") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_resources_fields) &&
    identical(x$contract_version, .gx_package_resources_contract_version) &&
    is.character(x$stage) && length(x$stage) == 1L &&
    !is.na(x$stage) && is.null(attributes(x$stage)) &&
    is.character(x$timeseries) && length(x$timeseries) == 1L &&
    !is.na(x$timeseries) && is.null(attributes(x$timeseries)) &&
    x$timeseries %in% c("csv", "parquet") &&
    is.character(x$bundle_id) && length(x$bundle_id) == 1L &&
    !is.na(x$bundle_id) && is.null(attributes(x$bundle_id)) &&
    gx_catalog_is_sha256(x$bundle_id)
  if (!valid_top) {
    gx_package_resources_abort(
      "Package resources violate their exact top-level contract."
    )
  }
  input_valid <- tryCatch({
    gx_package_input_validate_impl(x$input)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!input_valid || !identical(x$stage, x$input$stage)) {
    gx_package_resources_abort(
      "Package resources no longer bind one exact M9j input."
    )
  }
  parquet_content <- if (identical(x$timeseries, "parquet") &&
      is.list(x$contents)) {
    x$contents[["data/observations.parquet"]]
  } else {
    NULL
  }
  expected <- gx_package_resources_build_impl(
    x$input, x$timeseries, parquet_content = parquet_content
  )
  valid_resources <- inherits(x$resources, "tbl_df") &&
    identical(names(x$resources), .gx_package_resources_columns)
  valid_contents <- is.list(x$contents) &&
    identical(names(x$contents), x$resources$path) &&
    all(vapply(x$contents, function(content) {
      is.raw(content) && !is.object(content) && is.null(attributes(content))
    }, logical(1)))
  valid_metadata <- is.list(x$metadata) &&
    identical(names(x$metadata), .gx_package_resources_metadata_fields) &&
    is.list(x$metadata$counts) &&
    identical(
      names(x$metadata$counts),
      .gx_package_resources_count_fields
    )
  valid <- valid_resources && valid_contents && valid_metadata &&
    identical(x$resources, expected$resources) &&
    identical(x$contents, expected$contents) &&
    identical(x$metadata, expected$metadata) &&
    identical(x$bundle_id, expected$bundle_id)
  if (!valid) {
    gx_package_resources_abort(
      "Package resource bytes, evidence, metadata, or identity were forged."
    )
  }
  invisible(x)
}

# Internal M9k boundary. It derives exact in-memory bytes only and performs no
# filesystem staging, publication, optional-format, loading, report, or replay.
gx_package_resources_impl <- function(input, timeseries = "csv") {
  built <- gx_package_resources_build_impl(input, timeseries)
  object <- structure(
    list(
      contract_version = .gx_package_resources_contract_version,
      stage = input$stage,
      timeseries = unname(timeseries),
      input = input,
      resources = built$resources,
      contents = built$contents,
      metadata = built$metadata,
      bundle_id = built$bundle_id
    ),
    class = "gx_package_resources"
  )
  gx_package_resources_validate_impl(object)
  object
}
