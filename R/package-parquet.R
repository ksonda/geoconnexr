.gx_package_parquet_contract_version <- "0.1.0"
.gx_package_parquet_arrow_minimum <- "14.0.0"
.gx_package_parquet_required_exports <- c(
  "write_parquet", "read_parquet", "BufferOutputStream"
)
.gx_package_parquet_max_bytes <- 128 * 1024^2
.gx_package_parquet_buffer_capacity <- 1024L * 1024L
.gx_package_parquet_data_page_size <- 1024L * 1024L

.gx_package_parquet_fields <- c(
  "contract_version", "mode", "status", "harmonized", "table", "content",
  "metadata", "parquet_id"
)

.gx_package_parquet_metadata_fields <- c(
  "scope", "arrow_package_version", "arrow_minimum_version",
  "required_exports", "parquet_version", "compression",
  "compression_level", "use_dictionary", "write_statistics",
  "data_page_size", "buffer_initial_capacity", "chunk_size",
  "use_deprecated_int96_timestamps", "coerce_timestamps",
  "allow_truncated_timestamps", "rows", "columns", "bytes", "sha256",
  "in_memory", "deterministic_within_arrow_version",
  "cross_version_byte_stability", "writes", "public", "replayable",
  "limitations"
)

gx_package_parquet_abort <- function(
    message,
    class = "gx_error_package_parquet_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_parquet", "gx_error_package"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_parquet_arrow_version_impl <- function(package = "arrow") {
  if (!identical(package, "arrow")) {
    gx_package_parquet_abort(
      "The Parquet capability probe accepts only Arrow metadata.",
      "gx_error_package_parquet_capability"
    )
  }
  gx_package_options_package_version_impl(package)
}

gx_package_parquet_namespace_impl <- function(package = "arrow") {
  loadNamespace(package)
}

gx_package_parquet_namespace_version_impl <- function(namespace) {
  as.character(getNamespaceVersion(namespace))
}

gx_package_parquet_export_impl <- function(package, symbol) {
  getExportedValue(package, symbol)
}

gx_package_parquet_call_safely <- function(code, message, class) {
  tryCatch(
    withCallingHandlers(
      code,
      warning = function(cnd) {
        gx_package_parquet_abort(message, class)
      }
    ),
    gx_error_package_parquet = function(cnd) stop(cnd),
    error = function(cnd) gx_package_parquet_abort(message, class)
  )
}

gx_package_parquet_capability_impl <- function(
    version_resolver = gx_package_parquet_arrow_version_impl,
    namespace_loader = gx_package_parquet_namespace_impl,
    namespace_version_resolver = gx_package_parquet_namespace_version_impl,
    export_resolver = gx_package_parquet_export_impl) {
  resolvers <- list(
    version_resolver, namespace_loader, namespace_version_resolver,
    export_resolver
  )
  if (!all(vapply(resolvers, is.function, logical(1)))) {
    gx_package_parquet_abort(
      "Parquet capability resolvers must be functions.",
      "gx_error_package_parquet_input"
    )
  }
  observed <- gx_package_parquet_call_safely(
    version_resolver("arrow"),
    "Arrow package metadata could not be inspected safely.",
    "gx_error_package_parquet_capability"
  )
  observed <- tryCatch(
    gx_fetch_preflight_version_normalize(observed),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(observed)) {
    gx_package_parquet_abort(
      "Arrow package metadata violates the version contract.",
      "gx_error_package_parquet_capability"
    )
  }
  if (is.na(observed)) {
    gx_package_parquet_abort(
      "Parquet serialization requires the optional Arrow package.",
      "gx_error_package_parquet_missing"
    )
  }
  if (utils::compareVersion(
    observed, .gx_package_parquet_arrow_minimum
  ) < 0L) {
    gx_package_parquet_abort(
      "The installed Arrow package is older than the reviewed minimum.",
      "gx_error_package_parquet_version"
    )
  }

  namespace <- gx_package_parquet_call_safely(
    namespace_loader("arrow"),
    "The reviewed Arrow namespace could not be loaded.",
    "gx_error_package_parquet_capability"
  )
  loaded <- gx_package_parquet_call_safely(
    namespace_version_resolver(namespace),
    "The loaded Arrow namespace version could not be verified.",
    "gx_error_package_parquet_capability"
  )
  loaded <- tryCatch(
    gx_fetch_preflight_version_normalize(loaded, allow_na = FALSE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(loaded) || !identical(loaded, observed)) {
    gx_package_parquet_abort(
      "Arrow package metadata changed before capability resolution.",
      "gx_error_package_parquet_race"
    )
  }

  exports <- lapply(.gx_package_parquet_required_exports, function(symbol) {
    gx_package_parquet_call_safely(
      export_resolver("arrow", symbol),
      "A required reviewed Arrow export is unavailable.",
      "gx_error_package_parquet_symbol"
    )
  })
  names(exports) <- .gx_package_parquet_required_exports
  writer_formals <- c(
    "x", "sink", "chunk_size", "version", "compression",
    "compression_level", "use_dictionary", "write_statistics",
    "data_page_size", "use_deprecated_int96_timestamps",
    "coerce_timestamps", "allow_truncated_timestamps"
  )
  reader_formals <- c("file", "as_data_frame", "mmap")
  buffer <- exports$BufferOutputStream
  valid <- is.function(exports$write_parquet) &&
    all(writer_formals %in% names(formals(exports$write_parquet))) &&
    is.function(exports$read_parquet) &&
    all(reader_formals %in% names(formals(exports$read_parquet))) &&
    is.environment(buffer) && is.function(buffer$create) &&
    "initial_capacity" %in% names(formals(buffer$create))
  if (!valid) {
    gx_package_parquet_abort(
      "Reviewed Arrow exports no longer match the fixed symbol contract.",
      "gx_error_package_parquet_symbol"
    )
  }
  list(
    version = loaded,
    write_parquet = exports$write_parquet,
    read_parquet = exports$read_parquet,
    BufferOutputStream = buffer
  )
}

gx_package_parquet_table_validate_impl <- function(table) {
  template <- as.data.frame(
    gx_harmonized_empty_observations_impl(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  valid <- inherits(table, "data.frame") &&
    identical(class(table), "data.frame") &&
    identical(names(table), .gx_harmonized_observation_columns) &&
    identical(
      vapply(table, typeof, character(1)),
      vapply(template, typeof, character(1))
    ) &&
    identical(
      lapply(table, class),
      lapply(template, class)
    ) &&
    identical(attr(table$datetime, "tzone"), "UTC") &&
    as.double(nrow(table)) * as.double(ncol(table)) <=
      .gx_package_resources_max_fields &&
    all(lengths(table) == nrow(table))
  if (!valid) {
    gx_package_parquet_abort(
      "The Parquet observation table violates its exact typed schema.",
      "gx_error_package_parquet_table"
    )
  }
  character_columns <- vapply(table, is.character, logical(1))
  tryCatch(
    gx_snapshot_writer_validate_text(table[character_columns]),
    error = function(cnd) {
      gx_package_parquet_abort(
        "The Parquet observation table contains unsafe text.",
        "gx_error_package_parquet_table"
      )
    }
  )
  invisible(table)
}

gx_package_parquet_table_impl <- function(harmonized) {
  tryCatch(
    gx_harmonized_validate_impl(harmonized),
    error = function(cnd) {
      gx_package_parquet_abort(
        "Parquet serialization requires one exact harmonized result.",
        "gx_error_package_parquet_input"
      )
    }
  )
  table <- harmonized$observations[
    , .gx_harmonized_observation_columns, drop = FALSE
  ]
  table <- gx_snapshot_writer_redact_view(table)
  table <- as.data.frame(
    table,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  gx_package_parquet_table_validate_impl(table)
  table
}

gx_package_parquet_serialize_impl <- function(table, capability) {
  gx_package_parquet_table_validate_impl(table)
  valid_capability <- is.list(capability) &&
    identical(
      names(capability),
      c("version", "write_parquet", "read_parquet", "BufferOutputStream")
    ) && is.character(capability$version) && length(capability$version) == 1L &&
    !is.na(capability$version) && is.function(capability$write_parquet) &&
    is.function(capability$read_parquet) &&
    is.environment(capability$BufferOutputStream) &&
    is.function(capability$BufferOutputStream$create)
  if (!valid_capability) {
    gx_package_parquet_abort(
      "Resolved Arrow capabilities violate their exact contract.",
      "gx_error_package_parquet_capability"
    )
  }
  chunk_size <- unname(as.integer(max(1L, nrow(table))))
  sink <- gx_package_parquet_call_safely(
    capability$BufferOutputStream$create(
      initial_capacity = .gx_package_parquet_buffer_capacity
    ),
    "The in-memory Arrow output stream could not be created.",
    "gx_error_package_parquet_serialization"
  )
  gx_package_parquet_call_safely(
    capability$write_parquet(
      x = table,
      sink = sink,
      chunk_size = chunk_size,
      version = "2.4",
      compression = "uncompressed",
      compression_level = NULL,
      use_dictionary = FALSE,
      write_statistics = FALSE,
      data_page_size = .gx_package_parquet_data_page_size,
      use_deprecated_int96_timestamps = FALSE,
      coerce_timestamps = "us",
      allow_truncated_timestamps = FALSE
    ),
    "Arrow could not serialize the fixed Parquet profile.",
    "gx_error_package_parquet_serialization"
  )
  content <- gx_package_parquet_call_safely(
    as.raw(sink$finish()),
    "The in-memory Arrow output stream could not be finalized.",
    "gx_error_package_parquet_serialization"
  )
  valid_content <- is.raw(content) && !is.object(content) &&
    is.null(attributes(content)) && length(content) >= 8L &&
    length(content) <= .gx_package_parquet_max_bytes &&
    identical(content[seq_len(4L)], charToRaw("PAR1")) &&
    identical(content[(length(content) - 3L):length(content)], charToRaw("PAR1"))
  if (!valid_content) {
    gx_package_parquet_abort(
      "Arrow returned invalid or over-budget Parquet bytes.",
      "gx_error_package_parquet_serialization"
    )
  }
  roundtrip <- gx_package_parquet_call_safely(
    capability$read_parquet(
      file = content,
      as_data_frame = TRUE,
      mmap = FALSE
    ),
    "The fixed Parquet bytes could not be read back in memory.",
    "gx_error_package_parquet_roundtrip"
  )
  roundtrip <- tryCatch(
    as.data.frame(
      roundtrip,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(roundtrip) || !identical(roundtrip, table)) {
    gx_package_parquet_abort(
      "Parquet read-back did not preserve the exact typed observation table.",
      "gx_error_package_parquet_roundtrip"
    )
  }
  list(content = content, chunk_size = chunk_size)
}

gx_package_parquet_read_impl <- function(
    content,
    capability_resolver = gx_package_parquet_capability_impl) {
  valid_content <- is.raw(content) && !is.object(content) &&
    is.null(attributes(content)) && length(content) >= 8L &&
    length(content) <= .gx_package_parquet_max_bytes &&
    identical(content[seq_len(4L)], charToRaw("PAR1")) &&
    identical(
      content[(length(content) - 3L):length(content)],
      charToRaw("PAR1")
    )
  if (!valid_content || !is.function(capability_resolver)) {
    gx_package_parquet_abort(
      "Parquet hydration requires exact bounded self-identifying bytes.",
      "gx_error_package_parquet_input"
    )
  }
  capability <- gx_package_parquet_call_safely(
    capability_resolver(),
    "The reviewed Arrow capability could not be resolved.",
    "gx_error_package_parquet_capability"
  )
  value <- gx_package_parquet_call_safely(
    capability$read_parquet(
      file = content,
      as_data_frame = TRUE,
      mmap = FALSE
    ),
    "The fixed Parquet resource could not be read in memory.",
    "gx_error_package_parquet_roundtrip"
  )
  table <- tryCatch(
    as.data.frame(
      value,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(table)) {
    gx_package_parquet_abort(
      "The Parquet resource did not produce one exact data frame.",
      "gx_error_package_parquet_roundtrip"
    )
  }
  gx_package_parquet_table_validate_impl(table)
  table
}

gx_package_parquet_metadata_impl <- function(
    table, content, capability, chunk_size) {
  limitations <- c(
    "bundle_integration_deferred", "cross_arrow_version_bytes_unstable",
    "loading_deferred", "public_api_deferred", "replay_deferred",
    "writes_deferred"
  )
  limitations <- limitations[gx_catalog_byte_order(limitations)]
  list(
    scope = "fixed_arrow_parquet_v1",
    arrow_package_version = capability$version,
    arrow_minimum_version = .gx_package_parquet_arrow_minimum,
    required_exports = .gx_package_parquet_required_exports,
    parquet_version = "2.4",
    compression = "uncompressed",
    compression_level = "not_applicable",
    use_dictionary = FALSE,
    write_statistics = FALSE,
    data_page_size = .gx_package_parquet_data_page_size,
    buffer_initial_capacity = .gx_package_parquet_buffer_capacity,
    chunk_size = chunk_size,
    use_deprecated_int96_timestamps = FALSE,
    coerce_timestamps = "us",
    allow_truncated_timestamps = FALSE,
    rows = unname(as.integer(nrow(table))),
    columns = unname(as.integer(ncol(table))),
    bytes = unname(as.double(length(content))),
    sha256 = digest::digest(content, algo = "sha256", serialize = FALSE),
    in_memory = TRUE,
    deterministic_within_arrow_version = TRUE,
    cross_version_byte_stability = FALSE,
    writes = FALSE,
    public = FALSE,
    replayable = FALSE,
    limitations = limitations
  )
}

gx_package_parquet_id_impl <- function(metadata) {
  gx_contract_hash(
    list(
      "scope", metadata$scope,
      "arrow_package_version", metadata$arrow_package_version,
      "parquet_version", metadata$parquet_version,
      "rows", metadata$rows,
      "columns", metadata$columns,
      "bytes", metadata$bytes,
      "sha256", metadata$sha256
    ),
    namespace = "geoconnexr.package-parquet.v1",
    contract_version = .gx_package_parquet_contract_version
  )
}

gx_package_parquet_build_impl <- function(harmonized, capability_resolver) {
  if (!is.function(capability_resolver)) {
    gx_package_parquet_abort(
      "The Parquet capability resolver must be a function.",
      "gx_error_package_parquet_input"
    )
  }
  table <- gx_package_parquet_table_impl(harmonized)
  capability <- gx_package_parquet_call_safely(
    capability_resolver(),
    "The reviewed Arrow capability could not be resolved.",
    "gx_error_package_parquet_capability"
  )
  serialized <- gx_package_parquet_serialize_impl(table, capability)
  metadata <- gx_package_parquet_metadata_impl(
    table, serialized$content, capability, serialized$chunk_size
  )
  list(
    table = table,
    content = serialized$content,
    metadata = metadata,
    parquet_id = gx_package_parquet_id_impl(metadata)
  )
}

gx_package_parquet_validate_impl <- function(
    x,
    capability_resolver = gx_package_parquet_capability_impl) {
  valid_top <- is.list(x) && identical(class(x), "gx_package_parquet") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_parquet_fields) &&
    identical(x$contract_version, .gx_package_parquet_contract_version) &&
    identical(x$mode, "fixed_arrow_parquet_v1") &&
    identical(x$status, "serialized_verified") &&
    is.raw(x$content) && !is.object(x$content) &&
    is.null(attributes(x$content)) &&
    is.list(x$metadata) &&
    identical(names(x$metadata), .gx_package_parquet_metadata_fields) &&
    is.character(x$parquet_id) && length(x$parquet_id) == 1L &&
    !is.na(x$parquet_id) && is.null(attributes(x$parquet_id)) &&
    gx_catalog_is_sha256(x$parquet_id)
  if (!valid_top) {
    gx_package_parquet_abort(
      "Parquet evidence violates its exact top-level contract."
    )
  }
  expected <- tryCatch(
    gx_package_parquet_build_impl(x$harmonized, capability_resolver),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  valid <- !is.null(expected) && identical(x$table, expected$table) &&
    identical(x$content, expected$content) &&
    identical(x$metadata, expected$metadata) &&
    identical(x$parquet_id, expected$parquet_id)
  if (!valid) {
    gx_package_parquet_abort(
      "Parquet source, bytes, metadata, or identity were forged."
    )
  }
  invisible(x)
}

# Internal M9u boundary. It resolves the reviewed Arrow capability only after
# validating a harmonized source, then writes and reads one fixed, redacted
# observation profile entirely in memory. Bundle integration remains deferred.
gx_package_parquet_impl <- function(
    harmonized,
    capability_resolver = gx_package_parquet_capability_impl) {
  built <- gx_package_parquet_build_impl(harmonized, capability_resolver)
  object <- structure(
    list(
      contract_version = .gx_package_parquet_contract_version,
      mode = "fixed_arrow_parquet_v1",
      status = "serialized_verified",
      harmonized = harmonized,
      table = built$table,
      content = built$content,
      metadata = built$metadata,
      parquet_id = built$parquet_id
    ),
    class = "gx_package_parquet"
  )
  gx_package_parquet_validate_impl(object, capability_resolver)
  object
}
