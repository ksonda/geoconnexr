.gx_package_frictionless_contract_version <- "0.2.0"
.gx_package_frictionless_standard <- "data-package-v1"
.gx_package_frictionless_opaque_profile <- "opaque-file-v1"
.gx_package_frictionless_max_bytes <- 16 * 1024^2

.gx_package_frictionless_fields <- c(
  "contract_version", "mode", "status", "stage", "bundle", "descriptor",
  "bytes", "metadata", "profile_id"
)

.gx_package_frictionless_metadata_fields <- c(
  "scope", "in_memory", "deterministic", "writes", "publishes",
  "standard", "mixed_resource_profiles", "descriptor_validated",
  "cli_validated", "resources", "tabular_resources", "opaque_resources",
  "stored_bytes", "descriptor_bytes", "frictionless", "replayable",
  "limitations"
)

gx_package_frictionless_abort <- function(
    message,
    class = "gx_error_package_frictionless_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_frictionless", "gx_error_package_resources",
      "gx_error_package"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_frictionless_resource_name_impl <- function(path) {
  paste0(
    "resource-",
    digest::digest(enc2utf8(path), algo = "sha256", serialize = FALSE)
  )
}

gx_package_frictionless_media_type_impl <- function(media_type) {
  unname(sub(";.*\\z", "", media_type, perl = TRUE))
}

gx_package_frictionless_resource_impl <- function(resource, content) {
  valid_resource <- inherits(resource, "data.frame") && nrow(resource) == 1L &&
    all(.gx_package_resources_columns %in% names(resource))
  valid_content <- is.raw(content) && !is.object(content) &&
    is.null(attributes(content))
  if (!valid_resource || !valid_content ||
      !identical(as.numeric(length(content)), resource$bytes[[1L]]) ||
      !identical(
        digest::digest(content, algo = "sha256", serialize = FALSE),
        resource$sha256[[1L]]
      )) {
    gx_package_frictionless_abort(
      "A Frictionless resource no longer binds its exact bundle bytes.",
      "gx_error_package_frictionless_binding"
    )
  }
  path <- resource$path[[1L]]
  format <- resource$format[[1L]]
  media_type <- gx_package_frictionless_media_type_impl(
    resource$media_type[[1L]]
  )
  common <- list(
    name = gx_package_frictionless_resource_name_impl(path),
    path = path,
    title = path,
    description = paste0(
      "geoconnexr fixed-profile ", resource$role[[1L]], " resource."
    ),
    bytes = unname(as.integer(resource$bytes[[1L]])),
    hash = paste0("sha256:", resource$sha256[[1L]])
  )
  if (identical(format, "csv")) {
    table <- tryCatch(
      gx_package_table_parse_impl(content),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
    if (is.null(table) || !ncol(table)) {
      gx_package_frictionless_abort(
        "A Frictionless tabular resource is not one canonical CSV table.",
        "gx_error_package_frictionless_table"
      )
    }
    fields <- lapply(names(table), function(name) {
      list(name = unname(name), type = "string")
    })
    return(c(
      list(profile = "tabular-data-resource"),
      common,
      list(
        format = "csv",
        mediatype = "text/csv",
        encoding = "utf-8",
        schema = list(fields = fields)
      )
    ))
  }
  extension <- tolower(tools::file_ext(path))
  if (!nzchar(extension)) extension <- if (identical(format, "parquet")) {
    "parquet"
  } else {
    "bin"
  }
  descriptor <- c(
    list(profile = "data-resource"),
    common,
    list(
      format = "bin",
      mediatype = media_type,
      geoconnexr = list(
        format = extension,
        validation = .gx_package_frictionless_opaque_profile
      )
    )
  )
  if (startsWith(media_type, "text/")) descriptor$encoding <- "utf-8"
  descriptor
}

gx_package_frictionless_descriptor_records_impl <- function(
    resource_table,
    contents) {
  resources <- lapply(seq_len(nrow(resource_table)), function(index) {
    path <- resource_table$path[[index]]
    gx_package_frictionless_resource_impl(
      resource_table[index, , drop = FALSE], contents[[path]]
    )
  })
  names <- vapply(resources, `[[`, character(1), "name")
  paths <- vapply(resources, `[[`, character(1), "path")
  valid <- length(resources) >= 1L && !anyDuplicated(names) &&
    identical(paths, resource_table$path) &&
    all(grepl("^resource-[a-f0-9]{64}\\z", names, perl = TRUE))
  if (!valid) {
    gx_package_frictionless_abort(
      "The Frictionless resource identities are ambiguous or out of order.",
      "gx_error_package_frictionless_resource"
    )
  }
  list(
    profile = "data-package",
    name = "geoconnexr-package",
    title = "Verified geoconnexr data package",
    description = paste0(
      "A fixed-profile geoconnexr package. Integrity is bound by the ",
      "companion manifest.json; this descriptor does not establish ",
      "authenticity or replay authority."
    ),
    resources = resources
  )
}

gx_package_frictionless_descriptor_impl <- function(bundle) {
  if (identical(class(bundle), "gx_package_frictionless_resources")) {
    gx_package_frictionless_abort(
      "A Frictionless descriptor cannot recursively describe itself.",
      "gx_error_package_frictionless_resource"
    )
  }
  gx_package_bundle_validate_impl(bundle)
  gx_package_frictionless_descriptor_records_impl(
    bundle$resources, bundle$contents
  )
}

gx_package_frictionless_json_bytes_impl <- function(descriptor) {
  text <- tryCatch(
    jsonlite::toJSON(
      descriptor,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA,
      pretty = TRUE,
      force = TRUE
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(text) || length(text) != 1L) {
    gx_package_frictionless_abort(
      "The Frictionless descriptor could not be serialized.",
      "gx_error_package_frictionless_json"
    )
  }
  text <- sub("[\\r\\n]+\\z", "", enc2utf8(as.character(text)), perl = TRUE)
  bytes <- charToRaw(paste0(text, "\n"))
  if (length(bytes) > .gx_package_frictionless_max_bytes) {
    gx_package_frictionless_abort(
      "The Frictionless descriptor exceeds its serialized-byte ceiling.",
      "gx_error_package_frictionless_budget"
    )
  }
  decoded <- tryCatch(
    gx_snapshot_parse_json(bytes),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(decoded) || !identical(
      gx_snapshot_normalize_json(decoded),
      gx_snapshot_normalize_json(descriptor)
  )) {
    gx_package_frictionless_abort(
      "The Frictionless descriptor did not survive exact JSON round-trip.",
      "gx_error_package_frictionless_json"
    )
  }
  bytes
}

gx_package_frictionless_metadata_records_impl <- function(resources, bytes) {
  tabular <- sum(resources$format == "csv")
  resource_count <- nrow(resources)
  limitations <- c(
    "cli_runtime_external", "publication_deferred", "replay_deferred"
  )
  limitations <- limitations[gx_catalog_byte_order(limitations)]
  list(
    scope = "fixed_frictionless_descriptor_v1",
    in_memory = TRUE,
    deterministic = TRUE,
    writes = FALSE,
    publishes = FALSE,
    standard = .gx_package_frictionless_standard,
    mixed_resource_profiles = tabular != resource_count,
    descriptor_validated = TRUE,
    cli_validated = FALSE,
    resources = unname(as.integer(resource_count)),
    tabular_resources = unname(as.integer(tabular)),
    opaque_resources = unname(as.integer(resource_count - tabular)),
    stored_bytes = unname(as.double(sum(resources$bytes))),
    descriptor_bytes = unname(as.double(length(bytes))),
    frictionless = TRUE,
    replayable = FALSE,
    limitations = limitations
  )
}

gx_package_frictionless_metadata_impl <- function(bundle, bytes) {
  gx_package_frictionless_metadata_records_impl(bundle$resources, bytes)
}

gx_package_frictionless_id_values_impl <- function(
    stage,
    bundle_id,
    bytes,
    metadata) {
  gx_contract_hash(
    list(
      "mode", "fixed_frictionless_descriptor",
      "status", "described_and_validated",
      "stage", stage,
      "bundle_id", bundle_id,
      "descriptor_sha256", digest::digest(
        bytes, algo = "sha256", serialize = FALSE
      ),
      "resources", metadata$resources,
      "tabular_resources", metadata$tabular_resources,
      "opaque_resources", metadata$opaque_resources
    ),
    namespace = "geoconnexr.package-frictionless.v1",
    contract_version = .gx_package_frictionless_contract_version
  )
}

gx_package_frictionless_id_impl <- function(bundle, bytes, metadata) {
  gx_package_frictionless_id_values_impl(
    bundle$stage, bundle$bundle_id, bytes, metadata
  )
}

gx_package_frictionless_validate_impl <- function(x) {
  valid_top <- is.list(x) &&
    identical(class(x), "gx_package_frictionless") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_frictionless_fields) &&
    identical(x$contract_version, .gx_package_frictionless_contract_version) &&
    identical(x$mode, "fixed_frictionless_descriptor") &&
    identical(x$status, "described_and_validated") &&
    is.character(x$stage) && length(x$stage) == 1L && !is.na(x$stage) &&
    is.null(attributes(x$stage)) &&
    is.raw(x$bytes) && !is.object(x$bytes) && is.null(attributes(x$bytes)) &&
    length(x$bytes) <= .gx_package_frictionless_max_bytes &&
    is.character(x$profile_id) && length(x$profile_id) == 1L &&
    !is.na(x$profile_id) && is.null(attributes(x$profile_id)) &&
    gx_catalog_is_sha256(x$profile_id)
  if (!valid_top) {
    gx_package_frictionless_abort(
      "Frictionless evidence violates its exact top-level contract."
    )
  }
  bundle_valid <- tryCatch({
    gx_package_bundle_validate_impl(x$bundle)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  descriptor <- if (bundle_valid) tryCatch(
    gx_package_frictionless_descriptor_records_impl(
      x$bundle$resources, x$bundle$contents
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  bytes <- if (!is.null(descriptor)) tryCatch(
    gx_package_frictionless_json_bytes_impl(descriptor),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  metadata <- if (!is.null(bytes)) {
    gx_package_frictionless_metadata_impl(x$bundle, bytes)
  } else {
    NULL
  }
  profile_id <- if (!is.null(metadata)) {
    gx_package_frictionless_id_impl(x$bundle, bytes, metadata)
  } else {
    NULL
  }
  valid <- bundle_valid && !is.null(descriptor) && !is.null(bytes) &&
    identical(x$stage, x$bundle$stage) &&
    identical(x$descriptor, descriptor) && identical(x$bytes, bytes) &&
    is.list(x$metadata) &&
    identical(names(x$metadata), .gx_package_frictionless_metadata_fields) &&
    identical(x$metadata, metadata) && !is.null(profile_id) &&
    identical(x$profile_id, profile_id)
  if (!valid) {
    gx_package_frictionless_abort(
      "Frictionless evidence no longer binds its exact package bundle."
    )
  }
  invisible(x)
}

# Internal M9ac boundary. It describes one exact M9k/M9z bundle in memory
# using the mixed-resource Frictionless Data Package v1 profile. It performs
# no filesystem write, CLI invocation, publication, refresh, or replay.
gx_package_frictionless_impl <- function(bundle) {
  descriptor <- gx_package_frictionless_descriptor_impl(bundle)
  bytes <- gx_package_frictionless_json_bytes_impl(descriptor)
  metadata <- gx_package_frictionless_metadata_impl(bundle, bytes)
  object <- structure(
    list(
      contract_version = .gx_package_frictionless_contract_version,
      mode = "fixed_frictionless_descriptor",
      status = "described_and_validated",
      stage = bundle$stage,
      bundle = bundle,
      descriptor = descriptor,
      bytes = bytes,
      metadata = metadata,
      profile_id = gx_package_frictionless_id_impl(bundle, bytes, metadata)
    ),
    class = "gx_package_frictionless"
  )
  gx_package_frictionless_validate_impl(object)
  object
}
