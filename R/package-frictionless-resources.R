.gx_package_frictionless_resources_contract_version <- "0.2.0"
.gx_package_frictionless_resource_profile <-
  "fixed-in-memory-resources-v2+frictionless-data-package-v1"
.gx_package_frictionless_manifest_profile <-
  "fixed-frictionless-data-package-v1"
.gx_package_frictionless_cli_version <- "5.19.0"

.gx_package_frictionless_resources_fields <- c(
  "contract_version", "stage", "timeseries", "input", "base", "profile",
  "resources", "contents", "metadata", "bundle_id"
)

.gx_package_frictionless_resources_metadata_fields <- c(
  "scope", "in_memory", "deterministic", "serializes", "writes",
  "publishes", "arrow", "arrow_package_version", "quarto", "report",
  "frictionless",
  "profile_id", "cli_validated", "resources", "csv_resources",
  "raw_resources", "stored_bytes", "replayable", "limitations"
)

.gx_package_frictionless_manifest_fields <- c(
  "profile", "contract_version", "standard", "base_bundle_id",
  "profile_id", "descriptor_path", "descriptor_bytes",
  "descriptor_sha256", "cli_version", "cli_validation",
  "runtime_cli_executed"
)

gx_package_frictionless_resources_abort <- function(
    message,
    class = "gx_error_package_frictionless_resources_contract",
    ...,
    call = rlang::caller_env()) {
  gx_package_frictionless_abort(
    message,
    class = unique(c(
      class, "gx_error_package_frictionless_resources"
    )),
    ...,
    call = call
  )
}

gx_package_bundle_is_frictionless_impl <- function(bundle) {
  identical(class(bundle), "gx_package_frictionless_resources")
}

gx_package_frictionless_resources_metadata_impl <- function(
    resources,
    profile,
    base) {
  limitations <- c(
    "cli_validation_external_ci", "replay_deferred"
  )
  limitations <- limitations[gx_catalog_byte_order(limitations)]
  list(
    scope = "fixed_in_memory_resources_with_frictionless_v2",
    in_memory = TRUE,
    deterministic = base$metadata$deterministic,
    serializes = TRUE,
    writes = FALSE,
    publishes = FALSE,
    arrow = base$metadata$arrow,
    arrow_package_version = base$metadata$arrow_package_version,
    quarto = gx_package_bundle_is_report_impl(base),
    report = gx_package_bundle_is_report_impl(base),
    frictionless = TRUE,
    profile_id = profile$profile_id,
    cli_validated = FALSE,
    resources = unname(as.integer(nrow(resources))),
    csv_resources = unname(as.integer(sum(resources$format == "csv"))),
    raw_resources = unname(as.integer(sum(resources$format == "raw"))),
    stored_bytes = unname(as.double(sum(resources$bytes))),
    replayable = FALSE,
    limitations = limitations
  )
}

gx_package_frictionless_resources_id_impl <- function(
    base,
    profile,
    resources,
    metadata) {
  gx_contract_hash(
    list(
      "stage", base$stage,
      "timeseries", base$timeseries,
      "package_input_id", base$input$input_id,
      "base_bundle_id", base$bundle_id,
      "profile_id", profile$profile_id,
      "descriptor_sha256", digest::digest(
        profile$bytes, algo = "sha256", serialize = FALSE
      ),
      "resource_evidence_sha256", gx_package_input_vector_hash_impl(
        paste(resources$path, resources$bytes, resources$sha256, sep = "="),
        "geoconnexr.package-frictionless-resources.evidence.v1"
      ),
      "resources", metadata$resources,
      "stored_bytes", metadata$stored_bytes
    ),
    namespace = "geoconnexr.package-frictionless-resources.v1",
    contract_version = .gx_package_frictionless_resources_contract_version
  )
}

gx_package_frictionless_resources_build_impl <- function(base, profile = NULL) {
  gx_package_bundle_validate_impl(base)
  valid_scope <- !gx_package_bundle_is_frictionless_impl(base) &&
    base$timeseries %in% c("csv", "parquet") &&
    nrow(base$resources) >= 1L &&
    all(base$resources$format %in% c("csv", "parquet", "raw")) &&
    !"datapackage.json" %in% base$resources$path
  if (!valid_scope) {
    gx_package_frictionless_resources_abort(
      paste0(
        "Public Frictionless integration requires one exact finalized fixed ",
        "package bundle without a pre-existing descriptor."
      ),
      "gx_error_package_frictionless_resources_scope"
    )
  }
  if (is.null(profile)) {
    profile <- gx_package_frictionless_impl(base)
  } else {
    gx_package_frictionless_validate_impl(profile)
    if (!identical(profile$bundle, base)) {
      gx_package_frictionless_resources_abort(
        "The Frictionless descriptor no longer binds its exact base bundle.",
        "gx_error_package_frictionless_resources_lineage"
      )
    }
  }
  entry <- gx_package_resources_entry_impl(
    path = "datapackage.json",
    role = "frictionless_descriptor",
    format = "raw",
    media_type = "application/json",
    content = profile$bytes
  )
  resources <- rbind(
    base$resources,
    gx_package_resources_table_impl(list(entry))
  )
  resources <- tibble::as_tibble(resources)
  order <- order(resources$path, method = "radix")
  resources <- resources[order, , drop = FALSE]
  if (anyDuplicated(resources$path) ||
      anyDuplicated(gx_snapshot_ascii_fold(resources$path)) ||
      nrow(resources) > .gx_package_resources_max_resources) {
    gx_package_frictionless_resources_abort(
      "Frictionless package resource paths violate the fixed closed profile.",
      "gx_error_package_frictionless_resources_path"
    )
  }
  contents <- c(base$contents, list("datapackage.json" = entry$content))
  contents <- contents[resources$path]
  total <- sum(resources$bytes)
  if (!is.finite(total) || total > .gx_package_resources_max_total_bytes) {
    gx_package_frictionless_resources_abort(
      "Frictionless package resources exceed the aggregate byte ceiling.",
      "gx_error_package_frictionless_resources_budget"
    )
  }
  metadata <- gx_package_frictionless_resources_metadata_impl(
    resources, profile, base
  )
  list(
    profile = profile,
    resources = resources,
    contents = contents,
    metadata = metadata,
    bundle_id = gx_package_frictionless_resources_id_impl(
      base, profile, resources, metadata
    )
  )
}

gx_package_frictionless_resources_validate_impl <- function(x) {
  valid_top <- is.list(x) &&
    identical(class(x), "gx_package_frictionless_resources") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_frictionless_resources_fields) &&
    identical(
      x$contract_version,
      .gx_package_frictionless_resources_contract_version
    ) &&
    is.character(x$stage) && length(x$stage) == 1L && !is.na(x$stage) &&
    is.null(attributes(x$stage)) &&
    x$timeseries %in% c("csv", "parquet") &&
    is.character(x$bundle_id) && length(x$bundle_id) == 1L &&
    !is.na(x$bundle_id) && is.null(attributes(x$bundle_id)) &&
    gx_catalog_is_sha256(x$bundle_id)
  if (!valid_top) {
    gx_package_frictionless_resources_abort(
      "Frictionless package resources violate their top-level contract."
    )
  }
  base_valid <- tryCatch({
    gx_package_bundle_validate_impl(x$base)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  expected <- if (base_valid) tryCatch(
    gx_package_frictionless_resources_build_impl(x$base, x$profile),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  valid <- !is.null(expected) && identical(x$stage, x$base$stage) &&
    identical(x$timeseries, x$base$timeseries) &&
    identical(x$input, x$base$input) &&
    identical(x$profile, expected$profile) &&
    inherits(x$resources, "tbl_df") &&
    identical(names(x$resources), .gx_package_resources_columns) &&
    is.list(x$contents) && identical(names(x$contents), x$resources$path) &&
    all(vapply(x$contents, function(content) {
      is.raw(content) && !is.object(content) && is.null(attributes(content))
    }, logical(1))) &&
    is.list(x$metadata) && identical(
      names(x$metadata), .gx_package_frictionless_resources_metadata_fields
    ) &&
    identical(x$resources, expected$resources) &&
    identical(x$contents, expected$contents) &&
    identical(x$metadata, expected$metadata) &&
    identical(x$bundle_id, expected$bundle_id)
  if (!valid) {
    gx_package_frictionless_resources_abort(
      "Frictionless package resource bytes, evidence, or identity were forged."
    )
  }
  invisible(x)
}

gx_package_frictionless_resources_impl <- function(base) {
  built <- gx_package_frictionless_resources_build_impl(base)
  object <- structure(
    list(
      contract_version = .gx_package_frictionless_resources_contract_version,
      stage = base$stage,
      timeseries = base$timeseries,
      input = base$input,
      base = base,
      profile = built$profile,
      resources = built$resources,
      contents = built$contents,
      metadata = built$metadata,
      bundle_id = built$bundle_id
    ),
    class = "gx_package_frictionless_resources"
  )
  gx_package_frictionless_resources_validate_impl(object)
  object
}

gx_package_bundle_frictionless_manifest_impl <- function(bundle) {
  if (!gx_package_bundle_is_frictionless_impl(bundle)) return(NULL)
  profile <- bundle$profile
  list(
    profile = .gx_package_frictionless_manifest_profile,
    contract_version = profile$contract_version,
    standard = profile$metadata$standard,
    base_bundle_id = bundle$base$bundle_id,
    profile_id = profile$profile_id,
    descriptor_path = "datapackage.json",
    descriptor_bytes = unname(as.integer(length(profile$bytes))),
    descriptor_sha256 = digest::digest(
      profile$bytes, algo = "sha256", serialize = FALSE
    ),
    cli_version = .gx_package_frictionless_cli_version,
    cli_validation = "pinned_ci_profile",
    runtime_cli_executed = FALSE
  )
}

gx_package_frictionless_manifest_profile_impl <- function(verification) {
  gx_snapshot_verification_validate_impl(verification)
  manifest <- verification$manifest
  serialization <- manifest$effective_options$serialization
  frictionless <- serialization$frictionless
  base_profile <- tryCatch(
    if (isTRUE(manifest$recipe$output$report)) {
      gx_package_report_manifest_profile_impl(
        verification,
        resource_profile = .gx_package_frictionless_resource_profile
      )
    } else {
      gx_package_base_manifest_profile_impl(
        verification,
        resource_profile = .gx_package_frictionless_resource_profile
      )
    },
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  expected_fields <- .gx_package_frictionless_manifest_fields[
    gx_catalog_byte_order(.gx_package_frictionless_manifest_fields)
  ]
  position <- match(
    "datapackage.json",
    vapply(manifest$resources, `[[`, character(1), "path")
  )
  resource <- if (is.na(position)) NULL else manifest$resources[[position]]
  valid_frictionless <- is.list(frictionless) &&
    identical(names(frictionless), expected_fields) &&
    identical(
      frictionless$profile, .gx_package_frictionless_manifest_profile
    ) &&
    identical(
      frictionless$contract_version,
      .gx_package_frictionless_contract_version
    ) &&
    identical(frictionless$standard, .gx_package_frictionless_standard) &&
    gx_catalog_is_sha256(frictionless$base_bundle_id) &&
    gx_catalog_is_sha256(frictionless$profile_id) &&
    identical(frictionless$descriptor_path, "datapackage.json") &&
    is.numeric(frictionless$descriptor_bytes) &&
    length(frictionless$descriptor_bytes) == 1L &&
    is.finite(frictionless$descriptor_bytes) &&
    frictionless$descriptor_bytes >= 1 &&
    frictionless$descriptor_bytes <= .gx_package_frictionless_max_bytes &&
    frictionless$descriptor_bytes == trunc(frictionless$descriptor_bytes) &&
    gx_catalog_is_sha256(frictionless$descriptor_sha256) &&
    identical(
      frictionless$cli_version, .gx_package_frictionless_cli_version
    ) &&
    identical(frictionless$cli_validation, "pinned_ci_profile") &&
    identical(frictionless$runtime_cli_executed, FALSE)
  valid_resource <- is.list(resource) &&
    identical(resource$media_type, "application/json") &&
    identical(resource$bytes, frictionless$descriptor_bytes) &&
    identical(resource$sha256, frictionless$descriptor_sha256) &&
    identical(
      unlist(resource$roles, use.names = FALSE),
      c("metadata", "frictionless", "data-package-v1")
    )
  valid <- !is.null(base_profile) && valid_frictionless && valid_resource
  if (!valid) {
    gx_package_frictionless_resources_abort(
      "Package verification is outside the fixed Frictionless profile.",
      "gx_error_package_frictionless_resources_profile"
    )
  }
  base_profile$frictionless <- frictionless
  base_profile
}

gx_package_frictionless_loaded_validate_impl <- function(
    profile,
    resources,
    contents) {
  if (is.null(profile$frictionless)) return(invisible(TRUE))
  position <- match("datapackage.json", resources$path)
  base_positions <- which(resources$role != "frictionless_descriptor")
  valid_scope <- !is.na(position) && length(base_positions) >= 1L &&
    all(resources$format[base_positions] %in% c("csv", "parquet", "raw"))
  if (!valid_scope) {
    gx_package_frictionless_resources_abort(
      "Loaded Frictionless resources violate the fixed mixed-resource profile.",
      "gx_error_package_frictionless_resources_loaded"
    )
  }
  entries <- lapply(base_positions, function(index) {
    gx_package_resources_entry_impl(
      path = resources$path[[index]],
      role = resources$role[[index]],
      format = resources$format[[index]],
      media_type = resources$media_type[[index]],
      content = contents[[resources$path[[index]]]]
    )
  })
  table <- gx_package_resources_table_impl(entries)
  base_contents <- contents[resources$path[base_positions]]
  descriptor <- gx_package_frictionless_descriptor_records_impl(
    table, base_contents
  )
  bytes <- gx_package_frictionless_json_bytes_impl(descriptor)
  metadata <- gx_package_frictionless_metadata_records_impl(table, bytes)
  evidence <- profile$frictionless
  expected_id <- gx_package_frictionless_id_values_impl(
    profile$stage, evidence$base_bundle_id, bytes, metadata
  )
  valid <- identical(contents[[position]], bytes) &&
    identical(evidence$profile_id, expected_id) &&
    identical(evidence$descriptor_bytes, unname(as.integer(length(bytes)))) &&
    identical(
      evidence$descriptor_sha256,
      digest::digest(bytes, algo = "sha256", serialize = FALSE)
    )
  if (!valid) {
    gx_package_frictionless_resources_abort(
      "Loaded datapackage.json bytes no longer describe the stored resources.",
      "gx_error_package_frictionless_resources_loaded"
    )
  }
  invisible(TRUE)
}
