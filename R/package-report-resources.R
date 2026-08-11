.gx_package_report_resources_contract_version <- "0.1.0"
.gx_package_report_resource_profile <-
  "fixed-in-memory-resources-v2+fixed-report-v1"

.gx_package_report_resources_fields <- c(
  "contract_version", "stage", "timeseries", "input", "base", "report",
  "resources", "contents", "metadata", "bundle_id"
)

.gx_package_report_resources_metadata_fields <- c(
  "scope", "in_memory", "deterministic", "serializes", "writes",
  "publishes", "arrow", "arrow_package_version", "quarto",
  "quarto_package_version", "quarto_cli_version", "report", "report_id",
  "frictionless", "replayable", "counts", "limitations"
)

.gx_package_report_resources_count_fields <- c(
  "resources", "catalog_resources", "fetch_resources", "native_resources",
  "harmonized_resources", "report_resources", "csv_resources",
  "parquet_resources", "raw_resources", "stored_bytes"
)

gx_package_report_resources_abort <- function(
    message,
    class = "gx_error_package_report_resources_contract",
    ...,
    call = rlang::caller_env()) {
  gx_package_resources_abort(
    message,
    class = unique(c(
      class, "gx_error_package_report_resources",
      "gx_error_package_report"
    )),
    ...,
    call = call
  )
}

gx_package_report_resources_origin_impl <- function(base, report) {
  gx_package_resources_validate_impl(base)
  gx_package_report_validate_impl(report)
  verification <- report$hydrated$table_view$loaded$verification
  serialization <- verification$manifest$effective_options$serialization
  valid <- identical(report$stage, base$stage) &&
    identical(report$hydrated$stage, base$stage) &&
    identical(
      verification$manifest$recipe$output$timeseries,
      base$timeseries
    ) &&
    identical(verification$manifest$recipe$output$report, FALSE) &&
    identical(serialization$writer, .gx_package_writer_profile) &&
    identical(serialization$resource_profile, "fixed-in-memory-resources-v2") &&
    identical(serialization$package_input_id, base$input$input_id) &&
    identical(serialization$bundle_id, base$bundle_id)
  if (!valid) {
    gx_package_report_resources_abort(
      paste0(
        "The verified report does not originate from the exact base ",
        "package bundle."
      ),
      "gx_error_package_report_resources_lineage"
    )
  }
  invisible(verification)
}

gx_package_report_resources_metadata_impl <- function(
    resources,
    base,
    report) {
  roles <- resources$role
  limitations <- c(
    "cross_quarto_version_bytes_unclaimed", "frictionless_deferred",
    "public_report_exposure_deferred", "replay_deferred"
  )
  limitations <- limitations[gx_catalog_byte_order(limitations)]
  list(
    scope = "fixed_in_memory_resources_with_report_v1",
    in_memory = TRUE,
    deterministic = FALSE,
    serializes = TRUE,
    writes = FALSE,
    publishes = FALSE,
    arrow = base$metadata$arrow,
    arrow_package_version = base$metadata$arrow_package_version,
    quarto = TRUE,
    quarto_package_version = report$runtime$package_version,
    quarto_cli_version = report$runtime$cli_version,
    report = TRUE,
    report_id = report$report_id,
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
      report_resources = unname(as.integer(sum(roles == "report_html"))),
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

gx_package_report_resources_id_impl <- function(
    base,
    report,
    resources,
    metadata) {
  gx_contract_hash(
    list(
      "stage", base$stage,
      "timeseries", base$timeseries,
      "package_input_id", base$input$input_id,
      "base_bundle_id", base$bundle_id,
      "report_id", report$report_id,
      "report_sha256", report$output$sha256,
      "resource_evidence_sha256", gx_package_input_vector_hash_impl(
        paste(resources$path, resources$bytes, resources$sha256, sep = "="),
        "geoconnexr.package-report-resources.evidence.v1"
      ),
      "resource_count", metadata$counts$resources,
      "stored_bytes", metadata$counts$stored_bytes
    ),
    namespace = "geoconnexr.package-report-resources.v1",
    contract_version = .gx_package_report_resources_contract_version
  )
}

gx_package_report_resources_build_impl <- function(base, report) {
  gx_package_report_resources_origin_impl(base, report)
  entry <- gx_package_resources_entry_impl(
    path = "report/index.html",
    role = "report_html",
    format = "raw",
    media_type = "text/html; charset=utf-8",
    content = report$output$bytes
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
    gx_package_report_resources_abort(
      "Report package resource paths violate the fixed closed profile.",
      "gx_error_package_report_resources_path"
    )
  }
  contents <- c(base$contents, list("report/index.html" = entry$content))
  contents <- contents[resources$path]
  total <- sum(resources$bytes)
  if (!is.finite(total) || total > .gx_package_resources_max_total_bytes) {
    gx_package_report_resources_abort(
      "Report package resources exceed the aggregate byte ceiling.",
      "gx_error_package_report_resources_budget"
    )
  }
  metadata <- gx_package_report_resources_metadata_impl(
    resources, base, report
  )
  list(
    resources = resources,
    contents = contents,
    metadata = metadata,
    bundle_id = gx_package_report_resources_id_impl(
      base, report, resources, metadata
    )
  )
}

gx_package_report_resources_validate_impl <- function(x) {
  valid_top <- is.list(x) &&
    identical(class(x), "gx_package_report_resources") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_report_resources_fields) &&
    identical(
      x$contract_version, .gx_package_report_resources_contract_version
    ) &&
    is.character(x$stage) && length(x$stage) == 1L && !is.na(x$stage) &&
    is.null(attributes(x$stage)) &&
    is.character(x$timeseries) && length(x$timeseries) == 1L &&
    !is.na(x$timeseries) && is.null(attributes(x$timeseries)) &&
    x$timeseries %in% c("csv", "parquet") &&
    is.character(x$bundle_id) && length(x$bundle_id) == 1L &&
    !is.na(x$bundle_id) && is.null(attributes(x$bundle_id)) &&
    gx_catalog_is_sha256(x$bundle_id)
  if (!valid_top) {
    gx_package_report_resources_abort(
      "Report package resources violate their exact top-level contract."
    )
  }
  base_valid <- tryCatch({
    gx_package_resources_validate_impl(x$base)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  report_valid <- tryCatch({
    gx_package_report_validate_impl(x$report)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  expected <- if (base_valid && report_valid) tryCatch(
    gx_package_report_resources_build_impl(x$base, x$report),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  valid <- !is.null(expected) && identical(x$stage, x$base$stage) &&
    identical(x$timeseries, x$base$timeseries) &&
    identical(x$input, x$base$input) &&
    inherits(x$resources, "tbl_df") &&
    identical(names(x$resources), .gx_package_resources_columns) &&
    is.list(x$contents) && identical(names(x$contents), x$resources$path) &&
    all(vapply(x$contents, function(content) {
      is.raw(content) && !is.object(content) && is.null(attributes(content))
    }, logical(1))) &&
    is.list(x$metadata) && identical(
      names(x$metadata), .gx_package_report_resources_metadata_fields
    ) && is.list(x$metadata$counts) && identical(
      names(x$metadata$counts), .gx_package_report_resources_count_fields
    ) &&
    identical(x$resources, expected$resources) &&
    identical(x$contents, expected$contents) &&
    identical(x$metadata, expected$metadata) &&
    identical(x$bundle_id, expected$bundle_id)
  if (!valid) {
    gx_package_report_resources_abort(
      "Report package resource bytes, evidence, or identity were forged."
    )
  }
  invisible(x)
}

# Internal M9z resource boundary. It admits only an M9y report rendered from
# the exact published form of one M9k base bundle and adds its verified HTML as
# one in-memory report resource without writing or publishing.
gx_package_report_resources_impl <- function(base, report) {
  built <- gx_package_report_resources_build_impl(base, report)
  object <- structure(
    list(
      contract_version = .gx_package_report_resources_contract_version,
      stage = base$stage,
      timeseries = base$timeseries,
      input = base$input,
      base = base,
      report = report,
      resources = built$resources,
      contents = built$contents,
      metadata = built$metadata,
      bundle_id = built$bundle_id
    ),
    class = "gx_package_report_resources"
  )
  gx_package_report_resources_validate_impl(object)
  object
}

gx_package_bundle_is_report_impl <- function(bundle) {
  if (gx_package_bundle_is_frictionless_impl(bundle)) {
    return(gx_package_bundle_is_report_impl(bundle$base))
  }
  identical(class(bundle), "gx_package_report_resources")
}

gx_package_bundle_validate_impl <- function(bundle) {
  if (gx_package_bundle_is_frictionless_impl(bundle)) {
    gx_package_frictionless_resources_validate_impl(bundle)
  } else if (gx_package_bundle_is_report_impl(bundle)) {
    gx_package_report_resources_validate_impl(bundle)
  } else {
    gx_package_resources_validate_impl(bundle)
  }
  invisible(bundle)
}

gx_package_bundle_resource_profile_impl <- function(bundle) {
  if (gx_package_bundle_is_frictionless_impl(bundle)) {
    .gx_package_frictionless_resource_profile
  } else if (gx_package_bundle_is_report_impl(bundle)) {
    .gx_package_report_resource_profile
  } else {
    "fixed-in-memory-resources-v2"
  }
}

gx_package_bundle_report_manifest_impl <- function(bundle) {
  if (gx_package_bundle_is_frictionless_impl(bundle)) {
    bundle <- bundle$base
  }
  if (!gx_package_bundle_is_report_impl(bundle)) return(NULL)
  report <- bundle$report
  list(
    profile = "fixed-quarto-html-report-v1",
    contract_version = report$contract_version,
    report_id = report$report_id,
    base_bundle_id = bundle$base$bundle_id,
    hydration_id = report$hydrated$hydration_id,
    source_manifest_sha256 =
      report$hydrated$table_view$loaded$verification$manifest_sha256,
    source_sha256 = report$source$sha256,
    html_sha256 = report$output$sha256,
    quarto_package_version = report$runtime$package_version,
    quarto_cli_version = report$runtime$cli_version,
    execution_enabled = FALSE,
    cache = FALSE,
    minimal = TRUE,
    embed_resources = TRUE
  )
}

gx_package_report_manifest_profile_impl <- function(
    verification,
    resource_profile = .gx_package_report_resource_profile) {
  gx_snapshot_verification_validate_impl(verification)
  manifest <- verification$manifest
  serialization <- manifest$effective_options$serialization
  report <- serialization$report
  stage <- manifest$effective_options$package_stage
  timeseries <- manifest$recipe$output$timeseries
  parquet <- serialization$parquet
  expected_fields <- c(
    "profile", "contract_version", "report_id", "base_bundle_id",
    "hydration_id", "source_manifest_sha256", "source_sha256",
    "html_sha256", "quarto_package_version", "quarto_cli_version",
    "execution_enabled", "cache", "minimal", "embed_resources"
  )
  expected_fields <- expected_fields[gx_catalog_byte_order(expected_fields)]
  resource_position <- match(
    "report/index.html",
    vapply(manifest$resources, `[[`, character(1), "path")
  )
  resource <- if (is.na(resource_position)) NULL else
    manifest$resources[[resource_position]]
  package_version <- tryCatch(
    gx_fetch_preflight_version_normalize(
      report$quarto_package_version,
      allow_na = FALSE
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  cli_version <- tryCatch(
    gx_fetch_preflight_version_normalize(
      report$quarto_cli_version,
      allow_na = FALSE
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  valid_report <- is.list(report) && identical(names(report), expected_fields) &&
    identical(report$profile, "fixed-quarto-html-report-v1") &&
    identical(report$contract_version, .gx_package_report_contract_version) &&
    gx_catalog_is_sha256(report$report_id) &&
    gx_catalog_is_sha256(report$base_bundle_id) &&
    gx_catalog_is_sha256(report$hydration_id) &&
    gx_catalog_is_sha256(report$source_manifest_sha256) &&
    gx_catalog_is_sha256(report$source_sha256) &&
    gx_catalog_is_sha256(report$html_sha256) &&
    !is.null(package_version) && identical(
      package_version, report$quarto_package_version
    ) && utils::compareVersion(
      package_version, .gx_package_quarto_minimum
    ) >= 0L &&
    !is.null(cli_version) && identical(
      cli_version, report$quarto_cli_version
    ) && utils::compareVersion(
      cli_version, .gx_package_quarto_cli_minimum
    ) >= 0L &&
    identical(report$execution_enabled, FALSE) &&
    identical(report$cache, FALSE) &&
    identical(report$minimal, TRUE) &&
    identical(report$embed_resources, TRUE)
  valid_resource <- is.list(resource) &&
    identical(resource$media_type, "text/html; charset=utf-8") &&
    identical(resource$sha256, report$html_sha256) &&
    identical(
      unlist(resource$roles, use.names = FALSE),
      c("report", "html", "fixed-quarto-report-v1")
    )
  valid_base <- is.character(stage) && length(stage) == 1L && !is.na(stage) &&
    stage %in% c("catalog", "fetched", "harmonized") &&
    timeseries %in% c("csv", "parquet") &&
    gx_package_parquet_manifest_valid_impl(stage, timeseries, parquet) &&
    is.logical(manifest$recipe$output$keep_raw) &&
    length(manifest$recipe$output$keep_raw) == 1L &&
    !is.na(manifest$recipe$output$keep_raw) &&
    identical(serialization$request_export, "manifest-requests-csv-v1") &&
    identical(serialization$request_ledger_scope, "catalog_only") &&
    is.character(serialization$package_input_id) &&
    length(serialization$package_input_id) == 1L &&
    gx_catalog_is_sha256(serialization$package_input_id) &&
    is.character(serialization$bundle_id) &&
    length(serialization$bundle_id) == 1L &&
    gx_catalog_is_sha256(serialization$bundle_id)
  valid <- identical(verification$status, "verified") &&
    identical(manifest$recipe$pipeline$end_stage, "package") &&
    identical(manifest$recipe$output$report, TRUE) &&
    identical(manifest$replay$replayable, FALSE) &&
    identical(serialization$writer, .gx_package_writer_profile) &&
    identical(
      serialization$resource_profile,
      resource_profile
    ) &&
    valid_base && valid_report && valid_resource &&
    all(verification$resources$present) &&
    all(verification$resources$status == "verified")
  if (!valid) {
    gx_package_report_resources_abort(
      "Package verification is outside the fixed report profile.",
      "gx_error_package_report_resources_profile"
    )
  }
  list(
    manifest = manifest,
    serialization = serialization,
    stage = stage,
    timeseries = timeseries,
    parquet = parquet,
    report = report
  )
}

gx_package_owned_manifest_profile_impl <- function(verification) {
  public <- tryCatch(
    gx_package_manifest_profile_impl(verification),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (!is.null(public)) return(public)
  gx_package_report_manifest_profile_impl(verification)
}

gx_package_bundle_manifest_validate_impl <- function(verification, bundle) {
  gx_package_bundle_validate_impl(bundle)
  if (!identical(class(verification), "gx_snapshot_verification")) {
    verification <- structure(
      unclass(verification),
      class = "gx_snapshot_verification"
    )
  }
  profile <- if (gx_package_bundle_is_frictionless_impl(bundle)) {
    gx_package_frictionless_manifest_profile_impl(verification)
  } else if (gx_package_bundle_is_report_impl(bundle)) {
    gx_package_report_manifest_profile_impl(verification)
  } else {
    gx_package_manifest_profile_impl(verification)
  }
  expected_report <- gx_package_bundle_report_manifest_impl(bundle)
  if (!is.null(expected_report)) {
    expected_report <- gx_snapshot_normalize_json(expected_report)
  }
  observed_report <- profile$serialization$report
  expected_frictionless <- gx_package_bundle_frictionless_manifest_impl(bundle)
  if (!is.null(expected_frictionless)) {
    expected_frictionless <- gx_snapshot_normalize_json(
      expected_frictionless
    )
  }
  observed_frictionless <- profile$serialization$frictionless
  valid <- identical(
    profile$serialization$package_input_id,
    bundle$input$input_id
  ) && identical(profile$serialization$bundle_id, bundle$bundle_id) &&
    identical(
      verification$manifest$recipe$output$report,
      gx_package_bundle_is_report_impl(bundle)
    ) && identical(observed_report, expected_report) &&
    identical(observed_frictionless, expected_frictionless)
  if (!valid) {
    gx_package_report_resources_abort(
      "The package manifest no longer binds its exact report bundle.",
      "gx_error_package_report_resources_profile"
    )
  }
  invisible(profile)
}
