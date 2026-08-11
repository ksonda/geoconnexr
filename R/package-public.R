.gx_package_contract_version <- "0.6.0"

.gx_package_fields <- c(
  "contract_version", "mode", "status", "stage", "path", "verification",
  "previous", "metadata", "package_id"
)

.gx_package_metadata_fields <- c(
  "scope", "creation_only", "source_stage", "resources", "stored_bytes",
  "native_raw_resources", "timeseries", "keep_raw", "overwrite", "report",
  "arrow", "quarto", "frictionless", "request_ledger_scope", "replayable"
)

gx_package_public_abort <- function(
    message,
    class = "gx_error_package_public_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package", "gx_error_snapshot"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_flag_impl <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x) ||
      !is.null(attributes(x))) {
    gx_package_public_abort(
      "Package option {.arg {name}} must be one explicit logical value.",
      "gx_error_package_input"
    )
  }
  unname(x)
}

gx_package_timeseries_impl <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !is.null(attributes(x)) || !nzchar(x)) {
    gx_package_public_abort(
      "Package option {.arg timeseries} must be one explicit format.",
      "gx_error_package_input"
    )
  }
  if (!x %in% c("csv", "parquet")) {
    gx_package_public_abort(
      "Package option {.arg timeseries} must be {.val csv} or {.val parquet}.",
      "gx_error_package_scope"
    )
  }
  unname(x)
}

gx_package_parquet_manifest_valid_impl <- function(
    stage,
    timeseries,
    parquet) {
  if (identical(timeseries, "parquet")) {
    identical(stage, "harmonized") && is.list(parquet) &&
      setequal(names(parquet), c(
        "profile", "arrow_package_version", "arrow_minimum_version",
        "parquet_version", "compression", "use_dictionary",
        "write_statistics", "coerce_timestamps"
      )) && length(parquet) == 8L &&
      identical(parquet$profile, "fixed-arrow-parquet-v1") &&
      is.character(parquet$arrow_package_version) &&
      length(parquet$arrow_package_version) == 1L &&
      !is.na(parquet$arrow_package_version) && utils::compareVersion(
        parquet$arrow_package_version, .gx_package_parquet_arrow_minimum
      ) >= 0L && identical(
        parquet$arrow_minimum_version,
        .gx_package_parquet_arrow_minimum
      ) && identical(parquet$parquet_version, "2.4") &&
      identical(parquet$compression, "uncompressed") &&
      identical(parquet$use_dictionary, FALSE) &&
      identical(parquet$write_statistics, FALSE) &&
      identical(parquet$coerce_timestamps, "us")
  } else {
    identical(timeseries, "csv") && is.null(parquet)
  }
}

gx_package_base_manifest_profile_impl <- function(
    verification,
    resource_profile = "fixed-in-memory-resources-v2") {
  gx_snapshot_verification_validate_impl(verification)
  manifest <- verification$manifest
  serialization <- manifest$effective_options$serialization
  stage <- manifest$effective_options$package_stage
  timeseries <- manifest$recipe$output$timeseries
  parquet <- serialization$parquet
  valid_parquet <- gx_package_parquet_manifest_valid_impl(
    stage, timeseries, parquet
  )
  valid <- identical(verification$status, "verified") &&
    identical(manifest$recipe$pipeline$end_stage, "package") &&
    timeseries %in% c("csv", "parquet") && valid_parquet &&
    is.logical(manifest$recipe$output$keep_raw) &&
    length(manifest$recipe$output$keep_raw) == 1L &&
    !is.na(manifest$recipe$output$keep_raw) &&
    identical(manifest$recipe$output$report, FALSE) &&
    identical(manifest$replay$replayable, FALSE) &&
    is.character(stage) && length(stage) == 1L && !is.na(stage) &&
    stage %in% c("catalog", "fetched", "harmonized") &&
    is.list(serialization) &&
    identical(serialization$writer, .gx_package_writer_profile) &&
    identical(
      serialization$resource_profile,
      resource_profile
    ) &&
    identical(serialization$request_export, "manifest-requests-csv-v1") &&
    identical(serialization$request_ledger_scope, "catalog_only") &&
    is.character(serialization$package_input_id) &&
    length(serialization$package_input_id) == 1L &&
    gx_catalog_is_sha256(serialization$package_input_id) &&
    is.character(serialization$bundle_id) &&
    length(serialization$bundle_id) == 1L &&
    gx_catalog_is_sha256(serialization$bundle_id) &&
    all(verification$resources$present) &&
    all(verification$resources$status == "verified")
  if (!valid) {
    gx_package_public_abort(
      "Package verification is outside the fixed public writer profile.",
      "gx_error_package_public_contract"
    )
  }
  list(
    manifest = manifest,
    serialization = serialization,
    stage = unname(stage),
    timeseries = unname(timeseries),
    parquet = parquet
  )
}

gx_package_manifest_profile_impl <- function(verification) {
  gx_snapshot_verification_validate_impl(verification)
  resource_profile <-
    verification$manifest$effective_options$serialization$resource_profile
  if (identical(
      resource_profile,
      .gx_package_frictionless_resource_profile
    )) {
    return(gx_package_frictionless_manifest_profile_impl(verification))
  }
  gx_package_base_manifest_profile_impl(verification)
}

gx_package_metadata_impl <- function(verification, overwrite = FALSE) {
  profile <- gx_package_owned_manifest_profile_impl(verification)
  report <- isTRUE(verification$manifest$recipe$output$report)
  frictionless <- !is.null(profile$frictionless)
  raw_roles <- vapply(
    verification$resources$roles,
    function(roles) identical(roles, c("data", "native", "raw")),
    logical(1)
  )
  list(
    scope = if (overwrite) {
      "fixed_package_replacement_v1"
    } else {
      "fixed_package_creation_v1"
    },
    creation_only = !overwrite,
    source_stage = profile$stage,
    resources = unname(as.integer(nrow(verification$resources))),
    stored_bytes = unname(as.double(sum(
      verification$resources$actual_bytes
    ))),
    native_raw_resources = unname(as.integer(sum(raw_roles))),
    timeseries = profile$timeseries,
    keep_raw = TRUE,
    overwrite = overwrite,
    report = report,
    arrow = identical(profile$timeseries, "parquet"),
    quarto = report,
    frictionless = frictionless,
    request_ledger_scope = "catalog_only",
    replayable = FALSE
  )
}

gx_package_id_impl <- function(path, previous, verification, metadata) {
  profile <- gx_package_owned_manifest_profile_impl(verification)
  overwrite <- !is.null(previous)
  prior_manifest_sha256 <- if (overwrite) {
    previous$manifest_sha256
  } else {
    "none"
  }
  gx_contract_hash(
    list(
      "mode", if (overwrite) {
        "fixed_package_replacement"
      } else {
        "fixed_package_creation"
      },
      "status", if (overwrite) {
        "replaced_and_verified"
      } else {
        "written_and_verified"
      },
      "path", path,
      "source_stage", metadata$source_stage,
      "timeseries", metadata$timeseries,
      "overwrite", overwrite,
      "report", metadata$report,
      "frictionless", metadata$frictionless,
      "prior_manifest_sha256", prior_manifest_sha256,
      "manifest_sha256", verification$manifest_sha256,
      "package_input_id", profile$serialization$package_input_id,
      "bundle_id", profile$serialization$bundle_id,
      "resources", metadata$resources,
      "stored_bytes", metadata$stored_bytes
    ),
    namespace = "geoconnexr.package.v2",
    contract_version = .gx_package_contract_version
  )
}

gx_package_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_package") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_fields) &&
    identical(x$contract_version, .gx_package_contract_version) &&
    is.character(x$mode) && length(x$mode) == 1L &&
    is.null(attributes(x$mode)) &&
    (identical(x$mode, "fixed_package_creation") ||
      identical(x$mode, "fixed_package_replacement")) &&
    is.character(x$status) && length(x$status) == 1L &&
    is.null(attributes(x$status)) &&
    (identical(x$status, "written_and_verified") ||
      identical(x$status, "replaced_and_verified")) &&
    is.character(x$stage) && length(x$stage) == 1L &&
    !is.na(x$stage) && is.null(attributes(x$stage)) &&
    is.character(x$path) && length(x$path) == 1L &&
    !is.na(x$path) && is.null(attributes(x$path)) && nzchar(x$path) &&
    is.character(x$package_id) && length(x$package_id) == 1L &&
    !is.na(x$package_id) && is.null(attributes(x$package_id)) &&
    gx_catalog_is_sha256(x$package_id)
  if (!valid_top) {
    gx_package_public_abort(
      "Package evidence violates its exact top-level contract."
    )
  }
  root <- tryCatch(
    gx_snapshot_root(x$path),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  profile <- tryCatch(
    gx_package_owned_manifest_profile_impl(x$verification),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  overwrite <- identical(x$mode, "fixed_package_replacement") &&
    identical(x$status, "replaced_and_verified") &&
    !is.null(x$previous)
  creation <- identical(x$mode, "fixed_package_creation") &&
    identical(x$status, "written_and_verified") &&
    is.null(x$previous)
  previous_profile <- if (overwrite) {
    tryCatch(
      gx_package_owned_manifest_profile_impl(x$previous),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
  } else {
    NULL
  }
  expected_metadata <- if (!is.null(profile) &&
      (creation || !is.null(previous_profile))) {
    gx_package_metadata_impl(x$verification, overwrite = overwrite)
  } else {
    NULL
  }
  valid <- (creation || (overwrite && !is.null(previous_profile))) &&
    !is.null(root) && identical(root$path, x$path) &&
    !is.null(profile) && identical(x$stage, profile$stage) &&
    is.list(x$metadata) &&
    identical(names(x$metadata), .gx_package_metadata_fields) &&
    !is.null(expected_metadata) &&
    identical(x$metadata, expected_metadata) &&
    identical(
      x$package_id,
      gx_package_id_impl(
        x$path, x$previous, x$verification, x$metadata
      )
    )
  if (!valid) {
    gx_package_public_abort(
      "Package evidence no longer binds its verified publication result."
    )
  }
  invisible(x)
}

#' Create or replace a verified geoconnexr data package
#'
#' Serializes one validated catalog, fetched result, or harmonized result to a
#' directory using the fixed deterministic CSV/raw or Arrow/Parquet profile. The
#' complete resource bundle is determined in memory and verified through a
#' private sibling staging directory before final destination verification.
#'
#' Fetched and harmonized inputs require their explicit source `catalog`
#' because those contracts do not embed the complete catalog. The catalog is
#' rebound to the input's exact AOI and dataset identity before serialization.
#'
#' This public package contract remains non-replayable. `timeseries` may be
#' `"csv"` or `"parquet"`; Parquet requires a harmonized input and Arrow
#' 14.0.0 or newer. `keep_raw` must be `TRUE`. `report = TRUE` renders one
#' fixed, execution-disabled, cache-disabled, embedded-resource HTML report
#' through the reviewed Quarto R and CLI capability and binds its exact bytes
#' into the package. With `overwrite = FALSE`, the
#' destination must be absent. With `overwrite = TRUE`, the destination must
#' be an intact package produced by this fixed writer profile. The replacement
#' is fully staged and verified before the prior package moves to a sibling
#' backup; detected installation or final-verification failures synchronously
#' restore and re-verify the prior package. Report HTML can be read through
#' [gx_report()]. `frictionless = TRUE` adds a manifest-bound
#' `datapackage.json`. Canonical CSV resources carry exact string schemas;
#' retained raw bytes, Parquet, and report HTML remain opaque file resources.
#' Refresh, replay, and authenticity claims remain unsupported.
#'
#' @param x A validated `gx_catalog`, `gx_fetched`, or `gx_harmonized` object.
#' @param dir Destination directory beneath an existing safe parent.
#' @param catalog `NULL` for a catalog input. For fetched or harmonized input,
#'   the exact validated source `gx_catalog`.
#' @param timeseries `"csv"` (default) or `"parquet"`. Parquet is available
#'   only for harmonized inputs and requires Arrow 14.0.0 or newer.
#' @param keep_raw Must be `TRUE`; exact retained provider bodies are preserved.
#' @param overwrite One logical value. `FALSE` requires an absent destination;
#'   `TRUE` replaces only an intact package from the fixed writer profile.
#' @param report One logical value. When `TRUE`, render and integrate one fixed
#'   Quarto HTML report before final package publication.
#' @param frictionless One logical value. When `TRUE`, add one deterministic
#'   Frictionless Data Package v1 descriptor to the finalized package.
#'
#' @return A validated `gx_package` object containing the normalized absolute
#'   path, source stage, final [gx_snapshot_verify()] evidence, optional prior
#'   package verification for replacement, fixed scope metadata, and
#'   deterministic package identity.
#' @export
gx_package <- function(
    x,
    dir,
    catalog = NULL,
    timeseries = "csv",
    keep_raw = TRUE,
    overwrite = FALSE,
    report = FALSE,
    frictionless = FALSE) {
  timeseries <- gx_package_timeseries_impl(timeseries)
  keep_raw <- gx_package_flag_impl(keep_raw, "keep_raw")
  overwrite <- gx_package_flag_impl(overwrite, "overwrite")
  report <- gx_package_flag_impl(report, "report")
  frictionless <- gx_package_flag_impl(frictionless, "frictionless")
  if (!keep_raw) {
    gx_package_public_abort(
      paste0(
        "The public package boundary preserves retained raw resources; ",
        "keep_raw must be TRUE."
      ),
      "gx_error_package_scope"
    )
  }
  input <- gx_package_input_impl(x, catalog)
  if (identical(timeseries, "parquet") &&
      !identical(input$stage, "harmonized")) {
    gx_package_public_abort(
      "Parquet time-series output requires a harmonized package input.",
      "gx_error_package_scope"
    )
  }
  base <- gx_package_resources_impl(input, timeseries = timeseries)
  preparation <- if (report) gx_package_report_bundle_impl(base) else NULL
  finalized <- if (report) preparation$bundle else base
  bundle <- if (frictionless) {
    gx_package_frictionless_resources_impl(finalized)
  } else {
    finalized
  }
  written <- tryCatch(
    if (overwrite) {
      replacement <- gx_package_replace_impl(bundle, dir)
      gx_package_replacement_validate_impl(replacement)
      replacement
    } else {
      publication <- gx_package_write_impl(bundle, dir)
      gx_package_publication_validate_impl(publication)
      publication
    },
    error = function(cnd) cnd
  )
  if (report) {
    gx_package_report_preparation_cleanup_impl(
      preparation, committed = !inherits(written, "condition")
    )
  }
  if (inherits(written, "condition")) stop(written)
  verification <- structure(
    written$verification,
    class = "gx_snapshot_verification"
  )
  previous <- if (overwrite) {
    structure(written$previous, class = "gx_snapshot_verification")
  } else {
    NULL
  }
  metadata <- gx_package_metadata_impl(verification, overwrite = overwrite)
  object <- structure(
    list(
      contract_version = .gx_package_contract_version,
      mode = if (overwrite) {
        "fixed_package_replacement"
      } else {
        "fixed_package_creation"
      },
      status = if (overwrite) {
        "replaced_and_verified"
      } else {
        "written_and_verified"
      },
      stage = unname(written$stage),
      path = unname(written$path),
      verification = verification,
      previous = previous,
      metadata = metadata,
      package_id = gx_package_id_impl(
        unname(written$path), previous, verification, metadata
      )
    ),
    class = "gx_package"
  )
  gx_package_validate_impl(object)
  object
}

#' @export
print.gx_package <- function(x, ...) {
  gx_package_validate_impl(x)
  cli::cli_inform(c(
    "<gx_package>",
    "* Status: {x$status}",
    "* Source stage: {x$stage}",
    paste0(
      "* Resources: ", x$metadata$resources,
      "; retained raw: ", x$metadata$native_raw_resources
    ),
    paste0(
      "* Scope: fixed CSV/raw",
      if (x$metadata$arrow) "/Parquet" else "",
      "; ",
      if (x$metadata$report) "fixed HTML report; " else "",
      if (x$metadata$frictionless) "Frictionless Data Package v1; " else "",
      if (x$metadata$overwrite) "verified replacement" else "creation-only",
      "; non-replayable"
    ),
    if (x$metadata$overwrite) {
      "* Integrity: prior and final closed trees verified"
    } else {
      "* Integrity: final closed tree verified"
    }
  ))
  invisible(x)
}
