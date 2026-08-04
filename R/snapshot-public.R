.gx_snapshot_verification_fields <- c(
  "contract_version", "mode", "manifest", "aoi", "resources", "status",
  "request_count", "request_ledger_status", "manifest_sha256", "verified_at"
)

.gx_snapshot_verification_resource_columns <- c(
  "path", "media_type", "expected_bytes", "expected_sha256", "required",
  "roles", "present", "actual_bytes", "actual_sha256", "status"
)

.gx_snapshot_contract_version <- "0.1.0"

.gx_snapshot_fields <- c(
  "contract_version", "mode", "status", "path", "verification", "metadata",
  "snapshot_id"
)

.gx_snapshot_metadata_fields <- c(
  "scope", "creation_only", "catalog_contract_version", "resources",
  "requests", "overwrite", "fetch", "harmonize", "report", "frictionless",
  "replayable"
)

gx_snapshot_verification_validate_impl <- function(x) {
  valid_top <- is.list(x) &&
    identical(class(x), "gx_snapshot_verification") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_snapshot_verification_fields) &&
    identical(x$contract_version, "1.0.0") &&
    identical(x$mode, "offline_snapshot_verification") &&
    is.character(x$status) && length(x$status) == 1L &&
    !is.na(x$status) &&
    x$status %in% c("verified", "verified_with_optional_absences") &&
    is.character(x$request_ledger_status) &&
    length(x$request_ledger_status) == 1L &&
    !is.na(x$request_ledger_status) &&
    identical(x$request_ledger_status, "shape_validated") &&
    is.integer(x$request_count) && length(x$request_count) == 1L &&
    !is.na(x$request_count) && x$request_count >= 0L &&
    inherits(x$verified_at, "POSIXct") && length(x$verified_at) == 1L &&
    !is.na(x$verified_at) && is.finite(as.numeric(x$verified_at))
  if (!valid_top) {
    gx_snapshot_abort(
      "Snapshot-verification evidence violates its exact top-level contract.",
      "gx_error_snapshot_verification_contract"
    )
  }

  validated <- tryCatch(
    gx_snapshot_validate_manifest(x$manifest),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  manifest_sha256 <- tryCatch(
    gx_snapshot_sha256(x$manifest_sha256),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(validated) || is.null(manifest_sha256) ||
      !identical(x$manifest, validated$manifest) ||
      !identical(x$aoi, validated$aoi) ||
      !identical(
        x$request_count,
        as.integer(length(validated$manifest$requests))
      )) {
    gx_snapshot_abort(
      "Snapshot-verification evidence no longer binds its validated manifest.",
      "gx_error_snapshot_verification_contract"
    )
  }

  resources <- x$resources
  expected <- validated$manifest$resources
  valid_resources <- inherits(resources, "tbl_df") &&
    identical(class(resources), c("tbl_df", "tbl", "data.frame")) &&
    identical(
      names(resources),
      .gx_snapshot_verification_resource_columns
    ) &&
    nrow(resources) == length(expected) &&
    is.character(resources$path) &&
    is.character(resources$media_type) &&
    is.numeric(resources$expected_bytes) &&
    is.character(resources$expected_sha256) &&
    is.logical(resources$required) &&
    is.list(resources$roles) &&
    is.logical(resources$present) &&
    is.numeric(resources$actual_bytes) &&
    is.character(resources$actual_sha256) &&
    is.character(resources$status) &&
    !anyNA(resources[c(
      "path", "media_type", "expected_bytes", "expected_sha256", "required",
      "roles", "present", "status"
    )])
  if (!valid_resources) {
    gx_snapshot_abort(
      "Snapshot-verification resource evidence has an invalid shape.",
      "gx_error_snapshot_verification_contract"
    )
  }

  expected_paths <- vapply(expected, `[[`, character(1), "path")
  expected_media <- vapply(expected, `[[`, character(1), "media_type")
  expected_bytes <- vapply(expected, function(resource) {
    as.numeric(resource$bytes)
  }, numeric(1))
  expected_sha256 <- vapply(expected, `[[`, character(1), "sha256")
  expected_required <- vapply(expected, `[[`, logical(1), "required")
  expected_roles <- unname(lapply(expected, function(resource) {
    unname(unlist(resource$roles, use.names = FALSE))
  }))
  binds_manifest <- identical(resources$path, expected_paths) &&
    identical(resources$media_type, expected_media) &&
    identical(resources$expected_bytes, expected_bytes) &&
    identical(resources$expected_sha256, expected_sha256) &&
    identical(resources$required, expected_required) &&
    identical(resources$roles, expected_roles)

  verified <- resources$status == "verified"
  optional_absence <- resources$status == "missing_optional"
  valid_status <- all(verified | optional_absence) &&
    identical(verified, resources$present) &&
    all(!optional_absence | !resources$required) &&
    all(!optional_absence | is.na(resources$actual_bytes)) &&
    all(!optional_absence | is.na(resources$actual_sha256)) &&
    all(!verified | !is.na(resources$actual_bytes)) &&
    all(!verified | !is.na(resources$actual_sha256)) &&
    all(!verified | resources$actual_bytes == resources$expected_bytes) &&
    all(!verified |
      resources$actual_sha256 == resources$expected_sha256) &&
    identical(
      x$status,
      if (any(optional_absence)) {
        "verified_with_optional_absences"
      } else {
        "verified"
      }
    )
  if (!binds_manifest || !valid_status) {
    gx_snapshot_abort(
      "Snapshot-verification resources no longer bind their manifest evidence.",
      "gx_error_snapshot_verification_contract"
    )
  }
  invisible(x)
}

#' Verify a closed geoconnexr snapshot offline
#'
#' Verifies one existing snapshot directory against its fixed
#' `manifest.json`. The verifier validates the bounded manifest and AOI recipe,
#' inventories the closed resource tree, rejects links and undeclared entries,
#' and checks declared byte counts and SHA-256 values. Resources remain opaque:
#' this function does not parse, load, repair, replay, or refresh them.
#'
#' Verification performs no network, DNS, cache, optional-package, or write
#' work. A successful result proves internal consistency relative to the
#' unsigned manifest at verification time. It does not prove authenticity,
#' historical request truth, licence accuracy, or protection against
#' coordinated replacement of both manifest and resources.
#'
#' @param dir Existing snapshot directory containing `manifest.json`.
#'
#' @return A validated `gx_snapshot_verification` evidence object with the
#'   normalized manifest, rebound AOI, per-resource verification status,
#'   request-ledger shape status, and manifest SHA-256.
#' @export
gx_snapshot_verify <- function(dir) {
  object <- structure(
    gx_snapshot_verify_impl(dir),
    class = "gx_snapshot_verification"
  )
  gx_snapshot_verification_validate_impl(object)
  object
}

#' @export
print.gx_snapshot_verification <- function(x, ...) {
  gx_snapshot_verification_validate_impl(x)
  absent <- sum(x$resources$status == "missing_optional")
  cli::cli_inform(c(
    "<gx_snapshot_verification>",
    "* Status: {x$status}",
    paste0(
      "* Resources: ", nrow(x$resources),
      "; optional absences: ", absent
    ),
    "* Request ledger: shape validated ({x$request_count} entries)",
    "* Assurance: unsigned internal consistency; offline"
  ))
  invisible(x)
}

gx_snapshot_flag_impl <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x) ||
      !is.null(attributes(x))) {
    gx_snapshot_abort(
      "Snapshot option {.arg {name}} must be one explicit logical value.",
      "gx_error_snapshot_input"
    )
  }
  unname(x)
}

gx_snapshot_metadata_impl <- function(verification) {
  list(
    scope = "catalog_only_v1",
    creation_only = TRUE,
    catalog_contract_version = .gx_catalog_contract_version,
    resources = unname(as.integer(nrow(verification$resources))),
    requests = verification$request_count,
    overwrite = FALSE,
    fetch = FALSE,
    harmonize = FALSE,
    report = FALSE,
    frictionless = FALSE,
    replayable = FALSE
  )
}

gx_snapshot_id_impl <- function(path, verification, metadata) {
  gx_contract_hash(
    list(
      "mode", "catalog_only_snapshot",
      "status", "written_and_verified",
      "path", path,
      "manifest_sha256", verification$manifest_sha256,
      "scope", metadata$scope,
      "catalog_contract_version", metadata$catalog_contract_version,
      "resources", metadata$resources,
      "requests", metadata$requests
    ),
    namespace = "geoconnexr.catalog-snapshot.v1",
    contract_version = .gx_snapshot_contract_version
  )
}

gx_snapshot_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_snapshot") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_snapshot_fields) &&
    identical(x$contract_version, .gx_snapshot_contract_version) &&
    identical(x$mode, "catalog_only_snapshot") &&
    identical(x$status, "written_and_verified") &&
    is.character(x$path) && length(x$path) == 1L && !is.na(x$path) &&
    is.null(attributes(x$path)) && nzchar(x$path) &&
    is.character(x$snapshot_id) && length(x$snapshot_id) == 1L &&
    !is.na(x$snapshot_id) && is.null(attributes(x$snapshot_id)) &&
    isTRUE(gx_catalog_is_sha256(x$snapshot_id))
  if (!valid_top) {
    gx_snapshot_abort(
      "Catalog snapshot evidence violates its exact top-level contract.",
      "gx_error_snapshot_public_contract"
    )
  }
  root <- tryCatch(
    gx_snapshot_root(x$path),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  verification_valid <- tryCatch({
    gx_snapshot_verification_validate_impl(x$verification)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  expected_metadata <- if (verification_valid) {
    gx_snapshot_metadata_impl(x$verification)
  } else {
    NULL
  }
  expected_paths <- sort(
    unname(gx_snapshot_writer_paths),
    method = "radix"
  )
  valid_writer_scope <- verification_valid &&
    identical(x$verification$status, "verified") &&
    identical(
      x$verification$manifest$recipe$pipeline$end_stage,
      "catalog"
    ) &&
    identical(x$verification$manifest$replay$replayable, FALSE) &&
    identical(x$verification$resources$path, expected_paths) &&
    all(x$verification$resources$present) &&
    all(x$verification$resources$status == "verified")
  valid <- !is.null(root) && identical(root$path, x$path) &&
    is.list(x$metadata) &&
    identical(names(x$metadata), .gx_snapshot_metadata_fields) &&
    !is.null(expected_metadata) &&
    identical(x$metadata, expected_metadata) &&
    valid_writer_scope &&
    identical(
      x$snapshot_id,
      gx_snapshot_id_impl(x$path, x$verification, x$metadata)
    )
  if (!valid) {
    gx_snapshot_abort(
      "Catalog snapshot evidence no longer binds its creation result.",
      "gx_error_snapshot_public_contract"
    )
  }
  invisible(x)
}

#' Create a verified catalog-only snapshot
#'
#' Writes one validated [gx_catalog()] to a new snapshot directory using the
#' fixed deterministic catalog CSV profile and `manifest.json`. Creation occurs
#' in a sibling staging directory, the closed tree is verified before atomic
#' exposure, and the final directory is verified again. Existing destinations
#' are never replaced, repaired, or removed.
#'
#' This first public snapshot contract is deliberately catalog-only.
#' `fetch`, `report`, and `overwrite` must remain `FALSE`. It creates no
#' Frictionless data package, fetched or harmonized resources, report, loading
#' contract, signature, or replay authority. It performs no network, DNS,
#' discovery, cache, or optional-package work.
#'
#' @param x A validated `gx_catalog` object.
#' @param dir New destination directory beneath an existing safe parent.
#' @param fetch Must be `FALSE`; fetching during snapshot creation is not
#'   supported.
#' @param report Must be `FALSE`; report rendering is not supported.
#' @param overwrite Must be `FALSE`; existing destinations are preserved.
#'
#' @return A validated `gx_snapshot` object containing the normalized absolute
#'   path, final [gx_snapshot_verify()] evidence, and an exact catalog-only
#'   scope declaration.
#' @export
gx_snapshot <- function(
    x,
    dir,
    fetch = FALSE,
    report = FALSE,
    overwrite = FALSE) {
  fetch <- gx_snapshot_flag_impl(fetch, "fetch")
  report <- gx_snapshot_flag_impl(report, "report")
  overwrite <- gx_snapshot_flag_impl(overwrite, "overwrite")
  if (fetch || report || overwrite) {
    gx_snapshot_abort(
      paste0(
        "The public snapshot boundary is creation-only and catalog-only; ",
        "fetch, report, and overwrite must remain FALSE."
      ),
      "gx_error_snapshot_scope"
    )
  }
  valid_catalog <- inherits(x, "gx_catalog") && tryCatch({
    gx_catalog_validate_impl(x)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid_catalog) {
    gx_snapshot_abort(
      "The public snapshot boundary currently accepts only one valid catalog.",
      "gx_error_snapshot_input"
    )
  }

  written <- gx_snapshot_write_catalog_impl(x, dir)
  verification <- structure(
    written$verification,
    class = "gx_snapshot_verification"
  )
  gx_snapshot_verification_validate_impl(verification)
  metadata <- gx_snapshot_metadata_impl(verification)
  object <- structure(
    list(
      contract_version = .gx_snapshot_contract_version,
      mode = "catalog_only_snapshot",
      status = "written_and_verified",
      path = unname(written$path),
      verification = verification,
      metadata = metadata,
      snapshot_id = gx_snapshot_id_impl(
        unname(written$path), verification, metadata
      )
    ),
    class = "gx_snapshot"
  )
  gx_snapshot_validate_impl(object)
  object
}

#' @export
print.gx_snapshot <- function(x, ...) {
  gx_snapshot_validate_impl(x)
  cli::cli_inform(c(
    "<gx_snapshot>",
    "* Status: {x$status}",
    paste0(
      "* Resources: ", x$metadata$resources,
      "; requests: ", x$metadata$requests
    ),
    "* Scope: catalog-only; creation-only; non-replayable",
    "* Integrity: final closed tree verified"
  ))
  invisible(x)
}

#' Load a verified snapshot request ledger offline
#'
#' Loads the authoritative request ledger from an existing catalog-only
#' snapshot after proving that `requests.csv` is its exact canonical
#' quote-all UTF-8 projection. The complete snapshot is verified before and
#' after loading, and the returned typed request table is rebound to the
#' normalized manifest ledger.
#'
#' This function recognizes only snapshots created with the fixed geoconnexr
#' catalog writer and request-export profiles. It does not load catalog sites,
#' datasets, problems, fetched data, or harmonized data. It performs no
#' network, DNS, cache, write, repair, refresh, authenticity check, or replay.
#'
#' @param dir Existing catalog-only snapshot directory.
#'
#' @return A validated `gx_snapshot_request_export` object containing the
#'   typed request table, normalized path, request count, manifest and resource
#'   hashes, and a deterministic export identity.
#' @export
gx_snapshot_requests <- function(dir) {
  object <- gx_snapshot_load_requests_impl(dir)
  gx_snapshot_request_export_validate_impl(object)
  object
}

#' @export
print.gx_snapshot_request_export <- function(x, ...) {
  gx_snapshot_request_export_validate_impl(x)
  cli::cli_inform(c(
    "<gx_snapshot_request_export>",
    "* Status: {x$status}",
    "* Requests: {x$request_count}",
    "* Binding: canonical CSV bytes to typed manifest ledger",
    "* Assurance: unsigned internal consistency; offline and read-only"
  ))
  invisible(x)
}

#' Load a typed redacted catalog view from a snapshot
#'
#' Loads the fixed catalog resources and request ledger from an existing
#' catalog-only snapshot as a verified typed redacted view. The complete
#' snapshot is verified before and after loading, and every typed projection
#' is rebound to the canonical stored character evidence.
#'
#' This function recognizes only snapshots created with the fixed geoconnexr
#' catalog writer profile. It types exact CRS84 point geometry, writer UTC
#' timestamps, `true`/`false` fields, and canonical ordered JSON string
#' arrays. All other strings remain unchanged, including blank cells and
#' redacted values.
#'
#' The result is not a live `gx_catalog`: it does not reconstruct discarded
#' identities or missing values and does not authorize replay. Loading performs
#' no network, DNS, cache, write, repair, refresh, authenticity check,
#' Frictionless interpretation, or replay.
#'
#' @param dir Existing catalog-only snapshot directory.
#'
#' @return A validated `gx_snapshot_catalog_view` object containing typed
#'   sites, datasets, problems, and requests; the canonical character-table
#'   evidence; final verification evidence; and an exact deterministic view
#'   identity.
#' @export
gx_snapshot_catalog_view <- function(dir) {
  object <- gx_snapshot_load_catalog_view_impl(dir)
  gx_snapshot_catalog_view_validate_impl(object)
  object
}

#' @export
print.gx_snapshot_catalog_view <- function(x, ...) {
  gx_snapshot_catalog_view_validate_impl(x)
  cli::cli_inform(c(
    "<gx_snapshot_catalog_view>",
    "* Status: {x$status}",
    paste0(
      "* Sites: ", x$metadata$counts$sites,
      "; datasets: ", x$metadata$counts$datasets
    ),
    paste0(
      "* Problems: ", x$metadata$counts$problems,
      "; requests: ", x$metadata$counts$requests
    ),
    "* Scope: typed redacted snapshot view; not gx_catalog",
    "* Integrity: verified offline; read-only and non-replayable"
  ))
  invisible(x)
}
