.gx_replay_contract_version <- "0.2.0"

.gx_replay_fields <- c(
  "contract_version", "mode", "status", "kind", "stage", "path",
  "verification", "view", "report", "metadata", "replay_id"
)

.gx_replay_metadata_fields <- c(
  "scope", "refresh", "recipe_executed", "offline", "read_only",
  "verified", "loaded", "report", "authenticity", "frictionless",
  "replayable", "limitations"
)

.gx_replay_limitations <- c(
  "historical_request_truth_unchecked",
  "recipe_execution_not_authorized",
  "refresh_deferred",
  "unsigned_manifest"
)

gx_replay_abort <- function(
    message,
    class = "gx_error_replay_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_replay", "gx_error_snapshot")),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_replay_flag_impl <- function(x) {
  valid <- is.logical(x) && length(x) == 1L && !is.na(x) &&
    is.null(attributes(x))
  if (!valid) {
    gx_replay_abort(
      "Replay option {.arg refresh} must be one explicit logical value.",
      "gx_error_replay_input"
    )
  }
  unname(x)
}

gx_replay_source_path_impl <- function(manifest) {
  if (inherits(manifest, "gx_snapshot")) {
    gx_snapshot_validate_impl(manifest)
    return(manifest$path)
  }
  if (inherits(manifest, "gx_package")) {
    gx_package_validate_impl(manifest)
    return(manifest$path)
  }
  if (inherits(manifest, "gx_package_loaded")) {
    gx_package_loaded_validate_impl(manifest)
    return(manifest$path)
  }
  valid <- is.character(manifest) && !is.object(manifest) &&
    length(manifest) == 1L && is.null(attributes(manifest)) &&
    !is.na(manifest) && nzchar(manifest) &&
    isTRUE(stringi::stri_enc_isutf8(manifest))
  if (!valid) {
    gx_replay_abort(
      paste0(
        "Offline replay inspection requires one snapshot, package, loaded ",
        "package, directory, or exact manifest.json path."
      ),
      "gx_error_replay_input"
    )
  }
  if (!identical(basename(manifest), gx_snapshot_manifest_name)) {
    root <- tryCatch(
      gx_snapshot_root(manifest),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
    if (is.null(root)) {
      gx_replay_abort(
        "The replay source is not one existing snapshot directory.",
        "gx_error_replay_input"
      )
    }
    return(root$path)
  }
  absolute <- tryCatch(
    as.character(fs::path_abs(path.expand(manifest))),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  file_valid <- !is.null(absolute) && length(absolute) == 1L && tryCatch({
    gx_snapshot_assert_fs_type(absolute, "file")
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!file_valid) {
    gx_replay_abort(
      "The replay manifest path is not one existing regular manifest.json file.",
      "gx_error_replay_input"
    )
  }
  normalized <- tryCatch(
    normalizePath(absolute, winslash = "/", mustWork = TRUE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  root <- if (is.null(normalized)) NULL else tryCatch(
    gx_snapshot_root(dirname(normalized)),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(root) || !identical(
      file.path(root$path, gx_snapshot_manifest_name), normalized
  )) {
    gx_replay_abort(
      "The replay manifest path could not be bound to its snapshot root.",
      "gx_error_replay_input"
    )
  }
  root$path
}

gx_replay_profile_impl <- function(verification) {
  package <- tryCatch(
    gx_package_owned_manifest_profile_impl(verification),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  catalog <- tryCatch(
    gx_snapshot_request_profile_impl(verification),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (!is.null(package) && is.null(catalog)) {
    return(list(
      kind = "package",
      stage = package$stage,
      report = isTRUE(verification$manifest$recipe$output$report),
      frictionless = !is.null(package$frictionless)
    ))
  }
  if (!is.null(catalog) && is.null(package)) {
    return(list(
      kind = "catalog_snapshot", stage = "catalog", report = FALSE,
      frictionless = FALSE
    ))
  }
  gx_replay_abort(
    paste0(
      "The verified source is outside the fixed catalog-snapshot and package ",
      "inspection profiles."
    ),
    "gx_error_replay_profile"
  )
}

gx_replay_view_verification_impl <- function(view, kind) {
  if (identical(kind, "catalog_snapshot")) {
    gx_snapshot_catalog_view_validate_impl(view)
    return(view$verification)
  }
  gx_package_hydrated_validate_impl(view)
  view$table_view$loaded$verification
}

gx_replay_verification_equal_impl <- function(x, y) {
  valid <- tryCatch({
    gx_snapshot_verification_validate_impl(x)
    gx_snapshot_verification_validate_impl(y)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  valid && identical(
    x[setdiff(.gx_snapshot_verification_fields, "verified_at")],
    y[setdiff(.gx_snapshot_verification_fields, "verified_at")]
  )
}

gx_replay_metadata_impl <- function(report, frictionless) {
  list(
    scope = "fixed_stored_state_inspection_v1",
    refresh = FALSE,
    recipe_executed = FALSE,
    offline = TRUE,
    read_only = TRUE,
    verified = TRUE,
    loaded = TRUE,
    report = report,
    authenticity = FALSE,
    frictionless = frictionless,
    replayable = FALSE,
    limitations = .gx_replay_limitations
  )
}

gx_replay_id_impl <- function(
    kind,
    stage,
    path,
    verification,
    view,
    report,
    frictionless) {
  view_id <- if (identical(kind, "catalog_snapshot")) {
    view$view_id
  } else {
    view$hydration_id
  }
  gx_contract_hash(
    list(
      "mode", "stored_snapshot_inspection",
      "status", "verified_and_loaded",
      "kind", kind,
      "stage", stage,
      "path", path,
      "manifest_sha256", verification$manifest_sha256,
      "view_id", view_id,
      "report_id", if (is.null(report)) "none" else report$report_id,
      "frictionless", frictionless
    ),
    namespace = "geoconnexr.replay.v1",
    contract_version = .gx_replay_contract_version
  )
}

gx_replay_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_replay") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_replay_fields) &&
    identical(x$contract_version, .gx_replay_contract_version) &&
    identical(x$mode, "stored_snapshot_inspection") &&
    identical(x$status, "verified_and_loaded") &&
    is.character(x$kind) && length(x$kind) == 1L &&
    !is.na(x$kind) && is.null(attributes(x$kind)) &&
    x$kind %in% c("catalog_snapshot", "package") &&
    is.character(x$stage) && length(x$stage) == 1L &&
    !is.na(x$stage) && is.null(attributes(x$stage)) &&
    x$stage %in% c("catalog", "fetched", "harmonized") &&
    is.character(x$path) && length(x$path) == 1L && !is.na(x$path) &&
    is.null(attributes(x$path)) && nzchar(x$path) &&
    is.character(x$replay_id) && length(x$replay_id) == 1L &&
    !is.na(x$replay_id) && is.null(attributes(x$replay_id)) &&
    gx_catalog_is_sha256(x$replay_id)
  if (!valid_top) {
    gx_replay_abort("Replay evidence violates its exact top-level contract.")
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
  profile <- if (verification_valid) tryCatch(
    gx_replay_profile_impl(x$verification),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  view_verification <- if (!is.null(profile) &&
      identical(profile$kind, x$kind)) tryCatch(
    gx_replay_view_verification_impl(x$view, x$kind),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  report_valid <- if (is.null(profile) || !profile$report) {
    is.null(x$report)
  } else {
    tryCatch({
      gx_report_validate_impl(x$report)
      identical(x$report$path, x$path) &&
        identical(x$report$stage, x$stage) &&
        gx_replay_verification_equal_impl(
          x$verification, x$report$loaded$verification
        )
    }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  }
  current <- if (is.null(root)) NULL else tryCatch(
    gx_snapshot_verify(x$path),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  metadata <- if (!is.null(profile)) {
    gx_replay_metadata_impl(profile$report, profile$frictionless)
  } else {
    NULL
  }
  expected_id <- if (!is.null(profile) && !is.null(view_verification) &&
      report_valid) tryCatch(
    gx_replay_id_impl(
      x$kind, x$stage, x$path, x$verification, x$view, x$report,
      profile$frictionless
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  valid <- !is.null(root) && identical(root$path, x$path) &&
    verification_valid && !is.null(profile) &&
    identical(profile$kind, x$kind) && identical(profile$stage, x$stage) &&
    !is.null(view_verification) &&
    gx_replay_verification_equal_impl(x$verification, view_verification) &&
    !is.null(current) &&
    gx_replay_verification_equal_impl(x$verification, current) &&
    report_valid && is.list(x$metadata) &&
    identical(names(x$metadata), .gx_replay_metadata_fields) &&
    identical(x$metadata, metadata) && !is.null(expected_id) &&
    identical(x$replay_id, expected_id)
  if (!valid) {
    gx_replay_abort(
      "Replay evidence no longer binds its verified stored-state inspection."
    )
  }
  invisible(x)
}

#' Inspect a verified stored snapshot or package offline
#'
#' Verifies and loads one existing fixed geoconnexr catalog snapshot or data
#' package without executing its recipe. Catalog snapshots return the exact
#' typed redacted [gx_snapshot_catalog_view()]; packages return the exact typed
#' [gx_package_hydrate()] view and, when present, read-only [gx_report()]
#' evidence. Verification is repeated around loading by the underlying fixed
#' loaders, and the result is rebound to the current closed tree.
#'
#' This checkpoint supports only `refresh = FALSE`. It performs no network,
#' DNS, cache, optional runtime, write, repair, destination creation,
#' external Frictionless execution, authenticity check, or live workflow-object
#' reconstruction. It is stored-state inspection, not procedural replay.
#'
#' @param manifest A `gx_snapshot`, `gx_package`, `gx_package_loaded`, existing
#'   snapshot/package directory, or exact `manifest.json` file path.
#' @param dir Must be `NULL`. Replay publication to a destination is deferred.
#' @param refresh Must be `FALSE`. Recipe execution and refresh are deferred.
#' @param ... Must be empty; future replay options are not yet authorized.
#'
#' @return A validated `gx_replay` object containing the fixed source kind and
#'   stage, final verification evidence, typed read-only view, optional report
#'   evidence, explicit limitations, and deterministic inspection identity.
#' @export
gx_replay <- function(manifest, dir = NULL, refresh = FALSE, ...) {
  refresh <- gx_replay_flag_impl(refresh)
  if (refresh) {
    gx_replay_abort(
      "Recipe refresh is not yet supported; use {.code refresh = FALSE}.",
      "gx_error_replay_refresh"
    )
  }
  if (!is.null(dir)) {
    gx_replay_abort(
      "Offline replay inspection does not accept a destination directory.",
      "gx_error_replay_destination"
    )
  }
  if (length(list(...))) {
    gx_replay_abort(
      "Offline replay inspection does not accept additional options.",
      "gx_error_replay_input"
    )
  }
  path <- gx_replay_source_path_impl(manifest)
  initial <- gx_snapshot_verify(path)
  profile <- gx_replay_profile_impl(initial)
  view <- if (identical(profile$kind, "catalog_snapshot")) {
    gx_snapshot_catalog_view(path)
  } else {
    gx_package_hydrate(path)
  }
  verification <- gx_replay_view_verification_impl(view, profile$kind)
  report <- if (profile$report) gx_report(path) else NULL
  metadata <- gx_replay_metadata_impl(profile$report, profile$frictionless)
  object <- structure(
    list(
      contract_version = .gx_replay_contract_version,
      mode = "stored_snapshot_inspection",
      status = "verified_and_loaded",
      kind = profile$kind,
      stage = profile$stage,
      path = path,
      verification = verification,
      view = view,
      report = report,
      metadata = metadata,
      replay_id = gx_replay_id_impl(
      profile$kind, profile$stage, path, verification, view, report,
      profile$frictionless
      )
    ),
    class = "gx_replay"
  )
  gx_replay_validate_impl(object)
  object
}

#' @export
print.gx_replay <- function(x, ...) {
  gx_replay_validate_impl(x)
  cli::cli_inform(c(
    "<gx_replay>",
    "* Status: {x$status}",
    "* Source: {x$kind}; stage: {x$stage}",
    "* Scope: verified stored-state inspection; offline and read-only",
    "* Recipe execution: FALSE; refresh: deferred",
    "* Assurance: unsigned internal consistency; non-replayable"
  ))
  invisible(x)
}
