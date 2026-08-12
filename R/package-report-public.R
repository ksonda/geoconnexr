.gx_report_contract_version <- "0.1.0"

.gx_report_fields <- c(
  "contract_version", "mode", "status", "stage", "path", "loaded",
  "report", "html", "output", "metadata", "report_id"
)

.gx_report_metadata_fields <- c(
  "scope", "read_only", "byte_preserving", "exported",
  "execution_enabled", "cache", "isolated", "authenticity", "replayable"
)

gx_report_abort <- function(
    message,
    class = "gx_error_report_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_report", "gx_error_package_report",
      "gx_error_package"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

# Public report creation first materializes the exact report-free bundle in an
# owned private package, because the fixed M9y source is defined over the M9q
# typed package view. The private package is retained through final writer or
# replacement validation so lineage remains rebindable, then removed before
# the public package result is returned.
gx_package_report_bundle_impl <- function(base, temp_parent = tempdir()) {
  gx_package_resources_validate_impl(base)
  parent <- tryCatch(
    normalizePath(temp_parent, winslash = "/", mustWork = TRUE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(parent)) {
    gx_report_abort(
      "The private report-package staging parent is unavailable.",
      "gx_error_report_stage"
    )
  }
  gx_snapshot_assert_fs_type(parent, "directory")
  source_path <- tempfile(
    pattern = ".gx-package-report-source-", tmpdir = parent
  )
  owned <- FALSE
  result <- tryCatch(
    {
      publication <- gx_package_write_impl(base, source_path)
      owned <- TRUE
      hydrated <- gx_package_hydrate_impl(publication$path)
      report <- gx_package_report_impl(hydrated)
      gx_package_report_resources_impl(base, report)
    },
    error = function(cnd) cnd
  )
  if (inherits(result, "condition")) {
    if (owned) {
      cleaned <- gx_package_writer_cleanup_impl(source_path)
      if (!isTRUE(cleaned)) {
        gx_report_abort(
          "The failed private report-package stage could not be removed.",
          "gx_error_report_cleanup",
          recovery_path = normalizePath(
            source_path, winslash = "/", mustWork = FALSE
          )
        )
      }
    }
    stop(result)
  }
  gx_package_report_resources_validate_impl(result)
  list(
    bundle = result,
    source_path = normalizePath(source_path, winslash = "/", mustWork = TRUE)
  )
}

gx_package_report_preparation_cleanup_impl <- function(
    preparation,
    committed = FALSE) {
  valid <- is.list(preparation) && identical(
    names(preparation), c("bundle", "source_path")
  ) && is.character(preparation$source_path) &&
    length(preparation$source_path) == 1L && nzchar(preparation$source_path)
  if (!valid || !isTRUE(gx_package_writer_cleanup_impl(
    preparation$source_path
  ))) {
    gx_report_abort(
      if (committed) {
        paste0(
          "The report package committed, but its private source stage ",
          "could not be removed."
        )
      } else {
        "The private report-package source stage could not be removed."
      },
      "gx_error_report_cleanup",
      package_committed = committed,
      recovery_path = if (valid) normalizePath(
        preparation$source_path, winslash = "/", mustWork = FALSE
      ) else NULL
    )
  }
  invisible(TRUE)
}

gx_report_path_impl <- function(x) {
  if (inherits(x, "gx_package")) {
    gx_package_validate_impl(x)
    return(x$path)
  }
  if (inherits(x, "gx_package_loaded")) {
    gx_package_loaded_validate_impl(x)
    return(x$path)
  }
  valid <- is.character(x) && !is.object(x) && length(x) == 1L &&
    is.null(attributes(x)) && !is.na(x) && nzchar(x)
  if (!valid) {
    gx_report_abort(
      "Report access requires one package, loaded package, or package path.",
      "gx_error_report_input"
    )
  }
  unname(x)
}

gx_report_html_structure_impl <- function(bytes, report, stage) {
  text <- gx_package_report_html_text_impl(bytes)
  document <- tryCatch(
    xml2::read_html(
      text, options = c("RECOVER", "NOERROR", "NOWARNING", "NONET")
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(document)) {
    gx_report_abort(
      "The stored report is not parseable HTML.",
      "gx_error_report_html"
    )
  }
  landmark <- xml2::xml_find_all(document, "//*[@id='geoconnexr-report']")
  forbidden <- xml2::xml_find_all(
    document,
    "//script|//iframe|//frame|//object|//embed|//form|//base|//link|//img|//audio|//video|//source|//style[contains(translate(.,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'@import') or contains(translate(.,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'url(')]|//meta[translate(@http-equiv,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')='refresh']"
  )
  reference_nodes <- xml2::xml_find_all(
    document, "//*[@href or @src or @action or @poster or @data]"
  )
  references <- character()
  for (attribute in c("href", "src", "action", "poster", "data")) {
    values <- xml2::xml_attr(reference_nodes, attribute)
    references <- c(references, values[!is.na(values) & nzchar(values)])
  }
  title <- xml2::xml_find_all(document, "/html/head/title")
  valid <- length(title) == 1L && identical(
    trimws(xml2::xml_text(title[[1L]])),
    "Geoconnex data package report"
  ) && length(landmark) == 1L && length(forbidden) == 0L &&
    length(references) == 0L && identical(
      xml2::xml_attr(landmark, "data-contract-version"),
      report$contract_version
    ) && identical(
      xml2::xml_attr(landmark, "data-hydration-id"),
      report$hydration_id
    ) && identical(
      xml2::xml_attr(landmark, "data-stage"), stage
    )
  if (!valid) {
    gx_report_abort(
      "The stored report violates its fixed isolated HTML profile.",
      "gx_error_report_html"
    )
  }
  invisible(TRUE)
}

gx_report_export_impl <- function(bytes, output) {
  destination <- tryCatch(
    gx_snapshot_writer_scalar_path(output),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(destination)) {
    gx_report_abort(
      "The report output path is not a safe absent-file destination.",
      "gx_error_report_output"
    )
  }
  if (gx_snapshot_writer_entry_exists(destination$target)) {
    gx_report_abort(
      "The report output already exists; replacement is unsupported.",
      "gx_error_report_output_exists"
    )
  }
  stage <- tempfile(pattern = ".gx-report-export-", tmpdir = destination$parent)
  owned <- FALSE
  on.exit({
    if (owned) try(gx_snapshot_writer_cleanup_stage(stage), silent = TRUE)
  }, add = TRUE)
  gx_snapshot_writer_write_raw(stage, bytes)
  owned <- TRUE
  parent_info <- gx_snapshot_assert_fs_type(destination$parent, "directory")
  gx_snapshot_assert_same_info(
    parent_info,
    gx_snapshot_assert_fs_type(destination$parent, "directory")
  )
  if (gx_snapshot_writer_entry_exists(destination$target) ||
      !isTRUE(gx_snapshot_writer_rename(stage, destination$target))) {
    gx_report_abort(
      "The verified report could not be exposed atomically.",
      "gx_error_report_output"
    )
  }
  owned <- FALSE
  actual <- gx_package_report_read_raw_impl(
    destination$target, .gx_package_report_max_output_bytes
  )
  if (!identical(actual, bytes)) {
    gx_report_abort(
      "The report changed during atomic exposure.",
      "gx_error_report_output"
    )
  }
  normalizePath(destination$target, winslash = "/", mustWork = TRUE)
}

gx_report_metadata_impl <- function(output) {
  list(
    scope = "fixed_package_report_access_v1",
    read_only = is.null(output),
    byte_preserving = TRUE,
    exported = !is.null(output),
    execution_enabled = FALSE,
    cache = FALSE,
    isolated = TRUE,
    authenticity = FALSE,
    replayable = FALSE
  )
}

gx_report_id_impl <- function(loaded, report, html, output, metadata) {
  gx_contract_hash(
    list(
      "mode", "fixed_package_report_access",
      "status", "loaded_and_verified",
      "load_id", loaded$load_id,
      "source_report_id", report$report_id,
      "html_sha256", digest::digest(
        html, algo = "sha256", serialize = FALSE
      ),
      "output", output %||% "none",
      "exported", metadata$exported
    ),
    namespace = "geoconnexr.report.v1",
    contract_version = .gx_report_contract_version
  )
}

gx_report_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_report") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_report_fields) &&
    identical(x$contract_version, .gx_report_contract_version) &&
    identical(x$mode, "fixed_package_report_access") &&
    identical(x$status, "loaded_and_verified") &&
    is.character(x$stage) && length(x$stage) == 1L && !is.na(x$stage) &&
    is.character(x$path) && length(x$path) == 1L && nzchar(x$path) &&
    is.raw(x$html) && !is.object(x$html) && is.null(attributes(x$html)) &&
    (is.null(x$output) || (
      is.character(x$output) && length(x$output) == 1L && nzchar(x$output)
    )) && is.character(x$report_id) && length(x$report_id) == 1L &&
    gx_catalog_is_sha256(x$report_id)
  if (!valid_top) {
    gx_report_abort("Report evidence violates its exact top-level contract.")
  }
  loaded_valid <- tryCatch({
    gx_package_loaded_validate_impl(x$loaded)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  profile <- if (loaded_valid) tryCatch(
    gx_package_owned_manifest_profile_impl(x$loaded$verification),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  if (!is.list(profile$report)) profile <- NULL
  position <- if (loaded_valid) match(
    "report_html", x$loaded$resources$role
  ) else NA_integer_
  expected_html <- if (!is.na(position)) {
    x$loaded$contents[[position]]
  } else {
    NULL
  }
  html_valid <- !is.null(profile) && !is.null(expected_html) &&
    identical(x$html, expected_html) && identical(
      digest::digest(x$html, algo = "sha256", serialize = FALSE),
      profile$report$html_sha256
    ) && tryCatch({
      gx_report_html_structure_impl(x$html, profile$report, profile$stage)
      TRUE
    }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  output_valid <- if (is.null(x$output)) {
    TRUE
  } else {
    tryCatch(
      identical(
        gx_package_report_read_raw_impl(
          x$output, .gx_package_report_max_output_bytes
        ),
        x$html
      ),
      error = function(cnd) FALSE,
      warning = function(cnd) FALSE
    )
  }
  metadata <- gx_report_metadata_impl(x$output)
  expected_id <- if (html_valid && output_valid) {
    gx_report_id_impl(x$loaded, profile$report, x$html, x$output, metadata)
  } else {
    NULL
  }
  valid <- loaded_valid && !is.null(profile) &&
    identical(x$stage, profile$stage) && identical(x$path, x$loaded$path) &&
    identical(x$report, profile$report) && html_valid && output_valid &&
    is.list(x$metadata) && identical(
      names(x$metadata), .gx_report_metadata_fields
    ) && identical(x$metadata, metadata) &&
    !is.null(expected_id) && identical(x$report_id, expected_id)
  if (!valid) {
    gx_report_abort(
      "Report evidence no longer binds its package, HTML, and output."
    )
  }
  invisible(x)
}

#' Access a verified geoconnexr package report
#'
#' Loads the fixed HTML report from a report-bearing package as exact bounded
#' bytes, revalidates its isolated structure and manifest binding, and returns
#' read-only evidence. When `output` is supplied, the same bytes are atomically
#' written to one absent file beneath an existing safe parent.
#'
#' Access is offline and never invokes Quarto. The report is redacted,
#' execution-disabled, cache-disabled, unsigned, and non-replayable.
#'
#' @param x A report-bearing `gx_package`, `gx_package_loaded`, or package
#'   directory path.
#' @param output `NULL` for in-memory access, or one absent HTML file path.
#'
#' @return A validated `gx_report` object containing the verified package load,
#'   report manifest descriptor, exact HTML bytes, optional output path, fixed
#'   scope metadata, and report-access identity.
#' @export
gx_report <- function(x, output = NULL) {
  path <- gx_report_path_impl(x)
  loaded <- gx_package_load(path)
  profile <- tryCatch(
    gx_package_owned_manifest_profile_impl(loaded$verification),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(profile) || !is.list(profile$report)) {
    gx_report_abort(
      "The package does not contain one fixed verified report.",
      "gx_error_report_profile"
    )
  }
  position <- match("report_html", loaded$resources$role)
  if (is.na(position)) {
    gx_report_abort(
      "The package report resource is unavailable.",
      "gx_error_report_profile"
    )
  }
  html <- loaded$contents[[position]]
  gx_report_html_structure_impl(html, profile$report, profile$stage)
  output_path <- if (is.null(output)) {
    NULL
  } else {
    gx_report_export_impl(html, output)
  }
  metadata <- gx_report_metadata_impl(output_path)
  object <- structure(
    list(
      contract_version = .gx_report_contract_version,
      mode = "fixed_package_report_access",
      status = "loaded_and_verified",
      stage = profile$stage,
      path = loaded$path,
      loaded = loaded,
      report = profile$report,
      html = html,
      output = output_path,
      metadata = metadata,
      report_id = gx_report_id_impl(
        loaded, profile$report, html, output_path, metadata
      )
    ),
    class = "gx_report"
  )
  gx_report_validate_impl(object)
  object
}

#' @export
print.gx_report <- function(x, ...) {
  gx_report_validate_impl(x)
  cli::cli_inform(c(
    "<gx_report>",
    "* Status: {x$status}",
    "* Source stage: {x$stage}",
    "* HTML bytes: {length(x$html)}; isolated: TRUE",
    if (x$metadata$exported) {
      "* Output: {x$output}"
    } else {
      "* Output: retained in memory"
    },
    "* Scope: redacted; unsigned; non-replayable"
  ))
  invisible(x)
}
