.gx_package_quarto_cli_contract_version <- "0.1.0"
.gx_package_quarto_cli_minimum <- "1.8.27"
.gx_package_quarto_cli_timeout <- 5

.gx_package_quarto_cli_fields <- c(
  "contract_version", "mode", "status", "package_version", "path",
  "cli_version", "metadata", "cli_id"
)

.gx_package_quarto_cli_metadata_fields <- c(
  "scope", "host_specific", "package_minimum_version",
  "cli_minimum_version", "command", "timeout", "path_normalized",
  "regular_file", "executable", "file_size", "modified_at", "invokes_cli",
  "cli_version_ready", "rendering_ready", "public", "replayable",
  "limitations"
)

gx_package_quarto_cli_abort <- function(
    message,
    class = "gx_error_package_quarto_cli_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_quarto_cli", "gx_error_package_quarto",
      "gx_error_package"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_quarto_cli_path_impl <- function(capability) {
  gx_package_quarto_capability_validate_impl(capability)
  capability$quarto_path(normalize = TRUE)
}

gx_package_quarto_cli_version_impl <- function(
    path,
    timeout = .gx_package_quarto_cli_timeout) {
  output <- system2(
    command = path,
    args = "--version",
    stdout = TRUE,
    stderr = TRUE,
    timeout = timeout
  )
  status <- attr(output, "status", exact = TRUE)
  if (!is.null(status) && !identical(unname(as.integer(status)), 0L)) {
    stop("Quarto CLI version command failed.", call. = FALSE)
  }
  output
}

gx_package_quarto_cli_call_safely <- function(code, message, class) {
  tryCatch(
    withCallingHandlers(
      code,
      warning = function(cnd) {
        gx_package_quarto_cli_abort(message, class)
      }
    ),
    gx_error_package_quarto_cli = function(cnd) stop(cnd),
    error = function(cnd) gx_package_quarto_cli_abort(message, class)
  )
}

gx_package_quarto_cli_path_admit_impl <- function(path) {
  valid_text <- is.character(path) && length(path) == 1L &&
    !is.na(path) && is.null(attributes(path)) && nzchar(path)
  normalized <- if (valid_text) tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else {
    NULL
  }
  info <- if (!is.null(normalized)) tryCatch(
    file.info(normalized, extra_cols = FALSE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else {
    NULL
  }
  executable <- if (!is.null(normalized)) tryCatch(
    identical(unname(file.access(normalized, mode = 1L)), 0L),
    error = function(cnd) FALSE,
    warning = function(cnd) FALSE
  ) else {
    FALSE
  }
  valid <- !is.null(normalized) && identical(unname(path), normalized) &&
    !is.null(info) && nrow(info) == 1L && !anyNA(info$size) &&
    !isTRUE(info$isdir[[1L]]) && is.finite(info$size[[1L]]) &&
    info$size[[1L]] > 0 && executable
  if (!valid) {
    gx_package_quarto_cli_abort(
      "The resolved Quarto CLI path is not one normalized executable file.",
      "gx_error_package_quarto_cli_path"
    )
  }
  list(path = normalized, info = info)
}

gx_package_quarto_cli_assert_same_file_impl <- function(before, after) {
  fields <- c("size", "mode", "mtime", "ctime")
  valid <- is.data.frame(before) && is.data.frame(after) &&
    nrow(before) == 1L && nrow(after) == 1L &&
    all(fields %in% names(before)) && all(fields %in% names(after)) &&
    identical(before[fields], after[fields])
  if (!valid) {
    gx_package_quarto_cli_abort(
      "The Quarto CLI executable changed during version admission.",
      "gx_error_package_quarto_cli_race"
    )
  }
  invisible(after)
}

gx_package_quarto_cli_version_normalize_impl <- function(output) {
  valid <- is.character(output) && length(output) == 1L &&
    !is.na(output) && is.null(attributes(output)) &&
    !grepl("[\r\n]", output, perl = TRUE)
  value <- if (valid) tryCatch(
    gx_fetch_preflight_version_normalize(output, allow_na = FALSE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else {
    NULL
  }
  if (is.null(value)) {
    gx_package_quarto_cli_abort(
      "The Quarto CLI returned an invalid version response.",
      "gx_error_package_quarto_cli_version"
    )
  }
  value
}

gx_package_quarto_cli_metadata_impl <- function(admitted) {
  limitations <- c(
    "distribution_authenticity_unchecked", "fixed_report_deferred",
    "rendering_not_authorized"
  )
  limitations <- limitations[gx_catalog_byte_order(limitations)]
  list(
    scope = "fixed_quarto_cli_admission_v1",
    host_specific = TRUE,
    package_minimum_version = .gx_package_quarto_minimum,
    cli_minimum_version = .gx_package_quarto_cli_minimum,
    command = "--version",
    timeout = .gx_package_quarto_cli_timeout,
    path_normalized = TRUE,
    regular_file = TRUE,
    executable = TRUE,
    file_size = unname(as.double(admitted$info$size[[1L]])),
    modified_at = gx_snapshot_writer_time(admitted$info$mtime[[1L]]),
    invokes_cli = TRUE,
    cli_version_ready = TRUE,
    rendering_ready = FALSE,
    public = FALSE,
    replayable = FALSE,
    limitations = limitations
  )
}

gx_package_quarto_cli_id_impl <- function(
    package_version,
    path,
    cli_version,
    metadata) {
  gx_contract_hash(
    list(
      "mode", "fixed_quarto_cli_admission",
      "status", "version_admitted",
      "package_version", package_version,
      "path", path,
      "cli_version", cli_version,
      "file_size", metadata$file_size,
      "modified_at", metadata$modified_at,
      "command", metadata$command,
      "timeout", metadata$timeout
    ),
    namespace = "geoconnexr.package-quarto-cli.v1",
    contract_version = .gx_package_quarto_cli_contract_version
  )
}

gx_package_quarto_cli_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_package_quarto_cli") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_quarto_cli_fields) &&
    identical(x$contract_version, .gx_package_quarto_cli_contract_version) &&
    identical(x$mode, "fixed_quarto_cli_admission") &&
    identical(x$status, "version_admitted") &&
    is.character(x$package_version) && length(x$package_version) == 1L &&
    !is.na(x$package_version) && is.null(attributes(x$package_version)) &&
    is.character(x$path) && length(x$path) == 1L && !is.na(x$path) &&
    is.null(attributes(x$path)) && nzchar(x$path) &&
    is.character(x$cli_version) && length(x$cli_version) == 1L &&
    !is.na(x$cli_version) && is.null(attributes(x$cli_version)) &&
    is.character(x$cli_id) && length(x$cli_id) == 1L && !is.na(x$cli_id) &&
    is.null(attributes(x$cli_id)) && gx_catalog_is_sha256(x$cli_id)
  if (!valid_top) {
    gx_package_quarto_cli_abort(
      "Quarto CLI evidence violates its exact top-level contract."
    )
  }

  package_version <- tryCatch(
    gx_fetch_preflight_version_normalize(
      x$package_version,
      allow_na = FALSE
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  cli_version <- tryCatch(
    gx_fetch_preflight_version_normalize(x$cli_version, allow_na = FALSE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  admitted <- tryCatch(
    gx_package_quarto_cli_path_admit_impl(x$path),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  expected_metadata <- if (!is.null(admitted)) {
    gx_package_quarto_cli_metadata_impl(admitted)
  } else {
    NULL
  }
  valid <- !is.null(package_version) && identical(
    package_version, x$package_version
  ) && utils::compareVersion(
    package_version, .gx_package_quarto_minimum
  ) >= 0L && !is.null(cli_version) && identical(
    cli_version, x$cli_version
  ) && utils::compareVersion(
    cli_version, .gx_package_quarto_cli_minimum
  ) >= 0L && is.list(x$metadata) && identical(
    names(x$metadata), .gx_package_quarto_cli_metadata_fields
  ) && !is.null(expected_metadata) && identical(
    x$metadata, expected_metadata
  ) && identical(
    x$cli_id,
    gx_package_quarto_cli_id_impl(
      x$package_version, x$path, x$cli_version, x$metadata
    )
  )
  if (!valid) {
    gx_package_quarto_cli_abort(
      "Quarto CLI evidence no longer binds its admitted host executable."
    )
  }
  invisible(x)
}

# Internal M9x boundary. It resolves the admitted M9w R package capability,
# invokes only the exact normalized CLI path with a bounded `--version` command,
# detects executable mutation, and admits the reviewed CLI floor without
# rendering or publishing any report.
gx_package_quarto_cli_impl <- function(
    capability_resolver = gx_package_quarto_capability_impl,
    path_resolver = gx_package_quarto_cli_path_impl,
    version_resolver = gx_package_quarto_cli_version_impl) {
  resolvers <- list(capability_resolver, path_resolver, version_resolver)
  if (!all(vapply(resolvers, is.function, logical(1)))) {
    gx_package_quarto_cli_abort(
      "Quarto CLI resolvers must be functions.",
      "gx_error_package_quarto_cli_input"
    )
  }
  capability <- gx_package_quarto_cli_call_safely(
    capability_resolver(),
    "The reviewed Quarto R capability could not be resolved.",
    "gx_error_package_quarto_cli_capability"
  )
  gx_package_quarto_capability_validate_impl(capability)
  path <- gx_package_quarto_cli_call_safely(
    path_resolver(capability),
    "The Quarto CLI path could not be resolved safely.",
    "gx_error_package_quarto_cli_path"
  )
  admitted <- gx_package_quarto_cli_path_admit_impl(path)
  output <- gx_package_quarto_cli_call_safely(
    version_resolver(admitted$path, .gx_package_quarto_cli_timeout),
    "The bounded Quarto CLI version command failed.",
    "gx_error_package_quarto_cli_command"
  )
  after <- gx_package_quarto_cli_path_admit_impl(admitted$path)
  gx_package_quarto_cli_assert_same_file_impl(admitted$info, after$info)
  cli_version <- gx_package_quarto_cli_version_normalize_impl(output)
  if (utils::compareVersion(
    cli_version,
    .gx_package_quarto_cli_minimum
  ) < 0L) {
    gx_package_quarto_cli_abort(
      "The resolved Quarto CLI is older than the reviewed minimum.",
      "gx_error_package_quarto_cli_version"
    )
  }
  metadata <- gx_package_quarto_cli_metadata_impl(after)
  object <- structure(
    list(
      contract_version = .gx_package_quarto_cli_contract_version,
      mode = "fixed_quarto_cli_admission",
      status = "version_admitted",
      package_version = capability$version,
      path = after$path,
      cli_version = cli_version,
      metadata = metadata,
      cli_id = gx_package_quarto_cli_id_impl(
        capability$version, after$path, cli_version, metadata
      )
    ),
    class = "gx_package_quarto_cli"
  )
  gx_package_quarto_cli_validate_impl(object)
  object
}
