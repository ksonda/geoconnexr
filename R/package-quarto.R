.gx_package_quarto_contract_version <- "0.1.0"
.gx_package_quarto_minimum <- "1.5.1"
.gx_package_quarto_required_exports <- c(
  "quarto_render", "quarto_path", "quarto_version", "quarto_available"
)

.gx_package_quarto_required_formals <- list(
  quarto_render = c(
    "input", "output_format", "output_file", "execute", "metadata",
    "metadata_file", "quiet", "profile", "quarto_args", "pandoc_args",
    "as_job"
  ),
  quarto_path = "normalize",
  quarto_version = character(),
  quarto_available = c("min", "max", "error")
)

gx_package_quarto_abort <- function(
    message,
    class = "gx_error_package_quarto_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_quarto", "gx_error_package"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_quarto_version_impl <- function(package = "quarto") {
  if (!identical(package, "quarto")) {
    gx_package_quarto_abort(
      "The report capability probe accepts only Quarto package metadata.",
      "gx_error_package_quarto_capability"
    )
  }
  gx_package_options_package_version_impl(package)
}

gx_package_quarto_namespace_impl <- function(package = "quarto") {
  loadNamespace(package)
}

gx_package_quarto_namespace_version_impl <- function(namespace) {
  as.character(getNamespaceVersion(namespace))
}

gx_package_quarto_export_impl <- function(package, symbol) {
  getExportedValue(package, symbol)
}

gx_package_quarto_call_safely <- function(code, message, class) {
  tryCatch(
    withCallingHandlers(
      code,
      warning = function(cnd) {
        gx_package_quarto_abort(message, class)
      }
    ),
    gx_error_package_quarto = function(cnd) stop(cnd),
    error = function(cnd) gx_package_quarto_abort(message, class)
  )
}

gx_package_quarto_capability_validate_impl <- function(capability) {
  valid_top <- is.list(capability) && identical(
    names(capability),
    c("version", .gx_package_quarto_required_exports)
  ) && is.character(capability$version) &&
    length(capability$version) == 1L && !is.na(capability$version) &&
    is.null(attributes(capability$version)) &&
    identical(
      gx_fetch_preflight_version_normalize(
        capability$version,
        allow_na = FALSE
      ),
      capability$version
    ) && utils::compareVersion(
      capability$version,
      .gx_package_quarto_minimum
    ) >= 0L
  if (!valid_top) {
    gx_package_quarto_abort(
      "Resolved Quarto capabilities violate their exact version contract.",
      "gx_error_package_quarto_capability"
    )
  }

  for (symbol in .gx_package_quarto_required_exports) {
    value <- capability[[symbol]]
    required <- .gx_package_quarto_required_formals[[symbol]]
    observed <- if (is.function(value)) names(formals(value)) else NULL
    valid <- is.function(value) && all(required %in% observed)
    if (!length(required)) {
      valid <- valid && length(formals(value)) == 0L
    }
    if (!valid) {
      gx_package_quarto_abort(
        "Reviewed Quarto exports no longer match the fixed symbol contract.",
        "gx_error_package_quarto_symbol"
      )
    }
  }
  invisible(capability)
}

# Internal M9w boundary. It verifies Quarto R package metadata before loading
# the namespace, rejects metadata/namespace races, and resolves the reviewed
# report-facing exports and formals without locating or invoking the CLI.
gx_package_quarto_capability_impl <- function(
    version_resolver = gx_package_quarto_version_impl,
    namespace_loader = gx_package_quarto_namespace_impl,
    namespace_version_resolver = gx_package_quarto_namespace_version_impl,
    export_resolver = gx_package_quarto_export_impl) {
  resolvers <- list(
    version_resolver, namespace_loader, namespace_version_resolver,
    export_resolver
  )
  if (!all(vapply(resolvers, is.function, logical(1)))) {
    gx_package_quarto_abort(
      "Quarto capability resolvers must be functions.",
      "gx_error_package_quarto_input"
    )
  }

  observed <- gx_package_quarto_call_safely(
    version_resolver("quarto"),
    "Quarto package metadata could not be inspected safely.",
    "gx_error_package_quarto_capability"
  )
  observed <- tryCatch(
    gx_fetch_preflight_version_normalize(observed),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(observed)) {
    gx_package_quarto_abort(
      "Quarto package metadata violates the version contract.",
      "gx_error_package_quarto_capability"
    )
  }
  if (is.na(observed)) {
    gx_package_quarto_abort(
      "Report rendering requires the optional Quarto R package.",
      "gx_error_package_quarto_missing"
    )
  }
  if (utils::compareVersion(observed, .gx_package_quarto_minimum) < 0L) {
    gx_package_quarto_abort(
      "The installed Quarto R package is older than the reviewed minimum.",
      "gx_error_package_quarto_version"
    )
  }

  namespace <- gx_package_quarto_call_safely(
    namespace_loader("quarto"),
    "The reviewed Quarto namespace could not be loaded.",
    "gx_error_package_quarto_capability"
  )
  loaded <- gx_package_quarto_call_safely(
    namespace_version_resolver(namespace),
    "The loaded Quarto namespace version could not be verified.",
    "gx_error_package_quarto_capability"
  )
  loaded <- tryCatch(
    gx_fetch_preflight_version_normalize(loaded, allow_na = FALSE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(loaded) || !identical(loaded, observed)) {
    gx_package_quarto_abort(
      "Quarto package metadata changed before capability resolution.",
      "gx_error_package_quarto_race"
    )
  }

  exports <- lapply(.gx_package_quarto_required_exports, function(symbol) {
    gx_package_quarto_call_safely(
      export_resolver("quarto", symbol),
      "A required reviewed Quarto export is unavailable.",
      "gx_error_package_quarto_symbol"
    )
  })
  names(exports) <- .gx_package_quarto_required_exports
  capability <- c(list(version = loaded), exports)
  gx_package_quarto_capability_validate_impl(capability)
  capability
}
