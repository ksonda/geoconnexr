.gx_package_options_contract_version <- "0.3.0"
.gx_package_options_packages <- c("arrow", "quarto")
.gx_package_options_minimum_versions <- c(
  arrow = "14.0.0", quarto = "1.5.1"
)

.gx_package_options_fields <- c(
  "contract_version", "mode", "status", "capabilities", "metadata",
  "capability_id"
)

.gx_package_options_columns <- c(
  "contract_version", "feature", "package", "minimum_version",
  "installed_version", "package_status", "option_status"
)

.gx_package_options_metadata_fields <- c(
  "checked_at", "host_specific", "loads_namespaces", "execution_ready",
  "parquet", "report", "counts", "limitations"
)

.gx_package_options_count_fields <- c(
  "features", "packages_probed", "missing_packages", "unpinned_packages",
  "version_too_old_packages", "version_satisfied_packages",
  "ready_features"
)

gx_package_options_abort <- function(
    message,
    class = "gx_error_package_options_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_options", "gx_error_package"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_options_package_version_impl <- function(package) {
  gx_fetch_preflight_package_version_impl(
    package,
    allowed_packages = .gx_package_options_packages
  )
}

gx_package_options_now_impl <- function(now) {
  tryCatch(
    gx_fetch_preflight_now_impl(now),
    error = function(cnd) {
      gx_package_options_abort(
        "The optional-package preflight clock returned an invalid timestamp.",
        "gx_error_package_options_input"
      )
    }
  )
}

gx_package_options_probe_impl <- function(version_resolver) {
  if (!is.function(version_resolver)) {
    gx_package_options_abort(
      "The optional-package version resolver must be a function.",
      "gx_error_package_options_input"
    )
  }
  versions <- rep(NA_character_, length(.gx_package_options_packages))
  names(versions) <- .gx_package_options_packages
  for (package in .gx_package_options_packages) {
    ok <- TRUE
    value <- tryCatch(
      withCallingHandlers(
        version_resolver(package),
        warning = function(cnd) {
          ok <<- FALSE
          invokeRestart("muffleWarning")
        }
      ),
      error = function(cnd) {
        ok <<- FALSE
        NULL
      }
    )
    normalized <- if (ok) tryCatch(
      gx_fetch_preflight_version_normalize(value),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    ) else {
      NULL
    }
    if (is.null(normalized)) {
      gx_package_options_abort(
        "Optional-package metadata could not be inspected safely.",
        "gx_error_package_options_probe"
      )
    }
    versions[[package]] <- normalized
  }
  versions
}

gx_package_options_capabilities_impl <- function(versions) {
  valid <- is.character(versions) &&
    identical(names(versions), .gx_package_options_packages) &&
    is.null(attributes(unname(versions))) &&
    all(vapply(versions, function(version) {
      is.na(version) || identical(
        gx_fetch_preflight_version_normalize(version, allow_na = FALSE),
        version
      )
    }, logical(1)))
  if (!valid) {
    gx_package_options_abort(
      "Optional-package inventory violates its exact version contract."
    )
  }
  missing <- is.na(versions)
  pinned <- !is.na(.gx_package_options_minimum_versions)
  satisfied <- !missing & pinned & vapply(
    seq_along(versions),
    function(index) {
      if (missing[[index]] || !pinned[[index]]) return(FALSE)
      utils::compareVersion(
        versions[[index]], .gx_package_options_minimum_versions[[index]]
      ) >= 0L
    },
    logical(1)
  )
  too_old <- !missing & pinned & !satisfied
  package_status <- rep("present_requirement_unpinned", length(versions))
  package_status[missing] <- "missing"
  package_status[too_old] <- "version_too_old"
  package_status[satisfied] <- "version_satisfied"
  option_status <- rep("blocked_version_unpinned", length(versions))
  option_status[missing] <- "skipped_missing_pkg"
  option_status[too_old] <- "blocked_package_version"
  option_status[satisfied] <- "blocked_symbols_unchecked"
  tibble::tibble(
    contract_version = rep(
      .gx_package_options_contract_version,
      length(.gx_package_options_packages)
    ),
    feature = c("parquet", "report"),
    package = .gx_package_options_packages,
    minimum_version = unname(.gx_package_options_minimum_versions),
    installed_version = unname(versions),
    package_status = unname(package_status),
    option_status = unname(option_status)
  )
}

gx_package_options_metadata_impl <- function(capabilities, checked_at) {
  limitations <- c(
    "local_package_state", "package_symbols_unchecked",
    "preflight_not_runtime_authority", "rendering_not_authorized"
  )
  limitations <- limitations[gx_catalog_byte_order(limitations)]
  list(
    checked_at = checked_at,
    host_specific = TRUE,
    loads_namespaces = FALSE,
    execution_ready = FALSE,
    parquet = FALSE,
    report = FALSE,
    counts = list(
      features = unname(as.integer(nrow(capabilities))),
      packages_probed = unname(as.integer(nrow(capabilities))),
      missing_packages = unname(as.integer(sum(
        capabilities$package_status == "missing"
      ))),
      unpinned_packages = unname(as.integer(sum(
        capabilities$package_status == "present_requirement_unpinned"
      ))),
      version_too_old_packages = unname(as.integer(sum(
        capabilities$package_status == "version_too_old"
      ))),
      version_satisfied_packages = unname(as.integer(sum(
        capabilities$package_status == "version_satisfied"
      ))),
      ready_features = 0L
    ),
    limitations = limitations
  )
}

gx_package_options_id_impl <- function(capabilities) {
  values <- function(x, missing) ifelse(is.na(x), missing, x)
  evidence <- paste(
    capabilities$feature,
    capabilities$package,
    values(capabilities$minimum_version, "<unpinned>"),
    values(capabilities$installed_version, "<missing>"),
    capabilities$package_status,
    capabilities$option_status,
    sep = "="
  )
  gx_contract_hash(
    c(
      list(
        "mode", "optional_package_preflight",
        "status", "inspected_not_authorized"
      ),
      as.list(evidence)
    ),
    namespace = "geoconnexr.package-options.v1",
    contract_version = .gx_package_options_contract_version
  )
}

gx_package_options_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_package_options") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_options_fields) &&
    identical(x$contract_version, .gx_package_options_contract_version) &&
    identical(x$mode, "optional_package_preflight") &&
    identical(x$status, "inspected_not_authorized") &&
    is.character(x$capability_id) && length(x$capability_id) == 1L &&
    !is.na(x$capability_id) && is.null(attributes(x$capability_id)) &&
    gx_catalog_is_sha256(x$capability_id)
  if (!valid_top) {
    gx_package_options_abort(
      "Optional-package evidence violates its exact top-level contract."
    )
  }
  valid_table <- inherits(x$capabilities, "tbl_df") &&
    identical(names(x$capabilities), .gx_package_options_columns) &&
    nrow(x$capabilities) == 2L &&
    identical(x$capabilities$feature, c("parquet", "report")) &&
    identical(x$capabilities$package, .gx_package_options_packages)
  expected <- if (valid_table) tryCatch(
    gx_package_options_capabilities_impl(stats::setNames(
      x$capabilities$installed_version,
      x$capabilities$package
    )),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else {
    NULL
  }
  valid_metadata <- is.list(x$metadata) &&
    identical(names(x$metadata), .gx_package_options_metadata_fields) &&
    is.list(x$metadata$counts) &&
    identical(names(x$metadata$counts), .gx_package_options_count_fields)
  valid_time <- valid_metadata &&
    gx_fetch_preflight_is_utc_scalar(x$metadata$checked_at)
  expected_metadata <- if (!is.null(expected) && valid_time) {
    gx_package_options_metadata_impl(expected, x$metadata$checked_at)
  } else {
    NULL
  }
  valid <- valid_table && !is.null(expected) &&
    identical(x$capabilities, expected) &&
    valid_metadata &&
    !is.null(expected_metadata) &&
    identical(x$metadata, expected_metadata) &&
    identical(
      x$capability_id,
      gx_package_options_id_impl(x$capabilities)
    )
  if (!valid) {
    gx_package_options_abort(
      "Optional-package evidence no longer binds its inspected inventory."
    )
  }
  invisible(x)
}

# Internal M9t boundary. It safely probes Arrow and Quarto package metadata
# without loading their namespaces. Both reviewed minimums are classified, but
# runtime authority remains with the separate M9u/M9w boundaries that load the
# selected namespace and immediately resolve its pinned symbols.
gx_package_options_impl <- function(
    version_resolver = gx_package_options_package_version_impl,
    now = gx_now) {
  checked_at <- gx_package_options_now_impl(now)
  versions <- gx_package_options_probe_impl(version_resolver)
  capabilities <- gx_package_options_capabilities_impl(versions)
  metadata <- gx_package_options_metadata_impl(capabilities, checked_at)
  object <- structure(
    list(
      contract_version = .gx_package_options_contract_version,
      mode = "optional_package_preflight",
      status = "inspected_not_authorized",
      capabilities = capabilities,
      metadata = metadata,
      capability_id = gx_package_options_id_impl(capabilities)
    ),
    class = "gx_package_options"
  )
  gx_package_options_validate_impl(object)
  object
}
