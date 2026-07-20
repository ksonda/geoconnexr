.gx_fetch_preflight_contract_version <- "0.1.0"
.gx_fetch_preflight_max_version_bytes <- 128L
.gx_fetch_preflight_max_description_bytes <- 262144L
.gx_fetch_preflight_max_library_paths <- 256L
.gx_fetch_preflight_max_text_bytes <- 128L * 1024L^2

.gx_fetch_preflight_fields <- c(
  "contract_version", "plan", "handlers", "distributions", "metadata"
)

.gx_fetch_preflight_handler_columns <- c(
  "contract_version", "handler_id", "implementation_id",
  "selected_distributions", "implementation_status", "required_package",
  "minimum_version", "installed_version", "package_status",
  "preflight_status"
)

.gx_fetch_preflight_distribution_columns <- c(
  "contract_version", "selection_order", "fetch_order", "distribution_id",
  "handler_id", "selected", "plan_decision", "preflight_status"
)

.gx_fetch_preflight_metadata_fields <- c(
  "checked_at", "host_specific", "replayable", "execution_ready", "counts",
  "non_replayable_reasons"
)

.gx_fetch_preflight_count_fields <- c(
  "handlers", "distributions", "selected", "packages_probed",
  "blocked_implementation_planned", "skipped_missing_pkg",
  "skipped_package_version", "not_selected", "reference_only", "requests"
)

.gx_fetch_preflight_package_statuses <- c(
  "not_checked", "not_required", "missing", "version_too_old",
  "version_satisfied", "present_requirement_unpinned"
)

.gx_fetch_preflight_statuses <- c(
  "not_selected", "reference_only", "skipped_missing_pkg",
  "skipped_package_version", "blocked_implementation_planned"
)

gx_fetch_preflight_abort <- function(
    message,
    class = "gx_error_fetch_preflight_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_fetch_preflight", "gx_error_fetch_plan")),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_fetch_preflight_empty_handlers <- function() {
  tibble::tibble(
    contract_version = character(), handler_id = character(),
    implementation_id = character(), selected_distributions = integer(),
    implementation_status = character(), required_package = character(),
    minimum_version = character(), installed_version = character(),
    package_status = character(), preflight_status = character()
  )
}

gx_fetch_preflight_empty_distributions <- function() {
  tibble::tibble(
    contract_version = character(), selection_order = integer(),
    fetch_order = integer(), distribution_id = character(),
    handler_id = character(), selected = logical(), plan_decision = character(),
    preflight_status = character()
  )
}

gx_fetch_preflight_exact_attributes <- function(x, expected) {
  observed <- names(attributes(x))
  is.character(observed) && !anyNA(observed) &&
    length(observed) == length(expected) &&
    all(expected %in% observed)
}

gx_fetch_preflight_table_attributes <- function(x, rows) {
  expected_rows <- if (rows == 0L) {
    integer()
  } else {
    c(NA_integer_, -as.integer(rows))
  }
  gx_fetch_preflight_exact_attributes(
    x, c("class", "row.names", "names")
  ) && identical(.row_names_info(x, type = 0L), expected_rows) &&
    all(vapply(x, function(column) {
      is.null(attributes(column))
    }, logical(1)))
}

gx_fetch_preflight_is_utc_scalar <- function(x) {
  is.double(x) && identical(class(x), c("POSIXct", "POSIXt")) &&
    gx_fetch_preflight_exact_attributes(x, c("class", "tzone")) &&
    identical(attr(x, "tzone"), "UTC") && length(x) == 1L &&
    !is.na(x) && is.finite(unclass(x))
}

gx_fetch_preflight_version_normalize <- function(x, allow_na = TRUE) {
  if (!is.character(x) || length(x) != 1L ||
      (is.na(x) && !allow_na)) {
    gx_fetch_preflight_abort(
      "A package metadata probe returned an invalid version.",
      "gx_error_fetch_preflight_package"
    )
  }
  if (is.na(x)) return(NA_character_)
  bytes <- suppressWarnings(tryCatch(
    nchar(enc2utf8(x), type = "bytes", allowNA = TRUE),
    error = function(cnd) NA_integer_
  ))
  valid_text <- gx_fetch_plan_text_valid(
    x, allow_na = FALSE, nonempty = TRUE
  )
  if (!valid_text || length(bytes) != 1L || is.na(bytes) ||
      bytes > .gx_fetch_preflight_max_version_bytes) {
    gx_fetch_preflight_abort(
      "A package metadata probe returned an invalid version.",
      "gx_error_fetch_preflight_package"
    )
  }
  parsed <- suppressWarnings(tryCatch(
    package_version(x),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ))
  if (is.null(parsed) || length(parsed) != 1L) {
    gx_fetch_preflight_abort(
      "A package metadata probe returned an invalid version.",
      "gx_error_fetch_preflight_package"
    )
  }
  normalized <- as.character(parsed)
  if (!is.character(normalized) || length(normalized) != 1L ||
      is.na(normalized) || !nzchar(normalized)) {
    gx_fetch_preflight_abort(
      "A package metadata probe returned an invalid version.",
      "gx_error_fetch_preflight_package"
    )
  }
  normalized
}

gx_fetch_preflight_version_compare <- function(x, y) {
  x <- package_version(gx_fetch_preflight_version_normalize(x, allow_na = FALSE))
  y <- package_version(gx_fetch_preflight_version_normalize(y, allow_na = FALSE))
  if (x < y) -1L else if (x > y) 1L else 0L
}

gx_fetch_preflight_path_predicate <- function(path, predicate) {
  if (!is.function(predicate)) return(NA)
  value <- tryCatch(
    predicate(path),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (!is.logical(value) || length(value) != 1L || is.na(value)) NA else value
}

gx_fetch_preflight_description_info <- function(
    path,
    file_info = fs::file_info,
    path_real = fs::path_real,
    symlink_resolver = gx_path_is_symlink) {
  if (!gx_handler_registry_scalar_text(path) || !is.function(file_info) ||
      !is.function(path_real) || !is.function(symlink_resolver)) {
    return(NULL)
  }
  exists <- gx_fetch_preflight_path_predicate(path, file.exists)
  symlink <- gx_fetch_preflight_path_predicate(path, symlink_resolver)
  if (!identical(exists, TRUE) || !identical(symlink, FALSE)) {
    return(NULL)
  }
  info <- tryCatch(
    file_info(path, follow = FALSE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  size <- if (!is.null(info) && nrow(info) == 1L) {
    suppressWarnings(as.double(info$size[[1L]]))
  } else {
    NA_real_
  }
  regular <- !is.null(info) && nrow(info) == 1L &&
    identical(as.character(info$type[[1L]]), "file")
  if (!regular || !gx_handler_registry_whole_number(
    size,
    minimum = 1L,
    maximum = .gx_fetch_preflight_max_description_bytes
  )) {
    return(NULL)
  }
  real_path <- tryCatch(
    as.character(path_real(path)),
    error = function(cnd) NA_character_,
    warning = function(cnd) NA_character_
  )
  if (!gx_handler_registry_scalar_text(real_path)) return(NULL)
  list(
    size = as.integer(size),
    signature = list(
      path = real_path,
      type = as.character(info$type[[1L]]),
      size = as.double(info$size[[1L]]),
      permissions = as.character(info$permissions[[1L]]),
      modification_time = as.numeric(info$modification_time[[1L]]),
      change_time = as.numeric(info$change_time[[1L]]),
      device_id = as.double(info$device_id[[1L]]),
      inode = as.double(info$inode[[1L]]),
      hard_links = as.double(info$hard_links[[1L]])
    )
  )
}

gx_fetch_preflight_description_version_impl <- function(path, package) {
  if (!gx_handler_registry_scalar_text(package)) {
    gx_fetch_preflight_abort(
      "Package preflight received an invalid package identity.",
      "gx_error_fetch_preflight_input"
    )
  }
  before <- gx_fetch_preflight_description_info(path)
  if (is.null(before)) {
    gx_fetch_preflight_abort(
      "Installed package metadata could not be inspected safely.",
      "gx_error_fetch_preflight_package"
    )
  }
  bytes <- tryCatch(
    readBin(path, what = "raw", n = before$size + 1L),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  after <- gx_fetch_preflight_description_info(path)
  if (is.null(bytes) || !is.raw(bytes) || length(bytes) != before$size ||
      is.null(after) || !identical(after$signature, before$signature)) {
    gx_fetch_preflight_abort(
      "Installed package metadata could not be inspected safely.",
      "gx_error_fetch_preflight_package"
    )
  }

  values <- as.integer(bytes)
  has_bom <- length(bytes) >= 3L && identical(
    bytes[seq_len(3L)], as.raw(c(0xef, 0xbb, 0xbf))
  )
  invalid_control <- values < 9L | (values > 10L & values < 13L) |
    (values > 13L & values < 32L) | values == 127L
  bare_cr <- any(values == 13L & c(values[-1L], -1L) != 10L)
  if (has_bom || any(invalid_control) || bare_cr) {
    gx_fetch_preflight_abort(
      "Installed package metadata could not be inspected safely.",
      "gx_error_fetch_preflight_package"
    )
  }

  lines_connection <- base::rawConnection(bytes, open = "r")
  on.exit(close(lines_connection), add = TRUE)
  lines <- tryCatch(
    readLines(
      lines_connection, warn = FALSE, encoding = "bytes"
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  field_counts <- if (is.null(lines)) {
    integer()
  } else {
    c(
      Package = sum(grepl("^Package:", lines, useBytes = TRUE)),
      Version = sum(grepl("^Version:", lines, useBytes = TRUE))
    )
  }
  if (!identical(field_counts, c(Package = 1L, Version = 1L))) {
    gx_fetch_preflight_abort(
      "Installed package metadata could not be inspected safely.",
      "gx_error_fetch_preflight_package"
    )
  }
  connection <- base::rawConnection(bytes, open = "r")
  on.exit(close(connection), add = TRUE)
  metadata <- tryCatch(
    base::read.dcf(connection, fields = c("Package", "Version")),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(metadata) || !is.character(metadata) ||
      !identical(dim(metadata), c(1L, 2L)) ||
      !identical(colnames(metadata), c("Package", "Version")) ||
      anyNA(metadata) || !identical(unname(metadata[[1L]]), package)) {
    gx_fetch_preflight_abort(
      "Installed package metadata could not be inspected safely.",
      "gx_error_fetch_preflight_package"
    )
  }
  gx_fetch_preflight_version_normalize(unname(metadata[[2L]]), allow_na = FALSE)
}

gx_fetch_preflight_package_version_impl <- function(
    package, library_paths = base::.libPaths()) {
  if (!gx_handler_registry_scalar_text(package) ||
      !package %in% .gx_handler_registry_allowed_packages) {
    gx_fetch_preflight_abort(
      "Package preflight received an invalid package requirement.",
      "gx_error_fetch_preflight_input"
    )
  }
  valid_paths <- is.character(library_paths) &&
    length(library_paths) <= .gx_fetch_preflight_max_library_paths &&
    !anyNA(library_paths) && all(vapply(
      library_paths,
      gx_handler_registry_scalar_text,
      logical(1)
    ))
  if (!valid_paths) {
    gx_fetch_preflight_abort(
      "Package preflight received invalid library search paths.",
      "gx_error_fetch_preflight_input"
    )
  }
  for (library in unname(library_paths)) {
    package_dir <- file.path(library, package)
    is_directory <- gx_fetch_preflight_path_predicate(
      package_dir, dir.exists
    )
    exists <- gx_fetch_preflight_path_predicate(package_dir, file.exists)
    if (is.na(is_directory) || is.na(exists)) {
      gx_fetch_preflight_abort(
        "Installed package metadata could not be inspected safely.",
        "gx_error_fetch_preflight_package"
      )
    }
    if (is_directory) {
      description <- file.path(package_dir, "DESCRIPTION")
      symlink <- gx_fetch_preflight_path_predicate(
        description, gx_path_is_symlink
      )
      if (!identical(symlink, FALSE)) {
        gx_fetch_preflight_abort(
          "Installed package metadata could not be inspected safely.",
          "gx_error_fetch_preflight_package"
        )
      }
      description_exists <- gx_fetch_preflight_path_predicate(
        description, file.exists
      )
      if (is.na(description_exists)) {
        gx_fetch_preflight_abort(
          "Installed package metadata could not be inspected safely.",
          "gx_error_fetch_preflight_package"
        )
      }
      if (!description_exists) next
      return(gx_fetch_preflight_description_version_impl(
        description, package
      ))
    }
    if (exists) {
      gx_fetch_preflight_abort(
        "Installed package metadata could not be inspected safely.",
        "gx_error_fetch_preflight_package"
      )
    }
  }
  NA_character_
}

gx_fetch_preflight_probe_packages <- function(packages, version_resolver) {
  if (!is.function(version_resolver)) {
    gx_fetch_preflight_abort(
      "The package-version resolver must be a function.",
      "gx_error_fetch_preflight_input"
    )
  }
  if (!length(packages)) {
    return(stats::setNames(character(), character()))
  }
  values <- rep(NA_character_, length(packages))
  names(values) <- packages
  for (i in seq_along(packages)) {
    ok <- TRUE
    value <- tryCatch(
      withCallingHandlers(
        version_resolver(packages[[i]]),
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
    if (!ok) {
      gx_fetch_preflight_abort(
        "A package metadata probe failed.",
        "gx_error_fetch_preflight_package"
      )
    }
    values[[i]] <- gx_fetch_preflight_version_normalize(value)
  }
  values
}

gx_fetch_preflight_package_status <- function(
    required_package, minimum_version, installed_version, checked) {
  if (is.na(required_package)) return("not_required")
  if (!checked) return("not_checked")
  if (is.na(installed_version)) return("missing")
  if (is.na(minimum_version)) return("present_requirement_unpinned")
  if (gx_fetch_preflight_version_compare(installed_version, minimum_version) < 0L) {
    "version_too_old"
  } else {
    "version_satisfied"
  }
}

gx_fetch_preflight_handler_status <- function(
    implementation_status, selected_distributions, package_status) {
  if (identical(implementation_status, "classifier_only")) {
    return("reference_only")
  }
  if (selected_distributions == 0L) return("not_selected")
  if (identical(package_status, "missing")) return("skipped_missing_pkg")
  if (identical(package_status, "version_too_old")) {
    return("skipped_package_version")
  }
  "blocked_implementation_planned"
}

gx_fetch_preflight_build_handlers <- function(plan, inventory) {
  source <- plan$handlers
  selected_ids <- plan$distributions$handler_id[plan$distributions$selected]
  selected_counts <- as.integer(vapply(source$handler_id, function(id) {
    sum(selected_ids == id)
  }, integer(1)))
  rows <- tibble::tibble(
    contract_version = rep(
      .gx_fetch_preflight_contract_version, nrow(source)
    ),
    handler_id = source$handler_id,
    implementation_id = source$implementation_id,
    selected_distributions = selected_counts,
    implementation_status = source$availability,
    required_package = source$required_package,
    minimum_version = source$minimum_version,
    installed_version = rep(NA_character_, nrow(source)),
    package_status = rep(NA_character_, nrow(source)),
    preflight_status = rep(NA_character_, nrow(source))
  )
  for (i in seq_len(nrow(rows))) {
    package <- rows$required_package[[i]]
    checked <- !is.na(package) && package %in% names(inventory)
    installed <- if (checked) inventory[[package]] else NA_character_
    rows$installed_version[[i]] <- installed
    rows$package_status[[i]] <- gx_fetch_preflight_package_status(
      package, rows$minimum_version[[i]], installed, checked
    )
    rows$preflight_status[[i]] <- gx_fetch_preflight_handler_status(
      rows$implementation_status[[i]],
      rows$selected_distributions[[i]],
      rows$package_status[[i]]
    )
  }
  rows
}

gx_fetch_preflight_build_distributions <- function(plan, handlers) {
  source <- plan$distributions
  if (!nrow(source)) return(gx_fetch_preflight_empty_distributions())
  handler_position <- match(source$handler_id, handlers$handler_id)
  status <- vapply(seq_len(nrow(source)), function(i) {
    if (identical(source$decision[[i]], "reference_only")) {
      "reference_only"
    } else if (!source$selected[[i]]) {
      "not_selected"
    } else {
      handlers$preflight_status[[handler_position[[i]]]]
    }
  }, character(1))
  tibble::tibble(
    contract_version = rep(
      .gx_fetch_preflight_contract_version, nrow(source)
    ),
    selection_order = source$selection_order,
    fetch_order = source$fetch_order,
    distribution_id = source$distribution_id,
    handler_id = source$handler_id,
    selected = source$selected,
    plan_decision = source$decision,
    preflight_status = status
  )
}

gx_fetch_preflight_counts <- function(handlers, distributions, packages_probed) {
  count_status <- function(value) {
    as.integer(sum(distributions$preflight_status == value))
  }
  list(
    handlers = as.integer(nrow(handlers)),
    distributions = as.integer(nrow(distributions)),
    selected = as.integer(sum(distributions$selected)),
    packages_probed = as.integer(packages_probed),
    blocked_implementation_planned = count_status(
      "blocked_implementation_planned"
    ),
    skipped_missing_pkg = count_status("skipped_missing_pkg"),
    skipped_package_version = count_status("skipped_package_version"),
    not_selected = count_status("not_selected"),
    reference_only = count_status("reference_only"),
    requests = 0L
  )
}

gx_fetch_preflight_now_impl <- function(now) {
  if (!is.function(now)) {
    gx_fetch_preflight_abort(
      "The package-preflight clock seam must be a function.",
      "gx_error_fetch_preflight_input"
    )
  }
  ok <- TRUE
  value <- tryCatch(
    withCallingHandlers(
      now(),
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
  numeric_value <- suppressWarnings(tryCatch(
    as.numeric(value),
    error = function(cnd) NA_real_
  ))
  if (!ok || !inherits(value, "POSIXct") || length(value) != 1L ||
      !identical(attr(value, "tzone"), "UTC") ||
      length(numeric_value) != 1L || is.na(numeric_value) ||
      !is.finite(numeric_value)) {
    gx_fetch_preflight_abort(
      "The package-preflight clock returned an invalid timestamp.",
      "gx_error_fetch_preflight_input"
    )
  }
  as.POSIXct(numeric_value, origin = "1970-01-01", tz = "UTC")
}

gx_fetch_preflight_new_impl <- function(
    plan, handlers, distributions, metadata) {
  object <- structure(
    list(
      contract_version = .gx_fetch_preflight_contract_version,
      plan = plan,
      handlers = handlers,
      distributions = distributions,
      metadata = metadata
    ),
    class = "gx_fetch_preflight"
  )
  gx_fetch_preflight_validate_impl(object)
  object
}

gx_fetch_preflight_impl <- function(
    plan,
    version_resolver = gx_fetch_preflight_package_version_impl,
    now = gx_now) {
  tryCatch(
    gx_fetch_plan_validate_impl(plan),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_fetch_preflight")) stop(cnd)
      gx_fetch_preflight_abort(
        "M7b package preflight requires a valid M7a fetch plan.",
        "gx_error_fetch_preflight_input"
      )
    }
  )
  selected_handlers <- unique(
    plan$distributions$handler_id[plan$distributions$selected]
  )
  selected_positions <- match(selected_handlers, plan$handlers$handler_id)
  packages <- plan$handlers$required_package[selected_positions]
  packages <- unique(packages[!is.na(packages)])
  packages <- packages[gx_catalog_byte_order(packages)]
  inventory <- gx_fetch_preflight_probe_packages(packages, version_resolver)
  handlers <- gx_fetch_preflight_build_handlers(plan, inventory)
  distributions <- gx_fetch_preflight_build_distributions(plan, handlers)
  reasons <- unique(c(
    plan$metadata$non_replayable_reasons,
    "local_package_state",
    "package_symbols_unchecked"
  ))
  reasons <- reasons[gx_catalog_byte_order(reasons)]
  metadata <- list(
    checked_at = gx_fetch_preflight_now_impl(now),
    host_specific = TRUE,
    replayable = FALSE,
    execution_ready = FALSE,
    counts = gx_fetch_preflight_counts(
      handlers, distributions, length(packages)
    ),
    non_replayable_reasons = reasons
  )
  gx_fetch_preflight_new_impl(plan, handlers, distributions, metadata)
}

gx_fetch_preflight_validate_handlers <- function(handlers) {
  rows <- gx_catalog_table_rows(handlers)
  if (!inherits(handlers, "tbl_df") ||
      !identical(class(handlers), c("tbl_df", "tbl", "data.frame")) ||
      !identical(names(handlers), .gx_fetch_preflight_handler_columns) ||
      !gx_fetch_preflight_table_attributes(handlers, rows %||% -1L) ||
      is.null(rows) || rows < 1L || rows > .gx_fetch_plan_max_handlers) {
    gx_fetch_preflight_abort(
      "Package-preflight handlers violate their exact table contract."
    )
  }
  character_columns <- setdiff(
    .gx_fetch_preflight_handler_columns, "selected_distributions"
  )
  valid_types <- all(vapply(
    handlers[character_columns], is.character, logical(1)
  )) && is.integer(handlers$selected_distributions)
  required <- c(
    "contract_version", "handler_id", "implementation_id",
    "selected_distributions", "implementation_status", "package_status",
    "preflight_status"
  )
  if (!valid_types || any(vapply(handlers[required], anyNA, logical(1))) ||
      any(handlers$contract_version != .gx_fetch_preflight_contract_version) ||
      any(handlers$selected_distributions < 0L) ||
      any(handlers$selected_distributions > .gx_fetch_plan_max_distributions) ||
      any(!handlers$implementation_status %in% c("planned", "classifier_only")) ||
      any(!handlers$package_status %in% .gx_fetch_preflight_package_statuses) ||
      any(!handlers$preflight_status %in% .gx_fetch_preflight_statuses)) {
    gx_fetch_preflight_abort(
      "Package-preflight handler columns have invalid types or values."
    )
  }
  lapply(character_columns, function(name) {
    gx_fetch_plan_assert_text(
      handlers[[name]],
      allow_na = name %in% c(
        "required_package", "minimum_version", "installed_version"
      ),
      nonempty = TRUE
    )
  })
  for (i in seq_len(rows)) {
    package <- handlers$required_package[[i]]
    minimum <- handlers$minimum_version[[i]]
    installed <- handlers$installed_version[[i]]
    status <- handlers$package_status[[i]]
    if (is.na(package) &&
        (!is.na(minimum) || !is.na(installed) ||
         !identical(status, "not_required"))) {
      gx_fetch_preflight_abort(
        "Package-free handlers contain unexpected package metadata."
      )
    }
    if (!is.na(package) && handlers$selected_distributions[[i]] > 0L &&
        identical(status, "not_checked")) {
      gx_fetch_preflight_abort(
        "Selected package requirements must contain a probe result."
      )
    }
    if (!is.na(installed) &&
        !identical(
          gx_fetch_preflight_version_normalize(installed, allow_na = FALSE),
          installed
        )) {
      gx_fetch_preflight_abort(
        "Installed package versions are noncanonical.",
        "gx_error_fetch_preflight_package"
      )
    }
    if (!is.na(minimum)) {
      gx_fetch_preflight_version_normalize(minimum, allow_na = FALSE)
    }
    checked <- !identical(status, "not_checked") && !is.na(package)
    expected <- gx_fetch_preflight_package_status(
      package, minimum, installed, checked
    )
    if (!identical(status, expected)) {
      gx_fetch_preflight_abort(
        "Package-preflight handler requirements do not reconcile."
      )
    }
  }
  invisible(handlers)
}

gx_fetch_preflight_validate_distributions <- function(distributions) {
  rows <- gx_catalog_table_rows(distributions)
  if (!inherits(distributions, "tbl_df") ||
      !identical(class(distributions), c("tbl_df", "tbl", "data.frame")) ||
      !identical(
        names(distributions), .gx_fetch_preflight_distribution_columns
      ) || !gx_fetch_preflight_table_attributes(
        distributions, rows %||% -1L
      ) || is.null(rows) || rows > .gx_fetch_plan_max_distributions) {
    gx_fetch_preflight_abort(
      "Package-preflight distributions violate their exact table contract."
    )
  }
  integer_columns <- c("selection_order", "fetch_order")
  character_columns <- setdiff(
    .gx_fetch_preflight_distribution_columns,
    c(integer_columns, "selected")
  )
  valid_types <- all(vapply(
    distributions[character_columns], is.character, logical(1)
  )) && all(vapply(
    distributions[integer_columns], is.integer, logical(1)
  )) && is.logical(distributions$selected)
  required <- setdiff(.gx_fetch_preflight_distribution_columns, "fetch_order")
  if (!valid_types || any(vapply(distributions[required], anyNA, logical(1))) ||
      any(distributions$contract_version != .gx_fetch_preflight_contract_version) ||
      any(distributions$selection_order < 1L) ||
      any(!is.na(distributions$fetch_order) & distributions$fetch_order < 1L) ||
      !gx_catalog_is_sha256(distributions$distribution_id) ||
      any(!distributions$plan_decision %in% .gx_fetch_plan_decisions) ||
      any(!distributions$preflight_status %in% .gx_fetch_preflight_statuses)) {
    gx_fetch_preflight_abort(
      "Package-preflight distribution columns have invalid types or values."
    )
  }
  lapply(character_columns, function(name) {
    gx_fetch_plan_assert_text(
      distributions[[name]], allow_na = FALSE, nonempty = TRUE
    )
  })
  invisible(distributions)
}

gx_fetch_preflight_validate_metadata <- function(metadata) {
  if (!is.list(metadata) ||
      !identical(names(metadata), .gx_fetch_preflight_metadata_fields) ||
      !gx_fetch_preflight_exact_attributes(metadata, "names") ||
      !gx_fetch_preflight_is_utc_scalar(metadata$checked_at) ||
      !identical(metadata$host_specific, TRUE) ||
      !identical(metadata$replayable, FALSE) ||
      !identical(metadata$execution_ready, FALSE) ||
      !is.character(metadata$non_replayable_reasons) ||
      anyNA(metadata$non_replayable_reasons) ||
      !length(metadata$non_replayable_reasons) ||
      length(metadata$non_replayable_reasons) > 32L ||
      anyDuplicated(metadata$non_replayable_reasons) ||
      !gx_catalog_byte_sorted(metadata$non_replayable_reasons) ||
      !gx_catalog_is_token(metadata$non_replayable_reasons) ||
      !is.null(attributes(metadata$non_replayable_reasons))) {
    gx_fetch_preflight_abort(
      "Package-preflight metadata violates its exact contract."
    )
  }
  counts <- metadata$counts
  if (!is.list(counts) ||
      !identical(names(counts), .gx_fetch_preflight_count_fields) ||
      !gx_fetch_preflight_exact_attributes(counts, "names") ||
      !all(vapply(counts, function(value) {
        is.integer(value) && length(value) == 1L && !is.na(value) &&
          value >= 0L && value <= .gx_fetch_plan_max_parameters &&
          is.null(attributes(value))
      }, logical(1)))) {
    gx_fetch_preflight_abort(
      "Package-preflight metadata counts violate their exact contract."
    )
  }
  invisible(metadata)
}

gx_fetch_preflight_assert_text_budget <- function(x) {
  owned <- list(
    contract_version = x$contract_version,
    handlers = x$handlers,
    distributions = x$distributions,
    metadata = x$metadata
  )
  total <- gx_fetch_plan_text_total(
    owned, limit = .gx_fetch_preflight_max_text_bytes
  )
  if (!is.finite(total) || total > .gx_fetch_preflight_max_text_bytes) {
    gx_fetch_preflight_abort(
      "Package-preflight text exceeds its aggregate byte budget.",
      "gx_error_fetch_preflight_budget"
    )
  }
  invisible(total)
}

gx_fetch_preflight_validate_cross_contract <- function(x) {
  plan <- x$plan
  handlers <- x$handlers
  distributions <- x$distributions
  source_handlers <- plan$handlers
  source_distributions <- plan$distributions
  if (!identical(handlers$handler_id, source_handlers$handler_id) ||
      !identical(handlers$implementation_id, source_handlers$implementation_id) ||
      !identical(handlers$implementation_status, source_handlers$availability) ||
      !identical(handlers$required_package, source_handlers$required_package) ||
      !identical(handlers$minimum_version, source_handlers$minimum_version)) {
    gx_fetch_preflight_abort(
      "Package-preflight handlers do not match the embedded plan."
    )
  }
  selected_ids <- source_distributions$handler_id[source_distributions$selected]
  selected_counts <- as.integer(vapply(source_handlers$handler_id, function(id) {
    sum(selected_ids == id)
  }, integer(1)))
  if (!identical(handlers$selected_distributions, selected_counts)) {
    gx_fetch_preflight_abort(
      "Package-preflight handler selection counts do not reconcile."
    )
  }
  selected_packages <- unique(
    handlers$required_package[handlers$selected_distributions > 0L]
  )
  selected_packages <- selected_packages[!is.na(selected_packages)]
  selected_packages <- selected_packages[
    gx_catalog_byte_order(selected_packages)
  ]
  for (package in unique(handlers$required_package[!is.na(
    handlers$required_package
  )])) {
    index <- which(handlers$required_package == package)
    checked <- package %in% selected_packages
    versions <- handlers$installed_version[index]
    if (checked) {
      if (any(handlers$package_status[index] == "not_checked")) {
        gx_fetch_preflight_abort(
          "Selected package requirements must contain a probe result."
        )
      }
      first <- versions[[1L]]
      if (any(!vapply(versions, function(value) {
        identical(value, first)
      }, logical(1)))) {
        gx_fetch_preflight_abort(
          "A probed package has inconsistent installed versions."
        )
      }
    } else if (any(!is.na(versions)) ||
               any(handlers$package_status[index] != "not_checked")) {
      gx_fetch_preflight_abort(
        "Unselected package requirements were unexpectedly probed."
      )
    }
  }
  expected_handler_status <- vapply(seq_len(nrow(handlers)), function(i) {
    gx_fetch_preflight_handler_status(
      handlers$implementation_status[[i]],
      handlers$selected_distributions[[i]],
      handlers$package_status[[i]]
    )
  }, character(1))
  if (!identical(handlers$preflight_status, expected_handler_status)) {
    gx_fetch_preflight_abort(
      "Package-preflight handler statuses do not reconcile."
    )
  }
  projected_distributions <- list(
    selection_order = source_distributions$selection_order,
    fetch_order = source_distributions$fetch_order,
    distribution_id = source_distributions$distribution_id,
    handler_id = source_distributions$handler_id,
    selected = source_distributions$selected,
    plan_decision = source_distributions$decision
  )
  for (name in names(projected_distributions)) {
    if (!identical(distributions[[name]], projected_distributions[[name]])) {
      gx_fetch_preflight_abort(
        "Package-preflight distributions do not match the embedded plan."
      )
    }
  }
  handler_position <- match(distributions$handler_id, handlers$handler_id)
  expected_distribution_status <- vapply(
    seq_len(nrow(distributions)),
    function(i) {
      if (identical(distributions$plan_decision[[i]], "reference_only")) {
        "reference_only"
      } else if (!distributions$selected[[i]]) {
        "not_selected"
      } else {
        handlers$preflight_status[[handler_position[[i]]]]
      }
    },
    character(1)
  )
  if (!identical(
    distributions$preflight_status, expected_distribution_status
  )) {
    gx_fetch_preflight_abort(
      "Package-preflight distribution statuses do not reconcile."
    )
  }
  expected_counts <- gx_fetch_preflight_counts(
    handlers, distributions, length(selected_packages)
  )
  if (!identical(x$metadata$counts, expected_counts) ||
      x$metadata$counts$distributions !=
        x$metadata$counts$blocked_implementation_planned +
          x$metadata$counts$skipped_missing_pkg +
          x$metadata$counts$skipped_package_version +
          x$metadata$counts$not_selected +
          x$metadata$counts$reference_only) {
    gx_fetch_preflight_abort(
      "Package-preflight counts do not reconcile."
    )
  }
  expected_reasons <- unique(c(
    plan$metadata$non_replayable_reasons,
    "local_package_state",
    "package_symbols_unchecked"
  ))
  expected_reasons <- expected_reasons[
    gx_catalog_byte_order(expected_reasons)
  ]
  if (!identical(
    x$metadata$non_replayable_reasons, expected_reasons
  )) {
    gx_fetch_preflight_abort(
      "Package-preflight non-replayability reasons are incomplete."
    )
  }
  invisible(x)
}

gx_fetch_preflight_validate_body <- function(x) {
  if (!is.list(x) || !identical(class(x), "gx_fetch_preflight") ||
      !identical(names(x), .gx_fetch_preflight_fields) ||
      !gx_fetch_preflight_exact_attributes(x, c("names", "class")) ||
      !identical(
        x$contract_version, .gx_fetch_preflight_contract_version
      )) {
    gx_fetch_preflight_abort(
      "Package preflights violate their exact top-level contract."
    )
  }
  gx_fetch_plan_validate_impl(x$plan)
  gx_fetch_preflight_validate_handlers(x$handlers)
  gx_fetch_preflight_validate_distributions(x$distributions)
  gx_fetch_preflight_validate_metadata(x$metadata)
  gx_fetch_preflight_assert_text_budget(x)
  gx_fetch_preflight_validate_cross_contract(x)
  invisible(x)
}

gx_fetch_preflight_validate_impl <- function(x) {
  tryCatch(
    gx_fetch_preflight_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_fetch_preflight")) stop(cnd)
      gx_fetch_preflight_abort(
        "Package-preflight validation rejected a malformed object."
      )
    }
  )
}

#' @export
print.gx_fetch_preflight <- function(x, ...) {
  gx_fetch_preflight_validate_impl(x)
  counts <- x$metadata$counts
  cli::cli_inform(c(
    "<gx_fetch_preflight>",
    paste0(
      "* Selected distributions: {counts$selected}; ",
      "packages probed: {counts$packages_probed}"
    ),
    paste0(
      "* Blocked selected distributions: ",
      "{counts$blocked_implementation_planned}"
    ),
    paste0(
      "* Distributions skipped for missing packages: ",
      "{counts$skipped_missing_pkg}; version mismatches: ",
      "{counts$skipped_package_version}"
    ),
    "* Requests: 0; execution ready: FALSE"
  ))
  invisible(x)
}
