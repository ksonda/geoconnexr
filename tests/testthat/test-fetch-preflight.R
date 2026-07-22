test_that("empty fetch preflight has the exact host-specific contract", {
  plan <- fetch_plan_test_build(
    catalog = fetch_plan_test_catalog(populated = FALSE),
    time = NULL
  )
  calls <- new.env(parent = emptyenv())
  resolver <- fetch_preflight_test_resolver(character(), calls)
  preflight <- gx_fetch_preflight_impl(
    plan,
    version_resolver = resolver,
    now = fetch_preflight_test_now
  )

  expect_identical(class(preflight), "gx_fetch_preflight")
  expect_identical(names(preflight), c(
    "contract_version", "plan", "handlers", "distributions", "metadata"
  ))
  expect_identical(preflight$contract_version, "0.1.0")
  expect_identical(preflight$plan, plan)
  expect_identical(preflight$plan$requests, list())
  expect_false(preflight$plan$metadata$execution_ready)
  expect_identical(calls$packages, character())

  expect_identical(class(preflight$handlers), c(
    "tbl_df", "tbl", "data.frame"
  ))
  expect_identical(names(preflight$handlers), c(
    "contract_version", "handler_id", "implementation_id",
    "selected_distributions", "implementation_status", "required_package",
    "minimum_version", "installed_version", "package_status",
    "preflight_status"
  ))
  expect_equal(nrow(preflight$handlers), 9L)
  expect_type(preflight$handlers$selected_distributions, "integer")
  expect_true(all(preflight$handlers$selected_distributions == 0L))
  expect_true(all(is.na(preflight$handlers$installed_version)))
  expect_identical(
    preflight$handlers$implementation_status,
    c(rep("planned", 8L), "classifier_only")
  )
  expect_identical(
    preflight$handlers$package_status,
    c(rep("not_checked", 6L), "not_required", "not_checked", "not_required")
  )
  expect_identical(
    preflight$handlers$preflight_status,
    c(rep("not_selected", 8L), "reference_only")
  )

  expect_identical(class(preflight$distributions), c(
    "tbl_df", "tbl", "data.frame"
  ))
  expect_identical(names(preflight$distributions), c(
    "contract_version", "selection_order", "fetch_order", "distribution_id",
    "handler_id", "selected", "plan_decision", "preflight_status"
  ))
  expect_type(preflight$distributions$selection_order, "integer")
  expect_type(preflight$distributions$fetch_order, "integer")
  expect_type(preflight$distributions$selected, "logical")
  expect_equal(nrow(preflight$distributions), 0L)

  expect_identical(names(preflight$metadata), c(
    "checked_at", "host_specific", "replayable", "execution_ready", "counts",
    "non_replayable_reasons"
  ))
  expect_identical(preflight$metadata$checked_at, fetch_preflight_test_now())
  expect_true(preflight$metadata$host_specific)
  expect_false(preflight$metadata$replayable)
  expect_false(preflight$metadata$execution_ready)
  expect_identical(names(preflight$metadata$counts), c(
    "handlers", "distributions", "selected", "packages_probed",
    "blocked_implementation_planned", "skipped_missing_pkg",
    "skipped_package_version", "not_selected", "reference_only", "requests"
  ))
  expect_identical(preflight$metadata$counts, list(
    handlers = 9L,
    distributions = 0L,
    selected = 0L,
    packages_probed = 0L,
    blocked_implementation_planned = 0L,
    skipped_missing_pkg = 0L,
    skipped_package_version = 0L,
    not_selected = 0L,
    reference_only = 0L,
    requests = 0L
  ))
  expect_identical(preflight$metadata$non_replayable_reasons, c(
    "handler_implementations_planned", "local_package_state",
    "package_symbols_unchecked", "request_plans_absent"
  ))
  expect_identical(
    gx_fetch_preflight_validate_impl(preflight),
    invisible(preflight)
  )
})

test_that("populated preflight reconciles selected and nonselected distributions", {
  plan <- fetch_plan_test_build()
  plan_bytes <- serialize(plan, NULL)
  calls <- new.env(parent = emptyenv())
  preflight <- fetch_preflight_test_build(
    plan,
    versions = c(readr = "2.1.6"),
    calls = calls
  )

  expect_identical(serialize(plan, NULL), plan_bytes)
  expect_identical(serialize(preflight$plan, NULL), plan_bytes)
  expect_identical(calls$packages, "readr")
  expect_identical(preflight$plan$requests, list())
  expect_false(preflight$plan$metadata$execution_ready)

  handlers <- preflight$handlers
  csv <- match("csv", handlers$handler_id)
  unknown <- match("unknown", handlers$handler_id)
  oaf <- match("ogc_api_features", handlers$handler_id)
  expect_identical(handlers$selected_distributions[[csv]], 2L)
  expect_identical(handlers$installed_version[[csv]], "2.1.6")
  expect_identical(
    handlers$package_status[[csv]], "present_requirement_unpinned"
  )
  expect_identical(
    handlers$preflight_status[[csv]], "blocked_implementation_planned"
  )
  expect_identical(handlers$package_status[[oaf]], "not_required")
  expect_identical(handlers$preflight_status[[oaf]], "not_selected")
  expect_identical(handlers$package_status[[unknown]], "not_required")
  expect_identical(handlers$preflight_status[[unknown]], "reference_only")
  expect_true(all(
    handlers$selected_distributions[-csv] == 0L
  ))

  distributions <- preflight$distributions
  expect_identical(
    distributions$selection_order,
    plan$distributions$selection_order
  )
  expect_identical(distributions$fetch_order, plan$distributions$fetch_order)
  expect_identical(
    distributions$distribution_id,
    plan$distributions$distribution_id
  )
  expect_identical(distributions$handler_id, plan$distributions$handler_id)
  expect_identical(distributions$selected, plan$distributions$selected)
  expect_identical(distributions$plan_decision, plan$distributions$decision)
  expect_identical(distributions$preflight_status, c(
    "blocked_implementation_planned", "not_selected",
    "blocked_implementation_planned", "reference_only", "not_selected",
    "not_selected"
  ))
  expect_identical(preflight$metadata$counts, list(
    handlers = 9L,
    distributions = 6L,
    selected = 2L,
    packages_probed = 1L,
    blocked_implementation_planned = 2L,
    skipped_missing_pkg = 0L,
    skipped_package_version = 0L,
    not_selected = 3L,
    reference_only = 1L,
    requests = 0L
  ))
  expect_identical(
    gx_fetch_preflight_validate_impl(preflight),
    invisible(preflight)
  )

  statuses <- c(handlers$package_status, handlers$preflight_status,
                distributions$preflight_status)
  expect_false(any(grepl("ready|available", statuses, perl = TRUE)))
})

test_that("minimum package versions distinguish missing, old, exact, and newer", {
  plan <- fetch_preflight_test_plan("edr")
  cases <- list(
    missing = list(
      version = NA_character_, package = "missing",
      status = "skipped_missing_pkg", missing = 1L, old = 0L, blocked = 0L
    ),
    old = list(
      version = "0.1.0", package = "version_too_old",
      status = "skipped_package_version", missing = 0L, old = 1L,
      blocked = 0L
    ),
    exact = list(
      version = "0.1.1", package = "version_satisfied",
      status = "blocked_implementation_planned", missing = 0L, old = 0L,
      blocked = 1L
    ),
    newer = list(
      version = "0.2.0", package = "version_satisfied",
      status = "blocked_implementation_planned", missing = 0L, old = 0L,
      blocked = 1L
    )
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    calls <- new.env(parent = emptyenv())
    preflight <- fetch_preflight_test_build(
      plan,
      versions = stats::setNames(case$version, "edr4r"),
      calls = calls
    )
    row <- match("edr", preflight$handlers$handler_id)
    expect_identical(calls$packages, "edr4r", info = name)
    expect_identical(
      preflight$handlers$minimum_version[[row]], "0.1.1", info = name
    )
    expect_identical(
      preflight$handlers$installed_version[[row]], case$version, info = name
    )
    expect_identical(
      preflight$handlers$package_status[[row]], case$package, info = name
    )
    expect_identical(
      preflight$handlers$preflight_status[[row]], case$status, info = name
    )
    expect_identical(
      preflight$distributions$preflight_status, case$status, info = name
    )
    expect_identical(
      preflight$metadata$counts$skipped_missing_pkg, case$missing, info = name
    )
    expect_identical(
      preflight$metadata$counts$skipped_package_version, case$old, info = name
    )
    expect_identical(
      preflight$metadata$counts$blocked_implementation_planned,
      case$blocked,
      info = name
    )
    expect_false(preflight$metadata$execution_ready, info = name)
    expect_identical(
      gx_fetch_preflight_validate_impl(preflight),
      invisible(preflight),
      info = name
    )
  }
})

test_that("unpinned, native, and classifier-only handlers stay non-executable", {
  csv_calls <- new.env(parent = emptyenv())
  csv <- fetch_preflight_test_build(
    fetch_preflight_test_plan("csv"),
    versions = c(readr = "2.1.6"),
    calls = csv_calls
  )
  csv_row <- match("csv", csv$handlers$handler_id)
  expect_identical(csv_calls$packages, "readr")
  expect_true(is.na(csv$handlers$minimum_version[[csv_row]]))
  expect_identical(
    csv$handlers$package_status[[csv_row]],
    "present_requirement_unpinned"
  )
  expect_identical(
    csv$distributions$preflight_status,
    "blocked_implementation_planned"
  )

  native_calls <- new.env(parent = emptyenv())
  native <- fetch_preflight_test_build(
    fetch_preflight_test_plan("ogc_api_features"),
    versions = character(),
    calls = native_calls
  )
  native_row <- match("ogc_api_features", native$handlers$handler_id)
  expect_identical(native_calls$packages, character())
  expect_identical(native$handlers$package_status[[native_row]], "not_required")
  expect_identical(
    native$handlers$preflight_status[[native_row]],
    "blocked_implementation_planned"
  )
  expect_identical(
    native$distributions$preflight_status,
    "blocked_implementation_planned"
  )

  fallback_calls <- new.env(parent = emptyenv())
  fallback <- fetch_preflight_test_build(
    fetch_preflight_test_plan("unknown"),
    versions = character(),
    calls = fallback_calls
  )
  fallback_row <- match("unknown", fallback$handlers$handler_id)
  expect_identical(fallback_calls$packages, character())
  expect_identical(
    fallback$handlers$implementation_status[[fallback_row]],
    "classifier_only"
  )
  expect_identical(
    fallback$handlers$preflight_status[[fallback_row]], "reference_only"
  )
  expect_identical(fallback$distributions$preflight_status, "reference_only")
  expect_identical(fallback$metadata$counts$selected, 0L)
  expect_identical(fallback$metadata$counts$reference_only, 1L)

  for (x in list(csv, native, fallback)) {
    expect_false(x$metadata$execution_ready)
    expect_false(x$metadata$replayable)
    expect_identical(x$plan$requests, list())
  }
})

test_that("shared requirements probe once and unique packages use byte order", {
  data_handlers <- c(
    "usgs_waterdata_continuous", "usgs_waterdata_daily", "nwis_legacy_iv",
    "nwis_legacy_dv", "wqp"
  )
  shared_calls <- new.env(parent = emptyenv())
  shared <- fetch_preflight_test_build(
    fetch_preflight_test_plan(data_handlers),
    versions = c(dataRetrieval = "2.7.22"),
    calls = shared_calls
  )
  positions <- match(data_handlers, shared$handlers$handler_id)
  expect_identical(shared_calls$packages, "dataRetrieval")
  expect_true(all(
    shared$handlers$selected_distributions[positions] == 1L
  ))
  expect_identical(
    shared$handlers$package_status[positions],
    c(rep("version_satisfied", 2L),
      rep("present_requirement_unpinned", 3L))
  )
  expect_true(all(
    shared$handlers$preflight_status[positions] ==
      "blocked_implementation_planned"
  ))
  expect_identical(shared$metadata$counts$packages_probed, 1L)
  expect_identical(
    shared$metadata$counts$blocked_implementation_planned,
    5L
  )

  ordered_calls <- new.env(parent = emptyenv())
  ordered <- fetch_preflight_test_build(
    fetch_preflight_test_plan(c(
      "csv", "edr", "usgs_waterdata_daily"
    )),
    versions = c(
      dataRetrieval = "2.7.22", edr4r = "0.1.1", readr = "2.1.6"
    ),
    calls = ordered_calls
  )
  expect_identical(
    ordered_calls$packages,
    c("dataRetrieval", "edr4r", "readr")
  )
  expect_identical(ordered$metadata$counts$packages_probed, 3L)
  expect_identical(
    gx_fetch_preflight_validate_impl(ordered),
    invisible(ordered)
  )
})

test_that("resolver failures and malformed versions fail closed", {
  plan <- fetch_preflight_test_plan("edr")
  secret <- "private-library-path/preflight-secret"
  non_utf8 <- rawToChar(as.raw(255L))
  Encoding(non_utf8) <- "bytes"
  resolvers <- list(
    error = function(...) stop(secret, call. = FALSE),
    warning = function(...) {
      warning(secret, call. = FALSE)
      "0.1.1"
    },
    zero_length = function(...) character(),
    multiple = function(...) c("0.1.1", "0.1.2"),
    numeric = function(...) 0.1,
    empty = function(...) "",
    malformed = function(...) "not-a-version",
    control = function(...) "0.1.1\n2",
    non_utf8 = function(...) non_utf8,
    oversized = function(...) paste0("1.", strrep("0", 65536L))
  )

  for (name in names(resolvers)) {
    condition <- fetch_preflight_test_error(gx_fetch_preflight_impl(
      plan,
      version_resolver = resolvers[[name]],
      now = fetch_preflight_test_now
    ))
    expect_s3_class(condition, "gx_error_fetch_preflight")
    expect_false(grepl(secret, conditionMessage(condition), fixed = TRUE),
      info = name
    )
    trace_text <- paste(capture.output(str(condition$trace)), collapse = "\n")
    expect_false(grepl(secret, trace_text, fixed = TRUE), info = name)
  }

  expect_error(
    gx_fetch_preflight_impl(
      plan, version_resolver = "not-a-function", now = fetch_preflight_test_now
    ),
    class = "gx_error_fetch_preflight"
  )
})

test_that("direct DESCRIPTION reads are bounded, exact, and fail closed", {
  root <- withr::local_tempdir()
  write_description <- function(name, bytes) {
    path <- file.path(root, name)
    writeBin(bytes, path)
    path
  }
  valid <- write_description(
    "valid",
    charToRaw("Package: fixture\r\nVersion: 1.0-1\r\n")
  )
  expect_identical(
    gx_fetch_preflight_description_version_impl(valid, "fixture"),
    "1.0.1"
  )
  no_final_newline <- write_description(
    "no-final-newline",
    charToRaw("Package: fixture\nVersion: 2.0.0")
  )
  expect_identical(
    gx_fetch_preflight_description_version_impl(
      no_final_newline, "fixture"
    ),
    "2.0.0"
  )
  latin1 <- write_description(
    "latin1",
    c(
      charToRaw("Package: fixture\nTitle: caf"),
      as.raw(0xe9),
      charToRaw("\nEncoding: latin1\nVersion: 3.0.0\n")
    )
  )
  expect_identical(
    gx_fetch_preflight_description_version_impl(latin1, "fixture"),
    "3.0.0"
  )

  invalid <- list(
    empty = raw(),
    missing_package = charToRaw("Version: 1.0.0\n"),
    duplicate_package = charToRaw(paste0(
      "Package: fixture\nPackage: second\nVersion: 1.0.0\n"
    )),
    wrong_package = charToRaw("Package: second\nVersion: 1.0.0\n"),
    missing_version = charToRaw("Package: fixture\n"),
    duplicate_version = charToRaw(paste0(
      "Package: fixture\nVersion: 1.0.0\nVersion: 1.0.1\n"
    )),
    multiple_records = charToRaw(paste0(
      "Package: fixture\nVersion: 1.0.0\n\n",
      "Package: second\nVersion: 2.0.0\n"
    )),
    malformed_version = charToRaw(
      "Package: fixture\nVersion: not-a-version\n"
    ),
    folded_version = charToRaw(
      "Package: fixture\nVersion: 1.0.0\n 2\n"
    ),
    bom = c(
      as.raw(c(0xef, 0xbb, 0xbf)),
      charToRaw("Package: fixture\nVersion: 1.0.0\n")
    ),
    nul = c(
      charToRaw("Package: fixture\nVersion: 1.0.0"),
      as.raw(0L),
      charToRaw("\n")
    ),
    non_ascii_version = c(
      charToRaw("Package: fixture\nVersion: 1.0."),
      as.raw(0xe9),
      charToRaw("\n")
    ),
    bare_cr = charToRaw("Package: fixture\rVersion: 1.0.0\r"),
    oversized = rep(
      as.raw(0x41),
      .gx_fetch_preflight_max_description_bytes + 1L
    )
  )
  for (name in names(invalid)) {
    path <- write_description(name, invalid[[name]])
    expect_error(
      gx_fetch_preflight_description_version_impl(path, "fixture"),
      class = "gx_error_fetch_preflight_package",
      info = name
    )
  }
  expect_error(
    gx_fetch_preflight_description_version_impl(root, "fixture"),
    class = "gx_error_fetch_preflight_package"
  )
  expect_error(
    gx_fetch_preflight_description_version_impl(
      file.path(root, "missing"), "fixture"
    ),
    class = "gx_error_fetch_preflight_package"
  )
  expect_error(
    gx_fetch_preflight_description_version_impl(valid, NA_character_),
    class = "gx_error_fetch_preflight_input"
  )
})

test_that("direct DESCRIPTION reads reject a changed file signature", {
  root <- withr::local_tempdir()
  path <- file.path(root, "DESCRIPTION")
  writeBin(
    charToRaw("Package: fixture\nVersion: 1.0.0\n"),
    path
  )
  info_impl <- gx_fetch_preflight_description_info
  calls <- 0L
  expect_error(
    testthat::with_mocked_bindings(
      gx_fetch_preflight_description_version_impl(path, "fixture"),
      gx_fetch_preflight_description_info = function(path) {
        calls <<- calls + 1L
        info <- info_impl(path)
        if (calls == 2L) {
          info$signature$path <- paste0(info$signature$path, "-changed")
        }
        info
      },
      .package = "geoconnexr"
    ),
    class = "gx_error_fetch_preflight_package"
  )
  expect_identical(calls, 2L)
})

test_that("filesystem metadata warnings fail closed without disclosure", {
  root <- withr::local_tempdir()
  path <- file.path(root, "DESCRIPTION")
  writeBin(
    charToRaw("Package: fixture\nVersion: 1.0.0\n"),
    path
  )
  secret <- "private-filesystem-warning"
  warning_seen <- FALSE
  warning_file_info <- function(...) {
    warning(secret, call. = FALSE)
    fs::file_info(...)
  }
  warning_path_real <- function(...) {
    warning(secret, call. = FALSE)
    fs::path_real(...)
  }
  warning_predicate <- function(...) {
    warning(secret, call. = FALSE)
    TRUE
  }
  results <- withCallingHandlers(
    list(
      file_info = gx_fetch_preflight_description_info(
        path, file_info = warning_file_info
      ),
      path_real = gx_fetch_preflight_description_info(
        path, path_real = warning_path_real
      ),
      predicate = gx_fetch_preflight_path_predicate(
        path, warning_predicate
      )
    ),
    warning = function(cnd) {
      warning_seen <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  expect_false(warning_seen)
  expect_null(results$file_info)
  expect_null(results$path_real)
  expect_true(is.na(results$predicate))

  condition <- fetch_preflight_test_error(testthat::with_mocked_bindings(
    gx_fetch_preflight_description_version_impl(path, "fixture"),
    gx_fetch_preflight_description_info = function(...) results$file_info,
    .package = "geoconnexr"
  ))
  expect_s3_class(condition, "gx_error_fetch_preflight_package")
  expect_false(grepl(secret, conditionMessage(condition), fixed = TRUE))
})

test_that("package discovery follows bounded library precedence", {
  root <- withr::local_tempdir()
  first <- file.path(root, "first")
  second <- file.path(root, "second")
  dir.create(first)
  dir.create(second)
  install_fixture <- function(library, version) {
    package_dir <- file.path(library, "readr")
    dir.create(file.path(package_dir, "Meta"), recursive = TRUE)
    writeBin(
      charToRaw(paste0(
        "Package: readr\nVersion: ", version, "\n"
      )),
      file.path(package_dir, "DESCRIPTION")
    )
    writeBin(
      charToRaw("not-an-rds-file"),
      file.path(package_dir, "Meta", "package.rds")
    )
    package_dir
  }
  first_package <- install_fixture(first, "1.0.0")
  install_fixture(second, "2.0.0")

  expect_identical(
    gx_fetch_preflight_package_version_impl("readr", c(first, second)),
    "1.0.0"
  )
  unlink(first_package, recursive = TRUE)
  dir.create(first_package)
  expect_identical(
    gx_fetch_preflight_package_version_impl("readr", c(first, second)),
    "2.0.0"
  )
  expect_true(is.na(gx_fetch_preflight_package_version_impl(
    "edr4r", c(first, second)
  )))

  writeBin(
    charToRaw("Package: readr\nVersion: malformed\n"),
    file.path(first_package, "DESCRIPTION")
  )
  expect_error(
    gx_fetch_preflight_package_version_impl("readr", c(first, second)),
    class = "gx_error_fetch_preflight_package"
  )
  invalid_paths <- list(
    numeric = 1,
    missing = NA_character_,
    oversized = rep(first, .gx_fetch_preflight_max_library_paths + 1L)
  )
  for (name in names(invalid_paths)) {
    expect_error(
      gx_fetch_preflight_package_version_impl(
        "readr", invalid_paths[[name]]
      ),
      class = "gx_error_fetch_preflight_input",
      info = name
    )
  }
})

test_that("direct DESCRIPTION reads reject symlinks", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  target <- file.path(root, "DESCRIPTION-real")
  link <- file.path(root, "DESCRIPTION")
  writeBin(
    charToRaw("Package: fixture\nVersion: 1.0.0\n"),
    target
  )
  linked <- suppressWarnings(file.symlink(target, link))
  if (!isTRUE(linked)) skip("This platform does not permit symlink creation")
  expect_error(
    gx_fetch_preflight_description_version_impl(link, "fixture"),
    class = "gx_error_fetch_preflight_package"
  )

  library <- file.path(root, "library")
  real_package <- file.path(root, "real-readr")
  dir.create(library)
  dir.create(real_package)
  writeBin(
    charToRaw("Package: readr\nVersion: 1.2.3\n"),
    file.path(real_package, "DESCRIPTION")
  )
  package_linked <- suppressWarnings(file.symlink(
    real_package, file.path(library, "readr")
  ))
  if (!isTRUE(package_linked)) {
    skip("This platform does not permit package-directory symlinks")
  }
  expect_identical(
    gx_fetch_preflight_package_version_impl("readr", library),
    "1.2.3"
  )
})

test_that("clock results are exact finite UTC scalars", {
  plan <- fetch_preflight_test_plan("csv")
  resolver <- function(...) "2.1.6"
  invalid <- list(
    not_function = as.POSIXct("2026-07-15", tz = "UTC"),
    multiple = function() as.POSIXct(c(
      "2026-07-15 00:00:00", "2026-07-16 00:00:00"
    ), tz = "UTC"),
    non_utc = function() as.POSIXct(
      "2026-07-15 00:00:00", tz = "America/New_York"
    ),
    missing = function() as.POSIXct(NA, tz = "UTC"),
    character = function() "2026-07-15T00:00:00Z"
  )
  for (name in names(invalid)) {
    expect_error(
      gx_fetch_preflight_impl(
        plan, version_resolver = resolver, now = invalid[[name]]
      ),
      class = "gx_error_fetch_preflight",
      info = name
    )
  }
  secret <- "private-clock-warning"
  warning_seen <- FALSE
  condition <- withCallingHandlers(
    fetch_preflight_test_error(gx_fetch_preflight_impl(
      plan,
      version_resolver = resolver,
      now = function() {
        warning(secret, call. = FALSE)
        fetch_preflight_test_now()
      }
    )),
    warning = function(cnd) {
      warning_seen <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  expect_false(warning_seen)
  expect_s3_class(condition, "gx_error_fetch_preflight_input")
  expect_false(grepl(secret, conditionMessage(condition), fixed = TRUE))
})

test_that("validator rejects forged shapes, states, links, and counts", {
  preflight <- fetch_preflight_test_build()
  selected <- which(preflight$distributions$selected)[[1L]]
  csv <- match("csv", preflight$handlers$handler_id)
  oaf <- match("ogc_api_features", preflight$handlers$handler_id)
  mutations <- list(
    extra_root = function(x) {
      x$unexpected <- TRUE
      x
    },
    wrong_class = function(x) {
      class(x) <- c("gx_fetch_preflight", "list")
      x
    },
    root_attribute = function(x) {
      attr(x, "hidden") <- "unexpected"
      x
    },
    handler_subclass = function(x) {
      class(x$handlers) <- c("forged", class(x$handlers))
      x
    },
    handler_attribute = function(x) {
      attr(x$handlers, "hidden") <- "unexpected"
      x
    },
    handler_column_attribute = function(x) {
      attr(x$handlers$installed_version, "hidden") <- "unexpected"
      x
    },
    distribution_subclass = function(x) {
      class(x$distributions) <- c("forged", class(x$distributions))
      x
    },
    distribution_attribute = function(x) {
      attr(x$distributions, "hidden") <- "unexpected"
      x
    },
    handler_selected_count = function(x) {
      x$handlers$selected_distributions[[csv]] <- 99L
      x
    },
    implementation_status = function(x) {
      x$handlers$implementation_status[[csv]] <- "implemented"
      x
    },
    package_binding = function(x) {
      x$handlers$required_package[[csv]] <- "dataRetrieval"
      x
    },
    installed_missing_mismatch = function(x) {
      x$handlers$installed_version[[csv]] <- NA_character_
      x
    },
    selected_package_not_checked = function(x) {
      x$handlers$installed_version[[csv]] <- NA_character_
      x$handlers$package_status[[csv]] <- "not_checked"
      x
    },
    native_installed_version = function(x) {
      x$handlers$installed_version[[oaf]] <- "9.9.9"
      x
    },
    package_status = function(x) {
      x$handlers$package_status[[csv]] <- "available"
      x
    },
    handler_status = function(x) {
      x$handlers$preflight_status[[csv]] <- "ready"
      x
    },
    distribution_contract = function(x) {
      x$distributions$contract_version[[selected]] <- "9.9.9"
      x
    },
    distribution_identity = function(x) {
      x$distributions$distribution_id[[selected]] <- strrep("0", 64L)
      x
    },
    distribution_handler = function(x) {
      x$distributions$handler_id[[selected]] <- "edr"
      x
    },
    plan_decision = function(x) {
      x$distributions$plan_decision[[selected]] <- "reference_only"
      x
    },
    distribution_status = function(x) {
      x$distributions$preflight_status[[selected]] <- "not_selected"
      x
    },
    checked_timezone = function(x) {
      attr(x$metadata$checked_at, "tzone") <- "America/New_York"
      x
    },
    checked_subclass = function(x) {
      class(x$metadata$checked_at) <- c(
        "forged", class(x$metadata$checked_at)
      )
      x
    },
    metadata_attribute = function(x) {
      attr(x$metadata, "hidden") <- "unexpected"
      x
    },
    counts_attribute = function(x) {
      attr(x$metadata$counts, "hidden") <- "unexpected"
      x
    },
    reasons_attribute = function(x) {
      attr(x$metadata$non_replayable_reasons, "hidden") <- "unexpected"
      x
    },
    host_specific = function(x) {
      x$metadata$host_specific <- FALSE
      x
    },
    replayable = function(x) {
      x$metadata$replayable <- TRUE
      x
    },
    execution_ready = function(x) {
      x$metadata$execution_ready <- TRUE
      x
    },
    selected_count = function(x) {
      x$metadata$counts$selected <- 3L
      x
    },
    request_count = function(x) {
      x$metadata$counts$requests <- 1L
      x
    },
    missing_reason = function(x) {
      x$metadata$non_replayable_reasons <- setdiff(
        x$metadata$non_replayable_reasons,
        "package_symbols_unchecked"
      )
      x
    },
    unsorted_reasons = function(x) {
      x$metadata$non_replayable_reasons <- rev(
        x$metadata$non_replayable_reasons
      )
      x
    },
    embedded_requests = function(x) {
      x$plan$requests <- list(list(method = "GET"))
      x
    },
    embedded_registry_hash = function(x) {
      x$plan$metadata$registry_sha256 <- strrep("0", 64L)
      x
    },
    embedded_plan_class = function(x) {
      class(x$plan) <- c("gx_fetch_plan", "list")
      x
    }
  )

  for (name in names(mutations)) {
    forged <- mutations[[name]](fetch_preflight_test_clone(preflight))
    condition <- fetch_preflight_test_error(
      gx_fetch_preflight_validate_impl(forged)
    )
    expect_s3_class(condition, "gx_error")
    expect_true(
      inherits(condition, c("gx_error_fetch_preflight", "gx_error_fetch_plan")),
      info = name
    )
  }
})

test_that("preflight-owned text has a budget separate from its valid plan", {
  preflight <- fetch_preflight_test_build()
  plan_total <- gx_fetch_plan_text_total(preflight$plan)
  wrapped_total <- gx_fetch_plan_text_total(preflight)
  expect_gt(wrapped_total, plan_total)
  temporary_plan_limit <- as.integer(
    plan_total + max(1, floor((wrapped_total - plan_total) / 2))
  )
  expect_lt(plan_total, temporary_plan_limit)
  expect_gt(wrapped_total, temporary_plan_limit)

  validated <- testthat::with_mocked_bindings(
    gx_fetch_preflight_validate_impl(preflight),
    .gx_fetch_plan_max_text_bytes = temporary_plan_limit,
    .package = "geoconnexr"
  )
  expect_identical(validated, invisible(preflight))
})

test_that("incomplete plan reasons remain visible after host preflight", {
  plan <- fetch_plan_test_build(
    fetch_plan_test_catalog(status = "partial", truncated = TRUE)
  )
  preflight <- fetch_preflight_test_build(plan)

  expect_identical(preflight$metadata$non_replayable_reasons, c(
    "handler_implementations_planned", "local_package_state",
    "package_symbols_unchecked", "request_plans_absent",
    "source_catalog_incomplete"
  ))
  expect_false(preflight$metadata$replayable)
  expect_false(preflight$metadata$execution_ready)
  expect_identical(
    gx_fetch_preflight_validate_impl(preflight),
    invisible(preflight)
  )
})

test_that("default preflight reads metadata without namespace or external work", {
  plan <- fetch_preflight_test_plan("csv")
  before_plan <- serialize(plan, NULL)
  suggests <- c("dataRetrieval", "edr4r", "readr")
  loaded_before <- suggests %in% loadedNamespaces()
  expect_false(unname(loaded_before[[match("readr", suggests)]]))
  handler_paths <- file.path(
    system.file("handlers", package = "geoconnexr"),
    c("registry.yml", "implementations-r.json")
  )
  asset_bytes <- lapply(handler_paths, readBin, what = "raw", n = 2^20)
  temp_before <- sort(list.files(
    tempdir(), all.files = TRUE, no.. = TRUE, recursive = TRUE
  ))
  network_calls <- 0L
  dns_calls <- 0L
  cache_calls <- 0L

  preflight <- testthat::with_mocked_bindings(
    gx_fetch_preflight_impl(plan, now = fetch_preflight_test_now),
    gx_default_performer = function(...) {
      network_calls <<- network_calls + 1L
      stop("HTTP execution forbidden", call. = FALSE)
    },
    gx_default_file_performer = function(...) {
      network_calls <<- network_calls + 1L
      stop("download execution forbidden", call. = FALSE)
    },
    gx_default_dns_resolver = function(...) {
      dns_calls <<- dns_calls + 1L
      stop("DNS forbidden", call. = FALSE)
    },
    gx_cache_backend = function(...) {
      cache_calls <<- cache_calls + 1L
      stop("cache forbidden", call. = FALSE)
    },
    .package = "geoconnexr"
  )

  expect_identical(network_calls, 0L)
  expect_identical(dns_calls, 0L)
  expect_identical(cache_calls, 0L)
  expect_identical(suggests %in% loadedNamespaces(), loaded_before)
  expect_identical(serialize(plan, NULL), before_plan)
  expect_identical(serialize(preflight$plan, NULL), before_plan)
  expect_identical(preflight$plan$requests, list())
  expect_false(preflight$metadata$execution_ready)
  expect_identical(
    lapply(handler_paths, readBin, what = "raw", n = 2^20),
    asset_bytes
  )
  expect_identical(
    sort(list.files(tempdir(), all.files = TRUE, no.. = TRUE, recursive = TRUE)),
    temp_before
  )

  probe_source <- paste(c(
    deparse(body(gx_fetch_preflight_package_version_impl)),
    deparse(body(gx_fetch_preflight_description_version_impl))
  ), collapse = "\n")
  forbidden <- paste0(
    "\\b(requireNamespace|loadNamespace|getExportedValue|",
    "packageDescription|packageVersion|find\\.package|readRDS|",
    "system\\.file|library)\\s*\\("
  )
  expect_false(grepl(forbidden, probe_source, perl = TRUE))
})

test_that("printing and probe errors do not disclose URLs or library details", {
  catalog <- fetch_plan_test_catalog()
  secret <- "m7b-query-secret"
  alpha <- which(
    catalog$datasets$distribution_id ==
      fetch_plan_test_hash("distribution:alpha")
  )
  catalog$datasets$distribution_url[alpha] <- paste0(
    "https://example.org/data/alpha.csv?token=", secret
  )
  plan <- fetch_plan_test_build(catalog)
  preflight <- fetch_preflight_test_build(plan)

  printed <- character()
  output <- capture.output(withCallingHandlers(
    print(preflight),
    message = function(cnd) {
      printed <<- c(printed, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  ))
  printed <- c(printed, output)
  expect_false(any(grepl(secret, printed, fixed = TRUE)))
  expect_false(any(grepl("distribution_url", printed, fixed = TRUE)))
  expect_true(any(grepl(
    "Blocked selected distributions", printed, fixed = TRUE
  )))

  probe_secret <- "/private/library/m7b-probe-secret"
  condition <- fetch_preflight_test_error(gx_fetch_preflight_impl(
    plan,
    version_resolver = function(...) stop(probe_secret, call. = FALSE),
    now = fetch_preflight_test_now
  ))
  expect_s3_class(condition, "gx_error_fetch_preflight")
  expect_false(grepl(secret, conditionMessage(condition), fixed = TRUE))
  expect_false(grepl(probe_secret, conditionMessage(condition), fixed = TRUE))
  trace_text <- paste(capture.output(str(condition$trace)), collapse = "\n")
  expect_false(grepl(secret, trace_text, fixed = TRUE))
  expect_false(grepl(probe_secret, trace_text, fixed = TRUE))
})

test_that("M7b package preflight remains internal and exports no execution API", {
  internal <- c(
    "gx_fetch_preflight_impl", "gx_fetch_preflight_validate_impl",
    "gx_fetch_preflight_package_version_impl"
  )
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(internal %in% exports))
  expect_false("gx_fetch_preflight" %in% exports)
  expect_false("gx_fetch_plan" %in% exports)
  expect_false("gx_fetch" %in% exports)
})
