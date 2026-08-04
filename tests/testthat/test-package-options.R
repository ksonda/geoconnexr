package_options_test_now <- function() {
  as.POSIXct("2026-08-03T12:00:00Z", tz = "UTC")
}

package_options_test_resolver <- function(versions, calls) {
  force(versions)
  force(calls)
  function(package) {
    calls$packages <- c(calls$packages, package)
    if (package %in% names(versions)) versions[[package]] else NA_character_
  }
}

test_that("M9t classifies installed and missing optional packages", {
  calls <- new.env(parent = emptyenv())
  calls$packages <- character()
  value <- gx_package_options_impl(
    version_resolver = package_options_test_resolver(
      c(arrow = "22.0.0.1", quarto = NA_character_),
      calls
    ),
    now = package_options_test_now
  )

  expect_s3_class(value, "gx_package_options")
  expect_identical(value$status, "inspected_not_authorized")
  expect_identical(calls$packages, c("arrow", "quarto"))
  expect_identical(value$capabilities$feature, c("parquet", "report"))
  expect_identical(value$capabilities$package, c("arrow", "quarto"))
  expect_identical(
    value$capabilities$minimum_version,
    c("14.0.0", "1.5.1")
  )
  expect_identical(
    value$capabilities$package_status,
    c("version_satisfied", "missing")
  )
  expect_identical(
    value$capabilities$option_status,
    c("blocked_symbols_unchecked", "skipped_missing_pkg")
  )
  expect_true(value$metadata$host_specific)
  expect_false(value$metadata$loads_namespaces)
  expect_false(value$metadata$execution_ready)
  expect_false(value$metadata$parquet)
  expect_false(value$metadata$report)
  expect_identical(value$metadata$counts, list(
    features = 2L,
    packages_probed = 2L,
    missing_packages = 1L,
    unpinned_packages = 0L,
    version_too_old_packages = 0L,
    version_satisfied_packages = 1L,
    ready_features = 0L
  ))
  expect_identical(
    gx_package_options_validate_impl(value),
    invisible(value)
  )
})

test_that("M9w preflight classifies both reviewed minimums", {
  cases <- list(
    absent = c(arrow = NA_character_, quarto = NA_character_),
    old = c(arrow = "13.0.0", quarto = NA_character_),
    quarto_old = c(arrow = "14.0.0", quarto = "1.4.4"),
    present = c(arrow = "14.0.0", quarto = "1.5.1")
  )
  expected <- list(
    absent = c("skipped_missing_pkg", "skipped_missing_pkg"),
    old = c("blocked_package_version", "skipped_missing_pkg"),
    quarto_old = c("blocked_symbols_unchecked", "blocked_package_version"),
    present = c("blocked_symbols_unchecked", "blocked_symbols_unchecked")
  )
  package_status <- list(
    absent = c("missing", "missing"),
    old = c("version_too_old", "missing"),
    quarto_old = c("version_satisfied", "version_too_old"),
    present = c("version_satisfied", "version_satisfied")
  )
  for (name in names(cases)) {
    calls <- new.env(parent = emptyenv())
    calls$packages <- character()
    value <- gx_package_options_impl(
      version_resolver = package_options_test_resolver(cases[[name]], calls),
      now = package_options_test_now
    )
    expect_identical(
      value$capabilities$option_status,
      expected[[name]],
      info = name
    )
    expect_identical(
      value$capabilities$package_status,
      package_status[[name]],
      info = name
    )
    expect_false(value$metadata$execution_ready, info = name)
    expect_identical(value$metadata$counts$ready_features, 0L, info = name)
  }
})

test_that("M9t rejects failed and malformed metadata probes", {
  cases <- list(
    error = function(package) stop("probe failed"),
    warning = function(package) {
      warning("probe warning")
      "1.0.0"
    },
    malformed = function(package) "not a version"
  )
  for (name in names(cases)) {
    expect_error(
      gx_package_options_impl(cases[[name]], package_options_test_now),
      class = "gx_error_package_options_probe",
      info = name
    )
  }
  expect_error(
    gx_package_options_impl(NULL, package_options_test_now),
    class = "gx_error_package_options_input"
  )
})

test_that("M9t capability evidence fails closed under forgery", {
  calls <- new.env(parent = emptyenv())
  calls$packages <- character()
  value <- gx_package_options_impl(
    package_options_test_resolver(
      c(arrow = "22.0.0.1", quarto = NA_character_),
      calls
    ),
    package_options_test_now
  )
  mutations <- list(
    capability = function(x) {
      x$capabilities$option_status[[1L]] <- "ready"
      x
    },
    version = function(x) {
      x$capabilities$installed_version[[1L]] <- "21.0.0"
      x
    },
    metadata = function(x) {
      x$metadata$execution_ready <- TRUE
      x
    },
    metadata_shape = function(x) {
      x$metadata <- "forged"
      x
    },
    identity = function(x) {
      x$capability_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(value, NULL)))
    expect_error(
      gx_package_options_validate_impl(forged),
      class = "gx_error_package_options",
      info = name
    )
  }
})

test_that("M9t preflight performs no external or namespace work", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or namespace seam", call. = FALSE)
  }
  resolver_calls <- new.env(parent = emptyenv())
  resolver_calls$packages <- character()

  expect_no_error(testthat::with_mocked_bindings(
    gx_package_options_impl(
      package_options_test_resolver(
        c(arrow = "22.0.0.1", quarto = NA_character_),
        resolver_calls
      ),
      package_options_test_now
    ),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9t host probe is advisory and remains internal", {
  value <- gx_package_options_impl(now = package_options_test_now)
  expect_s3_class(value, "gx_package_options")
  expect_identical(value$capabilities$package, c("arrow", "quarto"))
  expect_false(value$metadata$execution_ready)
  expect_false(any(c(
    "gx_package_options_impl", "gx_package_options_validate_impl",
    "gx_package_options_package_version_impl"
  ) %in% getNamespaceExports("geoconnexr")))
})
