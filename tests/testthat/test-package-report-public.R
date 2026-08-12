package_report_public_test_cli <- function(.local_envir = parent.frame()) {
  path <- withr::local_tempfile(
    pattern = "gx-package-report-public-cli-",
    .local_envir = .local_envir
  )
  writeLines(c("#!/bin/sh", "exit 0"), path, useBytes = TRUE)
  Sys.chmod(path, mode = "0755")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

package_report_public_test_runtime <- function(path) {
  capability <- list(
    version = "1.5.1",
    quarto_render = function(
        input = NULL,
        output_format = NULL,
        output_file = NULL,
        execute = TRUE,
        metadata = NULL,
        metadata_file = NULL,
        quiet = FALSE,
        profile = NULL,
        quarto_args = NULL,
        pandoc_args = NULL,
        as_job = "auto") invisible(NULL),
    quarto_path = function(normalize = TRUE) path,
    quarto_version = function() numeric_version("1.8.27"),
    quarto_available = function(min = NULL, max = NULL, error = FALSE) TRUE
  )
  gx_package_quarto_cli_impl(
    capability_resolver = function() capability,
    version_resolver = function(...) "1.8.27"
  )
}

package_report_public_test_renderer <- function(path, input, timeout) {
  source <- paste(readLines(input, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  marker <- function(name) {
    pattern <- paste0("data-", name, "=\"([^\"]+)\"")
    sub(paste0(".*", pattern, ".*"), "\\1", grep(
      pattern, strsplit(source, "\n", fixed = TRUE)[[1L]],
      value = TRUE
    )[[1L]], perl = TRUE)
  }
  rows <- regmatches(
    source,
    gregexpr(
      "<tr data-summary=\"[^\"]+\"><th>[^<]+</th><td>[0-9]+</td></tr>",
      source,
      perl = TRUE
    )
  )[[1L]]
  html <- paste0(
    "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">",
    "<title>Geoconnex data package report</title></head><body>",
    "<main id=\"geoconnexr-report\" data-contract-version=\"",
    marker("contract-version"), "\" data-hydration-id=\"",
    marker("hydration-id"), "\" data-stage=\"", marker("stage"),
    "\"><table><tbody>", paste(rows, collapse = ""),
    "</tbody></table></main></body></html>\n"
  )
  writeBin(charToRaw(html), file.path(dirname(input), "report.html"))
  character()
}

package_report_public_test_call <- function(code, runtime) {
  testthat::with_mocked_bindings(
    code,
    gx_package_quarto_cli_impl = function(...) runtime,
    gx_package_report_render_command_impl =
      package_report_public_test_renderer,
    .package = "geoconnexr"
  )
}

test_that("M9aa publicly creates, loads, hydrates, and exposes one report", {
  parent <- withr::local_tempdir(pattern = "gx-package-report-public-")
  target <- file.path(parent, "package")
  output <- file.path(parent, "report.html")
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  runtime <- package_report_public_test_runtime(
    package_report_public_test_cli()
  )

  package <- package_report_public_test_call(
    gx_package(catalog, target, report = TRUE), runtime
  )
  expect_s3_class(package, "gx_package")
  expect_true(package$metadata$report)
  expect_true(package$metadata$quarto)
  expect_true(package$verification$manifest$recipe$output$report)
  expect_true(file.exists(file.path(target, "report", "index.html")))
  expect_length(list.files(
    tempdir(), pattern = "^\\.gx-package-report-source-", all.files = TRUE
  ), 0L)

  loaded <- gx_package_load(target)
  position <- match("report_html", loaded$resources$role)
  expect_false(is.na(position))
  expect_identical(
    loaded$contents[[position]],
    package_report_public_test_call(gx_report(package)$html, runtime)
  )
  expect_s3_class(gx_package_tables(target), "gx_package_tables")
  expect_s3_class(gx_package_hydrate(target), "gx_package_hydrated")

  report <- gx_report(loaded, output = output)
  expect_s3_class(report, "gx_report")
  expect_true(report$metadata$exported)
  expect_false(report$metadata$read_only)
  expect_identical(report$output, normalizePath(output, winslash = "/"))
  expect_identical(
    readBin(output, "raw", n = length(report$html)), report$html
  )
  expect_identical(gx_report_validate_impl(report), invisible(report))
})

test_that("M9af publicly composes reports with Frictionless metadata", {
  parent <- withr::local_tempdir(pattern = "gx-package-report-frictionless-")
  target <- file.path(parent, "package")
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  runtime <- package_report_public_test_runtime(
    package_report_public_test_cli()
  )

  package <- package_report_public_test_call(
    gx_package(
      catalog,
      target,
      report = TRUE,
      frictionless = TRUE
    ),
    runtime
  )
  serialization <-
    package$verification$manifest$effective_options$serialization
  descriptor <- gx_snapshot_parse_json(readBin(
    file.path(target, "datapackage.json"),
    what = "raw",
    n = .gx_package_frictionless_max_bytes + 1L
  ))
  report_resource <- descriptor$resources[[match(
    "report/index.html",
    vapply(descriptor$resources, `[[`, character(1), "path")
  )]]
  replay <- gx_replay(target)
  hydrated <- replay$view
  loaded <- hydrated$table_view$loaded

  expect_true(package$metadata$report)
  expect_true(package$metadata$frictionless)
  expect_identical(
    serialization$report$profile,
    "fixed-quarto-html-report-v1"
  )
  expect_identical(
    serialization$frictionless$profile,
    "fixed-frictionless-data-package-v1"
  )
  expect_identical(report_resource$profile, "data-resource")
  expect_identical(report_resource$format, "bin")
  expect_identical(report_resource$geoconnexr$format, "html")
  expect_true(any(loaded$resources$role == "report_html"))
  expect_true(loaded$metadata$frictionless)
  expect_s3_class(replay$report, "gx_report")
  expect_true(replay$metadata$report)
  expect_true(replay$metadata$frictionless)
  expect_false(replay$metadata$recipe_executed)
})

test_that("M9aa replaces safely between report-free and report packages", {
  parent <- withr::local_tempdir(pattern = "gx-package-report-replace-")
  target <- file.path(parent, "package")
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  runtime <- package_report_public_test_runtime(
    package_report_public_test_cli()
  )
  initial <- gx_package(catalog, target)
  with_report <- package_report_public_test_call(
    gx_package(catalog, target, overwrite = TRUE, report = TRUE), runtime
  )
  expect_false(initial$metadata$report)
  expect_true(with_report$metadata$report)
  expect_false(with_report$previous$manifest$recipe$output$report)
  expect_true(file.exists(file.path(target, "report", "index.html")))

  repeated <- package_report_public_test_call(
    gx_package(catalog, target, overwrite = TRUE, report = TRUE), runtime
  )
  expect_true(repeated$previous$manifest$recipe$output$report)
  expect_true(repeated$metadata$report)

  without_report <- gx_package(catalog, target, overwrite = TRUE)
  expect_true(without_report$previous$manifest$recipe$output$report)
  expect_false(without_report$metadata$report)
  expect_false(dir.exists(file.path(target, "report")))
  expect_error(gx_report(without_report), class = "gx_error_report_profile")
})

test_that("M9aa report failures preserve absent and prior destinations", {
  parent <- withr::local_tempdir(pattern = "gx-package-report-failure-")
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  runtime <- package_report_public_test_runtime(
    package_report_public_test_cli()
  )
  absent <- file.path(parent, "absent")
  expect_error(
    testthat::with_mocked_bindings(
      gx_package(catalog, absent, report = TRUE),
      gx_package_quarto_cli_impl = function(...) runtime,
      gx_package_report_render_command_impl = function(...) stop("render"),
      .package = "geoconnexr"
    ),
    class = "gx_error_package_report_render"
  )
  expect_false(file.exists(absent) || dir.exists(absent))

  target <- file.path(parent, "prior")
  prior <- gx_package(catalog, target)
  expect_error(
    testthat::with_mocked_bindings(
      gx_package(
        catalog, target, overwrite = TRUE, report = TRUE
      ),
      gx_package_quarto_cli_impl = function(...) runtime,
      gx_package_report_render_command_impl = function(...) stop("render"),
      .package = "geoconnexr"
    ),
    class = "gx_error_package_report_render"
  )
  expect_identical(
    gx_snapshot_verify(target)$manifest_sha256,
    prior$verification$manifest_sha256
  )
  expect_length(list.files(
    tempdir(), pattern = "^\\.gx-package-report-source-", all.files = TRUE
  ), 0L)
})

test_that("M9aa keeps report opt-in and output creation-only", {
  parent <- withr::local_tempdir(pattern = "gx-package-report-scope-")
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  calls <- 0L
  package <- testthat::with_mocked_bindings(
    gx_package(catalog, file.path(parent, "plain")),
    gx_package_report_bundle_impl = function(...) {
      calls <<- calls + 1L
      stop("unexpected report")
    },
    .package = "geoconnexr"
  )
  expect_identical(calls, 0L)
  expect_false(package$metadata$report)
  expect_error(
    gx_package(catalog, file.path(parent, "invalid"), report = NA),
    class = "gx_error_package_input"
  )

  runtime <- package_report_public_test_runtime(
    package_report_public_test_cli()
  )
  reported <- package_report_public_test_call(
    gx_package(catalog, file.path(parent, "reported"), report = TRUE),
    runtime
  )
  output <- file.path(parent, "existing.html")
  writeLines("preserve", output)
  expect_error(
    gx_report(reported, output),
    class = "gx_error_report_output_exists"
  )
  expect_identical(readLines(output), "preserve")
})

test_that("M9aa public report evidence fails closed under forgery", {
  parent <- withr::local_tempdir(pattern = "gx-package-report-forgery-")
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  runtime <- package_report_public_test_runtime(
    package_report_public_test_cli()
  )
  package <- package_report_public_test_call(
    gx_package(catalog, file.path(parent, "package"), report = TRUE), runtime
  )
  report <- gx_report(package)
  mutations <- list(
    loaded = function(x) {
      x$loaded$load_id <- paste(rep("0", 64L), collapse = "")
      x
    },
    descriptor = function(x) {
      x$report$report_id <- paste(rep("0", 64L), collapse = "")
      x
    },
    html = function(x) {
      x$html[[1L]] <- as.raw(bitwXor(as.integer(x$html[[1L]]), 1L))
      x
    },
    metadata = function(x) {
      x$metadata$replayable <- TRUE
      x
    },
    identity = function(x) {
      x$report_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(report, NULL)))
    expect_error(
      gx_report_validate_impl(forged),
      class = "gx_error_report",
      info = name
    )
  }
})

test_that("M9aa exports only gx_report and the extended package boundary", {
  exports <- getNamespaceExports("geoconnexr")
  expect_true(all(c("gx_package", "gx_report") %in% exports))
  expect_false(any(c(
    "gx_package_report_bundle_impl", "gx_report_validate_impl",
    "gx_report_export_impl"
  ) %in% exports))
})
