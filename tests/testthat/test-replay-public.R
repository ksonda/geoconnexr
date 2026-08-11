replay_test_inputs <- function() {
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  list(
    catalog = catalog,
    fetched = fetched,
    harmonized = gx_harmonize(fetched)
  )
}

replay_test_report_runtime <- function(.local_envir = parent.frame()) {
  path <- withr::local_tempfile(
    pattern = "gx-replay-quarto-", .local_envir = .local_envir
  )
  writeLines(c("#!/bin/sh", "exit 0"), path, useBytes = TRUE)
  Sys.chmod(path, mode = "0755")
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  capability <- list(
    version = "1.5.1",
    quarto_render = function(
        input = NULL, output_format = NULL, output_file = NULL,
        execute = TRUE, metadata = NULL, metadata_file = NULL,
        quiet = FALSE, profile = NULL, quarto_args = NULL,
        pandoc_args = NULL, as_job = "auto") invisible(NULL),
    quarto_path = function(normalize = TRUE) path,
    quarto_version = function() numeric_version("1.8.27"),
    quarto_available = function(min = NULL, max = NULL, error = FALSE) TRUE
  )
  gx_package_quarto_cli_impl(
    capability_resolver = function() capability,
    version_resolver = function(...) "1.8.27"
  )
}

replay_test_report_renderer <- function(path, input, timeout) {
  source <- paste(readLines(input, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  marker <- function(name) {
    pattern <- paste0("data-", name, "=\"([^\"]+)\"")
    line <- grep(
      pattern, strsplit(source, "\n", fixed = TRUE)[[1L]], value = TRUE
    )[[1L]]
    sub(paste0(".*", pattern, ".*"), "\\1", line, perl = TRUE)
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

test_that("M9ab inspects fixed catalog snapshots offline", {
  parent <- withr::local_tempdir(pattern = "gx-replay-catalog-")
  target <- file.path(parent, "snapshot")
  snapshot <- gx_snapshot(
    fetch_orchestration_test_usgs_daily_catalog(), target
  )

  value <- gx_replay(snapshot)
  from_manifest <- gx_replay(file.path(target, "manifest.json"))

  expect_s3_class(value, "gx_replay")
  expect_identical(value$kind, "catalog_snapshot")
  expect_identical(value$stage, "catalog")
  expect_s3_class(value$view, "gx_snapshot_catalog_view")
  expect_null(value$report)
  expect_true(value$metadata$offline)
  expect_true(value$metadata$read_only)
  expect_true(value$metadata$verified)
  expect_true(value$metadata$loaded)
  expect_false(value$metadata$refresh)
  expect_false(value$metadata$recipe_executed)
  expect_false(value$metadata$authenticity)
  expect_false(value$metadata$frictionless)
  expect_false(value$metadata$replayable)
  expect_identical(value$replay_id, from_manifest$replay_id)
  expect_identical(gx_replay_validate_impl(value), invisible(value))
})

test_that("M9ab hydrates every fixed package stage without reconstruction", {
  inputs <- replay_test_inputs()
  parent <- withr::local_tempdir(pattern = "gx-replay-package-")
  for (stage in names(inputs)) {
    target <- file.path(parent, stage)
    package <- gx_package(
      inputs[[stage]],
      target,
      catalog = if (identical(stage, "catalog")) NULL else inputs$catalog
    )
    loaded <- gx_package_load(target)
    value <- gx_replay(loaded)

    expect_identical(value$kind, "package")
    expect_identical(value$stage, stage)
    expect_s3_class(value$view, "gx_package_hydrated")
    expect_identical(value$view$stage, stage)
    expect_null(value$report)
    expect_false(value$view$metadata$reconstructed_objects)
    expect_identical(
      value$verification$manifest_sha256,
      package$verification$manifest_sha256
    )
  }
})

test_that("M9ab includes stored report evidence without invoking Quarto", {
  parent <- withr::local_tempdir(pattern = "gx-replay-report-")
  target <- file.path(parent, "package")
  runtime <- replay_test_report_runtime()
  package <- testthat::with_mocked_bindings(
    gx_package(
      fetch_orchestration_test_usgs_daily_catalog(),
      target,
      report = TRUE
    ),
    gx_package_quarto_cli_impl = function(...) runtime,
    gx_package_report_render_command_impl = replay_test_report_renderer,
    .package = "geoconnexr"
  )
  blocked <- function(...) stop("external capability used")

  value <- testthat::with_mocked_bindings(
    gx_replay(package),
    gx_package_quarto_cli_impl = blocked,
    gx_package_report_render_command_impl = blocked,
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    gx_package_write_impl = blocked,
    gx_snapshot_write_catalog_impl = blocked,
    .package = "geoconnexr"
  )

  expect_true(value$metadata$report)
  expect_s3_class(value$report, "gx_report")
  expect_false(value$report$metadata$execution_enabled)
  expect_false(value$report$metadata$exported)
})

test_that("M9ab rejects refresh and destinations before inspecting a source", {
  expect_error(
    gx_replay(stop("source inspected"), refresh = TRUE),
    class = "gx_error_replay_refresh"
  )
  expect_error(
    gx_replay(stop("source inspected"), dir = "destination"),
    class = "gx_error_replay_destination"
  )
  expect_error(
    gx_replay(stop("source inspected"), unsupported = TRUE),
    class = "gx_error_replay_input"
  )
  expect_error(gx_replay("missing", refresh = NA),
    class = "gx_error_replay_input"
  )
  expect_error(gx_replay("missing"), class = "gx_error_replay_input")
})

test_that("M9ab admits only exact fixed public loading profiles", {
  fixture <- test_path("..", "fixtures", "snapshot", "catalog-only-v1")
  expect_s3_class(gx_snapshot_verify(fixture), "gx_snapshot_verification")
  expect_error(gx_replay(fixture), class = "gx_error_replay_profile")
  expect_error(
    gx_replay(gx_snapshot_verify(fixture)),
    class = "gx_error_replay_input"
  )
})

test_that("M9ab replay evidence fails closed under forgery and mutation", {
  parent <- withr::local_tempdir(pattern = "gx-replay-forgery-")
  target <- file.path(parent, "package")
  gx_package(fetch_orchestration_test_usgs_daily_catalog(), target)
  value <- gx_replay(target)

  forged <- value
  forged$metadata$replayable <- TRUE
  expect_error(
    gx_replay_validate_impl(forged),
    class = "gx_error_replay_contract"
  )
  forged <- value
  forged$view$hydration_id <- paste(rep("0", 64L), collapse = "")
  expect_error(
    gx_replay_validate_impl(forged),
    class = "gx_error_replay_contract"
  )
  forged <- value
  forged$replay_id <- paste(rep("0", 64L), collapse = "")
  expect_error(
    gx_replay_validate_impl(forged),
    class = "gx_error_replay_contract"
  )

  writeBin(charToRaw("mutation"), file.path(target, "catalog", "sites.csv"))
  expect_error(
    gx_replay_validate_impl(value),
    class = "gx_error_replay_contract"
  )
})

test_that("M9ab adds only the reviewed public replay surface", {
  exports <- getNamespaceExports("geoconnexr")
  expect_true("gx_replay" %in% exports)
  expect_false(any(c(
    "gx_replay_validate_impl", "gx_replay_profile_impl",
    "gx_replay_source_path_impl"
  ) %in% exports))
})
