package_report_resources_test_cli <- function(.local_envir = parent.frame()) {
  path <- withr::local_tempfile(
    pattern = "gx-package-report-resources-cli-",
    .local_envir = .local_envir
  )
  writeLines(c("#!/bin/sh", "exit 0"), path, useBytes = TRUE)
  Sys.chmod(path, mode = "0755")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

package_report_resources_test_capability <- function(path) {
  list(
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
}

package_report_resources_test_runtime <- function(path) {
  capability <- package_report_resources_test_capability(path)
  gx_package_quarto_cli_impl(
    capability_resolver = function() capability,
    version_resolver = function(...) "1.8.27"
  )
}

package_report_resources_test_html <- function(hydrated, summary) {
  rows <- vapply(names(summary), function(name) {
    paste0(
      "<tr data-summary=\"", name, "\"><th>", name,
      "</th><td>", summary[[name]], "</td></tr>"
    )
  }, character(1))
  paste0(
    "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">",
    "<title>Geoconnex data package report</title></head><body>",
    "<main id=\"geoconnexr-report\" data-contract-version=\"0.1.0\" ",
    "data-hydration-id=\"", hydrated$hydration_id, "\" data-stage=\"",
    hydrated$stage, "\"><table><tbody>", paste(rows, collapse = ""),
    "</tbody></table></main></body></html>\n"
  )
}

package_report_resources_test_fixture <- function(
    .local_envir = parent.frame()) {
  parent <- withr::local_tempdir(
    pattern = "gx-package-report-resources-",
    .local_envir = .local_envir
  )
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  base <- gx_package_resources_impl(gx_package_input_impl(catalog))
  base_path <- file.path(parent, "base")
  gx_package_write_impl(base, base_path)
  hydrated <- gx_package_hydrate(base_path)
  cli_path <- package_report_resources_test_cli(.local_envir)
  runtime <- package_report_resources_test_runtime(cli_path)
  summary <- gx_package_report_summary_impl(hydrated)
  renderer <- function(path, input, timeout) {
    writeBin(
      charToRaw(package_report_resources_test_html(hydrated, summary)),
      file.path(dirname(input), "report.html")
    )
    character()
  }
  report <- gx_package_report_impl(
    hydrated,
    runtime_resolver = function() runtime,
    render_resolver = renderer
  )
  list(
    parent = parent,
    catalog = catalog,
    base = base,
    base_path = base_path,
    runtime = runtime,
    report = report,
    bundle = gx_package_report_resources_impl(base, report)
  )
}

test_that("M9z binds one verified report to its exact M9k base bundle", {
  fixture <- package_report_resources_test_fixture()
  bundle <- fixture$bundle

  expect_s3_class(bundle, "gx_package_report_resources")
  expect_identical(bundle$stage, fixture$base$stage)
  expect_identical(bundle$timeseries, fixture$base$timeseries)
  expect_identical(bundle$base, fixture$base)
  expect_identical(bundle$report, fixture$report)
  expect_identical(
    setdiff(bundle$resources$path, fixture$base$resources$path),
    "report/index.html"
  )
  expect_identical(
    bundle$contents[fixture$base$resources$path],
    fixture$base$contents
  )
  expect_identical(
    bundle$contents[["report/index.html"]],
    fixture$report$output$bytes
  )
  expect_true(bundle$metadata$quarto)
  expect_true(bundle$metadata$report)
  expect_false(bundle$metadata$deterministic)
  expect_identical(bundle$metadata$counts$report_resources, 1L)
  expect_false(bundle$metadata$publishes)
  expect_false(bundle$metadata$replayable)
  expect_identical(
    gx_package_report_resources_validate_impl(bundle),
    invisible(bundle)
  )
})

test_that("M9z stages and verifies one private report package", {
  fixture <- package_report_resources_test_fixture()
  target <- file.path(fixture$parent, "with-report")
  publication <- gx_package_write_impl(fixture$bundle, target)

  expect_s3_class(publication, "gx_package_publication")
  expect_true(publication$metadata$quarto)
  expect_true(publication$metadata$report)
  expect_true(file.exists(file.path(target, "report", "index.html")))
  expect_identical(
    readBin(
      file.path(target, "report", "index.html"),
      "raw",
      n = length(fixture$report$output$bytes)
    ),
    fixture$report$output$bytes
  )
  manifest <- publication$verification$manifest
  serialization <- manifest$effective_options$serialization
  expect_true(manifest$recipe$output$report)
  expect_identical(
    serialization$resource_profile,
    "fixed-in-memory-resources-v2+fixed-report-v1"
  )
  expect_identical(
    serialization$report,
    gx_snapshot_normalize_json(
      gx_package_bundle_report_manifest_impl(fixture$bundle)
    )
  )
  expect_identical(
    gx_package_report_manifest_profile_impl(publication$verification)$report,
    serialization$report
  )
  expect_identical(
    gx_package_publication_validate_impl(publication),
    invisible(publication)
  )

  loaded <- gx_package_load(target)
  expect_identical(
    loaded$resources$role[loaded$resources$path == "report/index.html"],
    "report_html"
  )
  expect_error(
    gx_package_manifest_profile_impl(publication$verification),
    class = "gx_error_package_public_contract"
  )
  before <- gx_snapshot_verify(target)$manifest_sha256
  replaced <- gx_package(fixture$catalog, target, overwrite = TRUE)
  expect_true(replaced$previous$manifest$recipe$output$report)
  expect_false(replaced$verification$manifest$recipe$output$report)
  expect_false(identical(
    gx_snapshot_verify(target)$manifest_sha256, before
  ))
  base_replacement <- gx_package_replace_impl(fixture$base, target)
  expect_s3_class(base_replacement, "gx_package_replacement")
})

test_that("M9z replaces base and private-report packages through M9r", {
  fixture <- package_report_resources_test_fixture()
  replacement <- gx_package_replace_impl(fixture$bundle, fixture$base_path)

  expect_s3_class(replacement, "gx_package_replacement")
  expect_false(replacement$previous$manifest$recipe$output$report)
  expect_true(replacement$verification$manifest$recipe$output$report)
  expect_true(file.exists(file.path(
    fixture$base_path, "report", "index.html"
  )))
  expect_identical(
    gx_package_replacement_validate_impl(replacement),
    invisible(replacement)
  )

  second <- gx_package_replace_impl(fixture$bundle, fixture$base_path)
  expect_true(second$previous$manifest$recipe$output$report)
  expect_true(second$verification$manifest$recipe$output$report)
  expect_identical(
    gx_package_replacement_validate_impl(second),
    invisible(second)
  )
})

test_that("M9z rejects mismatched report lineage and evidence forgery", {
  fixture <- package_report_resources_test_fixture()
  catalog <- fixture$catalog
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  other <- gx_package_resources_impl(
    gx_package_input_impl(gx_harmonize(fetched), catalog)
  )
  expect_error(
    gx_package_report_resources_impl(other, fixture$report),
    class = "gx_error_package_report_resources_lineage"
  )

  mutations <- list(
    base = function(x) {
      x$base$bundle_id <- paste(rep("0", 64L), collapse = "")
      x
    },
    report = function(x) {
      x$report$report_id <- paste(rep("0", 64L), collapse = "")
      x
    },
    resource = function(x) {
      position <- match("report/index.html", x$resources$path)
      x$resources$sha256[[position]] <- paste(rep("0", 64L), collapse = "")
      x
    },
    content = function(x) {
      x$contents[["report/index.html"]][[1L]] <- as.raw(0L)
      x
    },
    metadata = function(x) {
      x$metadata$replayable <- TRUE
      x
    },
    identity = function(x) {
      x$bundle_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(fixture$bundle, NULL)))
    expect_error(
      gx_package_report_resources_validate_impl(forged),
      class = "gx_error_package_report_resources",
      info = name
    )
  }
})

test_that("M9z leaves base packages unchanged and remains internal", {
  fixture <- package_report_resources_test_fixture()
  manifest <- gx_package_writer_manifest_impl(fixture$base)
  expect_false(manifest$recipe$output$report)
  expect_false("report" %in% names(
    manifest$effective_options$serialization
  ))
  expect_identical(
    manifest$effective_options$serialization$resource_profile,
    "fixed-in-memory-resources-v2"
  )
  expect_false(fixture$base$metadata$quarto)

  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_package_report_resources_impl",
    "gx_package_report_resources_validate_impl",
    "gx_package_report_manifest_profile_impl"
  ) %in% exports))
})
