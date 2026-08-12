package_report_test_hydrated <- function(.local_envir = parent.frame()) {
  parent <- withr::local_tempdir(
    pattern = "gx-package-report-package-",
    .local_envir = .local_envir
  )
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  target <- file.path(parent, "package")
  gx_package(catalog, target)
  gx_package_hydrate(target)
}

package_report_test_cli_file <- function(.local_envir = parent.frame()) {
  path <- withr::local_tempfile(
    pattern = "gx-package-report-cli-",
    .local_envir = .local_envir
  )
  writeLines(c("#!/bin/sh", "exit 0"), path, useBytes = TRUE)
  Sys.chmod(path, mode = "0755")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

package_report_test_capability <- function(path) {
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

package_report_test_runtime <- function(path) {
  capability <- package_report_test_capability(path)
  gx_package_quarto_cli_impl(
    capability_resolver = function() capability,
    version_resolver = function(...) "1.8.27"
  )
}

package_report_test_html <- function(hydrated, summary) {
  labels <- c(
    sites = "Sites",
    datasets = "Datasets",
    problems = "Problems",
    requests = "Catalog requests",
    fetch_statuses = "Fetch statuses",
    native_resources = "Native resources",
    observations = "Harmonized observations",
    harmonized_resources = "Harmonized resources"
  )
  rows <- vapply(names(summary), function(name) {
    paste0(
      "<tr data-summary=\"", name, "\"><th>", labels[[name]],
      "</th><td>", summary[[name]], "</td></tr>"
    )
  }, character(1))
  paste0(
    "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">",
    "<title>Geoconnex data package report</title></head><body>",
    "<main id=\"geoconnexr-report\" data-contract-version=\"0.1.0\" ",
    "data-hydration-id=\"", hydrated$hydration_id, "\" data-stage=\"",
    hydrated$stage, "\"><table><tbody>",
    paste(rows, collapse = ""),
    "</tbody></table></main></body></html>\n"
  )
}

package_report_test_renderer <- function(
    hydrated,
    calls = NULL,
    transform = identity) {
  summary <- gx_package_report_summary_impl(hydrated)
  force(transform)
  function(path, input, timeout) {
    if (!is.null(calls)) {
      calls$path <- path
      calls$input <- input
      calls$timeout <- timeout
      calls$source <- readBin(input, "raw", n = file.info(input)$size)
    }
    html <- transform(package_report_test_html(hydrated, summary))
    writeBin(charToRaw(html), file.path(dirname(input), "report.html"))
    character()
  }
}

test_that("M9y renders and verifies one fixed execution-disabled HTML report", {
  hydrated <- package_report_test_hydrated()
  cli_path <- package_report_test_cli_file()
  runtime <- package_report_test_runtime(cli_path)
  calls <- new.env(parent = emptyenv())
  calls$path <- NULL
  calls$input <- NULL
  calls$timeout <- NULL
  calls$source <- NULL
  stage_parent <- withr::local_tempdir(pattern = "gx-package-report-stage-")

  value <- gx_package_report_impl(
    hydrated,
    runtime_resolver = function() runtime,
    render_resolver = package_report_test_renderer(hydrated, calls),
    temp_parent = stage_parent
  )

  expect_s3_class(value, "gx_package_report")
  expect_identical(value$status, "rendered_and_verified")
  expect_identical(value$stage, "catalog")
  expect_identical(value$runtime, runtime)
  expect_identical(value$summary, gx_package_report_summary_impl(hydrated))
  expect_identical(calls$path, cli_path)
  expect_identical(calls$timeout, 30)
  expect_match(basename(calls$input), "^report[.]qmd$")
  expect_identical(calls$source, value$source$bytes)
  expect_match(rawToChar(value$source$bytes), "enabled: false", fixed = TRUE)
  expect_match(rawToChar(value$source$bytes), "cache: false", fixed = TRUE)
  expect_identical(value$output$structure$scripts, 0L)
  expect_identical(value$output$structure$external_references, 0L)
  expect_false(value$metadata$execution_enabled)
  expect_false(value$metadata$cache)
  expect_true(value$metadata$minimal)
  expect_true(value$metadata$embed_resources)
  expect_true(value$metadata$closed_output_tree)
  expect_true(value$metadata$temporary_stage_removed)
  expect_false(value$metadata$package_integrated)
  expect_false(value$metadata$public)
  expect_false(value$metadata$replayable)
  expect_identical(list.files(stage_parent, all.files = TRUE, no.. = TRUE),
                   character())
  expect_identical(gx_package_report_validate_impl(value), invisible(value))
})

test_that("M9y fixes exact CLI render controls", {
  input <- "/tmp/example/report.qmd"
  expect_identical(
    gx_package_report_render_arguments_impl(input),
    c(
      "render", input, "--to", "html", "--output", "report.html",
      "--no-execute", "--no-cache", "--quiet"
    )
  )
  hydrated <- package_report_test_hydrated()
  source <- rawToChar(gx_package_report_source_impl(
    hydrated, gx_package_report_summary_impl(hydrated)
  )$bytes)
  expect_match(source, "minimal: true", fixed = TRUE)
  expect_match(source, "embed-resources: true", fixed = TRUE)
  expect_false(grepl("```", source, fixed = TRUE))
})

test_that("M9y rejects active or externally linked HTML and cleans its stage", {
  hydrated <- package_report_test_hydrated()
  cli_path <- package_report_test_cli_file()
  runtime <- package_report_test_runtime(cli_path)
  cases <- list(
    script = function(html) sub("</body>", "<script>bad()</script></body>", html,
                               fixed = TRUE),
    external = function(html) sub(
      "</body>", "<a href=\"https://example.org\">remote</a></body>", html,
      fixed = TRUE
    ),
    active_uri = function(html) sub(
      "</body>", "<a href=\"javascript:bad()\">active</a></body>", html,
      fixed = TRUE
    ),
    refresh = function(html) sub(
      "<head>", "<head><meta http-equiv=\"refresh\" content=\"0\">", html,
      fixed = TRUE
    ),
    forged = function(html) sub(
      hydrated$hydration_id, paste(rep("0", 64L), collapse = ""), html,
      fixed = TRUE
    )
  )
  for (name in names(cases)) {
    stage_parent <- withr::local_tempdir(
      pattern = paste0("gx-package-report-", name, "-")
    )
    expect_error(
      gx_package_report_impl(
        hydrated,
        runtime_resolver = function() runtime,
        render_resolver = package_report_test_renderer(
          hydrated, transform = cases[[name]]
        ),
        temp_parent = stage_parent
      ),
      class = "gx_error_package_report_html",
      info = name
    )
    expect_identical(
      list.files(stage_parent, all.files = TRUE, no.. = TRUE),
      character(),
      info = name
    )
  }
})

test_that("M9y rejects unexpected render artifacts and source mutation", {
  hydrated <- package_report_test_hydrated()
  cli_path <- package_report_test_cli_file()
  runtime <- package_report_test_runtime(cli_path)
  stage_parent <- withr::local_tempdir(pattern = "gx-package-report-extra-")
  extra_renderer <- package_report_test_renderer(hydrated)
  expect_error(
    gx_package_report_impl(
      hydrated,
      runtime_resolver = function() runtime,
      render_resolver = function(path, input, timeout) {
        extra_renderer(path, input, timeout)
        writeLines("unexpected", file.path(dirname(input), "extra.txt"))
      },
      temp_parent = stage_parent
    ),
    class = "gx_error_package_report_inventory"
  )
  expect_identical(list.files(stage_parent, all.files = TRUE, no.. = TRUE),
                   character())

  stage_parent <- withr::local_tempdir(pattern = "gx-package-report-source-")
  expect_error(
    gx_package_report_impl(
      hydrated,
      runtime_resolver = function() runtime,
      render_resolver = function(path, input, timeout) {
        writeLines("changed", input)
        package_report_test_renderer(hydrated)(path, input, timeout)
      },
      temp_parent = stage_parent
    ),
    class = "gx_error_package_report_race"
  )
  expect_identical(list.files(stage_parent, all.files = TRUE, no.. = TRUE),
                   character())
})

test_that("M9y report evidence fails closed under forgery", {
  hydrated <- package_report_test_hydrated()
  cli_path <- package_report_test_cli_file()
  runtime <- package_report_test_runtime(cli_path)
  value <- gx_package_report_impl(
    hydrated,
    runtime_resolver = function() runtime,
    render_resolver = package_report_test_renderer(hydrated)
  )
  mutations <- list(
    stage = function(x) {
      x$stage <- "fetched"
      x
    },
    hydration = function(x) {
      x$hydrated$hydration_id <- paste(rep("0", 64L), collapse = "")
      x
    },
    runtime = function(x) {
      x$runtime$cli_version <- "1.8.28"
      x
    },
    summary = function(x) {
      x$summary$sites <- x$summary$sites + 1L
      x
    },
    source = function(x) {
      x$source$bytes[[1L]] <- as.raw(0L)
      x
    },
    output = function(x) {
      x$output$bytes[[1L]] <- as.raw(0L)
      x
    },
    metadata = function(x) {
      x$metadata$public <- TRUE
      x
    },
    identity = function(x) {
      x$report_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(value, NULL)))
    expect_error(
      gx_package_report_validate_impl(forged),
      class = "gx_error_package_report",
      info = name
    )
  }
})

test_that("M9y remains internal and performs no network, cache, or publication", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_package_report_impl", "gx_package_report_validate_impl",
    "gx_package_report_render_command_impl"
  ) %in% exports))

  hydrated <- package_report_test_hydrated()
  cli_path <- package_report_test_cli_file()
  runtime <- package_report_test_runtime(cli_path)
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or publication seam", call. = FALSE)
  }
  expect_no_error(testthat::with_mocked_bindings(
    gx_package_report_impl(
      hydrated,
      runtime_resolver = function() runtime,
      render_resolver = package_report_test_renderer(hydrated)
    ),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    gx_package_writer_rename = blocked,
    gx_snapshot_writer_rename = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})
