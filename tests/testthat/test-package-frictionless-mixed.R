package_frictionless_mixed_fetched <- function() {
  performer <- fetch_orchestration_test_performer()
  oaf_test_options(performer)
  limits <- gx_fetch_public_limits_impl()
  limits$max_response_bytes <- 20000L
  limits$max_rows <- 10000L
  limits$max_columns <- 100L
  limits$max_fields <- 1000L
  limits$max_executions <- 7L
  limits$max_total_bytes <- 140000
  limits$oaf_limit <- 2L
  limits$timeout <- 15
  limits$min_interval <- 0
  gx_fetch_impl(
    plan = fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    limits = limits,
    orchestration_scope_id = fetch_orchestration_test_scope(
      "package-frictionless-mixed"
    ),
    oaf_symbol_resolver = oaf_test_resolver(),
    wqp_symbol_resolver = wqp_test_resolver(),
    edr_symbol_resolver = edr_test_resolver(),
    usgs_continuous_symbol_resolver = usgs_continuous_test_resolver(),
    usgs_daily_symbol_resolver = usgs_daily_test_resolver()
  )
}

package_frictionless_mixed_quarto_cli <- function(
    .local_envir = parent.frame()) {
  path <- withr::local_tempfile(
    pattern = "gx-package-frictionless-mixed-quarto-",
    .local_envir = .local_envir
  )
  writeLines(c("#!/bin/sh", "exit 0"), path, useBytes = TRUE)
  Sys.chmod(path, mode = "0755")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

package_frictionless_mixed_runtime <- function(path) {
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

package_frictionless_mixed_report_html <- function(hydrated, summary) {
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

package_frictionless_mixed_report_bundle <- function(
    catalog,
    parent,
    .local_envir = parent.frame()) {
  base <- gx_package_resources_impl(gx_package_input_impl(catalog))
  base_path <- file.path(parent, "report-source")
  gx_package_write_impl(base, base_path)
  hydrated <- gx_package_hydrate(base_path)
  runtime <- package_frictionless_mixed_runtime(
    package_frictionless_mixed_quarto_cli(.local_envir)
  )
  summary <- gx_package_report_summary_impl(hydrated)
  renderer <- function(path, input, timeout) {
    writeBin(
      charToRaw(package_frictionless_mixed_report_html(hydrated, summary)),
      file.path(dirname(input), "report.html")
    )
    character()
  }
  report <- gx_package_report_impl(
    hydrated,
    runtime_resolver = function() runtime,
    render_resolver = renderer
  )
  gx_package_report_resources_impl(base, report)
}

test_that("M9ae mixed bundles pass the pinned opaque-file CLI profile", {
  cli <- Sys.getenv("GEOCONNEXR_FRICTIONLESS_CLI", unset = "")
  skip_if(!nzchar(cli), "Pinned Frictionless CLI is not configured")
  expect_true(file.exists(cli))
  expect_identical(
    unname(system2(cli, "--version", stdout = TRUE, stderr = TRUE)),
    "5.19.0"
  )
  expect_true(requireNamespace("arrow", quietly = TRUE))
  expect_true(utils::compareVersion(
    as.character(utils::packageVersion("arrow")), "14.0.0"
  ) >= 0L)

  parent <- withr::local_tempdir(pattern = "gx-frictionless-mixed-")
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- package_frictionless_mixed_fetched()
  harmonized <- gx_harmonize(fetched)
  bundles <- list(
    raw = gx_package_resources_impl(
      gx_package_input_impl(fetched, catalog)
    ),
    parquet = gx_package_resources_impl(
      gx_package_input_impl(harmonized, catalog),
      timeseries = "parquet"
    ),
    report = package_frictionless_mixed_report_bundle(
      catalog, parent, environment()
    )
  )

  for (name in names(bundles)) {
    bundle <- bundles[[name]]
    profile <- gx_package_frictionless_impl(bundle)
    opaque <- bundle$resources$format != "csv"
    descriptor_resources <- profile$descriptor$resources[opaque]
    opaque_paths <- bundle$resources$path[opaque]

    expect_true(any(opaque), info = name)
    expect_true(profile$metadata$mixed_resource_profiles, info = name)
    expect_identical(
      profile$metadata$opaque_resources,
      unname(as.integer(sum(opaque))),
      info = name
    )
    expect_true(all(vapply(descriptor_resources, function(resource) {
      identical(resource$profile, "data-resource") &&
        identical(resource$format, "bin") &&
        identical(
          resource$geoconnexr$validation,
          .gx_package_frictionless_opaque_profile
        )
    }, logical(1))), info = name)
    expect_identical(
      vapply(
        descriptor_resources,
        function(resource) resource$geoconnexr$format,
        character(1)
      ),
      unname(vapply(opaque_paths, function(path) {
        extension <- tolower(tools::file_ext(path))
        if (nzchar(extension)) extension else "bin"
      }, character(1))),
      info = name
    )

    target <- file.path(parent, paste0("validate-", name))
    publication <- gx_package_write_impl(bundle, target)
    gx_snapshot_writer_write_raw(
      file.path(publication$path, "datapackage.json"), profile$bytes
    )
    output <- withr::with_dir(publication$path, system2(
      cli,
      c("validate", "datapackage.json", "--standards", "v1", "--json"),
      stdout = TRUE,
      stderr = TRUE
    ))
    expect_null(attr(output, "status"), info = name)
    report <- jsonlite::fromJSON(paste(output, collapse = "\n"))
    expect_true(report$valid, info = name)
    expect_identical(report$stats$tasks, nrow(bundle$resources), info = name)
    expect_identical(report$stats$errors, 0L, info = name)
    expect_identical(report$stats$warnings, 0L, info = name)
    expect_true(all(report$tasks$valid), info = name)
    expect_true(all(
      report$tasks$type[match(opaque_paths, report$tasks$place)] == "file"
    ), info = name)
  }
})

test_that("M9af publicly publishes retained raw and Parquet descriptors", {
  skip_if_not_installed("arrow", minimum_version = "14.0.0")
  parent <- withr::local_tempdir(pattern = "gx-frictionless-public-mixed-")
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- package_frictionless_mixed_fetched()
  harmonized <- gx_harmonize(fetched)
  cases <- list(
    raw = list(x = fetched, catalog = catalog, timeseries = "csv"),
    parquet = list(
      x = harmonized, catalog = catalog, timeseries = "parquet"
    )
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    target <- file.path(parent, name)
    package <- gx_package(
      case$x,
      target,
      catalog = case$catalog,
      timeseries = case$timeseries,
      frictionless = TRUE
    )
    descriptor <- gx_snapshot_parse_json(readBin(
      file.path(target, "datapackage.json"),
      what = "raw",
      n = .gx_package_frictionless_max_bytes + 1L
    ))
    opaque <- Filter(function(resource) {
      identical(resource$profile, "data-resource")
    }, descriptor$resources)
    replay <- gx_replay(target)
    hydrated <- replay$view
    tables <- hydrated$table_view
    loaded <- tables$loaded

    expect_true(package$metadata$frictionless, info = name)
    expect_identical(package$metadata$timeseries, case$timeseries, info = name)
    expect_identical(package$metadata$arrow, identical(name, "parquet"))
    expect_true(length(opaque) >= 1L, info = name)
    expect_true(all(vapply(opaque, function(resource) {
      identical(resource$format, "bin") &&
        identical(
          resource$geoconnexr$validation,
          .gx_package_frictionless_opaque_profile
        )
    }, logical(1))), info = name)
    expect_true(loaded$metadata$frictionless, info = name)
    expect_true(tables$metadata$frictionless, info = name)
    expect_true(hydrated$metadata$frictionless, info = name)
    expect_true(replay$metadata$frictionless, info = name)
    expect_false(replay$metadata$recipe_executed, info = name)
  }
})

test_that("M9ae remains an internal validation gate", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_package_frictionless_impl",
    "gx_package_frictionless_resource_impl"
  ) %in% exports))
})
