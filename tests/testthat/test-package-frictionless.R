package_frictionless_test_bundles <- local({
  bundles <- NULL
  function() {
    if (!is.null(bundles)) return(bundles)
    catalog <- fetch_orchestration_test_usgs_daily_catalog()
    fetched <- gx_fetch(
      fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
      dry_run = TRUE
    )
    harmonized <- gx_harmonize(fetched)
    bundles <<- list(
      catalog = gx_package_resources_impl(gx_package_input_impl(catalog)),
      fetched = gx_package_resources_impl(
        gx_package_input_impl(fetched, catalog)
      ),
      harmonized = gx_package_resources_impl(
        gx_package_input_impl(harmonized, catalog)
      )
    )
    bundles
  }
})

test_that("M9ac describes exact fixed bundles as Data Package v1", {
  bundles <- package_frictionless_test_bundles()
  for (stage in names(bundles)) {
    bundle <- bundles[[stage]]
    value <- gx_package_frictionless_impl(bundle)

    expect_s3_class(value, "gx_package_frictionless")
    expect_identical(value$stage, stage)
    expect_identical(value$status, "described_and_validated")
    expect_identical(value$descriptor$profile, "data-package")
    expect_identical(value$descriptor$name, "geoconnexr-package")
    expect_identical(
      vapply(value$descriptor$resources, `[[`, character(1), "path"),
      bundle$resources$path
    )
    expect_identical(
      vapply(value$descriptor$resources, `[[`, numeric(1), "bytes"),
      bundle$resources$bytes
    )
    expect_identical(
      vapply(value$descriptor$resources, `[[`, character(1), "hash"),
      paste0("sha256:", bundle$resources$sha256)
    )
    expect_true(value$metadata$in_memory)
    expect_true(value$metadata$deterministic)
    expect_true(value$metadata$descriptor_validated)
    expect_true(value$metadata$frictionless)
    expect_false(value$metadata$writes)
    expect_false(value$metadata$publishes)
    expect_false(value$metadata$cli_validated)
    expect_false(value$metadata$replayable)
    expect_identical(
      gx_package_frictionless_validate_impl(value), invisible(value)
    )
    expect_identical(
      gx_package_frictionless_impl(bundle)$profile_id, value$profile_id
    )
  }
})

test_that("M9ac declares canonical CSV schemas as exact strings", {
  bundle <- package_frictionless_test_bundles()$harmonized
  value <- gx_package_frictionless_impl(bundle)
  tabular <- bundle$resources$format == "csv"
  resources <- value$descriptor$resources[tabular]
  tables <- lapply(bundle$contents[tabular], gx_package_table_parse_impl)

  expect_true(all(vapply(resources, function(resource) {
    identical(resource$profile, "tabular-data-resource") &&
      identical(resource$format, "csv") &&
      identical(resource$mediatype, "text/csv") &&
      identical(resource$encoding, "utf-8")
  }, logical(1))))
  for (index in seq_along(resources)) {
    fields <- resources[[index]]$schema$fields
    expect_identical(
      vapply(fields, `[[`, character(1), "name"), names(tables[[index]])
    )
    expect_true(all(vapply(
      fields, function(field) identical(field$type, "string"), logical(1)
    )))
  }
})

test_that("M9ac declares non-CSV bytes as explicit generic resources", {
  content <- charToRaw("<!doctype html><title>report</title>\n")
  entry <- gx_package_resources_entry_impl(
    path = "report/index.html",
    role = "report_html",
    format = "raw",
    media_type = "text/html; charset=utf-8",
    content = content
  )
  resource <- gx_package_frictionless_resource_impl(
    gx_package_resources_table_impl(list(entry)), content
  )
  expect_identical(resource$profile, "data-resource")
  expect_identical(resource$format, "bin")
  expect_identical(resource$mediatype, "text/html")
  expect_identical(resource$encoding, "utf-8")
  expect_identical(resource$geoconnexr, list(
    format = "html", validation = "opaque-file-v1"
  ))
  expect_null(resource$schema)

  parquet <- c(charToRaw("PAR1"), as.raw(c(0L, 0L, 0L, 0L)), charToRaw("PAR1"))
  entry <- gx_package_resources_entry_impl(
    path = "data/observations.parquet",
    role = "observations",
    format = "parquet",
    media_type = "application/vnd.apache.parquet",
    content = parquet
  )
  resource <- gx_package_frictionless_resource_impl(
    gx_package_resources_table_impl(list(entry)), parquet
  )
  expect_identical(resource$profile, "data-resource")
  expect_identical(resource$format, "bin")
  expect_identical(
    resource$mediatype, "application/vnd.apache.parquet"
  )
  expect_identical(resource$geoconnexr, list(
    format = "parquet", validation = "opaque-file-v1"
  ))
  expect_null(resource$encoding)
  expect_null(resource$schema)
})

test_that("M9ac fails closed under byte and evidence forgery", {
  bundle <- package_frictionless_test_bundles()$catalog
  value <- gx_package_frictionless_impl(bundle)

  expect_error(
    gx_package_frictionless_resource_impl(
      bundle$resources[1L, , drop = FALSE], charToRaw("forged")
    ),
    class = "gx_error_package_frictionless_binding"
  )
  mutations <- list(
    descriptor = function(x) {
      x$descriptor$name <- "forged"
      x
    },
    bytes = function(x) {
      x$bytes[[1L]] <- as.raw(0L)
      x
    },
    metadata = function(x) {
      x$metadata$cli_validated <- TRUE
      x
    },
    identity = function(x) {
      x$profile_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    expect_error(
      gx_package_frictionless_validate_impl(mutations[[name]](value)),
      class = "gx_error_package_frictionless_contract",
      info = name
    )
  }
})

test_that("M9ac construction performs no external or filesystem work", {
  bundle <- package_frictionless_test_bundles()$catalog
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked seam", call. = FALSE)
  }
  expect_no_error(testthat::with_mocked_bindings(
    gx_package_frictionless_impl(bundle),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    gx_package_write_impl = blocked,
    gx_snapshot_write_catalog_impl = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9ac profile passes the pinned Frictionless CLI", {
  cli <- Sys.getenv("GEOCONNEXR_FRICTIONLESS_CLI", unset = "")
  skip_if(!nzchar(cli), "Pinned Frictionless CLI is not configured")
  expect_true(file.exists(cli))
  expect_identical(
    unname(system2(cli, "--version", stdout = TRUE, stderr = TRUE)),
    "5.19.0"
  )
  bundles <- package_frictionless_test_bundles()
  parent <- withr::local_tempdir(pattern = "gx-frictionless-cli-")
  for (stage in names(bundles)) {
    bundle <- bundles[[stage]]
    profile <- gx_package_frictionless_impl(bundle)
    target <- file.path(parent, stage)
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
    expect_null(attr(output, "status"), info = stage)
    report <- jsonlite::fromJSON(paste(output, collapse = "\n"))
    expect_true(report$valid, info = stage)
    expect_identical(
      report$stats$tasks, nrow(bundle$resources), info = stage
    )
    expect_identical(report$stats$errors, 0L, info = stage)
    expect_identical(report$stats$warnings, 0L, info = stage)
    expect_true(all(report$tasks$valid), info = stage)
  }
})

test_that("M9ac remains internal", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_package_frictionless_impl",
    "gx_package_frictionless_validate_impl"
  ) %in% exports))
})
