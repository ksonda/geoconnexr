package_hydrate_test_parent <- function(.local_envir = parent.frame()) {
  withr::local_tempdir(
    pattern = "gx-package-hydrate-parent-",
    .local_envir = .local_envir
  )
}

package_hydrate_test_inputs <- function() {
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

test_that("M9p hydrates fixed package schemas for every source stage", {
  inputs <- package_hydrate_test_inputs()
  parent <- package_hydrate_test_parent()
  hydrated <- list()
  for (stage in names(inputs)) {
    target <- file.path(parent, stage)
    gx_package(
      inputs[[stage]],
      target,
      catalog = if (identical(stage, "catalog")) NULL else inputs$catalog
    )
    value <- gx_package_hydrate_impl(target)

    expect_s3_class(value, "gx_package_hydrated")
    expect_identical(value$stage, stage)
    expect_identical(value$status, "hydrated_and_verified")
    expect_s3_class(value$catalog$sites, "sf")
    expect_s3_class(value$catalog$datasets, "tbl_df")
    expect_s3_class(value$catalog$problems, "tbl_df")
    expect_s3_class(value$catalog$requests, "tbl_df")
    expect_identical(is.null(value$fetch), identical(stage, "catalog"))
    expect_identical(
      is.null(value$harmonized),
      !identical(stage, "harmonized")
    )
    expect_true(value$metadata$offline)
    expect_true(value$metadata$read_only)
    expect_true(value$metadata$typed_package_tables)
    expect_true(value$metadata$redacted)
    expect_false(value$metadata$native_payloads_typed)
    expect_false(value$metadata$reconstructed_objects)
    expect_false(value$metadata$authenticity)
    expect_false(value$metadata$frictionless)
    expect_false(value$metadata$replayable)
    expect_identical(
      gx_package_hydrated_validate_impl(value),
      invisible(value)
    )
    hydrated[[stage]] <- value
  }
  expect_identical(hydrated$catalog$metadata$fetch_tables, 0L)
  expect_identical(hydrated$fetched$metadata$fetch_tables, 2L)
  expect_identical(hydrated$harmonized$metadata$harmonized_tables, 2L)
})

test_that("M9p applies only exact fixed storage types", {
  status <- tibble::tibble(
    contract_version = "0.1.0",
    selection_order = "1",
    fetch_order = "",
    distribution_id = "distribution",
    handler_id = "csv",
    status = "dry_run",
    attempted = "false",
    succeeded = "false",
    physical_attempts = "0",
    encoded_bytes = "0",
    decoded_bytes = "0",
    execution_id = "",
    result_index = "",
    result_id = "",
    error_code = ""
  )
  typed <- gx_package_hydrate_fetch_status_impl(status)
  expect_type(typed$selection_order, "integer")
  expect_type(typed$fetch_order, "integer")
  expect_true(is.na(typed$fetch_order[[1L]]))
  expect_type(typed$attempted, "logical")
  expect_type(typed$encoded_bytes, "double")

  malformed <- status
  malformed$selection_order <- "01"
  expect_error(
    gx_package_hydrate_fetch_status_impl(malformed),
    class = "gx_error_package_hydrate_type"
  )
  malformed <- status
  malformed$attempted <- "TRUE"
  expect_error(
    gx_package_hydrate_fetch_status_impl(malformed),
    class = "gx_error_package_hydrate_type"
  )
  malformed <- status
  malformed$encoded_bytes <- "1.0"
  expect_error(
    gx_package_hydrate_fetch_status_impl(malformed),
    class = "gx_error_package_hydrate_type"
  )
})

test_that("M9p hydration evidence fails closed under forgery", {
  inputs <- package_hydrate_test_inputs()
  parent <- package_hydrate_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$harmonized, target, catalog = inputs$catalog)
  value <- gx_package_hydrate_impl(target)
  mutations <- list(
    stage = function(x) {
      x$stage <- "catalog"
      x
    },
    table_view = function(x) {
      x$table_view$view_id <- paste(rep("0", 64L), collapse = "")
      x
    },
    catalog = function(x) {
      x$catalog$datasets$dataset_name[[1L]] <- "forged"
      x
    },
    fetch = function(x) {
      x$fetch$status$status[[1L]] <- "forged"
      x
    },
    metadata = function(x) {
      x$metadata$replayable <- TRUE
      x
    },
    identity = function(x) {
      x$hydration_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(value, NULL)))
    expect_error(
      gx_package_hydrated_validate_impl(forged),
      class = "gx_error_package_hydrate",
      info = name
    )
  }
})

test_that("M9p hydration performs no external or write work", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or write seam", call. = FALSE)
  }
  inputs <- package_hydrate_test_inputs()
  parent <- package_hydrate_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$catalog, target)

  expect_no_error(testthat::with_mocked_bindings(
    gx_package_hydrate_impl(target),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    gx_package_writer_write_raw = blocked,
    gx_snapshot_writer_write_raw = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9q exposes only the public typed hydration wrapper", {
  inputs <- package_hydrate_test_inputs()
  parent <- package_hydrate_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$catalog, target)

  value <- gx_package_hydrate(target)
  expect_s3_class(value, "gx_package_hydrated")
  expect_identical(
    gx_package_hydrated_validate_impl(value),
    invisible(value)
  )
  expect_message(
    printed <- print(value),
    "redacted; read-only; non-replayable"
  )
  expect_identical(printed, value)

  exports <- getNamespaceExports("geoconnexr")
  expect_true("gx_package_hydrate" %in% exports)
  expect_false(any(c(
    "gx_package_hydrate_impl", "gx_package_hydrated_validate_impl",
    "gx_package_hydrate_projection_impl"
  ) %in% exports))
})

test_that("M9q public hydration preserves blocked-work guarantees", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or write seam", call. = FALSE)
  }
  inputs <- package_hydrate_test_inputs()
  parent <- package_hydrate_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$catalog, target)

  expect_no_error(testthat::with_mocked_bindings(
    gx_package_hydrate(target),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    gx_package_writer_write_raw = blocked,
    gx_snapshot_writer_write_raw = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})
