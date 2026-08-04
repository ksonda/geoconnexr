package_public_test_parent <- function(.local_envir = parent.frame()) {
  withr::local_tempdir(
    pattern = "gx-package-public-parent-",
    .local_envir = .local_envir
  )
}

package_public_test_inputs <- function() {
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  harmonized <- gx_harmonize(fetched)
  list(
    catalog = catalog,
    fetched = fetched,
    harmonized = harmonized
  )
}

test_that("M9m publicly packages catalog, fetched, and harmonized stages", {
  inputs <- package_public_test_inputs()
  parent <- package_public_test_parent()
  packages <- list()
  for (stage in names(inputs)) {
    target <- file.path(parent, stage)
    package <- gx_package(
      inputs[[stage]],
      target,
      catalog = if (identical(stage, "catalog")) NULL else inputs$catalog
    )
    expect_s3_class(package, "gx_package")
    expect_identical(package$stage, stage)
    expect_identical(package$status, "written_and_verified")
    expect_identical(package$metadata$source_stage, stage)
    expect_true(package$metadata$creation_only)
    expect_identical(package$metadata$timeseries, "csv")
    expect_true(package$metadata$keep_raw)
    expect_false(package$metadata$overwrite)
    expect_false(package$metadata$report)
    expect_false(package$metadata$arrow)
    expect_false(package$metadata$quarto)
    expect_false(package$metadata$frictionless)
    expect_false(package$metadata$replayable)
    expect_identical(package$metadata$request_ledger_scope, "catalog_only")
    expect_null(package$previous)
    expect_identical(
      package$verification$manifest$recipe$pipeline$end_stage,
      "package"
    )
    expect_false(package$verification$manifest$replay$replayable)
    expect_true(dir.exists(target))
    expect_identical(
      gx_snapshot_verify(target)$manifest_sha256,
      package$verification$manifest_sha256
    )
    expect_identical(gx_package_validate_impl(package), invisible(package))
    packages[[stage]] <- package
  }
  expect_true(
    packages$harmonized$metadata$resources >
      packages$catalog$metadata$resources
  )
})

test_that("M9m rejects unsupported scope before filesystem publication", {
  inputs <- package_public_test_inputs()
  parent <- package_public_test_parent()
  cases <- list(
    unsupported = list(timeseries = "feather"),
    all_formats = list(timeseries = c("csv", "parquet")),
    drop_raw = list(keep_raw = FALSE),
    invalid_overwrite = list(overwrite = NA),
    missing_catalog = list(x = inputs$fetched),
    catalog_with_catalog = list(
      x = inputs$catalog,
      catalog = inputs$catalog
    )
  )
  for (name in names(cases)) {
    target <- file.path(parent, name)
    arguments <- list(x = inputs$catalog, dir = target)
    for (argument in names(cases[[name]])) {
      arguments[[argument]] <- cases[[name]][[argument]]
    }
    expect_error(
      do.call(gx_package, arguments),
      class = "gx_error_package",
      info = name
    )
    expect_false(file.exists(target) || dir.exists(target), info = name)
  }
})

test_that("M9m preserves existing destinations", {
  inputs <- package_public_test_inputs()
  parent <- package_public_test_parent()
  target <- file.path(parent, "existing")
  dir.create(target)
  marker <- file.path(target, "marker")
  writeLines("preserve", marker)

  expect_error(
    gx_package(inputs$catalog, target),
    class = "gx_error_package_write_exists"
  )
  expect_true(file.exists(marker))
})

test_that("M9m public package evidence fails closed under forgery", {
  inputs <- package_public_test_inputs()
  parent <- package_public_test_parent()
  package <- gx_package(
    inputs$harmonized,
    file.path(parent, "package"),
    catalog = inputs$catalog
  )
  mutations <- list(
    stage = function(x) {
      x$stage <- "fetched"
      x
    },
    path = function(x) {
      x$path <- paste0(x$path, "-forged")
      x
    },
    verification = function(x) {
      x$verification$resources$actual_sha256[[1L]] <-
        paste(rep("0", 64L), collapse = "")
      x
    },
    metadata = function(x) {
      x$metadata$replayable <- TRUE
      x
    },
    identity = function(x) {
      x$package_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(package, NULL)))
    expect_error(
      gx_package_validate_impl(forged),
      class = "gx_error_package",
      info = name
    )
  }
})

test_that("M9m package creation performs no external work", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external seam", call. = FALSE)
  }
  inputs <- package_public_test_inputs()
  parent <- package_public_test_parent()

  expect_no_error(testthat::with_mocked_bindings(
    gx_package(inputs$catalog, file.path(parent, "package")),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9s publicly replaces an intact fixed-profile package", {
  inputs <- package_public_test_inputs()
  parent <- package_public_test_parent()
  target <- file.path(parent, "package")
  prior <- gx_package(inputs$catalog, target)
  replacement <- gx_package(
    inputs$harmonized,
    target,
    catalog = inputs$catalog,
    overwrite = TRUE
  )

  expect_s3_class(replacement, "gx_package")
  expect_identical(replacement$mode, "fixed_package_replacement")
  expect_identical(replacement$status, "replaced_and_verified")
  expect_identical(replacement$stage, "harmonized")
  expect_identical(replacement$path, prior$path)
  expect_s3_class(replacement$previous, "gx_snapshot_verification")
  expect_identical(
    replacement$previous$manifest_sha256,
    prior$verification$manifest_sha256
  )
  expect_true(replacement$metadata$overwrite)
  expect_false(replacement$metadata$creation_only)
  expect_identical(
    replacement$metadata$scope,
    "fixed_package_replacement_v1"
  )
  expect_false(identical(replacement$package_id, prior$package_id))
  expect_identical(
    gx_snapshot_verify(target)$manifest_sha256,
    replacement$verification$manifest_sha256
  )
  expect_identical(
    gx_package_validate_impl(replacement),
    invisible(replacement)
  )
  expect_length(list.files(
    parent,
    pattern = "^\\.gx-package-(prepared-replacement|prior-recovery|failed-replacement)-",
    all.files = TRUE
  ), 0L)
})

test_that("M9s public overwrite refuses destinations it does not own", {
  inputs <- package_public_test_inputs()
  parent <- package_public_test_parent()

  missing <- file.path(parent, "missing")
  expect_error(
    gx_package(inputs$catalog, missing, overwrite = TRUE),
    class = "gx_error_package_replace_ownership"
  )
  expect_false(file.exists(missing) || dir.exists(missing))

  arbitrary <- file.path(parent, "arbitrary")
  dir.create(arbitrary)
  marker <- file.path(arbitrary, "marker")
  writeLines("preserve", marker)
  expect_error(
    gx_package(inputs$catalog, arbitrary, overwrite = TRUE),
    class = "gx_error_package_replace_ownership"
  )
  expect_identical(readLines(marker), "preserve")

  corrupt <- file.path(parent, "corrupt")
  package <- gx_package(inputs$catalog, corrupt)
  resource <- file.path(corrupt, package$verification$resources$path[[1L]])
  bytes <- readBin(resource, what = "raw", n = file.info(resource)$size)
  bytes[[1L]] <- as.raw(bitwXor(as.integer(bytes[[1L]]), 1L))
  writeBin(bytes, resource)
  expect_error(
    gx_package(inputs$catalog, corrupt, overwrite = TRUE),
    class = "gx_error_package_replace_ownership"
  )
  expect_identical(
    readBin(resource, what = "raw", n = file.info(resource)$size),
    bytes
  )
})

test_that("M9s public overwrite restores the prior package on install failure", {
  inputs <- package_public_test_inputs()
  parent <- package_public_test_parent()
  target <- file.path(parent, "package")
  prior <- gx_package(inputs$catalog, target)
  calls <- 0L
  real_rename <- gx_package_replace_rename

  condition <- expect_error(testthat::with_mocked_bindings(
    gx_package(
      inputs$harmonized,
      target,
      catalog = inputs$catalog,
      overwrite = TRUE
    ),
    gx_package_replace_rename = function(from, to) {
      calls <<- calls + 1L
      if (calls == 2L) return(FALSE)
      real_rename(from, to)
    },
    .package = "geoconnexr"
  ), class = "gx_error_package_replace_io")

  expect_true(condition$rollback_restored)
  expect_identical(
    gx_snapshot_verify(target)$manifest_sha256,
    prior$verification$manifest_sha256
  )
})

test_that("M9s replacement evidence fails closed under forgery", {
  inputs <- package_public_test_inputs()
  parent <- package_public_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$catalog, target)
  replacement <- gx_package(
    inputs$harmonized,
    target,
    catalog = inputs$catalog,
    overwrite = TRUE
  )
  mutations <- list(
    mode = function(x) {
      x$mode <- "fixed_package_creation"
      x
    },
    previous = function(x) {
      x$previous$manifest_sha256 <- paste(rep("0", 64L), collapse = "")
      x
    },
    overwrite = function(x) {
      x$metadata$overwrite <- FALSE
      x
    },
    identity = function(x) {
      x$package_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(replacement, NULL)))
    expect_error(
      gx_package_validate_impl(forged),
      class = "gx_error_package",
      info = name
    )
  }
})

test_that("M9s public replacement performs no external work", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external seam", call. = FALSE)
  }
  inputs <- package_public_test_inputs()
  parent <- package_public_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$catalog, target)

  expect_no_error(testthat::with_mocked_bindings(
    gx_package(
      inputs$harmonized,
      target,
      catalog = inputs$catalog,
      overwrite = TRUE
    ),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9m exports only the public package boundary", {
  exports <- getNamespaceExports("geoconnexr")
  expect_true("gx_package" %in% exports)
  expect_false(any(c(
    "gx_package_validate_impl", "gx_package_metadata_impl",
    "gx_package_input_impl", "gx_package_resources_impl",
    "gx_package_write_impl", "gx_package_replace_impl",
    "gx_package_replacement_validate_impl"
  ) %in% exports))
})
