package_load_test_parent <- function(.local_envir = parent.frame()) {
  withr::local_tempdir(
    pattern = "gx-package-load-parent-",
    .local_envir = .local_envir
  )
}

package_load_test_inputs <- function() {
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

test_that("M9n loads exact package bytes for every public source stage", {
  inputs <- package_load_test_inputs()
  parent <- package_load_test_parent()
  loaded <- list()
  for (stage in names(inputs)) {
    target <- file.path(parent, stage)
    created <- gx_package(
      inputs[[stage]],
      target,
      catalog = if (identical(stage, "catalog")) NULL else inputs$catalog
    )
    result <- gx_package_load(target)

    expect_s3_class(result, "gx_package_loaded")
    expect_identical(result$stage, stage)
    expect_identical(result$status, "loaded_and_verified")
    expect_identical(result$path, created$path)
    expect_identical(
      result$verification$manifest_sha256,
      created$verification$manifest_sha256
    )
    expect_identical(names(result$contents), result$resources$path)
    expect_identical(
      as.numeric(vapply(result$contents, length, integer(1))),
      result$resources$bytes
    )
    expect_identical(
      unname(vapply(
        result$contents,
        digest::digest,
        character(1),
        algo = "sha256",
        serialize = FALSE
      )),
      result$resources$sha256
    )
    expect_true(result$metadata$read_only)
    expect_true(result$metadata$byte_preserving)
    expect_false(result$metadata$parses_tables)
    expect_false(result$metadata$authenticity)
    expect_false(result$metadata$frictionless)
    expect_false(result$metadata$replayable)
    expect_identical(
      gx_package_loaded_validate_impl(result),
      invisible(result)
    )
    loaded[[stage]] <- result
  }
  expect_true(
    loaded$harmonized$metadata$resources >
      loaded$catalog$metadata$resources
  )
})

test_that("M9n resource contents equal the stored files byte for byte", {
  inputs <- package_load_test_inputs()
  parent <- package_load_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$harmonized, target, catalog = inputs$catalog)

  loaded <- gx_package_load(target)

  for (path in loaded$resources$path) {
    file <- file.path(target, path)
    expect_identical(
      loaded$contents[[path]],
      readBin(file, what = "raw", n = file.info(file)$size)
    )
  }
})

test_that("M9n rejects non-package snapshots and corrupted package bytes", {
  inputs <- package_load_test_inputs()
  parent <- package_load_test_parent()
  snapshot <- file.path(parent, "snapshot")
  package <- file.path(parent, "package")
  gx_snapshot(inputs$catalog, snapshot)
  gx_package(inputs$catalog, package)

  expect_error(
    gx_package_load(snapshot),
    class = "gx_error_package_load"
  )

  verification <- gx_snapshot_verify(package)
  path <- verification$resources$path[[1L]]
  file <- file.path(package, path)
  bytes <- readBin(file, what = "raw", n = file.info(file)$size)
  bytes[[length(bytes)]] <- as.raw(bitwXor(
    as.integer(bytes[[length(bytes)]]),
    1L
  ))
  writeBin(bytes, file)
  expect_error(
    gx_package_load(package),
    class = "gx_error_package_load"
  )
})

test_that("M9n fails closed when a package changes during loading", {
  inputs <- package_load_test_inputs()
  parent <- package_load_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$harmonized, target, catalog = inputs$catalog)
  verification <- gx_snapshot_verify(target)
  mutate_path <- verification$resources$path[[nrow(verification$resources)]]
  original <- gx_package_load_read_impl
  calls <- 0L

  expect_error(testthat::with_mocked_bindings(
    gx_package_load(target),
    gx_package_load_read_impl = function(root, resource) {
      bytes <- original(root, resource)
      calls <<- calls + 1L
      if (calls == 1L) {
        path <- file.path(root, mutate_path)
        value <- readBin(path, what = "raw", n = file.info(path)$size)
        value[[1L]] <- as.raw(bitwXor(as.integer(value[[1L]]), 1L))
        writeBin(value, path)
      }
      bytes
    },
    .package = "geoconnexr"
  ), class = "gx_error_package_load")
})

test_that("M9n loaded evidence fails closed under forgery", {
  inputs <- package_load_test_inputs()
  parent <- package_load_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$harmonized, target, catalog = inputs$catalog)
  loaded <- gx_package_load(target)
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
    resources = function(x) {
      x$resources$bytes[[1L]] <- x$resources$bytes[[1L]] + 1
      x
    },
    contents = function(x) {
      x$contents[[1L]][[1L]] <- as.raw(bitwXor(
        as.integer(x$contents[[1L]][[1L]]),
        1L
      ))
      x
    },
    metadata = function(x) {
      x$metadata$replayable <- TRUE
      x
    },
    identity = function(x) {
      x$load_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(loaded, NULL)))
    expect_error(
      gx_package_loaded_validate_impl(forged),
      class = "gx_error_package_load",
      info = name
    )
  }
})

test_that("M9n loading performs no external or write work", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or write seam", call. = FALSE)
  }
  inputs <- package_load_test_inputs()
  parent <- package_load_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$catalog, target)

  expect_no_error(testthat::with_mocked_bindings(
    gx_package_load(target),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    gx_package_writer_write_raw = blocked,
    gx_snapshot_writer_write_raw = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9n exports only its public loader and print method", {
  exports <- getNamespaceExports("geoconnexr")
  expect_true(all(c("gx_package", "gx_package_load") %in% exports))
  expect_false(any(c(
    "gx_package_load_read_impl", "gx_package_load_resource_profile_impl",
    "gx_package_loaded_validate_impl", "gx_package_load_metadata_impl"
  ) %in% exports))
})
