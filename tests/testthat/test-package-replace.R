package_replace_test_parent <- function(.local_envir = parent.frame()) {
  withr::local_tempdir(
    pattern = "gx-package-replace-parent-",
    .local_envir = .local_envir
  )
}

package_replace_test_bundles <- function() {
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  list(
    prior = gx_package_resources_impl(gx_package_input_impl(catalog)),
    replacement = gx_package_resources_impl(
      gx_package_input_impl(gx_harmonize(fetched), catalog)
    )
  )
}

package_replace_test_temp_paths <- function(parent) {
  list.files(
    parent,
    pattern = "^\\.gx-package-(?:prepared-replacement|prior-recovery|failed-replacement)-",
    all.files = TRUE,
    full.names = TRUE
  )
}

test_that("M9r replaces only a verified fixed-profile package", {
  bundles <- package_replace_test_bundles()
  parent <- package_replace_test_parent()
  target <- file.path(parent, "package")
  prior <- gx_package_write_impl(bundles$prior, target)
  external_calls <- 0L
  blocked <- function(...) {
    external_calls <<- external_calls + 1L
    stop("blocked external seam", call. = FALSE)
  }

  replacement <- testthat::with_mocked_bindings(
    gx_package_replace_impl(bundles$replacement, target),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    .package = "geoconnexr"
  )

  expect_s3_class(replacement, "gx_package_replacement")
  expect_identical(replacement$status, "replaced_and_verified")
  expect_identical(replacement$stage, "harmonized")
  expect_identical(replacement$path, prior$path)
  expect_identical(
    replacement$previous$manifest_sha256,
    prior$verification$manifest_sha256
  )
  expect_identical(
    replacement$verification$manifest$effective_options$serialization$bundle_id,
    bundles$replacement$bundle_id
  )
  expect_true(replacement$metadata$overwrite)
  expect_identical(
    replacement$metadata$ownership_profile,
    "fixed-package-writer-v0.2"
  )
  expect_identical(
    replacement$metadata$rollback_strategy,
    "verified_sibling_backup_v1"
  )
  expect_false(replacement$metadata$backup_retained)
  expect_false(replacement$metadata$authenticity)
  expect_false(replacement$metadata$replayable)
  expect_identical(external_calls, 0L)
  expect_length(package_replace_test_temp_paths(parent), 0L)
  expect_identical(
    gx_package_replacement_validate_impl(replacement),
    invisible(replacement)
  )
})

test_that("M9r refuses unowned, malformed, and corrupt destinations", {
  bundles <- package_replace_test_bundles()
  parent <- package_replace_test_parent()

  missing <- file.path(parent, "missing")
  expect_error(
    gx_package_replace_impl(bundles$replacement, missing),
    class = "gx_error_package_replace_ownership"
  )

  arbitrary <- file.path(parent, "arbitrary")
  dir.create(arbitrary)
  marker <- file.path(arbitrary, "keep")
  writeLines("user-owned", marker)
  expect_error(
    gx_package_replace_impl(bundles$replacement, arbitrary),
    class = "gx_error_package_replace_ownership"
  )
  expect_identical(readLines(marker), "user-owned")

  corrupt <- file.path(parent, "corrupt")
  written <- gx_package_write_impl(bundles$prior, corrupt)
  resource <- file.path(corrupt, written$bundle$resources$path[[1L]])
  bytes <- readBin(resource, what = "raw", n = file.info(resource)$size)
  bytes[[1L]] <- as.raw(bitwXor(as.integer(bytes[[1L]]), 1L))
  writeBin(bytes, resource)
  expect_error(
    gx_package_replace_impl(bundles$replacement, corrupt),
    class = "gx_error_package_replace_ownership"
  )
  expect_length(package_replace_test_temp_paths(parent), 0L)
})

test_that("M9r restores the prior package when installation fails", {
  bundles <- package_replace_test_bundles()
  parent <- package_replace_test_parent()
  target <- file.path(parent, "package")
  prior <- gx_package_write_impl(bundles$prior, target)
  calls <- 0L
  real_rename <- gx_package_replace_rename

  condition <- expect_error(testthat::with_mocked_bindings(
    gx_package_replace_impl(bundles$replacement, target),
    gx_package_replace_rename = function(from, to) {
      calls <<- calls + 1L
      if (calls == 2L) return(FALSE)
      real_rename(from, to)
    },
    .package = "geoconnexr"
  ), class = "gx_error_package_replace_io")

  expect_true(condition$rollback_restored)
  expect_identical(calls, 3L)
  expect_identical(
    gx_snapshot_verify(target)$manifest_sha256,
    prior$verification$manifest_sha256
  )
  expect_length(package_replace_test_temp_paths(parent), 0L)
})

test_that("M9r retains explicit recovery paths when rollback fails", {
  bundles <- package_replace_test_bundles()
  parent <- package_replace_test_parent()
  target <- file.path(parent, "package")
  gx_package_write_impl(bundles$prior, target)
  calls <- 0L
  real_rename <- gx_package_replace_rename

  condition <- expect_error(testthat::with_mocked_bindings(
    gx_package_replace_impl(bundles$replacement, target),
    gx_package_replace_rename = function(from, to) {
      calls <<- calls + 1L
      if (calls == 1L) return(real_rename(from, to))
      FALSE
    },
    .package = "geoconnexr"
  ), class = "gx_error_package_replace_recovery")

  expect_false(condition$rollback_restored)
  expect_false(dir.exists(target))
  expect_true(dir.exists(condition$recovery_path))
  expect_true(dir.exists(condition$prepared_path))
  expect_s3_class(gx_snapshot_verify(condition$recovery_path),
                  "gx_snapshot_verification")
  expect_s3_class(gx_snapshot_verify(condition$prepared_path),
                  "gx_snapshot_verification")
})

test_that("M9r rolls back a replacement that fails final verification", {
  bundles <- package_replace_test_bundles()
  parent <- package_replace_test_parent()
  target <- file.path(parent, "package")
  prior <- gx_package_write_impl(bundles$prior, target)
  calls <- 0L
  real_verify <- gx_package_replace_verify

  condition <- expect_error(testthat::with_mocked_bindings(
    gx_package_replace_impl(bundles$replacement, target),
    gx_package_replace_verify = function(path) {
      calls <<- calls + 1L
      if (calls == 3L) stop("injected final verification failure")
      real_verify(path)
    },
    .package = "geoconnexr"
  ), class = "gx_error_package_replace_verification")

  expect_true(condition$rollback_restored)
  expect_identical(calls, 4L)
  expect_identical(
    gx_snapshot_verify(target)$manifest_sha256,
    prior$verification$manifest_sha256
  )
  expect_length(package_replace_test_temp_paths(parent), 0L)
})

test_that("M9r replacement evidence fails closed under forgery", {
  bundles <- package_replace_test_bundles()
  parent <- package_replace_test_parent()
  target <- file.path(parent, "package")
  gx_package_write_impl(bundles$prior, target)
  replacement <- gx_package_replace_impl(bundles$replacement, target)
  mutations <- list(
    stage = function(x) {
      x$stage <- "catalog"
      x
    },
    bundle = function(x) {
      x$bundle$metadata$publishes <- TRUE
      x
    },
    previous = function(x) {
      x$previous$manifest_sha256 <- paste(rep("0", 64L), collapse = "")
      x
    },
    verification = function(x) {
      x$verification$manifest_sha256 <- paste(rep("0", 64L), collapse = "")
      x
    },
    metadata = function(x) {
      x$metadata$backup_retained <- TRUE
      x
    },
    identity = function(x) {
      x$replacement_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(replacement, NULL)))
    expect_error(
      gx_package_replacement_validate_impl(forged),
      class = "gx_error_package_replace",
      info = name
    )
  }
})

test_that("M9r replacement internals remain unexported", {
  expect_false(any(c(
    "gx_package_replace_impl", "gx_package_replacement_validate_impl",
    "gx_package_replace_owned_impl"
  ) %in% getNamespaceExports("geoconnexr")))
})

test_that("M9r reports retained backup cleanup after a committed replacement", {
  condition <- expect_error(testthat::with_mocked_bindings(
    gx_package_replace_cleanup_impl("unused-recovery-path", committed = TRUE),
    gx_package_replace_cleanup = function(path) FALSE,
    .package = "geoconnexr"
  ), class = "gx_error_package_replace_cleanup")
  expect_true(condition$replacement_committed)
  expect_match(condition$recovery_path, "unused-recovery-path$", perl = TRUE)
})
