package_write_test_clock <- function() {
  as.POSIXct("2026-07-29T12:00:00Z", tz = "UTC")
}

package_write_test_parent <- function(.local_envir = parent.frame()) {
  withr::local_tempdir(
    pattern = "gx-package-write-parent-",
    .local_envir = .local_envir
  )
}

package_write_test_stage_paths <- function(parent) {
  list.files(
    parent,
    pattern = "^\\.gx-package-stage-",
    all.files = TRUE,
    full.names = TRUE
  )
}

package_write_test_catalog_bundle <- function() {
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  gx_package_resources_impl(gx_package_input_impl(catalog))
}

package_write_test_harmonized_bundle <- function() {
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  harmonized <- gx_harmonize(fetched)
  gx_package_resources_impl(
    gx_package_input_impl(harmonized, catalog)
  )
}

test_that("M9l stages, verifies, and atomically publishes exact bundles", {
  testthat::local_mocked_bindings(
    gx_now = package_write_test_clock,
    .package = "geoconnexr"
  )
  cases <- list(
    catalog = package_write_test_catalog_bundle(),
    harmonized = package_write_test_harmonized_bundle()
  )
  publications <- list()
  for (stage in names(cases)) {
    bundle <- cases[[stage]]
    parent <- package_write_test_parent()
    target <- file.path(parent, paste0("package-", stage))

    publication <- gx_package_write_impl(bundle, target)

    expect_s3_class(publication, "gx_package_publication")
    expect_identical(publication$stage, stage)
    expect_identical(publication$bundle, bundle)
    expect_identical(publication$status, "written_and_verified")
    expect_identical(publication$metadata$end_stage, "package")
    expect_true(publication$metadata$creation_only)
    expect_false(publication$metadata$overwrite)
    expect_true(publication$metadata$manifest)
    expect_false(publication$metadata$arrow)
    expect_false(publication$metadata$quarto)
    expect_false(publication$metadata$report)
    expect_false(publication$metadata$frictionless)
    expect_false(publication$metadata$replayable)
    expect_true(dir.exists(target))
    expect_length(package_write_test_stage_paths(parent), 0L)
    expect_identical(
      publication$verification$manifest$recipe$pipeline$end_stage,
      "package"
    )
    expect_false(publication$verification$manifest$replay$replayable)
    serialization <-
      publication$verification$manifest$effective_options$serialization
    expect_identical(serialization$writer, "fixed-package-writer-v0.2")
    expect_identical(
      serialization$resource_profile,
      "fixed-in-memory-resources-v2"
    )
    expect_identical(serialization$package_input_id, bundle$input$input_id)
    expect_identical(serialization$bundle_id, bundle$bundle_id)
    expect_identical(
      publication$verification$resources$path,
      bundle$resources$path
    )
    expect_identical(
      publication$verification$resources$actual_bytes,
      bundle$resources$bytes
    )
    expect_identical(
      publication$verification$resources$actual_sha256,
      bundle$resources$sha256
    )
    for (path in bundle$resources$path) {
      expect_identical(
        readBin(
          file.path(target, path),
          what = "raw",
          n = file.info(file.path(target, path))$size
        ),
        bundle$contents[[path]]
      )
    }
    public_verification <- gx_snapshot_verify(target)
    expect_identical(
      public_verification$manifest_sha256,
      publication$verification$manifest_sha256
    )
    expect_identical(
      gx_package_publication_validate_impl(publication),
      invisible(publication)
    )
    publications[[stage]] <- publication
  }
  harmonized_recipe <-
    cases$harmonized$input$fetched$plan$distributions
  selected <- which(
    harmonized_recipe$selected & !is.na(harmonized_recipe$fetch_order)
  )
  selected <- selected[order(harmonized_recipe$fetch_order[selected])]
  expected_handlers <- unique(harmonized_recipe$handler_id[selected])
  written_recipe <- publications$harmonized$verification$manifest$recipe
  expect_identical(
    unlist(written_recipe$fetch$handler_order, use.names = FALSE),
    expected_handlers
  )
  expect_true(length(expected_handlers) > 0L)
  expect_false(written_recipe$fetch$enabled)
  expect_true(written_recipe$harmonize$enabled)
})

test_that("M9l publication bytes are deterministic apart from destination evidence", {
  testthat::local_mocked_bindings(
    gx_now = package_write_test_clock,
    .package = "geoconnexr"
  )
  bundle <- package_write_test_catalog_bundle()
  parent <- package_write_test_parent()
  left <- gx_package_write_impl(bundle, file.path(parent, "left"))
  right <- gx_package_write_impl(bundle, file.path(parent, "right"))

  paths <- c(gx_snapshot_manifest_name, bundle$resources$path)
  for (path in paths) {
    left_path <- file.path(left$path, path)
    right_path <- file.path(right$path, path)
    expect_identical(
      readBin(left_path, what = "raw", n = file.info(left_path)$size),
      readBin(right_path, what = "raw", n = file.info(right_path)$size),
      info = path
    )
  }
  expect_identical(
    left$verification$manifest_sha256,
    right$verification$manifest_sha256
  )
  expect_false(identical(left$publication_id, right$publication_id))
})

test_that("M9l preserves existing destinations and cleans failed staging", {
  bundle <- package_write_test_catalog_bundle()
  parent <- package_write_test_parent()
  existing <- file.path(parent, "existing")
  dir.create(existing)
  marker <- file.path(existing, "marker")
  writeLines("preserve", marker)

  expect_error(
    gx_package_write_impl(bundle, existing),
    class = "gx_error_package_write_exists"
  )
  expect_true(file.exists(marker))
  expect_length(package_write_test_stage_paths(parent), 0L)

  failures <- list(
    write = list(
      gx_package_writer_write_raw = function(...) {
        gx_package_writer_abort(
          "injected write failure",
          "gx_error_package_write_io"
        )
      }
    ),
    verify = list(
      gx_package_writer_verify = function(...) {
        gx_package_writer_abort(
          "injected verify failure",
          "gx_error_package_write_verification"
        )
      }
    ),
    rename = list(gx_package_writer_rename = function(...) FALSE)
  )
  for (name in names(failures)) {
    target <- file.path(parent, paste0("failed-", name))
    expect_error(
      do.call(
        testthat::with_mocked_bindings,
        c(
          list(code = quote(gx_package_write_impl(bundle, target))),
          failures[[name]],
          list(.package = "geoconnexr")
        )
      ),
      class = "gx_error_package_write",
      info = name
    )
    expect_false(file.exists(target) || dir.exists(target), info = name)
    expect_length(package_write_test_stage_paths(parent), 0L)
  }
})

test_that("M9l never removes a target after atomic exposure", {
  bundle <- package_write_test_catalog_bundle()
  parent <- package_write_test_parent()
  target <- file.path(parent, "published")
  real_verify <- gx_package_writer_verify
  calls <- 0L

  expect_error(
    testthat::with_mocked_bindings(
      gx_package_write_impl(bundle, target),
      gx_package_writer_verify = function(path) {
        calls <<- calls + 1L
        if (calls == 1L) return(real_verify(path))
        gx_package_writer_abort(
          "injected final verification failure",
          "gx_error_package_write_verification"
        )
      },
      .package = "geoconnexr"
    ),
    class = "gx_error_package_write_verification"
  )
  expect_identical(calls, 2L)
  expect_true(dir.exists(target))
  expect_true(file.exists(file.path(target, gx_snapshot_manifest_name)))
  expect_identical(gx_snapshot_verify(target)$status, "verified")
  expect_length(package_write_test_stage_paths(parent), 0L)
})

test_that("M9l publication evidence and stored mutation fail closed", {
  testthat::local_mocked_bindings(
    gx_now = package_write_test_clock,
    .package = "geoconnexr"
  )
  bundle <- package_write_test_catalog_bundle()
  parent <- package_write_test_parent()
  publication <- gx_package_write_impl(
    bundle,
    file.path(parent, "package")
  )
  mutations <- list(
    stage = function(x) {
      x$stage <- "fetched"
      x
    },
    bundle = function(x) {
      x$bundle$metadata$publishes <- TRUE
      x
    },
    verification = function(x) {
      x$verification$resources$actual_sha256[[1L]] <-
        paste(rep("0", 64L), collapse = "")
      x
    },
    metadata = function(x) {
      x$metadata$overwrite <- TRUE
      x
    },
    path = function(x) {
      x$path <- paste0(x$path, "-forged")
      x
    },
    identity = function(x) {
      x$publication_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(publication, NULL)))
    expect_error(
      gx_package_publication_validate_impl(forged),
      class = "gx_error_package_write",
      info = name
    )
  }

  resource <- file.path(publication$path, bundle$resources$path[[1L]])
  bytes <- readBin(resource, what = "raw", n = file.info(resource)$size)
  bytes[[1L]] <- as.raw(bitwXor(as.integer(bytes[[1L]]), 1L))
  writeBin(bytes, resource)
  expect_error(
    gx_snapshot_verify(publication$path),
    class = "gx_error_snapshot_resource"
  )
})

test_that("M9l package publication performs no external work", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external seam", call. = FALSE)
  }
  bundle <- package_write_test_catalog_bundle()
  parent <- package_write_test_parent()

  expect_no_error(testthat::with_mocked_bindings(
    gx_package_write_impl(bundle, file.path(parent, "package")),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9l writer and publication internals remain unexported", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_package_write_impl", "gx_package_publication_validate_impl",
    "gx_package_writer_manifest_impl", "gx_package_writer_write_raw",
    "gx_package_writer_rename", "gx_package_writer_unlink"
  ) %in% exports))
})
