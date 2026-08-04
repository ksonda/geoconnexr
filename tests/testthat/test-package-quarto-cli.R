package_quarto_cli_test_file <- function(.local_envir = parent.frame()) {
  path <- withr::local_tempfile(
    pattern = "gx-quarto-cli-",
    .local_envir = .local_envir
  )
  writeLines(c("#!/bin/sh", "exit 0"), path, useBytes = TRUE)
  Sys.chmod(path, mode = "0755")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

package_quarto_cli_test_capability <- function(path, calls = NULL) {
  invoked <- function(name) {
    if (!is.null(calls)) calls[[name]] <- calls[[name]] + 1L
  }
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
        as_job = "auto") {
      invoked("render")
      invisible(NULL)
    },
    quarto_path = function(normalize = TRUE) {
      invoked("path")
      path
    },
    quarto_version = function() {
      invoked("version")
      numeric_version("1.8.27")
    },
    quarto_available = function(min = NULL, max = NULL, error = FALSE) {
      invoked("available")
      TRUE
    }
  )
}

test_that("M9x admits one normalized reviewed Quarto CLI", {
  path <- package_quarto_cli_test_file()
  calls <- new.env(parent = emptyenv())
  calls$path <- 0L
  calls$render <- 0L
  calls$version <- 0L
  calls$available <- 0L
  calls$command_path <- NULL
  calls$timeout <- NULL
  capability <- package_quarto_cli_test_capability(path, calls)

  value <- gx_package_quarto_cli_impl(
    capability_resolver = function() capability,
    version_resolver = function(path, timeout) {
      calls$command_path <- path
      calls$timeout <- timeout
      "1.8.27"
    }
  )

  expect_s3_class(value, "gx_package_quarto_cli")
  expect_identical(value$status, "version_admitted")
  expect_identical(value$package_version, "1.5.1")
  expect_identical(value$path, path)
  expect_identical(value$cli_version, "1.8.27")
  expect_identical(calls$path, 1L)
  expect_identical(calls$command_path, path)
  expect_identical(calls$timeout, 5)
  expect_identical(calls$render, 0L)
  expect_identical(calls$version, 0L)
  expect_identical(calls$available, 0L)
  expect_true(value$metadata$invokes_cli)
  expect_true(value$metadata$cli_version_ready)
  expect_false(value$metadata$rendering_ready)
  expect_false(value$metadata$public)
  expect_false(value$metadata$replayable)
  expect_identical(
    gx_package_quarto_cli_validate_impl(value),
    invisible(value)
  )
})

test_that("M9x rejects missing package capability before CLI execution", {
  calls <- 0L
  expect_error(
    gx_package_quarto_cli_impl(
      capability_resolver = function() {
        gx_package_quarto_abort(
          "missing Quarto R package",
          "gx_error_package_quarto_missing"
        )
      },
      version_resolver = function(...) {
        calls <<- calls + 1L
        "1.8.27"
      }
    ),
    class = "gx_error_package_quarto_cli_capability"
  )
  expect_identical(calls, 0L)
})

test_that("M9x rejects unsafe paths and old CLI versions", {
  path <- package_quarto_cli_test_file()
  capability <- package_quarto_cli_test_capability(path)
  calls <- 0L
  expect_error(
    gx_package_quarto_cli_impl(
      capability_resolver = function() capability,
      path_resolver = function(capability) paste0(path, "-missing"),
      version_resolver = function(...) {
        calls <<- calls + 1L
        "1.8.27"
      }
    ),
    class = "gx_error_package_quarto_cli_path"
  )
  expect_identical(calls, 0L)

  expect_error(
    gx_package_quarto_cli_impl(
      capability_resolver = function() capability,
      version_resolver = function(...) "1.8.26"
    ),
    class = "gx_error_package_quarto_cli_version"
  )
})

test_that("M9x rejects CLI mutation and malformed version output", {
  path <- package_quarto_cli_test_file()
  capability <- package_quarto_cli_test_capability(path)
  expect_error(
    gx_package_quarto_cli_impl(
      capability_resolver = function() capability,
      version_resolver = function(path, timeout) {
        writeLines(c("#!/bin/sh", "echo changed", "exit 0"), path)
        Sys.chmod(path, mode = "0755")
        "1.8.27"
      }
    ),
    class = "gx_error_package_quarto_cli_race"
  )

  path <- package_quarto_cli_test_file()
  capability <- package_quarto_cli_test_capability(path)
  outputs <- list(
    malformed = "not-a-version",
    multiline = c("1.8.27", "unexpected")
  )
  for (name in names(outputs)) {
    expect_error(
      gx_package_quarto_cli_impl(
        capability_resolver = function() capability,
        version_resolver = function(...) outputs[[name]]
      ),
      class = "gx_error_package_quarto_cli_version",
      info = name
    )
  }
})

test_that("M9x evidence fails closed under forgery", {
  path <- package_quarto_cli_test_file()
  capability <- package_quarto_cli_test_capability(path)
  value <- gx_package_quarto_cli_impl(
    capability_resolver = function() capability,
    version_resolver = function(...) "1.8.27"
  )
  mutations <- list(
    package = function(x) {
      x$package_version <- "1.5.0"
      x
    },
    path = function(x) {
      x$path <- paste0(x$path, "-forged")
      x
    },
    cli = function(x) {
      x$cli_version <- "1.8.26"
      x
    },
    metadata = function(x) {
      x$metadata$rendering_ready <- TRUE
      x
    },
    identity = function(x) {
      x$cli_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(value, NULL)))
    expect_error(
      gx_package_quarto_cli_validate_impl(forged),
      class = "gx_error_package_quarto_cli",
      info = name
    )
  }
})

test_that("M9x remains internal and performs no render or external work", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_package_quarto_cli_impl",
    "gx_package_quarto_cli_validate_impl",
    "gx_package_quarto_cli_version_impl"
  ) %in% exports))

  path <- package_quarto_cli_test_file()
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or render seam", call. = FALSE)
  }
  expect_no_error(testthat::with_mocked_bindings(
    gx_package_quarto_cli_impl(
      capability_resolver = function() {
        package_quarto_cli_test_capability(path)
      },
      version_resolver = function(...) "1.8.27"
    ),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})
