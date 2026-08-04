package_quarto_test_exports <- function(calls = NULL) {
  invoked <- function() {
    if (!is.null(calls)) calls$invocations <- calls$invocations + 1L
  }
  list(
    quarto_render = function(
        input = NULL,
        output_format = NULL,
        output_file = NULL,
        execute = TRUE,
        execute_params = NULL,
        execute_dir = NULL,
        execute_daemon = NULL,
        execute_daemon_restart = FALSE,
        execute_debug = FALSE,
        use_freezer = FALSE,
        cache = NULL,
        cache_refresh = FALSE,
        metadata = NULL,
        metadata_file = NULL,
        debug = FALSE,
        quiet = FALSE,
        profile = NULL,
        quarto_args = NULL,
        pandoc_args = NULL,
        as_job = "auto") {
      invoked()
      invisible(NULL)
    },
    quarto_path = function(normalize = TRUE) {
      invoked()
      "/usr/local/bin/quarto"
    },
    quarto_version = function() {
      invoked()
      numeric_version("1.6.0")
    },
    quarto_available = function(min = NULL, max = NULL, error = FALSE) {
      invoked()
      TRUE
    }
  )
}

package_quarto_test_export_resolver <- function(exports) {
  force(exports)
  function(package, symbol) exports[[symbol]]
}

test_that("M9w gates namespace loading on reviewed Quarto metadata", {
  calls <- new.env(parent = emptyenv())
  calls$loads <- 0L
  loader <- function(package) {
    calls$loads <- calls$loads + 1L
    new.env(parent = emptyenv())
  }
  expect_error(
    gx_package_quarto_capability_impl(
      version_resolver = function(package) NA_character_,
      namespace_loader = loader
    ),
    class = "gx_error_package_quarto_missing"
  )
  expect_error(
    gx_package_quarto_capability_impl(
      version_resolver = function(package) "1.5.0",
      namespace_loader = loader
    ),
    class = "gx_error_package_quarto_version"
  )
  expect_identical(calls$loads, 0L)
})

test_that("M9w rejects Quarto namespace races and changed exports", {
  namespace <- new.env(parent = emptyenv())
  expect_error(
    gx_package_quarto_capability_impl(
      version_resolver = function(package) "1.5.1",
      namespace_loader = function(package) namespace,
      namespace_version_resolver = function(namespace) "1.5.2"
    ),
    class = "gx_error_package_quarto_race"
  )

  exports <- package_quarto_test_exports()
  exports$quarto_render <- function(input = NULL) invisible(NULL)
  expect_error(
    gx_package_quarto_capability_impl(
      version_resolver = function(package) "1.5.1",
      namespace_loader = function(package) namespace,
      namespace_version_resolver = function(namespace) "1.5.1",
      export_resolver = package_quarto_test_export_resolver(exports)
    ),
    class = "gx_error_package_quarto_symbol"
  )
})

test_that("M9w resolves the reviewed report symbols without invoking them", {
  namespace <- new.env(parent = emptyenv())
  calls <- new.env(parent = emptyenv())
  calls$invocations <- 0L
  exports <- package_quarto_test_exports(calls)
  capability <- gx_package_quarto_capability_impl(
    version_resolver = function(package) "1.5.1",
    namespace_loader = function(package) namespace,
    namespace_version_resolver = function(namespace) "1.5.1",
    export_resolver = package_quarto_test_export_resolver(exports)
  )

  expect_identical(
    names(capability),
    c("version", .gx_package_quarto_required_exports)
  )
  expect_identical(capability$version, "1.5.1")
  expect_identical(calls$invocations, 0L)
  expect_identical(
    gx_package_quarto_capability_validate_impl(capability),
    invisible(capability)
  )
})

test_that("M9w capability fails closed under malformed resolvers", {
  resolvers <- list(
    version = list(version_resolver = function(package) "not-a-version"),
    loader = list(namespace_loader = function(package) {
      warning("changed loader")
      new.env(parent = emptyenv())
    }),
    export = list(export_resolver = function(package, symbol) {
      stop("missing export")
    })
  )
  for (name in names(resolvers)) {
    arguments <- c(
      list(
        version_resolver = function(package) "1.5.1",
        namespace_loader = function(package) new.env(parent = emptyenv()),
        namespace_version_resolver = function(namespace) "1.5.1",
        export_resolver = package_quarto_test_export_resolver(
          package_quarto_test_exports()
        )
      ),
      resolvers[[name]]
    )
    arguments <- arguments[!duplicated(names(arguments), fromLast = TRUE)]
    expect_error(
      do.call(gx_package_quarto_capability_impl, arguments),
      class = "gx_error_package_quarto",
      info = name
    )
  }
  expect_error(
    gx_package_quarto_capability_impl(export_resolver = NULL),
    class = "gx_error_package_quarto_input"
  )
})

test_that("M9w remains internal and performs no report or external work", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_package_quarto_capability_impl",
    "gx_package_quarto_capability_validate_impl",
    "gx_package_quarto_version_impl"
  ) %in% exports))

  namespace <- new.env(parent = emptyenv())
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or report seam", call. = FALSE)
  }
  expect_no_error(testthat::with_mocked_bindings(
    gx_package_quarto_capability_impl(
      version_resolver = function(package) "1.5.1",
      namespace_loader = function(package) namespace,
      namespace_version_resolver = function(namespace) "1.5.1",
      export_resolver = package_quarto_test_export_resolver(
        package_quarto_test_exports()
      )
    ),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})
