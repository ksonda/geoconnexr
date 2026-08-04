package_tables_test_parent <- function(.local_envir = parent.frame()) {
  withr::local_tempdir(
    pattern = "gx-package-tables-parent-",
    .local_envir = .local_envir
  )
}

package_tables_test_inputs <- function() {
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

test_that("M9o exposes canonical package tables for every source stage", {
  inputs <- package_tables_test_inputs()
  parent <- package_tables_test_parent()
  views <- list()
  for (stage in names(inputs)) {
    target <- file.path(parent, stage)
    package <- gx_package(
      inputs[[stage]],
      target,
      catalog = if (identical(stage, "catalog")) NULL else inputs$catalog
    )
    view <- gx_package_tables(target)
    csv <- view$loaded$resources$format == "csv"

    expect_s3_class(view, "gx_package_tables")
    expect_identical(view$stage, stage)
    expect_identical(view$status, "parsed_and_verified")
    expect_identical(view$path, package$path)
    expect_identical(names(view$tables), view$loaded$resources$path[csv])
    expect_identical(
      view$metadata$csv_tables,
      as.integer(sum(csv))
    )
    expect_identical(
      view$metadata$raw_resources,
      as.integer(sum(view$loaded$resources$format == "raw"))
    )
    expect_true(view$metadata$read_only)
    expect_true(view$metadata$canonical)
    expect_true(view$metadata$character_only)
    expect_false(view$metadata$reconstructed_objects)
    expect_false(view$metadata$authenticity)
    expect_false(view$metadata$frictionless)
    expect_false(view$metadata$replayable)
    expect_true(all(vapply(view$tables, function(table) {
      inherits(table, "tbl_df") &&
        all(vapply(table, is.character, logical(1)))
    }, logical(1))))
    expect_identical(
      gx_package_tables_validate_impl(view),
      invisible(view)
    )
    views[[stage]] <- view
  }
  expect_true(
    views$harmonized$metadata$csv_tables >
      views$catalog$metadata$csv_tables
  )
})

test_that("M9o table projections round-trip to exact loaded bytes", {
  inputs <- package_tables_test_inputs()
  parent <- package_tables_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$harmonized, target, catalog = inputs$catalog)
  view <- gx_package_tables(target)

  for (path in names(view$tables)) {
    expect_identical(
      gx_snapshot_csv_bytes_impl(
        view$tables[[path]],
        max_bytes = .gx_package_resources_max_resource_bytes,
        max_columns = gx_snapshot_csv_load_max_columns,
        max_fields = .gx_package_resources_max_fields
      ),
      view$loaded$contents[[path]],
      info = path
    )
  }
  expect_identical(
    view$metadata$rows,
    as.double(sum(vapply(view$tables, nrow, integer(1))))
  )
  expect_identical(
    view$metadata$columns,
    as.double(sum(vapply(view$tables, ncol, integer(1))))
  )
})

test_that("M9o parser preserves UTF-8 and rejects noncanonical CSV", {
  table <- tibble::tibble(
    label = c("Café \u2603", "quoted \" value"),
    value = c("1", "")
  )
  bytes <- gx_snapshot_csv_bytes_impl(table)
  expect_identical(gx_package_table_parse_impl(bytes), table)

  cases <- list(
    unquoted = charToRaw("label,value\nx,1\n"),
    crlf = charToRaw("\"label\",\"value\"\r\n\"x\",\"1\"\r\n"),
    bom = c(as.raw(c(239L, 187L, 191L)), bytes),
    malformed = charToRaw("\"label\",\"value\"\n\"x,\"1\"\n")
  )
  for (name in names(cases)) {
    expect_error(
      gx_package_table_parse_impl(cases[[name]]),
      class = "gx_error_package_tables",
      info = name
    )
  }
})

test_that("M9o rejects snapshot profiles and package corruption", {
  inputs <- package_tables_test_inputs()
  parent <- package_tables_test_parent()
  snapshot <- file.path(parent, "snapshot")
  package <- file.path(parent, "package")
  gx_snapshot(inputs$catalog, snapshot)
  gx_package(inputs$catalog, package)

  expect_error(
    gx_package_tables(snapshot),
    class = "gx_error_package_tables"
  )

  verification <- gx_snapshot_verify(package)
  path <- verification$resources$path[[1L]]
  file <- file.path(package, path)
  bytes <- readBin(file, what = "raw", n = file.info(file)$size)
  bytes[[1L]] <- as.raw(bitwXor(as.integer(bytes[[1L]]), 1L))
  writeBin(bytes, file)
  expect_error(
    gx_package_tables(package),
    class = "gx_error_package_tables"
  )
})

test_that("M9o table evidence fails closed under forgery", {
  inputs <- package_tables_test_inputs()
  parent <- package_tables_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$catalog, target)
  view <- gx_package_tables(target)
  mutations <- list(
    stage = function(x) {
      x$stage <- "fetched"
      x
    },
    path = function(x) {
      x$path <- paste0(x$path, "-forged")
      x
    },
    loaded = function(x) {
      x$loaded$load_id <- paste(rep("0", 64L), collapse = "")
      x
    },
    tables = function(x) {
      x$tables[[1L]][[1L]][[1L]] <- "forged"
      x
    },
    metadata = function(x) {
      x$metadata$replayable <- TRUE
      x
    },
    identity = function(x) {
      x$view_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(view, NULL)))
    expect_error(
      gx_package_tables_validate_impl(forged),
      class = "gx_error_package_tables",
      info = name
    )
  }
})

test_that("M9o table loading performs no external or write work", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or write seam", call. = FALSE)
  }
  inputs <- package_tables_test_inputs()
  parent <- package_tables_test_parent()
  target <- file.path(parent, "package")
  gx_package(inputs$catalog, target)

  expect_no_error(testthat::with_mocked_bindings(
    gx_package_tables(target),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_cache_backend = blocked,
    gx_package_writer_write_raw = blocked,
    gx_snapshot_writer_write_raw = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9o exports only its public table-view boundary", {
  exports <- getNamespaceExports("geoconnexr")
  expect_true(all(c(
    "gx_package", "gx_package_load", "gx_package_tables"
  ) %in% exports))
  expect_false(any(c(
    "gx_package_table_parse_impl", "gx_package_tables_build_impl",
    "gx_package_tables_validate_impl", "gx_package_tables_metadata_impl"
  ) %in% exports))
})
