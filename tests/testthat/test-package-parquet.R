package_parquet_test_harmonized <- function() {
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  gx_harmonize(fetched)
}

package_parquet_test_populated_harmonized <- function() {
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
  fetched <- gx_fetch_impl(
    plan = fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    limits = limits,
    orchestration_scope_id = fetch_orchestration_test_scope(
      "package-parquet"
    ),
    oaf_symbol_resolver = oaf_test_resolver(),
    wqp_symbol_resolver = wqp_test_resolver(),
    edr_symbol_resolver = edr_test_resolver(),
    usgs_continuous_symbol_resolver = usgs_continuous_test_resolver(),
    usgs_daily_symbol_resolver = usgs_daily_test_resolver()
  )
  gx_harmonize(fetched)
}

package_parquet_test_table <- function() {
  template <- gx_harmonized_empty_observations_impl()
  values <- lapply(names(template), function(name) {
    column <- template[[name]]
    if (inherits(column, "POSIXct")) {
      return(as.POSIXct("2026-08-03T12:00:00Z", tz = "UTC"))
    }
    switch(
      typeof(column),
      character = if (grepl("(?:^|_)(?:uri|url)$", name, perl = TRUE)) {
        "https://user:secret@example.org/path?token=secret#fragment"
      } else {
        paste0("value-", name)
      },
      integer = 1L,
      double = 1.25,
      logical = TRUE
    )
  })
  names(values) <- names(template)
  as.data.frame(
    values,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    optional = TRUE
  )
}

test_that("M9u capability gates namespace loading on reviewed metadata", {
  calls <- new.env(parent = emptyenv())
  calls$loads <- 0L
  loader <- function(package) {
    calls$loads <- calls$loads + 1L
    new.env(parent = emptyenv())
  }
  expect_error(
    gx_package_parquet_capability_impl(
      version_resolver = function(package) NA_character_,
      namespace_loader = loader
    ),
    class = "gx_error_package_parquet_missing"
  )
  expect_error(
    gx_package_parquet_capability_impl(
      version_resolver = function(package) "13.0.0",
      namespace_loader = loader
    ),
    class = "gx_error_package_parquet_version"
  )
  expect_identical(calls$loads, 0L)
})

test_that("M9u capability rejects races and changed Arrow symbols", {
  namespace <- new.env(parent = emptyenv())
  expect_error(
    gx_package_parquet_capability_impl(
      version_resolver = function(package) "14.0.0",
      namespace_loader = function(package) namespace,
      namespace_version_resolver = function(namespace) "14.0.1"
    ),
    class = "gx_error_package_parquet_race"
  )
  expect_error(
    gx_package_parquet_capability_impl(
      version_resolver = function(package) "14.0.0",
      namespace_loader = function(package) namespace,
      namespace_version_resolver = function(namespace) "14.0.0",
      export_resolver = function(package, symbol) function() NULL
    ),
    class = "gx_error_package_parquet_symbol"
  )
})

test_that("M9u serializes the exact typed profile deterministically in memory", {
  skip_if_not_installed("arrow", minimum_version = "14.0.0")
  harmonized <- package_parquet_test_harmonized()
  first <- gx_package_parquet_impl(harmonized)
  second <- gx_package_parquet_impl(harmonized)

  expect_s3_class(first, "gx_package_parquet")
  expect_identical(first$status, "serialized_verified")
  expect_identical(first$harmonized, harmonized)
  expect_identical(first$content, second$content)
  expect_identical(first$parquet_id, second$parquet_id)
  expect_identical(first$content[seq_len(4L)], charToRaw("PAR1"))
  expect_identical(
    first$content[(length(first$content) - 3L):length(first$content)],
    charToRaw("PAR1")
  )
  expect_identical(first$metadata$scope, "fixed_arrow_parquet_v1")
  expect_identical(first$metadata$arrow_minimum_version, "14.0.0")
  expect_identical(first$metadata$parquet_version, "2.4")
  expect_identical(first$metadata$compression, "uncompressed")
  expect_false(first$metadata$use_dictionary)
  expect_false(first$metadata$write_statistics)
  expect_true(first$metadata$in_memory)
  expect_true(first$metadata$deterministic_within_arrow_version)
  expect_false(first$metadata$cross_version_byte_stability)
  expect_false(first$metadata$writes)
  expect_false(first$metadata$public)
  expect_false(first$metadata$replayable)
  expect_identical(
    gx_package_parquet_validate_impl(first),
    invisible(first)
  )
})

test_that("M9u preserves reviewed column types and redacts URL values", {
  skip_if_not_installed("arrow", minimum_version = "14.0.0")
  table <- package_parquet_test_table()
  redacted <- gx_snapshot_writer_redact_view(table)
  expect_false(any(grepl(
    "secret|token|fragment", unlist(redacted), fixed = FALSE
  )))
  expect_identical(lapply(redacted, class), lapply(table, class))
  capability <- gx_package_parquet_capability_impl()
  serialized <- gx_package_parquet_serialize_impl(redacted, capability)
  second <- gx_package_parquet_serialize_impl(redacted, capability)
  expect_identical(serialized$content, second$content)
  expect_identical(serialized$chunk_size, 1L)
})

test_that("M9u admits a populated exact harmonized observation source", {
  skip_if_not_installed("arrow", minimum_version = "14.0.0")
  harmonized <- package_parquet_test_populated_harmonized()
  expect_gt(nrow(harmonized$observations), 0L)
  value <- gx_package_parquet_impl(harmonized)
  expect_identical(value$metadata$rows, nrow(harmonized$observations))
  expect_identical(
    lapply(value$table, class),
    lapply(as.data.frame(harmonized$observations), class)
  )
  expect_false(any(grepl(
    "[?#]|@", unlist(value$table[grepl(
      "(?:^|_)(?:uri|url)$", names(value$table), perl = TRUE
    )]), perl = TRUE
  )))
})

test_that("M9u evidence fails closed under forgery", {
  skip_if_not_installed("arrow", minimum_version = "14.0.0")
  value <- gx_package_parquet_impl(package_parquet_test_harmonized())
  mutations <- list(
    source = function(x) {
      x$harmonized$metadata$counts$observations <- 1L
      x
    },
    table = function(x) {
      names(x$table)[[1L]] <- "forged_contract_version"
      x
    },
    content = function(x) {
      x$content[[5L]] <- as.raw(bitwXor(as.integer(x$content[[5L]]), 1L))
      x
    },
    metadata = function(x) {
      x$metadata$cross_version_byte_stability <- TRUE
      x
    },
    identity = function(x) {
      x$parquet_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(value, NULL)))
    expect_error(
      gx_package_parquet_validate_impl(forged),
      class = "gx_error_package_parquet",
      info = name
    )
  }
})

test_that("M9u remains internal and does not call publication seams", {
  skip_if_not_installed("arrow", minimum_version = "14.0.0")
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external seam", call. = FALSE)
  }
  expect_no_error(testthat::with_mocked_bindings(
    gx_package_parquet_impl(package_parquet_test_harmonized()),
    gx_http_request = blocked,
    gx_package_write_impl = blocked,
    gx_package_replace_impl = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
  expect_false(any(c(
    "gx_package_parquet_impl", "gx_package_parquet_validate_impl",
    "gx_package_parquet_capability_impl"
  ) %in% getNamespaceExports("geoconnexr")))
})

test_that("M9v publishes and loads verified Parquet packages end to end", {
  skip_if_not_installed("arrow", minimum_version = "14.0.0")
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  harmonized <- package_parquet_test_harmonized()
  parent <- withr::local_tempdir(pattern = "gx-package-parquet-public-")
  target <- file.path(parent, "package")

  package <- gx_package(
    harmonized,
    target,
    catalog = catalog,
    timeseries = "parquet"
  )
  expect_s3_class(package, "gx_package")
  expect_identical(package$metadata$timeseries, "parquet")
  expect_true(package$metadata$arrow)
  expect_identical(
    package$verification$manifest$recipe$output$timeseries,
    "parquet"
  )
  serialization <-
    package$verification$manifest$effective_options$serialization
  expect_identical(serialization$writer, "fixed-package-writer-v0.2")
  expect_identical(serialization$parquet$profile, "fixed-arrow-parquet-v1")
  expect_identical(serialization$parquet$parquet_version, "2.4")
  expect_identical(
    package$verification$resources$path[
      vapply(
        package$verification$resources$roles,
        function(x) identical(x, c("data", "observations")),
        logical(1)
      )
    ],
    "data/observations.parquet"
  )

  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked Arrow capability", call. = FALSE)
  }
  loaded <- testthat::with_mocked_bindings(
    gx_package_load(target),
    gx_package_parquet_capability_impl = blocked,
    .package = "geoconnexr"
  )
  expect_identical(calls, 0L)
  expect_identical(loaded$metadata$parquet_resources, 1L)
  expect_identical(
    loaded$resources$format[loaded$resources$role == "observations"],
    "parquet"
  )
  expect_identical(
    loaded$contents[["data/observations.parquet"]][seq_len(4L)],
    charToRaw("PAR1")
  )

  tables <- testthat::with_mocked_bindings(
    gx_package_tables(target),
    gx_package_parquet_capability_impl = blocked,
    .package = "geoconnexr"
  )
  expect_identical(calls, 0L)
  expect_identical(tables$metadata$parquet_resources, 1L)
  expect_false("data/observations.parquet" %in% names(tables$tables))

  hydrated <- gx_package_hydrate(target)
  expect_identical(hydrated$metadata$observations_format, "parquet")
  expect_true(hydrated$metadata$arrow)
  expect_identical(
    hydrated$harmonized$observations,
    tibble::as_tibble(gx_package_parquet_table_impl(harmonized))
  )
})

test_that("M9v scopes Parquet to harmonized inputs before publication", {
  skip_if_not_installed("arrow", minimum_version = "14.0.0")
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  parent <- withr::local_tempdir(pattern = "gx-package-parquet-scope-")
  cases <- list(catalog = catalog, fetched = fetched)

  for (stage in names(cases)) {
    target <- file.path(parent, stage)
    expect_error(
      gx_package(
        cases[[stage]],
        target,
        catalog = if (stage == "catalog") NULL else catalog,
        timeseries = "parquet"
      ),
      class = "gx_error_package_scope",
      info = stage
    )
    expect_false(file.exists(target) || dir.exists(target), info = stage)
  }
})

test_that("M9v replacement preserves verified format generations", {
  skip_if_not_installed("arrow", minimum_version = "14.0.0")
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  harmonized <- package_parquet_test_harmonized()
  parent <- withr::local_tempdir(pattern = "gx-package-parquet-replace-")
  target <- file.path(parent, "package")

  prior <- gx_package(harmonized, target, catalog = catalog)
  replacement <- gx_package(
    harmonized,
    target,
    catalog = catalog,
    timeseries = "parquet",
    overwrite = TRUE
  )

  expect_identical(replacement$status, "replaced_and_verified")
  expect_identical(replacement$metadata$timeseries, "parquet")
  expect_identical(
    replacement$previous$manifest_sha256,
    prior$verification$manifest_sha256
  )
  expect_identical(gx_package_load(target)$metadata$parquet_resources, 1L)
})
