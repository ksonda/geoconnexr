package_frictionless_public_inputs <- local({
  inputs <- NULL
  function() {
    if (!is.null(inputs)) return(inputs)
    catalog <- fetch_orchestration_test_usgs_daily_catalog()
    fetched <- gx_fetch(
      fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
      dry_run = TRUE
    )
    inputs <<- list(
      catalog = catalog,
      fetched = fetched,
      harmonized = gx_harmonize(fetched)
    )
    inputs
  }
})

test_that("M9ad publishes Data Package v1 for every fixed all-CSV stage", {
  inputs <- package_frictionless_public_inputs()
  parent <- withr::local_tempdir(pattern = "gx-frictionless-public-")
  for (stage in names(inputs)) {
    target <- file.path(parent, stage)
    package <- gx_package(
      inputs[[stage]],
      target,
      catalog = if (identical(stage, "catalog")) NULL else inputs$catalog,
      frictionless = TRUE
    )
    expect_true(package$metadata$frictionless, info = stage)
    expect_false(package$metadata$report, info = stage)
    expect_false(package$metadata$arrow, info = stage)
    expect_true(file.exists(file.path(target, "datapackage.json")))
    serialization <-
      package$verification$manifest$effective_options$serialization
    expect_identical(
      serialization$resource_profile,
      "fixed-in-memory-resources-v2+frictionless-data-package-v1"
    )
    expect_identical(
      serialization$frictionless$profile,
      "fixed-frictionless-data-package-v1"
    )
    expect_identical(serialization$frictionless$cli_version, "5.19.0")
    expect_identical(
      serialization$frictionless$runtime_cli_executed, FALSE
    )
    expect_identical(
      serialization$frictionless$descriptor_path, "datapackage.json"
    )
    descriptor <- gx_snapshot_parse_json(
      readBin(
        file.path(target, "datapackage.json"),
        what = "raw",
        n = .gx_package_frictionless_max_bytes + 1L
      )
    )
    expect_identical(descriptor$profile, "data-package")
    expect_false("datapackage.json" %in% vapply(
      descriptor$resources, `[[`, character(1), "path"
    ))
    expect_identical(
      vapply(descriptor$resources, `[[`, character(1), "path"),
      setdiff(package$verification$resources$path, "datapackage.json")
    )
    expect_identical(gx_package_validate_impl(package), invisible(package))
  }
})

test_that("M9ad remains exact through offline inspection", {
  inputs <- package_frictionless_public_inputs()
  parent <- withr::local_tempdir(pattern = "gx-frictionless-inspect-")
  target <- file.path(parent, "package")
  package <- gx_package(inputs$catalog, target, frictionless = TRUE)
  loaded <- gx_package_load(package$path)
  tables <- gx_package_tables(package$path)
  hydrated <- gx_package_hydrate(target)
  replay <- gx_replay(hydrated$table_view$loaded)

  expect_true(loaded$metadata$frictionless)
  expect_true(tables$metadata$frictionless)
  expect_true(hydrated$metadata$frictionless)
  expect_true(replay$metadata$frictionless)
  expect_identical(
    loaded$contents[["datapackage.json"]],
    readBin(
      file.path(target, "datapackage.json"),
      what = "raw",
      n = .gx_package_frictionless_max_bytes + 1L
    )
  )
  expect_false("datapackage.json" %in% names(tables$tables))
  expect_false(replay$metadata$recipe_executed)
  expect_false(replay$metadata$replayable)
})

test_that("M9ad replacement admits adding and removing the descriptor", {
  catalog <- package_frictionless_public_inputs()$catalog
  parent <- withr::local_tempdir(pattern = "gx-frictionless-replace-")
  target <- file.path(parent, "package")
  plain <- gx_package(catalog, target)
  expect_false(file.exists(file.path(target, "datapackage.json")))
  expect_false("frictionless" %in% names(
    plain$verification$manifest$effective_options$serialization
  ))
  described <- gx_package(
    catalog, target, overwrite = TRUE, frictionless = TRUE
  )
  expect_false(plain$metadata$frictionless)
  expect_true(described$metadata$frictionless)
  expect_true(file.exists(file.path(target, "datapackage.json")))
  plain_again <- gx_package(catalog, target, overwrite = TRUE)
  expect_false(plain_again$metadata$frictionless)
  expect_false(file.exists(file.path(target, "datapackage.json")))
  expect_identical(
    gx_snapshot_verify(target)$manifest_sha256,
    plain_again$verification$manifest_sha256
  )
})

test_that("M9af keeps mixed Frictionless options inside public input scope", {
  catalog <- package_frictionless_public_inputs()$catalog
  parent <- withr::local_tempdir(pattern = "gx-frictionless-scope-")
  parquet_target <- file.path(parent, "catalog-parquet")
  expect_error(
    gx_package(
      catalog,
      parquet_target,
      timeseries = "parquet",
      frictionless = TRUE
    ),
    class = "gx_error_package_scope"
  )
  expect_false(file.exists(parquet_target) || dir.exists(parquet_target))
  expect_error(
    gx_package(
      catalog, file.path(parent, "invalid"), frictionless = NA
    ),
    class = "gx_error_package_input"
  )
})

test_that("M9ad detects a self-consistent but semantically forged descriptor", {
  catalog <- package_frictionless_public_inputs()$catalog
  parent <- withr::local_tempdir(pattern = "gx-frictionless-forgery-")
  target <- file.path(parent, "package")
  gx_package(catalog, target, frictionless = TRUE)

  descriptor_path <- file.path(target, "datapackage.json")
  descriptor <- gx_snapshot_parse_json(readBin(
    descriptor_path, what = "raw", n = .gx_package_frictionless_max_bytes + 1L
  ))
  descriptor$name <- "forged-package"
  descriptor_bytes <- gx_package_frictionless_json_bytes_impl(descriptor)
  writeBin(descriptor_bytes, descriptor_path)

  manifest_path <- file.path(target, gx_snapshot_manifest_name)
  manifest <- gx_snapshot_parse_json(readBin(
    manifest_path, what = "raw", n = gx_snapshot_max_manifest_bytes + 1L
  ))
  position <- match(
    "datapackage.json",
    vapply(manifest$resources, `[[`, character(1), "path")
  )
  sha256 <- digest::digest(
    descriptor_bytes, algo = "sha256", serialize = FALSE
  )
  manifest$resources[[position]]$bytes <- as.integer(length(descriptor_bytes))
  manifest$resources[[position]]$sha256 <- sha256
  manifest$effective_options$serialization$frictionless$descriptor_bytes <-
    as.integer(length(descriptor_bytes))
  manifest$effective_options$serialization$frictionless$descriptor_sha256 <-
    sha256
  writeBin(gx_snapshot_writer_json_bytes(manifest), manifest_path)

  expect_s3_class(gx_snapshot_verify(target), "gx_snapshot_verification")
  expect_error(
    gx_package_load(target),
    class = "gx_error_package_load"
  )
})

test_that("M9ad published package passes the pinned Frictionless CLI", {
  cli <- Sys.getenv("GEOCONNEXR_FRICTIONLESS_CLI", unset = "")
  skip_if(!nzchar(cli), "Pinned Frictionless CLI is not configured")
  expect_true(file.exists(cli))
  expect_identical(
    unname(system2(cli, "--version", stdout = TRUE, stderr = TRUE)),
    "5.19.0"
  )
  parent <- withr::local_tempdir(pattern = "gx-frictionless-public-cli-")
  package <- gx_package(
    package_frictionless_public_inputs()$catalog,
    file.path(parent, "package"),
    frictionless = TRUE
  )
  output <- withr::with_dir(package$path, system2(
    cli,
    c("validate", "datapackage.json", "--standards", "v1", "--json"),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_null(attr(output, "status"))
  report <- jsonlite::fromJSON(paste(output, collapse = "\n"))
  expect_true(report$valid)
  expect_identical(report$stats$errors, 0L)
  expect_identical(report$stats$warnings, 0L)
})
