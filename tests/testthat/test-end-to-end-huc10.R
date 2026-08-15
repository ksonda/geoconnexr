test_that("installed HUC10 profile builds the reviewed USGS daily plan", {
  site_uri <- "https://example.org/geoconnexr/usgs-01491000"
  profile_path <- system.file(
    "extdata", "huc10-usgs-daily-profile.json",
    package = "geoconnexr"
  )
  profile <- jsonlite::fromJSON(profile_path, simplifyVector = FALSE)

  catalog <- gx_catalog(
    gx_aoi("0206000502"),
    site_uri = site_uri,
    profiles = stats::setNames(list(profile), site_uri),
    max_sites = 1L
  )
  plan <- gx_fetch_plan(
    catalog,
    time = as.POSIXct(
      c("2025-06-01 00:00:00", "2025-06-02 23:59:59"),
      tz = "UTC"
    ),
    max_datasets = 1L,
    max_bytes = 1024^2
  )

  expect_identical(nrow(catalog$sites), 1L)
  expect_identical(nrow(catalog$datasets), 1L)
  expect_identical(catalog$datasets$handler_id, "usgs_waterdata_daily")
  expect_true(catalog$datasets$fetchable)
  expect_identical(nrow(plan$distributions), 1L)
})

test_that("installed HUC10 demo verifies and hydrates offline", {
  archive <- system.file(
    "extdata", "huc10-usgs-daily-demo.tgz",
    package = "geoconnexr"
  )
  parent <- withr::local_tempdir(pattern = "gx-huc10-demo-")
  status <- utils::untar(archive, exdir = parent)
  path <- file.path(parent, "huc10-usgs-daily-demo")

  expect_true(nzchar(archive))
  expect_identical(status, 0L)
  verification <- gx_snapshot_verify(path)
  hydrated <- gx_package_hydrate(path)
  observations <- hydrated$harmonized$observations
  replay <- gx_replay(path)

  expect_identical(verification$status, "verified")
  expect_identical(hydrated$stage, "harmonized")
  expect_identical(nrow(observations), 2L)
  expect_equal(
    observations$value,
    c(5.181982926336, 4.275843835392),
    tolerance = 1e-12
  )
  expect_identical(observations$original_value, c("183", "151"))
  expect_identical(
    observations$unit_uri,
    rep("http://qudt.org/vocab/unit/M3-PER-SEC", 2L)
  )
  expect_identical(
    observations$conversion_rule_id,
    rep("flow-ft3-s-to-m3-s", 2L)
  )
  expect_identical(replay$status, "verified_and_loaded")
  expect_false(replay$metadata$recipe_executed)
  expect_true(replay$metadata$offline)
})
