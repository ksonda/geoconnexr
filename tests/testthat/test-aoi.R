test_that("AOI auto dispatch is deterministic", {
  expect_equal(gx_aoi("02070010")$type, "huc")
  expect_equal(gx_aoi("37135")$type, "county")
  expect_equal(gx_aoi("nc")$id, "NC")
  expect_s3_class(gx_aoi("02070010"), "gx_aoi")
  expect_equal(gx_aoi("02070010")$recipe$aoi$kind, "huc")
  expect_equal(gx_aoi("02070010")$recipe$pipeline$end_stage, "catalog")
  expect_equal(gx_aoi("02070010")$recipe$contract_version, "1.0.0")
})

test_that("AOI input preserves character identifiers", {
  expect_error(gx_aoi(2070010), class = "gx_error_identifier")
  expect_error(gx_aoi("123"), class = "gx_error_aoi")
  expect_error(gx_aoi(c("01", "02")), class = "gx_error_aoi")
})
