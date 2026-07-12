test_that("HUC validation preserves leading zeroes", {
  expect_invisible(gx_validate_huc(c("01", "02070010", "010100020101")))
  expect_error(gx_validate_huc(2070010), class = "gx_error_identifier")
  expect_error(gx_validate_huc("12345"), class = "gx_error_identifier")
})

test_that("COMIDs require character positive integers", {
  expect_invisible(gx_validate_comid(c("1", "17789327")))
  expect_error(gx_validate_comid(17789327), class = "gx_error_identifier")
  expect_error(gx_validate_comid(c("0", "-1")), class = "gx_error_identifier")
})

test_that("contract hashes are deterministic and typed", {
  a <- gx_contract_hash(list("01", 1L, NA_character_), "site")
  b <- gx_contract_hash(list("01", 1L, NA_character_), "site")
  c <- gx_contract_hash(list("01", "1", NA_character_), "site")
  d <- gx_contract_hash(list("01", 1L, NA_integer_), "site")
  expect_identical(a, b)
  expect_false(identical(a, c))
  expect_false(identical(a, d))
  expect_match(a, "^[0-9a-f]{64}$")
})
