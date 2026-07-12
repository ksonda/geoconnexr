test_that("endpoint defaults and overrides are validated", {
  expect_named(gx_endpoints(), c("graph", "reference", "pid"))
  withr::local_options(geoconnexr.endpoint_graph = "https://example.org/sparql")
  expect_equal(unname(gx_endpoints()["graph"]), "https://example.org/sparql")
  withr::local_options(geoconnexr.endpoint_graph = "file:///tmp/graph")
  expect_error(gx_endpoints(), class = "gx_error_option")
})
