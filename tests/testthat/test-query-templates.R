test_that("bundled query manifest is internally consistent", {
  templates <- gx_templates()
  expect_s3_class(templates, "tbl_df")
  expect_true(all(c("sites_on_mainstem", "sites_in_aoi") %in% templates$name))
  expect_true(all(templates$query_type == "select"))
  expect_true(all(templates$row_budget > 0L))
})

test_that("mainstem query renders typed URIs and bounded paging", {
  query <- gx_render_query(
    "sites_on_mainstem",
    list(
      mainstem_uri = "https://geoconnex.us/ref/mainstems/1622734",
      limit = 100,
      offset = 0
    )
  )
  expect_match(query, "<https://geoconnex.us/ref/mainstems/1622734>", fixed = TRUE)
  expect_match(query, "LIMIT 100", fixed = TRUE)
  expect_match(query, "HY_IndirectPosition", fixed = TRUE)
  expect_false(grepl("{{", query, fixed = TRUE))
})

test_that("AOI query uses the GeoSPARQL function namespace", {
  query <- gx_render_query(
    "sites_in_aoi",
    list(
      aoi_wkt = "POLYGON((-108 35,-107 35,-107 36,-108 36,-108 35))",
      limit = 50,
      offset = 0
    )
  )
  expect_match(query, "PREFIX geof: <http://www.opengis.net/def/function/geosparql/>", fixed = TRUE)
  expect_match(query, "geof:sfIntersects", fixed = TRUE)
  expect_false(grepl("gsp:sfIntersects", query, fixed = TRUE))
})

test_that("query parameter injection and budget violations are rejected", {
  bad <- list(
    mainstem_uri = "https://example.org/> } DELETE { ?s ?p ?o } #",
    limit = 100,
    offset = 0
  )
  expect_error(
    gx_render_query("sites_on_mainstem", bad),
    class = "gx_error_query_parameter"
  )
  expect_error(
    gx_render_query(
      "sites_on_mainstem",
      list(
        mainstem_uri = "https://geoconnex.us/ref/mainstems/1",
        limit = 1001,
        offset = 0
      )
    ),
    class = "gx_error_query_parameter"
  )
  expect_error(
    gx_render_query(
      "sites_on_mainstem",
      list(
        mainstem_uri = "https://geoconnex.us/ref/mainstems/1",
        limit = 1000,
        offset = 9501
      )
    ),
    class = "gx_error_query_parameter"
  )
})
