test_that("portable handlers are ordered with an explicit fallback", {
  handlers <- gx_handlers()
  expect_s3_class(handlers, "tbl_df")
  expect_equal(nrow(handlers), 9L)
  expect_true(all(diff(handlers$precedence) > 0L))
  expect_equal(tail(handlers$id, 1L), "unknown")
  expect_equal(tail(handlers$outcome, 1L), "reference_only")
})

test_that("handler precedence does not let EDR shadow USGS or Features", {
  expect_equal(
    gx_classify_distribution(
      "https://api.waterdata.usgs.gov/ogcapi/beta/collections/daily/items"
    ),
    "usgs_waterdata_daily"
  )
  expect_equal(
    gx_classify_distribution(
      "https://reference.geoconnex.us/collections/gages/items"
    ),
    "ogc_api_features"
  )
  expect_equal(
    gx_classify_distribution(
      "https://example.org/collections/streamflow/cube"
    ),
    "edr"
  )
  expect_equal(
    gx_classify_distribution("https://example.org/data.csv"),
    "csv"
  )
  expect_equal(
    gx_classify_distribution("https://example.org/landing-page"),
    "unknown"
  )
})

test_that("unit conversions support affine and multiplicative rules", {
  rules <- gx_unit_conversions()
  expect_equal(nrow(rules), 8L)
  expect_true(all(rules$status == "reviewed"))

  f_to_c <- rules[rules$rule_id == "temperature-deg-f-to-deg-c", ]
  c_to_f <- rules[rules$rule_id == "temperature-deg-c-to-deg-f", ]
  expect_equal(32 * f_to_c$scale + f_to_c$offset, 0, tolerance = 1e-12)
  expect_equal(0 * c_to_f$scale + c_to_f$offset, 32, tolerance = 1e-12)

  ft_to_m <- rules[rules$rule_id == "length-ft-to-m", ]
  expect_equal(1 * ft_to_m$scale + ft_to_m$offset, 0.3048)
})

test_that("JSON schemas compile and shipped YAML assets validate", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("jsonvalidate")

  schema_dir <- system.file("schema", package = "geoconnexr")
  schemas <- list.files(schema_dir, pattern = "\\.json$", full.names = TRUE)
  expect_length(schemas, 4L)
  for (schema in schemas) {
    expect_no_error(jsonvalidate::json_validator(schema, engine = "ajv"))
  }

  query_manifest <- yaml::read_yaml(
    system.file("queries", "manifest.yml", package = "geoconnexr")
  )
  query_manifest$templates <- lapply(query_manifest$templates, function(x) {
    x$stable_order <- as.list(x$stable_order)
    x$result_key <- as.list(x$result_key)
    x
  })
  query_json <- jsonlite::toJSON(query_manifest, auto_unbox = TRUE, null = "null")
  expect_true(jsonvalidate::json_validate(
    query_json,
    file.path(schema_dir, "query-manifest-v1.json"),
    engine = "ajv"
  ))

  handler_registry <- yaml::read_yaml(
    system.file("handlers", "registry.yml", package = "geoconnexr")
  )
  handler_json <- jsonlite::toJSON(handler_registry, auto_unbox = TRUE, null = "null")
  expect_true(jsonvalidate::json_validate(
    handler_json,
    file.path(schema_dir, "handler-registry-v1.json"),
    engine = "ajv"
  ))
})

test_that("identifier AOI recipes satisfy the replay schema", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("jsonvalidate")
  recipe <- jsonlite::toJSON(gx_aoi("02070010")$recipe, auto_unbox = TRUE)
  expect_true(jsonvalidate::json_validate(
    recipe,
    system.file("schema", "recipe-v1.json", package = "geoconnexr"),
    engine = "ajv"
  ))

  bad_pipeline <- gx_aoi("02070010")$recipe
  bad_pipeline$pipeline <- list(
    start_stage = "harmonized",
    end_stage = "catalog"
  )
  expect_false(jsonvalidate::json_validate(
    jsonlite::toJSON(bad_pipeline, auto_unbox = TRUE),
    system.file("schema", "recipe-v1.json", package = "geoconnexr"),
    engine = "ajv"
  ))

  bad_geometry <- list(
    contract_version = "1.0.0",
    aoi = list(
      kind = "sf",
      canonical_geojson = list(type = "Point"),
      crs = "OGC:CRS84"
    ),
    pipeline = list(start_stage = "aoi", end_stage = "catalog")
  )
  expect_false(jsonvalidate::json_validate(
    jsonlite::toJSON(bad_geometry, auto_unbox = TRUE),
    system.file("schema", "recipe-v1.json", package = "geoconnexr"),
    engine = "ajv"
  ))
})

test_that("manifest resource paths exclude traversal and absolute forms", {
  skip_if_not_installed("jsonlite")
  schema <- jsonlite::fromJSON(
    system.file("schema", "manifest-v1.json", package = "geoconnexr"),
    simplifyVector = FALSE
  )
  pattern <- schema[["$defs"]]$resource$properties$path$pattern
  expect_true(grepl(pattern, "catalog/datasets.csv", perl = TRUE))
  expect_false(grepl(pattern, "../outside.csv", perl = TRUE))
  expect_false(grepl(pattern, "a/../../outside.csv", perl = TRUE))
  expect_false(grepl(pattern, "C:\\outside.csv", perl = TRUE))
  expect_false(grepl(pattern, "/outside.csv", perl = TRUE))
})

test_that("runtime implementation metadata covers every classifier honestly", {
  skip_if_not_installed("jsonlite")
  metadata <- jsonlite::fromJSON(
    system.file("handlers", "implementations-r.json", package = "geoconnexr"),
    simplifyVector = FALSE
  )
  expect_setequal(names(metadata$implementations), gx_handlers()$id)
  availability <- vapply(
    metadata$implementations,
    function(x) x$availability,
    character(1)
  )
  expect_true(all(availability %in% c("planned", "classifier_only")))
  expect_match(metadata$runtime_status, "metadata_only", fixed = TRUE)
})
