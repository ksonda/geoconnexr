harmonize_test_fetched <- function(
    label = "harmonize-live",
    edr_body = edr_test_body(),
    csv_body = csv_response_validation_test_body()) {
  performer <- fetch_orchestration_test_performer(
    edr_body = edr_body,
    csv_body = csv_body
  )
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
  gx_fetch_impl(
    plan = fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    limits = limits,
    orchestration_scope_id = fetch_orchestration_test_scope(label),
    oaf_symbol_resolver = oaf_test_resolver(),
    wqp_symbol_resolver = wqp_test_resolver(),
    edr_symbol_resolver = edr_test_resolver(),
    usgs_continuous_symbol_resolver = usgs_continuous_test_resolver(),
    usgs_daily_symbol_resolver = usgs_daily_test_resolver()
  )
}

harmonize_test_csv_fetched <- function(label = "harmonize-csv") {
  catalog <- csv_intents_test_fixture_catalog()
  position <- which(catalog$datasets$handler_id == "csv")[[1L]]
  catalog$datasets$unit_uri[[position]] <-
    "http://qudt.org/vocab/unit/M3-PER-SEC"
  catalog$datasets$unit_label[[position]] <- "m3/s"
  catalog <- gx_catalog_new_impl(
    aoi = catalog$aoi,
    sites = catalog$sites,
    datasets = catalog$datasets,
    reference = catalog$reference,
    problems = catalog$problems,
    requests = catalog$requests,
    metadata = fetch_plan_test_metadata(catalog$sites, catalog$datasets)
  )
  withr::local_options(list(geoconnexr.clock = fetch_plan_test_now))
  plan <- gx_fetch_plan(
    catalog,
    time = fetch_plan_test_time(),
    max_datasets = 5L,
    max_bytes = 100000
  )
  path <- testthat::test_path(
    "fixtures", "fetch", "csv-response-validation",
    "observations-mapped-v1.csv"
  )
  csv_body <- readBin(path, what = "raw", n = file.info(path)$size)
  performer <- fetch_orchestration_test_performer(csv_body = csv_body)
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
  gx_fetch_impl(
    plan = plan,
    limits = limits,
    orchestration_scope_id = fetch_orchestration_test_scope(label),
    oaf_symbol_resolver = oaf_test_resolver(),
    wqp_symbol_resolver = wqp_test_resolver(),
    edr_symbol_resolver = edr_test_resolver(),
    usgs_continuous_symbol_resolver = usgs_continuous_test_resolver(),
    usgs_daily_symbol_resolver = usgs_daily_test_resolver()
  )
}

harmonize_test_wqp_fetched <- function(
    label = "harmonize-wqp",
    fixture = "result-temperature-utc.csv") {
  catalog <- csv_intents_test_fixture_catalog()
  position <- which(catalog$datasets$handler_id == "wqp")
  catalog$datasets$distribution_url[[position]] <- paste0(
    "https://www.waterqualitydata.us/data/Result/search?",
    "siteid=USGS-01234567&characteristicName=Temperature&mimeType=csv"
  )
  catalog$datasets$variable_uri[[position]] <-
    "http://qudt.org/vocab/quantitykind/Temperature"
  catalog$datasets$variable_name[[position]] <- "Temperature"
  catalog$datasets$unit_uri[[position]] <-
    "http://qudt.org/vocab/unit/DEG_C"
  catalog$datasets$unit_label[[position]] <- "deg C"
  catalog <- gx_catalog_new_impl(
    aoi = catalog$aoi,
    sites = catalog$sites,
    datasets = catalog$datasets,
    reference = catalog$reference,
    problems = catalog$problems,
    requests = catalog$requests,
    metadata = fetch_plan_test_metadata(catalog$sites, catalog$datasets)
  )
  withr::local_options(list(geoconnexr.clock = fetch_plan_test_now))
  plan <- gx_fetch_plan(
    catalog,
    time = fetch_plan_test_time(),
    max_datasets = 5L,
    max_bytes = 100000
  )
  performer <- fetch_orchestration_test_performer(
    wqp_body = wqp_test_body(fixture)
  )
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
  gx_fetch_impl(
    plan = plan,
    limits = limits,
    orchestration_scope_id = fetch_orchestration_test_scope(label),
    oaf_symbol_resolver = oaf_test_resolver(),
    wqp_symbol_resolver = wqp_test_resolver(),
    edr_symbol_resolver = edr_test_resolver(),
    usgs_continuous_symbol_resolver = usgs_continuous_test_resolver(),
    usgs_daily_symbol_resolver = usgs_daily_test_resolver()
  )
}

harmonize_test_feature_fetched <- function(
    label = "harmonize-feature",
    fixture = "items-observations.geojson") {
  catalog <- csv_intents_test_fixture_catalog()
  position <- which(
    catalog$datasets$handler_id == "ogc_api_features"
  )
  catalog$datasets$distribution_url[[position]] <-
    "https://reference.geoconnex.us/collections/gages/items"
  catalog$datasets$media_type[[position]] <- "application/geo+json"
  catalog$datasets$unit_uri[[position]] <-
    "http://qudt.org/vocab/unit/M3-PER-SEC"
  catalog$datasets$unit_label[[position]] <- "m3/s"
  catalog <- gx_catalog_new_impl(
    aoi = catalog$aoi,
    sites = catalog$sites,
    datasets = catalog$datasets,
    reference = catalog$reference,
    problems = catalog$problems,
    requests = catalog$requests,
    metadata = fetch_plan_test_metadata(catalog$sites, catalog$datasets)
  )
  withr::local_options(list(geoconnexr.clock = fetch_plan_test_now))
  plan <- gx_fetch_plan(
    catalog,
    time = fetch_plan_test_time(),
    max_datasets = 5L,
    max_bytes = 100000
  )
  performer <- fetch_orchestration_test_performer(
    oaf_body = oaf_test_body(fixture)
  )
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
  gx_fetch_impl(
    plan = plan,
    limits = limits,
    orchestration_scope_id = fetch_orchestration_test_scope(label),
    oaf_symbol_resolver = oaf_test_resolver(),
    wqp_symbol_resolver = wqp_test_resolver(),
    edr_symbol_resolver = edr_test_resolver(),
    usgs_continuous_symbol_resolver = usgs_continuous_test_resolver(),
    usgs_daily_symbol_resolver = usgs_daily_test_resolver()
  )
}

test_that("reviewed targets are exact, dimensioned, and asset-bound", {
  targets <- gx_target_units()
  expect_s3_class(targets, "gx_target_units")
  expect_identical(targets$contract_version, "0.1.0")
  expect_identical(
    targets$units$unit_uri,
    c(
      "http://qudt.org/vocab/unit/DEG_C",
      "http://qudt.org/vocab/unit/M",
      "http://qudt.org/vocab/unit/M3-PER-SEC"
    )
  )
  imperial <- gx_target_units(
    thermodynamic_temperature = "http://qudt.org/vocab/unit/DEG_F",
    length = "http://qudt.org/vocab/unit/FT",
    volume_flow_rate = "http://qudt.org/vocab/unit/FT3-PER-SEC"
  )
  expect_identical(
    imperial$units$unit_label,
    c("deg F", "ft", "ft^3/s")
  )
  expect_identical(
    gx_target_units_validate_impl(imperial), invisible(imperial)
  )

  expect_error(
    gx_target_units(length = "http://qudt.org/vocab/unit/DEG_C"),
    class = "gx_error_target_units_selection"
  )
  expect_error(
    gx_target_units(volume_flow_rate = "https://example.org/unit/unknown"),
    class = "gx_error_target_units_selection"
  )
  forged <- unserialize(serialize(targets, NULL))
  forged$asset_sha256 <- paste(rep("0", 64L), collapse = "")
  expect_error(
    gx_target_units_validate_impl(forged),
    class = "gx_error_target_units_contract"
  )
})

test_that("reviewed affine and multiplicative conversions work both ways", {
  metric <- gx_target_units()
  imperial <- gx_target_units(
    thermodynamic_temperature = "http://qudt.org/vocab/unit/DEG_F",
    length = "http://qudt.org/vocab/unit/FT",
    volume_flow_rate = "http://qudt.org/vocab/unit/FT3-PER-SEC"
  )
  convert <- function(value, source_uri, source_label, targets) {
    gx_harmonize_value_impl(
      value = value,
      source_unit_uri = source_uri,
      source_unit_label = source_label,
      variable_mapped = TRUE,
      unit_corroborated = TRUE,
      target_units = targets
    )
  }

  f_to_c <- convert(
    32, "http://qudt.org/vocab/unit/DEG_F", "deg F", metric
  )
  c_to_f <- convert(
    100, "http://qudt.org/vocab/unit/DEG_C", "deg C", imperial
  )
  ft_to_m <- convert(
    10, "http://qudt.org/vocab/unit/FT", "ft", metric
  )
  flow_to_imperial <- convert(
    1, "http://qudt.org/vocab/unit/M3-PER-SEC", "m3/s", imperial
  )

  expect_equal(f_to_c$value, 0, tolerance = 1e-12)
  expect_identical(
    f_to_c$conversion_rule_id, "temperature-deg-f-to-deg-c"
  )
  expect_equal(c_to_f$value, 212, tolerance = 1e-12)
  expect_equal(ft_to_m$value, 3.048, tolerance = 1e-12)
  expect_equal(flow_to_imperial$value, 35.31466672148859, tolerance = 1e-12)
  expect_true(all(vapply(
    list(f_to_c, c_to_f, ft_to_m, flow_to_imperial),
    `[[`, logical(1), "harmonized"
  )))
})

test_that("direct-CSV mappings are exact, bounded, and identity-bound", {
  distribution_id <- paste(rep("a", 64L), collapse = "")
  mapping <- gx_csv_mapping(
    distribution_id = distribution_id,
    datetime_column = "datetime",
    value_column = "value",
    unit_column = "unit",
    qualifier_column = "qualifier",
    missing_values = c("NA", "")
  )
  expect_s3_class(mapping, "gx_csv_mapping")
  expect_identical(mapping$contract_version, "0.1.0")
  expect_identical(mapping$missing_values, c("", "NA"))
  expect_match(mapping$mapping_id, "^[0-9a-f]{64}$")
  expect_identical(
    gx_csv_mapping_validate_impl(mapping), invisible(mapping)
  )

  expect_error(
    gx_csv_mapping(
      distribution_id,
      datetime_column = "value",
      value_column = "value",
      unit_column = "unit"
    ),
    class = "gx_error_csv_mapping"
  )
  expect_error(
    gx_csv_mapping(
      "not-a-distribution",
      datetime_column = "datetime",
      value_column = "value",
      unit_column = "unit"
    ),
    class = "gx_error_csv_mapping"
  )
  forged <- unserialize(serialize(mapping, NULL))
  forged$columns$value <- "forged"
  expect_error(
    gx_csv_mapping_validate_impl(forged),
    class = "gx_error_csv_mapping"
  )
  expect_identical(
    format(
      gx_harmonize_csv_datetime_impl("2025-06-01T12:34:56Z"),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    "2025-06-01T12:34:56Z"
  )
  expect_null(gx_harmonize_csv_datetime_impl(
    "2025-02-30T12:34:56Z"
  ))
  expect_null(gx_harmonize_csv_datetime_impl(
    "2025-06-01T12:34:56-04:00"
  ))
})

test_that("feature mappings are exact, bounded, and identity-bound", {
  distribution_id <- paste(rep("b", 64L), collapse = "")
  mapping <- gx_feature_mapping(
    distribution_id = distribution_id,
    datetime_property = "observed_at",
    value_property = "result_value",
    unit_property = "result_unit",
    qualifier_property = "result_qualifier",
    missing_values = c("NA", "")
  )
  expect_s3_class(mapping, "gx_feature_mapping")
  expect_identical(mapping$contract_version, "0.1.0")
  expect_identical(mapping$missing_values, c("", "NA"))
  expect_match(mapping$mapping_id, "^[0-9a-f]{64}$")
  expect_identical(
    gx_feature_mapping_validate_impl(mapping), invisible(mapping)
  )

  expect_error(
    gx_feature_mapping(
      distribution_id,
      datetime_property = "result_value",
      value_property = "result_value",
      unit_property = "result_unit"
    ),
    class = "gx_error_feature_mapping"
  )
  expect_error(
    gx_feature_mapping(
      distribution_id,
      datetime_property = "observed_at",
      value_property = "geometry",
      unit_property = "result_unit"
    ),
    class = "gx_error_feature_mapping"
  )
  expect_error(
    gx_feature_mapping(
      "not-a-distribution",
      datetime_property = "observed_at",
      value_property = "result_value",
      unit_property = "result_unit"
    ),
    class = "gx_error_feature_mapping"
  )
  forged <- unserialize(serialize(mapping, NULL))
  forged$properties$value <- "forged"
  expect_error(
    gx_feature_mapping_validate_impl(forged),
    class = "gx_error_feature_mapping"
  )
})

test_that("missing, ambiguous, conflicting, and unavailable mappings stay visible", {
  targets <- gx_target_units()
  base <- list(
    value = "1.5",
    source_unit_uri = "http://qudt.org/vocab/unit/FT",
    source_unit_label = "ft",
    variable_mapped = TRUE,
    unit_corroborated = TRUE,
    target_units = targets
  )
  ambiguous <- do.call(
    gx_harmonize_value_impl,
    utils::modifyList(base, list(variable_mapped = FALSE))
  )
  conflict <- do.call(
    gx_harmonize_value_impl,
    utils::modifyList(base, list(unit_corroborated = FALSE))
  )
  invalid <- do.call(
    gx_harmonize_value_impl,
    utils::modifyList(base, list(value = "1,500"))
  )
  unavailable <- do.call(
    gx_harmonize_value_impl,
    utils::modifyList(base, list(
      source_unit_uri = "https://example.org/unit/unknown"
    ))
  )
  missing <- do.call(
    gx_harmonize_value_impl,
    utils::modifyList(base, list(value = NA_real_))
  )

  expect_identical(ambiguous$status, "variable_ambiguous")
  expect_identical(conflict$status, "unit_conflict")
  expect_identical(invalid$status, "invalid_value")
  expect_identical(unavailable$status, "unit_unmapped")
  expect_false(any(vapply(
    list(ambiguous, conflict, invalid, unavailable),
    `[[`, logical(1), "harmonized"
  )))
  expect_true(missing$harmonized)
  expect_identical(missing$status, "harmonized_missing")
  expect_true(is.na(missing$value))
})

test_that("public harmonization preserves native payloads and conservative mappings", {
  fetched <- harmonize_test_fetched()
  touched <- new.env(parent = emptyenv())
  touched$count <- 0L
  forbidden <- function(...) {
    touched$count <- touched$count + 1L
    stop("harmonization touched host state", call. = FALSE)
  }
  withr::local_options(list(
    geoconnexr.performer = forbidden,
    geoconnexr.dns_resolver = forbidden,
    geoconnexr.clock = forbidden,
    geoconnexr.throttle_clock = forbidden,
    geoconnexr.throttle_sleep = forbidden
  ))
  harmonized <- gx_harmonize(fetched)

  expect_identical(touched$count, 0L)
  expect_s3_class(harmonized, "gx_harmonized")
  expect_identical(harmonized$fetched, fetched)
  expect_identical(harmonized$contract_version, "0.5.0")
  expect_identical(nrow(harmonized$observations), 4L)
  expect_identical(
    harmonized$observations$handler_id,
    c("edr", "edr", "usgs_waterdata_daily", "usgs_waterdata_daily")
  )
  expect_identical(
    harmonized$observations$status,
    c("unit_unmapped", "unit_unmapped", "unit_conflict", "unit_conflict")
  )
  expect_false(any(harmonized$observations$harmonized))
  expect_identical(
    harmonized$observations$original_value,
    c("1.25", NA_character_, "167.5", "170")
  )
  expect_identical(
    tail(harmonized$observations$qualifier, 2L),
    c(NA_character_, "P")
  )
  expect_s3_class(harmonized$observations$datetime, "POSIXct")
  expect_identical(attr(harmonized$observations$datetime, "tzone"), "UTC")
  expect_identical(
    format(
      tail(harmonized$observations$datetime, 2L),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    c("2025-06-01T00:00:00Z", "2025-06-02T00:00:00Z")
  )
  expect_identical(
    harmonized$metadata$counts,
    list(
      resources = 7L,
      timeseries_resources = 2L,
      native_only_resources = 5L,
      observations = 4L,
      harmonized = 0L,
      unchanged = 4L
    )
  )
  expect_identical(
    gx_harmonized_validate_impl(harmonized), invisible(harmonized)
  )
})

test_that("explicit direct-CSV mappings normalize one exact distribution", {
  fetched <- harmonize_test_csv_fetched()
  csv_results <- fetched$results$handler_id == "csv"
  distribution_id <- fetched$results$distribution_id[which(csv_results)[[1L]]]
  mapping <- gx_csv_mapping(
    distribution_id = distribution_id,
    datetime_column = "datetime",
    value_column = "value",
    unit_column = "unit",
    qualifier_column = "qualifier",
    missing_values = c("", "NA")
  )
  harmonized <- gx_harmonize(fetched, csv_mappings = mapping)
  csv <- harmonized$observations$handler_id == "csv"
  resources <- harmonized$resources$handler_id == "csv"
  mapped_resource <- harmonized$resources$distribution_id == distribution_id

  expect_identical(harmonized$csv_mappings, list(mapping))
  expect_identical(sum(csv), 2L)
  expect_identical(
    harmonized$observations$original_value[csv], c("4.5", "NA")
  )
  expect_equal(
    harmonized$observations$value[csv], c(4.5, NA_real_),
    tolerance = 1e-12
  )
  expect_identical(harmonized$observations$status[csv], c(
    "harmonized_identity", "harmonized_missing"
  ))
  expect_identical(
    harmonized$observations$qualifier[csv], c(NA_character_, "P")
  )
  expect_identical(
    format(
      harmonized$observations$datetime[csv],
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    c("2025-06-01T00:00:00Z", "2025-06-01T01:00:00Z")
  )
  expect_identical(sum(resources), 3L)
  expect_identical(sum(harmonized$resources$timeseries[resources]), 1L)
  expect_true(harmonized$resources$timeseries[[which(mapped_resource)]])
  expect_identical(
    harmonized$resources$observation_count[[which(mapped_resource)]], 2L
  )
  expect_identical(harmonized$fetched, fetched)

  forged <- unserialize(serialize(harmonized, NULL))
  forged$csv_mappings[[1L]]$mapping_id <- paste(rep("0", 64L), collapse = "")
  expect_error(
    gx_harmonized_validate_impl(forged),
    class = "gx_error_harmonize"
  )
})

test_that("direct-CSV mappings fail closed on binding and schema mismatch", {
  fetched <- harmonize_test_csv_fetched("harmonize-csv-rejection")
  wqp_id <- fetched$results$distribution_id[
    which(fetched$results$handler_id == "wqp")[[1L]]
  ]
  invalid_binding <- gx_csv_mapping(
    distribution_id = wqp_id,
    datetime_column = "datetime",
    value_column = "value",
    unit_column = "unit"
  )
  expect_error(
    gx_harmonize(fetched, csv_mappings = invalid_binding),
    class = "gx_error_csv_mapping_binding"
  )

  csv_id <- fetched$results$distribution_id[
    which(fetched$results$handler_id == "csv")[[1L]]
  ]
  incompatible <- gx_csv_mapping(
    distribution_id = csv_id,
    datetime_column = "unknown_datetime",
    value_column = "value",
    unit_column = "unit"
  )
  harmonized <- gx_harmonize(fetched, csv_mappings = incompatible)
  resource <- harmonized$resources$distribution_id == csv_id
  expect_false(harmonized$resources$timeseries[[which(resource)]])
  expect_identical(
    harmonized$resources$status[[which(resource)]], "native_only"
  )
  expect_false(any(
    harmonized$observations$distribution_id == csv_id
  ))
})

test_that("explicit feature mappings normalize one exact distribution", {
  fetched <- harmonize_test_feature_fetched()
  feature_results <- fetched$results$handler_id == "ogc_api_features"
  distribution_id <- fetched$results$distribution_id[
    which(feature_results)[[1L]]
  ]
  mapping <- gx_feature_mapping(
    distribution_id = distribution_id,
    datetime_property = "observed_at",
    value_property = "result_value",
    unit_property = "result_unit",
    qualifier_property = "result_qualifier",
    missing_values = c("", "NA")
  )
  harmonized <- gx_harmonize(fetched, feature_mappings = mapping)
  feature <- harmonized$observations$handler_id == "ogc_api_features"
  resource <- harmonized$resources$distribution_id == distribution_id
  distribution <- fetched$plan$distributions[
    fetched$plan$distributions$distribution_id == distribution_id,
    ,
    drop = FALSE
  ]

  expect_identical(harmonized$feature_mappings, list(mapping))
  expect_identical(sum(feature), 2L)
  expect_identical(
    harmonized$observations$original_value[feature], c("4.5", "NA")
  )
  expect_equal(
    harmonized$observations$value[feature], c(4.5, NA_real_),
    tolerance = 1e-12
  )
  expect_identical(harmonized$observations$status[feature], c(
    "harmonized_identity", "harmonized_missing"
  ))
  expect_identical(
    harmonized$observations$qualifier[feature],
    c(NA_character_, "P")
  )
  expect_identical(
    format(
      harmonized$observations$datetime[feature],
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    c("2025-06-01T00:00:00Z", "2025-06-01T01:00:00Z")
  )
  expect_identical(
    unique(harmonized$observations$site_uri[feature]),
    distribution$site_uri
  )
  expect_true(harmonized$resources$timeseries[[which(resource)]])
  expect_identical(
    harmonized$resources$observation_count[[which(resource)]], 2L
  )
  expect_identical(
    harmonized$resources$status[[which(resource)]],
    "observations_extracted"
  )
  expect_identical(harmonized$fetched, fetched)

  forged <- unserialize(serialize(harmonized, NULL))
  forged$feature_mappings[[1L]]$mapping_id <-
    paste(rep("0", 64L), collapse = "")
  expect_error(
    gx_harmonized_validate_impl(forged),
    class = "gx_error_harmonize"
  )
})

test_that("feature mappings fail closed on binding and schema mismatch", {
  fetched <- harmonize_test_feature_fetched(
    "harmonize-feature-rejection"
  )
  csv_id <- fetched$results$distribution_id[
    which(fetched$results$handler_id == "csv")[[1L]]
  ]
  invalid_binding <- gx_feature_mapping(
    distribution_id = csv_id,
    datetime_property = "observed_at",
    value_property = "result_value",
    unit_property = "result_unit"
  )
  expect_error(
    gx_harmonize(fetched, feature_mappings = invalid_binding),
    class = "gx_error_feature_mapping_binding"
  )

  feature_id <- fetched$results$distribution_id[
    which(fetched$results$handler_id == "ogc_api_features")[[1L]]
  ]
  incompatible <- gx_feature_mapping(
    distribution_id = feature_id,
    datetime_property = "unknown_datetime",
    value_property = "result_value",
    unit_property = "result_unit"
  )
  harmonized <- gx_harmonize(fetched, feature_mappings = incompatible)
  resource <- harmonized$resources$distribution_id == feature_id
  expect_false(harmonized$resources$timeseries[[which(resource)]])
  expect_identical(
    harmonized$resources$status[[which(resource)]], "native_only"
  )
  expect_false(any(
    harmonized$observations$distribution_id == feature_id
  ))

  valid <- gx_feature_mapping(
    distribution_id = feature_id,
    datetime_property = "observed_at",
    value_property = "result_value",
    unit_property = "result_unit",
    qualifier_property = "result_qualifier",
    missing_values = c("", "NA")
  )
  result <- fetched$results[
    fetched$results$distribution_id == feature_id,
    ,
    drop = FALSE
  ]
  offset_body <- charToRaw(sub(
    "2025-06-01T00:00:00Z",
    "2025-06-01T00:00:00-04:00",
    rawToChar(result$raw_body[[1L]]),
    fixed = TRUE
  ))
  expect_false(gx_harmonize_feature_native_fields_impl(
    result$data[[1L]], offset_body, valid
  )$supported)
})

test_that("filtered UTC WQP results align through exact catalog facts", {
  fetched <- harmonize_test_wqp_fetched()
  imperial <- gx_target_units(
    thermodynamic_temperature = "http://qudt.org/vocab/unit/DEG_F"
  )
  harmonized <- gx_harmonize(fetched, target_units = imperial)
  wqp <- harmonized$observations$handler_id == "wqp"
  resource <- harmonized$resources$handler_id == "wqp"

  expect_identical(sum(wqp), 2L)
  expect_identical(harmonized$observations$variable_name[wqp], c(
    "Temperature", "Temperature"
  ))
  expect_identical(harmonized$observations$original_value[wqp], c(
    "21.2", "NA"
  ))
  expect_equal(
    harmonized$observations$value[wqp],
    c(70.16, NA_real_),
    tolerance = 1e-12
  )
  expect_identical(harmonized$observations$unit_label[wqp], c(
    "deg F", "deg F"
  ))
  expect_identical(harmonized$observations$status[wqp], c(
    "harmonized_converted", "harmonized_missing"
  ))
  expect_identical(harmonized$observations$qualifier[wqp], c(
    NA_character_, "P"
  ))
  expect_identical(
    format(
      harmonized$observations$datetime[wqp],
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    c("2025-06-01T12:34:56Z", "2025-06-01T12:34:56Z")
  )
  expect_true(harmonized$resources$timeseries[[which(resource)]])
  expect_identical(
    harmonized$resources$observation_count[[which(resource)]], 2L
  )
  expect_identical(
    harmonized$resources$status[[which(resource)]],
    "observations_extracted"
  )
  expect_identical(harmonized$fetched, fetched)

  result <- fetched$results[
    fetched$results$handler_id == "wqp", , drop = FALSE
  ]
  distribution <- fetched$plan$distributions[
    fetched$plan$distributions$distribution_id ==
      result$distribution_id[[1L]],
    ,
    drop = FALSE
  ]
  parameters <- fetched$plan$parameters[
    fetched$plan$parameters$distribution_id ==
      result$distribution_id[[1L]],
    ,
    drop = FALSE
  ]
  native <- result$data[[1L]]
  mixed <- native
  mixed$CharacteristicName[[2L]] <- "pH"
  wrong_site <- native
  wrong_site$MonitoringLocationIdentifier[[1L]] <- "USGS-76543210"
  incomplete <- native[
    setdiff(names(native), "ActivityStartTime.TimeZoneCode")
  ]
  expect_false(gx_harmonize_wqp_native_fields_impl(
    mixed, distribution, parameters
  )$supported)
  expect_false(gx_harmonize_wqp_native_fields_impl(
    wrong_site, distribution, parameters
  )$supported)
  expect_false(gx_harmonize_wqp_native_fields_impl(
    incomplete, distribution, parameters
  )$supported)
})

test_that("reviewed WQP timezone codes normalize exact fixed offsets", {
  asset <- gx_wqp_timezone_asset_impl()
  expect_identical(asset$zones$code, .gx_wqp_timezone_codes)
  expect_identical(nrow(asset$zones), 23L)
  expect_false(any(c("AHST", "BST", "YST") %in% asset$zones$code))
  expect_match(asset$asset_sha256, "^[0-9a-f]{64}$")

  datetime <- gx_harmonize_wqp_datetime_impl(
    date = rep("2025-06-01", 4L),
    time = c("00:00:00", "09:00:00", "08:34:56", "09:04:56"),
    zone = c("UTC", "KST", "EDT", "NST")
  )
  expect_identical(
    format(datetime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    c(
      "2025-06-01T00:00:00Z", "2025-06-01T00:00:00Z",
      "2025-06-01T12:34:56Z", "2025-06-01T12:34:56Z"
    )
  )

  fetched <- harmonize_test_wqp_fetched(
    label = "harmonize-wqp-reviewed-zones",
    fixture = "result-temperature-reviewed-zones.csv"
  )
  harmonized <- gx_harmonize(fetched)
  wqp <- harmonized$observations$handler_id == "wqp"
  expect_identical(sum(wqp), 2L)
  expect_identical(
    format(
      harmonized$observations$datetime[wqp],
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    c("2025-06-01T12:34:56Z", "2025-06-01T12:34:56Z")
  )
  expect_identical(
    harmonized$metadata$wqp_timezone_asset_sha256,
    asset$asset_sha256
  )
  expect_identical(harmonized$fetched, fetched)
})

test_that("WQP alignment rejects unfiltered or unknown-timezone semantics", {
  expect_null(gx_harmonize_wqp_query_impl(
    paste0(
      "https://www.waterqualitydata.us/data/Result/search?",
      "siteid=USGS-01234567&mimeType=csv"
    )
  ))
  expect_null(gx_harmonize_wqp_datetime_impl(
    "2025-06-01", "12:34:56", "XYZ"
  ))
  expect_null(gx_harmonize_wqp_datetime_impl(
    "2025-06-01", "12:34:56", "AHST"
  ))
  expect_null(gx_harmonize_wqp_datetime_impl(
    "2025-02-30", "12:34:56", "UTC"
  ))

  harmonized <- gx_harmonize(
    harmonize_test_fetched(label = "harmonize-wqp-native-only")
  )
  wqp <- harmonized$resources$handler_id == "wqp"
  expect_false(harmonized$resources$timeseries[[which(wqp)]])
  expect_identical(
    harmonized$resources$status[[which(wqp)]], "native_only"
  )
  expect_identical(
    harmonized$resources$observation_count[[which(wqp)]], 0L
  )
})

test_that("duplicate timestamps remain in source order", {
  text <- rawToChar(edr_test_body())
  duplicate <- charToRaw(sub(
    "2025-06-01T01:00:00Z",
    "2025-06-01T00:00:00Z",
    text,
    fixed = TRUE
  ))
  fetched <- harmonize_test_fetched(
    label = "harmonize-duplicate",
    edr_body = duplicate
  )
  harmonized <- gx_harmonize(fetched)
  edr <- harmonized$observations$handler_id == "edr"

  expect_identical(sum(edr), 2L)
  expect_identical(
    as.numeric(harmonized$observations$datetime[edr]),
    rep(as.numeric(harmonized$observations$datetime[edr][[1L]]), 2L)
  )
  expect_identical(harmonized$observations$native_row[edr], 1:2)
  expect_true(harmonized$metadata$duplicates_preserved)
})

test_that("dry-run harmonization is an exact empty offline result", {
  plan <- fetch_orchestration_test_usgs_daily_plan()$intent_set$plan
  fetched <- gx_fetch(plan, dry_run = TRUE)
  harmonized <- gx_harmonize(fetched)

  expect_identical(nrow(harmonized$observations), 0L)
  expect_identical(nrow(harmonized$resources), 0L)
  expect_identical(harmonized$metadata$counts$observations, 0L)
  expect_identical(harmonized$fetched, fetched)
})

test_that("harmonized results fail closed on forgery", {
  harmonized <- gx_harmonize(
    harmonize_test_fetched(label = "harmonize-forgery")
  )
  mutations <- list(
    observation = function(x) {
      x$observations$status[[1L]] <- "forged"
      x
    },
    resource = function(x) {
      x$resources$status[[1L]] <- "forged"
      x
    },
    target = function(x) {
      x$target_units$units$unit_label[[1L]] <- "forged"
      x
    },
    metadata = function(x) {
      x$metadata$offline <- FALSE
      x
    },
    fetched = function(x) {
      x$fetched$status$status[[1L]] <- "forged"
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(harmonized, NULL)))
    expect_error(
      gx_harmonized_validate_impl(forged),
      class = "gx_error_harmonize",
      info = name
    )
  }
})

test_that("M9j binds exact catalog, fetched, and harmonized package inputs", {
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- harmonize_test_fetched(label = "package-input")
  harmonized <- gx_harmonize(fetched)

  catalog_input <- gx_package_input_impl(catalog)
  fetched_input <- gx_package_input_impl(fetched, catalog)
  harmonized_input <- gx_package_input_impl(harmonized, catalog)

  expect_s3_class(catalog_input, "gx_package_input")
  expect_s3_class(fetched_input, "gx_package_input")
  expect_s3_class(harmonized_input, "gx_package_input")
  expect_identical(
    vapply(
      list(catalog_input, fetched_input, harmonized_input),
      `[[`,
      character(1),
      "stage"
    ),
    c("catalog", "fetched", "harmonized")
  )
  expect_identical(catalog_input$catalog, catalog)
  expect_null(catalog_input$fetched)
  expect_null(catalog_input$harmonized)
  expect_identical(fetched_input$fetched, fetched)
  expect_null(fetched_input$harmonized)
  expect_identical(harmonized_input$fetched, fetched)
  expect_identical(harmonized_input$harmonized, harmonized)
  expect_identical(
    names(catalog_input$evidence$catalog_resources),
    c("sites", "datasets", "problems", "requests")
  )
  expect_true(all(vapply(
    catalog_input$evidence$catalog_resources,
    gx_catalog_is_sha256,
    logical(1)
  )))
  expect_true(is.na(catalog_input$evidence$fetch_plan_sha256))
  expect_true(is.na(fetched_input$evidence$harmonization_sha256))
  expect_true(gx_catalog_is_sha256(
    harmonized_input$evidence$harmonization_sha256
  ))
  expect_identical(
    harmonized_input$metadata$catalog_lineage,
    "explicit_rebound"
  )
  expect_true(harmonized_input$metadata$native_payloads_preserved)
  expect_false(harmonized_input$metadata$serializes)
  expect_false(harmonized_input$metadata$publishes)
  expect_false(harmonized_input$metadata$replayable)
  expect_identical(
    harmonized_input$metadata$counts$observations,
    as.integer(nrow(harmonized$observations))
  )
  expect_false(identical(catalog_input$input_id, fetched_input$input_id))
  expect_false(identical(fetched_input$input_id, harmonized_input$input_id))
  for (input in list(catalog_input, fetched_input, harmonized_input)) {
    expect_identical(
      gx_package_input_validate_impl(input),
      invisible(input)
    )
  }
})

test_that("M9j rejects missing or mismatched catalog lineage", {
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  harmonized <- gx_harmonize(fetched)

  expect_error(
    gx_package_input_impl(fetched),
    class = "gx_error_package_input"
  )
  expect_error(
    gx_package_input_impl(harmonized),
    class = "gx_error_package_input"
  )
  expect_error(
    gx_package_input_impl(catalog, catalog),
    class = "gx_error_package_input"
  )
  expect_error(
    gx_package_input_impl(fetched, csv_intents_test_fixture_catalog()),
    class = "gx_error_package_input_lineage"
  )
  expect_error(
    gx_package_input_impl(list(), catalog),
    class = "gx_error_package_input"
  )
})

test_that("M9j package-input forgery fails closed", {
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  harmonized <- gx_harmonize(fetched)
  input <- gx_package_input_impl(harmonized, catalog)
  mutations <- list(
    stage = function(x) {
      x$stage <- "fetched"
      x
    },
    catalog = function(x) {
      x$catalog$datasets$dataset_name[[1L]] <- "forged"
      x
    },
    fetched = function(x) {
      x$fetched$status$status[[1L]] <- "forged"
      x
    },
    harmonized = function(x) {
      x$harmonized$metadata$offline <- FALSE
      x
    },
    evidence = function(x) {
      x$evidence$fetched_status_sha256 <-
        paste(rep("0", 64L), collapse = "")
      x
    },
    metadata = function(x) {
      x$metadata$replayable <- TRUE
      x
    },
    identity = function(x) {
      x$input_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(input, NULL)))
    expect_error(
      gx_package_input_validate_impl(forged),
      class = "gx_error_package_input",
      info = name
    )
  }
})

test_that("M9j package-input admission is offline and read-only", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or write seam", call. = FALSE)
  }
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  harmonized <- gx_harmonize(fetched)

  expect_no_error(testthat::with_mocked_bindings(
    gx_package_input_impl(harmonized, catalog),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_snapshot_writer_write_csv = blocked,
    gx_snapshot_writer_write_raw = blocked,
    gx_snapshot_writer_unlink = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9k serializes populated fixed package resources deterministically", {
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- harmonize_test_fetched(label = "package-resources")
  harmonized <- gx_harmonize(fetched)
  input <- gx_package_input_impl(harmonized, catalog)

  bundle <- gx_package_resources_impl(input)

  expect_s3_class(bundle, "gx_package_resources")
  expect_identical(bundle$stage, "harmonized")
  expect_identical(bundle$input, input)
  expect_identical(nrow(bundle$resources), 15L)
  expect_identical(bundle$resources$path, sort(
    bundle$resources$path,
    method = "radix"
  ))
  expect_identical(names(bundle$contents), bundle$resources$path)
  expect_true(all(vapply(bundle$contents, function(content) {
    is.raw(content) && !is.object(content) && is.null(attributes(content))
  }, logical(1))))
  expect_identical(
    unname(vapply(bundle$contents, length, integer(1))),
    as.integer(bundle$resources$bytes)
  )
  expect_identical(
    vapply(
      bundle$contents,
      digest::digest,
      character(1),
      algo = "sha256",
      serialize = FALSE
    ),
    stats::setNames(bundle$resources$sha256, bundle$resources$path)
  )
  catalog_paths <- c(
    sites = "catalog/sites.csv",
    datasets = "catalog/datasets.csv",
    problems = "catalog/problems.csv",
    requests = "requests.csv"
  )
  for (resource in names(catalog_paths)) {
    position <- match(catalog_paths[[resource]], bundle$resources$path)
    expect_identical(
      bundle$resources$sha256[[position]],
      unname(input$evidence$catalog_resources[[resource]])
    )
  }
  raw_results <- which(fetched$results$raw_body_available)
  for (position in raw_results) {
    resource <- which(
      bundle$resources$result_id == fetched$results$result_id[[position]]
    )
    expect_identical(length(resource), 1L)
    expect_identical(bundle$resources$format[[resource]], "raw")
    expect_identical(
      bundle$contents[[resource]],
      fetched$results$raw_body[[position]]
    )
  }
  csv_results <- which(!fetched$results$raw_body_available)
  for (position in csv_results) {
    resource <- which(
      bundle$resources$result_id == fetched$results$result_id[[position]]
    )
    expect_identical(length(resource), 1L)
    expect_identical(bundle$resources$format[[resource]], "csv")
    expect_true(endsWith(rawToChar(bundle$contents[[resource]]), "\n"))
  }
  expect_true(all(c(
    "catalog/fetch_status.csv", "data/native/index.csv",
    "data/observations.csv", "data/harmonized_resources.csv"
  ) %in% bundle$resources$path))
  expect_identical(bundle$metadata$counts, list(
    resources = 15L,
    catalog_resources = 4L,
    fetch_resources = 2L,
    native_resources = 7L,
    harmonized_resources = 2L,
    csv_resources = 11L,
    parquet_resources = 0L,
    raw_resources = 4L,
    stored_bytes = sum(bundle$resources$bytes)
  ))
  expect_true(bundle$metadata$in_memory)
  expect_true(bundle$metadata$deterministic)
  expect_true(bundle$metadata$serializes)
  expect_false(bundle$metadata$writes)
  expect_false(bundle$metadata$publishes)
  expect_false(bundle$metadata$arrow)
  expect_false(bundle$metadata$quarto)
  expect_false(bundle$metadata$frictionless)
  expect_false(bundle$metadata$replayable)
  expect_true(gx_catalog_is_sha256(bundle$bundle_id))
})

test_that("M9k has exact catalog, empty fetched, and empty harmonized profiles", {
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  harmonized <- gx_harmonize(fetched)
  bundles <- list(
    catalog = gx_package_resources_impl(gx_package_input_impl(catalog)),
    fetched = gx_package_resources_impl(
      gx_package_input_impl(fetched, catalog)
    ),
    harmonized = gx_package_resources_impl(
      gx_package_input_impl(harmonized, catalog)
    )
  )

  expect_identical(
    vapply(bundles, function(x) nrow(x$resources), integer(1)),
    c(catalog = 4L, fetched = 6L, harmonized = 8L)
  )
  expect_identical(
    unname(vapply(bundles, `[[`, character(1), "stage")),
    names(bundles)
  )
  expect_identical(
    bundles$fetched$metadata$counts$native_resources,
    0L
  )
  expect_identical(
    bundles$harmonized$metadata$counts$harmonized_resources,
    2L
  )
  expect_identical(length(unique(vapply(
    bundles,
    `[[`,
    character(1),
    "bundle_id"
  ))), 3L)
})

test_that("M9k resource and bundle forgery fails closed", {
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  harmonized <- gx_harmonize(fetched)
  bundle <- gx_package_resources_impl(
    gx_package_input_impl(harmonized, catalog)
  )
  mutations <- list(
    content = function(x) {
      x$contents[[1L]][[1L]] <- as.raw(
        bitwXor(as.integer(x$contents[[1L]][[1L]]), 1L)
      )
      x
    },
    resource = function(x) {
      x$resources$sha256[[1L]] <- paste(rep("0", 64L), collapse = "")
      x
    },
    metadata = function(x) {
      x$metadata$publishes <- TRUE
      x
    },
    input = function(x) {
      x$input$metadata$replayable <- TRUE
      x
    },
    identity = function(x) {
      x$bundle_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(bundle, NULL)))
    expect_error(
      gx_package_resources_validate_impl(forged),
      class = "gx_error_package_resources",
      info = name
    )
  }
  expect_error(
    gx_package_resources_entry_impl(
      "../escape.csv", "native_table", "csv", "text/csv",
      charToRaw("x\n")
    ),
    class = "gx_error_package_resources"
  )
})

test_that("M9k resource serialization is offline and write-free", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or write seam", call. = FALSE)
  }
  catalog <- fetch_orchestration_test_usgs_daily_catalog()
  fetched <- gx_fetch(
    fetch_orchestration_test_usgs_daily_plan()$intent_set$plan,
    dry_run = TRUE
  )
  input <- gx_package_input_impl(gx_harmonize(fetched), catalog)

  expect_no_error(testthat::with_mocked_bindings(
    gx_package_resources_impl(input),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_snapshot_writer_write_csv = blocked,
    gx_snapshot_writer_write_raw = blocked,
    gx_snapshot_writer_unlink = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M8 public boundaries are exported and internals remain private", {
  exports <- getNamespaceExports("geoconnexr")
  expect_true(all(c(
    "gx_target_units", "gx_csv_mapping", "gx_feature_mapping",
    "gx_harmonize"
  ) %in% exports))
  expect_false(any(c(
    "gx_target_units_impl", "gx_target_units_validate_impl",
    "gx_csv_mapping_validate_impl", "gx_csv_mappings_normalize_impl",
    "gx_feature_mapping_validate_impl",
    "gx_feature_mappings_normalize_impl",
    "gx_harmonized_new_impl", "gx_harmonized_validate_impl",
    "gx_harmonize_value_impl", "gx_package_input_impl",
    "gx_package_input_validate_impl", "gx_package_resources_impl",
    "gx_package_resources_validate_impl"
  ) %in% exports))
})
