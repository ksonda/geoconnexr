test_that("M7l plans one bounded EDR position request without host activity", {
  touched <- new.env(parent = emptyenv())
  touched$count <- 0L
  forbidden <- function(...) {
    touched$count <- touched$count + 1L
    stop("planning touched host state", call. = FALSE)
  }
  withr::local_options(list(
    geoconnexr.performer = forbidden,
    geoconnexr.dns_resolver = forbidden,
    geoconnexr.clock = forbidden,
    geoconnexr.throttle_clock = forbidden,
    geoconnexr.throttle_sleep = forbidden
  ))
  source <- edr_test_m7d_plan()
  first <- edr_test_request_plan(source)
  second <- edr_test_request_plan(source)

  expect_identical(touched$count, 0L)
  expect_identical(first, second)
  expect_identical(class(first), "gx_edr_request_plan")
  expect_identical(first$request_plan, source)
  expect_identical(first$request$query_type, "position")
  expect_identical(first$request$collection_id, "streamflow")
  expect_identical(first$request$coords_wkt, "POINT(-77.5 38.9)")
  expect_identical(first$request$longitude, -77.5)
  expect_identical(first$request$latitude, 38.9)
  expect_identical(first$request$parameter_name, "discharge")
  expect_identical(first$request$time_start, "2025-06-01T00:00:00Z")
  expect_identical(first$request$time_end, "2025-06-30T23:59:59Z")
  expect_identical(first$request$response_format, "CoverageJSON")
  expect_identical(first$request$max_physical_attempts, 1L)
  expect_match(first$request$canonical_url_redacted, "\\?\\[redacted\\]$")
  expect_false(first$metadata$transport_authorized)
  expect_false(first$metadata$budgets_consumed)
  expect_identical(
    gx_edr_request_plan_validate_impl(first), invisible(first)
  )
  expect_snapshot(str(list(policy = first$policy, request = first$request)))
  expect_snapshot(print(first))
})

test_that("M7l resolves EDR capability before one bounded provider request", {
  events <- new.env(parent = emptyenv())
  events$values <- character()
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  result <- edr_test_execute(
    performer = edr_test_performer(calls = calls, events = events),
    symbol_resolver = edr_test_resolver(events = events)
  )

  expect_identical(events$values, c("resolve", "transport", "normalize"))
  expect_length(calls$requests, 1L)
  request <- calls$requests[[1L]]
  expect_identical(request$retries, 0L)
  expect_identical(request$max_bytes, 20000L)
  expect_match(request$url, "collections/streamflow/position", fixed = TRUE)
  expect_match(request$url, "parameter-name=discharge", fixed = TRUE)
  expect_match(request$url, "datetime=2025-06-01T00", fixed = TRUE)
  expect_match(request$url, "f=CoverageJSON", fixed = TRUE)
  expect_identical(class(result), "gx_edr_execution")
  expect_identical(nrow(result$data), 2L)
  expect_identical(ncol(result$data), 9L)
  expect_identical(result$data$value, c(1.25, NA_real_))
  expect_s3_class(result$data$datetime, "POSIXct")
  expect_identical(result$metadata$counts$physical_attempts, 1L)
  expect_true(result$metadata$external_normalizer_matched)
  expect_identical(result$implementation$package_version, "0.1.1")
  expect_identical(
    gx_edr_execution_validate_impl(result), invisible(result)
  )
})

test_that("M7l capability failures occur before DNS or transport", {
  touched <- new.env(parent = emptyenv())
  touched$count <- 0L
  forbidden <- function(...) {
    touched$count <- touched$count + 1L
    stop("capability failure touched transport", call. = FALSE)
  }
  edr_test_options(forbidden)
  withr::local_options(list(geoconnexr.dns_resolver = forbidden))

  for (resolver in list(
    edr_test_resolver(available = FALSE),
    edr_test_resolver(version = "0.1.0")
  )) {
    error <- edr_test_error(gx_edr_execution_impl(
      edr_test_request_plan(),
      timeout = 15,
      min_interval = 0,
      execution_scope_id = edr_test_scope("missing-capability"),
      symbol_resolver = resolver
    ))
    expect_s3_class(error, "gx_error_edr_execution_capability")
  }
  expect_identical(touched$count, 0L)
})

test_that("M7l rejects ambiguous plans, redirects, and parser disagreement", {
  extra <- edr_test_error(edr_test_request_plan(
    edr_test_m7d_plan(edr_test_url("&token=secret"))
  ))
  expect_s3_class(extra, "gx_error_edr_plan_url")
  expect_false(grepl("secret", conditionMessage(extra), fixed = TRUE))

  wrong_datetime <- paste0(
    "https://edr.example.org/api/collections/streamflow/position?",
    "coords=POINT%28-77.5%2038.9%29&parameter-name=discharge&",
    "datetime=2024-01-01T00%3A00%3A00Z%2F2024-01-02T00%3A00%3A00Z"
  )
  expect_error(
    edr_test_request_plan(edr_test_m7d_plan(wrong_datetime)),
    class = "gx_error_edr_plan_time"
  )

  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  redirected <- edr_test_error(edr_test_execute(
    performer = edr_test_performer(
      final_url = "https://evil.example.org/position", calls = calls
    ),
    scope = edr_test_scope("redirect")
  ))
  expect_s3_class(redirected, "gx_error_edr_execution_transport")
  expect_length(calls$requests, 1L)
  expect_identical(nrow(redirected$attempts), 1L)
  expect_false(any(grepl(
    "evil.example.org",
    unlist(redirected, recursive = TRUE, use.names = FALSE),
    fixed = TRUE
  )))

  mismatch <- edr_test_error(edr_test_execute(
    symbol_resolver = edr_test_resolver(mismatch = TRUE),
    scope = edr_test_scope("normalizer-mismatch")
  ))
  expect_s3_class(mismatch, "gx_error_edr_execution_parse")
  expect_identical(nrow(mismatch$attempts), 1L)
  expect_gt(mismatch$attempts$charged_bytes[[1L]], 0)
})

test_that("M7l fails closed on unreviewed position request semantics", {
  cases <- list(
    duplicate = paste0(edr_test_url(), "&coords=POINT%28-77.5%2038.9%29"),
    out_of_bounds = sub("-77.5", "200", edr_test_url(), fixed = TRUE),
    multi_parameter = sub(
      "discharge", "discharge%2Ctemperature", edr_test_url(), fixed = TRUE
    ),
    wrong_crs = paste0(edr_test_url(), "&crs=EPSG%3A4326"),
    wrong_format = sub("CoverageJSON", "GeoJSON", edr_test_url(), fixed = TRUE)
  )
  for (name in names(cases)) {
    expect_error(
      edr_test_request_plan(edr_test_m7d_plan(cases[[name]])),
      class = "gx_error_edr_plan_url",
      info = name
    )
  }
})

test_that("M7l rejects unsupported or inconsistent CoverageJSON", {
  bodies <- list(
    duplicate_member = edr_test_body_replace(
      '"type": "Coverage",',
      '"type": "Coverage",\n  "type": "Coverage",'
    ),
    domain = edr_test_body_replace(
      '"domainType": "PointSeries"', '"domainType": "Trajectory"'
    ),
    coordinate = edr_test_body_replace(
      '"x": {"values": [-77.5]}', '"x": {"values": [-77.4]}'
    ),
    shape = edr_test_body_replace('"shape": [2]', '"shape": [3]'),
    structural_depth = charToRaw(paste0(
      '{"type":"Coverage","id":"deep","extra":',
      strrep("[", 33L), "0", strrep("]", 33L), "}"
    ))
  )
  for (name in names(bodies)) {
    error <- edr_test_error(edr_test_execute(
      performer = edr_test_performer(body = bodies[[name]]),
      scope = edr_test_scope(paste0("payload-", name))
    ))
    expect_s3_class(error, "gx_error_edr_execution_parse")
    expect_identical(nrow(error$attempts), 1L)
    expect_gt(error$attempts$charged_bytes[[1L]], 0)
  }

  parameter_url <- sub(
    "discharge", "temperature", edr_test_url(), fixed = TRUE
  )
  parameter <- edr_test_error(edr_test_execute(
    request_plan = edr_test_request_plan(edr_test_m7d_plan(parameter_url)),
    scope = edr_test_scope("payload-parameter")
  ))
  expect_s3_class(parameter, "gx_error_edr_execution_parse")

  rows <- edr_test_error(edr_test_execute(
    request_plan = edr_test_request_plan(edr_test_m7d_plan(max_rows = 1L)),
    scope = edr_test_scope("payload-row-limit")
  ))
  expect_s3_class(rows, "gx_error_edr_execution_parse")

  fields <- edr_test_error(edr_test_execute(
    request_plan = edr_test_request_plan(max_fields = 17L),
    scope = edr_test_scope("payload-field-limit")
  ))
  expect_s3_class(fields, "gx_error_edr_execution_parse")
})

test_that("M7l whole-object validation fails closed on forged facts", {
  value <- edr_test_execute()
  mutations <- list(
    body = function(x) {
      x$response_body[[1L]] <- as.raw(bitwXor(
        as.integer(x$response_body[[1L]]), 1L
      ))
      x
    },
    data = function(x) {
      x$data$value[[1L]] <- 999
      x
    },
    schema = function(x) {
      x$schema$column_name[[1L]] <- "forged"
      x
    },
    implementation = function(x) {
      x$implementation$package_version <- "0.1.0"
      x
    },
    attempt = function(x) {
      x$attempts$encoded_bytes[[1L]] <- x$attempts$encoded_bytes[[1L]] + 1
      x
    },
    execution = function(x) {
      x$execution$row_count <- x$execution$row_count + 1L
      x
    },
    metadata = function(x) {
      x$metadata$external_normalizer_matched <- FALSE
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](edr_test_clone(value))
    expect_error(
      gx_edr_execution_validate_impl(forged),
      class = "gx_error_edr",
      info = name
    )
  }
})

test_that("M7l accepts the installed official normalizer when compatible", {
  skip_if_not_installed("edr4r", minimum_version = "0.1.1")
  result <- gx_edr_result_impl(
    edr_test_body(), edr_test_request_plan(),
    parser = edr4r::covjson_to_tibble
  )
  expect_identical(result$parse$row_count, 2L)
  expect_identical(result$parse$column_count, 9L)
})

test_that("M7l accepts the reviewed pygeoapi CoverageJSON profile", {
  skip_if_not_installed("edr4r", minimum_version = "0.1.1")
  url <- httr2::url_modify_query(
    "https://demo.pygeoapi.io/master/collections/icoads-sst/position",
    coords = "POINT(-29 31)",
    `parameter-name` = "SST",
    crs = "http://www.opengis.net/def/crs/OGC/1.3/CRS84",
    f = "json"
  )
  catalog <- edr_test_catalog(url)
  position <- which(catalog$datasets$handler_id == "edr")
  time <- as.POSIXct(
    c("2000-01-16 06:00:00", "2000-02-16 06:00:00"), tz = "UTC"
  )
  catalog$datasets$temporal_coverage[position] <-
    "2000-01-16T06:00:00Z/2000-02-16T06:00:00Z"
  catalog$datasets$temporal_start[position] <- time[[1L]]
  catalog$datasets$temporal_end[position] <- time[[2L]]
  gx_catalog_validate_impl(catalog)
  plan <- csv_request_plan_test_build(
    csv_request_plan_test_intent_set(
      catalog = catalog,
      time = time,
      max_encoded_bytes = 2 * 1024^2,
      max_decoded_bytes = 2 * 1024^2
    ),
    max_response_bytes = 2 * 1024^2,
    max_rows = 1000L,
    max_columns = 100L
  )
  request_plan <- gx_edr_request_plan_impl(
    plan, edr_test_distribution_id(plan), max_fields = 10000L
  )
  result <- gx_edr_result_impl(
    edr_test_body("pygeoapi-position-pointseries.covjson"),
    request_plan,
    parser = edr4r::covjson_to_tibble
  )

  expect_identical(request_plan$request$crs,
    "http://www.opengis.net/def/crs/OGC/1.3/CRS84")
  expect_identical(result$parse$coverage_id, "1")
  expect_identical(result$parse$row_count, 2L)
  expect_identical(result$data$parameter, c("SST", "SST"))
  expect_equal(result$data$value, c(18.932044982910156, 17.774076461791992))
})

test_that("the M7l EDR handler contract remains internal", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_edr_request_plan_impl", "gx_edr_execution_impl", "gx_handler_edr"
  ) %in% exports))
})
