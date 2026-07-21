test_that("M7m plans one bounded USGS continuous request without host activity", {
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
  source <- usgs_continuous_test_m7d_plan()
  first <- usgs_continuous_test_request_plan(source)
  second <- usgs_continuous_test_request_plan(source)

  expect_identical(touched$count, 0L)
  expect_identical(first, second)
  expect_identical(class(first), "gx_usgs_continuous_request_plan")
  expect_identical(first$request_plan, source)
  expect_identical(first$request$api_version, "v0")
  expect_identical(first$request$collection_id, "continuous")
  expect_identical(first$request$monitoring_location_id, "USGS-01491000")
  expect_identical(first$request$parameter_code, "00060")
  expect_identical(first$request$time_start, "2025-06-01T00:00:00Z")
  expect_identical(first$request$time_end, "2025-06-30T23:59:59Z")
  expect_identical(first$request$response_format, "GeoJSON")
  expect_identical(first$request$limit, 90L)
  expect_identical(first$request$max_physical_attempts, 1L)
  expect_match(first$request$canonical_url_redacted, "\\?\\[redacted\\]$")
  expect_false(first$metadata$transport_authorized)
  expect_false(first$metadata$budgets_consumed)
  expect_identical(
    gx_usgs_continuous_request_plan_validate_impl(first), invisible(first)
  )
})

test_that("M7m checks capability before one bounded provider request", {
  events <- new.env(parent = emptyenv())
  events$values <- character()
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  result <- usgs_continuous_test_execute(
    performer = usgs_continuous_test_performer(calls = calls, events = events),
    symbol_resolver = usgs_continuous_test_resolver(events = events)
  )

  expect_identical(events$values, c("resolve", "transport"))
  expect_length(calls$requests, 1L)
  request <- calls$requests[[1L]]
  expect_identical(request$retries, 0L)
  expect_identical(request$max_bytes, 20000L)
  expect_match(request$url, "/collections/continuous/items", fixed = TRUE)
  expect_match(request$url, "monitoring_location_id=USGS-01491000", fixed = TRUE)
  expect_match(request$url, "parameter_code=00060", fixed = TRUE)
  expect_match(request$url, "skipGeometry=true", fixed = TRUE)
  expect_match(request$url, "limit=90", fixed = TRUE)
  expect_identical(class(result), "gx_usgs_continuous_execution")
  expect_identical(dim(result$data), c(2L, 11L))
  expect_identical(result$data$value, c("168", "171"))
  expect_identical(result$data$qualifier, c(NA_character_, "P"))
  expect_s3_class(result$data$time, "POSIXct")
  expect_true(result$parse$truncated)
  expect_identical(result$parse$stop_reason, "next_link_not_followed")
  expect_identical(result$metadata$counts$physical_attempts, 1L)
  expect_true(result$metadata$dataretrieval_symbol_checked)
  expect_true(result$metadata$native_geojson_parsed)
  expect_identical(result$implementation$package_version, "2.7.22")
  expect_identical(
    gx_usgs_continuous_execution_validate_impl(result), invisible(result)
  )
})

test_that("M7m capability failures occur before DNS or transport", {
  touched <- new.env(parent = emptyenv())
  touched$count <- 0L
  forbidden <- function(...) {
    touched$count <- touched$count + 1L
    stop("capability failure touched transport", call. = FALSE)
  }
  usgs_continuous_test_options(forbidden)
  withr::local_options(list(geoconnexr.dns_resolver = forbidden))

  for (resolver in list(
    usgs_continuous_test_resolver(available = FALSE),
    usgs_continuous_test_resolver(version = "2.7.21")
  )) {
    error <- usgs_continuous_test_error(gx_usgs_continuous_execution_impl(
      usgs_continuous_test_request_plan(),
      timeout = 15,
      min_interval = 0,
      execution_scope_id = usgs_continuous_test_scope("missing-capability"),
      symbol_resolver = resolver
    ))
    expect_s3_class(
      error, "gx_error_usgs_continuous_execution_capability"
    )
  }
  expect_identical(touched$count, 0L)
})

test_that("M7m rejects unreviewed request semantics and redirects", {
  cases <- list(
    extra = usgs_continuous_test_url("&token=secret"),
    duplicate = usgs_continuous_test_url("&parameter_code=00060"),
    wrong_site = sub(
      "USGS-01491000", "OTHER-01491000", usgs_continuous_test_url(),
      fixed = TRUE
    ),
    multi_parameter = sub(
      "00060", "00060%2C00065", usgs_continuous_test_url(), fixed = TRUE
    ),
    daily = sub(
      "/continuous/", "/daily/", usgs_continuous_test_url(), fixed = TRUE
    ),
    latest = sub(
      "/continuous/", "/latest-continuous/", usgs_continuous_test_url(),
      fixed = TRUE
    )
  )
  for (name in names(cases)) {
    error <- usgs_continuous_test_error(usgs_continuous_test_request_plan(
      usgs_continuous_test_m7d_plan(cases[[name]])
    ))
    expected_class <- if (identical(name, "daily")) {
      "gx_error_usgs_continuous_plan_input"
    } else {
      "gx_error_usgs_continuous_plan_url"
    }
    expect_s3_class(error, expected_class)
    expect_false(grepl("secret", conditionMessage(error), fixed = TRUE))
  }

  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  redirected <- usgs_continuous_test_error(usgs_continuous_test_execute(
    performer = usgs_continuous_test_performer(
      final_url = "https://evil.example.org/items", calls = calls
    ),
    scope = usgs_continuous_test_scope("redirect")
  ))
  expect_s3_class(redirected, "gx_error_usgs_continuous_execution_transport")
  expect_length(calls$requests, 1L)
  expect_identical(nrow(redirected$attempts), 1L)
  expect_false(any(grepl(
    "evil.example.org",
    unlist(redirected, recursive = TRUE, use.names = FALSE),
    fixed = TRUE
  )))
})

test_that("M7m rejects malformed or semantically inconsistent GeoJSON", {
  bodies <- list(
    duplicate_member = usgs_continuous_test_body_replace(
      '"type": "FeatureCollection",',
      '"type": "FeatureCollection",\n  "type": "FeatureCollection",'
    ),
    site = usgs_continuous_test_body_replace(
      '"monitoring_location_id": "USGS-01491000"',
      '"monitoring_location_id": "USGS-01491001"'
    ),
    numeric_value = usgs_continuous_test_body_replace(
      '"value": "168"', '"value": 168'
    ),
    outside_time = usgs_continuous_test_body_replace(
      '"time": "2025-06-01T00:00:00+00:00"',
      '"time": "2025-07-01T00:00:00+00:00"'
    ),
    geometry = usgs_continuous_test_body_replace(
      '"geometry": null', '"geometry": {"type":"Point","coordinates":[0,0]}'
    ),
    count = usgs_continuous_test_body_replace(
      '"numberReturned": 2', '"numberReturned": 3'
    ),
    structural_depth = charToRaw(paste0(
      '{"type":"FeatureCollection","features":[],"numberReturned":0,',
      '"numberMatched":null,"links":[],"extra":',
      strrep("[", 33L), "0", strrep("]", 33L), "}"
    ))
  )
  for (name in names(bodies)) {
    error <- usgs_continuous_test_error(usgs_continuous_test_execute(
      performer = usgs_continuous_test_performer(body = bodies[[name]]),
      scope = usgs_continuous_test_scope(paste0("payload-", name))
    ))
    expect_s3_class(error, "gx_error_usgs_continuous_execution_parse")
    expect_identical(nrow(error$attempts), 1L)
    expect_gt(error$attempts$charged_bytes[[1L]], 0)
  }

  rows <- usgs_continuous_test_error(usgs_continuous_test_execute(
    request_plan = usgs_continuous_test_request_plan(
      usgs_continuous_test_m7d_plan(max_rows = 1L)
    ),
    scope = usgs_continuous_test_scope("row-limit")
  ))
  expect_s3_class(rows, "gx_error_usgs_continuous_execution_parse")
})

test_that("M7m whole-object validation rejects forged facts", {
  value <- usgs_continuous_test_execute()
  mutations <- list(
    body = function(x) {
      x$response_body[[1L]] <- as.raw(bitwXor(
        as.integer(x$response_body[[1L]]), 1L
      ))
      x
    },
    data = function(x) {
      x$data$value[[1L]] <- "999"
      x
    },
    schema = function(x) {
      x$schema$column_name[[1L]] <- "forged"
      x
    },
    implementation = function(x) {
      x$implementation$package_version <- "2.7.21"
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
      x$metadata$native_geojson_parsed <- FALSE
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](usgs_continuous_test_clone(value))
    expect_error(
      gx_usgs_continuous_execution_validate_impl(forged),
      class = "gx_error_usgs_continuous",
      info = name
    )
  }
})

test_that("M7m recognizes the installed dataRetrieval capability", {
  skip_if_not_installed("dataRetrieval", minimum_version = "2.7.22")
  capability <- gx_usgs_continuous_symbol_resolver_impl(
    "dataRetrieval", "read_waterdata_continuous", "2.7.22"
  )
  expect_identical(names(capability), c("package_version", "query"))
  expect_true(is.function(capability$query))
})

test_that("the M7m USGS continuous handler contract remains internal", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_usgs_continuous_request_plan_impl",
    "gx_usgs_continuous_execution_impl",
    "gx_handler_usgs_waterdata_continuous"
  ) %in% exports))
})
