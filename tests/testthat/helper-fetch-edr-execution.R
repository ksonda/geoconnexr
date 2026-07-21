edr_test_clone <- function(x) unserialize(serialize(x, NULL))

edr_test_error <- function(expr) tryCatch(expr, error = identity)

edr_test_fixture_path <- function(name = "position-pointseries.covjson") {
  testthat::test_path("fixtures", "fetch", "edr", name)
}

edr_test_body <- function(name = "position-pointseries.covjson") {
  path <- edr_test_fixture_path(name)
  readBin(path, what = "raw", n = file.info(path)$size)
}

edr_test_body_replace <- function(from, to) {
  text <- rawToChar(edr_test_body())
  stopifnot(grepl(from, text, fixed = TRUE))
  charToRaw(sub(from, to, text, fixed = TRUE))
}

edr_test_url <- function(extra = "") {
  paste0(
    "https://edr.example.org/api/collections/streamflow/position?",
    "coords=POINT%28-77.5%2038.9%29&parameter-name=discharge&",
    "f=CoverageJSON", extra
  )
}

edr_test_catalog <- function(url = edr_test_url()) {
  catalog <- csv_intents_test_fixture_catalog()
  position <- which(catalog$datasets$handler_id == "ogc_api_features")
  catalog$datasets$distribution_url[[position]] <- url
  catalog$datasets$media_type[[position]] <- "application/prs.coverage+json"
  catalog$datasets$handler_id[[position]] <- "edr"
  catalog$datasets$conforms_to[[position]] <-
    "http://www.opengis.net/spec/ogcapi-edr-1/1.1/conf/core"
  gx_catalog_new_impl(
    aoi = catalog$aoi,
    sites = catalog$sites,
    datasets = catalog$datasets,
    reference = catalog$reference,
    problems = catalog$problems,
    requests = catalog$requests,
    metadata = fetch_plan_test_metadata(catalog$sites, catalog$datasets)
  )
}

edr_test_m7d_plan <- function(
    url = edr_test_url(), max_response_bytes = 20000,
    max_rows = 10000L, max_columns = 100L) {
  intent_set <- csv_request_plan_test_intent_set(
    catalog = edr_test_catalog(url),
    max_encoded_bytes = as.double(max_response_bytes) * 5,
    max_decoded_bytes = as.double(max_response_bytes) * 5
  )
  csv_request_plan_test_build(
    intent_set = intent_set,
    max_response_bytes = max_response_bytes,
    max_rows = max_rows,
    max_columns = max_columns
  )
}

edr_test_distribution_id <- function(plan) {
  position <- which(plan$coverage$handler_id == "edr")
  plan$coverage$distribution_id[[position]]
}

edr_test_request_plan <- function(
    plan = edr_test_m7d_plan(), max_fields = 1000L) {
  gx_edr_request_plan_impl(
    plan,
    distribution_id = edr_test_distribution_id(plan),
    max_fields = max_fields
  )
}

edr_test_scope <- function(label = "execution") {
  gx_contract_hash(
    list("fixture", label),
    namespace = "geoconnexr.edr-test.v1",
    contract_version = "0.1.0"
  )
}

edr_test_normalizer <- function(events = NULL, mismatch = FALSE) {
  function(document, datetime_as_posix) {
    if (is.environment(events)) events$values <- c(events$values, "normalize")
    times <- vapply(document$domain$axes$t$values, identity, character(1))
    values <- vapply(document$ranges$discharge$values, function(value) {
      if (is.null(value)) NA_real_ else as.numeric(value)
    }, numeric(1))
    data <- tibble::tibble(
      coverage_id = rep(document$id, length(values)),
      parameter = rep("discharge", length(values)),
      parameter_label = rep("Discharge", length(values)),
      unit = rep("m3/s", length(values)),
      datetime = as.POSIXct(times, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      x = rep(as.numeric(document$domain$axes$x$values[[1L]]), length(values)),
      y = rep(as.numeric(document$domain$axes$y$values[[1L]]), length(values)),
      z = rep(NA_real_, length(values)),
      value = values
    )
    if (mismatch) data$value[[1L]] <- 999
    data
  }
}

edr_test_resolver <- function(
    events = NULL, available = TRUE, version = "0.1.1", mismatch = FALSE) {
  function(package, query_symbol, normalizer_symbol, minimum_version) {
    if (is.environment(events)) events$values <- c(events$values, "resolve")
    if (!available) return(NULL)
    list(
      package_version = version,
      query = function(...) invisible(NULL),
      normalizer = edr_test_normalizer(events = events, mismatch = mismatch)
    )
  }
}

edr_test_performer <- function(
    body = edr_test_body(),
    status = 200L,
    media_type = "application/prs.coverage+json",
    final_url = NULL,
    calls = NULL,
    events = NULL) {
  force(body)
  function(request) {
    if (is.environment(calls)) {
      calls$requests <- c(calls$requests %||% list(), list(request))
    }
    if (is.environment(events)) events$values <- c(events$values, "transport")
    list(
      status = status,
      headers = list(
        `Content-Type` = media_type,
        `Content-Length` = as.character(length(body))
      ),
      body = body,
      url = final_url %||% request$url
    )
  }
}

edr_test_options <- function(performer) {
  gx_http_throttle_reset()
  withr::local_options(
    list(
      geoconnexr.performer = performer,
      geoconnexr.dns_resolver = function(host) "8.8.8.8",
      geoconnexr.clock = csv_execution_test_clock(),
      geoconnexr.throttle_clock = function() 0,
      geoconnexr.throttle_sleep = function(seconds) {
        stop("A zero-interval EDR execution attempted to sleep.", call. = FALSE)
      }
    ),
    .local_envir = parent.frame()
  )
  invisible(NULL)
}

edr_test_execute <- function(
    request_plan = edr_test_request_plan(),
    performer = edr_test_performer(),
    symbol_resolver = edr_test_resolver(),
    scope = edr_test_scope()) {
  edr_test_options(performer)
  gx_edr_execution_impl(
    request_plan,
    timeout = 15,
    min_interval = 0,
    execution_scope_id = scope,
    symbol_resolver = symbol_resolver
  )
}
