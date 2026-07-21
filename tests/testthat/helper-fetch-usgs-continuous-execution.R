usgs_continuous_test_clone <- function(x) unserialize(serialize(x, NULL))

usgs_continuous_test_error <- function(expr) tryCatch(expr, error = identity)

usgs_continuous_test_fixture_path <- function(
    name = "continuous-single-page.geojson") {
  testthat::test_path("fixtures", "fetch", "usgs", name)
}

usgs_continuous_test_body <- function(name = "continuous-single-page.geojson") {
  path <- usgs_continuous_test_fixture_path(name)
  readBin(path, what = "raw", n = file.info(path)$size)
}

usgs_continuous_test_body_replace <- function(from, to) {
  text <- rawToChar(usgs_continuous_test_body())
  stopifnot(grepl(from, text, fixed = TRUE))
  charToRaw(sub(from, to, text, fixed = TRUE))
}

usgs_continuous_test_url <- function(extra = "") {
  paste0(
    "https://api.waterdata.usgs.gov/ogcapi/v0/collections/continuous/items?",
    "monitoring_location_id=USGS-01491000&parameter_code=00060", extra
  )
}

usgs_continuous_test_catalog <- function(url = usgs_continuous_test_url()) {
  catalog <- csv_intents_test_fixture_catalog()
  position <- which(catalog$datasets$handler_id == "ogc_api_features")
  catalog$datasets$distribution_url[[position]] <- url
  catalog$datasets$media_type[[position]] <- "application/geo+json"
  catalog$datasets$handler_id[[position]] <- "usgs_waterdata_continuous"
  catalog$datasets$conforms_to[[position]] <-
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core"
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

usgs_continuous_test_m7d_plan <- function(
    url = usgs_continuous_test_url(), max_response_bytes = 20000,
    max_rows = 10000L, max_columns = 100L) {
  intent_set <- csv_request_plan_test_intent_set(
    catalog = usgs_continuous_test_catalog(url),
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

usgs_continuous_test_distribution_id <- function(plan) {
  position <- which(
    plan$coverage$handler_id == "usgs_waterdata_continuous"
  )
  plan$coverage$distribution_id[[position]]
}

usgs_continuous_test_request_plan <- function(
    plan = usgs_continuous_test_m7d_plan(), max_fields = 1000L) {
  gx_usgs_continuous_request_plan_impl(
    plan,
    distribution_id = usgs_continuous_test_distribution_id(plan),
    max_fields = max_fields
  )
}

usgs_continuous_test_scope <- function(label = "execution") {
  gx_contract_hash(
    list("fixture", label),
    namespace = "geoconnexr.usgs-continuous-test.v1",
    contract_version = "0.1.0"
  )
}

usgs_continuous_test_resolver <- function(
    events = NULL, available = TRUE, version = "2.7.22") {
  function(package, query_symbol, minimum_version) {
    if (is.environment(events)) events$values <- c(events$values, "resolve")
    if (!available) return(NULL)
    list(
      package_version = version,
      query = function(...) invisible(NULL)
    )
  }
}

usgs_continuous_test_performer <- function(
    body = usgs_continuous_test_body(),
    status = 200L,
    media_type = "application/geo+json",
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

usgs_continuous_test_options <- function(performer) {
  gx_http_throttle_reset()
  withr::local_options(
    list(
      geoconnexr.performer = performer,
      geoconnexr.dns_resolver = function(host) "8.8.8.8",
      geoconnexr.clock = csv_execution_test_clock(),
      geoconnexr.throttle_clock = function() 0,
      geoconnexr.throttle_sleep = function(seconds) {
        stop("A zero-interval USGS continuous execution attempted to sleep.", call. = FALSE)
      }
    ),
    .local_envir = parent.frame()
  )
  invisible(NULL)
}

usgs_continuous_test_execute <- function(
    request_plan = usgs_continuous_test_request_plan(),
    performer = usgs_continuous_test_performer(),
    symbol_resolver = usgs_continuous_test_resolver(),
    scope = usgs_continuous_test_scope()) {
  usgs_continuous_test_options(performer)
  gx_usgs_continuous_execution_impl(
    request_plan,
    timeout = 15,
    min_interval = 0,
    execution_scope_id = scope,
    symbol_resolver = symbol_resolver
  )
}
