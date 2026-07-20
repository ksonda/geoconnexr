oaf_test_clone <- function(x) unserialize(serialize(x, NULL))

oaf_test_error <- function(expr) tryCatch(expr, error = identity)

oaf_test_fixture_path <- function(name) {
  testthat::test_path("fixtures", "fetch", "oaf", name)
}

oaf_test_body <- function(name = "items-page.geojson") {
  path <- oaf_test_fixture_path(name)
  readBin(path, what = "raw", n = file.info(path)$size)
}

oaf_test_m7d_plan <- function(max_response_bytes = 20000) {
  catalog <- csv_intents_test_fixture_catalog()
  position <- which(catalog$datasets$handler_id == "ogc_api_features")
  catalog <- csv_request_plan_test_replace_distribution_url(
    catalog,
    catalog$datasets$distribution_id[[position]],
    "https://reference.geoconnex.us/collections/gages/items"
  )
  intent_set <- csv_request_plan_test_intent_set(
    catalog = catalog,
    max_encoded_bytes = as.double(max_response_bytes) * 5,
    max_decoded_bytes = as.double(max_response_bytes) * 5
  )
  csv_request_plan_test_build(
    intent_set = intent_set,
    max_response_bytes = max_response_bytes
  )
}

oaf_test_distribution_id <- function(plan) {
  position <- which(plan$coverage$handler_id == "ogc_api_features")
  plan$coverage$distribution_id[[position]]
}

oaf_test_request_plan <- function(
    plan = oaf_test_m7d_plan(), limit = 2L) {
  gx_oaf_request_plan_impl(
    plan,
    distribution_id = oaf_test_distribution_id(plan),
    limit = limit
  )
}

oaf_test_scope <- function(label = "execution") {
  gx_contract_hash(
    list("fixture", label),
    namespace = "geoconnexr.oaf-test.v1",
    contract_version = "0.1.0"
  )
}

oaf_test_performer <- function(
    body = oaf_test_body(),
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

oaf_test_options <- function(performer) {
  gx_http_throttle_reset()
  withr::local_options(
    list(
      geoconnexr.performer = performer,
      geoconnexr.dns_resolver = function(host) "8.8.8.8",
      geoconnexr.clock = csv_execution_test_clock(),
      geoconnexr.throttle_clock = function() 0,
      geoconnexr.throttle_sleep = function(seconds) {
        stop("A zero-interval OGC execution attempted to sleep.", call. = FALSE)
      }
    ),
    .local_envir = parent.frame()
  )
  invisible(NULL)
}

oaf_test_resolver <- function(events = NULL, available = TRUE) {
  function(package, symbol) {
    if (is.environment(events)) events$values <- c(events$values, "resolve")
    if (!available) return(NULL)
    function(request_plan, timeout, min_interval) {
      if (is.environment(events)) events$values <- c(events$values, "invoke")
      gx_handler_oaf(request_plan, timeout, min_interval)
    }
  }
}

oaf_test_execute <- function(
    request_plan = oaf_test_request_plan(),
    performer = oaf_test_performer(),
    symbol_resolver = oaf_test_resolver(),
    scope = oaf_test_scope()) {
  oaf_test_options(performer)
  gx_oaf_execution_impl(
    request_plan,
    timeout = 15,
    min_interval = 0,
    execution_scope_id = scope,
    symbol_resolver = symbol_resolver
  )
}
