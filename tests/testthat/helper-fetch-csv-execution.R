csv_execution_test_clone <- function(x) {
  unserialize(serialize(x, NULL))
}

csv_execution_test_error <- function(expr) {
  tryCatch(expr, error = identity)
}

csv_execution_test_scope <- function(label = "scope") {
  gx_contract_hash(
    list("fixture", label),
    namespace = "geoconnexr.csv-execution-test.v1",
    contract_version = "0.1.0"
  )
}

csv_execution_test_clock <- function() {
  values <- as.POSIXct(
    c(
      "2026-07-20 12:00:00.000000",
      "2026-07-20 12:00:01.250000",
      "2026-07-20 12:00:01.500000",
      "2026-07-20 12:00:02.000000"
    ),
    tz = "UTC"
  )
  position <- 0L
  function() {
    position <<- position + 1L
    values[[min(position, length(values))]]
  }
}

csv_execution_test_plan <- function(
    body = csv_response_validation_test_body(),
    max_response_bytes = max(100L, length(body)),
    max_rows = 100L,
    max_columns = 20L) {
  intent_set <- csv_request_plan_test_intent_set(
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

csv_execution_test_logical_id <- function(plan, request_order = 1L) {
  position <- csv_response_validation_test_request_position(
    plan, request_order
  )
  plan$request_plans$logical_request_id[[position]]
}

csv_execution_test_performer <- function(
    body = csv_response_validation_test_body(),
    status = 200L,
    content_type = "text/csv",
    content_encoding = NULL,
    content_length = as.character(length(body)),
    final_url = NULL,
    calls = NULL) {
  force(body)
  force(status)
  force(content_type)
  force(content_encoding)
  force(content_length)
  force(final_url)
  function(request) {
    if (is.environment(calls)) {
      calls$requests <- c(calls$requests %||% list(), list(request))
    }
    headers <- list(`Content-Type` = content_type)
    if (!is.null(content_encoding)) {
      headers[["Content-Encoding"]] <- content_encoding
    }
    if (!is.null(content_length)) {
      headers[["Content-Length"]] <- content_length
    }
    list(
      status = status,
      headers = headers,
      body = body,
      url = final_url %||% request$url
    )
  }
}

csv_execution_test_options <- function(performer, dns = "8.8.8.8") {
  gx_http_throttle_reset()
  withr::local_options(
    list(
      geoconnexr.performer = performer,
      geoconnexr.dns_resolver = function(host) dns,
      geoconnexr.clock = csv_execution_test_clock(),
      geoconnexr.throttle_clock = function() 0,
      geoconnexr.throttle_sleep = function(seconds) {
        stop("A zero-interval execution attempted to sleep.", call. = FALSE)
      }
    ),
    .local_envir = parent.frame()
  )
  invisible(NULL)
}

csv_execution_test_build <- function(
    plan = csv_execution_test_plan(),
    body = csv_response_validation_test_body(),
    performer = csv_execution_test_performer(body = body),
    scope = csv_execution_test_scope(),
    max_fields = 1000L,
    timeout = 15,
    min_interval = 0) {
  csv_execution_test_options(performer)
  gx_csv_execution_impl(
    request_plan = plan,
    logical_request_id = csv_execution_test_logical_id(plan),
    max_fields = max_fields,
    timeout = timeout,
    min_interval = min_interval,
    execution_scope_id = scope
  )
}
