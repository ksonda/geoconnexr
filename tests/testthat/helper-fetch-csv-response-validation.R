csv_response_validation_test_clone <- function(x) {
  unserialize(serialize(x, NULL))
}

csv_response_validation_test_error <- function(expr) {
  tryCatch(expr, error = identity)
}

csv_response_validation_test_fixture_dir <- function() {
  testthat::test_path("fixtures", "fetch", "csv-response-validation")
}

csv_response_validation_test_read_json <- function(name) {
  jsonlite::fromJSON(
    file.path(csv_response_validation_test_fixture_dir(), name),
    simplifyVector = FALSE
  )
}

csv_response_validation_test_plan <- function(
    intent_set = csv_request_plan_test_intent_set(),
    max_response_bytes = 100L,
    max_rows = 10000L,
    max_columns = 100L) {
  csv_request_plan_test_build(
    intent_set = intent_set,
    max_response_bytes = max_response_bytes,
    max_rows = max_rows,
    max_columns = max_columns
  )
}

csv_response_validation_test_request_position <- function(
    request_plan, request_order = 1L) {
  position <- which(request_plan$request_plans$request_order == request_order)
  if (length(position) != 1L) {
    stop("The requested logical request is absent.", call. = FALSE)
  }
  unname(as.integer(position))
}

csv_response_validation_test_url <- function(
    request_plan, request_order = 1L) {
  position <- csv_response_validation_test_request_position(
    request_plan, request_order
  )
  distribution_id <- request_plan$request_plans$distribution_id[[position]]
  distributions <- request_plan$intent_set$plan$distributions
  distribution_position <- match(
    distribution_id, distributions$distribution_id
  )
  distributions$distribution_url[[distribution_position]]
}

csv_response_validation_test_body <- function() {
  charToRaw("station,value\n00123,4.5\n")
}

csv_response_validation_test_candidate <- function(
    request_plan,
    request_order = 1L,
    status = 200L,
    headers = NULL,
    body = csv_response_validation_test_body(),
    url = csv_response_validation_test_url(request_plan, request_order),
    content_type = "text/csv",
    content_encoding = NULL,
    content_length = as.character(length(body))) {
  if (is.null(headers)) {
    headers <- list(`Content-Type` = content_type)
    if (!is.null(content_encoding)) {
      headers[["Content-Encoding"]] <- content_encoding
    }
    if (!is.null(content_length)) {
      headers[["Content-Length"]] <- content_length
    }
  }
  list(status = status, headers = headers, body = body, url = url)
}

csv_response_validation_test_build <- function(
    request_plan = csv_response_validation_test_plan(),
    request_order = 1L,
    candidate = csv_response_validation_test_candidate(
      request_plan, request_order
    )) {
  position <- csv_response_validation_test_request_position(
    request_plan, request_order
  )
  gx_csv_validated_response_impl(
    request_plan = request_plan,
    logical_request_id = request_plan$request_plans$logical_request_id[[position]],
    candidate = candidate
  )
}

csv_response_validation_test_owned_text <- function(x) {
  unlist(list(
    contract_version = x$contract_version,
    validation = x$validation,
    metadata = x$metadata
  ), use.names = FALSE)
}
