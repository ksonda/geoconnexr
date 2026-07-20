csv_parse_test_clone <- function(x) {
  unserialize(serialize(x, NULL))
}

csv_parse_test_error <- function(expr) {
  tryCatch(expr, error = identity)
}

csv_parse_test_fixture_dir <- function() {
  testthat::test_path("fixtures", "fetch", "csv-parse")
}

csv_parse_test_read_json <- function(name) {
  jsonlite::fromJSON(
    file.path(csv_parse_test_fixture_dir(), name),
    simplifyVector = FALSE
  )
}

csv_parse_test_response <- function(
    body = charToRaw("station,value\n00123,4.5\n"),
    max_response_bytes = max(100L, length(body)),
    max_rows = 100L,
    max_columns = 20L) {
  intent_set <- csv_request_plan_test_intent_set(
    max_encoded_bytes = as.double(max_response_bytes) * 5,
    max_decoded_bytes = as.double(max_response_bytes) * 5
  )
  request_plan <- csv_response_validation_test_plan(
    intent_set = intent_set,
    max_response_bytes = max_response_bytes,
    max_rows = max_rows,
    max_columns = max_columns
  )
  candidate <- csv_response_validation_test_candidate(
    request_plan,
    body = body,
    content_length = as.character(length(body))
  )
  csv_response_validation_test_build(
    request_plan = request_plan,
    candidate = candidate
  )
}

csv_parse_test_build <- function(
    body = charToRaw("station,value\n00123,4.5\n"),
    max_response_bytes = max(100L, length(body)),
    max_rows = 100L,
    max_columns = 20L,
    max_fields = 1000L) {
  gx_csv_parsed_response_impl(
    csv_parse_test_response(
      body = body,
      max_response_bytes = max_response_bytes,
      max_rows = max_rows,
      max_columns = max_columns
    ),
    max_fields = max_fields
  )
}

csv_parse_test_raw <- function(...) {
  as.raw(c(...))
}
