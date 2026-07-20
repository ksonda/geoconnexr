csv_request_plan_test_clone <- function(x) {
  unserialize(serialize(x, NULL))
}

csv_request_plan_test_error <- function(expr) {
  tryCatch(expr, error = identity)
}

csv_request_plan_test_fixture_dir <- function() {
  testthat::test_path("fixtures", "fetch", "csv-request-plan")
}

csv_request_plan_test_read_json <- function(name) {
  jsonlite::fromJSON(
    file.path(csv_request_plan_test_fixture_dir(), name),
    simplifyVector = FALSE
  )
}

csv_request_plan_test_intent_set <- function(
    catalog = csv_intents_test_fixture_catalog(),
    max_datasets = 5L,
    max_requests = 5L,
    max_encoded_bytes = 500,
    max_decoded_bytes = 500,
    time = fetch_plan_test_time()) {
  gx_csv_get_intents_impl(fetch_plan_test_build(
    catalog = catalog,
    time = time,
    max_datasets = max_datasets,
    max_requests = max_requests,
    max_encoded_bytes = max_encoded_bytes,
    max_decoded_bytes = max_decoded_bytes
  ))
}

csv_request_plan_test_empty_intent_set <- function(
    max_requests = 7L,
    max_encoded_bytes = 70,
    max_decoded_bytes = 90) {
  gx_csv_get_intents_impl(fetch_plan_test_build(
    catalog = fetch_plan_test_catalog(populated = FALSE),
    time = NULL,
    max_datasets = 0L,
    max_requests = max_requests,
    max_encoded_bytes = max_encoded_bytes,
    max_decoded_bytes = max_decoded_bytes
  ))
}

csv_request_plan_test_build <- function(
    intent_set = csv_request_plan_test_intent_set(),
    max_response_bytes = 100L,
    max_rows = 10000L,
    max_columns = 100L) {
  gx_csv_request_plan_impl(
    intent_set = intent_set,
    max_response_bytes = max_response_bytes,
    max_rows = max_rows,
    max_columns = max_columns
  )
}

csv_request_plan_test_fair_partition <- function(total, slots, ceiling) {
  slots <- as.integer(slots)
  if (!slots) return(double())
  reserved <- min(as.double(total), as.double(slots) * ceiling)
  quotient <- floor(reserved / slots)
  remainder <- as.integer(reserved - quotient * slots)
  unname(rep(quotient, slots) + as.double(seq_len(slots) <= remainder))
}

csv_request_plan_test_replace_distribution_url <- function(
    catalog, distribution_id, replacement) {
  out <- csv_request_plan_test_clone(catalog)
  index <- which(out$datasets$distribution_id == distribution_id)
  if (!length(index)) {
    stop("The requested fixture distribution is absent.", call. = FALSE)
  }
  out$datasets$distribution_url[index] <- replacement
  gx_catalog_validate_impl(out)
  out
}
