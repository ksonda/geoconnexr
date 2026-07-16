csv_intents_test_clone <- function(x) {
  unserialize(serialize(x, NULL))
}

csv_intents_test_error <- function(expr) {
  tryCatch(expr, error = identity)
}

csv_intents_test_build <- function(plan = fetch_plan_test_build()) {
  gx_csv_get_intents_impl(plan)
}

csv_intents_test_fixture_dir <- function() {
  testthat::test_path("fixtures", "fetch", "csv-intent")
}

csv_intents_test_read_json <- function(name) {
  jsonlite::fromJSON(
    file.path(csv_intents_test_fixture_dir(), name),
    simplifyVector = FALSE
  )
}

csv_intents_test_fixture_catalog <- function() {
  fixture <- csv_intents_test_read_json("cases-v1.json")
  cases <- fixture$cases
  sites <- fetch_plan_test_sites(length(cases))
  rows <- list()
  for (case_index in seq_along(cases)) {
    case <- cases[[case_index]]
    descriptor <- case$descriptor
    variables <- as.character(unlist(
      descriptor$variables, use.names = FALSE
    ))
    conforms_to <- as.character(unlist(
      descriptor$conforms_to, use.names = FALSE
    ))
    outside <- identical(case$case_id, "csv_outside_time")
    plan_url <- if (identical(
      case$case_id, "csv_query_fragment_default_port"
    )) {
      sub(":443", "", descriptor$url, fixed = TRUE)
    } else {
      case$expectation$canonical_transport_url
    }
    for (variable_index in seq_along(variables)) {
      rows[[length(rows) + 1L]] <- fetch_plan_test_dataset_row(
        site_uri = sites$site_uri[[case_index]],
        dataset_key = paste0("csv-intent-", case$case_id),
        distribution_key = descriptor$distribution_key,
        variable_key = paste0(
          case$case_id, "-", variables[[variable_index]]
        ),
        provider_key = sprintf("csv-intent-%02d", case_index),
        distribution_url = plan_url,
        media_type = descriptor$media_type,
        handler_id = case$expectation$classifier,
        fetchable = isTRUE(descriptor$fetchable),
        temporal_start = if (outside) {
          as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
        } else {
          as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
        },
        temporal_end = if (outside) {
          as.POSIXct("2023-12-31 23:59:59", tz = "UTC")
        } else {
          as.POSIXct("2025-12-31 23:59:59", tz = "UTC")
        },
        conforms_to = conforms_to
      )
    }
  }
  datasets <- tibble::as_tibble(do.call(rbind, rows))
  gx_catalog_new_impl(
    aoi = fetch_plan_test_aoi(),
    sites = sites,
    datasets = datasets,
    reference = list(),
    problems = gx_catalog_empty_problems(),
    requests = gx_catalog_empty_requests(),
    metadata = fetch_plan_test_metadata(sites, datasets)
  )
}

csv_intents_test_fixture_plan <- function() {
  fetch_plan_test_build(
    catalog = csv_intents_test_fixture_catalog(),
    time = fetch_plan_test_time(),
    max_datasets = 5L,
    max_requests = 0L,
    max_encoded_bytes = 0,
    max_decoded_bytes = 0
  )
}
