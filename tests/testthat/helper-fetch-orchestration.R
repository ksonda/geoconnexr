fetch_orchestration_test_clone <- function(x) {
  unserialize(serialize(x, NULL))
}

fetch_orchestration_test_scope <- function(label = "batch") {
  gx_contract_hash(
    list("fixture", label),
    namespace = "geoconnexr.fetch-orchestration-test.v1",
    contract_version = "0.1.0"
  )
}

fetch_orchestration_test_performer <- function(
    fail_csv_on = integer(),
    fail_edr = FALSE,
    fail_wqp = FALSE,
    edr_body = edr_test_body(),
    wqp_body = wqp_test_body(),
    oaf_body = oaf_test_body(),
    calls = NULL,
    events = NULL) {
  csv_position <- 0L
  force(fail_csv_on)
  force(fail_edr)
  force(fail_wqp)
  force(edr_body)
  force(wqp_body)
  force(oaf_body)
  function(request) {
    handler <- if (grepl("edr.example.org", request$url, fixed = TRUE)) {
      "edr"
    } else if (grepl("waterqualitydata.us", request$url, fixed = TRUE)) {
      "wqp"
    } else if (grepl("/collections/", request$url, fixed = TRUE)) {
      "ogc_api_features"
    } else {
      "csv"
    }
    if (is.environment(calls)) {
      calls$handlers <- c(calls$handlers, handler)
      calls$requests <- c(calls$requests %||% list(), list(request))
    }
    if (is.environment(events)) events$values <- c(events$values, handler)
    if (handler == "edr") {
      if (fail_edr) {
        stop("sensitive EDR transport detail", call. = FALSE)
      }
      return(list(
        status = 200L,
        headers = list(
          `Content-Type` = "application/prs.coverage+json",
          `Content-Length` = as.character(length(edr_body))
        ),
        body = edr_body,
        url = request$url
      ))
    }
    if (handler == "wqp") {
      if (fail_wqp) {
        stop("sensitive WQP transport detail", call. = FALSE)
      }
      return(list(
        status = 200L,
        headers = list(
          `Content-Type` = "text/csv",
          `Content-Length` = as.character(length(wqp_body))
        ),
        body = wqp_body,
        url = request$url
      ))
    }
    if (handler == "csv") {
      csv_position <<- csv_position + 1L
      if (csv_position %in% fail_csv_on) {
        stop("sensitive cross-handler transport detail", call. = FALSE)
      }
      body <- csv_response_validation_test_body()
      return(list(
        status = 200L,
        headers = list(
          `Content-Type` = "text/csv",
          `Content-Length` = as.character(length(body))
        ),
        body = body,
        url = request$url
      ))
    }
    list(
      status = 200L,
      headers = list(
        `Content-Type` = "application/geo+json",
        `Content-Length` = as.character(length(oaf_body))
      ),
      body = oaf_body,
      url = request$url
    )
  }
}

fetch_orchestration_test_edr_plan <- function(max_response_bytes = 20000) {
  catalog <- csv_intents_test_fixture_catalog()
  oaf_position <- which(
    catalog$datasets$handler_id == "ogc_api_features"
  )
  deferred_position <- which(grepl(
    "/overflow/skipped.csv", catalog$datasets$distribution_url, fixed = TRUE
  ))
  catalog$datasets$distribution_url[[oaf_position]] <- edr_test_url()
  catalog$datasets$media_type[[oaf_position]] <-
    "application/prs.coverage+json"
  catalog$datasets$handler_id[[oaf_position]] <- "edr"
  catalog$datasets$conforms_to[[oaf_position]] <-
    "http://www.opengis.net/spec/ogcapi-edr-1/1.1/conf/core"
  catalog$datasets$distribution_url[[deferred_position]] <-
    "https://features.example.org/collections/gages/items"
  catalog$datasets$media_type[[deferred_position]] <- "application/geo+json"
  catalog$datasets$handler_id[[deferred_position]] <- "ogc_api_features"
  catalog$datasets$conforms_to[[deferred_position]] <-
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core"
  catalog <- gx_catalog_new_impl(
    aoi = catalog$aoi,
    sites = catalog$sites,
    datasets = catalog$datasets,
    reference = catalog$reference,
    problems = catalog$problems,
    requests = catalog$requests,
    metadata = fetch_plan_test_metadata(catalog$sites, catalog$datasets)
  )
  intent_set <- csv_request_plan_test_intent_set(
    catalog = catalog,
    max_datasets = 6L,
    max_requests = 6L,
    max_encoded_bytes = as.double(max_response_bytes) * 6,
    max_decoded_bytes = as.double(max_response_bytes) * 6
  )
  csv_request_plan_test_build(
    intent_set = intent_set,
    max_response_bytes = max_response_bytes
  )
}

fetch_orchestration_test_build <- function(
    plan = oaf_test_m7d_plan(),
    dry_run = FALSE,
    performer = fetch_orchestration_test_performer(),
    max_executions = 5L,
    max_total_bytes = 100000,
    max_fields = 1000L,
    oaf_limit = 2L,
    timeout = 15,
    min_interval = 0,
    scope = fetch_orchestration_test_scope(),
    oaf_symbol_resolver = oaf_test_resolver(),
    wqp_symbol_resolver = wqp_test_resolver(),
    edr_symbol_resolver = edr_test_resolver()) {
  if (!dry_run) oaf_test_options(performer)
  gx_fetch_orchestration_impl(
    request_plan = plan,
    dry_run = dry_run,
    max_executions = max_executions,
    max_total_bytes = max_total_bytes,
    max_fields = max_fields,
    oaf_limit = oaf_limit,
    timeout = timeout,
    min_interval = min_interval,
    orchestration_scope_id = scope,
    oaf_symbol_resolver = oaf_symbol_resolver,
    wqp_symbol_resolver = wqp_symbol_resolver,
    edr_symbol_resolver = edr_symbol_resolver
  )
}
