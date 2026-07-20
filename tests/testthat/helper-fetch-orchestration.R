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
    oaf_body = oaf_test_body(),
    calls = NULL,
    events = NULL) {
  csv_position <- 0L
  force(fail_csv_on)
  force(oaf_body)
  function(request) {
    handler <- if (grepl("/collections/", request$url, fixed = TRUE)) {
      "ogc_api_features"
    } else {
      "csv"
    }
    if (is.environment(calls)) {
      calls$handlers <- c(calls$handlers, handler)
      calls$requests <- c(calls$requests %||% list(), list(request))
    }
    if (is.environment(events)) events$values <- c(events$values, handler)
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

fetch_orchestration_test_build <- function(
    plan = oaf_test_m7d_plan(),
    dry_run = FALSE,
    performer = fetch_orchestration_test_performer(),
    max_executions = 4L,
    max_total_bytes = 80000,
    max_fields = 1000L,
    oaf_limit = 2L,
    timeout = 15,
    min_interval = 0,
    scope = fetch_orchestration_test_scope(),
    oaf_symbol_resolver = oaf_test_resolver()) {
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
    oaf_symbol_resolver = oaf_symbol_resolver
  )
}
