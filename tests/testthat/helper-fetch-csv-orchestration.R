csv_orchestration_test_clone <- function(x) {
  unserialize(serialize(x, NULL))
}

csv_orchestration_test_scope <- function(label = "batch") {
  gx_contract_hash(
    list("fixture", label),
    namespace = "geoconnexr.csv-orchestration-test.v1",
    contract_version = "0.1.0"
  )
}

csv_orchestration_test_performer <- function(
    fail_on = integer(),
    parse_fail_on = integer(),
    body = csv_response_validation_test_body(),
    calls = NULL) {
  position <- 0L
  force(fail_on)
  force(parse_fail_on)
  force(body)
  function(request) {
    position <<- position + 1L
    if (is.environment(calls)) {
      calls$requests <- c(calls$requests %||% list(), list(request))
    }
    if (position %in% fail_on) {
      stop("sensitive orchestration transport detail", call. = FALSE)
    }
    selected_body <- if (position %in% parse_fail_on) {
      charToRaw("a,b\n\"unterminated,2\n")
    } else {
      body
    }
    list(
      status = 200L,
      headers = list(
        `Content-Type` = "text/csv",
        `Content-Length` = as.character(length(selected_body))
      ),
      body = selected_body,
      url = request$url
    )
  }
}

csv_orchestration_test_build <- function(
    plan = csv_execution_test_plan(),
    dry_run = FALSE,
    performer = csv_orchestration_test_performer(),
    max_executions = 3L,
    max_total_bytes = 1000,
    max_fields = 1000L,
    timeout = 15,
    min_interval = 0,
    scope = csv_orchestration_test_scope()) {
  if (!dry_run) csv_execution_test_options(performer)
  gx_csv_orchestration_impl(
    request_plan = plan,
    dry_run = dry_run,
    max_executions = max_executions,
    max_total_bytes = max_total_bytes,
    max_fields = max_fields,
    timeout = timeout,
    min_interval = min_interval,
    orchestration_scope_id = scope
  )
}
