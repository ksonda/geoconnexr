wqp_test_clone <- function(x) unserialize(serialize(x, NULL))

wqp_test_error <- function(expr) tryCatch(expr, error = identity)

wqp_test_fixture_path <- function(name = "result-narrow.csv") {
  testthat::test_path("fixtures", "fetch", "wqp", name)
}

wqp_test_body <- function(name = "result-narrow.csv") {
  path <- wqp_test_fixture_path(name)
  readBin(path, what = "raw", n = file.info(path)$size)
}

wqp_test_distribution_id <- function(plan) {
  position <- which(plan$coverage$handler_id == "wqp")
  plan$coverage$distribution_id[[position]]
}

wqp_test_request_plan <- function(
    plan = oaf_test_m7d_plan(), max_fields = 1000L) {
  gx_wqp_request_plan_impl(
    plan,
    distribution_id = wqp_test_distribution_id(plan),
    max_fields = max_fields
  )
}

wqp_test_scope <- function(label = "execution") {
  gx_contract_hash(
    list("fixture", label),
    namespace = "geoconnexr.wqp-test.v1",
    contract_version = "0.1.0"
  )
}

wqp_test_parser <- function(events = NULL, mismatch = FALSE) {
  function(obs_url, tz, csv, convertType) {
    if (is.environment(events)) events$values <- c(events$values, "parse")
    stopifnot(
      is.character(obs_url), length(obs_url) == 1L,
      !grepl("https://", obs_url, fixed = TRUE)
    )
    parsed <- utils::read.csv(
      text = obs_url,
      colClasses = "character",
      check.names = FALSE,
      na.strings = c("", "NA"),
      stringsAsFactors = FALSE
    )
    if (mismatch) parsed[[1L]][[1L]] <- "forged"
    tibble::as_tibble(parsed, .name_repair = "minimal")
  }
}

wqp_test_resolver <- function(
    events = NULL, available = TRUE, mismatch = FALSE) {
  function(package, symbol) {
    if (is.environment(events)) events$values <- c(events$values, "resolve")
    if (!available) return(NULL)
    wqp_test_parser(events = events, mismatch = mismatch)
  }
}

wqp_test_performer <- function(
    body = wqp_test_body(),
    status = 200L,
    media_type = "text/csv",
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

wqp_test_options <- function(performer) {
  gx_http_throttle_reset()
  withr::local_options(
    list(
      geoconnexr.performer = performer,
      geoconnexr.dns_resolver = function(host) "8.8.8.8",
      geoconnexr.clock = csv_execution_test_clock(),
      geoconnexr.throttle_clock = function() 0,
      geoconnexr.throttle_sleep = function(seconds) {
        stop("A zero-interval WQP execution attempted to sleep.", call. = FALSE)
      }
    ),
    .local_envir = parent.frame()
  )
  invisible(NULL)
}

wqp_test_execute <- function(
    request_plan = wqp_test_request_plan(),
    performer = wqp_test_performer(),
    symbol_resolver = wqp_test_resolver(),
    scope = wqp_test_scope()) {
  wqp_test_options(performer)
  gx_wqp_execution_impl(
    request_plan,
    timeout = 15,
    min_interval = 0,
    execution_scope_id = scope,
    symbol_resolver = symbol_resolver
  )
}
