.gx_fetched_contract_version <- "0.1.0"

.gx_fetched_fields <- c(
  "contract_version", "plan", "status", "results", "provenance", "metadata"
)

.gx_fetched_status_columns <- c(
  "contract_version", "selection_order", "fetch_order", "distribution_id",
  "handler_id", "status", "attempted", "succeeded", "physical_attempts",
  "encoded_bytes", "decoded_bytes", "execution_id", "result_index",
  "result_id", "error_code"
)

.gx_fetched_result_columns <- c(
  "contract_version", "result_index", "result_id", "distribution_id",
  "handler_id", "payload_class", "row_count", "column_count",
  "raw_body_available", "data", "raw_body"
)

.gx_fetched_supported_handlers <- c(
  "csv", "wqp", "edr", "usgs_waterdata_continuous",
  "usgs_waterdata_daily", "ogc_api_features"
)

.gx_fetch_scope_state <- new.env(parent = emptyenv())
.gx_fetch_scope_state$counter <- 0

.gx_fetched_metadata_fields <- c(
  "scope", "supported_handlers", "parallel", "dry_run", "pagination",
  "latest_usgs", "legacy_usgs", "registration", "replayable",
  "execution_status", "counts", "limitations"
)

.gx_fetched_count_fields <- c(
  "distributions", "candidate_requests", "attempted", "succeeded", "failed",
  "results", "physical_attempts", "encoded_bytes", "decoded_bytes"
)

gx_fetched_abort <- function(
    message,
    class = "gx_error_fetched_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_fetched", "gx_error_fetch_plan")),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_fetch_parallel_impl <- function(parallel) {
  if (!is.numeric(parallel) || length(parallel) != 1L || is.na(parallel) ||
      !is.finite(parallel) || parallel != 1 || !is.null(attributes(parallel))) {
    gx_fetched_abort(
      "The frozen fetch contract supports only sequential execution with `parallel = 1L`.",
      "gx_error_fetch_parallel"
    )
  }
  1L
}

gx_fetch_dry_run_impl <- function(dry_run) {
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run) ||
      !is.null(attributes(dry_run))) {
    gx_fetched_abort(
      "`dry_run` must be one explicit logical value.",
      "gx_error_fetch_policy"
    )
  }
  unname(dry_run)
}

gx_fetch_public_limits_impl <- function() {
  list(
    max_response_bytes = 8L * 1024L^2,
    max_rows = 100000L,
    max_columns = 1000L,
    max_fields = 1000000L,
    max_executions = 32L,
    max_total_bytes = 64 * 1024^2,
    oaf_limit = 10000L,
    timeout = 60,
    min_interval = 0.2
  )
}

gx_fetch_scope_impl <- function(plan, dry_run) {
  created_at <- as.numeric(plan$metadata$created_at)
  if (dry_run) {
    return(gx_contract_hash(
      list(
        "mode", "dry_run",
        "datasets_sha256", plan$source$datasets_sha256,
        "created_at", created_at
      ),
      namespace = "geoconnexr.public-fetch-scope.v1",
      contract_version = .gx_fetched_contract_version
    ))
  }
  counter <- get0(
    "counter", envir = .gx_fetch_scope_state, inherits = FALSE,
    ifnotfound = 0
  ) + 1
  if (!is.finite(counter) || counter > .Machine$integer.max) counter <- 1
  .gx_fetch_scope_state$counter <- counter
  gx_contract_hash(
    list(
      "mode", "live",
      "datasets_sha256", plan$source$datasets_sha256,
      "created_at", created_at,
      "pid", as.character(Sys.getpid()),
      "started_at", as.numeric(gx_now()),
      "counter", as.double(counter)
    ),
    namespace = "geoconnexr.public-fetch-scope.v1",
    contract_version = .gx_fetched_contract_version
  )
}

gx_fetched_plan_from_provenance_impl <- function(provenance) {
  provenance$request_plan$intent_set$plan
}

gx_fetched_status_impl <- function(provenance) {
  source <- provenance$status
  result_id <- rep.int(NA_character_, nrow(source))
  successful <- which(source$succeeded)
  if (length(successful)) {
    result_id[successful] <- vapply(
      provenance$results,
      function(result) result$result_id,
      character(1),
      USE.NAMES = FALSE
    )
  }
  tibble::tibble(
    contract_version = rep.int(.gx_fetched_contract_version, nrow(source)),
    selection_order = unname(source$selection_order),
    fetch_order = unname(source$fetch_order),
    distribution_id = unname(source$distribution_id),
    handler_id = unname(source$handler_id),
    status = unname(source$orchestration_status),
    attempted = unname(source$attempted),
    succeeded = unname(source$succeeded),
    physical_attempts = unname(source$physical_attempts),
    encoded_bytes = unname(source$encoded_bytes),
    decoded_bytes = unname(source$decoded_bytes),
    execution_id = unname(source$execution_id),
    result_index = unname(source$result_index),
    result_id = result_id,
    error_code = unname(source$error_code)
  )
}

gx_fetched_empty_results_impl <- function() {
  tibble::tibble(
    contract_version = character(),
    result_index = integer(),
    result_id = character(),
    distribution_id = character(),
    handler_id = character(),
    payload_class = character(),
    row_count = integer(),
    column_count = integer(),
    raw_body_available = logical(),
    data = list(),
    raw_body = list()
  )
}

gx_fetched_results_impl <- function(provenance) {
  count <- length(provenance$results)
  if (!count) return(gx_fetched_empty_results_impl())

  result_id <- character(count)
  distribution_id <- character(count)
  handler_id <- character(count)
  payload_class <- character(count)
  row_count <- integer(count)
  column_count <- integer(count)
  raw_body_available <- logical(count)
  data <- vector("list", count)
  raw_body <- vector("list", count)

  for (index in seq_len(count)) {
    source <- provenance$results[[index]]
    status_position <- which(provenance$status$result_index == index)
    value <- if (inherits(source, "gx_oaf_orchestration_result")) {
      source$result
    } else {
      source$data
    }
    body_available <- "response_body" %in% names(source)
    body <- if (body_available) source$response_body else raw()

    result_id[[index]] <- source$result_id
    distribution_id[[index]] <-
      provenance$status$distribution_id[[status_position]]
    handler_id[[index]] <-
      provenance$status$handler_id[[status_position]]
    payload_class[[index]] <- if (inherits(value, "sf")) "sf" else "table"
    row_count[[index]] <- as.integer(nrow(value))
    column_count[[index]] <- as.integer(ncol(value))
    raw_body_available[[index]] <- body_available
    data[[index]] <- value
    raw_body[[index]] <- body
  }

  tibble::tibble(
    contract_version = rep.int(.gx_fetched_contract_version, count),
    result_index = seq_len(count),
    result_id = result_id,
    distribution_id = distribution_id,
    handler_id = handler_id,
    payload_class = payload_class,
    row_count = row_count,
    column_count = column_count,
    raw_body_available = raw_body_available,
    data = data,
    raw_body = raw_body
  )
}

gx_fetched_metadata_impl <- function(provenance) {
  source <- provenance$metadata$counts
  status <- provenance$status
  limitations <- c(
    "latest_usgs_deferred", "legacy_usgs_deferred",
    "pagination_deferred", "registration_deferred", "replay_deferred"
  )
  limitations <- limitations[gx_catalog_byte_order(limitations)]
  list(
    scope = "supported_subset_v1",
    supported_handlers = .gx_fetched_supported_handlers,
    parallel = 1L,
    dry_run = provenance$policy$dry_run,
    pagination = "single_page_no_follow",
    latest_usgs = FALSE,
    legacy_usgs = FALSE,
    registration = FALSE,
    replayable = FALSE,
    execution_status = provenance$orchestration$orchestration_status,
    counts = list(
      distributions = unname(as.integer(nrow(status))),
      candidate_requests = source$candidate_requests,
      attempted = unname(as.integer(sum(status$attempted))),
      succeeded = unname(as.integer(sum(status$succeeded))),
      failed = unname(as.integer(sum(status$attempted & !status$succeeded))),
      results = unname(as.integer(length(provenance$results))),
      physical_attempts = source$physical_attempts,
      encoded_bytes = source$encoded_bytes,
      decoded_bytes = source$decoded_bytes
    ),
    limitations = limitations
  )
}

gx_fetched_new_impl <- function(provenance) {
  gx_fetch_orchestration_validate_impl(provenance)
  object <- structure(
    list(
      contract_version = .gx_fetched_contract_version,
      plan = gx_fetched_plan_from_provenance_impl(provenance),
      status = gx_fetched_status_impl(provenance),
      results = gx_fetched_results_impl(provenance),
      provenance = provenance,
      metadata = gx_fetched_metadata_impl(provenance)
    ),
    class = "gx_fetched"
  )
  gx_fetched_validate_impl(object)
  object
}

gx_fetched_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_fetched") &&
    identical(names(x), .gx_fetched_fields) &&
    identical(x$contract_version, .gx_fetched_contract_version) &&
    gx_csv_execution_exact_attributes(x, c("names", "class")) &&
    is.null(attributes(x$contract_version))
  if (!valid_top) {
    gx_fetched_abort("Fetched results violate their exact top-level contract.")
  }

  valid_plan <- tryCatch({
    gx_fetch_plan_validate_impl(x$plan)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  valid_provenance <- tryCatch({
    gx_fetch_orchestration_validate_impl(x$provenance)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid_plan || !valid_provenance) {
    gx_fetched_abort(
      "Fetched-result plan or provenance violates its validated contract."
    )
  }
  source_plan <- gx_fetched_plan_from_provenance_impl(x$provenance)
  if (!identical(x$plan, source_plan)) {
    gx_fetched_abort("Fetched-result provenance no longer binds its source plan.")
  }

  expected_status <- gx_fetched_status_impl(x$provenance)
  expected_results <- gx_fetched_results_impl(x$provenance)
  expected_metadata <- gx_fetched_metadata_impl(x$provenance)
  valid_status <- identical(names(x$status), .gx_fetched_status_columns)
  valid_results <- identical(names(x$results), .gx_fetched_result_columns)
  valid_metadata <- is.list(x$metadata) && identical(
    names(x$metadata), .gx_fetched_metadata_fields
  ) && is.list(x$metadata$counts) && identical(
    names(x$metadata$counts), .gx_fetched_count_fields
  )
  if (!valid_status || !valid_results || !valid_metadata ||
      !identical(x$status, expected_status) ||
      !identical(x$results, expected_results) ||
      !identical(x$metadata, expected_metadata)) {
    gx_fetched_abort(
      "Fetched status, payload, or metadata facts no longer match provenance."
    )
  }
  invisible(x)
}

gx_fetch_impl <- function(
    plan,
    parallel = 1L,
    dry_run = FALSE,
    limits = gx_fetch_public_limits_impl(),
    orchestration_scope_id = NULL,
    oaf_symbol_resolver = NULL,
    wqp_symbol_resolver = NULL,
    edr_symbol_resolver = NULL,
    usgs_continuous_symbol_resolver = NULL,
    usgs_daily_symbol_resolver = NULL) {
  gx_fetch_plan_validate_impl(plan)
  parallel <- gx_fetch_parallel_impl(parallel)
  dry_run <- gx_fetch_dry_run_impl(dry_run)
  expected_limits <- gx_fetch_public_limits_impl()
  if (!is.list(limits) || !identical(names(limits), names(expected_limits))) {
    gx_fetched_abort(
      "Internal fetch limits violate the frozen supported-subset shape.",
      "gx_error_fetch_policy"
    )
  }

  intent_set <- gx_csv_get_intents_impl(plan)
  request_plan <- gx_csv_request_plan_impl(
    intent_set,
    max_response_bytes = limits$max_response_bytes,
    max_rows = limits$max_rows,
    max_columns = limits$max_columns
  )
  if (is.null(orchestration_scope_id)) {
    orchestration_scope_id <- gx_fetch_scope_impl(plan, dry_run)
  }
  provenance <- gx_fetch_orchestration_impl(
    request_plan = request_plan,
    dry_run = dry_run,
    max_executions = limits$max_executions,
    max_total_bytes = limits$max_total_bytes,
    max_fields = limits$max_fields,
    oaf_limit = limits$oaf_limit,
    timeout = limits$timeout,
    min_interval = limits$min_interval,
    orchestration_scope_id = orchestration_scope_id,
    oaf_symbol_resolver = oaf_symbol_resolver,
    wqp_symbol_resolver = wqp_symbol_resolver,
    edr_symbol_resolver = edr_symbol_resolver,
    usgs_continuous_symbol_resolver = usgs_continuous_symbol_resolver,
    usgs_daily_symbol_resolver = usgs_daily_symbol_resolver
  )
  gx_fetched_new_impl(provenance)
}

#' Build a bounded fetch plan
#'
#' Collapses a validated `gx_catalog` to one deterministic row per
#' distribution, intersects requested time coverage, applies the built-in
#' first-match handler registry, and records count and byte budgets. Planning
#' performs no DNS lookup or provider request.
#'
#' The frozen M7 contract accepts only the built-in registry returned by
#' [gx_handlers()]. Runtime registration is deferred. `max_bytes` is applied
#' independently to encoded and decoded response budgets, and each selected
#' distribution receives at most one physical-attempt reservation.
#'
#' @param catalog A validated `gx_catalog` object.
#' @param time `NULL`, two UTC `POSIXct` values, or an exact list containing
#'   `start` and `end` UTC bounds.
#' @param handlers The exact built-in registry returned by [gx_handlers()].
#' @param max_datasets Maximum number of distributions to select.
#' @param max_bytes Maximum aggregate encoded bytes and maximum aggregate
#'   decoded bytes.
#'
#' @return A validated `gx_fetch_plan` object.
#' @export
gx_fetch_plan <- function(
    catalog,
    time = NULL,
    handlers = gx_handlers(),
    max_datasets = 100L,
    max_bytes = 1e9) {
  if (!identical(handlers, gx_handlers())) {
    gx_fetch_plan_abort(
      "The frozen M7 plan accepts only the exact built-in handler registry.",
      "gx_error_fetch_plan_handler"
    )
  }
  gx_fetch_plan_impl(
    catalog = catalog,
    time = time,
    max_datasets = max_datasets,
    max_requests = max_datasets,
    max_encoded_bytes = max_bytes,
    max_decoded_bytes = max_bytes
  )
}

#' Fetch the supported M7 data subset
#'
#' Executes the six frozen handler families—direct CSV, WQP Result, EDR
#' position, current USGS continuous, current USGS daily, and OGC API
#' Features—in deterministic global order. Failures are isolated to one
#' distribution and remain visible in the returned status table.
#'
#' Execution is deliberately sequential and single-page. Each response is
#' capped at 8 MiB, one call admits at most 32 requests and 64 MiB, tabular
#' payloads are bounded to 100,000 rows, 1,000 columns, and 1,000,000 fields,
#' and OGC API Features requests use a 10,000-feature page ceiling. Redirects,
#' retries, and cache reuse remain disabled by the underlying execution
#' contract. Latest and legacy USGS variants, page following, runtime handler
#' registration, serialization, and replay are deferred rather than silently
#' attempted.
#'
#' A dry run performs the same planning and admission without package probing,
#' DNS, provider transport, clocks, throttling, cache access, or writes.
#'
#' @param plan A validated `gx_fetch_plan` from [gx_fetch_plan()].
#' @param parallel Must be exactly `1L`; parallel execution is not part of the
#'   frozen M7 contract.
#' @param dry_run If `TRUE`, return planned statuses without provider work.
#'
#' @return A `gx_fetched` object. `$status` contains one row per distribution;
#'   `$results` contains one row per successful handler-native payload with
#'   `data` and retained `raw_body` list-columns; `$provenance` retains the
#'   validated bounded execution contract.
#' @export
gx_fetch <- function(plan, parallel = 1L, dry_run = FALSE) {
  gx_fetch_impl(plan, parallel = parallel, dry_run = dry_run)
}

#' @export
print.gx_fetched <- function(x, ...) {
  gx_fetched_validate_impl(x)
  counts <- x$metadata$counts
  cli::cli_inform(c(
    "<gx_fetched>",
    paste0(
      "* Mode: ", if (x$metadata$dry_run) "dry run" else "live",
      "; results: ", counts$results, "/", counts$candidate_requests
    ),
    paste0(
      "* Attempted: ", counts$attempted, "; succeeded: ", counts$succeeded,
      "; failed: ", counts$failed
    ),
    paste0(
      "* Physical attempts: ", counts$physical_attempts,
      "; decoded bytes: ", format(counts$decoded_bytes, scientific = FALSE)
    ),
    "* Scope: supported subset v1; pagination: single page, no follow"
  ))
  invisible(x)
}
