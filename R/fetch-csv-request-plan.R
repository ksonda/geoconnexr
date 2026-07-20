.gx_csv_request_plan_contract_version <- "0.1.0"
.gx_csv_request_plan_max_text_bytes <- 128L * 1024L^2

.gx_csv_request_plan_fields <- c(
  "contract_version", "intent_set", "policy", "budgets", "reservations",
  "request_plans", "coverage", "metadata"
)

.gx_csv_request_plan_policy_fields <- c(
  "slice_id", "method", "accept", "accept_encoding", "body_bytes",
  "body_sha256", "credential_policy", "redirect_policy", "max_redirects",
  "retry_policy", "max_retries", "max_physical_attempts", "cache_policy",
  "success_status", "response_media_types", "response_content_encoding",
  "allocation_policy", "max_response_bytes", "max_rows", "max_columns",
  "parser_policy"
)

.gx_csv_request_plan_budget_fields <- c(
  "source_budgets", "reserved_requests", "reserved_encoded_bytes",
  "reserved_decoded_bytes", "remaining_requests", "remaining_encoded_bytes",
  "remaining_decoded_bytes"
)

.gx_csv_request_plan_reservation_columns <- c(
  "contract_version", "reservation_order", "reservation_id",
  "distribution_id", "fetch_order", "handler_id", "max_physical_attempts",
  "max_encoded_bytes", "max_decoded_bytes", "reservation_status"
)

.gx_csv_request_plan_request_columns <- c(
  "contract_version", "request_order", "logical_request_id", "intent_id",
  "reservation_id", "distribution_id", "fetch_order", "handler_id",
  "method", "canonical_url_redacted", "declared_media_type",
  "max_physical_attempts", "max_encoded_bytes", "max_decoded_bytes",
  "response_byte_limit", "max_rows", "max_columns", "request_status"
)

.gx_csv_request_plan_coverage_columns <- c(
  "contract_version", "selection_order", "fetch_order", "distribution_id",
  "handler_id", "selected", "plan_decision", "intent_id", "reservation_id",
  "logical_request_id", "request_status"
)

.gx_csv_request_plan_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "budgets_allocated", "budgets_consumed", "allocation_complete", "counts",
  "non_replayable_reasons"
)

.gx_csv_request_plan_count_fields <- c(
  "distributions", "selected", "intents", "reservations", "request_plans",
  "csv_request_planned", "csv_budget_deferred", "handler_reserved",
  "handler_budget_deferred", "not_selected", "reference_only",
  "physical_attempts_reserved", "requests_executed",
  "physical_attempts_executed"
)

.gx_csv_request_plan_reservation_statuses <- c(
  "csv_request_planned", "held_deferred_handler"
)

.gx_csv_request_plan_coverage_statuses <- c(
  "csv_request_planned", "csv_budget_deferred", "handler_reserved",
  "handler_budget_deferred", "not_selected", "reference_only"
)

gx_csv_request_plan_abort <- function(
    message,
    class = "gx_error_csv_request_plan_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_csv_request_plan", "gx_error_fetch_plan"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_csv_request_plan_exact_attributes <- function(x, expected) {
  observed <- names(attributes(x))
  is.character(observed) && !anyNA(observed) &&
    length(observed) == length(expected) && all(expected %in% observed)
}

gx_csv_request_plan_table_attributes <- function(x, rows) {
  expected_rows <- if (rows == 0L) integer() else {
    c(NA_integer_, -as.integer(rows))
  }
  gx_csv_request_plan_exact_attributes(
    x, c("class", "row.names", "names")
  ) && identical(.row_names_info(x, type = 0L), expected_rows) &&
    all(vapply(x, function(column) is.null(attributes(column)), logical(1)))
}

gx_csv_request_plan_limit_impl <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x) || x > .Machine$integer.max) {
    gx_csv_request_plan_abort(
      "Direct-CSV request limits must be explicit positive whole numbers.",
      "gx_error_csv_request_plan_budget"
    )
  }
  unname(as.integer(x))
}

gx_csv_request_plan_policy_impl <- function(
    max_response_bytes, max_rows, max_columns) {
  max_response_bytes <- gx_csv_request_plan_limit_impl(
    max_response_bytes, "max_response_bytes"
  )
  max_rows <- gx_csv_request_plan_limit_impl(max_rows, "max_rows")
  max_columns <- gx_csv_request_plan_limit_impl(max_columns, "max_columns")
  list(
    slice_id = "direct_csv_request_plan_v1",
    method = "GET",
    accept = "text/csv, application/csv;q=0.9",
    accept_encoding = "identity",
    body_bytes = 0L,
    body_sha256 = .gx_csv_get_intents_empty_body_sha256,
    credential_policy = "source_url_opaque_no_additional_credentials",
    redirect_policy = "reject",
    max_redirects = 0L,
    retry_policy = "none",
    max_retries = 0L,
    max_physical_attempts = 1L,
    cache_policy = "bypass",
    success_status = 200L,
    response_media_types = c("text/csv", "application/csv"),
    response_content_encoding = "identity",
    allocation_policy = "global_selected_fair_share_v1",
    max_response_bytes = max_response_bytes,
    max_rows = max_rows,
    max_columns = max_columns,
    parser_policy = "shape_limits_only"
  )
}

gx_csv_request_plan_empty_reservations <- function() {
  tibble::tibble(
    contract_version = character(), reservation_order = integer(),
    reservation_id = character(), distribution_id = character(),
    fetch_order = integer(), handler_id = character(),
    max_physical_attempts = integer(), max_encoded_bytes = numeric(),
    max_decoded_bytes = numeric(), reservation_status = character()
  )
}

gx_csv_request_plan_empty_requests <- function() {
  tibble::tibble(
    contract_version = character(), request_order = integer(),
    logical_request_id = character(), intent_id = character(),
    reservation_id = character(), distribution_id = character(),
    fetch_order = integer(), handler_id = character(), method = character(),
    canonical_url_redacted = character(), declared_media_type = character(),
    max_physical_attempts = integer(), max_encoded_bytes = numeric(),
    max_decoded_bytes = numeric(), response_byte_limit = numeric(),
    max_rows = integer(), max_columns = integer(), request_status = character()
  )
}

gx_csv_request_plan_empty_coverage <- function() {
  tibble::tibble(
    contract_version = character(), selection_order = integer(),
    fetch_order = integer(), distribution_id = character(),
    handler_id = character(), selected = logical(), plan_decision = character(),
    intent_id = character(), reservation_id = character(),
    logical_request_id = character(), request_status = character()
  )
}

gx_csv_request_plan_selected_impl <- function(intent_set) {
  distributions <- intent_set$plan$distributions
  index <- which(distributions$selected)
  if (!length(index)) return(integer())
  unname(index[order(distributions$fetch_order[index])])
}

gx_csv_request_plan_reservation_count_impl <- function(intent_set) {
  budgets <- intent_set$plan$budgets
  selected <- gx_csv_request_plan_selected_impl(intent_set)
  as.integer(min(
    length(selected),
    as.double(budgets$max_requests),
    floor(budgets$max_encoded_bytes),
    floor(budgets$max_decoded_bytes)
  ))
}

gx_csv_request_plan_partition_impl <- function(total, count, ceiling) {
  if (count == 0L) return(numeric())
  reserved <- min(
    as.double(total), as.double(count) * as.double(ceiling)
  )
  quotient <- floor(reserved / as.double(count))
  remainder <- as.integer(reserved - quotient * as.double(count))
  values <- rep.int(as.double(quotient), count)
  if (remainder > 0L) {
    values[seq_len(remainder)] <- values[seq_len(remainder)] + 1
  }
  unname(values)
}

gx_csv_request_plan_byte_hash_value <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 0 || x != floor(x) || x > .gx_fetch_plan_max_safe_integer) {
    gx_csv_request_plan_abort(
      "A direct-CSV byte identity input is invalid.",
      "gx_error_csv_request_plan_budget"
    )
  }
  unname(sprintf("%.0f", as.double(x)))
}

gx_csv_request_plan_reservation_id_impl <- function(
    distribution_id, fetch_order, handler_id, reservation_order,
    max_encoded_bytes, max_decoded_bytes, reservation_status,
    source_budgets, policy) {
  gx_contract_hash(
    list(
      "distribution_id", distribution_id,
      "fetch_order", fetch_order,
      "handler_id", handler_id,
      "reservation_order", reservation_order,
      "max_physical_attempts", policy$max_physical_attempts,
      "max_encoded_bytes", gx_csv_request_plan_byte_hash_value(
        max_encoded_bytes
      ),
      "max_decoded_bytes", gx_csv_request_plan_byte_hash_value(
        max_decoded_bytes
      ),
      "reservation_status", reservation_status,
      "source_max_datasets", source_budgets$max_datasets,
      "source_max_requests", source_budgets$max_requests,
      "source_max_encoded_bytes", gx_csv_request_plan_byte_hash_value(
        source_budgets$max_encoded_bytes
      ),
      "source_max_decoded_bytes", gx_csv_request_plan_byte_hash_value(
        source_budgets$max_decoded_bytes
      ),
      "allocation_policy", policy$allocation_policy,
      "max_response_bytes", policy$max_response_bytes
    ),
    namespace = "geoconnexr.fetch-budget-reservation.v1",
    contract_version = .gx_csv_request_plan_contract_version
  )
}

gx_csv_request_plan_reservations_impl <- function(intent_set, policy) {
  plan <- intent_set$plan
  distributions <- plan$distributions
  selected <- gx_csv_request_plan_selected_impl(intent_set)
  count <- gx_csv_request_plan_reservation_count_impl(intent_set)
  if (count == 0L) return(gx_csv_request_plan_empty_reservations())
  index <- selected[seq_len(count)]
  encoded <- gx_csv_request_plan_partition_impl(
    plan$budgets$max_encoded_bytes, count, policy$max_response_bytes
  )
  decoded <- gx_csv_request_plan_partition_impl(
    plan$budgets$max_decoded_bytes, count, policy$max_response_bytes
  )
  statuses <- ifelse(
    distributions$handler_id[index] == "csv",
    "csv_request_planned",
    "held_deferred_handler"
  )
  reservation_ids <- unname(vapply(seq_len(count), function(position) {
    i <- index[[position]]
    gx_csv_request_plan_reservation_id_impl(
      distributions$distribution_id[[i]],
      distributions$fetch_order[[i]],
      distributions$handler_id[[i]],
      as.integer(position),
      encoded[[position]],
      decoded[[position]],
      statuses[[position]],
      plan$budgets,
      policy
    )
  }, character(1)))
  tibble::tibble(
    contract_version = rep.int(
      .gx_csv_request_plan_contract_version, count
    ),
    reservation_order = as.integer(seq_len(count)),
    reservation_id = reservation_ids,
    distribution_id = unname(distributions$distribution_id[index]),
    fetch_order = unname(distributions$fetch_order[index]),
    handler_id = unname(distributions$handler_id[index]),
    max_physical_attempts = rep.int(policy$max_physical_attempts, count),
    max_encoded_bytes = encoded,
    max_decoded_bytes = decoded,
    reservation_status = unname(statuses)
  )
}

gx_csv_request_plan_budget_impl <- function(intent_set, reservations) {
  source <- intent_set$plan$budgets
  reserved_requests <- as.integer(sum(reservations$max_physical_attempts))
  reserved_encoded <- as.double(sum(reservations$max_encoded_bytes))
  reserved_decoded <- as.double(sum(reservations$max_decoded_bytes))
  list(
    source_budgets = source,
    reserved_requests = reserved_requests,
    reserved_encoded_bytes = reserved_encoded,
    reserved_decoded_bytes = reserved_decoded,
    remaining_requests = as.integer(source$max_requests - reserved_requests),
    remaining_encoded_bytes = as.double(
      source$max_encoded_bytes - reserved_encoded
    ),
    remaining_decoded_bytes = as.double(
      source$max_decoded_bytes - reserved_decoded
    )
  )
}

gx_csv_request_plan_logical_id_impl <- function(
    intent_id, reservation_id, distribution_id, fetch_order, handler_id,
    canonical_url, max_encoded_bytes, max_decoded_bytes, response_byte_limit,
    policy) {
  policy_values <- list()
  for (name in .gx_csv_request_plan_policy_fields) {
    value <- policy[[name]]
    if (length(value) == 1L) {
      policy_values <- c(policy_values, list(name, value))
    } else {
      policy_values <- c(
        policy_values,
        list(paste0(name, "_count"), as.integer(length(value)))
      )
      for (position in seq_along(value)) {
        policy_values <- c(
          policy_values,
          list(paste0(name, "_", position), value[[position]])
        )
      }
    }
  }
  gx_contract_hash(
    c(
      list(
        "intent_id", intent_id,
        "reservation_id", reservation_id,
        "distribution_id", distribution_id,
        "fetch_order", fetch_order,
        "handler_id", handler_id,
        "canonical_url", canonical_url,
        "max_encoded_bytes", gx_csv_request_plan_byte_hash_value(
          max_encoded_bytes
        ),
        "max_decoded_bytes", gx_csv_request_plan_byte_hash_value(
          max_decoded_bytes
        ),
        "response_byte_limit", gx_csv_request_plan_byte_hash_value(
          response_byte_limit
        )
      ),
      policy_values
    ),
    namespace = "geoconnexr.csv-logical-request.v1",
    contract_version = .gx_csv_request_plan_contract_version
  )
}

gx_csv_request_plan_requests_impl <- function(
    intent_set, policy, reservations) {
  index <- which(reservations$reservation_status == "csv_request_planned")
  if (!length(index)) return(gx_csv_request_plan_empty_requests())
  plan <- intent_set$plan
  distributions <- plan$distributions
  intents <- intent_set$intents
  targets <- vector("list", length(index))
  intent_rows <- integer(length(index))
  distribution_rows <- integer(length(index))
  for (position in seq_along(index)) {
    reservation_row <- index[[position]]
    distribution_rows[[position]] <- match(
      reservations$distribution_id[[reservation_row]],
      distributions$distribution_id
    )
    intent_rows[[position]] <- match(
      reservations$distribution_id[[reservation_row]], intents$distribution_id
    )
    targets[[position]] <- gx_csv_get_intents_target_impl(
      distributions$distribution_url[[distribution_rows[[position]]]]
    )
  }
  encoded <- unname(reservations$max_encoded_bytes[index])
  decoded <- unname(reservations$max_decoded_bytes[index])
  response_limits <- unname(pmin(encoded, decoded))
  logical_ids <- unname(vapply(seq_along(index), function(position) {
    reservation_row <- index[[position]]
    gx_csv_request_plan_logical_id_impl(
      intents$intent_id[[intent_rows[[position]]]],
      reservations$reservation_id[[reservation_row]],
      reservations$distribution_id[[reservation_row]],
      reservations$fetch_order[[reservation_row]],
      reservations$handler_id[[reservation_row]],
      targets[[position]]$url,
      encoded[[position]],
      decoded[[position]],
      response_limits[[position]],
      policy
    )
  }, character(1)))
  tibble::tibble(
    contract_version = rep.int(
      .gx_csv_request_plan_contract_version, length(index)
    ),
    request_order = as.integer(seq_along(index)),
    logical_request_id = logical_ids,
    intent_id = unname(intents$intent_id[intent_rows]),
    reservation_id = unname(reservations$reservation_id[index]),
    distribution_id = unname(reservations$distribution_id[index]),
    fetch_order = unname(reservations$fetch_order[index]),
    handler_id = rep.int("csv", length(index)),
    method = rep.int(policy$method, length(index)),
    canonical_url_redacted = unname(vapply(
      targets, `[[`, character(1), "redacted"
    )),
    declared_media_type = unname(intents$declared_media_type[intent_rows]),
    max_physical_attempts = rep.int(
      policy$max_physical_attempts, length(index)
    ),
    max_encoded_bytes = encoded,
    max_decoded_bytes = decoded,
    response_byte_limit = response_limits,
    max_rows = rep.int(policy$max_rows, length(index)),
    max_columns = rep.int(policy$max_columns, length(index)),
    request_status = rep.int("planned_non_executable", length(index))
  )
}

gx_csv_request_plan_coverage_impl <- function(
    intent_set, reservations, request_plans) {
  distributions <- intent_set$plan$distributions
  if (!nrow(distributions)) return(gx_csv_request_plan_empty_coverage())
  intents <- intent_set$intents
  intent_position <- match(distributions$distribution_id, intents$distribution_id)
  reservation_position <- match(
    distributions$distribution_id, reservations$distribution_id
  )
  request_position <- match(
    distributions$distribution_id, request_plans$distribution_id
  )
  intent_ids <- rep.int(NA_character_, nrow(distributions))
  reservation_ids <- rep.int(NA_character_, nrow(distributions))
  logical_ids <- rep.int(NA_character_, nrow(distributions))
  has_intent <- !is.na(intent_position)
  has_reservation <- !is.na(reservation_position)
  has_request <- !is.na(request_position)
  intent_ids[has_intent] <- intents$intent_id[intent_position[has_intent]]
  reservation_ids[has_reservation] <-
    reservations$reservation_id[reservation_position[has_reservation]]
  logical_ids[has_request] <-
    request_plans$logical_request_id[request_position[has_request]]
  statuses <- unname(vapply(seq_len(nrow(distributions)), function(i) {
    if (identical(distributions$decision[[i]], "reference_only")) {
      "reference_only"
    } else if (!distributions$selected[[i]]) {
      "not_selected"
    } else if (identical(distributions$handler_id[[i]], "csv")) {
      if (has_request[[i]]) "csv_request_planned" else "csv_budget_deferred"
    } else if (has_reservation[[i]]) {
      "handler_reserved"
    } else {
      "handler_budget_deferred"
    }
  }, character(1)))
  tibble::tibble(
    contract_version = rep.int(
      .gx_csv_request_plan_contract_version, nrow(distributions)
    ),
    selection_order = unname(distributions$selection_order),
    fetch_order = unname(distributions$fetch_order),
    distribution_id = unname(distributions$distribution_id),
    handler_id = unname(distributions$handler_id),
    selected = unname(distributions$selected),
    plan_decision = unname(distributions$decision),
    intent_id = intent_ids,
    reservation_id = reservation_ids,
    logical_request_id = logical_ids,
    request_status = statuses
  )
}

gx_csv_request_plan_counts_impl <- function(
    intent_set, reservations, request_plans, coverage) {
  count_status <- function(status) {
    as.integer(sum(coverage$request_status == status))
  }
  list(
    distributions = as.integer(nrow(coverage)),
    selected = as.integer(sum(coverage$selected)),
    intents = as.integer(nrow(intent_set$intents)),
    reservations = as.integer(nrow(reservations)),
    request_plans = as.integer(nrow(request_plans)),
    csv_request_planned = count_status("csv_request_planned"),
    csv_budget_deferred = count_status("csv_budget_deferred"),
    handler_reserved = count_status("handler_reserved"),
    handler_budget_deferred = count_status("handler_budget_deferred"),
    not_selected = count_status("not_selected"),
    reference_only = count_status("reference_only"),
    physical_attempts_reserved = as.integer(
      sum(reservations$max_physical_attempts)
    ),
    requests_executed = 0L,
    physical_attempts_executed = 0L
  )
}

gx_csv_request_plan_reasons_impl <- function(intent_set) {
  resolved <- c(
    "cache_policy_unbound", "credential_policy_unbound",
    "parser_limits_unbound", "redirect_policy_unbound",
    "request_budgets_unallocated", "request_plans_absent",
    "response_contract_unproven"
  )
  reasons <- c(
    setdiff(intent_set$metadata$non_replayable_reasons, resolved),
    "arbitrary_provider_client_unimplemented",
    "attempt_identity_unbound",
    "attempt_ledger_unbound",
    "csv_parser_enforcement_unimplemented",
    "csv_parser_semantics_unbound",
    "non_csv_request_plans_absent",
    "provider_transport_unauthorized",
    "response_validator_unimplemented",
    "result_schema_unbound",
    "runtime_package_preflight_required",
    "serialization_unbound",
    "timeout_policy_unbound",
    "transport_adapter_unimplemented"
  )
  reasons <- unique(reasons)
  reasons[gx_catalog_byte_order(reasons)]
}

gx_csv_request_plan_new_impl <- function(
    intent_set, policy, budgets, reservations, request_plans, coverage,
    metadata) {
  object <- structure(
    list(
      contract_version = .gx_csv_request_plan_contract_version,
      intent_set = intent_set,
      policy = policy,
      budgets = budgets,
      reservations = reservations,
      request_plans = request_plans,
      coverage = coverage,
      metadata = metadata
    ),
    class = "gx_csv_request_plan"
  )
  gx_csv_request_plan_validate_impl(object)
  object
}

gx_csv_request_plan_impl <- function(
    intent_set, max_response_bytes = NULL, max_rows = NULL,
    max_columns = NULL) {
  valid <- tryCatch({
    gx_csv_get_intents_validate_impl(intent_set)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid) {
    gx_csv_request_plan_abort(
      "M7d construction requires a valid M7c direct-CSV intent set.",
      "gx_error_csv_request_plan_input"
    )
  }
  policy <- gx_csv_request_plan_policy_impl(
    max_response_bytes, max_rows, max_columns
  )
  reservations <- gx_csv_request_plan_reservations_impl(intent_set, policy)
  budgets <- gx_csv_request_plan_budget_impl(intent_set, reservations)
  request_plans <- gx_csv_request_plan_requests_impl(
    intent_set, policy, reservations
  )
  coverage <- gx_csv_request_plan_coverage_impl(
    intent_set, reservations, request_plans
  )
  metadata <- list(
    host_specific = FALSE,
    replayable = FALSE,
    execution_ready = FALSE,
    transport_authorized = FALSE,
    budgets_allocated = TRUE,
    budgets_consumed = FALSE,
    allocation_complete = TRUE,
    counts = gx_csv_request_plan_counts_impl(
      intent_set, reservations, request_plans, coverage
    ),
    non_replayable_reasons = gx_csv_request_plan_reasons_impl(intent_set)
  )
  gx_csv_request_plan_new_impl(
    intent_set, policy, budgets, reservations, request_plans, coverage,
    metadata
  )
}

gx_csv_request_plan_validate_policy <- function(policy) {
  valid_shape <- is.list(policy) &&
    identical(names(policy), .gx_csv_request_plan_policy_fields) &&
    gx_csv_request_plan_exact_attributes(policy, "names")
  if (!valid_shape || !is.integer(policy$max_response_bytes) ||
      !is.integer(policy$max_rows) || !is.integer(policy$max_columns) ||
      length(policy$max_response_bytes) != 1L || length(policy$max_rows) != 1L ||
      length(policy$max_columns) != 1L) {
    gx_csv_request_plan_abort(
      "Direct-CSV request policy violates its exact shape."
    )
  }
  expected <- gx_csv_request_plan_policy_impl(
    policy$max_response_bytes, policy$max_rows, policy$max_columns
  )
  if (!identical(policy, expected) ||
      !all(vapply(policy, function(value) {
        is.null(attributes(value))
      }, logical(1)))) {
    gx_csv_request_plan_abort(
      "Direct-CSV request policy violates its exact bounded contract."
    )
  }
  if (!identical(
    digest::digest(raw(), algo = "sha256", serialize = FALSE),
    policy$body_sha256
  )) {
    gx_csv_request_plan_abort(
      "Direct-CSV request policy has an invalid empty-body binding."
    )
  }
  invisible(policy)
}

gx_csv_request_plan_validate_budgets <- function(budgets) {
  if (!is.list(budgets) ||
      !identical(names(budgets), .gx_csv_request_plan_budget_fields) ||
      !gx_csv_request_plan_exact_attributes(budgets, "names") ||
      !is.list(budgets$source_budgets) ||
      !identical(names(budgets$source_budgets), .gx_fetch_plan_budget_fields) ||
      !gx_csv_request_plan_exact_attributes(budgets$source_budgets, "names")) {
    gx_csv_request_plan_abort(
      "Direct-CSV budget allocation violates its exact shape."
    )
  }
  integer_names <- c("reserved_requests", "remaining_requests")
  byte_names <- c(
    "reserved_encoded_bytes", "reserved_decoded_bytes",
    "remaining_encoded_bytes", "remaining_decoded_bytes"
  )
  valid_integers <- all(vapply(budgets[integer_names], function(value) {
    is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L &&
      value <= .gx_fetch_plan_max_requests && is.null(attributes(value))
  }, logical(1)))
  valid_bytes <- all(vapply(budgets[byte_names], function(value) {
    is.double(value) && length(value) == 1L && !is.na(value) &&
      is.finite(value) && value >= 0 && value == floor(value) &&
      value <= .gx_fetch_plan_max_safe_integer && is.null(attributes(value))
  }, logical(1)))
  if (!valid_integers || !valid_bytes) {
    gx_csv_request_plan_abort(
      "Direct-CSV budget allocation contains invalid bounded values.",
      "gx_error_csv_request_plan_budget"
    )
  }
  invisible(budgets)
}

gx_csv_request_plan_validate_reservations <- function(reservations) {
  rows <- gx_catalog_table_rows(reservations)
  if (!inherits(reservations, "tbl_df") ||
      !identical(class(reservations), c("tbl_df", "tbl", "data.frame")) ||
      !identical(names(reservations), .gx_csv_request_plan_reservation_columns) ||
      is.null(rows) || rows > .gx_fetch_plan_max_distributions ||
      !gx_csv_request_plan_table_attributes(reservations, as.integer(rows))) {
    gx_csv_request_plan_abort(
      "Direct-CSV reservations violate their exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_fetch_plan_max_distributions) {
        "gx_error_csv_request_plan_budget"
      } else {
        "gx_error_csv_request_plan_contract"
      }
    )
  }
  integer_columns <- c(
    "reservation_order", "fetch_order", "max_physical_attempts"
  )
  numeric_columns <- c("max_encoded_bytes", "max_decoded_bytes")
  character_columns <- setdiff(
    .gx_csv_request_plan_reservation_columns,
    c(integer_columns, numeric_columns)
  )
  valid_types <- all(vapply(
    reservations[integer_columns], is.integer, logical(1)
  )) && all(vapply(
    reservations[numeric_columns], is.double, logical(1)
  )) && all(vapply(
    reservations[character_columns], is.character, logical(1)
  ))
  if (!valid_types || anyNA(reservations)) {
    gx_csv_request_plan_abort(
      "Direct-CSV reservation columns have invalid types or required values."
    )
  }
  for (name in character_columns) {
    gx_fetch_plan_assert_text(reservations[[name]], allow_na = FALSE, nonempty = TRUE)
  }
  increasing <- nrow(reservations) < 2L ||
    all(diff(reservations$fetch_order) > 0L)
  if (!identical(
    reservations$reservation_order, as.integer(seq_len(nrow(reservations)))
  ) || !increasing || any(reservations$fetch_order < 1L) ||
      any(reservations$max_physical_attempts != 1L) ||
      any(reservations$max_encoded_bytes < 1) ||
      any(reservations$max_decoded_bytes < 1) ||
      !gx_catalog_is_sha256(reservations$reservation_id) ||
      !gx_catalog_is_sha256(reservations$distribution_id) ||
      anyDuplicated(reservations$reservation_id) ||
      anyDuplicated(reservations$distribution_id) ||
      any(!reservations$reservation_status %in%
        .gx_csv_request_plan_reservation_statuses)) {
    gx_csv_request_plan_abort(
      "Direct-CSV reservation identities, ordering, or statuses are invalid."
    )
  }
  invisible(reservations)
}

gx_csv_request_plan_validate_requests <- function(request_plans) {
  rows <- gx_catalog_table_rows(request_plans)
  if (!inherits(request_plans, "tbl_df") ||
      !identical(class(request_plans), c("tbl_df", "tbl", "data.frame")) ||
      !identical(names(request_plans), .gx_csv_request_plan_request_columns) ||
      is.null(rows) || rows > .gx_fetch_plan_max_distributions ||
      !gx_csv_request_plan_table_attributes(request_plans, as.integer(rows))) {
    gx_csv_request_plan_abort(
      "Direct-CSV request plans violate their exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_fetch_plan_max_distributions) {
        "gx_error_csv_request_plan_budget"
      } else {
        "gx_error_csv_request_plan_contract"
      }
    )
  }
  integer_columns <- c(
    "request_order", "fetch_order", "max_physical_attempts", "max_rows",
    "max_columns"
  )
  numeric_columns <- c(
    "max_encoded_bytes", "max_decoded_bytes", "response_byte_limit"
  )
  character_columns <- setdiff(
    .gx_csv_request_plan_request_columns,
    c(integer_columns, numeric_columns)
  )
  valid_types <- all(vapply(
    request_plans[integer_columns], is.integer, logical(1)
  )) && all(vapply(
    request_plans[numeric_columns], is.double, logical(1)
  )) && all(vapply(
    request_plans[character_columns], is.character, logical(1)
  ))
  if (!valid_types || anyNA(request_plans[setdiff(
    names(request_plans), "declared_media_type"
  )])) {
    gx_csv_request_plan_abort(
      "Direct-CSV request-plan columns have invalid types or required values."
    )
  }
  for (name in character_columns) {
    gx_fetch_plan_assert_text(
      request_plans[[name]],
      allow_na = identical(name, "declared_media_type"),
      nonempty = !identical(name, "declared_media_type")
    )
  }
  increasing <- nrow(request_plans) < 2L ||
    all(diff(request_plans$fetch_order) > 0L)
  valid_redacted <- all(vapply(
    request_plans$canonical_url_redacted,
    function(value) tryCatch(
      isTRUE(gx_catalog_parseable_url(value, redacted = TRUE)),
      error = function(cnd) FALSE,
      warning = function(cnd) FALSE
    ),
    logical(1)
  ))
  if (!identical(
    request_plans$request_order, as.integer(seq_len(nrow(request_plans)))
  ) || !increasing || any(request_plans$fetch_order < 1L) ||
      any(request_plans$handler_id != "csv") ||
      any(request_plans$method != "GET") ||
      any(request_plans$max_physical_attempts != 1L) ||
      any(request_plans$max_encoded_bytes < 1) ||
      any(request_plans$max_decoded_bytes < 1) ||
      any(request_plans$response_byte_limit < 1) ||
      any(request_plans$max_rows < 1L) || any(request_plans$max_columns < 1L) ||
      any(request_plans$request_status != "planned_non_executable") ||
      !gx_catalog_is_sha256(request_plans$logical_request_id) ||
      !gx_catalog_is_sha256(request_plans$intent_id) ||
      !gx_catalog_is_sha256(request_plans$reservation_id) ||
      !gx_catalog_is_sha256(request_plans$distribution_id) ||
      anyDuplicated(request_plans$logical_request_id) ||
      anyDuplicated(request_plans$intent_id) ||
      anyDuplicated(request_plans$reservation_id) ||
      anyDuplicated(request_plans$distribution_id) || !valid_redacted) {
    gx_csv_request_plan_abort(
      "Direct-CSV logical request identities, ordering, or limits are invalid."
    )
  }
  invisible(request_plans)
}

gx_csv_request_plan_validate_coverage <- function(coverage) {
  rows <- gx_catalog_table_rows(coverage)
  if (!inherits(coverage, "tbl_df") ||
      !identical(class(coverage), c("tbl_df", "tbl", "data.frame")) ||
      !identical(names(coverage), .gx_csv_request_plan_coverage_columns) ||
      is.null(rows) || rows > .gx_fetch_plan_max_distributions ||
      !gx_csv_request_plan_table_attributes(coverage, as.integer(rows))) {
    gx_csv_request_plan_abort(
      "Direct-CSV request coverage violates its exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_fetch_plan_max_distributions) {
        "gx_error_csv_request_plan_budget"
      } else {
        "gx_error_csv_request_plan_contract"
      }
    )
  }
  integer_columns <- c("selection_order", "fetch_order")
  logical_columns <- "selected"
  character_columns <- setdiff(
    .gx_csv_request_plan_coverage_columns,
    c(integer_columns, logical_columns)
  )
  valid_types <- all(vapply(
    coverage[integer_columns], is.integer, logical(1)
  )) && is.logical(coverage$selected) && all(vapply(
    coverage[character_columns], is.character, logical(1)
  ))
  required <- c(
    "contract_version", "selection_order", "distribution_id", "handler_id",
    "selected", "plan_decision", "request_status"
  )
  if (!valid_types || anyNA(coverage[required])) {
    gx_csv_request_plan_abort(
      "Direct-CSV request coverage has invalid types or required values."
    )
  }
  for (name in character_columns) {
    gx_fetch_plan_assert_text(
      coverage[[name]],
      allow_na = name %in% c(
        "intent_id", "reservation_id", "logical_request_id"
      ),
      nonempty = TRUE
    )
  }
  if (!identical(
    coverage$selection_order, as.integer(seq_len(nrow(coverage)))
  ) || !gx_catalog_is_sha256(coverage$distribution_id) ||
      !gx_catalog_is_sha256(coverage$intent_id, allow_na = TRUE) ||
      !gx_catalog_is_sha256(coverage$reservation_id, allow_na = TRUE) ||
      !gx_catalog_is_sha256(coverage$logical_request_id, allow_na = TRUE) ||
      anyDuplicated(coverage$distribution_id) ||
      any(!coverage$request_status %in% .gx_csv_request_plan_coverage_statuses) ||
      any(!coverage$plan_decision %in% .gx_fetch_plan_decisions) ||
      any(coverage$selected != !is.na(coverage$fetch_order))) {
    gx_csv_request_plan_abort(
      "Direct-CSV request coverage identities or statuses are invalid."
    )
  }
  invisible(coverage)
}

gx_csv_request_plan_validate_metadata <- function(metadata) {
  if (!is.list(metadata) ||
      !identical(names(metadata), .gx_csv_request_plan_metadata_fields) ||
      !gx_csv_request_plan_exact_attributes(metadata, "names") ||
      !identical(metadata$host_specific, FALSE) ||
      !identical(metadata$replayable, FALSE) ||
      !identical(metadata$execution_ready, FALSE) ||
      !identical(metadata$transport_authorized, FALSE) ||
      !identical(metadata$budgets_allocated, TRUE) ||
      !identical(metadata$budgets_consumed, FALSE) ||
      !identical(metadata$allocation_complete, TRUE) ||
      !is.character(metadata$non_replayable_reasons) ||
      !length(metadata$non_replayable_reasons) ||
      anyNA(metadata$non_replayable_reasons) ||
      anyDuplicated(metadata$non_replayable_reasons) ||
      !gx_catalog_byte_sorted(metadata$non_replayable_reasons) ||
      !gx_catalog_is_token(metadata$non_replayable_reasons) ||
      !is.null(attributes(metadata$non_replayable_reasons))) {
    gx_csv_request_plan_abort(
      "Direct-CSV request metadata violates its exact non-executable contract."
    )
  }
  counts <- metadata$counts
  if (!is.list(counts) ||
      !identical(names(counts), .gx_csv_request_plan_count_fields) ||
      !gx_csv_request_plan_exact_attributes(counts, "names") ||
      !all(vapply(counts, function(value) {
        is.integer(value) && length(value) == 1L && !is.na(value) &&
          value >= 0L && value <= .gx_fetch_plan_max_distributions &&
          is.null(attributes(value))
      }, logical(1)))) {
    gx_csv_request_plan_abort(
      "Direct-CSV request counts violate their exact bounded contract."
    )
  }
  invisible(metadata)
}

gx_csv_request_plan_assert_text_budget <- function(x) {
  owned <- list(
    contract_version = x$contract_version,
    policy = x$policy,
    budgets = x$budgets,
    reservations = x$reservations,
    request_plans = x$request_plans,
    coverage = x$coverage,
    metadata = x$metadata
  )
  total <- gx_fetch_plan_text_total(
    owned, limit = .gx_csv_request_plan_max_text_bytes
  )
  if (!is.finite(total) || total > .gx_csv_request_plan_max_text_bytes) {
    gx_csv_request_plan_abort(
      "Direct-CSV request-plan text exceeds its aggregate byte budget.",
      "gx_error_csv_request_plan_budget"
    )
  }
  invisible(total)
}

gx_csv_request_plan_validate_cross_contract <- function(x) {
  expected_reservations <- gx_csv_request_plan_reservations_impl(
    x$intent_set, x$policy
  )
  expected_budgets <- gx_csv_request_plan_budget_impl(
    x$intent_set, expected_reservations
  )
  expected_requests <- gx_csv_request_plan_requests_impl(
    x$intent_set, x$policy, expected_reservations
  )
  expected_coverage <- gx_csv_request_plan_coverage_impl(
    x$intent_set, expected_reservations, expected_requests
  )
  if (!identical(x$reservations, expected_reservations) ||
      !identical(x$budgets, expected_budgets) ||
      !identical(x$request_plans, expected_requests) ||
      !identical(x$coverage, expected_coverage)) {
    gx_csv_request_plan_abort(
      "Direct-CSV request planning does not rebind to its intent set and budgets."
    )
  }
  expected_counts <- gx_csv_request_plan_counts_impl(
    x$intent_set, expected_reservations, expected_requests, expected_coverage
  )
  counts <- x$metadata$counts
  if (!identical(counts, expected_counts) ||
      counts$distributions != counts$csv_request_planned +
        counts$csv_budget_deferred + counts$handler_reserved +
        counts$handler_budget_deferred + counts$not_selected +
        counts$reference_only ||
      counts$selected != counts$csv_request_planned +
        counts$csv_budget_deferred + counts$handler_reserved +
        counts$handler_budget_deferred ||
      counts$request_plans != counts$csv_request_planned ||
      counts$reservations != counts$csv_request_planned +
        counts$handler_reserved ||
      counts$physical_attempts_reserved != counts$reservations ||
      counts$requests_executed != 0L ||
      counts$physical_attempts_executed != 0L) {
    gx_csv_request_plan_abort(
      "Direct-CSV request-plan counts do not reconcile exactly."
    )
  }
  if (!identical(
    x$metadata$non_replayable_reasons,
    gx_csv_request_plan_reasons_impl(x$intent_set)
  )) {
    gx_csv_request_plan_abort(
      "Direct-CSV request-plan blockers do not reconcile exactly."
    )
  }
  if (!identical(x$intent_set$plan$requests, list()) ||
      x$intent_set$plan$metadata$counts$requests != 0L ||
      x$intent_set$plan$metadata$execution_ready ||
      x$intent_set$metadata$counts$requests != 0L ||
      x$intent_set$metadata$execution_ready ||
      x$intent_set$metadata$transport_authorized) {
    gx_csv_request_plan_abort(
      "The embedded M7c and M7a contracts no longer remain request-empty."
    )
  }
  invisible(x)
}

gx_csv_request_plan_validate_body <- function(x) {
  if (!is.list(x) || !identical(class(x), "gx_csv_request_plan") ||
      !identical(names(x), .gx_csv_request_plan_fields) ||
      !gx_csv_request_plan_exact_attributes(x, c("names", "class")) ||
      !identical(x$contract_version, .gx_csv_request_plan_contract_version) ||
      !is.null(attributes(x$contract_version))) {
    gx_csv_request_plan_abort(
      "Direct-CSV request-plan objects violate their exact top-level contract."
    )
  }
  gx_csv_get_intents_validate_impl(x$intent_set)
  gx_csv_request_plan_validate_policy(x$policy)
  gx_csv_request_plan_validate_budgets(x$budgets)
  gx_csv_request_plan_validate_reservations(x$reservations)
  gx_csv_request_plan_validate_requests(x$request_plans)
  gx_csv_request_plan_validate_coverage(x$coverage)
  gx_csv_request_plan_validate_metadata(x$metadata)
  gx_csv_request_plan_assert_text_budget(x)
  gx_csv_request_plan_validate_cross_contract(x)
  invisible(x)
}

gx_csv_request_plan_validate_impl <- function(x) {
  tryCatch(
    gx_csv_request_plan_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_csv_request_plan")) stop(cnd)
      gx_csv_request_plan_abort(
        "Direct-CSV request-plan validation rejected a malformed object."
      )
    },
    warning = function(cnd) {
      gx_csv_request_plan_abort(
        "Direct-CSV request-plan validation rejected a warning-producing object."
      )
    }
  )
}

#' @export
print.gx_csv_request_plan <- function(x, ...) {
  gx_csv_request_plan_validate_impl(x)
  counts <- x$metadata$counts
  cli::cli_inform(c(
    "<gx_csv_request_plan>",
    paste0(
      "* Selected distributions: {counts$selected}; reservations: ",
      "{counts$reservations}"
    ),
    paste0(
      "* Non-executable CSV request plans: {counts$request_plans}; ",
      "budget-deferred CSV intents: {counts$csv_budget_deferred}"
    ),
    "* Requests executed: 0; transport authorized: FALSE; execution ready: FALSE"
  ))
  invisible(x)
}
