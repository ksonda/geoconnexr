.gx_csv_get_intents_contract_version <- "0.1.0"
.gx_csv_get_intents_max_text_bytes <- 128L * 1024L^2
.gx_csv_get_intents_empty_body_sha256 <-
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

.gx_csv_get_intents_fields <- c(
  "contract_version", "plan", "policy", "intents", "coverage", "metadata"
)

.gx_csv_get_intents_policy_fields <- c(
  "slice_id", "method", "accept", "accept_encoding", "body_bytes",
  "body_sha256", "credential_policy", "redirect_policy", "cache_policy",
  "parser_policy"
)

.gx_csv_get_intents_intent_columns <- c(
  "contract_version", "intent_order", "intent_id", "distribution_id",
  "fetch_order", "handler_id", "declared_media_type",
  "canonical_url_redacted", "intent_status"
)

.gx_csv_get_intents_coverage_columns <- c(
  "contract_version", "selection_order", "fetch_order", "distribution_id",
  "handler_id", "selected", "plan_decision", "intent_id", "intent_status"
)

.gx_csv_get_intents_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "budgets_allocated", "counts", "non_replayable_reasons"
)

.gx_csv_get_intents_count_fields <- c(
  "distributions", "selected", "intents", "intent_created",
  "deferred_handler", "not_selected", "reference_only", "requests"
)

.gx_csv_get_intents_statuses <- c(
  "intent_created", "deferred_handler", "not_selected", "reference_only"
)

gx_csv_get_intents_abort <- function(
    message,
    class = "gx_error_csv_get_intents_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_csv_get_intents", "gx_error_fetch_plan"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_csv_get_intents_policy_impl <- function() {
  list(
    slice_id = "direct_csv_get_v1",
    method = "GET",
    accept = "text/csv, application/csv;q=0.9",
    accept_encoding = "identity",
    body_bytes = 0L,
    body_sha256 = .gx_csv_get_intents_empty_body_sha256,
    credential_policy = "unbound",
    redirect_policy = "unbound",
    cache_policy = "unbound",
    parser_policy = "unbound"
  )
}

gx_csv_get_intents_empty_intents <- function() {
  tibble::tibble(
    contract_version = character(), intent_order = integer(),
    intent_id = character(), distribution_id = character(),
    fetch_order = integer(), handler_id = character(),
    declared_media_type = character(), canonical_url_redacted = character(),
    intent_status = character()
  )
}

gx_csv_get_intents_empty_coverage <- function() {
  tibble::tibble(
    contract_version = character(), selection_order = integer(),
    fetch_order = integer(), distribution_id = character(),
    handler_id = character(), selected = logical(),
    plan_decision = character(), intent_id = character(),
    intent_status = character()
  )
}

gx_csv_get_intents_exact_attributes <- function(x, expected) {
  observed <- names(attributes(x))
  is.character(observed) && !anyNA(observed) &&
    length(observed) == length(expected) && all(expected %in% observed)
}

gx_csv_get_intents_table_attributes <- function(x, rows) {
  expected_rows <- if (rows == 0L) integer() else c(NA_integer_, -as.integer(rows))
  gx_csv_get_intents_exact_attributes(
    x, c("class", "row.names", "names")
  ) && identical(.row_names_info(x, type = 0L), expected_rows) &&
    all(vapply(x, function(column) is.null(attributes(column)), logical(1)))
}

gx_csv_get_intents_target_impl <- function(url) {
  target <- tryCatch(
    gx_safe_target(url, resolve_dns = FALSE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (!is.list(target) || !identical(names(target), c(
    "url", "host", "port", "addresses"
  )) || !is.character(target$url) || length(target$url) != 1L ||
      is.na(target$url) || !nzchar(target$url)) {
    gx_csv_get_intents_abort(
      "A direct-CSV source URL could not be canonicalized safely.",
      "gx_error_csv_get_intents_url"
    )
  }
  redacted <- tryCatch(
    gx_redact_url(target$url),
    error = function(cnd) "<invalid-url>",
    warning = function(cnd) "<invalid-url>"
  )
  redacted_valid <- tryCatch(
    isTRUE(gx_catalog_parseable_url(redacted, redacted = TRUE)),
    error = function(cnd) FALSE,
    warning = function(cnd) FALSE
  )
  if (!redacted_valid) {
    gx_csv_get_intents_abort(
      "A direct-CSV source URL could not be redacted safely.",
      "gx_error_csv_get_intents_url"
    )
  }
  list(url = unname(target$url), redacted = unname(redacted))
}

gx_csv_get_intents_id_impl <- function(
    distribution_id, fetch_order, handler_id, declared_media_type,
    canonical_url, policy) {
  gx_contract_hash(
    list(
      "distribution_id", distribution_id,
      "fetch_order", fetch_order,
      "handler_id", handler_id,
      "declared_media_type", declared_media_type,
      "canonical_url", canonical_url,
      "slice_id", policy$slice_id,
      "method", policy$method,
      "accept", policy$accept,
      "accept_encoding", policy$accept_encoding,
      "body_bytes", policy$body_bytes,
      "body_sha256", policy$body_sha256,
      "credential_policy", policy$credential_policy,
      "redirect_policy", policy$redirect_policy,
      "cache_policy", policy$cache_policy,
      "parser_policy", policy$parser_policy
    ),
    namespace = "geoconnexr.csv-get-intent.v1",
    contract_version = .gx_csv_get_intents_contract_version
  )
}

gx_csv_get_intents_intents_impl <- function(plan, policy) {
  distributions <- plan$distributions
  index <- which(distributions$selected & distributions$handler_id == "csv")
  if (!length(index)) return(gx_csv_get_intents_empty_intents())
  index <- index[order(distributions$fetch_order[index])]
  targets <- lapply(
    distributions$distribution_url[index],
    gx_csv_get_intents_target_impl
  )
  canonical_urls <- unname(vapply(targets, `[[`, character(1), "url"))
  redacted_urls <- unname(vapply(targets, `[[`, character(1), "redacted"))
  intent_ids <- unname(vapply(seq_along(index), function(position) {
    i <- index[[position]]
    gx_csv_get_intents_id_impl(
      distributions$distribution_id[[i]],
      distributions$fetch_order[[i]],
      distributions$handler_id[[i]],
      distributions$media_type[[i]],
      canonical_urls[[position]],
      policy
    )
  }, character(1)))
  tibble::tibble(
    contract_version = rep.int(
      .gx_csv_get_intents_contract_version, length(index)
    ),
    intent_order = as.integer(seq_along(index)),
    intent_id = intent_ids,
    distribution_id = unname(distributions$distribution_id[index]),
    fetch_order = unname(distributions$fetch_order[index]),
    handler_id = rep.int("csv", length(index)),
    declared_media_type = unname(distributions$media_type[index]),
    canonical_url_redacted = redacted_urls,
    intent_status = rep.int("inert", length(index))
  )
}

gx_csv_get_intents_coverage_impl <- function(plan, intents) {
  distributions <- plan$distributions
  if (!nrow(distributions)) return(gx_csv_get_intents_empty_coverage())
  intent_position <- match(
    distributions$distribution_id, intents$distribution_id
  )
  intent_ids <- rep.int(NA_character_, nrow(distributions))
  present <- !is.na(intent_position)
  intent_ids[present] <- intents$intent_id[intent_position[present]]
  statuses <- unname(vapply(seq_len(nrow(distributions)), function(i) {
    if (identical(distributions$decision[[i]], "reference_only")) {
      "reference_only"
    } else if (!distributions$selected[[i]]) {
      "not_selected"
    } else if (identical(distributions$handler_id[[i]], "csv")) {
      "intent_created"
    } else {
      "deferred_handler"
    }
  }, character(1)))
  tibble::tibble(
    contract_version = rep.int(
      .gx_csv_get_intents_contract_version, nrow(distributions)
    ),
    selection_order = unname(distributions$selection_order),
    fetch_order = unname(distributions$fetch_order),
    distribution_id = unname(distributions$distribution_id),
    handler_id = unname(distributions$handler_id),
    selected = unname(distributions$selected),
    plan_decision = unname(distributions$decision),
    intent_id = intent_ids,
    intent_status = statuses
  )
}

gx_csv_get_intents_counts_impl <- function(coverage, intents) {
  count_status <- function(status) {
    as.integer(sum(coverage$intent_status == status))
  }
  list(
    distributions = as.integer(nrow(coverage)),
    selected = as.integer(sum(coverage$selected)),
    intents = as.integer(nrow(intents)),
    intent_created = count_status("intent_created"),
    deferred_handler = count_status("deferred_handler"),
    not_selected = count_status("not_selected"),
    reference_only = count_status("reference_only"),
    requests = 0L
  )
}

gx_csv_get_intents_reasons_impl <- function(plan) {
  reasons <- unique(c(
    plan$metadata$non_replayable_reasons,
    "attempt_ledger_unbound",
    "cache_policy_unbound",
    "credential_policy_unbound",
    "parser_limits_unbound",
    "provider_transport_unauthorized",
    "redirect_policy_unbound",
    "request_budgets_unallocated",
    "response_contract_unproven"
  ))
  reasons[gx_catalog_byte_order(reasons)]
}

gx_csv_get_intents_new_impl <- function(
    plan, policy, intents, coverage, metadata) {
  object <- structure(
    list(
      contract_version = .gx_csv_get_intents_contract_version,
      plan = plan,
      policy = policy,
      intents = intents,
      coverage = coverage,
      metadata = metadata
    ),
    class = "gx_csv_get_intents"
  )
  gx_csv_get_intents_validate_impl(object)
  object
}

gx_csv_get_intents_impl <- function(plan) {
  valid <- tryCatch({
    gx_fetch_plan_validate_impl(plan)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid) {
    gx_csv_get_intents_abort(
      "M7c construction requires a valid M7a fetch plan.",
      "gx_error_csv_get_intents_input"
    )
  }
  policy <- gx_csv_get_intents_policy_impl()
  intents <- gx_csv_get_intents_intents_impl(plan, policy)
  coverage <- gx_csv_get_intents_coverage_impl(plan, intents)
  metadata <- list(
    host_specific = FALSE,
    replayable = FALSE,
    execution_ready = FALSE,
    transport_authorized = FALSE,
    budgets_allocated = FALSE,
    counts = gx_csv_get_intents_counts_impl(coverage, intents),
    non_replayable_reasons = gx_csv_get_intents_reasons_impl(plan)
  )
  gx_csv_get_intents_new_impl(plan, policy, intents, coverage, metadata)
}

gx_csv_get_intents_validate_policy <- function(policy) {
  expected <- gx_csv_get_intents_policy_impl()
  if (!is.list(policy) || !identical(names(policy), .gx_csv_get_intents_policy_fields) ||
      !gx_csv_get_intents_exact_attributes(policy, "names") ||
      !identical(policy, expected) ||
      !all(vapply(policy, function(value) {
        if (is.character(value)) is.null(attributes(value)) else TRUE
      }, logical(1))) || !is.null(attributes(policy$body_bytes))) {
    gx_csv_get_intents_abort(
      "Direct-CSV intent policy violates its exact inert contract."
    )
  }
  if (!identical(
    digest::digest(raw(), algo = "sha256", serialize = FALSE),
    policy$body_sha256
  )) {
    gx_csv_get_intents_abort(
      "Direct-CSV intent policy has an invalid empty-body binding."
    )
  }
  invisible(policy)
}

gx_csv_get_intents_validate_intents <- function(intents) {
  rows <- gx_catalog_table_rows(intents)
  if (!inherits(intents, "tbl_df") ||
      !identical(class(intents), c("tbl_df", "tbl", "data.frame")) ||
      !identical(names(intents), .gx_csv_get_intents_intent_columns) ||
      is.null(rows) || rows > .gx_fetch_plan_max_distributions ||
      !gx_csv_get_intents_table_attributes(intents, as.integer(rows))) {
    gx_csv_get_intents_abort(
      "Direct-CSV intents violate their exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_fetch_plan_max_distributions) {
        "gx_error_csv_get_intents_budget"
      } else {
        "gx_error_csv_get_intents_contract"
      }
    )
  }
  integer_columns <- c("intent_order", "fetch_order")
  character_columns <- setdiff(
    .gx_csv_get_intents_intent_columns, integer_columns
  )
  valid_types <- all(vapply(
    intents[character_columns], is.character, logical(1)
  )) && all(vapply(intents[integer_columns], is.integer, logical(1)))
  if (!valid_types || anyNA(intents$contract_version) ||
      any(intents$contract_version != .gx_csv_get_intents_contract_version) ||
      anyNA(intents$intent_order) || anyNA(intents$intent_id) ||
      anyNA(intents$distribution_id) || anyNA(intents$fetch_order) ||
      anyNA(intents$handler_id) || anyNA(intents$canonical_url_redacted) ||
      anyNA(intents$intent_status)) {
    gx_csv_get_intents_abort(
      "Direct-CSV intent columns have invalid types or required values."
    )
  }
  for (name in character_columns) {
    gx_fetch_plan_assert_text(
      intents[[name]],
      allow_na = identical(name, "declared_media_type"),
      nonempty = !identical(name, "declared_media_type")
    )
  }
  expected_order <- as.integer(seq_len(nrow(intents)))
  increasing_fetch_order <- nrow(intents) < 2L ||
    all(diff(intents$fetch_order) > 0L)
  valid_redacted <- all(vapply(
    intents$canonical_url_redacted,
    function(value) tryCatch(
      isTRUE(gx_catalog_parseable_url(value, redacted = TRUE)),
      error = function(cnd) FALSE,
      warning = function(cnd) FALSE
    ),
    logical(1)
  ))
  if (!identical(intents$intent_order, expected_order) ||
      any(intents$fetch_order < 1L) || !increasing_fetch_order ||
      any(intents$handler_id != "csv") ||
      any(intents$intent_status != "inert") ||
      !gx_catalog_is_sha256(intents$intent_id) ||
      !gx_catalog_is_sha256(intents$distribution_id) ||
      anyDuplicated(intents$intent_id) ||
      anyDuplicated(intents$distribution_id) || !valid_redacted) {
    gx_csv_get_intents_abort(
      "Direct-CSV intent identities, ordering, or policy projections are invalid."
    )
  }
  invisible(intents)
}

gx_csv_get_intents_validate_coverage <- function(coverage) {
  rows <- gx_catalog_table_rows(coverage)
  if (!inherits(coverage, "tbl_df") ||
      !identical(class(coverage), c("tbl_df", "tbl", "data.frame")) ||
      !identical(names(coverage), .gx_csv_get_intents_coverage_columns) ||
      is.null(rows) || rows > .gx_fetch_plan_max_distributions ||
      !gx_csv_get_intents_table_attributes(coverage, as.integer(rows))) {
    gx_csv_get_intents_abort(
      "Direct-CSV coverage violates its exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_fetch_plan_max_distributions) {
        "gx_error_csv_get_intents_budget"
      } else {
        "gx_error_csv_get_intents_contract"
      }
    )
  }
  integer_columns <- c("selection_order", "fetch_order")
  logical_columns <- "selected"
  character_columns <- setdiff(
    .gx_csv_get_intents_coverage_columns,
    c(integer_columns, logical_columns)
  )
  valid_types <- all(vapply(
    coverage[character_columns], is.character, logical(1)
  )) && all(vapply(coverage[integer_columns], is.integer, logical(1))) &&
    is.logical(coverage$selected)
  if (!valid_types || anyNA(coverage$contract_version) ||
      any(coverage$contract_version != .gx_csv_get_intents_contract_version) ||
      anyNA(coverage$selection_order) || anyNA(coverage$distribution_id) ||
      anyNA(coverage$handler_id) || anyNA(coverage$selected) ||
      anyNA(coverage$plan_decision) || anyNA(coverage$intent_status)) {
    gx_csv_get_intents_abort(
      "Direct-CSV coverage columns have invalid types or required values."
    )
  }
  for (name in character_columns) {
    gx_fetch_plan_assert_text(
      coverage[[name]],
      allow_na = identical(name, "intent_id"),
      nonempty = TRUE
    )
  }
  if (!identical(
    coverage$selection_order, as.integer(seq_len(nrow(coverage)))
  ) || !gx_catalog_is_sha256(coverage$distribution_id) ||
      !gx_catalog_is_sha256(coverage$intent_id, allow_na = TRUE) ||
      anyDuplicated(coverage$distribution_id) ||
      any(!coverage$intent_status %in% .gx_csv_get_intents_statuses) ||
      any(!coverage$plan_decision %in% .gx_fetch_plan_decisions) ||
      any(coverage$selected != !is.na(coverage$fetch_order))) {
    gx_csv_get_intents_abort(
      "Direct-CSV coverage identities, ordering, or statuses are invalid."
    )
  }
  invisible(coverage)
}

gx_csv_get_intents_validate_metadata <- function(metadata) {
  if (!is.list(metadata) ||
      !identical(names(metadata), .gx_csv_get_intents_metadata_fields) ||
      !gx_csv_get_intents_exact_attributes(metadata, "names") ||
      !identical(metadata$host_specific, FALSE) ||
      !identical(metadata$replayable, FALSE) ||
      !identical(metadata$execution_ready, FALSE) ||
      !identical(metadata$transport_authorized, FALSE) ||
      !identical(metadata$budgets_allocated, FALSE) ||
      !is.character(metadata$non_replayable_reasons) ||
      !length(metadata$non_replayable_reasons) ||
      anyNA(metadata$non_replayable_reasons) ||
      anyDuplicated(metadata$non_replayable_reasons) ||
      !gx_catalog_byte_sorted(metadata$non_replayable_reasons) ||
      !gx_catalog_is_token(metadata$non_replayable_reasons) ||
      !is.null(attributes(metadata$non_replayable_reasons))) {
    gx_csv_get_intents_abort(
      "Direct-CSV intent metadata violates its exact inert contract."
    )
  }
  counts <- metadata$counts
  if (!is.list(counts) ||
      !identical(names(counts), .gx_csv_get_intents_count_fields) ||
      !gx_csv_get_intents_exact_attributes(counts, "names") ||
      !all(vapply(counts, function(value) {
        is.integer(value) && length(value) == 1L && !is.na(value) &&
          value >= 0L && value <= .gx_fetch_plan_max_distributions &&
          is.null(attributes(value))
      }, logical(1)))) {
    gx_csv_get_intents_abort(
      "Direct-CSV intent counts violate their exact bounded contract."
    )
  }
  invisible(metadata)
}

gx_csv_get_intents_assert_text_budget <- function(x) {
  owned <- list(
    contract_version = x$contract_version,
    policy = x$policy,
    intents = x$intents,
    coverage = x$coverage,
    metadata = x$metadata
  )
  total <- gx_fetch_plan_text_total(
    owned, limit = .gx_csv_get_intents_max_text_bytes
  )
  if (!is.finite(total) || total > .gx_csv_get_intents_max_text_bytes) {
    gx_csv_get_intents_abort(
      "Direct-CSV intent text exceeds its aggregate byte budget.",
      "gx_error_csv_get_intents_budget"
    )
  }
  invisible(total)
}

gx_csv_get_intents_validate_cross_contract <- function(x) {
  expected_intents <- gx_csv_get_intents_intents_impl(x$plan, x$policy)
  expected_coverage <- gx_csv_get_intents_coverage_impl(
    x$plan, expected_intents
  )
  if (!identical(x$intents, expected_intents)) {
    gx_csv_get_intents_abort(
      "Direct-CSV intents do not rebind to the embedded fetch plan."
    )
  }
  if (!identical(x$coverage, expected_coverage)) {
    gx_csv_get_intents_abort(
      "Direct-CSV coverage does not reconcile with the embedded fetch plan."
    )
  }
  expected_counts <- gx_csv_get_intents_counts_impl(
    expected_coverage, expected_intents
  )
  counts <- x$metadata$counts
  if (!identical(counts, expected_counts) ||
      counts$distributions != counts$intent_created +
        counts$deferred_handler + counts$not_selected +
        counts$reference_only ||
      counts$selected != counts$intent_created + counts$deferred_handler ||
      counts$intents != counts$intent_created || counts$requests != 0L) {
    gx_csv_get_intents_abort(
      "Direct-CSV intent counts do not reconcile exactly."
    )
  }
  expected_reasons <- gx_csv_get_intents_reasons_impl(x$plan)
  if (!identical(
    x$metadata$non_replayable_reasons, expected_reasons
  )) {
    gx_csv_get_intents_abort(
      "Direct-CSV intent blockers do not reconcile with the embedded plan."
    )
  }
  if (!identical(x$plan$requests, list()) ||
      x$plan$metadata$counts$requests != 0L ||
      x$plan$metadata$execution_ready) {
    gx_csv_get_intents_abort(
      "The embedded M7a plan no longer has its exact request-empty contract."
    )
  }
  invisible(x)
}

gx_csv_get_intents_validate_body <- function(x) {
  if (!is.list(x) || !identical(class(x), "gx_csv_get_intents") ||
      !identical(names(x), .gx_csv_get_intents_fields) ||
      !gx_csv_get_intents_exact_attributes(x, c("names", "class")) ||
      !identical(
        x$contract_version, .gx_csv_get_intents_contract_version
      ) || !is.null(attributes(x$contract_version))) {
    gx_csv_get_intents_abort(
      "Direct-CSV intent objects violate their exact top-level contract."
    )
  }
  gx_fetch_plan_validate_impl(x$plan)
  gx_csv_get_intents_validate_policy(x$policy)
  gx_csv_get_intents_validate_intents(x$intents)
  gx_csv_get_intents_validate_coverage(x$coverage)
  gx_csv_get_intents_validate_metadata(x$metadata)
  gx_csv_get_intents_assert_text_budget(x)
  gx_csv_get_intents_validate_cross_contract(x)
  invisible(x)
}

gx_csv_get_intents_validate_impl <- function(x) {
  tryCatch(
    gx_csv_get_intents_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_csv_get_intents")) stop(cnd)
      gx_csv_get_intents_abort(
        "Direct-CSV intent validation rejected a malformed object."
      )
    },
    warning = function(cnd) {
      gx_csv_get_intents_abort(
        "Direct-CSV intent validation rejected a warning-producing object."
      )
    }
  )
}

#' @export
print.gx_csv_get_intents <- function(x, ...) {
  gx_csv_get_intents_validate_impl(x)
  counts <- x$metadata$counts
  cli::cli_inform(c(
    "<gx_csv_get_intents>",
    paste0(
      "* Distributions: {counts$distributions}; selected: {counts$selected}"
    ),
    paste0(
      "* Inert CSV intents: {counts$intents}; deferred handlers: ",
      "{counts$deferred_handler}"
    ),
    "* Requests: 0; transport authorized: FALSE; execution ready: FALSE"
  ))
  invisible(x)
}
