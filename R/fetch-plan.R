.gx_fetch_plan_contract_version <- "0.1.0"
.gx_fetch_plan_max_distributions <- 10000L
.gx_fetch_plan_max_parameters <- 10000L
.gx_fetch_plan_max_handlers <- 64L
.gx_fetch_plan_max_requests <- 10000L
.gx_fetch_plan_max_safe_integer <- 9007199254740991
.gx_fetch_plan_max_scalar_bytes <- 16L * 1024L
.gx_fetch_plan_max_text_bytes <- 64L * 1024L^2
.gx_fetch_plan_max_conforms_per_distribution <- 64L
.gx_fetch_plan_max_conforms_total <- 100000L

.gx_fetch_plan_fields <- c(
  "contract_version", "source", "time", "distributions", "parameters",
  "handlers", "requests", "budgets", "metadata"
)

.gx_fetch_plan_source_fields <- c(
  "catalog_contract_version", "aoi_contract_version", "aoi_type", "aoi_id",
  "datasets_sha256", "datasets_status", "datasets_truncated", "dataset_rows"
)

.gx_fetch_plan_distribution_columns <- c(
  "contract_version", "selection_order", "fetch_order", "site_uri",
  "dataset_id", "distribution_id", "provider_uri", "provider_name",
  "distribution_url", "media_type", "conforms_to", "handler_id",
  "fetchable", "temporal_start", "temporal_end", "time_start", "time_end",
  "variable_count", "selected", "decision"
)

.gx_fetch_plan_parameter_columns <- c(
  "contract_version", "distribution_id", "parameter_order", "variable_id",
  "variable_uri", "variable_name", "unit_uri", "unit_label",
  "measurement_technique", "parameter_key", "mapping_status"
)

.gx_fetch_plan_handler_columns <- c(
  "contract_version", "handler_id", "precedence", "lifecycle", "outcome",
  "implementation_id", "availability", "required_package", "minimum_version",
  "replayable"
)

.gx_fetch_plan_budget_fields <- c(
  "max_datasets", "max_requests", "max_encoded_bytes", "max_decoded_bytes"
)

.gx_fetch_plan_metadata_fields <- c(
  "created_at", "registry_contract_version", "registry_sha256",
  "implementation_contract_version", "implementation_sha256", "ordering",
  "counts", "execution_ready", "non_replayable_reasons"
)

.gx_fetch_plan_count_fields <- c(
  "catalog_rows", "unplannable_rows", "distributions", "parameters",
  "handlers", "selected", "reference_only", "not_fetchable", "outside_time",
  "skipped_max_datasets", "requests"
)

.gx_fetch_plan_decisions <- c(
  "selected_unplanned", "reference_only", "not_fetchable", "outside_time",
  "skipped_max_datasets"
)

.gx_fetch_plan_ordering <- c(
  "provider_uri_missing", "provider_uri", "provider_name_missing",
  "provider_name", "site_uri", "distribution_id"
)

gx_fetch_plan_abort <- function(message, class = "gx_error_fetch_plan_contract",
                                ..., call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_fetch_plan")),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_fetch_plan_na_time <- function() {
  as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
}

gx_fetch_plan_empty_distributions <- function() {
  tibble::tibble(
    contract_version = character(), selection_order = integer(),
    fetch_order = integer(), site_uri = character(), dataset_id = character(),
    distribution_id = character(), provider_uri = character(),
    provider_name = character(), distribution_url = character(),
    media_type = character(), conforms_to = list(), handler_id = character(),
    fetchable = logical(),
    temporal_start = as.POSIXct(character(), tz = "UTC"),
    temporal_end = as.POSIXct(character(), tz = "UTC"),
    time_start = as.POSIXct(character(), tz = "UTC"),
    time_end = as.POSIXct(character(), tz = "UTC"),
    variable_count = integer(), selected = logical(), decision = character()
  )
}

gx_fetch_plan_empty_parameters <- function() {
  tibble::tibble(
    contract_version = character(), distribution_id = character(),
    parameter_order = integer(), variable_id = character(),
    variable_uri = character(), variable_name = character(),
    unit_uri = character(), unit_label = character(),
    measurement_technique = character(), parameter_key = character(),
    mapping_status = character()
  )
}

gx_fetch_plan_empty_handlers <- function() {
  tibble::tibble(
    contract_version = character(), handler_id = character(),
    precedence = integer(), lifecycle = character(), outcome = character(),
    implementation_id = character(), availability = character(),
    required_package = character(), minimum_version = character(),
    replayable = logical()
  )
}

gx_fetch_plan_is_utc_scalar <- function(x, allow_na = TRUE) {
  inherits(x, "POSIXct") && length(x) == 1L &&
    identical(attr(x, "tzone"), "UTC") &&
    (allow_na || !is.na(x)) && (is.na(x) || is.finite(as.numeric(x)))
}

gx_fetch_plan_time_impl <- function(time = NULL) {
  if (is.null(time)) {
    out <- list(start = gx_fetch_plan_na_time(), end = gx_fetch_plan_na_time())
  } else if (inherits(time, "POSIXct") && length(time) == 2L &&
             identical(attr(time, "tzone"), "UTC")) {
    out <- list(start = time[1L], end = time[2L])
  } else if (is.list(time) && identical(names(time), c("start", "end"))) {
    out <- time
  } else {
    gx_fetch_plan_abort(
      "Fetch-plan time must be NULL, two UTC timestamps, or an exact start/end list.",
      "gx_error_fetch_plan_time"
    )
  }
  if (!gx_fetch_plan_is_utc_scalar(out$start) ||
      !gx_fetch_plan_is_utc_scalar(out$end) ||
      (!is.na(out$start) && !is.na(out$end) && out$start > out$end)) {
    gx_fetch_plan_abort(
      "Fetch-plan time bounds must be finite, UTC, and ordered.",
      "gx_error_fetch_plan_time"
    )
  }
  out
}

gx_fetch_plan_budget_integer <- function(x, name, maximum) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 0 || x != floor(x) || x > maximum) {
    gx_fetch_plan_abort(
      "A fetch-plan count budget is invalid.",
      "gx_error_fetch_plan_budget"
    )
  }
  as.integer(x)
}

gx_fetch_plan_budget_bytes <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 0 || x != floor(x) || x > .gx_fetch_plan_max_safe_integer) {
    gx_fetch_plan_abort(
      "A fetch-plan byte budget is invalid.",
      "gx_error_fetch_plan_budget"
    )
  }
  as.double(x)
}

gx_fetch_plan_budgets_impl <- function(
    max_datasets = 100L,
    max_requests = 256L,
    max_encoded_bytes = 1e9,
    max_decoded_bytes = 1e9) {
  list(
    max_datasets = gx_fetch_plan_budget_integer(
      max_datasets, "max_datasets", .gx_fetch_plan_max_distributions
    ),
    max_requests = gx_fetch_plan_budget_integer(
      max_requests, "max_requests", .gx_fetch_plan_max_requests
    ),
    max_encoded_bytes = gx_fetch_plan_budget_bytes(max_encoded_bytes),
    max_decoded_bytes = gx_fetch_plan_budget_bytes(max_decoded_bytes)
  )
}

gx_fetch_plan_hash_time <- function(x) {
  if (is.na(x)) return(NA_real_)
  as.numeric(x)
}

gx_fetch_plan_catalog_row_hash <- function(datasets, i) {
  values <- list()
  for (name in .gx_catalog_dataset_columns) {
    if (identical(name, "conforms_to")) {
      conforms <- datasets$conforms_to[[i]]
      values <- c(values, list(as.integer(length(conforms))), as.list(conforms))
    } else if (name %in% c("temporal_start", "temporal_end")) {
      values <- c(values, list(gx_fetch_plan_hash_time(datasets[[name]][i])))
    } else {
      values <- c(values, list(datasets[[name]][[i]]))
    }
  }
  gx_contract_hash(
    values,
    namespace = "geoconnexr.fetch-plan.catalog-row.v1",
    contract_version = .gx_fetch_plan_contract_version
  )
}

gx_fetch_plan_catalog_hash <- function(datasets) {
  hashes <- vapply(
    seq_len(nrow(datasets)),
    function(i) gx_fetch_plan_catalog_row_hash(datasets, i),
    character(1)
  )
  hashes <- hashes[gx_catalog_byte_order(hashes)]
  gx_contract_hash(
    c(list(as.integer(length(hashes))), as.list(hashes)),
    namespace = "geoconnexr.fetch-plan.catalog-datasets.v1",
    contract_version = .gx_fetch_plan_contract_version
  )
}

gx_fetch_plan_text_total <- function(x, limit = Inf) {
  total <- 0
  pending <- list(x)
  while (length(pending)) {
    value <- pending[[length(pending)]]
    pending[[length(pending)]] <- NULL
    if (is.character(value)) {
      for (item in value) {
        if (is.na(item)) next
        bytes <- suppressWarnings(tryCatch(
          nchar(enc2utf8(item), type = "bytes", allowNA = TRUE),
          error = function(cnd) NA_integer_
        ))
        if (length(bytes) != 1L || is.na(bytes) || !is.finite(bytes) ||
            bytes > limit - total) return(Inf)
        total <- total + as.double(bytes)
      }
    } else if (is.list(value)) {
      pending <- c(pending, unname(value))
    }
  }
  total
}

gx_fetch_plan_assert_text_budget <- function(x) {
  total <- gx_fetch_plan_text_total(x, .gx_fetch_plan_max_text_bytes)
  if (!is.finite(total) || total > .gx_fetch_plan_max_text_bytes) {
    gx_fetch_plan_abort(
      "Fetch-plan text exceeds its aggregate byte budget.",
      "gx_error_fetch_plan_budget"
    )
  }
  invisible(total)
}

gx_fetch_plan_text_valid <- function(x, allow_na = TRUE, nonempty = FALSE) {
  suppressWarnings(tryCatch(
    isTRUE(gx_catalog_text_valid(
      x, allow_na = allow_na, nonempty = nonempty
    )),
    error = function(cnd) FALSE
  ))
}

gx_fetch_plan_column_value <- function(data, name, i) {
  if (identical(name, "conforms_to")) data[[name]][[i]] else data[[name]][i]
}

gx_fetch_plan_group_conflicts <- function(data, index) {
  variable_fields <- c(
    "variable_id", "variable_uri", "variable_name", "unit_uri", "unit_label",
    "measurement_technique"
  )
  fixed_fields <- setdiff(.gx_catalog_dataset_columns, variable_fields)
  any(vapply(fixed_fields, function(name) {
    expected <- gx_fetch_plan_column_value(data, name, index[[1L]])
    any(!vapply(index[-1L], function(i) {
      identical(gx_fetch_plan_column_value(data, name, i), expected)
    }, logical(1)))
  }, logical(1)))
}

gx_fetch_plan_classify_impl <- function(registry, access_url, media_type,
                                        conforms_to) {
  facts <- list(
    access_url = access_url,
    media_type = if (is.na(media_type)) NULL else media_type,
    conforms_to = conforms_to
  )
  matched <- tryCatch(
    vapply(
      registry$handlers$classifier,
      gx_classifier_matches,
      logical(1),
      facts = facts
    ),
    error = function(cnd) NULL
  )
  if (is.null(matched) || !any(matched)) {
    gx_fetch_plan_abort(
      "The bound handler registry did not classify a catalog distribution.",
      "gx_error_fetch_plan_handler"
    )
  }
  registry$handlers$id[[which(matched)[[1L]]]]
}

gx_fetch_plan_safe_target <- function(url) {
  valid <- tryCatch({
    gx_assert_safe_url(url, resolve_dns = FALSE)
    TRUE
  }, error = function(cnd) FALSE)
  if (!valid) {
    gx_fetch_plan_abort(
      "A catalog distribution violates the offline target-safety policy.",
      "gx_error_fetch_plan_security"
    )
  }
  invisible(url)
}

gx_fetch_plan_intersect_time <- function(requested, temporal_start,
                                         temporal_end) {
  starts <- c(
    if (!is.na(requested$start)) as.numeric(requested$start) else numeric(),
    if (!is.na(temporal_start)) as.numeric(temporal_start) else numeric()
  )
  ends <- c(
    if (!is.na(requested$end)) as.numeric(requested$end) else numeric(),
    if (!is.na(temporal_end)) as.numeric(temporal_end) else numeric()
  )
  start <- if (length(starts)) max(starts) else NA_real_
  end <- if (length(ends)) min(ends) else NA_real_
  outside <- !is.na(start) && !is.na(end) && start > end
  if (outside) {
    start <- NA_real_
    end <- NA_real_
  }
  list(
    start = as.POSIXct(start, origin = "1970-01-01", tz = "UTC"),
    end = as.POSIXct(end, origin = "1970-01-01", tz = "UTC"),
    outside = outside
  )
}

gx_fetch_plan_source_impl <- function(catalog) {
  if (!"datasets" %in% catalog$metadata$selection$include) {
    gx_fetch_plan_abort(
      "M7a requires a catalog whose selected components include datasets.",
      "gx_error_fetch_plan_input"
    )
  }
  position <- match("datasets", catalog$metadata$completeness$stage)
  if (is.na(position)) {
    gx_fetch_plan_abort(
      "Dataset completeness metadata is required for fetch planning.",
      "gx_error_fetch_plan_input"
    )
  }
  list(
    catalog_contract_version = catalog$contract_version,
    aoi_contract_version = catalog$aoi$contract_version,
    aoi_type = catalog$aoi$type,
    aoi_id = catalog$aoi$id,
    datasets_sha256 = gx_fetch_plan_catalog_hash(catalog$datasets),
    datasets_status = catalog$metadata$completeness$status[[position]],
    datasets_truncated = catalog$metadata$completeness$truncated[[position]],
    dataset_rows = as.integer(nrow(catalog$datasets))
  )
}

gx_fetch_plan_handlers_impl <- function(registry) {
  handlers <- registry$handlers
  tibble::tibble(
    contract_version = rep(.gx_fetch_plan_contract_version, nrow(handlers)),
    handler_id = handlers$id,
    precedence = handlers$precedence,
    lifecycle = handlers$lifecycle,
    outcome = handlers$outcome,
    implementation_id = handlers$implementation_id,
    availability = handlers$availability,
    required_package = handlers$package,
    minimum_version = handlers$minimum_version,
    replayable = rep(FALSE, nrow(handlers))
  )
}

gx_fetch_plan_time_vector <- function(rows, name) {
  as.POSIXct(
    vapply(rows, function(row) as.numeric(row[[name]]), numeric(1)),
    origin = "1970-01-01",
    tz = "UTC"
  )
}

gx_fetch_plan_bind_distributions <- function(rows) {
  if (!length(rows)) return(gx_fetch_plan_empty_distributions())
  tibble::tibble(
    contract_version = rep(.gx_fetch_plan_contract_version, length(rows)),
    selection_order = vapply(rows, `[[`, integer(1), "selection_order"),
    fetch_order = vapply(rows, `[[`, integer(1), "fetch_order"),
    site_uri = vapply(rows, `[[`, character(1), "site_uri"),
    dataset_id = vapply(rows, `[[`, character(1), "dataset_id"),
    distribution_id = vapply(rows, `[[`, character(1), "distribution_id"),
    provider_uri = vapply(rows, `[[`, character(1), "provider_uri"),
    provider_name = vapply(rows, `[[`, character(1), "provider_name"),
    distribution_url = vapply(rows, `[[`, character(1), "distribution_url"),
    media_type = vapply(rows, `[[`, character(1), "media_type"),
    conforms_to = unname(lapply(rows, `[[`, "conforms_to")),
    handler_id = vapply(rows, `[[`, character(1), "handler_id"),
    fetchable = vapply(rows, `[[`, logical(1), "fetchable"),
    temporal_start = gx_fetch_plan_time_vector(rows, "temporal_start"),
    temporal_end = gx_fetch_plan_time_vector(rows, "temporal_end"),
    time_start = gx_fetch_plan_time_vector(rows, "time_start"),
    time_end = gx_fetch_plan_time_vector(rows, "time_end"),
    variable_count = vapply(rows, `[[`, integer(1), "variable_count"),
    selected = vapply(rows, `[[`, logical(1), "selected"),
    decision = vapply(rows, `[[`, character(1), "decision")
  )
}

gx_fetch_plan_bind_parameters <- function(rows) {
  if (!length(rows)) return(gx_fetch_plan_empty_parameters())
  tibble::tibble(
    contract_version = rep(.gx_fetch_plan_contract_version, length(rows)),
    distribution_id = vapply(rows, `[[`, character(1), "distribution_id"),
    parameter_order = vapply(rows, `[[`, integer(1), "parameter_order"),
    variable_id = vapply(rows, `[[`, character(1), "variable_id"),
    variable_uri = vapply(rows, `[[`, character(1), "variable_uri"),
    variable_name = vapply(rows, `[[`, character(1), "variable_name"),
    unit_uri = vapply(rows, `[[`, character(1), "unit_uri"),
    unit_label = vapply(rows, `[[`, character(1), "unit_label"),
    measurement_technique = vapply(
      rows, `[[`, character(1), "measurement_technique"
    ),
    parameter_key = rep(NA_character_, length(rows)),
    mapping_status = rep("unplanned", length(rows))
  )
}

gx_fetch_plan_parameter_order <- function(data, index) {
  fields <- c(
    "variable_id", "variable_uri", "variable_name", "unit_uri", "unit_label",
    "measurement_technique"
  )
  keys <- unlist(lapply(fields, function(name) {
    value <- data[[name]][index]
    list(is.na(value), ifelse(is.na(value), "", value))
  }), recursive = FALSE)
  index[do.call(gx_catalog_byte_order, keys)]
}

gx_fetch_plan_reconcile_impl <- function(catalog, time, registry, budgets) {
  data <- catalog$datasets
  plannable <- !is.na(data$dataset_id) & !is.na(data$distribution_id) &
    !is.na(data$distribution_url)
  unplannable_rows <- as.integer(sum(!plannable))
  if (!any(plannable)) {
    return(list(
      distributions = gx_fetch_plan_empty_distributions(),
      parameters = gx_fetch_plan_empty_parameters(),
      unplannable_rows = unplannable_rows
    ))
  }

  group_ids <- unique(data$distribution_id[plannable])
  group_ids <- group_ids[gx_catalog_byte_order(group_ids)]
  groups <- lapply(group_ids, function(id) which(plannable & data$distribution_id == id))
  names(groups) <- group_ids
  rows <- vector("list", length(groups))

  for (g in seq_along(groups)) {
    index <- groups[[g]]
    if (gx_fetch_plan_group_conflicts(data, index)) {
      gx_fetch_plan_abort(
        "Catalog rows sharing a distribution identity disagree on fixed facts.",
        "gx_error_fetch_plan_conflict"
      )
    }
    i <- index[[1L]]
    gx_fetch_plan_safe_target(data$distribution_url[[i]])
    classified <- gx_fetch_plan_classify_impl(
      registry,
      data$distribution_url[[i]],
      data$media_type[[i]],
      data$conforms_to[[i]]
    )
    if (!identical(classified, data$handler_id[[i]])) {
      gx_fetch_plan_abort(
        "A catalog handler identity disagrees with the bound classifier registry.",
        "gx_error_fetch_plan_handler"
      )
    }
    handler_position <- match(classified, registry$handlers$id)
    if (is.na(handler_position)) {
      gx_fetch_plan_abort(
        "A catalog handler is absent from the bound implementation registry.",
        "gx_error_fetch_plan_handler"
      )
    }
    effective_time <- gx_fetch_plan_intersect_time(
      time, data$temporal_start[i], data$temporal_end[i]
    )
    outcome <- registry$handlers$outcome[[handler_position]]
    decision <- if (identical(outcome, "reference_only")) {
      "reference_only"
    } else if (!isTRUE(data$fetchable[[i]])) {
      "not_fetchable"
    } else if (effective_time$outside) {
      "outside_time"
    } else {
      "candidate"
    }
    rows[[g]] <- list(
      selection_order = NA_integer_,
      fetch_order = NA_integer_,
      site_uri = data$site_uri[[i]],
      dataset_id = data$dataset_id[[i]],
      distribution_id = data$distribution_id[[i]],
      provider_uri = data$provider_uri[[i]],
      provider_name = data$provider_name[[i]],
      distribution_url = data$distribution_url[[i]],
      media_type = data$media_type[[i]],
      conforms_to = data$conforms_to[[i]],
      handler_id = classified,
      fetchable = data$fetchable[[i]],
      temporal_start = data$temporal_start[i],
      temporal_end = data$temporal_end[i],
      time_start = effective_time$start,
      time_end = effective_time$end,
      variable_count = as.integer(length(index)),
      selected = FALSE,
      decision = decision,
      source_index = index
    )
  }

  order <- gx_catalog_byte_order(
    vapply(rows, function(row) is.na(row$provider_uri), logical(1)),
    vapply(rows, function(row) ifelse(is.na(row$provider_uri), "", row$provider_uri),
      character(1)
    ),
    vapply(rows, function(row) is.na(row$provider_name), logical(1)),
    vapply(rows, function(row) ifelse(is.na(row$provider_name), "", row$provider_name),
      character(1)
    ),
    vapply(rows, `[[`, character(1), "site_uri"),
    vapply(rows, `[[`, character(1), "distribution_id")
  )
  rows <- rows[order]
  fetch_order <- 0L
  for (i in seq_along(rows)) {
    rows[[i]]$selection_order <- as.integer(i)
    if (identical(rows[[i]]$decision, "candidate")) {
      if (fetch_order < budgets$max_datasets) {
        fetch_order <- fetch_order + 1L
        rows[[i]]$fetch_order <- fetch_order
        rows[[i]]$selected <- TRUE
        rows[[i]]$decision <- "selected_unplanned"
      } else {
        rows[[i]]$decision <- "skipped_max_datasets"
      }
    }
  }

  parameter_rows <- list()
  for (row in rows) {
    index <- gx_fetch_plan_parameter_order(data, row$source_index)
    for (j in seq_along(index)) {
      i <- index[[j]]
      parameter_rows[[length(parameter_rows) + 1L]] <- list(
        distribution_id = row$distribution_id,
        parameter_order = as.integer(j),
        variable_id = data$variable_id[[i]],
        variable_uri = data$variable_uri[[i]],
        variable_name = data$variable_name[[i]],
        unit_uri = data$unit_uri[[i]],
        unit_label = data$unit_label[[i]],
        measurement_technique = data$measurement_technique[[i]]
      )
    }
  }

  list(
    distributions = gx_fetch_plan_bind_distributions(rows),
    parameters = gx_fetch_plan_bind_parameters(parameter_rows),
    unplannable_rows = unplannable_rows
  )
}

gx_fetch_plan_counts_impl <- function(source, distributions, parameters,
                                      handlers, unplannable_rows) {
  decision_count <- function(value) {
    as.integer(sum(distributions$decision == value))
  }
  list(
    catalog_rows = source$dataset_rows,
    unplannable_rows = as.integer(unplannable_rows),
    distributions = as.integer(nrow(distributions)),
    parameters = as.integer(nrow(parameters)),
    handlers = as.integer(nrow(handlers)),
    selected = as.integer(sum(distributions$selected)),
    reference_only = decision_count("reference_only"),
    not_fetchable = decision_count("not_fetchable"),
    outside_time = decision_count("outside_time"),
    skipped_max_datasets = decision_count("skipped_max_datasets"),
    requests = 0L
  )
}

gx_fetch_plan_now_impl <- function(now) {
  if (!is.function(now)) {
    gx_fetch_plan_abort(
      "The fetch-plan clock seam must be a function.",
      "gx_error_fetch_plan_input"
    )
  }
  value <- tryCatch(now(), error = function(cnd) NULL)
  numeric_value <- suppressWarnings(tryCatch(as.numeric(value), error = function(cnd) NA_real_))
  if (!inherits(value, "POSIXct") || length(value) != 1L ||
      !identical(attr(value, "tzone"), "UTC") ||
      length(numeric_value) != 1L || is.na(numeric_value) || !is.finite(numeric_value)) {
    gx_fetch_plan_abort(
      "The fetch-plan clock returned an invalid timestamp.",
      "gx_error_fetch_plan_input"
    )
  }
  as.POSIXct(numeric_value, origin = "1970-01-01", tz = "UTC")
}

gx_fetch_plan_new_impl <- function(source, time, distributions, parameters,
                                   handlers, requests, budgets, metadata) {
  object <- structure(
    list(
      contract_version = .gx_fetch_plan_contract_version,
      source = source,
      time = time,
      distributions = distributions,
      parameters = parameters,
      handlers = handlers,
      requests = requests,
      budgets = budgets,
      metadata = metadata
    ),
    class = "gx_fetch_plan"
  )
  gx_fetch_plan_validate_impl(object)
  object
}

gx_fetch_plan_impl <- function(
    catalog,
    time = NULL,
    max_datasets = 100L,
    max_requests = 256L,
    max_encoded_bytes = 1e9,
    max_decoded_bytes = 1e9,
    now = gx_now,
    registry = NULL) {
  valid_catalog <- tryCatch({
    gx_catalog_validate_impl(catalog)
    TRUE
  }, error = function(cnd) FALSE)
  if (!valid_catalog) {
    gx_fetch_plan_abort(
      "M7a construction requires a valid M6c catalog.",
      "gx_error_fetch_plan_input"
    )
  }
  time <- gx_fetch_plan_time_impl(time)
  budgets <- gx_fetch_plan_budgets_impl(
    max_datasets, max_requests, max_encoded_bytes, max_decoded_bytes
  )
  if (is.null(registry)) registry <- gx_handler_registry_load_impl()
  tryCatch(
    gx_handler_registry_validate_impl(registry),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_fetch_plan")) stop(cnd)
      gx_fetch_plan_abort(
        "The effective handler registry is invalid.",
        "gx_error_fetch_plan_handler"
      )
    }
  )
  source <- gx_fetch_plan_source_impl(catalog)
  reconciled <- gx_fetch_plan_reconcile_impl(catalog, time, registry, budgets)
  handlers <- gx_fetch_plan_handlers_impl(registry)
  counts <- gx_fetch_plan_counts_impl(
    source, reconciled$distributions, reconciled$parameters, handlers,
    reconciled$unplannable_rows
  )
  reasons <- c("handler_implementations_planned", "request_plans_absent")
  if (!identical(source$datasets_status, "complete") || source$datasets_truncated) {
    reasons <- c(reasons, "source_catalog_incomplete")
  }
  reasons <- reasons[gx_catalog_byte_order(reasons)]
  metadata <- list(
    created_at = gx_fetch_plan_now_impl(now),
    registry_contract_version = registry$contract_version,
    registry_sha256 = registry$portable_sha256,
    implementation_contract_version = registry$contract_version,
    implementation_sha256 = registry$implementations_sha256,
    ordering = .gx_fetch_plan_ordering,
    counts = counts,
    execution_ready = FALSE,
    non_replayable_reasons = reasons
  )
  gx_fetch_plan_new_impl(
    source = source,
    time = time,
    distributions = reconciled$distributions,
    parameters = reconciled$parameters,
    handlers = handlers,
    requests = list(),
    budgets = budgets,
    metadata = metadata
  )
}

gx_fetch_plan_assert_text <- function(x, allow_na = TRUE, nonempty = FALSE) {
  if (gx_fetch_plan_text_valid(x, allow_na = allow_na, nonempty = nonempty)) {
    return(invisible(x))
  }
  over_budget <- FALSE
  if (is.character(x)) {
    present <- x[!is.na(x)]
    bytes <- suppressWarnings(tryCatch(
      nchar(enc2utf8(present), type = "bytes", allowNA = TRUE),
      error = function(cnd) NA_integer_
    ))
    over_budget <- any(!is.na(bytes) & bytes > .gx_fetch_plan_max_scalar_bytes)
  }
  gx_fetch_plan_abort(
    "Fetch-plan text violates its UTF-8, control, or scalar-byte contract.",
    if (over_budget) "gx_error_fetch_plan_budget" else
      "gx_error_fetch_plan_contract"
  )
}

gx_fetch_plan_valid_iri <- function(x, allow_na = TRUE) {
  if (!is.character(x) || (!allow_na && anyNA(x))) return(FALSE)
  present <- x[!is.na(x)]
  all(vapply(present, function(value) {
    canonical <- tryCatch(gx_identity_iri(value), error = function(cnd) NA_character_)
    !is.na(canonical) && identical(canonical, value)
  }, logical(1)))
}

gx_fetch_plan_valid_url <- function(x, allow_na = TRUE) {
  if (!is.character(x) || (!allow_na && anyNA(x))) return(FALSE)
  present <- x[!is.na(x)]
  all(vapply(present, function(value) {
    suppressWarnings(tryCatch(
      isTRUE(gx_catalog_parseable_url(value)),
      error = function(cnd) FALSE
    ))
  }, logical(1)))
}

gx_fetch_plan_time_equal <- function(x, y) {
  if (!gx_fetch_plan_is_utc_scalar(x) || !gx_fetch_plan_is_utc_scalar(y)) {
    return(FALSE)
  }
  if (is.na(x) || is.na(y)) return(is.na(x) && is.na(y))
  identical(as.numeric(x), as.numeric(y))
}

gx_fetch_plan_validate_source <- function(source) {
  if (!is.list(source) || !identical(names(source), .gx_fetch_plan_source_fields) ||
      !identical(source$catalog_contract_version, .gx_catalog_contract_version) ||
      !identical(source$aoi_contract_version, gx_aoi_contract_version) ||
      !is.character(source$aoi_type) || length(source$aoi_type) != 1L ||
      !is.character(source$aoi_id) || length(source$aoi_id) != 1L ||
      !gx_catalog_is_sha256(source$datasets_sha256) ||
      length(source$datasets_sha256) != 1L ||
      !is.character(source$datasets_status) || length(source$datasets_status) != 1L ||
      !source$datasets_status %in% c("complete", "partial", "not_run", "unknown") ||
      !is.logical(source$datasets_truncated) ||
      length(source$datasets_truncated) != 1L || is.na(source$datasets_truncated) ||
      !is.integer(source$dataset_rows) || length(source$dataset_rows) != 1L ||
      is.na(source$dataset_rows) || source$dataset_rows < 0L ||
      source$dataset_rows > .gx_fetch_plan_max_parameters) {
    gx_fetch_plan_abort("Fetch-plan source metadata has an invalid exact contract.")
  }
  gx_fetch_plan_assert_text(source$aoi_type, allow_na = FALSE, nonempty = TRUE)
  gx_fetch_plan_assert_text(source$aoi_id, allow_na = FALSE, nonempty = TRUE)
  invisible(source)
}

gx_fetch_plan_validate_budgets <- function(budgets) {
  if (!is.list(budgets) || !identical(names(budgets), .gx_fetch_plan_budget_fields)) {
    gx_fetch_plan_abort(
      "Fetch-plan budgets have an invalid exact shape.",
      "gx_error_fetch_plan_budget"
    )
  }
  normalized <- gx_fetch_plan_budgets_impl(
    budgets$max_datasets,
    budgets$max_requests,
    budgets$max_encoded_bytes,
    budgets$max_decoded_bytes
  )
  if (!identical(normalized, budgets)) {
    gx_fetch_plan_abort(
      "Fetch-plan budgets have invalid storage types.",
      "gx_error_fetch_plan_budget"
    )
  }
  invisible(budgets)
}

gx_fetch_plan_validate_handlers <- function(handlers) {
  rows <- gx_catalog_table_rows(handlers)
  if (!inherits(handlers, "tbl_df") ||
      !identical(class(handlers), c("tbl_df", "tbl", "data.frame")) ||
      !identical(names(handlers), .gx_fetch_plan_handler_columns) ||
      is.null(rows) || rows < 1 || rows > .gx_fetch_plan_max_handlers) {
    gx_fetch_plan_abort(
      "Fetch-plan handlers violate their exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_fetch_plan_max_handlers) {
        "gx_error_fetch_plan_budget"
      } else {
        "gx_error_fetch_plan_contract"
      }
    )
  }
  character_columns <- setdiff(
    .gx_fetch_plan_handler_columns, c("precedence", "replayable")
  )
  valid_types <- all(vapply(handlers[character_columns], is.character, logical(1))) &&
    is.integer(handlers$precedence) && is.logical(handlers$replayable)
  if (!valid_types || anyNA(handlers$contract_version) ||
      any(handlers$contract_version != .gx_fetch_plan_contract_version) ||
      anyNA(handlers$handler_id) || anyNA(handlers$precedence) ||
      anyNA(handlers$lifecycle) || anyNA(handlers$outcome) ||
      anyNA(handlers$implementation_id) || anyNA(handlers$availability) ||
      anyNA(handlers$replayable) || any(handlers$replayable)) {
    gx_fetch_plan_abort("Fetch-plan handler columns have invalid types or values.")
  }
  lapply(character_columns, function(name) {
    gx_fetch_plan_assert_text(
      handlers[[name]],
      allow_na = name %in% c("required_package", "minimum_version"),
      nonempty = TRUE
    )
  })
  valid_ids <- grepl("^[a-z][a-z0-9_]{0,63}\\z", handlers$handler_id, perl = TRUE)
  valid_impl <- grepl(
    "^[A-Za-z][A-Za-z0-9._-]*:[A-Za-z0-9._-]+\\z",
    handlers$implementation_id,
    perl = TRUE
  )
  package_valid <- is.na(handlers$required_package) |
    grepl("^[A-Za-z][A-Za-z0-9.]*\\z", handlers$required_package, perl = TRUE)
  version_valid <- is.na(handlers$minimum_version) |
    grepl("^[0-9]+(?:[.][0-9]+)*(?:[-.][A-Za-z0-9]+)*\\z",
      handlers$minimum_version, perl = TRUE
    )
  if (!all(valid_ids) || !all(valid_impl) || !all(package_valid) ||
      !all(version_valid) || anyDuplicated(handlers$handler_id) ||
      anyDuplicated(handlers$implementation_id) ||
      anyDuplicated(handlers$precedence) || any(handlers$precedence < 0L) ||
      !identical(handlers$precedence, sort(handlers$precedence)) ||
      any(!handlers$lifecycle %in% c("active", "deprecated")) ||
      any(!handlers$outcome %in% c("fetch", "reference_only")) ||
      any(!handlers$availability %in% c("planned", "classifier_only"))) {
    gx_fetch_plan_abort("Fetch-plan handler identities or ordering are invalid.")
  }
  last <- nrow(handlers)
  unknown_ok <- identical(handlers$handler_id[[last]], "unknown") &&
    identical(handlers$outcome[[last]], "reference_only") &&
    identical(handlers$availability[[last]], "classifier_only") &&
    is.na(handlers$required_package[[last]]) &&
    all(handlers$outcome[-last] == "fetch") &&
    all(handlers$availability[-last] == "planned")
  if (!unknown_ok) {
    gx_fetch_plan_abort("Fetch-plan handler fallback semantics are invalid.")
  }
  invisible(handlers)
}

gx_fetch_plan_validate_distributions <- function(distributions) {
  rows <- gx_catalog_table_rows(distributions)
  if (!inherits(distributions, "tbl_df") ||
      !identical(class(distributions), c("tbl_df", "tbl", "data.frame")) ||
      !identical(names(distributions), .gx_fetch_plan_distribution_columns) ||
      is.null(rows) || rows > .gx_fetch_plan_max_distributions) {
    gx_fetch_plan_abort(
      "Fetch-plan distributions violate their exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_fetch_plan_max_distributions) {
        "gx_error_fetch_plan_budget"
      } else {
        "gx_error_fetch_plan_contract"
      }
    )
  }
  integer_columns <- c("selection_order", "fetch_order", "variable_count")
  time_columns <- c("temporal_start", "temporal_end", "time_start", "time_end")
  logical_columns <- c("fetchable", "selected")
  list_columns <- "conforms_to"
  character_columns <- setdiff(
    .gx_fetch_plan_distribution_columns,
    c(integer_columns, time_columns, logical_columns, list_columns)
  )
  valid_types <- all(vapply(
    distributions[character_columns], is.character, logical(1)
  )) && all(vapply(distributions[integer_columns], is.integer, logical(1))) &&
    all(vapply(distributions[time_columns], gx_catalog_is_utc, logical(1))) &&
    all(vapply(distributions[logical_columns], is.logical, logical(1))) &&
    is.list(distributions$conforms_to)
  if (!valid_types || anyNA(distributions$contract_version) ||
      any(distributions$contract_version != .gx_fetch_plan_contract_version) ||
      anyNA(distributions$selection_order) || anyNA(distributions$site_uri) ||
      anyNA(distributions$dataset_id) || anyNA(distributions$distribution_id) ||
      anyNA(distributions$distribution_url) || anyNA(distributions$handler_id) ||
      anyNA(distributions$fetchable) || anyNA(distributions$variable_count) ||
      anyNA(distributions$selected) || anyNA(distributions$decision)) {
    gx_fetch_plan_abort("Fetch-plan distribution columns have invalid required values.")
  }
  lapply(character_columns, function(name) {
    gx_fetch_plan_assert_text(
      distributions[[name]],
      allow_na = name %in% c("provider_uri", "provider_name", "media_type"),
      nonempty = !name %in% c("provider_name", "media_type")
    )
  })
  if (!gx_catalog_is_sha256(distributions$dataset_id) ||
      !gx_catalog_is_sha256(distributions$distribution_id) ||
      anyDuplicated(distributions$distribution_id) ||
      !gx_fetch_plan_valid_url(distributions$site_uri, allow_na = FALSE) ||
      !gx_fetch_plan_valid_url(distributions$distribution_url, allow_na = FALSE) ||
      !gx_fetch_plan_valid_iri(distributions$provider_uri) ||
      any(!grepl("^[a-z][a-z0-9_]{0,63}\\z", distributions$handler_id,
        perl = TRUE
      )) || any(distributions$selection_order < 1L) ||
      any(!is.na(distributions$fetch_order) & distributions$fetch_order < 1L) ||
      any(distributions$variable_count < 1L) ||
      any(!distributions$decision %in% .gx_fetch_plan_decisions) ||
      any(distributions$selected !=
        (distributions$decision == "selected_unplanned"))) {
    gx_fetch_plan_abort("Fetch-plan distribution identities or decisions are invalid.")
  }
  temporal_bad <- !is.na(distributions$temporal_start) &
    !is.na(distributions$temporal_end) &
    distributions$temporal_start > distributions$temporal_end
  if (any(temporal_bad)) {
    gx_fetch_plan_abort(
      "Fetch-plan temporal coverage is reversed.",
      "gx_error_fetch_plan_time"
    )
  }
  conforms_lengths <- lengths(distributions$conforms_to)
  valid_conforms <- all(conforms_lengths <= .gx_fetch_plan_max_conforms_per_distribution) &&
    sum(as.double(conforms_lengths)) <= .gx_fetch_plan_max_conforms_total &&
    all(vapply(distributions$conforms_to, function(value) {
      is.character(value) && !anyNA(value) && !anyDuplicated(value) &&
        gx_fetch_plan_text_valid(value, allow_na = FALSE, nonempty = TRUE) &&
        gx_catalog_byte_sorted(value) && gx_fetch_plan_valid_iri(value, allow_na = FALSE)
    }, logical(1)))
  if (!valid_conforms) {
    gx_fetch_plan_abort(
      "Fetch-plan conformance arrays violate their canonical bounded contract.",
      if (any(conforms_lengths > .gx_fetch_plan_max_conforms_per_distribution) ||
          sum(as.double(conforms_lengths)) > .gx_fetch_plan_max_conforms_total) {
        "gx_error_fetch_plan_budget"
      } else {
        "gx_error_fetch_plan_contract"
      }
    )
  }
  invisible(distributions)
}

gx_fetch_plan_variable_id_valid <- function(x) {
  if (!is.character(x)) return(FALSE)
  all(vapply(x, function(value) {
    if (is.na(value)) return(TRUE)
    if (grepl("^[a-f0-9]{64}\\z", value, perl = TRUE)) return(TRUE)
    canonical <- tryCatch(gx_identity_iri(value), error = function(cnd) NA_character_)
    !is.na(canonical) && identical(canonical, value)
  }, logical(1)))
}

gx_fetch_plan_validate_parameters <- function(parameters) {
  rows <- gx_catalog_table_rows(parameters)
  if (!inherits(parameters, "tbl_df") ||
      !identical(class(parameters), c("tbl_df", "tbl", "data.frame")) ||
      !identical(names(parameters), .gx_fetch_plan_parameter_columns) ||
      is.null(rows) || rows > .gx_fetch_plan_max_parameters) {
    gx_fetch_plan_abort(
      "Fetch-plan parameters violate their exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_fetch_plan_max_parameters) {
        "gx_error_fetch_plan_budget"
      } else {
        "gx_error_fetch_plan_contract"
      }
    )
  }
  character_columns <- setdiff(.gx_fetch_plan_parameter_columns, "parameter_order")
  valid_types <- all(vapply(parameters[character_columns], is.character, logical(1))) &&
    is.integer(parameters$parameter_order)
  if (!valid_types || anyNA(parameters$contract_version) ||
      any(parameters$contract_version != .gx_fetch_plan_contract_version) ||
      anyNA(parameters$distribution_id) || anyNA(parameters$parameter_order) ||
      any(parameters$parameter_order < 1L) || anyNA(parameters$mapping_status) ||
      any(parameters$mapping_status != "unplanned") ||
      any(!is.na(parameters$parameter_key))) {
    gx_fetch_plan_abort("Fetch-plan parameter columns have invalid required values.")
  }
  lapply(character_columns, function(name) {
    gx_fetch_plan_assert_text(
      parameters[[name]],
      allow_na = name %in% c(
        "variable_id", "variable_uri", "variable_name", "unit_uri", "unit_label",
        "measurement_technique", "parameter_key"
      ),
      nonempty = !name %in% c("variable_name", "unit_label")
    )
  })
  if (!gx_catalog_is_sha256(parameters$distribution_id) ||
      !gx_fetch_plan_variable_id_valid(parameters$variable_id) ||
      !gx_fetch_plan_valid_iri(parameters$variable_uri) ||
      !gx_fetch_plan_valid_iri(parameters$unit_uri)) {
    gx_fetch_plan_abort("Fetch-plan parameter identities are invalid.")
  }
  invisible(parameters)
}

gx_fetch_plan_validate_metadata <- function(metadata, source) {
  if (!is.list(metadata) ||
      !identical(names(metadata), .gx_fetch_plan_metadata_fields) ||
      !gx_fetch_plan_is_utc_scalar(metadata$created_at, allow_na = FALSE) ||
      !identical(metadata$registry_contract_version,
        .gx_fetch_plan_contract_version
      ) ||
      !gx_catalog_is_sha256(metadata$registry_sha256) ||
      length(metadata$registry_sha256) != 1L ||
      !identical(metadata$implementation_contract_version,
        .gx_fetch_plan_contract_version
      ) ||
      !gx_catalog_is_sha256(metadata$implementation_sha256) ||
      length(metadata$implementation_sha256) != 1L ||
      !identical(metadata$ordering, .gx_fetch_plan_ordering) ||
      !is.logical(metadata$execution_ready) ||
      length(metadata$execution_ready) != 1L ||
      is.na(metadata$execution_ready) || metadata$execution_ready ||
      !is.character(metadata$non_replayable_reasons) ||
      anyNA(metadata$non_replayable_reasons) ||
      !length(metadata$non_replayable_reasons) ||
      length(metadata$non_replayable_reasons) > 32L ||
      anyDuplicated(metadata$non_replayable_reasons) ||
      !gx_catalog_byte_sorted(metadata$non_replayable_reasons) ||
      !gx_catalog_is_token(metadata$non_replayable_reasons)) {
    gx_fetch_plan_abort("Fetch-plan metadata has an invalid exact contract.")
  }
  counts <- metadata$counts
  if (!is.list(counts) || !identical(names(counts), .gx_fetch_plan_count_fields) ||
      !all(vapply(counts, function(value) {
        is.integer(value) && length(value) == 1L && !is.na(value) &&
          value >= 0L && value <= .gx_fetch_plan_max_parameters
      }, logical(1)))) {
    gx_fetch_plan_abort("Fetch-plan metadata counts have an invalid exact contract.")
  }
  expected_reasons <- c(
    "handler_implementations_planned", "request_plans_absent"
  )
  if (!identical(source$datasets_status, "complete") || source$datasets_truncated) {
    expected_reasons <- c(expected_reasons, "source_catalog_incomplete")
  }
  expected_reasons <- expected_reasons[gx_catalog_byte_order(expected_reasons)]
  if (!identical(metadata$non_replayable_reasons, expected_reasons)) {
    gx_fetch_plan_abort("Fetch-plan non-replayability reasons are incomplete.")
  }
  invisible(metadata)
}

gx_fetch_plan_validate_registry_binding <- function(x) {
  registry <- gx_handler_registry_load_impl()
  expected_handlers <- gx_fetch_plan_handlers_impl(registry)
  metadata_matches <-
    identical(
      x$metadata$registry_contract_version,
      registry$contract_version
    ) &&
    identical(x$metadata$registry_sha256, registry$portable_sha256) &&
    identical(
      x$metadata$implementation_contract_version,
      registry$contract_version
    ) &&
    identical(
      x$metadata$implementation_sha256,
      registry$implementations_sha256
    )
  if (!metadata_matches || !identical(x$handlers, expected_handlers)) {
    gx_fetch_plan_abort(
      "Fetch-plan handler metadata does not match the bundled registry.",
      "gx_error_fetch_plan_handler"
    )
  }
  for (i in seq_len(nrow(x$distributions))) {
    expected_id <- gx_fetch_plan_classify_impl(
      registry,
      access_url = x$distributions$distribution_url[[i]],
      media_type = x$distributions$media_type[[i]],
      conforms_to = x$distributions$conforms_to[[i]]
    )
    if (!identical(x$distributions$handler_id[[i]], expected_id)) {
      gx_fetch_plan_abort(
        "A fetch-plan distribution does not match its bound classifier.",
        "gx_error_fetch_plan_handler"
      )
    }
  }
  invisible(x)
}

gx_fetch_plan_expected_distribution_order <- function(distributions) {
  gx_catalog_byte_order(
    is.na(distributions$provider_uri),
    ifelse(is.na(distributions$provider_uri), "", distributions$provider_uri),
    is.na(distributions$provider_name),
    ifelse(is.na(distributions$provider_name), "", distributions$provider_name),
    distributions$site_uri,
    distributions$distribution_id
  )
}

gx_fetch_plan_expected_parameter_order <- function(parameters, index) {
  fields <- c(
    "variable_id", "variable_uri", "variable_name", "unit_uri", "unit_label",
    "measurement_technique"
  )
  keys <- unlist(lapply(fields, function(name) {
    value <- parameters[[name]][index]
    list(is.na(value), ifelse(is.na(value), "", value))
  }), recursive = FALSE)
  index[do.call(gx_catalog_byte_order, keys)]
}

gx_fetch_plan_validate_cross_contract <- function(x) {
  distributions <- x$distributions
  parameters <- x$parameters
  handlers <- x$handlers
  counts <- x$metadata$counts

  expected_order <- gx_fetch_plan_expected_distribution_order(distributions)
  if (!identical(expected_order, seq_len(nrow(distributions))) ||
      !identical(distributions$selection_order, seq_len(nrow(distributions)))) {
    gx_fetch_plan_abort("Fetch-plan distribution ordering is noncanonical.")
  }
  handler_positions <- match(distributions$handler_id, handlers$handler_id)
  if (anyNA(handler_positions)) {
    gx_fetch_plan_abort(
      "A fetch-plan distribution references an unknown handler.",
      "gx_error_fetch_plan_handler"
    )
  }

  selected_seen <- 0L
  expected_fetch_order <- rep(NA_integer_, nrow(distributions))
  expected_decision <- character(nrow(distributions))
  for (i in seq_len(nrow(distributions))) {
    effective <- gx_fetch_plan_intersect_time(
      x$time, distributions$temporal_start[i], distributions$temporal_end[i]
    )
    if (!gx_fetch_plan_time_equal(distributions$time_start[i], effective$start) ||
        !gx_fetch_plan_time_equal(distributions$time_end[i], effective$end)) {
      gx_fetch_plan_abort(
        "A fetch-plan effective time does not match its requested/catalog interval.",
        "gx_error_fetch_plan_time"
      )
    }
    outcome <- handlers$outcome[[handler_positions[[i]]]]
    expected <- if (identical(outcome, "reference_only")) {
      if (distributions$fetchable[[i]]) {
        gx_fetch_plan_abort("Reference-only distributions cannot be fetchable.")
      }
      "reference_only"
    } else if (!distributions$fetchable[[i]]) {
      "not_fetchable"
    } else if (effective$outside) {
      "outside_time"
    } else if (selected_seen < x$budgets$max_datasets) {
      selected_seen <- selected_seen + 1L
      expected_fetch_order[[i]] <- selected_seen
      "selected_unplanned"
    } else {
      "skipped_max_datasets"
    }
    expected_decision[[i]] <- expected
    gx_fetch_plan_safe_target(distributions$distribution_url[[i]])
  }
  if (!identical(distributions$decision, expected_decision) ||
      !identical(distributions$fetch_order, expected_fetch_order)) {
    gx_fetch_plan_abort("Fetch-plan selection decisions do not reconcile.")
  }

  expected_parameter_ids <- unlist(Map(
    rep,
    distributions$distribution_id,
    distributions$variable_count
  ), use.names = FALSE)
  if (is.null(expected_parameter_ids)) expected_parameter_ids <- character()
  if (!identical(parameters$distribution_id, expected_parameter_ids)) {
    gx_fetch_plan_abort("Fetch-plan parameter foreign keys or cardinality are invalid.")
  }
  for (i in seq_len(nrow(distributions))) {
    index <- which(parameters$distribution_id == distributions$distribution_id[[i]])
    if (!identical(parameters$parameter_order[index], seq_along(index)) ||
        !identical(gx_fetch_plan_expected_parameter_order(parameters, index), index)) {
      gx_fetch_plan_abort("Fetch-plan parameter ordering is noncanonical.")
    }
  }

  actual_counts <- list(
    catalog_rows = x$source$dataset_rows,
    unplannable_rows = as.integer(x$source$dataset_rows - nrow(parameters)),
    distributions = as.integer(nrow(distributions)),
    parameters = as.integer(nrow(parameters)),
    handlers = as.integer(nrow(handlers)),
    selected = as.integer(sum(distributions$selected)),
    reference_only = as.integer(sum(distributions$decision == "reference_only")),
    not_fetchable = as.integer(sum(distributions$decision == "not_fetchable")),
    outside_time = as.integer(sum(distributions$decision == "outside_time")),
    skipped_max_datasets = as.integer(sum(
      distributions$decision == "skipped_max_datasets"
    )),
    requests = 0L
  )
  if (x$source$dataset_rows < nrow(parameters) ||
      !identical(counts, actual_counts) ||
      as.double(counts$parameters) != sum(as.double(distributions$variable_count)) ||
      as.double(counts$distributions) !=
        as.double(counts$selected) + counts$reference_only +
          counts$not_fetchable + counts$outside_time +
          counts$skipped_max_datasets ||
      counts$selected > x$budgets$max_datasets) {
    gx_fetch_plan_abort("Fetch-plan counts do not reconcile.")
  }
  invisible(x)
}

gx_fetch_plan_validate_body <- function(x) {
  if (!is.list(x) || !identical(class(x), "gx_fetch_plan") ||
      !identical(names(x), .gx_fetch_plan_fields) ||
      !identical(x$contract_version, .gx_fetch_plan_contract_version)) {
    gx_fetch_plan_abort("Fetch plans violate their exact top-level contract.")
  }
  gx_fetch_plan_assert_text_budget(x)
  gx_fetch_plan_validate_source(x$source)
  if (!is.list(x$time) || !identical(names(x$time), c("start", "end"))) {
    gx_fetch_plan_abort(
      "Fetch-plan time has an invalid exact shape.",
      "gx_error_fetch_plan_time"
    )
  }
  normalized_time <- gx_fetch_plan_time_impl(x$time)
  if (!gx_fetch_plan_time_equal(x$time$start, normalized_time$start) ||
      !gx_fetch_plan_time_equal(x$time$end, normalized_time$end)) {
    gx_fetch_plan_abort(
      "Fetch-plan time is noncanonical.",
      "gx_error_fetch_plan_time"
    )
  }
  gx_fetch_plan_validate_distributions(x$distributions)
  gx_fetch_plan_validate_parameters(x$parameters)
  gx_fetch_plan_validate_handlers(x$handlers)
  if (!identical(x$requests, list())) {
    gx_fetch_plan_abort(
      "Fetch-plan contract 0.1.0 requires an exact empty request list.",
      "gx_error_fetch_plan_contract"
    )
  }
  gx_fetch_plan_validate_budgets(x$budgets)
  gx_fetch_plan_validate_metadata(x$metadata, x$source)
  gx_fetch_plan_validate_registry_binding(x)
  gx_fetch_plan_validate_cross_contract(x)
  gx_fetch_plan_assert_text_budget(x)
  invisible(x)
}

gx_fetch_plan_validate_impl <- function(x) {
  tryCatch(
    gx_fetch_plan_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_fetch_plan")) stop(cnd)
      gx_fetch_plan_abort(
        "Fetch-plan validation rejected a malformed object.",
        "gx_error_fetch_plan_contract"
      )
    }
  )
}

#' @export
print.gx_fetch_plan <- function(x, ...) {
  gx_fetch_plan_validate_impl(x)
  counts <- x$metadata$counts
  cli::cli_inform(c(
    "<gx_fetch_plan>",
    "* Distributions: {counts$distributions}; parameters: {counts$parameters}",
    "* Selected: {counts$selected}; reference-only: {counts$reference_only}",
    "* Requests: 0; execution ready: FALSE"
  ))
  invisible(x)
}
