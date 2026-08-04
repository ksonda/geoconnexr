.gx_package_input_contract_version <- "0.1.0"

.gx_package_input_fields <- c(
  "contract_version", "stage", "catalog", "fetched", "harmonized",
  "evidence", "metadata", "input_id"
)

.gx_package_input_evidence_fields <- c(
  "catalog_resources", "fetch_plan_sha256", "fetched_status_sha256",
  "fetched_results_sha256", "harmonization_sha256"
)

.gx_package_input_metadata_fields <- c(
  "scope", "end_stage", "catalog_lineage", "fetched_included",
  "harmonized_included", "native_payloads_preserved", "serializes",
  "publishes", "replayable", "counts", "limitations"
)

.gx_package_input_count_fields <- c(
  "sites", "datasets", "problems", "requests", "distributions",
  "fetched_results", "harmonized_resources", "observations"
)

gx_package_input_abort <- function(
    message,
    class = "gx_error_package_input_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_input", "gx_error_package"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_input_table_hash_impl <- function(x, namespace) {
  valid <- is.data.frame(x) && is.character(namespace) &&
    length(namespace) == 1L && !is.na(namespace) && nzchar(namespace) &&
    all(vapply(x, is.atomic, logical(1)))
  if (!valid) {
    gx_package_input_abort(
      "Package-input table evidence requires one atomic data-frame projection."
    )
  }
  row_hashes <- character(nrow(x))
  if (nrow(x)) {
    for (index in seq_len(nrow(x))) {
      values <- list("row", as.integer(index))
      for (field in names(x)) {
        values <- c(
          values,
          list("field", field, "value", x[[field]][[index]])
        )
      }
      row_hashes[[index]] <- gx_contract_hash(
        values,
        namespace = paste0(namespace, ".row"),
        contract_version = .gx_package_input_contract_version
      )
    }
  }
  schema <- paste(
    names(x),
    vapply(x, typeof, character(1)),
    sep = ":",
    collapse = "|"
  )
  digest::digest(
    paste(
      c(
        paste0("namespace:", namespace),
        paste0("rows:", nrow(x)),
        paste0("columns:", ncol(x)),
        paste0("schema:", schema),
        row_hashes
      ),
      collapse = "\n"
    ),
    algo = "sha256",
    serialize = FALSE
  )
}

gx_package_input_vector_hash_impl <- function(x, namespace) {
  if (!is.character(x) || anyNA(x) || is.null(namespace) ||
      !is.character(namespace) || length(namespace) != 1L ||
      is.na(namespace) || !nzchar(namespace)) {
    gx_package_input_abort(
      "Package-input vector evidence requires bounded character identities."
    )
  }
  digest::digest(
    paste(
      c(
        paste0("namespace:", namespace),
        paste0("count:", length(x)),
        paste0(nchar(enc2utf8(x), type = "bytes"), ":", enc2utf8(x))
      ),
      collapse = "\n"
    ),
    algo = "sha256",
    serialize = FALSE
  )
}

gx_package_input_catalog_tables_impl <- function(catalog) {
  gx_catalog_validate_impl(catalog)
  views <- gx_catalog_export_views_impl(catalog)
  requests <- gx_snapshot_writer_requests(views$requests)
  views$requests <- gx_snapshot_writer_requests_view(requests)
  resources <- c("sites", "datasets", "problems", "requests")
  tables <- lapply(resources, function(resource) {
    gx_snapshot_writer_redact_view(
      gx_snapshot_writer_character_view(views[[resource]])
    )
  })
  names(tables) <- resources
  tables
}

gx_package_input_catalog_resources_impl <- function(catalog) {
  tables <- gx_package_input_catalog_tables_impl(catalog)
  resources <- names(tables)
  hashes <- vapply(resources, function(resource) {
    bytes <- gx_snapshot_csv_bytes_impl(tables[[resource]])
    digest::digest(bytes, algo = "sha256", serialize = FALSE)
  }, character(1), USE.NAMES = TRUE)
  unname(hashes) |> stats::setNames(resources)
}

gx_package_input_plan_hash_impl <- function(plan) {
  gx_fetch_plan_validate_impl(plan)
  distributions <- plan$distributions[
    !vapply(plan$distributions, is.list, logical(1))
  ]
  source <- unlist(plan$source, use.names = TRUE)
  budgets <- unlist(plan$budgets, use.names = TRUE)
  counts <- unlist(plan$metadata$counts, use.names = TRUE)
  gx_contract_hash(
    list(
      "source", gx_package_input_vector_hash_impl(
        paste(names(source), source, sep = "="),
        "geoconnexr.package-input.plan-source.v1"
      ),
      "time_start", plan$time$start,
      "time_end", plan$time$end,
      "distributions", gx_package_input_table_hash_impl(
        distributions,
        "geoconnexr.package-input.plan-distributions.v1"
      ),
      "parameters", gx_package_input_table_hash_impl(
        plan$parameters,
        "geoconnexr.package-input.plan-parameters.v1"
      ),
      "handlers", gx_package_input_table_hash_impl(
        plan$handlers,
        "geoconnexr.package-input.plan-handlers.v1"
      ),
      "budgets", gx_package_input_vector_hash_impl(
        paste(names(budgets), budgets, sep = "="),
        "geoconnexr.package-input.plan-budgets.v1"
      ),
      "created_at", plan$metadata$created_at,
      "registry_sha256", plan$metadata$registry_sha256,
      "implementation_sha256", plan$metadata$implementation_sha256,
      "counts", gx_package_input_vector_hash_impl(
        paste(names(counts), counts, sep = "="),
        "geoconnexr.package-input.plan-counts.v1"
      )
    ),
    namespace = "geoconnexr.package-input.fetch-plan.v1",
    contract_version = .gx_package_input_contract_version
  )
}

gx_package_input_fetched_evidence_impl <- function(fetched) {
  gx_fetched_validate_impl(fetched)
  result_projection <- fetched$results[
    setdiff(names(fetched$results), c("data", "raw_body"))
  ]
  list(
    fetch_plan_sha256 = gx_package_input_plan_hash_impl(fetched$plan),
    fetched_status_sha256 = gx_package_input_table_hash_impl(
      fetched$status,
      "geoconnexr.package-input.fetched-status.v1"
    ),
    fetched_results_sha256 = gx_package_input_table_hash_impl(
      result_projection,
      "geoconnexr.package-input.fetched-results.v1"
    )
  )
}

gx_package_input_harmonization_hash_impl <- function(harmonized) {
  gx_harmonized_validate_impl(harmonized)
  csv_ids <- vapply(
    harmonized$csv_mappings,
    `[[`,
    character(1),
    "mapping_id",
    USE.NAMES = FALSE
  )
  feature_ids <- vapply(
    harmonized$feature_mappings,
    `[[`,
    character(1),
    "mapping_id",
    USE.NAMES = FALSE
  )
  counts <- unlist(harmonized$metadata$counts, use.names = TRUE)
  gx_contract_hash(
    list(
      "target_asset_sha256", harmonized$target_units$asset_sha256,
      "target_units_sha256", gx_package_input_table_hash_impl(
        harmonized$target_units$units,
        "geoconnexr.package-input.target-units.v1"
      ),
      "timezone_asset_sha256",
      harmonized$metadata$wqp_timezone_asset_sha256,
      "csv_mappings_sha256", gx_package_input_vector_hash_impl(
        csv_ids,
        "geoconnexr.package-input.csv-mappings.v1"
      ),
      "feature_mappings_sha256", gx_package_input_vector_hash_impl(
        feature_ids,
        "geoconnexr.package-input.feature-mappings.v1"
      ),
      "resources_sha256", gx_package_input_table_hash_impl(
        harmonized$resources,
        "geoconnexr.package-input.harmonized-resources.v1"
      ),
      "counts_sha256", gx_package_input_vector_hash_impl(
        paste(names(counts), counts, sep = "="),
        "geoconnexr.package-input.harmonized-counts.v1"
      )
    ),
    namespace = "geoconnexr.package-input.harmonization.v1",
    contract_version = .gx_package_input_contract_version
  )
}

gx_package_input_lineage_impl <- function(catalog, fetched) {
  expected <- gx_fetch_plan_source_impl(catalog)
  if (!identical(fetched$plan$source, expected)) {
    gx_package_input_abort(
      paste0(
        "The explicit catalog does not match the fetched plan's exact AOI ",
        "and dataset source identity."
      ),
      "gx_error_package_input_lineage"
    )
  }
  invisible(catalog)
}

gx_package_input_parts_impl <- function(x, catalog) {
  if (identical(class(x), "gx_catalog")) {
    if (!is.null(catalog)) {
      gx_package_input_abort(
        "Catalog-stage package input must not supply a second catalog.",
        "gx_error_package_input"
      )
    }
    gx_catalog_validate_impl(x)
    return(list(
      stage = "catalog",
      catalog = x,
      fetched = NULL,
      harmonized = NULL
    ))
  }

  if (!identical(class(catalog), "gx_catalog")) {
    gx_package_input_abort(
      paste0(
        "Fetched and harmonized package input requires the explicit valid ",
        "catalog that produced the embedded fetch plan."
      ),
      "gx_error_package_input"
    )
  }
  gx_catalog_validate_impl(catalog)

  if (identical(class(x), "gx_fetched")) {
    gx_fetched_validate_impl(x)
    gx_package_input_lineage_impl(catalog, x)
    return(list(
      stage = "fetched",
      catalog = catalog,
      fetched = x,
      harmonized = NULL
    ))
  }
  if (identical(class(x), "gx_harmonized")) {
    gx_harmonized_validate_impl(x)
    gx_package_input_lineage_impl(catalog, x$fetched)
    return(list(
      stage = "harmonized",
      catalog = catalog,
      fetched = x$fetched,
      harmonized = x
    ))
  }
  gx_package_input_abort(
    "Package input accepts only one exact catalog, fetched, or harmonized object.",
    "gx_error_package_input"
  )
}

gx_package_input_evidence_impl <- function(parts) {
  fetched <- if (is.null(parts$fetched)) {
    list(
      fetch_plan_sha256 = NA_character_,
      fetched_status_sha256 = NA_character_,
      fetched_results_sha256 = NA_character_
    )
  } else {
    gx_package_input_fetched_evidence_impl(parts$fetched)
  }
  list(
    catalog_resources = gx_package_input_catalog_resources_impl(parts$catalog),
    fetch_plan_sha256 = fetched$fetch_plan_sha256,
    fetched_status_sha256 = fetched$fetched_status_sha256,
    fetched_results_sha256 = fetched$fetched_results_sha256,
    harmonization_sha256 = if (is.null(parts$harmonized)) {
      NA_character_
    } else {
      gx_package_input_harmonization_hash_impl(parts$harmonized)
    }
  )
}

gx_package_input_metadata_impl <- function(parts) {
  fetched <- parts$fetched
  harmonized <- parts$harmonized
  limitations <- c(
    "arrow_deferred", "frictionless_deferred", "publication_deferred",
    "quarto_deferred", "replay_deferred", "serialization_deferred"
  )
  limitations <- limitations[gx_catalog_byte_order(limitations)]
  list(
    scope = "package_input_v1",
    end_stage = parts$stage,
    catalog_lineage = if (identical(parts$stage, "catalog")) {
      "self"
    } else {
      "explicit_rebound"
    },
    fetched_included = !is.null(fetched),
    harmonized_included = !is.null(harmonized),
    native_payloads_preserved = !is.null(fetched),
    serializes = FALSE,
    publishes = FALSE,
    replayable = FALSE,
    counts = list(
      sites = unname(as.integer(nrow(parts$catalog$sites))),
      datasets = unname(as.integer(nrow(parts$catalog$datasets))),
      problems = unname(as.integer(nrow(parts$catalog$problems))),
      requests = unname(as.integer(nrow(parts$catalog$requests))),
      distributions = if (is.null(fetched)) {
        0L
      } else {
        unname(as.integer(nrow(fetched$status)))
      },
      fetched_results = if (is.null(fetched)) {
        0L
      } else {
        unname(as.integer(nrow(fetched$results)))
      },
      harmonized_resources = if (is.null(harmonized)) {
        0L
      } else {
        unname(as.integer(nrow(harmonized$resources)))
      },
      observations = if (is.null(harmonized)) {
        0L
      } else {
        unname(as.integer(nrow(harmonized$observations)))
      }
    ),
    limitations = limitations
  )
}

gx_package_input_id_impl <- function(stage, evidence, metadata) {
  counts <- unlist(metadata$counts, use.names = TRUE)
  gx_contract_hash(
    list(
      "stage", stage,
      "catalog_sites_sha256", evidence$catalog_resources[["sites"]],
      "catalog_datasets_sha256", evidence$catalog_resources[["datasets"]],
      "catalog_problems_sha256", evidence$catalog_resources[["problems"]],
      "catalog_requests_sha256", evidence$catalog_resources[["requests"]],
      "fetch_plan_sha256", evidence$fetch_plan_sha256,
      "fetched_status_sha256", evidence$fetched_status_sha256,
      "fetched_results_sha256", evidence$fetched_results_sha256,
      "harmonization_sha256", evidence$harmonization_sha256,
      "counts_sha256", gx_package_input_vector_hash_impl(
        paste(names(counts), counts, sep = "="),
        "geoconnexr.package-input.counts.v1"
      )
    ),
    namespace = "geoconnexr.package-input.v1",
    contract_version = .gx_package_input_contract_version
  )
}

gx_package_input_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_package_input") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_input_fields) &&
    identical(x$contract_version, .gx_package_input_contract_version) &&
    is.character(x$stage) && length(x$stage) == 1L &&
    x$stage %in% c("catalog", "fetched", "harmonized") &&
    is.character(x$input_id) && length(x$input_id) == 1L &&
    !is.na(x$input_id) && is.null(attributes(x$input_id)) &&
    gx_catalog_is_sha256(x$input_id)
  if (!valid_top) {
    gx_package_input_abort(
      "Package input violates its exact top-level contract."
    )
  }
  parts <- tryCatch(
    gx_package_input_parts_impl(
      if (identical(x$stage, "catalog")) {
        x$catalog
      } else if (identical(x$stage, "fetched")) {
        x$fetched
      } else {
        x$harmonized
      },
      if (identical(x$stage, "catalog")) NULL else x$catalog
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(parts) ||
      !identical(x$catalog, parts$catalog) ||
      !identical(x$fetched, parts$fetched) ||
      !identical(x$harmonized, parts$harmonized)) {
    gx_package_input_abort(
      "Package input no longer binds its exact stage lineage."
    )
  }
  evidence <- gx_package_input_evidence_impl(parts)
  metadata <- gx_package_input_metadata_impl(parts)
  valid_evidence <- is.list(x$evidence) &&
    identical(names(x$evidence), .gx_package_input_evidence_fields) &&
    is.character(x$evidence$catalog_resources) &&
    identical(
      names(x$evidence$catalog_resources),
      c("sites", "datasets", "problems", "requests")
    )
  valid_metadata <- is.list(x$metadata) &&
    identical(names(x$metadata), .gx_package_input_metadata_fields) &&
    is.list(x$metadata$counts) &&
    identical(names(x$metadata$counts), .gx_package_input_count_fields)
  valid <- valid_evidence && valid_metadata &&
    identical(x$evidence, evidence) &&
    identical(x$metadata, metadata) &&
    identical(
      x$input_id,
      gx_package_input_id_impl(x$stage, evidence, metadata)
    )
  if (!valid) {
    gx_package_input_abort(
      "Package-input evidence, metadata, or identity no longer matches its inputs."
    )
  }
  invisible(x)
}

# Internal M9j boundary. It admits exact package-stage inputs and rebinds the
# explicit catalog to fetched/harmonized lineage without serializing or writing.
gx_package_input_impl <- function(x, catalog = NULL) {
  parts <- gx_package_input_parts_impl(x, catalog)
  evidence <- gx_package_input_evidence_impl(parts)
  metadata <- gx_package_input_metadata_impl(parts)
  object <- structure(
    list(
      contract_version = .gx_package_input_contract_version,
      stage = parts$stage,
      catalog = parts$catalog,
      fetched = parts$fetched,
      harmonized = parts$harmonized,
      evidence = evidence,
      metadata = metadata,
      input_id = gx_package_input_id_impl(parts$stage, evidence, metadata)
    ),
    class = "gx_package_input"
  )
  gx_package_input_validate_impl(object)
  object
}
