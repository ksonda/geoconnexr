.gx_package_writer_profile <- "fixed-package-writer-v0.2"
.gx_package_publication_contract_version <- "0.2.0"

.gx_package_publication_fields <- c(
  "contract_version", "mode", "status", "stage", "path", "bundle",
  "verification", "metadata", "publication_id"
)

.gx_package_publication_metadata_fields <- c(
  "scope", "creation_only", "end_stage", "resources", "stored_bytes",
  "overwrite", "manifest", "arrow", "quarto", "report", "frictionless",
  "replayable"
)

gx_package_writer_abort <- function(
    message,
    class = "gx_error_package_write_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_write", "gx_error_snapshot_write",
      "gx_error_snapshot", "gx_error_package_resources",
      "gx_error_package"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_writer_roles_impl <- function(role) {
  switch(
    role,
    catalog_sites = c("catalog", "sites"),
    catalog_datasets = c("catalog", "datasets"),
    catalog_problems = c("catalog", "diagnostics"),
    catalog_requests = c(
      "request-ledger-export", "request-ledger-export-v1"
    ),
    fetch_status = c("fetch", "status"),
    native_index = c("fetch", "native-index"),
    native_raw = c("data", "native", "raw"),
    native_table = c("data", "native", "table"),
    observations = c("data", "observations"),
    harmonized_index = c("harmonize", "resource-index"),
    report_html = c("report", "html", "fixed-quarto-report-v1"),
    frictionless_descriptor = c(
      "metadata", "frictionless", "data-package-v1"
    ),
    gx_package_writer_abort(
      "A package resource role cannot be represented in manifest-v1.",
      "gx_error_package_write_manifest"
    )
  )
}

gx_package_writer_manifest_resource_impl <- function(resource) {
  list(
    path = resource$path[[1L]],
    media_type = resource$media_type[[1L]],
    bytes = resource$bytes[[1L]],
    sha256 = resource$sha256[[1L]],
    required = TRUE,
    roles = as.list(gx_package_writer_roles_impl(resource$role[[1L]])),
    source_uri = NULL,
    license_uri = NULL
  )
}

gx_package_writer_recipe_impl <- function(bundle, recipe) {
  recipe$pipeline$end_stage <- "package"
  if (!is.null(bundle$input$fetched)) {
    plan <- bundle$input$fetched$plan
    recipe$time <- list(
      start = if (is.na(plan$time$start)) {
        NULL
      } else {
        gx_snapshot_writer_time(plan$time$start)
      },
      end = if (is.na(plan$time$end)) {
        NULL
      } else {
        gx_snapshot_writer_time(plan$time$end)
      }
    )
    selected <- which(
      plan$distributions$selected &
        !is.na(plan$distributions$fetch_order)
    )
    selected <- selected[order(
      plan$distributions$fetch_order[selected],
      method = "radix"
    )]
    selected_handlers <- unique(
      plan$distributions$handler_id[selected]
    )
    recipe$fetch <- list(
      enabled = !isTRUE(bundle$input$fetched$metadata$dry_run),
      max_datasets = plan$budgets$max_datasets,
      max_requests = plan$budgets$max_requests,
      max_encoded_bytes = plan$budgets$max_encoded_bytes,
      max_decoded_bytes = plan$budgets$max_decoded_bytes,
      handler_order = as.list(unname(selected_handlers))
    )
  }
  if (!is.null(bundle$input$harmonized)) {
    units <- bundle$input$harmonized$target_units$units
    target_units <- as.list(stats::setNames(
      units$unit_uri,
      units$dimension
    ))
    recipe$harmonize <- list(
      enabled = TRUE,
      target_units = target_units
    )
  }
  recipe$output <- list(
    timeseries = bundle$timeseries,
    keep_raw = any(bundle$resources$role == "native_raw"),
    report = gx_package_bundle_is_report_impl(bundle)
  )
  recipe
}

gx_package_writer_completeness_impl <- function(bundle, catalog_rows) {
  rows <- catalog_rows
  if (!is.null(bundle$input$fetched)) {
    fetched <- bundle$input$fetched
    failed <- fetched$metadata$counts$failed
    deferred <- any(fetched$status$status %in% c(
      "batch_limit_deferred", "handler_plan_unsupported"
    ))
    if (isTRUE(fetched$metadata$dry_run)) {
      status <- "not_run"
      reason <- "dry_run"
    } else if (failed > 0L || deferred) {
      status <- "partial"
      reason <- "failures_or_deferred_distributions"
    } else {
      status <- "complete"
      reason <- NULL
    }
    item <- list(
      stage = "fetched",
      status = status,
      truncated = deferred
    )
    if (!is.null(reason)) item$reason <- reason
    rows[[length(rows) + 1L]] <- item
  }
  if (!is.null(bundle$input$harmonized)) {
    rows[[length(rows) + 1L]] <- list(
      stage = "harmonized",
      status = "complete",
      truncated = FALSE
    )
  }
  rows[[length(rows) + 1L]] <- list(
    stage = "package",
    status = "complete",
    truncated = FALSE
  )
  order <- order(
    vapply(rows, `[[`, character(1), "stage"),
    method = "radix"
  )
  rows[order]
}

gx_package_writer_manifest_impl <- function(bundle) {
  gx_package_bundle_validate_impl(bundle)
  catalog <- bundle$input$catalog
  tables <- gx_package_input_catalog_tables_impl(catalog)
  requests <- gx_snapshot_writer_requests(
    gx_catalog_export_views_impl(catalog)$requests
  )
  catalog_paths <- c(
    sites = "catalog/sites.csv",
    datasets = "catalog/datasets.csv",
    problems = "catalog/problems.csv",
    requests = "requests.csv"
  )
  states <- lapply(catalog_paths, function(path) {
    position <- match(path, bundle$resources$path)
    if (is.na(position)) {
      gx_package_writer_abort(
        "The M9k bundle is missing one required catalog resource.",
        "gx_error_package_write_manifest"
      )
    }
    list(
      bytes = bundle$resources$bytes[[position]],
      sha256 = bundle$resources$sha256[[position]]
    )
  })
  manifest <- gx_snapshot_writer_manifest(catalog, states, requests)
  manifest$recipe <- gx_package_writer_recipe_impl(
    bundle,
    manifest$recipe
  )
  manifest$replay <- list(
    replayable = FALSE,
    non_replayable_reasons = as.list(c(
      "package_writer_v0_1_non_replayable",
      if (!is.null(bundle$input$fetched)) {
        "fetch_request_ledger_incomplete"
      },
      "refresh_contract_deferred"
    )),
    handler_versions = list()
  )
  manifest$effective_options$serialization <- list(
    writer = .gx_package_writer_profile,
    csv = gx_snapshot_writer_csv_profile,
    parquet = if (identical(bundle$timeseries, "parquet")) {
      list(
        profile = "fixed-arrow-parquet-v1",
        arrow_package_version = bundle$metadata$arrow_package_version,
        arrow_minimum_version = .gx_package_parquet_arrow_minimum,
        parquet_version = "2.4",
        compression = "uncompressed",
        use_dictionary = FALSE,
        write_statistics = FALSE,
        coerce_timestamps = "us"
      )
    } else {
      NULL
    },
    resource_profile = gx_package_bundle_resource_profile_impl(bundle),
    package_input_id = bundle$input$input_id,
    bundle_id = bundle$bundle_id,
    request_export = "manifest-requests-csv-v1",
    request_ledger_scope = "catalog_only"
  )
  if (gx_package_bundle_is_report_impl(bundle)) {
    manifest$effective_options$serialization$report <-
      gx_package_bundle_report_manifest_impl(bundle)
  }
  if (gx_package_bundle_is_frictionless_impl(bundle)) {
    manifest$effective_options$serialization$frictionless <-
      gx_package_bundle_frictionless_manifest_impl(bundle)
  }
  manifest$effective_options$package_stage <- bundle$stage
  manifest$effective_options$package_resource_counts <-
    bundle$metadata$counts
  manifest$resources <- lapply(
    seq_len(nrow(bundle$resources)),
    function(position) {
      gx_package_writer_manifest_resource_impl(
        bundle$resources[position, , drop = FALSE]
      )
    }
  )
  manifest$completeness <- gx_package_writer_completeness_impl(
    bundle,
    manifest$completeness
  )
  manifest
}

gx_package_writer_mkdir <- function(path) {
  dir.create(path, mode = "0700", showWarnings = FALSE)
}

gx_package_writer_write_raw <- function(path, bytes) {
  gx_snapshot_writer_write_raw(path, bytes)
}

gx_package_writer_rename <- function(from, to) {
  gx_snapshot_writer_rename(from, to)
}

gx_package_writer_unlink <- function(path) {
  gx_snapshot_writer_unlink(path)
}

gx_package_writer_verify <- function(path) {
  gx_snapshot_verify_impl(path)
}

gx_package_writer_directories_impl <- function(paths) {
  directories <- unique(dirname(paths))
  directories <- setdiff(directories, ".")
  ancestors <- character()
  for (directory in directories) {
    current <- directory
    while (!identical(current, ".") && nzchar(current)) {
      ancestors <- c(ancestors, current)
      current <- dirname(current)
    }
  }
  ancestors <- unique(ancestors)
  depth <- lengths(strsplit(ancestors, "/", fixed = TRUE))
  ancestors[order(depth, ancestors, method = "radix")]
}

gx_package_writer_cleanup_impl <- function(path) {
  result <- tryCatch(
    gx_snapshot_writer_muffle_warnings(gx_package_writer_unlink(path)),
    error = function(cnd) NULL
  )
  if (is.null(result) || length(result) != 1L ||
      is.na(result) || result != 0L) {
    return(FALSE)
  }
  absent <- tryCatch(
    gx_snapshot_writer_muffle_warnings({
      link <- Sys.readlink(path)
      !file.exists(path) && !dir.exists(path) &&
        length(link) == 1L && (is.na(link) || !nzchar(link))
    }),
    error = function(cnd) FALSE
  )
  isTRUE(absent)
}

gx_package_writer_assert_verification_impl <- function(
    verification,
    bundle) {
  profile_valid <- tryCatch({
    gx_package_bundle_manifest_validate_impl(verification, bundle)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  valid <- profile_valid && identical(verification$status, "verified") &&
    identical(verification$resources$path, bundle$resources$path) &&
    identical(
      verification$resources$expected_bytes,
      bundle$resources$bytes
    ) &&
    identical(
      verification$resources$expected_sha256,
      bundle$resources$sha256
    ) &&
    all(verification$resources$present) &&
    all(verification$resources$status == "verified")
  if (!valid) {
    gx_package_writer_abort(
      "Closed-tree verification no longer matches the exact M9k bundle.",
      "gx_error_package_write_verification"
    )
  }
  invisible(verification)
}

gx_package_publication_metadata_impl <- function(bundle) {
  list(
    scope = "fixed_package_publication_v1",
    creation_only = TRUE,
    end_stage = "package",
    resources = unname(as.integer(nrow(bundle$resources))),
    stored_bytes = unname(as.double(sum(bundle$resources$bytes))),
    overwrite = FALSE,
    manifest = TRUE,
    arrow = bundle$metadata$arrow,
    quarto = gx_package_bundle_is_report_impl(bundle),
    report = gx_package_bundle_is_report_impl(bundle),
    frictionless = gx_package_bundle_is_frictionless_impl(bundle),
    replayable = FALSE
  )
}

gx_package_publication_id_impl <- function(
    path,
    bundle,
    verification,
    metadata) {
  gx_contract_hash(
    list(
      "mode", "fixed_package_publication",
      "status", "written_and_verified",
      "path", path,
      "stage", bundle$stage,
      "bundle_id", bundle$bundle_id,
      "manifest_sha256", verification$manifest_sha256,
      "resources", metadata$resources,
      "stored_bytes", metadata$stored_bytes
    ),
    namespace = "geoconnexr.package-publication.v1",
    contract_version = .gx_package_publication_contract_version
  )
}

gx_package_publication_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_package_publication") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_publication_fields) &&
    identical(
      x$contract_version,
      .gx_package_publication_contract_version
    ) &&
    identical(x$mode, "fixed_package_publication") &&
    identical(x$status, "written_and_verified") &&
    is.character(x$stage) && length(x$stage) == 1L &&
    !is.na(x$stage) && is.null(attributes(x$stage)) &&
    is.character(x$path) && length(x$path) == 1L &&
    !is.na(x$path) && is.null(attributes(x$path)) && nzchar(x$path) &&
    is.character(x$publication_id) && length(x$publication_id) == 1L &&
    !is.na(x$publication_id) &&
    is.null(attributes(x$publication_id)) &&
    gx_catalog_is_sha256(x$publication_id)
  if (!valid_top) {
    gx_package_writer_abort(
      "Package publication violates its exact top-level contract.",
      "gx_error_package_publication_contract"
    )
  }
  bundle_valid <- tryCatch({
    gx_package_bundle_validate_impl(x$bundle)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  verification_valid <- tryCatch({
    gx_snapshot_verification_validate_impl(x$verification)
    gx_package_bundle_manifest_validate_impl(x$verification, x$bundle)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  root <- tryCatch(
    gx_snapshot_root(x$path),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  expected_metadata <- if (bundle_valid) {
    gx_package_publication_metadata_impl(x$bundle)
  } else {
    NULL
  }
  serialization <- if (verification_valid) {
    x$verification$manifest$effective_options$serialization
  } else {
    NULL
  }
  valid <- bundle_valid && verification_valid && !is.null(root) &&
    identical(root$path, x$path) &&
    identical(x$stage, x$bundle$stage) &&
    identical(
      x$verification$manifest$recipe$pipeline$end_stage,
      "package"
    ) &&
    identical(x$verification$manifest$replay$replayable, FALSE) &&
    identical(serialization$writer, .gx_package_writer_profile) &&
    identical(
      serialization$resource_profile,
      gx_package_bundle_resource_profile_impl(x$bundle)
    ) &&
    identical(serialization$package_input_id, x$bundle$input$input_id) &&
    identical(serialization$bundle_id, x$bundle$bundle_id) &&
    identical(
      x$verification$resources$path,
      x$bundle$resources$path
    ) &&
    identical(
      x$verification$resources$expected_bytes,
      x$bundle$resources$bytes
    ) &&
    identical(
      x$verification$resources$expected_sha256,
      x$bundle$resources$sha256
    ) &&
    all(x$verification$resources$present) &&
    all(x$verification$resources$status == "verified") &&
    is.list(x$metadata) &&
    identical(names(x$metadata), .gx_package_publication_metadata_fields) &&
    !is.null(expected_metadata) &&
    identical(x$metadata, expected_metadata) &&
    identical(
      x$publication_id,
      gx_package_publication_id_impl(
        x$path,
        x$bundle,
        x$verification,
        x$metadata
      )
    )
  if (!valid) {
    gx_package_writer_abort(
      "Package publication no longer binds its bundle and final verification.",
      "gx_error_package_publication_contract"
    )
  }
  invisible(x)
}

# Internal M9l boundary. It publishes one already-serialized M9k bundle only
# through verified sibling staging and creation-only atomic rename.
gx_package_write_impl <- function(bundle, dir) {
  cleanup <- new.env(parent = emptyenv())
  cleanup$stage <- NULL
  cleanup$owned <- FALSE
  on.exit({
    if (isTRUE(cleanup$owned) && is.character(cleanup$stage)) {
      try(gx_package_writer_cleanup_impl(cleanup$stage), silent = TRUE)
    }
  }, add = TRUE)
  tryCatch(
    {
      gx_package_bundle_validate_impl(bundle)
      destination <- gx_snapshot_writer_scalar_path(dir)
      if (gx_snapshot_writer_entry_exists(destination$target)) {
        gx_package_writer_abort(
          "The package destination already exists; overwrite is unsupported.",
          "gx_error_package_write_exists"
        )
      }
      stage <- tempfile(
        pattern = ".gx-package-stage-",
        tmpdir = destination$parent
      )
      if (!isTRUE(gx_package_writer_mkdir(stage))) {
        gx_package_writer_abort(
          "The package staging directory could not be created.",
          "gx_error_package_write_io"
        )
      }
      cleanup$stage <- stage
      cleanup$owned <- TRUE
      destination$parent_info <- gx_snapshot_assert_fs_type(
        destination$parent,
        "directory"
      )
      gx_snapshot_assert_fs_type(stage, "directory")

      gx_package_bundle_validate_impl(bundle)
      directories <- gx_package_writer_directories_impl(
        bundle$resources$path
      )
      for (directory in directories) {
        parent <- dirname(file.path(stage, directory))
        gx_snapshot_assert_fs_type(parent, "directory")
        path <- file.path(stage, directory)
        if (!isTRUE(gx_package_writer_mkdir(path))) {
          gx_package_writer_abort(
            "A package resource directory could not be created.",
            "gx_error_package_write_io"
          )
        }
        gx_snapshot_assert_fs_type(path, "directory")
      }
      for (path in bundle$resources$path) {
        gx_package_writer_write_raw(
          file.path(stage, path),
          bundle$contents[[path]]
        )
      }
      manifest <- gx_package_writer_manifest_impl(bundle)
      manifest_bytes <- gx_snapshot_writer_json_bytes(manifest)
      gx_package_writer_write_raw(
        file.path(stage, gx_snapshot_manifest_name),
        manifest_bytes
      )
      staged_verification <- gx_package_writer_verify(stage)
      gx_package_writer_assert_verification_impl(
        staged_verification,
        bundle
      )

      gx_snapshot_assert_same_info(
        destination$parent_info,
        gx_snapshot_assert_fs_type(destination$parent, "directory")
      )
      if (gx_snapshot_writer_entry_exists(destination$target)) {
        gx_package_writer_abort(
          "The package destination changed before atomic exposure.",
          "gx_error_package_write_race"
        )
      }
      if (!isTRUE(gx_package_writer_rename(stage, destination$target))) {
        gx_package_writer_abort(
          "The verified package could not be exposed atomically.",
          "gx_error_package_write_io"
        )
      }
      cleanup$owned <- FALSE
      final_verification <- gx_package_writer_verify(destination$target)
      gx_package_writer_assert_verification_impl(
        final_verification,
        bundle
      )
      if (!identical(
        staged_verification$manifest_sha256,
        final_verification$manifest_sha256
      )) {
        gx_package_writer_abort(
          "The package changed during atomic exposure.",
          "gx_error_package_write_race"
        )
      }
      path <- normalizePath(
        destination$target,
        winslash = "/",
        mustWork = TRUE
      )
      verification <- structure(
        final_verification,
        class = "gx_snapshot_verification"
      )
      gx_snapshot_verification_validate_impl(verification)
      metadata <- gx_package_publication_metadata_impl(bundle)
      object <- structure(
        list(
          contract_version = .gx_package_publication_contract_version,
          mode = "fixed_package_publication",
          status = "written_and_verified",
          stage = bundle$stage,
          path = unname(path),
          bundle = bundle,
          verification = verification,
          metadata = metadata,
          publication_id = gx_package_publication_id_impl(
            unname(path),
            bundle,
            verification,
            metadata
          )
        ),
        class = "gx_package_publication"
      )
      gx_package_publication_validate_impl(object)
      object
    },
    error = function(cnd) {
      if (isTRUE(cleanup$owned)) {
        cleaned <- gx_package_writer_cleanup_impl(cleanup$stage)
        if (isTRUE(cleaned)) {
          cleanup$owned <- FALSE
        } else {
          gx_package_writer_abort(
            "Package staging cleanup failed; no destination was published.",
            "gx_error_package_write_cleanup"
          )
        }
      }
      if (inherits(cnd, "gx_error_package_write")) stop(cnd)
      gx_package_writer_abort(
        "Package writing failed closed.",
        "gx_error_package_write_contract"
      )
    }
  )
}
