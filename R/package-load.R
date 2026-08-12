.gx_package_loaded_contract_version <- "0.2.0"

.gx_package_loaded_fields <- c(
  "contract_version", "mode", "status", "stage", "path", "verification",
  "resources", "contents", "metadata", "load_id"
)

.gx_package_loaded_resource_columns <- c(
  "path", "role", "format", "media_type", "bytes", "sha256", "roles"
)

.gx_package_loaded_metadata_fields <- c(
  "scope", "read_only", "byte_preserving", "resources", "csv_resources",
  "parquet_resources", "raw_resources", "stored_bytes", "parses_tables", "authenticity",
  "frictionless", "replayable"
)

gx_package_load_abort <- function(
    message,
    class = "gx_error_package_load_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_load", "gx_error_package",
      "gx_error_snapshot"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_load_role_impl <- function(roles) {
  key <- paste(roles, collapse = "\x1f")
  role <- switch(
    key,
    "catalog\x1fsites" = "catalog_sites",
    "catalog\x1fdatasets" = "catalog_datasets",
    "catalog\x1fdiagnostics" = "catalog_problems",
    "request-ledger-export\x1frequest-ledger-export-v1" =
      "catalog_requests",
    "fetch\x1fstatus" = "fetch_status",
    "fetch\x1fnative-index" = "native_index",
    "data\x1fnative\x1fraw" = "native_raw",
    "data\x1fnative\x1ftable" = "native_table",
    "data\x1fobservations" = "observations",
    "harmonize\x1fresource-index" = "harmonized_index",
    "report\x1fhtml\x1ffixed-quarto-report-v1" = "report_html",
    "metadata\x1ffrictionless\x1fdata-package-v1" =
      "frictionless_descriptor",
    NULL
  )
  if (is.null(role)) {
    gx_package_load_abort(
      "A stored resource is outside the fixed package loading profile.",
      "gx_error_package_load_profile"
    )
  }
  role
}

gx_package_load_resource_profile_impl <- function(verification) {
  profile <- gx_package_owned_manifest_profile_impl(verification)
  has_report <- isTRUE(verification$manifest$recipe$output$report)
  has_frictionless <- !is.null(profile$frictionless)
  evidence <- verification$resources
  roles <- lapply(evidence$roles, unname)
  role <- vapply(roles, gx_package_load_role_impl, character(1))
  format <- ifelse(
    role %in% c("native_raw", "report_html", "frictionless_descriptor"),
    "raw",
    ifelse(
      role == "observations" &
        evidence$media_type == "application/vnd.apache.parquet",
      "parquet",
      "csv"
    )
  )
  resources <- tibble::tibble(
    path = unname(evidence$path),
    role = unname(role),
    format = unname(format),
    media_type = unname(evidence$media_type),
    bytes = unname(as.numeric(evidence$actual_bytes)),
    sha256 = unname(evidence$actual_sha256),
    roles = unname(roles)
  )

  base <- c(
    "catalog_sites", "catalog_datasets", "catalog_problems",
    "catalog_requests"
  )
  expected <- switch(
    profile$stage,
    catalog = base,
    fetched = c(base, "fetch_status", "native_index"),
    harmonized = c(
      base, "fetch_status", "native_index", "observations",
      "harmonized_index"
    )
  )
  if (has_report) expected <- c(expected, "report_html")
  if (has_frictionless) expected <- c(expected, "frictionless_descriptor")
  singleton <- setdiff(unique(resources$role), c(
    "native_raw", "native_table"
  ))
  valid_counts <- all(vapply(
    expected,
    function(item) sum(resources$role == item) == 1L,
    logical(1)
  )) &&
    setequal(singleton, expected)

  fixed_paths <- c(
    catalog_sites = "catalog/sites.csv",
    catalog_datasets = "catalog/datasets.csv",
    catalog_problems = "catalog/problems.csv",
    catalog_requests = "requests.csv",
    fetch_status = "catalog/fetch_status.csv",
    native_index = "data/native/index.csv",
    harmonized_index = "data/harmonized_resources.csv",
    report_html = "report/index.html",
    frictionless_descriptor = "datapackage.json"
  )
  fixed <- resources$role %in% names(fixed_paths)
  valid_paths <- all(
    resources$path[fixed] ==
      unname(fixed_paths[resources$role[fixed]])
  )
  observations <- resources$role == "observations"
  expected_observations <- if (identical(profile$timeseries, "parquet")) {
    "data/observations.parquet"
  } else {
    "data/observations.csv"
  }
  valid_paths <- valid_paths && all(
    resources$path[observations] == expected_observations
  )
  native_raw <- resources$role == "native_raw"
  native_table <- resources$role == "native_table"
  valid_native_paths <- all(grepl(
    "^data/raw/[0-9]{6}-[a-f0-9]{64}\\.bin\\z",
    resources$path[native_raw],
    perl = TRUE
  )) &&
    all(grepl(
      "^data/native/[0-9]{6}-[a-f0-9]{64}\\.csv\\z",
      resources$path[native_table],
      perl = TRUE
    ))
  valid_media <- all(
    resources$media_type[resources$format == "csv"] ==
      "text/csv; charset=utf-8"
  ) && all(
    resources$media_type[resources$format == "parquet"] ==
      "application/vnd.apache.parquet"
  ) && identical(
    sum(resources$format == "parquet"),
    if (identical(profile$timeseries, "parquet")) 1L else 0L
  ) && identical(
    sum(resources$role == "report_html"),
    if (has_report) 1L else 0L
  ) && all(
    resources$media_type[resources$role == "report_html"] ==
      "text/html; charset=utf-8"
  ) && identical(
    sum(resources$role == "frictionless_descriptor"),
    if (has_frictionless) 1L else 0L
  ) && all(
    resources$media_type[resources$role == "frictionless_descriptor"] ==
      "application/json"
  )
  order <- order(resources$path, method = "radix")
  total <- sum(resources$bytes)
  valid <- identical(verification$status, "verified") &&
    all(evidence$required) && all(evidence$present) &&
    all(evidence$status == "verified") &&
    nrow(resources) >= length(expected) &&
    nrow(resources) <= .gx_package_resources_max_resources &&
    identical(resources$path, resources$path[order]) &&
    valid_counts && valid_paths && valid_native_paths && valid_media &&
    all(is.finite(resources$bytes)) &&
    all(resources$bytes >= 0) &&
    all(resources$bytes == trunc(resources$bytes)) &&
    all(resources$bytes <= .gx_package_resources_max_resource_bytes) &&
    is.finite(total) && total <= .gx_package_resources_max_total_bytes
  if (!valid) {
    gx_package_load_abort(
      "Stored resources violate the fixed package loading profile.",
      "gx_error_package_load_profile"
    )
  }
  resources
}

gx_package_load_read_impl <- function(root, resource) {
  size <- resource$bytes[[1L]]
  if (!is.finite(size) || size < 0 || size != trunc(size) ||
      size > .gx_package_resources_max_resource_bytes ||
      size > .Machine$integer.max - 1) {
    gx_package_load_abort(
      "A package resource exceeds its exact loading byte ceiling.",
      "gx_error_package_load_budget"
    )
  }
  path <- file.path(root, resource$path[[1L]])
  before <- gx_snapshot_assert_fs_type(path, "file")
  if (!identical(as.numeric(before$size[[1L]]), size)) {
    gx_package_load_abort(
      "A package resource changed before loading.",
      "gx_error_package_load_mutation"
    )
  }
  bytes <- tryCatch(
    readBin(path, what = "raw", n = as.integer(size + 1)),
    warning = function(cnd) NULL,
    error = function(cnd) NULL
  )
  after <- gx_snapshot_assert_fs_type(path, "file")
  if (is.null(bytes) || length(bytes) != size) {
    gx_package_load_abort(
      "A package resource could not be read as exact bounded bytes.",
      "gx_error_package_load_io"
    )
  }
  unchanged <- tryCatch({
    gx_snapshot_assert_same_info(before, after)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  if (!unchanged || !identical(sha256, resource$sha256[[1L]])) {
    gx_package_load_abort(
      "A package resource changed while it was being loaded.",
      "gx_error_package_load_mutation"
    )
  }
  bytes
}

gx_package_load_metadata_impl <- function(resources) {
  list(
    scope = "fixed_package_bytes_v1",
    read_only = TRUE,
    byte_preserving = TRUE,
    resources = unname(as.integer(nrow(resources))),
    csv_resources = unname(as.integer(sum(resources$format == "csv"))),
    parquet_resources = unname(as.integer(sum(
      resources$format == "parquet"
    ))),
    raw_resources = unname(as.integer(sum(resources$format == "raw"))),
    stored_bytes = unname(as.double(sum(resources$bytes))),
    parses_tables = FALSE,
    authenticity = FALSE,
    frictionless = any(resources$role == "frictionless_descriptor"),
    replayable = FALSE
  )
}

gx_package_load_id_impl <- function(
    path,
    stage,
    verification,
    resources,
    metadata) {
  gx_contract_hash(
    c(
      list(
        "mode", "fixed_package_loading",
        "status", "loaded_and_verified",
        "path", path,
        "stage", stage,
        "manifest_sha256", verification$manifest_sha256,
        "resources", metadata$resources,
        "stored_bytes", metadata$stored_bytes
      ),
      as.list(resources$sha256)
    ),
    namespace = "geoconnexr.package-load.v1",
    contract_version = .gx_package_loaded_contract_version
  )
}

gx_package_loaded_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_package_loaded") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_loaded_fields) &&
    identical(
      x$contract_version,
      .gx_package_loaded_contract_version
    ) &&
    identical(x$mode, "fixed_package_loading") &&
    identical(x$status, "loaded_and_verified") &&
    is.character(x$stage) && length(x$stage) == 1L &&
    !is.na(x$stage) && is.null(attributes(x$stage)) &&
    is.character(x$path) && length(x$path) == 1L &&
    !is.na(x$path) && is.null(attributes(x$path)) && nzchar(x$path) &&
    is.character(x$load_id) && length(x$load_id) == 1L &&
    !is.na(x$load_id) && is.null(attributes(x$load_id)) &&
    gx_catalog_is_sha256(x$load_id)
  if (!valid_top) {
    gx_package_load_abort(
      "Loaded package evidence violates its exact top-level contract."
    )
  }
  root <- tryCatch(
    gx_snapshot_root(x$path),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  verification_valid <- tryCatch({
    gx_snapshot_verification_validate_impl(x$verification)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  expected_resources <- if (verification_valid) {
    tryCatch(
      gx_package_load_resource_profile_impl(x$verification),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
  } else {
    NULL
  }
  profile <- if (verification_valid) {
    tryCatch(
      gx_package_owned_manifest_profile_impl(x$verification),
      error = function(cnd) NULL,
      warning = function(cnd) NULL
    )
  } else {
    NULL
  }
  valid_resources <- inherits(x$resources, "tbl_df") &&
    identical(names(x$resources), .gx_package_loaded_resource_columns) &&
    !is.null(expected_resources) &&
    identical(x$resources, expected_resources)
  valid_contents <- is.list(x$contents) &&
    identical(names(x$contents), x$resources$path) &&
    all(vapply(x$contents, function(bytes) {
      is.raw(bytes) && !is.object(bytes) && is.null(attributes(bytes))
    }, logical(1)))
  content_bytes <- if (valid_contents) {
    vapply(x$contents, length, integer(1))
  } else {
    integer()
  }
  content_hashes <- if (valid_contents) {
    vapply(x$contents, digest::digest, character(1),
      algo = "sha256", serialize = FALSE
    )
  } else {
    character()
  }
  expected_metadata <- if (valid_resources) {
    gx_package_load_metadata_impl(x$resources)
  } else {
    NULL
  }
  expected_id <- if (valid_resources && !is.null(profile) &&
      !is.null(expected_metadata)) {
    gx_package_load_id_impl(
      x$path,
      profile$stage,
      x$verification,
      x$resources,
      expected_metadata
    )
  } else {
    NULL
  }
  frictionless_valid <- if (valid_resources && valid_contents &&
      !is.null(profile)) tryCatch({
    gx_package_frictionless_loaded_validate_impl(
      profile, x$resources, x$contents
    )
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE) else FALSE
  valid <- !is.null(root) && identical(root$path, x$path) &&
    verification_valid && !is.null(profile) &&
    identical(x$stage, profile$stage) &&
    valid_resources && valid_contents && frictionless_valid &&
    identical(as.numeric(content_bytes), x$resources$bytes) &&
    identical(unname(content_hashes), x$resources$sha256) &&
    is.list(x$metadata) &&
    identical(names(x$metadata), .gx_package_loaded_metadata_fields) &&
    identical(x$metadata, expected_metadata) &&
    !is.null(expected_id) && identical(x$load_id, expected_id)
  if (!valid) {
    gx_package_load_abort(
      "Loaded package evidence no longer binds its verified resource bytes."
    )
  }
  invisible(x)
}

#' Load verified geoconnexr package bytes offline
#'
#' Loads every declared resource from a package created by [gx_package()] as
#' exact bounded raw bytes. The directory is verified before and after loading,
#' each file is checked for replacement while open, and all loaded bytes are
#' rebound to their manifest SHA-256 values.
#'
#' This boundary is deliberately byte-preserving. CSV and Parquet resources
#' remain raw bytes, a fixed report remains exact HTML bytes, and provider
#' payloads remain opaque raw bytes; it does not
#' reconstruct a live catalog, fetched result, or harmonized result. Loading is
#' read-only, offline, unsigned, and non-replayable. When the fixed package
#' declares Frictionless metadata, loading rederives its descriptor without an
#' external runtime and preserves that compatibility evidence.
#'
#' @param dir Existing package directory created with the fixed public
#'   [gx_package()] profile.
#'
#' @return A validated `gx_package_loaded` object containing the normalized
#'   path, source stage, final verification, path-sorted resource inventory,
#'   exact named resource bytes, fixed scope metadata, and load identity.
#' @export
gx_package_load <- function(dir) {
  tryCatch(
    {
      root <- gx_snapshot_root(dir)
      before <- gx_snapshot_verify(root$path)
      resources <- gx_package_load_resource_profile_impl(before)
      contents <- lapply(
        seq_len(nrow(resources)),
        function(index) {
          gx_package_load_read_impl(
            root$path,
            resources[index, , drop = FALSE]
          )
        }
      )
      names(contents) <- resources$path
      after <- gx_snapshot_verify(root$path)
      stable <- identical(
        before$manifest_sha256,
        after$manifest_sha256
      ) &&
        identical(before$manifest, after$manifest) &&
        identical(before$resources, after$resources)
      if (!stable) {
        gx_package_load_abort(
          "The package changed while its resources were being loaded.",
          "gx_error_package_load_mutation"
        )
      }
      profile <- gx_package_owned_manifest_profile_impl(after)
      gx_package_frictionless_loaded_validate_impl(
        profile, resources, contents
      )
      metadata <- gx_package_load_metadata_impl(resources)
      object <- structure(
        list(
          contract_version = .gx_package_loaded_contract_version,
          mode = "fixed_package_loading",
          status = "loaded_and_verified",
          stage = profile$stage,
          path = root$path,
          verification = after,
          resources = resources,
          contents = contents,
          metadata = metadata,
          load_id = gx_package_load_id_impl(
            root$path,
            profile$stage,
            after,
            resources,
            metadata
          )
        ),
        class = "gx_package_loaded"
      )
      gx_package_loaded_validate_impl(object)
      object
    },
    error = function(cnd) {
      if (inherits(cnd, "gx_error_package_load")) stop(cnd)
      gx_package_load_abort(
        "Package loading failed closed.",
        "gx_error_package_load_contract"
      )
    }
  )
}

#' @export
print.gx_package_loaded <- function(x, ...) {
  gx_package_loaded_validate_impl(x)
  cli::cli_inform(c(
    "<gx_package_loaded>",
    "* Status: {x$status}",
    "* Source stage: {x$stage}",
    paste0(
      "* Resources: ", x$metadata$resources,
      "; exact bytes: ", x$metadata$stored_bytes
    ),
    "* Scope: byte-preserving; read-only; non-replayable",
    "* Assurance: unsigned internal consistency; offline"
  ))
  invisible(x)
}
