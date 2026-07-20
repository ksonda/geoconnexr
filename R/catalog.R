.gx_catalog_contract_version <- "0.1.0"
.gx_catalog_max_sites <- 10000L
.gx_catalog_max_datasets <- 10000L
.gx_catalog_max_problems <- 50000L
.gx_catalog_max_requests <- 10000L
.gx_catalog_max_completeness <- 32L
.gx_catalog_max_completeness_count <- 1073741823
.gx_catalog_max_scalar_bytes <- 16L * 1024L
.gx_catalog_max_text_bytes <- 64L * 1024L^2
.gx_catalog_max_conforms_per_row <- 64L
.gx_catalog_max_conforms_total <- 100000L
.gx_catalog_max_geometry_bytes <- 16L * 1024L^2

.gx_catalog_site_columns <- c(
  "contract_version", "site_uri", "name", "description", "site_type",
  "provider_id", "provider_uri", "provider_name", "provider_url",
  "mainstem_uri", "landing_url", "source_url", "geometry"
)

.gx_catalog_dataset_columns <- c(
  "contract_version", "site_uri", "dataset_id", "distribution_id",
  "variable_id", "dataset_uri", "dataset_name", "dataset_description",
  "temporal_coverage", "temporal_start", "temporal_end", "variable_uri",
  "variable_name", "unit_uri", "unit_label", "measurement_technique",
  "distribution_url", "media_type", "conforms_to", "provider_uri",
  "provider_name", "provider_url", "license", "access_rights", "handler_id",
  "fetchable", "source_url"
)

.gx_catalog_problem_columns <- c(
  "stage", "source_uri", "path", "code", "severity", "message",
  "recoverable", "occurred_at"
)

.gx_catalog_request_columns <- c(
  "request_id", "stage", "method", "canonical_url_redacted",
  "request_hash", "body_hash", "final_url", "response_status",
  "response_media_type", "encoded_bytes", "decoded_bytes", "content_hash",
  "etag", "last_modified", "retrieved_at", "elapsed_ms", "cache_origin",
  "error_code"
)

.gx_catalog_completeness_columns <- c(
  "stage", "status", "truncated", "input_count", "attempted_count",
  "succeeded_count", "failed_count", "skipped_count", "output_count",
  "reason"
)

.gx_catalog_metadata_fields <- c(
  "created_at", "selection", "completeness", "counts", "endpoints",
  "hydrologic_vintage", "source_contracts"
)

.gx_catalog_count_fields <- c(
  "sites", "datasets", "reference_layers", "reference_features",
  "problems", "requests"
)

gx_catalog_abort <- function(message, class = "gx_error_catalog_contract", ...,
                             call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_catalog")),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_catalog_empty_sites <- function() {
  geometry <- sf::st_sfc(sf::st_point(), crs = gx_aoi_crs)[FALSE]
  class(geometry) <- c("sfc_POINT", "sfc")
  sf::st_sf(
    tibble::tibble(
      contract_version = character(), site_uri = character(),
      name = character(), description = character(), site_type = character(),
      provider_id = character(), provider_uri = character(),
      provider_name = character(), provider_url = character(),
      mainstem_uri = character(), landing_url = character(),
      source_url = character()
    ),
    geometry = geometry
  )
}

gx_catalog_empty_datasets <- function() {
  tibble::tibble(
    contract_version = character(), site_uri = character(),
    dataset_id = character(), distribution_id = character(),
    variable_id = character(), dataset_uri = character(),
    dataset_name = character(), dataset_description = character(),
    temporal_coverage = character(),
    temporal_start = as.POSIXct(character(), tz = "UTC"),
    temporal_end = as.POSIXct(character(), tz = "UTC"),
    variable_uri = character(), variable_name = character(),
    unit_uri = character(), unit_label = character(),
    measurement_technique = character(), distribution_url = character(),
    media_type = character(), conforms_to = list(), provider_uri = character(),
    provider_name = character(), provider_url = character(),
    license = character(), access_rights = character(), handler_id = character(),
    fetchable = logical(), source_url = character()
  )
}

gx_catalog_empty_problems <- function() {
  tibble::tibble(
    stage = character(), source_uri = character(), path = character(),
    code = character(), severity = character(), message = character(),
    recoverable = logical(),
    occurred_at = as.POSIXct(character(), tz = "UTC")
  )
}

gx_catalog_empty_requests <- function() {
  tibble::tibble(
    request_id = character(), stage = character(), method = character(),
    canonical_url_redacted = character(), request_hash = character(),
    body_hash = character(), final_url = character(),
    response_status = integer(), response_media_type = character(),
    encoded_bytes = integer(), decoded_bytes = integer(),
    content_hash = character(), etag = character(), last_modified = character(),
    retrieved_at = as.POSIXct(character(), tz = "UTC"),
    elapsed_ms = numeric(), cache_origin = character(), error_code = character()
  )
}

gx_catalog_empty_completeness <- function() {
  tibble::tibble(
    stage = character(), status = character(), truncated = logical(),
    input_count = integer(), attempted_count = integer(),
    succeeded_count = integer(), failed_count = integer(),
    skipped_count = integer(), output_count = integer(), reason = character()
  )
}

gx_catalog_exact_names <- function(x, expected) {
  is.list(x) && !is.null(names(x)) && identical(names(x), expected)
}

gx_catalog_table_rows <- function(x) {
  if (!is.list(x) || !is.data.frame(x) || is.null(attr(x, "row.names"))) {
    return(NULL)
  }
  rows <- tryCatch(nrow(x), error = function(cnd) NULL)
  if (!is.numeric(rows) || length(rows) != 1L || is.na(rows) ||
      !is.finite(rows) || rows < 0 || rows != floor(rows)) {
    return(NULL)
  }
  column_lengths <- tryCatch(
    vapply(x, length, numeric(1)),
    error = function(cnd) NULL
  )
  if (is.null(column_lengths) || any(column_lengths != rows)) return(NULL)
  as.double(rows)
}

gx_catalog_text_valid <- function(x, allow_na = TRUE, nonempty = FALSE) {
  if (!is.character(x) || (!allow_na && anyNA(x))) return(FALSE)
  present <- !is.na(x)
  if (!any(present)) return(TRUE)
  value <- x[present]
  valid_utf8 <- vapply(value, stringi::stri_enc_isutf8, logical(1))
  bytes <- nchar(enc2utf8(value), type = "bytes", allowNA = TRUE)
  controls <- stringi::stri_detect_regex(value, "[\\p{Cc}\\p{Cf}\\p{Cs}]")
  all(valid_utf8) && all(!is.na(bytes) & bytes <= .gx_catalog_max_scalar_bytes) &&
    all(!controls) && (!nonempty || all(nzchar(value)))
}

gx_catalog_assert_text <- function(x, label, allow_na = TRUE,
                                   nonempty = FALSE,
                                   class = "gx_error_catalog_contract") {
  if (!gx_catalog_text_valid(x, allow_na = allow_na, nonempty = nonempty)) {
    over_budget <- is.character(x) && any(
      nchar(enc2utf8(x[!is.na(x)]), type = "bytes") >
        .gx_catalog_max_scalar_bytes
    )
    gx_catalog_abort(
      "Catalog text in {label} violates its UTF-8, control, or byte contract.",
      if (over_budget) "gx_error_catalog_budget" else class
    )
  }
  invisible(x)
}

gx_catalog_is_utc <- function(x) {
  inherits(x, "POSIXct") && identical(attr(x, "tzone"), "UTC") &&
    all(is.na(x) | is.finite(as.numeric(x)))
}

gx_catalog_is_token <- function(x) {
  gx_catalog_text_valid(x, allow_na = FALSE, nonempty = TRUE) &&
    all(grepl("^[a-z][a-z0-9_]{0,63}\\z", x, perl = TRUE))
}

gx_catalog_is_sha256 <- function(x, allow_na = FALSE) {
  is.character(x) && (allow_na || !anyNA(x)) &&
    all(is.na(x) | grepl("^[a-f0-9]{64}\\z", x, perl = TRUE))
}

gx_catalog_parseable_url <- function(value, redacted = FALSE) {
  if (!gx_catalog_text_valid(value, allow_na = FALSE, nonempty = TRUE) ||
      length(value) != 1L ||
      stringi::stri_detect_regex(value, "[\\p{Z}\\p{Cc}\\p{Cf}\\p{Cs}]")) {
    return(FALSE)
  }
  if (redacted && !identical(gx_redact_url(value), value)) return(FALSE)
  parse_value <- sub("[?#]\\[redacted\\].*$", "", value, perl = TRUE)
  canonical <- tryCatch(gx_identity_iri(parse_value), error = function(cnd) NA_character_)
  !is.na(canonical) && identical(canonical, parse_value) &&
    grepl("^https?://", parse_value, ignore.case = TRUE)
}

gx_catalog_assert_urls <- function(x, label, allow_na = TRUE,
                                   redacted = FALSE,
                                   class = "gx_error_catalog_contract") {
  if (!is.character(x) || (!allow_na && anyNA(x))) {
    gx_catalog_abort("Catalog URLs in {label} have an invalid type.", class)
  }
  present <- x[!is.na(x)]
  valid <- vapply(present, gx_catalog_parseable_url, logical(1), redacted = redacted)
  if (!all(valid)) {
    gx_catalog_abort(
      "Catalog URLs in {label} violate the canonical or redaction contract.",
      class
    )
  }
  invisible(x)
}

gx_catalog_assert_iris <- function(x, label, allow_na = TRUE,
                                   class = "gx_error_catalog_contract") {
  if (!is.character(x) || (!allow_na && anyNA(x))) {
    gx_catalog_abort("Catalog IRIs in {label} have an invalid type.", class)
  }
  present <- x[!is.na(x)]
  valid <- vapply(present, function(value) {
    canonical <- tryCatch(gx_identity_iri(value), error = function(cnd) NA_character_)
    !is.na(canonical) && identical(canonical, value)
  }, logical(1))
  if (!all(valid)) {
    gx_catalog_abort("Catalog IRIs in {label} are not canonical.", class)
  }
  invisible(x)
}

gx_catalog_hex_key <- function(x) {
  vapply(x, function(value) {
    if (is.na(value)) return("~")
    paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(value)))), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

gx_catalog_byte_order <- function(...) {
  values <- list(...)
  keys <- lapply(values, function(x) {
    if (inherits(x, "POSIXct")) return(as.numeric(x))
    if (is.character(x)) return(gx_catalog_hex_key(x))
    x
  })
  do.call(order, c(keys, list(na.last = TRUE, method = "radix")))
}

gx_catalog_byte_sorted <- function(x) {
  length(x) < 2L || identical(x, x[gx_catalog_byte_order(x)])
}

gx_catalog_validate_sites <- function(sites) {
  rows <- gx_catalog_table_rows(sites)
  if (!inherits(sites, "sf") || !identical(names(sites), .gx_catalog_site_columns) ||
      is.null(rows) || rows > .gx_catalog_max_sites) {
    gx_catalog_abort(
      "Catalog sites violate their exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_catalog_max_sites) {
        "gx_error_catalog_budget"
      } else {
        "gx_error_catalog_contract"
      }
    )
  }
  character_columns <- setdiff(.gx_catalog_site_columns, "geometry")
  site_data <- sf::st_drop_geometry(sites)
  if (!all(vapply(site_data[character_columns], is.character, logical(1)))) {
    gx_catalog_abort("Catalog site columns have invalid types.")
  }
  lapply(character_columns, function(name) {
    gx_catalog_assert_text(
      sites[[name]], paste0("sites$", name),
      allow_na = !name %in% c("contract_version", "site_uri", "source_url"),
      nonempty = name %in% c("contract_version", "site_uri", "source_url")
    )
  })
  if (any(sites$contract_version != .gx_catalog_contract_version) ||
      anyDuplicated(sites$site_uri)) {
    gx_catalog_abort("Catalog site identities or contract versions are invalid.")
  }
  gx_catalog_assert_urls(sites$site_uri, "sites$site_uri", allow_na = FALSE)
  gx_catalog_assert_urls(sites$source_url, "sites$source_url", allow_na = FALSE)
  gx_catalog_assert_iris(sites$provider_uri, "sites$provider_uri")
  gx_catalog_assert_urls(sites$provider_url, "sites$provider_url")
  gx_catalog_assert_urls(sites$mainstem_uri, "sites$mainstem_uri")
  gx_catalog_assert_urls(sites$landing_url, "sites$landing_url")

  geometry <- sf::st_geometry(sites)
  crs_input <- tryCatch(sf::st_crs(geometry)$input, error = function(cnd) NA_character_)
  if (!inherits(geometry, "sfc_POINT") || !identical(crs_input, gx_aoi_crs)) {
    gx_catalog_abort(
      "Catalog sites require POINT geometry in OGC:CRS84.",
      "gx_error_catalog_geometry"
    )
  }
  empty <- tryCatch(sf::st_is_empty(geometry), error = function(cnd) NULL)
  valid_coordinates <- vapply(seq_along(geometry), function(i) {
    if (!is.null(empty) && isTRUE(empty[[i]])) return(TRUE)
    point <- geometry[[i]]
    coordinates <- unclass(point)
    is.numeric(coordinates) && length(coordinates) == 2L &&
      all(is.finite(coordinates)) &&
      coordinates[[1L]] >= -180 && coordinates[[1L]] <= 180 &&
      coordinates[[2L]] >= -90 && coordinates[[2L]] <= 90
  }, logical(1))
  if (!all(valid_coordinates)) {
    gx_catalog_abort(
      "Catalog site coordinates violate CRS84 point bounds.",
      "gx_error_catalog_geometry"
    )
  }
  wkb <- tryCatch(
    sf::st_as_binary(geometry, EWKB = FALSE, endian = "little", pureR = TRUE),
    error = function(cnd) NULL
  )
  geometry_bytes <- if (is.null(wkb)) Inf else sum(lengths(wkb))
  if (!is.finite(geometry_bytes) || geometry_bytes > .gx_catalog_max_geometry_bytes) {
    gx_catalog_abort(
      "Catalog site geometry exceeds its serialization budget.",
      "gx_error_catalog_budget"
    )
  }
  invisible(sites)
}

gx_catalog_validate_datasets <- function(datasets, sites) {
  rows <- gx_catalog_table_rows(datasets)
  if (!inherits(datasets, "tbl_df") ||
      !identical(names(datasets), .gx_catalog_dataset_columns) ||
      is.null(rows) || rows > .gx_catalog_max_datasets) {
    gx_catalog_abort(
      "Catalog datasets violate their exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_catalog_max_datasets) {
        "gx_error_catalog_budget"
      } else {
        "gx_error_catalog_contract"
      }
    )
  }
  list_columns <- "conforms_to"
  time_columns <- c("temporal_start", "temporal_end")
  logical_columns <- "fetchable"
  character_columns <- setdiff(
    .gx_catalog_dataset_columns,
    c(list_columns, time_columns, logical_columns)
  )
  valid_types <- all(vapply(datasets[character_columns], is.character, logical(1))) &&
    is.list(datasets$conforms_to) &&
    all(vapply(datasets[time_columns], gx_catalog_is_utc, logical(1))) &&
    is.logical(datasets$fetchable)
  if (!valid_types || anyNA(datasets$contract_version) ||
      any(datasets$contract_version != .gx_catalog_contract_version) ||
      anyNA(datasets$site_uri) || anyNA(datasets$fetchable)) {
    gx_catalog_abort("Catalog dataset columns have invalid types or required values.")
  }
  lapply(character_columns, function(name) {
    gx_catalog_assert_text(
      datasets[[name]], paste0("datasets$", name),
      allow_na = !name %in% c("contract_version", "site_uri", "handler_id", "source_url"),
      nonempty = name %in% c("contract_version", "site_uri", "handler_id", "source_url")
    )
  })
  if (any(!datasets$site_uri %in% sites$site_uri)) {
    gx_catalog_abort("Every catalog dataset must reference one catalog site.")
  }
  if (!gx_catalog_is_sha256(datasets$dataset_id, allow_na = TRUE) ||
      !gx_catalog_is_sha256(datasets$distribution_id, allow_na = TRUE)) {
    gx_catalog_abort("Present catalog dataset and distribution IDs must be SHA-256 values.")
  }
  variable_valid <- is.na(datasets$variable_id) |
    grepl("^[a-f0-9]{64}\\z", datasets$variable_id, perl = TRUE)
  iri_variables <- which(!variable_valid)
  if (length(iri_variables)) {
    canonical <- vapply(
      datasets$variable_id[iri_variables], gx_identity_iri, character(1)
    )
    variable_valid[iri_variables] <- !is.na(canonical) &
      canonical == datasets$variable_id[iri_variables]
  }
  if (!all(variable_valid)) {
    gx_catalog_abort("Present catalog variable IDs must be canonical IRIs or SHA-256 values.")
  }
  identity_key <- paste(
    ifelse(is.na(datasets$dataset_id), "<NA>", datasets$dataset_id),
    ifelse(is.na(datasets$distribution_id), "<NA>", datasets$distribution_id),
    ifelse(is.na(datasets$variable_id), "<NA>", datasets$variable_id),
    sep = "\r"
  )
  if (anyDuplicated(identity_key)) {
    gx_catalog_abort("Catalog dataset identity triples must be unique.")
  }
  gx_catalog_assert_iris(datasets$dataset_uri, "datasets$dataset_uri")
  gx_catalog_assert_iris(datasets$variable_uri, "datasets$variable_uri")
  gx_catalog_assert_iris(datasets$unit_uri, "datasets$unit_uri")
  gx_catalog_assert_iris(datasets$provider_uri, "datasets$provider_uri")
  gx_catalog_assert_urls(datasets$distribution_url, "datasets$distribution_url")
  gx_catalog_assert_urls(datasets$provider_url, "datasets$provider_url")
  gx_catalog_assert_urls(datasets$source_url, "datasets$source_url", allow_na = FALSE)
  temporal_bad <- !is.na(datasets$temporal_start) & !is.na(datasets$temporal_end) &
    datasets$temporal_start > datasets$temporal_end
  if (any(temporal_bad)) {
    gx_catalog_abort("Catalog temporal intervals must be ordered.")
  }
  conforms_lengths <- lengths(datasets$conforms_to)
  valid_conforms <- all(conforms_lengths <= .gx_catalog_max_conforms_per_row) &&
    sum(as.double(conforms_lengths)) <= .gx_catalog_max_conforms_total &&
    all(vapply(datasets$conforms_to, function(value) {
      is.character(value) && !anyNA(value) && !anyDuplicated(value) &&
        gx_catalog_text_valid(value, allow_na = FALSE, nonempty = TRUE) &&
        gx_catalog_byte_sorted(value) &&
        all(vapply(value, function(iri) {
          canonical <- gx_identity_iri(iri)
          !is.na(canonical) && identical(canonical, iri)
        }, logical(1)))
    }, logical(1)))
  if (!valid_conforms) {
    gx_catalog_abort(
      "Catalog conforms-to values violate their type, order, or budget contract.",
      if (any(conforms_lengths > .gx_catalog_max_conforms_per_row) ||
          sum(as.double(conforms_lengths)) > .gx_catalog_max_conforms_total) {
        "gx_error_catalog_budget"
      } else {
        "gx_error_catalog_contract"
      }
    )
  }
  fetchable_bad <- datasets$fetchable & (
    is.na(datasets$dataset_id) | is.na(datasets$distribution_id) |
      is.na(datasets$distribution_url) | datasets$handler_id == "unknown"
  )
  if (any(fetchable_bad)) {
    gx_catalog_abort("Fetchable catalog rows require stable distribution and handler identities.")
  }
  invisible(datasets)
}

gx_catalog_validate_problems <- function(problems) {
  rows <- gx_catalog_table_rows(problems)
  if (!inherits(problems, "tbl_df") ||
      !identical(names(problems), .gx_catalog_problem_columns) ||
      is.null(rows) || rows > .gx_catalog_max_problems) {
    gx_catalog_abort(
      "Catalog problems violate their exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_catalog_max_problems) {
        "gx_error_catalog_budget"
      } else {
        "gx_error_catalog_contract"
      }
    )
  }
  character_columns <- setdiff(
    .gx_catalog_problem_columns, c("recoverable", "occurred_at")
  )
  valid_types <- all(vapply(problems[character_columns], is.character, logical(1))) &&
    is.logical(problems$recoverable) && gx_catalog_is_utc(problems$occurred_at)
  if (!valid_types || anyNA(problems$recoverable) ||
      any(!problems$recoverable) || anyNA(problems$occurred_at) ||
      !gx_catalog_is_token(problems$stage) || !gx_catalog_is_token(problems$code) ||
      anyNA(problems$severity) ||
      any(!problems$severity %in% c("info", "warning", "error"))) {
    gx_catalog_abort("Catalog problem rows violate their type or recoverability contract.")
  }
  lapply(character_columns, function(name) {
    gx_catalog_assert_text(
      problems[[name]], paste0("problems$", name),
      allow_na = identical(name, "source_uri"),
      nonempty = name %in% c("stage", "code", "severity", "message")
    )
  })
  gx_catalog_assert_urls(
    problems$source_uri, "problems$source_uri", redacted = TRUE
  )
  invisible(problems)
}

gx_catalog_validate_requests <- function(requests) {
  rows <- gx_catalog_table_rows(requests)
  if (!inherits(requests, "tbl_df") ||
      !identical(names(requests), .gx_catalog_request_columns) ||
      is.null(rows) || rows > .gx_catalog_max_requests) {
    gx_catalog_abort(
      "Catalog requests violate their exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_catalog_max_requests) {
        "gx_error_catalog_budget"
      } else {
        "gx_error_catalog_contract"
      }
    )
  }
  integer_columns <- c("response_status", "encoded_bytes", "decoded_bytes")
  character_columns <- setdiff(
    .gx_catalog_request_columns,
    c(integer_columns, "retrieved_at", "elapsed_ms")
  )
  valid_types <- all(vapply(requests[character_columns], is.character, logical(1))) &&
    all(vapply(requests[integer_columns], is.integer, logical(1))) &&
    gx_catalog_is_utc(requests$retrieved_at) && is.numeric(requests$elapsed_ms)
  if (!valid_types || anyNA(requests$request_id) || anyDuplicated(requests$request_id) ||
      !gx_catalog_is_token(requests$stage) || anyNA(requests$method) ||
      any(!requests$method %in% c("GET", "HEAD", "POST")) ||
      !gx_catalog_is_sha256(requests$request_hash) ||
      !gx_catalog_is_sha256(requests$body_hash, allow_na = TRUE) ||
      !gx_catalog_is_sha256(requests$content_hash, allow_na = TRUE) ||
      anyNA(requests$retrieved_at)) {
    gx_catalog_abort("Catalog request rows violate identity, method, hash, or time contracts.")
  }
  lapply(character_columns, function(name) {
    gx_catalog_assert_text(
      requests[[name]], paste0("requests$", name),
      allow_na = !name %in% c(
        "request_id", "stage", "method", "canonical_url_redacted",
        "request_hash", "cache_origin"
      ),
      nonempty = name %in% c(
        "request_id", "stage", "method", "canonical_url_redacted",
        "request_hash", "cache_origin"
      )
    )
  })
  gx_catalog_assert_urls(
    requests$canonical_url_redacted,
    "requests$canonical_url_redacted",
    allow_na = FALSE,
    redacted = TRUE
  )
  gx_catalog_assert_urls(
    requests$final_url, "requests$final_url", redacted = TRUE
  )
  status_bad <- !is.na(requests$response_status) &
    (requests$response_status < 100L | requests$response_status > 599L)
  byte_bad <- vapply(requests[integer_columns[-1L]], function(value) {
    any(!is.na(value) & (value < 0L | value > 1024L^3))
  }, logical(1))
  elapsed_bad <- !is.na(requests$elapsed_ms) &
    (!is.finite(requests$elapsed_ms) | requests$elapsed_ms < 0)
  if (any(status_bad) || any(byte_bad) || any(elapsed_bad) ||
      anyNA(requests$cache_origin) ||
      any(!requests$cache_origin %in% c("network", "fresh_cache", "offline_cache"))) {
    gx_catalog_abort("Catalog request response fields violate their bounded contract.")
  }
  invisible(requests)
}

gx_catalog_validate_completeness <- function(completeness) {
  rows <- gx_catalog_table_rows(completeness)
  if (!inherits(completeness, "tbl_df") ||
      !identical(names(completeness), .gx_catalog_completeness_columns) ||
      is.null(rows) || rows > .gx_catalog_max_completeness) {
    gx_catalog_abort(
      "Catalog completeness violates its exact table shape or row budget.",
      if (!is.null(rows) && rows > .gx_catalog_max_completeness) {
        "gx_error_catalog_budget"
      } else {
        "gx_error_catalog_completeness"
      }
    )
  }
  count_columns <- c(
    "input_count", "attempted_count", "succeeded_count", "failed_count",
    "skipped_count", "output_count"
  )
  valid_types <- is.character(completeness$stage) &&
    is.character(completeness$status) && is.logical(completeness$truncated) &&
    all(vapply(completeness[count_columns], is.integer, logical(1))) &&
    is.character(completeness$reason)
  if (!valid_types || !gx_catalog_is_token(completeness$stage) ||
      anyDuplicated(completeness$stage) || anyNA(completeness$status) ||
      any(!completeness$status %in% c("complete", "partial", "not_run", "unknown")) ||
      anyNA(completeness$truncated) ||
      any(vapply(completeness[count_columns], anyNA, logical(1))) ||
      any(vapply(completeness[count_columns], function(x) {
        values <- as.double(x)
        any(values < 0 | values > .gx_catalog_max_completeness_count)
      }, logical(1)))) {
    gx_catalog_abort(
      "Catalog completeness rows contain invalid types or values.",
      "gx_error_catalog_completeness"
    )
  }
  gx_catalog_assert_text(
    completeness$reason, "completeness$reason", allow_na = TRUE,
    class = "gx_error_catalog_completeness"
  )
  input_count <- as.double(completeness$input_count)
  attempted_count <- as.double(completeness$attempted_count)
  succeeded_count <- as.double(completeness$succeeded_count)
  failed_count <- as.double(completeness$failed_count)
  skipped_count <- as.double(completeness$skipped_count)
  if (any(input_count != attempted_count + skipped_count) ||
      any(attempted_count != succeeded_count + failed_count)) {
    gx_catalog_abort(
      "Catalog completeness counts do not reconcile.",
      "gx_error_catalog_completeness"
    )
  }
  for (i in seq_len(nrow(completeness))) {
    row <- completeness[i, , drop = FALSE]
    reason_present <- !is.na(row$reason[[1L]]) && nzchar(row$reason[[1L]])
    if (identical(row$status[[1L]], "complete")) {
      valid <- !row$truncated[[1L]] && row$failed_count[[1L]] == 0L &&
        row$skipped_count[[1L]] == 0L && !reason_present
    } else if (identical(row$status[[1L]], "partial")) {
      valid <- reason_present && (
        row$truncated[[1L]] || row$failed_count[[1L]] > 0L ||
          row$skipped_count[[1L]] > 0L
      )
    } else if (identical(row$status[[1L]], "not_run")) {
      valid <- reason_present && !row$truncated[[1L]] &&
        all(unlist(row[count_columns], use.names = FALSE) == 0L)
    } else {
      valid <- reason_present
    }
    if (!isTRUE(valid)) {
      gx_catalog_abort(
        "A catalog completeness status disagrees with its counts or reason.",
        "gx_error_catalog_completeness"
      )
    }
  }
  invisible(completeness)
}

gx_catalog_text_total <- function(x, limit = Inf) {
  total <- 0
  add <- function(values) {
    if (!is.character(values)) return(TRUE)
    for (i in seq_along(values)) {
      value <- values[[i]]
      if (is.na(value)) next
      bytes <- suppressWarnings(tryCatch(
        nchar(enc2utf8(value), type = "bytes", allowNA = TRUE),
        error = function(cnd) NA_integer_
      ))
      if (length(bytes) != 1L || is.na(bytes)) next
      bytes <- as.double(bytes)
      if (!is.finite(bytes) || bytes > limit - total) {
        total <<- Inf
        return(FALSE)
      }
      total <<- total + bytes
    }
    TRUE
  }
  add_character_fields <- function(value) {
    if (!is.list(value)) return(TRUE)
    for (i in seq_along(value)) {
      if (is.character(value[[i]]) && !add(value[[i]])) return(FALSE)
    }
    TRUE
  }

  for (component in c("sites", "datasets", "problems", "requests")) {
    if (!add_character_fields(x[[component]])) return(Inf)
  }
  conforms <- if (is.list(x$datasets)) {
    x$datasets[["conforms_to"]]
  } else {
    NULL
  }
  if (is.list(conforms)) {
    for (value in conforms) {
      if (is.character(value) && !add(value)) return(Inf)
    }
  }

  metadata <- x$metadata
  if (!is.list(metadata)) return(total)
  if (!add_character_fields(metadata$completeness)) return(Inf)
  selection <- metadata$selection
  if (is.list(selection)) {
    for (field in c("include", "providers", "variables")) {
      if (!add(selection[[field]])) return(Inf)
    }
  }
  endpoints <- metadata$endpoints
  if (!add(names(endpoints)) || !add(endpoints)) return(Inf)
  vintage <- metadata$hydrologic_vintage
  if (is.list(vintage)) {
    for (value in vintage) {
      if (!add(value)) return(Inf)
    }
  }
  contracts <- metadata$source_contracts
  if (!add(names(contracts)) || !add(contracts)) return(Inf)
  total
}

gx_catalog_assert_text_budget <- function(x) {
  total <- gx_catalog_text_total(x, limit = .gx_catalog_max_text_bytes)
  if (!is.finite(total) || total > .gx_catalog_max_text_bytes) {
    gx_catalog_abort(
      "Catalog text exceeds its aggregate byte budget.",
      "gx_error_catalog_budget"
    )
  }
  invisible(total)
}

gx_catalog_validate_metadata <- function(metadata, x) {
  if (!gx_catalog_exact_names(metadata, .gx_catalog_metadata_fields)) {
    gx_catalog_abort("Catalog metadata has an invalid exact shape.")
  }
  if (!gx_catalog_is_utc(metadata$created_at) ||
      length(metadata$created_at) != 1L || is.na(metadata$created_at)) {
    gx_catalog_abort("Catalog creation time must be one UTC timestamp.")
  }
  selection <- metadata$selection
  if (!gx_catalog_exact_names(selection, c("include", "providers", "variables"))) {
    gx_catalog_abort("Catalog selection metadata has an invalid exact shape.")
  }
  include_order <- c("sites", "datasets", "reference")
  include <- selection$include
  valid_include <- is.character(include) && length(include) > 0L &&
    !anyNA(include) && !anyDuplicated(include) && all(include %in% include_order) &&
    identical(include, include_order[include_order %in% include]) &&
    (!("datasets" %in% include) || "sites" %in% include)
  valid_filters <- all(vapply(
    selection[c("providers", "variables")],
    function(value) {
      is.character(value) && !anyNA(value) && !anyDuplicated(value) &&
        gx_catalog_text_valid(value, allow_na = FALSE, nonempty = TRUE) &&
        gx_catalog_byte_sorted(value)
    },
    logical(1)
  ))
  if (!valid_include || !valid_filters) {
    gx_catalog_abort("Catalog selection values are invalid or noncanonical.")
  }
  gx_catalog_validate_completeness(metadata$completeness)
  completeness <- metadata$completeness
  if (!all(include %in% completeness$stage)) {
    gx_catalog_abort(
      "Every selected catalog component requires completeness metadata.",
      "gx_error_catalog_completeness"
    )
  }
  if (any(!unique(x$problems$stage) %in% completeness$stage)) {
    gx_catalog_abort(
      "Every catalog problem stage requires completeness metadata.",
      "gx_error_catalog_completeness"
    )
  }
  expected_output <- c(
    sites = nrow(x$sites), datasets = nrow(x$datasets), reference = 0L
  )
  for (stage in intersect(names(expected_output), completeness$stage)) {
    if (completeness$output_count[match(stage, completeness$stage)] !=
        expected_output[[stage]]) {
      gx_catalog_abort(
        "Catalog completeness output counts disagree with catalog tables.",
        "gx_error_catalog_completeness"
      )
    }
  }
  error_stages <- unique(x$problems$stage[x$problems$severity == "error"])
  if (length(error_stages)) {
    positions <- match(error_stages, completeness$stage)
    if (anyNA(positions) || any(completeness$status[positions] == "complete")) {
      gx_catalog_abort(
        "Error-level catalog problems require visible incomplete stage metadata.",
        "gx_error_catalog_completeness"
      )
    }
  }

  counts <- metadata$counts
  if (!gx_catalog_exact_names(counts, .gx_catalog_count_fields) ||
      !all(vapply(counts, function(value) {
        is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
      }, logical(1)))) {
    gx_catalog_abort("Catalog component counts have an invalid exact shape.")
  }
  actual_counts <- list(
    sites = as.integer(nrow(x$sites)),
    datasets = as.integer(nrow(x$datasets)),
    reference_layers = 0L,
    reference_features = 0L,
    problems = as.integer(nrow(x$problems)),
    requests = as.integer(nrow(x$requests))
  )
  if (!identical(counts, actual_counts)) {
    gx_catalog_abort("Catalog metadata counts disagree with catalog components.")
  }
  endpoints <- metadata$endpoints
  endpoint_names <- names(endpoints)
  if (!is.character(endpoints) || !length(endpoints) || anyNA(endpoints) ||
      is.null(endpoint_names) || anyNA(endpoint_names) || anyDuplicated(endpoint_names) ||
      !gx_catalog_is_token(endpoint_names) || !gx_catalog_byte_sorted(endpoint_names)) {
    gx_catalog_abort("Catalog endpoints have invalid names or values.")
  }
  gx_catalog_assert_urls(
    endpoints, "metadata$endpoints", allow_na = FALSE, redacted = TRUE
  )
  if (any(grepl("[?#]\\[redacted\\]", endpoints, perl = TRUE))) {
    gx_catalog_abort("Catalog endpoints cannot contain redaction placeholders.")
  }
  vintage <- metadata$hydrologic_vintage
  if (!gx_catalog_exact_names(
    vintage, c("reference_collection", "vintage", "migration_policy")
  ) || !all(vapply(vintage, function(value) {
    is.character(value) && length(value) == 1L
  }, logical(1))) || is.na(vintage$migration_policy) ||
      !gx_catalog_text_valid(unlist(vintage, use.names = FALSE), nonempty = FALSE) ||
      !nzchar(vintage$migration_policy)) {
    gx_catalog_abort("Catalog hydrologic-vintage metadata is invalid.")
  }
  contracts <- metadata$source_contracts
  contract_names <- names(contracts)
  if (!is.character(contracts) || anyNA(contracts) || !length(contracts) ||
      is.null(contract_names) || anyNA(contract_names) || anyDuplicated(contract_names) ||
      !gx_catalog_is_token(contract_names) || !gx_catalog_byte_sorted(contract_names) ||
      !gx_catalog_text_valid(contracts, allow_na = FALSE, nonempty = TRUE) ||
      !all(c("aoi", "catalog") %in% contract_names) ||
      !identical(unname(contracts[["aoi"]]), gx_aoi_contract_version) ||
      !identical(unname(contracts[["catalog"]]), .gx_catalog_contract_version)) {
    gx_catalog_abort("Catalog source-contract metadata is invalid.")
  }
  invisible(metadata)
}

gx_catalog_validate_impl <- function(x) {
  expected <- c(
    "contract_version", "aoi", "sites", "datasets", "reference",
    "problems", "requests", "metadata"
  )
  if (!is.list(x) || !identical(class(x), "gx_catalog") ||
      !identical(names(x), expected) ||
      !identical(x$contract_version, .gx_catalog_contract_version)) {
    gx_catalog_abort("Catalog objects violate their exact top-level contract.")
  }
  gx_catalog_assert_text_budget(x)
  valid_aoi <- tryCatch({
    gx_validate_aoi(x$aoi)
    TRUE
  }, error = function(cnd) FALSE)
  if (!valid_aoi) {
    gx_catalog_abort("Catalog AOI identity violates its contract.")
  }
  if (!identical(x$reference, list())) {
    gx_catalog_abort(
      "Catalog contract 0.1.0 accepts only an empty reference-layer list.",
      "gx_error_catalog_input"
    )
  }
  gx_catalog_validate_sites(x$sites)
  gx_catalog_validate_datasets(x$datasets, x$sites)
  gx_catalog_validate_problems(x$problems)
  gx_catalog_validate_requests(x$requests)
  gx_catalog_validate_metadata(x$metadata, x)
  gx_catalog_assert_text_budget(x)
  invisible(x)
}

gx_catalog_new_impl <- function(
    aoi,
    sites = gx_catalog_empty_sites(),
    datasets = gx_catalog_empty_datasets(),
    reference = list(),
    problems = gx_catalog_empty_problems(),
    requests = gx_catalog_empty_requests(),
    metadata) {
  object <- structure(
    list(
      contract_version = .gx_catalog_contract_version,
      aoi = aoi,
      sites = sites,
      datasets = datasets,
      reference = reference,
      problems = problems,
      requests = requests,
      metadata = metadata
    ),
    class = "gx_catalog"
  )
  gx_catalog_assert_text_budget(object)
  valid_aoi <- tryCatch({
    gx_validate_aoi(aoi)
    TRUE
  }, error = function(cnd) FALSE)
  if (!valid_aoi) {
    gx_catalog_abort(
      "Catalog construction requires a valid AOI object.",
      "gx_error_catalog_input"
    )
  }
  gx_catalog_validate_impl(object)
  object
}

gx_catalog_problems_from_diagnostics_impl <- function(
    diagnostics,
    stage,
    source_uri = NA_character_,
    occurred_at = gx_now()) {
  if (!gx_is_diagnostics(diagnostics) || !gx_catalog_is_token(stage) ||
      length(stage) != 1L || !is.character(source_uri) ||
      length(source_uri) != 1L || !inherits(occurred_at, "POSIXct") ||
      length(occurred_at) != 1L || is.na(occurred_at)) {
    gx_catalog_abort(
      "Diagnostics cannot be converted to catalog problems.",
      "gx_error_catalog_input"
    )
  }
  gx_catalog_assert_urls(
    source_uri, "source_uri", redacted = TRUE,
    class = "gx_error_catalog_input"
  )
  out <- tibble::tibble(
    stage = rep(stage, nrow(diagnostics)),
    source_uri = rep(source_uri, nrow(diagnostics)),
    path = diagnostics$path,
    code = diagnostics$code,
    severity = diagnostics$severity,
    message = diagnostics$message,
    recoverable = diagnostics$recoverable,
    occurred_at = rep(as.POSIXct(occurred_at, tz = "UTC"), nrow(diagnostics))
  )
  gx_catalog_validate_problems(out)
  out
}

gx_catalog_encode_conforms <- function(value) {
  as.character(jsonlite::toJSON(
    unname(value), auto_unbox = FALSE, null = "null", na = "null",
    pretty = FALSE
  ))
}

gx_catalog_redact_uri <- function(x) {
  out <- x
  redact <- !is.na(out) & grepl(
    "^[A-Za-z][A-Za-z0-9+.-]*:", out, perl = TRUE
  )
  out[redact] <- vapply(out[redact], gx_redact_url, character(1))
  out
}

# Retain the pre-hardening internal seam for the catalog-only snapshot writer.
gx_catalog_identity_sha256 <- function(x, namespace) {
  vapply(x, function(value) {
    if (is.na(value)) return(NA_character_)
    gx_contract_hash(
      list(value), namespace = namespace,
      contract_version = .gx_catalog_contract_version
    )
  }, character(1), USE.NAMES = FALSE)
}

gx_catalog_export_views_impl <- function(x) {
  gx_catalog_validate_impl(x)
  sites <- x$sites[gx_catalog_byte_order(x$sites$site_uri), , drop = FALSE]
  site_geometry <- sf::st_as_text(sf::st_geometry(sites), digits = 17)
  sites <- tibble::as_tibble(sf::st_drop_geometry(sites))
  sites <- tibble::add_column(
    sites,
    site_uri_sha256 = gx_catalog_identity_sha256(
      sites$site_uri, "geoconnexr.catalog.site-uri.v1"
    ),
    .after = "site_uri"
  )
  for (field in c(
    "site_uri", "site_type", "provider_uri", "provider_url", "mainstem_uri",
    "landing_url", "source_url"
  )) {
    sites[[field]] <- gx_catalog_redact_uri(sites[[field]])
  }
  sites$geometry_wkt <- unname(site_geometry)
  dataset_order <- gx_catalog_byte_order(
    x$datasets$site_uri, x$datasets$dataset_id, x$datasets$dataset_uri,
    x$datasets$distribution_id, x$datasets$distribution_url,
    x$datasets$variable_id, x$datasets$variable_uri,
    x$datasets$variable_name, x$datasets$source_url
  )
  datasets <- x$datasets[dataset_order, , drop = FALSE]
  datasets <- tibble::add_column(
    datasets,
    site_uri_sha256 = gx_catalog_identity_sha256(
      datasets$site_uri, "geoconnexr.catalog.site-uri.v1"
    ),
    .after = "site_uri"
  )
  datasets <- tibble::add_column(
    datasets,
    variable_id_sha256 = gx_catalog_identity_sha256(
      datasets$variable_id, "geoconnexr.catalog.variable-id.v1"
    ),
    .after = "variable_id"
  )
  for (field in c(
    "site_uri", "variable_id", "dataset_uri", "variable_uri", "unit_uri",
    "measurement_technique", "distribution_url", "provider_uri",
    "provider_url", "license", "access_rights", "source_url"
  )) {
    datasets[[field]] <- gx_catalog_redact_uri(datasets[[field]])
  }
  datasets$conforms_to <- vapply(
    lapply(datasets$conforms_to, gx_catalog_redact_uri),
    gx_catalog_encode_conforms,
    character(1)
  )
  problem_order <- gx_catalog_byte_order(
    x$problems$occurred_at, x$problems$stage, x$problems$source_uri,
    x$problems$code, x$problems$path, x$problems$severity,
    x$problems$message, x$problems$recoverable
  )
  problems <- x$problems[problem_order, , drop = FALSE]
  request_order <- gx_catalog_byte_order(
    x$requests$retrieved_at, x$requests$request_id
  )
  requests <- x$requests[request_order, , drop = FALSE]
  completeness <- x$metadata$completeness[
    gx_catalog_byte_order(x$metadata$completeness$stage), , drop = FALSE
  ]
  list(
    sites = sites,
    datasets = datasets,
    reference = list(),
    problems = problems,
    requests = requests,
    completeness = completeness
  )
}

#' @export
print.gx_catalog <- function(x, ...) {
  gx_catalog_validate_impl(x)
  selected <- paste(x$metadata$selection$include, collapse = ", ")
  complete <- all(
    x$metadata$completeness$status[
      match(x$metadata$selection$include, x$metadata$completeness$stage)
    ] == "complete"
  )
  cli::cli_inform(c(
    "<gx_catalog>",
    "* AOI: {x$aoi$type} {x$aoi$id}",
    "* Sites: {nrow(x$sites)}; datasets: {nrow(x$datasets)}",
    "* Problems: {nrow(x$problems)}; requests: {nrow(x$requests)}",
    "* Include: {selected}; complete: {complete}"
  ))
  invisible(x)
}
