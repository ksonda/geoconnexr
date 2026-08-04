.gx_target_units_contract_version <- "0.1.0"
.gx_csv_mapping_contract_version <- "0.1.0"
.gx_feature_mapping_contract_version <- "0.1.0"
.gx_harmonized_contract_version <- "0.5.0"
.gx_harmonized_max_observations <- 1000000L

.gx_target_unit_dimensions <- c(
  "thermodynamic-temperature", "length", "volume-flow-rate"
)

.gx_wqp_timezone_codes <- c(
  "ADT", "AKDT", "AKST", "AST", "CDT", "CEST", "CET", "CST", "EDT",
  "EST", "GMT", "GST", "HADT", "HAST", "KST", "MDT", "MST", "NDT",
  "NST", "PDT", "PST", "SST", "UTC"
)

.gx_harmonized_fields <- c(
  "contract_version", "fetched", "target_units", "csv_mappings",
  "feature_mappings", "observations", "resources", "metadata"
)

.gx_csv_mapping_fields <- c(
  "contract_version", "distribution_id", "columns", "datetime_format",
  "missing_values", "mapping_id"
)

.gx_csv_mapping_column_fields <- c(
  "datetime", "value", "unit", "qualifier"
)

.gx_feature_mapping_fields <- c(
  "contract_version", "distribution_id", "properties", "datetime_format",
  "missing_values", "mapping_id"
)

.gx_feature_mapping_property_fields <- c(
  "datetime", "value", "unit", "qualifier"
)

.gx_feature_mapping_reserved_properties <- c(
  "contract_version", "feature_id", "id", "geometry"
)

.gx_harmonized_observation_columns <- c(
  "contract_version", "observation_index", "result_id", "native_row",
  "site_uri", "dataset_id", "distribution_id", "handler_id", "variable_id",
  "variable_uri", "variable_name", "datetime", "value", "unit_uri",
  "unit_label", "original_value", "original_unit_uri", "original_unit_label",
  "qualifier", "harmonized", "conversion_rule_id", "status"
)

.gx_harmonized_resource_columns <- c(
  "contract_version", "result_index", "result_id", "distribution_id",
  "handler_id", "payload_class", "native_rows", "native_columns",
  "timeseries", "observation_count", "status"
)

.gx_harmonized_metadata_fields <- c(
  "scope", "offline", "raw_payloads_preserved", "timestamps_utc",
  "duplicates_preserved", "max_observations", "target_asset_sha256",
  "wqp_timezone_asset_sha256", "counts", "limitations"
)

.gx_harmonized_count_fields <- c(
  "resources", "timeseries_resources", "native_only_resources",
  "observations", "harmonized", "unchanged"
)

gx_harmonize_abort <- function(
    message,
    class = "gx_error_harmonize_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_harmonize")),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_target_unit_asset_impl <- function() {
  path <- file.path(gx_asset_dir("vocab"), "target-units-v1.csv")
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  asset_sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  units <- utils::read.csv(
    path,
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  required <- c(
    "dimension", "unit_uri", "unit_label", "preferred", "source_uri",
    "review_date", "status"
  )
  valid <- identical(names(units), required) && nrow(units) > 0L &&
    !anyNA(units) && all(nzchar(as.matrix(units))) &&
    !anyDuplicated(units[c("dimension", "unit_uri")]) &&
    all(units$dimension %in% .gx_target_unit_dimensions) &&
    all(units$preferred %in% c("true", "false")) &&
    all(units$status == "reviewed") &&
    all(vapply(units$unit_uri, gx_is_http_uri, logical(1))) &&
    all(vapply(units$source_uri, gx_is_http_uri, logical(1)))
  if (!valid) {
    gx_harmonize_abort(
      "The reviewed target-unit asset has an invalid contract.",
      "gx_error_target_units_asset"
    )
  }
  preferred <- units$preferred == "true"
  if (!identical(
    unname(tabulate(
      match(units$dimension[preferred], .gx_target_unit_dimensions),
      nbins = length(.gx_target_unit_dimensions)
    )),
    rep.int(1L, length(.gx_target_unit_dimensions))
  )) {
    gx_harmonize_abort(
      "The reviewed target-unit asset must select one default per dimension.",
      "gx_error_target_units_asset"
    )
  }

  rules <- gx_unit_conversions()
  rule_units <- rbind(
    data.frame(
      dimension = rules$dimension,
      unit_uri = rules$from_unit_uri,
      stringsAsFactors = FALSE
    ),
    data.frame(
      dimension = rules$dimension,
      unit_uri = rules$to_unit_uri,
      stringsAsFactors = FALSE
    )
  )
  rule_units <- unique(rule_units)
  asset_keys <- paste(units$dimension, units$unit_uri, sep = "\r")
  rule_keys <- paste(rule_units$dimension, rule_units$unit_uri, sep = "\r")
  if (!all(rule_keys %in% asset_keys)) {
    gx_harmonize_abort(
      "The target-unit asset does not cover every reviewed conversion unit.",
      "gx_error_target_units_asset"
    )
  }

  units$preferred <- preferred
  list(
    units = tibble::as_tibble(units),
    asset_sha256 = unname(asset_sha256)
  )
}

gx_wqp_timezone_asset_impl <- function() {
  path <- file.path(gx_asset_dir("vocab"), "wqp-timezones-v1.csv")
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  asset_sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  zones <- utils::read.csv(
    path,
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  required <- c(
    "code", "name", "offset_minutes", "source_uri", "review_date", "status"
  )
  offsets <- suppressWarnings(as.numeric(zones$offset_minutes))
  valid <- identical(names(zones), required) &&
    identical(unname(zones$code), .gx_wqp_timezone_codes) &&
    !anyNA(zones) && all(nzchar(as.matrix(zones))) &&
    !anyDuplicated(zones$code) &&
    all(grepl("^[A-Z]{2,4}$", zones$code)) &&
    length(offsets) == nrow(zones) && !anyNA(offsets) &&
    all(is.finite(offsets)) && all(offsets == floor(offsets)) &&
    all(offsets >= -12L * 60L & offsets <= 14L * 60L) &&
    all(vapply(zones$source_uri, gx_is_http_uri, logical(1))) &&
    all(zones$status == "reviewed")
  if (!valid) {
    gx_harmonize_abort(
      "The reviewed WQP timezone asset has an invalid contract.",
      "gx_error_harmonize_timezone_asset"
    )
  }
  zones$offset_minutes <- as.integer(offsets)
  list(
    zones = tibble::as_tibble(zones),
    asset_sha256 = unname(asset_sha256)
  )
}

gx_target_units_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_target_units") &&
    identical(names(x), c("contract_version", "units", "asset_sha256")) &&
    identical(x$contract_version, .gx_target_units_contract_version) &&
    is.character(x$asset_sha256) && length(x$asset_sha256) == 1L &&
    !is.na(x$asset_sha256) && grepl("^[0-9a-f]{64}$", x$asset_sha256)
  if (!valid_top) {
    gx_harmonize_abort(
      "Target units violate their exact top-level contract.",
      "gx_error_target_units_contract"
    )
  }
  asset <- gx_target_unit_asset_impl()
  valid_units <- inherits(x$units, "tbl_df") &&
    identical(
      names(x$units),
      c("dimension", "unit_uri", "unit_label")
    ) &&
    nrow(x$units) == length(.gx_target_unit_dimensions) &&
    identical(unname(x$units$dimension), .gx_target_unit_dimensions)
  if (!valid_units || !identical(x$asset_sha256, asset$asset_sha256)) {
    gx_harmonize_abort(
      "Target units no longer bind the reviewed asset.",
      "gx_error_target_units_contract"
    )
  }
  for (index in seq_len(nrow(x$units))) {
    candidates <- asset$units[
      asset$units$dimension == x$units$dimension[[index]] &
        asset$units$unit_uri == x$units$unit_uri[[index]],
      ,
      drop = FALSE
    ]
    if (nrow(candidates) != 1L ||
        !identical(
          x$units$unit_label[[index]],
          candidates$unit_label[[1L]]
        )) {
      gx_harmonize_abort(
        "A target unit is not a reviewed choice for its dimension.",
        "gx_error_target_units_selection"
      )
    }
  }
  invisible(x)
}

gx_target_units_impl <- function(selections) {
  asset <- gx_target_unit_asset_impl()
  rows <- vector("list", length(.gx_target_unit_dimensions))
  for (index in seq_along(.gx_target_unit_dimensions)) {
    dimension <- .gx_target_unit_dimensions[[index]]
    requested <- selections[[index]]
    if (is.null(requested)) {
      candidates <- asset$units[
        asset$units$dimension == dimension & asset$units$preferred,
        ,
        drop = FALSE
      ]
    } else {
      if (!is.character(requested) || length(requested) != 1L ||
          is.na(requested) || !nzchar(requested) ||
          !is.null(attributes(requested))) {
        gx_harmonize_abort(
          "Each target must be NULL or one reviewed unit URI.",
          "gx_error_target_units_selection"
        )
      }
      candidates <- asset$units[
        asset$units$dimension == dimension &
          asset$units$unit_uri == requested,
        ,
        drop = FALSE
      ]
    }
    if (nrow(candidates) != 1L) {
      gx_harmonize_abort(
        "A requested target URI is unknown or belongs to another dimension.",
        "gx_error_target_units_selection"
      )
    }
    rows[[index]] <- candidates[c("dimension", "unit_uri", "unit_label")]
  }
  units <- tibble::as_tibble(do.call(rbind, rows))
  rownames(units) <- NULL
  object <- structure(
    list(
      contract_version = .gx_target_units_contract_version,
      units = units,
      asset_sha256 = asset$asset_sha256
    ),
    class = "gx_target_units"
  )
  gx_target_units_validate_impl(object)
  object
}

#' Select reviewed harmonization target units
#'
#' Selects exactly one target for each dimension covered by the bundled,
#' reviewed conversion vocabulary. `NULL` selects the reviewed default:
#' Celsius, metre, and cubic metre per second. Overrides must be exact unit URIs
#' already reviewed for the corresponding dimension.
#'
#' @param thermodynamic_temperature `NULL` or one reviewed temperature unit URI.
#' @param length `NULL` or one reviewed length unit URI.
#' @param volume_flow_rate `NULL` or one reviewed volume-flow-rate unit URI.
#'
#' @return A validated `gx_target_units` object.
#' @export
gx_target_units <- function(
    thermodynamic_temperature = NULL,
    length = NULL,
    volume_flow_rate = NULL) {
  gx_target_units_impl(list(
    thermodynamic_temperature,
    length,
    volume_flow_rate
  ))
}

#' @export
print.gx_target_units <- function(x, ...) {
  gx_target_units_validate_impl(x)
  cli::cli_inform(c(
    "<gx_target_units>",
    stats::setNames(
      paste0(x$units$unit_label, " (", x$units$unit_uri, ")"),
      paste0("* ", x$units$dimension, ":")
    )
  ))
  invisible(x)
}

gx_csv_mapping_text_impl <- function(
    x,
    label,
    allow_empty = FALSE,
    maximum = 1024L) {
  valid <- is.character(x) && length(x) == 1L && !is.na(x) &&
    is.null(attributes(x)) && (allow_empty || nzchar(x)) &&
    nchar(x, type = "bytes") <= maximum &&
    isTRUE(tryCatch(
      stringi::stri_enc_isutf8(x),
      error = function(cnd) FALSE,
      warning = function(cnd) FALSE
    )) &&
    !isTRUE(tryCatch(
      stringi::stri_detect_regex(x, "[\\p{Cc}\\p{Cf}\\p{Cs}]"),
      error = function(cnd) TRUE,
      warning = function(cnd) TRUE
    ))
  if (!valid) {
    gx_harmonize_abort(
      "The CSV {label} must be one bounded control-safe UTF-8 value.",
      "gx_error_csv_mapping"
    )
  }
  unname(enc2utf8(x))
}

gx_csv_mapping_missing_values_impl <- function(x) {
  valid <- is.character(x) && is.null(attributes(x)) &&
    length(x) <= 16L && !anyNA(x) && !anyDuplicated(x) &&
    all(nchar(x, type = "bytes") <= 128L) &&
    all(vapply(x, function(value) {
      isTRUE(tryCatch(
        stringi::stri_enc_isutf8(value),
        error = function(cnd) FALSE,
        warning = function(cnd) FALSE
      )) &&
        !isTRUE(tryCatch(
          stringi::stri_detect_regex(value, "[\\p{Cc}\\p{Cf}\\p{Cs}]"),
          error = function(cnd) TRUE,
          warning = function(cnd) TRUE
        ))
    }, logical(1)))
  if (!valid) {
    gx_harmonize_abort(
      "CSV missing values must be at most 16 unique bounded UTF-8 tokens.",
      "gx_error_csv_mapping"
    )
  }
  values <- unname(enc2utf8(x))
  values[gx_catalog_byte_order(values)]
}

gx_csv_mapping_id_impl <- function(
    distribution_id,
    columns,
    datetime_format,
    missing_values) {
  facts <- list(
    "distribution_id", distribution_id,
    "datetime_column", columns$datetime,
    "value_column", columns$value,
    "unit_column", columns$unit,
    "qualifier_column", columns$qualifier,
    "datetime_format", datetime_format,
    "missing_value_count", as.integer(length(missing_values))
  )
  if (length(missing_values)) {
    for (value in missing_values) {
      facts <- c(facts, list("missing_value", value))
    }
  }
  gx_contract_hash(
    facts,
    namespace = "geoconnexr.csv-mapping.v1",
    contract_version = .gx_csv_mapping_contract_version
  )
}

gx_csv_mapping_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_csv_mapping") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_csv_mapping_fields) &&
    identical(x$contract_version, .gx_csv_mapping_contract_version) &&
    is.character(x$distribution_id) && length(x$distribution_id) == 1L &&
    !is.na(x$distribution_id) && is.null(attributes(x$distribution_id)) &&
    isTRUE(gx_catalog_is_sha256(x$distribution_id)) &&
    is.list(x$columns) &&
    identical(names(attributes(x$columns)), "names") &&
    identical(names(x$columns), .gx_csv_mapping_column_fields) &&
    identical(x$datetime_format, "iso8601_utc") &&
    is.character(x$mapping_id) && length(x$mapping_id) == 1L &&
    !is.na(x$mapping_id) && is.null(attributes(x$mapping_id)) &&
    grepl("^[0-9a-f]{64}$", x$mapping_id)
  if (!valid_top) {
    gx_harmonize_abort(
      "The CSV mapping violates its exact top-level contract.",
      "gx_error_csv_mapping"
    )
  }
  required <- x$columns[c("datetime", "value", "unit")]
  valid_columns <- all(vapply(required, function(value) {
    tryCatch({
      gx_csv_mapping_text_impl(value, "column name")
      TRUE
    }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  }, logical(1)))
  qualifier <- x$columns$qualifier
  valid_qualifier <- is.character(qualifier) && length(qualifier) == 1L &&
    is.null(attributes(qualifier)) &&
    (is.na(qualifier) || tryCatch({
      gx_csv_mapping_text_impl(qualifier, "qualifier column")
      TRUE
    }, error = function(cnd) FALSE, warning = function(cnd) FALSE))
  used <- unname(c(unlist(required, use.names = FALSE), qualifier))
  used <- used[!is.na(used)]
  missing_values <- tryCatch(
    gx_csv_mapping_missing_values_impl(x$missing_values),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  valid <- valid_columns && valid_qualifier && !anyDuplicated(used) &&
    !is.null(missing_values) &&
    identical(x$missing_values, missing_values) &&
    identical(
      x$mapping_id,
      gx_csv_mapping_id_impl(
        x$distribution_id,
        x$columns,
        x$datetime_format,
        x$missing_values
      )
    )
  if (!valid) {
    gx_harmonize_abort(
      "The CSV mapping columns, missing values, or identity are invalid.",
      "gx_error_csv_mapping"
    )
  }
  invisible(x)
}

#' Declare one direct-CSV observation mapping
#'
#' Creates an exact distribution-scoped mapping for one strict direct-CSV
#' result. Column names are never guessed. This first mapping contract accepts
#' only UTC instants spelled `YYYY-MM-DDTHH:MM:SSZ`, one value column, one unit
#' label column, and an optional qualifier column. Missing-value tokens apply
#' only to values and qualifiers.
#'
#' @param distribution_id Exact distribution SHA-256 from a [gx_fetch_plan()]
#'   or `gx_fetched` object.
#' @param datetime_column Exact UTC timestamp column name.
#' @param value_column Exact observation-value column name.
#' @param unit_column Exact native unit-label column name.
#' @param qualifier_column `NULL` or one exact qualifier column name.
#' @param missing_values Up to 16 exact text tokens treated as missing in value
#'   and qualifier columns. The original value text remains preserved.
#'
#' @return A validated `gx_csv_mapping` object for [gx_harmonize()].
#' @export
gx_csv_mapping <- function(
    distribution_id,
    datetime_column,
    value_column,
    unit_column,
    qualifier_column = NULL,
    missing_values = "") {
  if (!is.character(distribution_id) || length(distribution_id) != 1L ||
      is.na(distribution_id) ||
      !isTRUE(gx_catalog_is_sha256(distribution_id))) {
    gx_harmonize_abort(
      "A CSV mapping requires one exact distribution SHA-256.",
      "gx_error_csv_mapping"
    )
  }
  columns <- list(
    datetime = gx_csv_mapping_text_impl(
      datetime_column, "datetime column"
    ),
    value = gx_csv_mapping_text_impl(value_column, "value column"),
    unit = gx_csv_mapping_text_impl(unit_column, "unit column"),
    qualifier = if (is.null(qualifier_column)) {
      NA_character_
    } else {
      gx_csv_mapping_text_impl(qualifier_column, "qualifier column")
    }
  )
  if (anyDuplicated(unname(unlist(columns, use.names = FALSE)[
    !is.na(unlist(columns, use.names = FALSE))
  ]))) {
    gx_harmonize_abort(
      "CSV mapping roles must use distinct columns.",
      "gx_error_csv_mapping"
    )
  }
  missing_values <- gx_csv_mapping_missing_values_impl(missing_values)
  datetime_format <- "iso8601_utc"
  object <- structure(
    list(
      contract_version = .gx_csv_mapping_contract_version,
      distribution_id = unname(distribution_id),
      columns = columns,
      datetime_format = datetime_format,
      missing_values = missing_values,
      mapping_id = gx_csv_mapping_id_impl(
        distribution_id, columns, datetime_format, missing_values
      )
    ),
    class = "gx_csv_mapping"
  )
  gx_csv_mapping_validate_impl(object)
  object
}

#' @export
print.gx_csv_mapping <- function(x, ...) {
  gx_csv_mapping_validate_impl(x)
  qualifier <- if (is.na(x$columns$qualifier)) {
    "none"
  } else {
    x$columns$qualifier
  }
  cli::cli_inform(c(
    "<gx_csv_mapping>",
    "* Distribution: {x$distribution_id}",
    paste0(
      "* Columns: datetime=", x$columns$datetime,
      "; value=", x$columns$value,
      "; unit=", x$columns$unit,
      "; qualifier=", qualifier
    ),
    "* Datetime format: YYYY-MM-DDTHH:MM:SSZ"
  ))
  invisible(x)
}

gx_csv_mappings_normalize_impl <- function(x, fetched = NULL) {
  if (inherits(x, "gx_csv_mapping")) x <- list(x)
  if (!is.list(x)) {
    gx_harmonize_abort(
      "CSV mappings must be a mapping or an unnamed list of mappings.",
      "gx_error_csv_mapping"
    )
  }
  mappings <- unname(lapply(seq_along(x), function(index) x[[index]]))
  if (length(mappings) > 1024L ||
      !all(vapply(mappings, function(mapping) {
        tryCatch({
          gx_csv_mapping_validate_impl(mapping)
          TRUE
        }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
      }, logical(1)))) {
    gx_harmonize_abort(
      "CSV mappings contain an invalid object or exceed the fixed limit.",
      "gx_error_csv_mapping"
    )
  }
  ids <- vapply(
    mappings, `[[`, character(1), "distribution_id",
    USE.NAMES = FALSE
  )
  if (anyDuplicated(ids)) {
    gx_harmonize_abort(
      "Each CSV distribution may have at most one mapping.",
      "gx_error_csv_mapping"
    )
  }
  if (!is.null(fetched) && length(mappings)) {
    positions <- match(ids, fetched$plan$distributions$distribution_id)
    valid_binding <- !anyNA(positions) && all(
      fetched$plan$distributions$handler_id[positions] == "csv"
    )
    if (!valid_binding) {
      gx_harmonize_abort(
        "Every CSV mapping must bind one planned direct-CSV distribution.",
        "gx_error_csv_mapping_binding"
      )
    }
  }
  mappings
}

gx_feature_mapping_text_impl <- function(x, label) {
  valid <- is.character(x) && length(x) == 1L && !is.na(x) &&
    is.null(attributes(x)) && nzchar(x) &&
    nchar(x, type = "bytes") <= 1024L &&
    isTRUE(tryCatch(
      stringi::stri_enc_isutf8(x),
      error = function(cnd) FALSE,
      warning = function(cnd) FALSE
    )) &&
    !isTRUE(tryCatch(
      stringi::stri_detect_regex(x, "[\\p{Cc}\\p{Cf}\\p{Cs}]"),
      error = function(cnd) TRUE,
      warning = function(cnd) TRUE
    ))
  if (!valid) {
    gx_harmonize_abort(
      "The feature {label} must be one bounded control-safe UTF-8 value.",
      "gx_error_feature_mapping"
    )
  }
  unname(enc2utf8(x))
}

gx_feature_mapping_missing_values_impl <- function(x) {
  valid <- is.character(x) && is.null(attributes(x)) &&
    length(x) <= 16L && !anyNA(x) && !anyDuplicated(x) &&
    all(nchar(x, type = "bytes") <= 128L) &&
    all(vapply(x, function(value) {
      isTRUE(tryCatch(
        stringi::stri_enc_isutf8(value),
        error = function(cnd) FALSE,
        warning = function(cnd) FALSE
      )) &&
        !isTRUE(tryCatch(
          stringi::stri_detect_regex(value, "[\\p{Cc}\\p{Cf}\\p{Cs}]"),
          error = function(cnd) TRUE,
          warning = function(cnd) TRUE
        ))
    }, logical(1)))
  if (!valid) {
    gx_harmonize_abort(
      "Feature missing values must be at most 16 unique bounded UTF-8 tokens.",
      "gx_error_feature_mapping"
    )
  }
  values <- unname(enc2utf8(x))
  values[gx_catalog_byte_order(values)]
}

gx_feature_mapping_id_impl <- function(
    distribution_id,
    properties,
    datetime_format,
    missing_values) {
  facts <- list(
    "distribution_id", distribution_id,
    "datetime_property", properties$datetime,
    "value_property", properties$value,
    "unit_property", properties$unit,
    "qualifier_property", properties$qualifier,
    "datetime_format", datetime_format,
    "missing_value_count", as.integer(length(missing_values))
  )
  if (length(missing_values)) {
    for (value in missing_values) {
      facts <- c(facts, list("missing_value", value))
    }
  }
  gx_contract_hash(
    facts,
    namespace = "geoconnexr.feature-mapping.v1",
    contract_version = .gx_feature_mapping_contract_version
  )
}

gx_feature_mapping_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_feature_mapping") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_feature_mapping_fields) &&
    identical(
      x$contract_version,
      .gx_feature_mapping_contract_version
    ) &&
    is.character(x$distribution_id) && length(x$distribution_id) == 1L &&
    !is.na(x$distribution_id) && is.null(attributes(x$distribution_id)) &&
    isTRUE(gx_catalog_is_sha256(x$distribution_id)) &&
    is.list(x$properties) &&
    identical(names(attributes(x$properties)), "names") &&
    identical(
      names(x$properties),
      .gx_feature_mapping_property_fields
    ) &&
    identical(x$datetime_format, "iso8601_utc") &&
    is.character(x$mapping_id) && length(x$mapping_id) == 1L &&
    !is.na(x$mapping_id) && is.null(attributes(x$mapping_id)) &&
    grepl("^[0-9a-f]{64}$", x$mapping_id)
  if (!valid_top) {
    gx_harmonize_abort(
      "The feature mapping violates its exact top-level contract.",
      "gx_error_feature_mapping"
    )
  }
  required <- x$properties[c("datetime", "value", "unit")]
  valid_properties <- all(vapply(required, function(value) {
    tryCatch({
      gx_feature_mapping_text_impl(value, "property name")
      TRUE
    }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  }, logical(1)))
  qualifier <- x$properties$qualifier
  valid_qualifier <- is.character(qualifier) && length(qualifier) == 1L &&
    is.null(attributes(qualifier)) &&
    (is.na(qualifier) || tryCatch({
      gx_feature_mapping_text_impl(qualifier, "qualifier property")
      TRUE
    }, error = function(cnd) FALSE, warning = function(cnd) FALSE))
  used <- unname(c(unlist(required, use.names = FALSE), qualifier))
  used <- used[!is.na(used)]
  missing_values <- tryCatch(
    gx_feature_mapping_missing_values_impl(x$missing_values),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  valid <- valid_properties && valid_qualifier &&
    !anyDuplicated(used) &&
    !any(used %in% .gx_feature_mapping_reserved_properties) &&
    !is.null(missing_values) &&
    identical(x$missing_values, missing_values) &&
    identical(
      x$mapping_id,
      gx_feature_mapping_id_impl(
        x$distribution_id,
        x$properties,
        x$datetime_format,
        x$missing_values
      )
    )
  if (!valid) {
    gx_harmonize_abort(
      "The feature mapping properties, missing values, or identity are invalid.",
      "gx_error_feature_mapping"
    )
  }
  invisible(x)
}

#' Declare one OGC API Features observation mapping
#'
#' Creates an exact distribution-scoped property mapping for one bounded OGC
#' API Features result. Property names are never guessed. Generated feature
#' identifiers and geometry cannot be assigned observation roles. This first
#' contract accepts only UTC instants spelled `YYYY-MM-DDTHH:MM:SSZ`, one value
#' property, one unit-label property, and an optional qualifier property.
#'
#' @param distribution_id Exact distribution SHA-256 from a [gx_fetch_plan()]
#'   or `gx_fetched` object.
#' @param datetime_property Exact UTC timestamp property name.
#' @param value_property Exact observation-value property name.
#' @param unit_property Exact native unit-label property name.
#' @param qualifier_property `NULL` or one exact qualifier property name.
#' @param missing_values Up to 16 exact text tokens treated as missing in
#'   character value and qualifier properties. The original value remains
#'   preserved.
#'
#' @return A validated `gx_feature_mapping` object for [gx_harmonize()].
#' @export
gx_feature_mapping <- function(
    distribution_id,
    datetime_property,
    value_property,
    unit_property,
    qualifier_property = NULL,
    missing_values = "") {
  if (!is.character(distribution_id) || length(distribution_id) != 1L ||
      is.na(distribution_id) ||
      !isTRUE(gx_catalog_is_sha256(distribution_id))) {
    gx_harmonize_abort(
      "A feature mapping requires one exact distribution SHA-256.",
      "gx_error_feature_mapping"
    )
  }
  properties <- list(
    datetime = gx_feature_mapping_text_impl(
      datetime_property, "datetime property"
    ),
    value = gx_feature_mapping_text_impl(
      value_property, "value property"
    ),
    unit = gx_feature_mapping_text_impl(
      unit_property, "unit property"
    ),
    qualifier = if (is.null(qualifier_property)) {
      NA_character_
    } else {
      gx_feature_mapping_text_impl(
        qualifier_property, "qualifier property"
      )
    }
  )
  used <- unname(unlist(properties, use.names = FALSE))
  used <- used[!is.na(used)]
  if (anyDuplicated(used) ||
      any(used %in% .gx_feature_mapping_reserved_properties)) {
    gx_harmonize_abort(
      "Feature mapping roles must use distinct non-reserved properties.",
      "gx_error_feature_mapping"
    )
  }
  missing_values <- gx_feature_mapping_missing_values_impl(missing_values)
  datetime_format <- "iso8601_utc"
  object <- structure(
    list(
      contract_version = .gx_feature_mapping_contract_version,
      distribution_id = unname(distribution_id),
      properties = properties,
      datetime_format = datetime_format,
      missing_values = missing_values,
      mapping_id = gx_feature_mapping_id_impl(
        distribution_id, properties, datetime_format, missing_values
      )
    ),
    class = "gx_feature_mapping"
  )
  gx_feature_mapping_validate_impl(object)
  object
}

#' @export
print.gx_feature_mapping <- function(x, ...) {
  gx_feature_mapping_validate_impl(x)
  qualifier <- if (is.na(x$properties$qualifier)) {
    "none"
  } else {
    x$properties$qualifier
  }
  cli::cli_inform(c(
    "<gx_feature_mapping>",
    "* Distribution: {x$distribution_id}",
    paste0(
      "* Properties: datetime=", x$properties$datetime,
      "; value=", x$properties$value,
      "; unit=", x$properties$unit,
      "; qualifier=", qualifier
    ),
    "* Datetime format: YYYY-MM-DDTHH:MM:SSZ",
    "* Feature IDs and geometry: not mapped"
  ))
  invisible(x)
}

gx_feature_mappings_normalize_impl <- function(x, fetched = NULL) {
  if (inherits(x, "gx_feature_mapping")) x <- list(x)
  if (!is.list(x)) {
    gx_harmonize_abort(
      "Feature mappings must be a mapping or an unnamed list of mappings.",
      "gx_error_feature_mapping"
    )
  }
  mappings <- unname(lapply(seq_along(x), function(index) x[[index]]))
  if (length(mappings) > 1024L ||
      !all(vapply(mappings, function(mapping) {
        tryCatch({
          gx_feature_mapping_validate_impl(mapping)
          TRUE
        }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
      }, logical(1)))) {
    gx_harmonize_abort(
      "Feature mappings contain an invalid object or exceed the fixed limit.",
      "gx_error_feature_mapping"
    )
  }
  ids <- vapply(
    mappings, `[[`, character(1), "distribution_id",
    USE.NAMES = FALSE
  )
  if (anyDuplicated(ids)) {
    gx_harmonize_abort(
      "Each OGC API Features distribution may have at most one mapping.",
      "gx_error_feature_mapping"
    )
  }
  if (!is.null(fetched) && length(mappings)) {
    positions <- match(ids, fetched$plan$distributions$distribution_id)
    valid_binding <- !anyNA(positions) && all(
      fetched$plan$distributions$handler_id[positions] ==
        "ogc_api_features"
    )
    if (!valid_binding) {
      gx_harmonize_abort(
        paste0(
          "Every feature mapping must bind one planned OGC API Features ",
          "distribution."
        ),
        "gx_error_feature_mapping_binding"
      )
    }
  }
  mappings
}

gx_harmonized_empty_observations_impl <- function() {
  tibble::tibble(
    contract_version = character(), observation_index = integer(),
    result_id = character(), native_row = integer(), site_uri = character(),
    dataset_id = character(), distribution_id = character(),
    handler_id = character(), variable_id = character(),
    variable_uri = character(), variable_name = character(),
    datetime = as.POSIXct(character(), tz = "UTC"), value = double(),
    unit_uri = character(), unit_label = character(),
    original_value = character(), original_unit_uri = character(),
    original_unit_label = character(), qualifier = character(),
    harmonized = logical(), conversion_rule_id = character(),
    status = character()
  )
}

gx_harmonized_empty_resources_impl <- function() {
  tibble::tibble(
    contract_version = character(), result_index = integer(),
    result_id = character(), distribution_id = character(),
    handler_id = character(), payload_class = character(),
    native_rows = integer(), native_columns = integer(),
    timeseries = logical(), observation_count = integer(),
    status = character()
  )
}

gx_harmonize_numeric_impl <- function(value) {
  if (length(value) != 1L) return(list(missing = FALSE, valid = FALSE))
  if (is.numeric(value) && !is.logical(value)) {
    if (is.na(value)) {
      return(list(missing = TRUE, valid = TRUE, value = NA_real_))
    }
    return(list(
      missing = FALSE,
      valid = is.finite(value),
      value = as.numeric(value)
    ))
  }
  if (is.character(value) && is.na(value)) {
    return(list(missing = TRUE, valid = TRUE, value = NA_real_))
  }
  if (!is.character(value) ||
      !grepl(
        "^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?$",
        value,
        perl = TRUE
      )) {
    return(list(missing = FALSE, valid = FALSE))
  }
  parsed <- suppressWarnings(as.numeric(value))
  list(missing = FALSE, valid = is.finite(parsed), value = parsed)
}

gx_harmonize_value_impl <- function(
    value,
    source_unit_uri,
    source_unit_label,
    variable_mapped,
    unit_corroborated,
    target_units,
    rules = gx_unit_conversions()) {
  numeric <- gx_harmonize_numeric_impl(value)
  original_uri <- if (unit_corroborated) source_unit_uri else NA_character_
  unchanged <- list(
    value = if (isTRUE(numeric$valid)) numeric$value else NA_real_,
    unit_uri = original_uri,
    unit_label = source_unit_label,
    original_unit_uri = original_uri,
    harmonized = FALSE,
    conversion_rule_id = NA_character_
  )
  if (!variable_mapped) {
    unchanged$status <- "variable_ambiguous"
    return(unchanged)
  }
  if (!unit_corroborated) {
    unchanged$status <- "unit_conflict"
    return(unchanged)
  }
  if (!numeric$valid) {
    unchanged$status <- "invalid_value"
    return(unchanged)
  }

  dimensions <- unique(c(
    rules$dimension[rules$from_unit_uri == source_unit_uri],
    rules$dimension[rules$to_unit_uri == source_unit_uri]
  ))
  if (!length(dimensions)) {
    unchanged$status <- "unit_unmapped"
    return(unchanged)
  }
  if (length(dimensions) != 1L) {
    gx_harmonize_abort(
      "A reviewed unit URI belongs to more than one dimension.",
      "gx_error_harmonize_rules"
    )
  }
  target <- target_units$units[
    target_units$units$dimension == dimensions[[1L]],
    ,
    drop = FALSE
  ]
  if (nrow(target) != 1L) {
    gx_harmonize_abort(
      "Target units do not cover a reviewed source dimension.",
      "gx_error_harmonize_rules"
    )
  }
  if (identical(source_unit_uri, target$unit_uri[[1L]])) {
    unchanged$unit_uri <- target$unit_uri[[1L]]
    unchanged$unit_label <- target$unit_label[[1L]]
    unchanged$harmonized <- TRUE
    unchanged$status <- if (numeric$missing) {
      "harmonized_missing"
    } else {
      "harmonized_identity"
    }
    return(unchanged)
  }
  matched <- which(
    rules$from_unit_uri == source_unit_uri &
      rules$to_unit_uri == target$unit_uri[[1L]] &
      rules$dimension == dimensions[[1L]] &
      rules$status == "reviewed"
  )
  if (!length(matched)) {
    unchanged$status <- "conversion_unavailable"
    return(unchanged)
  }
  if (length(matched) != 1L) {
    gx_harmonize_abort(
      "More than one reviewed direct conversion matched an observation.",
      "gx_error_harmonize_rules"
    )
  }
  rule <- rules[matched, , drop = FALSE]
  unchanged$value <- if (numeric$missing) {
    NA_real_
  } else {
    numeric$value * rule$scale[[1L]] + rule$offset[[1L]]
  }
  unchanged$unit_uri <- target$unit_uri[[1L]]
  unchanged$unit_label <- target$unit_label[[1L]]
  unchanged$harmonized <- TRUE
  unchanged$conversion_rule_id <- rule$rule_id[[1L]]
  unchanged$status <- if (numeric$missing) {
    "harmonized_missing"
  } else {
    "harmonized_converted"
  }
  unchanged
}

gx_harmonize_csv_datetime_impl <- function(value) {
  valid <- is.character(value) && !anyNA(value) &&
    all(grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
      value
    ))
  if (!valid) return(NULL)
  datetime <- suppressWarnings(as.POSIXct(
    value,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  ))
  if (anyNA(datetime) || !identical(
    unname(format(datetime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    unname(value)
  )) {
    return(NULL)
  }
  datetime
}

gx_harmonize_csv_native_fields_impl <- function(data, mapping) {
  if (is.null(mapping) || !is.data.frame(data)) {
    return(list(supported = FALSE))
  }
  required <- unname(unlist(
    mapping$columns[c("datetime", "value", "unit")],
    use.names = FALSE
  ))
  qualifier_column <- mapping$columns$qualifier
  mapped_columns <- c(
    required,
    if (is.na(qualifier_column)) character() else qualifier_column
  )
  if (!all(mapped_columns %in% names(data)) ||
      !all(vapply(data[mapped_columns], is.character, logical(1)))) {
    return(list(supported = FALSE))
  }
  datetime <- gx_harmonize_csv_datetime_impl(
    data[[mapping$columns$datetime]]
  )
  if (is.null(datetime)) return(list(supported = FALSE))
  original_value <- unname(data[[mapping$columns$value]])
  value <- original_value
  value[value %in% mapping$missing_values] <- NA_character_
  qualifier <- if (is.na(qualifier_column)) {
    rep.int(NA_character_, nrow(data))
  } else {
    unname(data[[qualifier_column]])
  }
  qualifier[qualifier %in% mapping$missing_values] <- NA_character_
  list(
    supported = TRUE,
    datetime = datetime,
    value = as.list(value),
    original_value = original_value,
    unit_label = unname(data[[mapping$columns$unit]]),
    qualifier = qualifier,
    variable_name = rep.int(NA_character_, nrow(data))
  )
}

gx_harmonize_feature_native_fields_impl <- function(
    data,
    raw_body,
    mapping) {
  if (is.null(mapping) || !inherits(data, "gx_oaf_features") ||
      !inherits(data, "sf") || !is.raw(raw_body)) {
    return(list(supported = FALSE))
  }
  required <- unname(unlist(
    mapping$properties[c("datetime", "value", "unit")],
    use.names = FALSE
  ))
  qualifier_property <- mapping$properties$qualifier
  mapped_properties <- c(
    required,
    if (is.na(qualifier_property)) character() else qualifier_property
  )
  geometry_property <- attr(data, "sf_column", exact = TRUE)
  if (!all(mapped_properties %in% names(data)) ||
      any(mapped_properties %in% c(
        .gx_feature_mapping_reserved_properties,
        geometry_property
      ))) {
    return(list(supported = FALSE))
  }
  features <- tryCatch({
    response <- list(
      headers = list(`Content-Type` = "application/geo+json"),
      body = raw_body
    )
    gx_ref_feature_array(gx_ref_json(response, "features"))
  }, error = function(cnd) NULL, warning = function(cnd) NULL)
  if (is.null(features) || length(features) != nrow(data) ||
      !all(vapply(features, function(feature) {
        properties <- feature$properties %||% list()
        all(mapped_properties %in% names(properties))
      }, logical(1)))) {
    return(list(supported = FALSE))
  }
  property_values <- function(name) {
    unname(lapply(features, function(feature) {
      feature$properties[[name]]
    }))
  }
  datetime_values <- property_values(mapping$properties$datetime)
  unit_values <- property_values(mapping$properties$unit)
  value_values <- property_values(mapping$properties$value)
  qualifier_values <- if (is.na(qualifier_property)) {
    rep(list(NULL), nrow(data))
  } else {
    property_values(qualifier_property)
  }
  valid_datetime <- all(vapply(datetime_values, function(value) {
    is.character(value) && length(value) == 1L && !is.na(value)
  }, logical(1)))
  valid_units <- all(vapply(unit_values, function(value) {
    is.character(value) && length(value) == 1L && !is.na(value)
  }, logical(1)))
  valid_values <- all(vapply(value_values, function(value) {
    is.null(value) ||
      (is.character(value) && length(value) == 1L) ||
      (is.numeric(value) && !is.logical(value) && length(value) == 1L &&
         (is.na(value) || is.finite(value)))
  }, logical(1)))
  valid_qualifiers <- all(vapply(qualifier_values, function(value) {
    is.null(value) ||
      (is.character(value) && length(value) == 1L)
  }, logical(1)))
  if (!valid_datetime || !valid_units || !valid_values ||
      !valid_qualifiers) {
    return(list(supported = FALSE))
  }
  datetime_text <- unname(vapply(
    datetime_values, `[[`, character(1), 1L
  ))
  datetime <- gx_harmonize_csv_datetime_impl(datetime_text)
  if (is.null(datetime)) return(list(supported = FALSE))

  original_value <- unname(vapply(value_values, function(value) {
    if (is.null(value) || is.na(value)) NA_character_ else as.character(value)
  }, character(1)))
  value <- unname(lapply(value_values, function(value) {
    if (is.null(value) ||
        (is.character(value) && value %in% mapping$missing_values)) {
      NA_real_
    } else {
      value
    }
  }))
  unit_label <- unname(vapply(
    unit_values, `[[`, character(1), 1L
  ))
  qualifier <- unname(vapply(qualifier_values, function(value) {
    if (is.null(value)) NA_character_ else value
  }, character(1)))
  qualifier[qualifier %in% mapping$missing_values] <- NA_character_
  list(
    supported = TRUE,
    datetime = datetime,
    value = value,
    original_value = unname(original_value),
    unit_label = unit_label,
    qualifier = qualifier,
    variable_name = rep.int(NA_character_, nrow(data))
  )
}

gx_harmonize_wqp_datetime_impl <- function(
    date,
    time,
    zone,
    zones = gx_wqp_timezone_asset_impl()$zones) {
  valid_shape <- is.character(date) && is.character(time) &&
    is.character(zone) && length(date) == length(time) &&
    length(time) == length(zone) && !anyNA(date) && !anyNA(time) &&
    !anyNA(zone) &&
    all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date)) &&
    all(grepl("^[0-9]{2}:[0-9]{2}:[0-9]{2}$", time))
  if (!valid_shape) return(NULL)
  positions <- match(zone, zones$code)
  if (anyNA(positions)) return(NULL)
  text <- paste(date, time)
  local <- suppressWarnings(as.POSIXct(
    text,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  ))
  if (anyNA(local) || !identical(
    unname(format(local, "%Y-%m-%d %H:%M:%S", tz = "UTC")),
    unname(text)
  )) {
    return(NULL)
  }
  as.POSIXct(
    as.numeric(local) - zones$offset_minutes[positions] * 60,
    origin = "1970-01-01",
    tz = "UTC"
  )
}

gx_harmonize_wqp_query_impl <- function(distribution_url) {
  parsed <- tryCatch(
    httr2::url_parse(distribution_url),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(parsed) || !is.list(parsed$query)) return(NULL)
  site <- parsed$query[names(parsed$query) == "siteid"]
  characteristic <- parsed$query[
    names(parsed$query) == "characteristicName"
  ]
  if (length(site) != 1L || length(characteristic) != 1L ||
      !is.character(site[[1L]]) || length(site[[1L]]) != 1L ||
      is.na(site[[1L]]) || !nzchar(site[[1L]]) ||
      !is.character(characteristic[[1L]]) ||
      length(characteristic[[1L]]) != 1L ||
      is.na(characteristic[[1L]]) || !nzchar(characteristic[[1L]])) {
    return(NULL)
  }
  list(
    site_id = unname(site[[1L]]),
    characteristic_name = unname(characteristic[[1L]])
  )
}

gx_harmonize_wqp_native_fields_impl <- function(
    data,
    distribution,
    parameters) {
  required <- c(
    "MonitoringLocationIdentifier", "ActivityStartDate",
    "ActivityStartTime.Time", "ActivityStartTime.TimeZoneCode",
    "CharacteristicName", "ResultMeasureValue",
    "ResultMeasure.MeasureUnitCode"
  )
  if (!is.data.frame(data) || !all(required %in% names(data)) ||
      nrow(parameters) != 1L || nrow(distribution) != 1L ||
      is.na(parameters$variable_name[[1L]]) ||
      !nzchar(parameters$variable_name[[1L]])) {
    return(list(supported = FALSE))
  }
  query <- gx_harmonize_wqp_query_impl(
    distribution$distribution_url[[1L]]
  )
  if (is.null(query) ||
      !identical(
        query$characteristic_name,
        parameters$variable_name[[1L]]
      ) ||
      anyNA(data$MonitoringLocationIdentifier) ||
      any(data$MonitoringLocationIdentifier != query$site_id) ||
      anyNA(data$CharacteristicName) ||
      any(data$CharacteristicName != query$characteristic_name)) {
    return(list(supported = FALSE))
  }
  datetime <- gx_harmonize_wqp_datetime_impl(
    data$ActivityStartDate,
    data$ActivityStartTime.Time,
    data$ActivityStartTime.TimeZoneCode
  )
  if (is.null(datetime)) return(list(supported = FALSE))
  original_value <- unname(data$ResultMeasureValue)
  value <- original_value
  value[value %in% c("", "NA")] <- NA_character_
  qualifier <- if ("MeasureQualifierCode" %in% names(data)) {
    unname(data$MeasureQualifierCode)
  } else {
    rep.int(NA_character_, nrow(data))
  }
  qualifier[qualifier %in% c("", "NA")] <- NA_character_
  list(
    supported = TRUE,
    datetime = datetime,
    value = as.list(value),
    original_value = original_value,
    unit_label = unname(data$ResultMeasure.MeasureUnitCode),
    qualifier = qualifier,
    variable_name = unname(data$CharacteristicName)
  )
}

gx_harmonize_native_fields_impl <- function(
    result,
    distribution,
    parameters,
    csv_mapping = NULL,
    feature_mapping = NULL) {
  data <- result$data[[1L]]
  handler <- result$handler_id[[1L]]
  if (handler == "edr") {
    return(list(
      supported = TRUE,
      datetime = data$datetime,
      value = as.list(data$value),
      original_value = ifelse(is.na(data$value), NA_character_,
                              as.character(data$value)),
      unit_label = unname(data$unit),
      qualifier = rep.int(NA_character_, nrow(data)),
      variable_name = unname(data$parameter_label)
    ))
  }
  if (handler == "usgs_waterdata_continuous") {
    return(list(
      supported = TRUE,
      datetime = data$time,
      value = as.list(data$value),
      original_value = unname(data$value),
      unit_label = unname(data$unit_of_measure),
      qualifier = unname(data$qualifier),
      variable_name = unname(data$parameter_code)
    ))
  }
  if (handler == "usgs_waterdata_daily") {
    datetime <- as.POSIXct(
      as.character(data$time),
      format = "%Y-%m-%d",
      tz = "UTC"
    )
    return(list(
      supported = TRUE,
      datetime = datetime,
      value = as.list(data$value),
      original_value = unname(data$value),
      unit_label = unname(data$unit_of_measure),
      qualifier = unname(data$qualifier),
      variable_name = unname(data$parameter_code)
    ))
  }
  if (handler == "wqp") {
    return(gx_harmonize_wqp_native_fields_impl(
      data, distribution, parameters
    ))
  }
  if (handler == "csv") {
    return(gx_harmonize_csv_native_fields_impl(data, csv_mapping))
  }
  if (handler == "ogc_api_features") {
    return(gx_harmonize_feature_native_fields_impl(
      data, result$raw_body[[1L]], feature_mapping
    ))
  }
  list(supported = FALSE)
}

gx_harmonize_observation_piece_impl <- function(
    fetched,
    result_index,
    target_units,
    rules,
    csv_mappings,
    feature_mappings) {
  result <- fetched$results[result_index, , drop = FALSE]
  distribution_id <- result$distribution_id[[1L]]
  distribution <- fetched$plan$distributions[
    fetched$plan$distributions$distribution_id == distribution_id,
    ,
    drop = FALSE
  ]
  parameters <- fetched$plan$parameters[
    fetched$plan$parameters$distribution_id == distribution_id,
    ,
    drop = FALSE
  ]
  csv_positions <- which(vapply(
    csv_mappings,
    function(mapping) {
      identical(mapping$distribution_id, distribution_id)
    },
    logical(1)
  ))
  csv_mapping <- if (length(csv_positions) == 1L) {
    csv_mappings[[csv_positions[[1L]]]]
  } else {
    NULL
  }
  feature_positions <- which(vapply(
    feature_mappings,
    function(mapping) {
      identical(mapping$distribution_id, distribution_id)
    },
    logical(1)
  ))
  feature_mapping <- if (length(feature_positions) == 1L) {
    feature_mappings[[feature_positions[[1L]]]]
  } else {
    NULL
  }
  native <- gx_harmonize_native_fields_impl(
    result, distribution, parameters, csv_mapping, feature_mapping
  )
  if (!native$supported) return(gx_harmonized_empty_observations_impl())
  rows <- result$row_count[[1L]]
  if (rows == 0L) return(gx_harmonized_empty_observations_impl())
  variable_mapped <- nrow(parameters) == 1L &&
    !is.na(parameters$variable_id[[1L]]) &&
    nzchar(parameters$variable_id[[1L]]) &&
    !is.na(parameters$variable_uri[[1L]]) &&
    nzchar(parameters$variable_uri[[1L]])
  source_unit_uri <- if (nrow(parameters) == 1L) {
    parameters$unit_uri[[1L]]
  } else {
    NA_character_
  }
  source_unit_label <- if (nrow(parameters) == 1L) {
    parameters$unit_label[[1L]]
  } else {
    NA_character_
  }
  unit_corroborated <- variable_mapped &&
    !is.na(source_unit_uri) && nzchar(source_unit_uri) &&
    !is.na(source_unit_label) && nzchar(source_unit_label)

  converted <- lapply(seq_len(rows), function(row) {
    corroborated <- unit_corroborated &&
      !is.na(native$unit_label[[row]]) &&
      identical(native$unit_label[[row]], source_unit_label)
    gx_harmonize_value_impl(
      value = native$value[[row]],
      source_unit_uri = source_unit_uri,
      source_unit_label = native$unit_label[[row]],
      variable_mapped = variable_mapped,
      unit_corroborated = corroborated,
      target_units = target_units,
      rules = rules
    )
  })

  variable_id <- if (variable_mapped) {
    rep.int(parameters$variable_id[[1L]], rows)
  } else {
    rep.int(NA_character_, rows)
  }
  variable_uri <- if (variable_mapped) {
    rep.int(parameters$variable_uri[[1L]], rows)
  } else {
    rep.int(NA_character_, rows)
  }
  variable_name <- if (variable_mapped &&
      !is.na(parameters$variable_name[[1L]]) &&
      nzchar(parameters$variable_name[[1L]])) {
    rep.int(parameters$variable_name[[1L]], rows)
  } else {
    native$variable_name
  }
  tibble::tibble(
    contract_version = rep.int(.gx_harmonized_contract_version, rows),
    observation_index = rep.int(NA_integer_, rows),
    result_id = rep.int(result$result_id[[1L]], rows),
    native_row = seq_len(rows),
    site_uri = rep.int(distribution$site_uri[[1L]], rows),
    dataset_id = rep.int(distribution$dataset_id[[1L]], rows),
    distribution_id = rep.int(distribution_id, rows),
    handler_id = rep.int(result$handler_id[[1L]], rows),
    variable_id = variable_id,
    variable_uri = variable_uri,
    variable_name = variable_name,
    datetime = as.POSIXct(native$datetime, origin = "1970-01-01", tz = "UTC"),
    value = vapply(converted, `[[`, numeric(1), "value"),
    unit_uri = vapply(converted, `[[`, character(1), "unit_uri"),
    unit_label = vapply(converted, `[[`, character(1), "unit_label"),
    original_value = native$original_value,
    original_unit_uri = vapply(
      converted, `[[`, character(1), "original_unit_uri"
    ),
    original_unit_label = native$unit_label,
    qualifier = native$qualifier,
    harmonized = vapply(converted, `[[`, logical(1), "harmonized"),
    conversion_rule_id = vapply(
      converted, `[[`, character(1), "conversion_rule_id"
    ),
    status = vapply(converted, `[[`, character(1), "status")
  )
}

gx_harmonized_observations_impl <- function(
    fetched,
    target_units,
    csv_mappings,
    feature_mappings) {
  count <- nrow(fetched$results)
  if (!count) return(gx_harmonized_empty_observations_impl())
  rules <- gx_unit_conversions()
  pieces <- lapply(seq_len(count), function(index) {
    gx_harmonize_observation_piece_impl(
      fetched, index, target_units, rules, csv_mappings, feature_mappings
    )
  })
  total <- sum(vapply(pieces, nrow, integer(1)))
  if (total > .gx_harmonized_max_observations) {
    gx_harmonize_abort(
      "The fetched payloads exceed the fixed harmonization row ceiling.",
      "gx_error_harmonize_limit"
    )
  }
  if (!total) return(gx_harmonized_empty_observations_impl())
  observations <- do.call(rbind, pieces[vapply(pieces, nrow, integer(1)) > 0L])
  rownames(observations) <- NULL
  observations$observation_index <- seq_len(nrow(observations))
  observations
}

gx_harmonized_resources_impl <- function(fetched, observations) {
  count <- nrow(fetched$results)
  if (!count) return(gx_harmonized_empty_resources_impl())
  timeseries_handlers <- c(
    "edr", "usgs_waterdata_continuous", "usgs_waterdata_daily"
  )
  observations_per_result <- tabulate(
    match(observations$result_id, fetched$results$result_id),
    nbins = count
  )
  timeseries <- fetched$results$handler_id %in% timeseries_handlers |
    (fetched$results$handler_id %in% c(
      "wqp", "csv", "ogc_api_features"
    ) &
       observations_per_result > 0L)
  status <- ifelse(
    !timeseries,
    "native_only",
    ifelse(observations_per_result == 0L, "timeseries_empty",
           "observations_extracted")
  )
  tibble::tibble(
    contract_version = rep.int(.gx_harmonized_contract_version, count),
    result_index = unname(fetched$results$result_index),
    result_id = unname(fetched$results$result_id),
    distribution_id = unname(fetched$results$distribution_id),
    handler_id = unname(fetched$results$handler_id),
    payload_class = unname(fetched$results$payload_class),
    native_rows = unname(fetched$results$row_count),
    native_columns = unname(fetched$results$column_count),
    timeseries = unname(timeseries),
    observation_count = unname(as.integer(observations_per_result)),
    status = unname(status)
  )
}

gx_harmonized_metadata_impl <- function(
    target_units,
    observations,
    resources) {
  limitations <- c(
    "csv_unmapped_or_unsupported_native_only",
    "oaf_unmapped_or_unsupported_native_only",
    "wqp_unfiltered_unaligned_or_unknown_zone_native_only",
    "single_step_conversions_only"
  )
  limitations <- limitations[gx_catalog_byte_order(limitations)]
  timezone_asset <- gx_wqp_timezone_asset_impl()
  list(
    scope = "reviewed_timeseries_v5",
    offline = TRUE,
    raw_payloads_preserved = TRUE,
    timestamps_utc = TRUE,
    duplicates_preserved = TRUE,
    max_observations = .gx_harmonized_max_observations,
    target_asset_sha256 = target_units$asset_sha256,
    wqp_timezone_asset_sha256 = timezone_asset$asset_sha256,
    counts = list(
      resources = unname(as.integer(nrow(resources))),
      timeseries_resources = unname(as.integer(sum(resources$timeseries))),
      native_only_resources = unname(as.integer(sum(!resources$timeseries))),
      observations = unname(as.integer(nrow(observations))),
      harmonized = unname(as.integer(sum(observations$harmonized))),
      unchanged = unname(as.integer(sum(!observations$harmonized)))
    ),
    limitations = limitations
  )
}

gx_harmonized_new_impl <- function(
    fetched,
    target_units,
    csv_mappings,
    feature_mappings) {
  gx_fetched_validate_impl(fetched)
  gx_target_units_validate_impl(target_units)
  csv_mappings <- gx_csv_mappings_normalize_impl(csv_mappings, fetched)
  feature_mappings <- gx_feature_mappings_normalize_impl(
    feature_mappings, fetched
  )
  observations <- gx_harmonized_observations_impl(
    fetched, target_units, csv_mappings, feature_mappings
  )
  resources <- gx_harmonized_resources_impl(fetched, observations)
  object <- structure(
    list(
      contract_version = .gx_harmonized_contract_version,
      fetched = fetched,
      target_units = target_units,
      csv_mappings = csv_mappings,
      feature_mappings = feature_mappings,
      observations = observations,
      resources = resources,
      metadata = gx_harmonized_metadata_impl(
        target_units, observations, resources
      )
    ),
    class = "gx_harmonized"
  )
  gx_harmonized_validate_impl(object)
  object
}

gx_harmonized_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_harmonized") &&
    identical(names(x), .gx_harmonized_fields) &&
    identical(x$contract_version, .gx_harmonized_contract_version)
  if (!valid_top) {
    gx_harmonize_abort(
      "Harmonized results violate their exact top-level contract."
    )
  }
  fetched_valid <- tryCatch({
    gx_fetched_validate_impl(x$fetched)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  targets_valid <- tryCatch({
    gx_target_units_validate_impl(x$target_units)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  normalized_mappings <- tryCatch(
    gx_csv_mappings_normalize_impl(x$csv_mappings, x$fetched),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  mappings_valid <- !is.null(normalized_mappings) &&
    identical(x$csv_mappings, normalized_mappings)
  normalized_feature_mappings <- tryCatch(
    gx_feature_mappings_normalize_impl(
      x$feature_mappings, x$fetched
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  feature_mappings_valid <- !is.null(normalized_feature_mappings) &&
    identical(x$feature_mappings, normalized_feature_mappings)
  if (!fetched_valid || !targets_valid || !mappings_valid ||
      !feature_mappings_valid) {
    gx_harmonize_abort(
      paste0(
        "Harmonized results contain an invalid fetched, target-unit, or ",
        "explicit-mapping object."
      )
    )
  }
  expected_observations <- gx_harmonized_observations_impl(
    x$fetched, x$target_units, x$csv_mappings, x$feature_mappings
  )
  expected_resources <- gx_harmonized_resources_impl(
    x$fetched, expected_observations
  )
  expected_metadata <- gx_harmonized_metadata_impl(
    x$target_units, expected_observations, expected_resources
  )
  valid_shapes <- identical(
    names(x$observations), .gx_harmonized_observation_columns
  ) && identical(
    names(x$resources), .gx_harmonized_resource_columns
  ) && is.list(x$metadata) &&
    identical(names(x$metadata), .gx_harmonized_metadata_fields) &&
    is.list(x$metadata$counts) &&
    identical(names(x$metadata$counts), .gx_harmonized_count_fields)
  if (!valid_shapes ||
      !identical(x$observations, expected_observations) ||
      !identical(x$resources, expected_resources) ||
      !identical(x$metadata, expected_metadata)) {
    gx_harmonize_abort(
      "Harmonized observations, resources, or metadata no longer match their inputs."
    )
  }
  invisible(x)
}

#' Harmonize reviewed fetched time-series observations
#'
#' Normalizes strict EDR position, current USGS continuous/daily,
#' single-characteristic WQP Result schemas with reviewed fixed-offset timezone
#' codes, explicitly mapped direct-CSV tables, and explicitly mapped OGC API
#' Features properties to a shared UTC observation table. Conversion occurs
#' only when one catalog variable and unit URI is unambiguous, native labels
#' exactly corroborate catalog labels, and one reviewed directed conversion
#' rule reaches the requested target. Unknown or conflicting mappings remain
#' unchanged with `harmonized = FALSE`.
#'
#' Unfiltered, mixed-characteristic, unknown-timezone, or incomplete WQP
#' results remain native-only. Unmapped or schema-incompatible direct CSV or
#' OGC API Features payloads remain losslessly available through `$fetched`
#' and are indexed as native-only `$resources`. Feature identifiers and
#' geometry never supply observation semantics. Timestamps are UTC, daily
#' dates become UTC midnight, qualifiers and duplicate timestamps are
#' preserved, and no network or optional-package work occurs.
#'
#' @param fetched A validated `gx_fetched` object from [gx_fetch()].
#' @param target_units A validated target selection from [gx_target_units()].
#' @param csv_mappings An empty list, one [gx_csv_mapping()] object, or an
#'   unnamed list of mappings for distinct planned direct-CSV distributions.
#' @param feature_mappings An empty list, one [gx_feature_mapping()] object, or
#'   an unnamed list of mappings for distinct planned OGC API Features
#'   distributions.
#'
#' @return A validated `gx_harmonized` object containing `$observations`,
#'   `$resources`, normalized `$csv_mappings` and `$feature_mappings`, and the
#'   exact original `$fetched` object.
#' @export
gx_harmonize <- function(
    fetched,
    target_units = gx_target_units(),
    csv_mappings = list(),
    feature_mappings = list()) {
  gx_harmonized_new_impl(
    fetched, target_units, csv_mappings, feature_mappings
  )
}

#' @export
print.gx_harmonized <- function(x, ...) {
  gx_harmonized_validate_impl(x)
  counts <- x$metadata$counts
  cli::cli_inform(c(
    "<gx_harmonized>",
    paste0(
      "* Observations: ", counts$observations,
      "; harmonized: ", counts$harmonized,
      "; unchanged: ", counts$unchanged
    ),
    paste0(
      "* Resources: ", counts$resources,
      "; native-only: ", counts$native_only_resources
    ),
    paste0(
      "* Scope: reviewed EDR, current USGS, and aligned reviewed-zone WQP ",
      "plus explicitly mapped CSV and OGC Features time series; offline"
    )
  ))
  invisible(x)
}
