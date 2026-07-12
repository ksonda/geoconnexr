gx_assert_character_ids <- function(x, what) {
  if (!is.character(x)) {
    gx_abort(
      "{.arg {what}} must be character so leading zeroes are preserved.",
      "gx_error_identifier"
    )
  }
  if (!length(x) || anyNA(x) || any(!nzchar(x))) {
    gx_abort(
      "{.arg {what}} must contain at least one non-missing identifier.",
      "gx_error_identifier"
    )
  }
  x
}

#' Validate hydrologic unit codes
#'
#' @param x A character vector. Numeric input is rejected because it cannot
#'   preserve leading zeroes.
#'
#' @return `x`, invisibly, after validation.
#' @export
gx_validate_huc <- function(x) {
  x <- gx_assert_character_ids(x, "x")
  valid_length <- nchar(x) %in% c(2L, 4L, 6L, 8L, 10L, 12L)
  valid_digits <- grepl("^[0-9]+$", x)
  if (any(!valid_length | !valid_digits)) {
    gx_abort(
      "HUCs must contain 2, 4, 6, 8, 10, or 12 digits.",
      "gx_error_identifier"
    )
  }
  invisible(x)
}

#' Validate NHDPlus COMIDs
#'
#' @param x A character vector containing positive integer identifiers.
#'
#' @return `x`, invisibly, after validation.
#' @export
gx_validate_comid <- function(x) {
  x <- gx_assert_character_ids(x, "x")
  if (any(!grepl("^[1-9][0-9]*$", x))) {
    gx_abort(
      "COMIDs must be positive integer identifiers supplied as character values.",
      "gx_error_identifier"
    )
  }
  invisible(x)
}

#' Compute a versioned contract fingerprint
#'
#' Creates a deterministic SHA-256 fingerprint from a namespace and a vector
#' of scalar values. Values are UTF-8 encoded and length-prefixed; missing
#' values use a distinct marker. This helper is intended for contract keys,
#' not cryptographic authentication.
#'
#' @param values An atomic vector or list of scalar atomic values.
#' @param namespace A non-empty scalar character namespace.
#' @param contract_version A non-empty scalar contract version.
#'
#' @return A lowercase hexadecimal SHA-256 string.
#' @export
gx_contract_hash <- function(values, namespace, contract_version = "0.1.0") {
  if (!is.character(namespace) || length(namespace) != 1L ||
      is.na(namespace) || !nzchar(namespace)) {
    gx_abort("{.arg namespace} must be one non-empty string.", "gx_error_contract")
  }
  if (!is.character(contract_version) || length(contract_version) != 1L ||
      is.na(contract_version) || !nzchar(contract_version)) {
    gx_abort(
      "{.arg contract_version} must be one non-empty string.",
      "gx_error_contract"
    )
  }

  if (is.atomic(values) && !is.list(values)) {
    values <- as.list(values)
  }
  if (!is.list(values) || any(lengths(values) != 1L)) {
    gx_abort("{.arg values} must contain scalar atomic values.", "gx_error_contract")
  }

  encode_one <- function(value) {
    if (length(value) != 1L || is.list(value)) {
      gx_abort("Each contract value must be scalar and atomic.", "gx_error_contract")
    }
    type <- typeof(value)
    if (is.na(value)) {
      return(paste0(type, ":NA:"))
    }
    text <- enc2utf8(as.character(value))
    paste0(type, ":", nchar(text, type = "bytes"), ":", text)
  }

  payload <- paste(
    c(
      paste0("contract:", enc2utf8(contract_version)),
      paste0("namespace:", enc2utf8(namespace)),
      vapply(values, encode_one, character(1))
    ),
    collapse = "\n"
  )
  digest::digest(payload, algo = "sha256", serialize = FALSE)
}
