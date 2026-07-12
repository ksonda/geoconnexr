#' Define an identifier-based area of interest
#'
#' The P0 implementation supports HUCs, five-digit county FIPS codes, and
#' two-letter state abbreviations. Spatial and upstream-basin inputs will be
#' added after their recipe contracts pass the architecture spike.
#'
#' @param x One character identifier.
#' @param type One of `"auto"`, `"huc"`, `"county"`, or `"state"`.
#'
#' @return An object of class `gx_aoi`.
#' @export
gx_aoi <- function(x, type = c("auto", "huc", "county", "state")) {
  type <- match.arg(type)
  x <- gx_assert_character_ids(x, "x")
  if (length(x) != 1L) {
    gx_abort("{.arg x} must contain exactly one AOI identifier.", "gx_error_aoi")
  }

  if (identical(type, "auto")) {
    if (grepl("^[0-9]+$", x) && nchar(x) %in% c(2L, 4L, 6L, 8L, 10L, 12L)) {
      type <- "huc"
    } else if (grepl("^[0-9]{5}$", x)) {
      type <- "county"
    } else if (grepl("^[A-Za-z]{2}$", x)) {
      type <- "state"
    } else {
      gx_abort(
        "Could not infer the AOI type; supply {.arg type} explicitly.",
        "gx_error_aoi"
      )
    }
  }

  if (identical(type, "huc")) {
    gx_validate_huc(x)
  } else if (identical(type, "county")) {
    if (!grepl("^[0-9]{5}$", x)) {
      gx_abort("County FIPS must contain exactly five digits.", "gx_error_aoi")
    }
  } else if (identical(type, "state")) {
    if (!grepl("^[A-Za-z]{2}$", x)) {
      gx_abort("State identifiers must be two letters.", "gx_error_aoi")
    }
    x <- toupper(x)
  }

  structure(
    list(
      contract_version = "1.0.0",
      type = type,
      id = x,
      recipe = list(
        contract_version = "1.0.0",
        aoi = list(
          kind = type,
          identifier = x
        ),
        pipeline = list(
          start_stage = "aoi",
          end_stage = "catalog"
        )
      )
    ),
    class = "gx_aoi"
  )
}

#' @export
print.gx_aoi <- function(x, ...) {
  cli::cli_inform(c(
    "<gx_aoi>",
    "* Type: {x$type}",
    "* Identifier: {x$id}",
    "* Contract: {x$contract_version}"
  ))
  invisible(x)
}
