gx_is_http_uri <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^https?://", x, ignore.case = TRUE) &&
    !grepl("[<>\"{}|^`\\\\]", x) &&
    !grepl("[[:space:][:cntrl:]]", x)
}

gx_encode_uri <- function(x) {
  if (!gx_is_http_uri(x) || nchar(enc2utf8(x), type = "bytes") > 8192L) {
    gx_abort("URI parameters must be safe absolute HTTP(S) URIs.", "gx_error_query_parameter")
  }
  paste0("<", x, ">")
}

gx_encode_uri_list <- function(x, spec) {
  if (!is.character(x) || !length(x) || anyNA(x)) {
    gx_abort("URI-list parameters must be non-missing character vectors.", "gx_error_query_parameter")
  }
  minimum <- spec$minimum_items %||% 1L
  maximum <- spec$maximum_items %||% 200L
  if (length(x) < minimum || length(x) > maximum) {
    gx_abort(
      "URI-list parameter length must be between {minimum} and {maximum}.",
      "gx_error_query_parameter"
    )
  }
  encoded <- paste(vapply(x, gx_encode_uri, character(1)), collapse = " ")
  if (nchar(encoded, type = "bytes") > 65536L) {
    gx_abort("Encoded URI-list parameters may not exceed 64 KiB.", "gx_error_query_parameter")
  }
  encoded
}

gx_encode_integer <- function(x, spec) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) || x != trunc(x)) {
    gx_abort("Integer parameters must be one finite whole number.", "gx_error_query_parameter")
  }
  minimum <- spec$minimum %||% -Inf
  maximum <- spec$maximum %||% Inf
  if (!is.finite(x) || x < minimum || x > maximum) {
    gx_abort(
      "Integer parameter must be between {minimum} and {maximum}.",
      "gx_error_query_parameter"
    )
  }
  format(x, scientific = FALSE, trim = TRUE)
}

gx_encode_wkt <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    gx_abort("WKT parameters must be one non-empty string.", "gx_error_query_parameter")
  }
  if (nchar(enc2utf8(x), type = "bytes") > 131072L) {
    gx_abort("WKT parameters may not exceed 128 KiB.", "gx_error_query_parameter")
  }
  valid_type <- grepl(
    "^(POINT|LINESTRING|POLYGON|MULTIPOINT|MULTILINESTRING|MULTIPOLYGON|GEOMETRYCOLLECTION)( Z| M| ZM)?( EMPTY| ?\\()",
    x,
    ignore.case = TRUE
  )
  valid_chars <- grepl("^[A-Za-z0-9_.,() +eE-]+$", x)
  if (!valid_type || !valid_chars) {
    gx_abort(
      "WKT contains an unsupported geometry type or unsafe characters.",
      "gx_error_query_parameter"
    )
  }
  paste0(
    "\"", x, "\"^^<http://www.opengis.net/ont/geosparql#wktLiteral>"
  )
}

gx_encode_parameter <- function(value, spec) {
  switch(
    spec$type,
    uri = gx_encode_uri(value),
    uri_list = gx_encode_uri_list(value, spec),
    integer = gx_encode_integer(value, spec),
    wkt = gx_encode_wkt(value),
    gx_abort(
      "Unsupported query parameter type {.val {spec$type}}.",
      "gx_error_query_manifest"
    )
  )
}
