gx_http_iri_parts <- function(x, maximum_bytes = 8192L) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x) ||
      !isTRUE(stringi::stri_enc_isutf8(x)) ||
      nchar(enc2utf8(x), type = "bytes") > maximum_bytes ||
      !grepl("^https?://[^/]", x, ignore.case = TRUE, perl = TRUE) ||
      grepl("[<>\"{}|^`\\\\]", x) ||
      grepl("%(?![0-9A-Fa-f]{2})", x, perl = TRUE) ||
      isTRUE(stringi::stri_detect_regex(x, "[\\p{Z}\\p{Cc}\\p{Cf}\\p{Cs}]"))) {
    return(NULL)
  }

  parsed <- tryCatch(
    unclass(httr2::url_parse(x)),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(parsed) || !tolower(parsed$scheme %||% "") %in% c("http", "https") ||
      !gx_query_scalar_character(parsed$hostname %||% "") ||
      !is.null(parsed$username) || !is.null(parsed$password)) {
    return(NULL)
  }
  parsed
}

gx_is_http_uri <- function(x) {
  !is.null(gx_http_iri_parts(x))
}

gx_encode_uri <- function(x, spec = list(maximum_bytes = 8192L)) {
  maximum_bytes <- spec$maximum_bytes %||% 8192L
  if (!gx_query_whole_number(maximum_bytes, 1L, 8192L) ||
      is.null(gx_http_iri_parts(x, maximum_bytes))) {
    gx_abort(
      "HTTP IRI parameters must be valid, credential-free absolute HTTP(S) IRIs.",
      "gx_error_query_parameter"
    )
  }
  paste0("<", enc2utf8(x), ">")
}

gx_utf8_byte_sort_key <- function(x) {
  vapply(
    enc2utf8(x),
    function(value) paste(sprintf("%02x", as.integer(charToRaw(value))), collapse = ""),
    character(1),
    USE.NAMES = FALSE
  )
}

gx_encode_uri_list <- function(x, spec) {
  if (!is.character(x) || !length(x) || anyNA(x) ||
      !all(stringi::stri_enc_isutf8(x))) {
    gx_abort(
      "HTTP IRI-list parameters must be non-missing character vectors.",
      "gx_error_query_parameter"
    )
  }
  x <- enc2utf8(x)
  minimum <- spec$minimum_items %||% 1L
  maximum <- spec$maximum_items %||% 200L
  if (length(x) < minimum || length(x) > maximum) {
    gx_abort(
      "HTTP IRI-list parameter length must be between {minimum} and {maximum}.",
      "gx_error_query_parameter"
    )
  }
  if (isTRUE(spec$unique_items %||% TRUE) && anyDuplicated(x)) {
    gx_abort(
      "HTTP IRI-list parameters may not contain duplicate lexical IRIs.",
      "gx_error_query_parameter"
    )
  }
  if (identical(spec$sort %||% "bytewise", "bytewise")) {
    x <- x[order(gx_utf8_byte_sort_key(x), method = "radix")]
  }

  item_spec <- list(maximum_bytes = spec$item_maximum_bytes %||% 8192L)
  encoded <- paste(
    vapply(x, gx_encode_uri, character(1), spec = item_spec),
    collapse = " "
  )
  maximum_bytes <- spec$encoded_maximum_bytes %||% 65536L
  if (nchar(encoded, type = "bytes") > maximum_bytes) {
    gx_abort(
      "Encoded HTTP IRI-list parameters exceed their byte ceiling.",
      "gx_error_query_parameter"
    )
  }
  encoded
}

gx_encode_integer <- function(x, spec) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) || is.logical(x) ||
      !is.finite(x) || x != trunc(x)) {
    gx_abort(
      "Integer parameters must be one finite whole number.",
      "gx_error_query_parameter"
    )
  }
  minimum <- spec$minimum %||% -.Machine$integer.max
  maximum <- spec$maximum %||% .Machine$integer.max
  if (x < minimum || x > maximum) {
    gx_abort(
      "Integer parameter must be between {minimum} and {maximum}.",
      "gx_error_query_parameter"
    )
  }
  if (identical(x, 0) || x == 0) {
    return("0")
  }
  formatC(x, format = "f", digits = 0L, decimal.mark = ".")
}

gx_encode_literal <- function(x, spec = list(maximum_bytes = 131072L)) {
  maximum_bytes <- spec$maximum_bytes %||% 131072L
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !isTRUE(stringi::stri_enc_isutf8(x)) ||
      nchar(enc2utf8(x), type = "bytes") > maximum_bytes) {
    gx_abort(
      "Literal parameters must be one valid UTF-8 string within their byte ceiling.",
      "gx_error_query_parameter"
    )
  }
  as.character(jsonlite::toJSON(enc2utf8(x), auto_unbox = TRUE, pretty = FALSE))
}

gx_encode_datetime <- function(x) {
  if (inherits(x, "POSIXt")) {
    numeric_time <- suppressWarnings(as.numeric(x))
    if (length(x) != 1L || length(numeric_time) != 1L ||
        is.na(numeric_time) || !is.finite(numeric_time) ||
        numeric_time != trunc(numeric_time)) {
      gx_abort(
        "Datetime parameters must identify one finite whole UTC second.",
        "gx_error_query_parameter"
      )
    }
    value <- format(
      as.POSIXct(numeric_time, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC",
      usetz = FALSE
    )
  } else {
    if (!is.character(x) || length(x) != 1L || is.na(x) ||
        !isTRUE(stringi::stri_enc_isutf8(x)) ||
        !grepl(
          "^[0-9]{4}-(0[1-9]|1[0-2])-([0-2][0-9]|3[01])T([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]Z$",
          x,
          perl = TRUE
        )) {
      gx_abort(
        "Datetime parameters must use canonical UTC YYYY-MM-DDTHH:MM:SSZ form.",
        "gx_error_query_parameter"
      )
    }
    parsed <- suppressWarnings(as.POSIXct(
      x,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ))
    if (is.na(parsed) || !identical(
      format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC", usetz = FALSE),
      x
    )) {
      gx_abort(
        "Datetime parameter is not a valid Gregorian UTC date and time.",
        "gx_error_query_parameter"
      )
    }
    value <- x
  }
  paste0(
    gx_encode_literal(value),
    "^^<http://www.w3.org/2001/XMLSchema#dateTime>"
  )
}

gx_encode_crs84_wkt <- function(x, spec) {
  maximum_bytes <- spec$maximum_bytes %||% 131072L
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x) ||
      !isTRUE(stringi::stri_enc_isutf8(x)) ||
      nchar(enc2utf8(x), type = "bytes") > maximum_bytes ||
      grepl("^[[:space:]]*(<|SRID[[:space:]]*=)", x, ignore.case = TRUE) ||
      !grepl(
        "^[[:space:]]*(POLYGON|MULTIPOLYGON)([[:space:]]+(Z|M|ZM))?[[:space:]]*(\\(|EMPTY[[:space:]]*$)",
        x,
        ignore.case = TRUE,
        perl = TRUE
      ) ||
      !grepl("^[A-Za-z0-9_.,() +eE\\t\\r\\n-]+$", x, perl = TRUE) ||
      grepl("(^|[^A-Za-z0-9_])(NAN|INF(INITY)?)([^A-Za-z0-9_]|$)", x,
        ignore.case = TRUE, perl = TRUE) ||
      grepl(
        "[A-DF-Za-df-z]",
        gsub("(?i)MULTIPOLYGON|POLYGON|EMPTY|E", "", x, perl = TRUE),
        perl = TRUE
      )) {
    gx_abort(
      "CRS84 WKT parameters must be one implicit-CRS geometry literal.",
      "gx_error_query_parameter"
    )
  }

  geometry <- tryCatch(
    suppressMessages(suppressWarnings(sf::st_as_sfc(x, crs = "OGC:CRS84"))),
    error = function(e) NULL
  )
  allowed <- spec$geometry_types %||% c("POLYGON", "MULTIPOLYGON")
  geometry_type <- if (is.null(geometry)) character() else tryCatch(
    as.character(sf::st_geometry_type(geometry, by_geometry = TRUE)),
    error = function(e) character()
  )
  empty <- if (is.null(geometry)) TRUE else tryCatch(
    isTRUE(sf::st_is_empty(geometry)[[1]]),
    error = function(e) TRUE
  )
  coordinates <- if (is.null(geometry)) NULL else tryCatch(
    suppressWarnings(sf::st_coordinates(geometry)),
    error = function(e) NULL
  )
  valid_geometry <- if (is.null(geometry)) FALSE else tryCatch(
    isTRUE(suppressMessages(suppressWarnings(sf::st_is_valid(geometry)))[[1]]),
    error = function(e) FALSE
  )
  rings_closed <- FALSE
  if (!is.null(coordinates) && nrow(coordinates)) {
    level_columns <- grep("^L[0-9]+$", colnames(coordinates), value = TRUE)
    coordinate_columns <- intersect(c("X", "Y", "Z", "M"), colnames(coordinates))
    groups <- if (length(level_columns)) {
      interaction(
        as.data.frame(coordinates[, level_columns, drop = FALSE]),
        drop = TRUE,
        lex.order = TRUE
      )
    } else {
      factor(rep(1L, nrow(coordinates)))
    }
    rings_closed <- all(vapply(
      split(seq_len(nrow(coordinates)), groups),
      function(index) {
        length(index) >= 4L && identical(
          unname(coordinates[index[[1]], coordinate_columns, drop = TRUE]),
          unname(coordinates[index[[length(index)]], coordinate_columns, drop = TRUE])
        )
      },
      logical(1)
    ))
  }
  if (is.null(geometry) || length(geometry) != 1L ||
      length(geometry_type) != 1L || !geometry_type %in% allowed ||
      (empty && !isTRUE(spec$allow_empty)) || !valid_geometry ||
      is.null(coordinates) || (!empty && !rings_closed) ||
      (!empty && (!nrow(coordinates) || any(!is.finite(coordinates[, c("X", "Y"), drop = FALSE]))))) {
    gx_abort(
      "CRS84 WKT must be a finite allowed polygonal geometry.",
      "gx_error_query_parameter"
    )
  }

  paste0(
    gx_encode_literal(enc2utf8(x), list(maximum_bytes = maximum_bytes)),
    "^^<http://www.opengis.net/ont/geosparql#wktLiteral>"
  )
}

gx_encode_wkt <- function(x) {
  gx_encode_crs84_wkt(
    x,
    list(
      maximum_bytes = 131072L,
      geometry_types = c("POLYGON", "MULTIPOLYGON"),
      allow_empty = FALSE
    )
  )
}

gx_encode_parameter <- function(value, spec) {
  switch(
    spec$type,
    http_iri = gx_encode_uri(value, spec),
    http_iri_list = gx_encode_uri_list(value, spec),
    integer = gx_encode_integer(value, spec),
    crs84_wkt_literal = gx_encode_crs84_wkt(value, spec),
    literal = gx_encode_literal(value, spec),
    datetime = gx_encode_datetime(value),
    gx_abort(
      "Unsupported query parameter type {.val {spec$type}}.",
      "gx_error_query_manifest"
    )
  )
}
