gx_aoi_recipe_max_depth <- 7L

gx_aoi_recipe_max_json_bytes <- function() {
  gx_aoi_max_serialized_bytes + 4096L
}

gx_aoi_recipe_max_members <- function() {
  3L * gx_aoi_max_coordinates +
    2L * floor(gx_aoi_max_coordinates / 4L) + 11L
}

gx_aoi_recipe_max_structural_units <- function() {
  3L * gx_aoi_max_coordinates +
    2L * floor(gx_aoi_max_coordinates / 4L) + 22L
}

gx_aoi_abort_recipe <- function(
    message,
    subclass = "gx_error_aoi_recipe_contract",
    call = rlang::caller_env()) {
  gx_abort(
    message,
    c(subclass, "gx_error_aoi_recipe", "gx_error_aoi"),
    call = call,
    .redact_trace = TRUE
  )
}

gx_aoi_recipe_named_object <- function(x, label) {
  object_names <- attr(x, "names", exact = TRUE)
  attrs <- attributes(x)
  valid <- typeof(x) == "list" && !is.object(x) &&
    is.list(attrs) && identical(names(attrs), "names") &&
    identical(attrs$names, object_names) &&
    !is.null(object_names) && !anyNA(object_names) &&
    all(nzchar(object_names)) && !anyDuplicated(object_names)
  if (!isTRUE(valid)) {
    gx_aoi_abort_recipe(
      "{label} must be a plain JSON object with unique non-empty members.",
      "gx_error_aoi_recipe_structure"
    )
  }
  x
}

gx_aoi_recipe_plain_object <- function(x, expected, label) {
  x <- gx_aoi_recipe_named_object(x, label)
  if (length(x) != length(expected) || !setequal(names(x), expected)) {
    gx_aoi_abort_recipe(
      "{label} must contain exactly its declared object members.",
      "gx_error_aoi_recipe_structure"
    )
  }
  unname(x[expected]) |>
    stats::setNames(expected)
}

gx_aoi_recipe_plain_array <- function(x, label, minimum = 0L) {
  valid <- typeof(x) == "list" && !is.object(x) &&
    is.null(attributes(x)) &&
    length(x) >= minimum
  if (!isTRUE(valid)) {
    gx_aoi_abort_recipe(
      "{label} must be an unnamed JSON array with at least {minimum} member{?s}.",
      "gx_error_aoi_recipe_structure"
    )
  }
  x
}

gx_aoi_recipe_string <- function(x, label) {
  valid <- is.character(x) && !is.object(x) && length(x) == 1L &&
    is.null(attributes(x)) && !is.na(x) && nzchar(x) &&
    isTRUE(stringi::stri_enc_isutf8(x))
  if (!isTRUE(valid)) {
    gx_aoi_abort_recipe(
      "{label} must be one non-empty UTF-8 JSON string.",
      "gx_error_aoi_recipe_structure"
    )
  }
  x
}

gx_aoi_recipe_assert_complexity <- function(value) {
  members <- 0
  atomic_bytes <- 0
  deepest <- 0L
  stack <- list(list(value = value, depth = 1L))
  while (length(stack)) {
    item <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL

    current <- item$value
    if (typeof(current) != "list") {
      valid_atomic <- !is.object(current) && is.null(attributes(current)) &&
        typeof(current) %in% c(
          "NULL", "logical", "integer", "double", "character"
        )
      if (!isTRUE(valid_atomic)) {
        gx_aoi_abort_recipe(
          "AOI recipe values must be plain decoded JSON values.",
          "gx_error_aoi_recipe_structure"
        )
      }
      next
    }

    if (is.object(current)) {
      gx_aoi_abort_recipe(
        "AOI recipe values must be plain decoded JSON values.",
        "gx_error_aoi_recipe_structure"
      )
    }
    attrs <- attributes(current)
    attrs_valid <- is.null(attrs) || (
      typeof(attrs) == "list" && length(attrs) == 1L &&
        identical(names(attrs), "names")
    )
    if (!isTRUE(attrs_valid)) {
      gx_aoi_abort_recipe(
        "AOI recipe values must be plain decoded JSON values.",
        "gx_error_aoi_recipe_structure"
      )
    }
    object_names <- if (is.null(attrs)) NULL else attrs[[1L]]
    valid_names <- is.null(object_names) || (
      is.character(object_names) && !is.object(object_names) &&
        is.null(attributes(object_names)) &&
        length(object_names) == length(current) &&
        !anyNA(object_names) &&
        all(stringi::stri_enc_isutf8(object_names))
    )
    if (!isTRUE(valid_names)) {
      gx_aoi_abort_recipe(
        "AOI recipe object names must be plain valid UTF-8 strings.",
        "gx_error_aoi_recipe_structure"
      )
    }
    if (!is.null(object_names) && anyDuplicated(object_names)) {
      gx_aoi_abort_recipe(
        "AOI recipe JSON contains duplicate object members.",
        "gx_error_aoi_recipe_structure"
      )
    }

    deepest <- max(deepest, item$depth)
    members <- members + length(current)
    if (deepest > gx_aoi_recipe_max_depth ||
        members > gx_aoi_recipe_max_members()) {
      gx_aoi_abort_recipe(
        "AOI recipe JSON exceeds its bounded depth or member contract.",
        "gx_error_aoi_recipe_budget"
      )
    }
    if (!is.null(object_names)) {
      atomic_bytes <- atomic_bytes + gx_json_atomic_bytes(object_names)
      if (atomic_bytes > gx_aoi_recipe_max_json_bytes()) {
        gx_aoi_abort_recipe(
          "AOI recipe JSON exceeds its bounded atomic-byte contract.",
          "gx_error_aoi_recipe_budget"
        )
      }
    }

    for (index in seq_along(current)) {
      child <- current[[index]]
      if (typeof(child) == "list") {
        stack[[length(stack) + 1L]] <- list(
          value = child,
          depth = item$depth + 1L
        )
        next
      }
      valid_atomic <- !is.object(child) && is.null(attributes(child)) &&
        typeof(child) %in% c(
          "NULL", "logical", "integer", "double", "character"
        )
      if (!isTRUE(valid_atomic)) {
        gx_aoi_abort_recipe(
          "AOI recipe values must be plain decoded JSON values.",
          "gx_error_aoi_recipe_structure"
        )
      }
      if (length(child) > 1L) {
        if (item$depth + 1L > gx_aoi_recipe_max_depth) {
          gx_aoi_abort_recipe(
            "AOI recipe JSON exceeds its bounded depth contract.",
            "gx_error_aoi_recipe_budget"
          )
        }
        members <- members + length(child)
        if (members > gx_aoi_recipe_max_members()) {
          gx_aoi_abort_recipe(
            "AOI recipe JSON exceeds its bounded member contract.",
            "gx_error_aoi_recipe_budget"
          )
        }
      }
      atomic_bytes <- atomic_bytes + gx_json_atomic_bytes(child)
      if (atomic_bytes > gx_aoi_recipe_max_json_bytes()) {
        gx_aoi_abort_recipe(
          "AOI recipe JSON exceeds its bounded atomic-byte contract.",
          "gx_error_aoi_recipe_budget"
        )
      }
    }
  }
  invisible(list(
    exceeded = NA_character_,
    members = members,
    atomic_bytes = atomic_bytes,
    depth = deepest
  ))
}

gx_aoi_recipe_json_bytes <- function(json) {
  if (is.character(json)) {
    valid <- !is.object(json) && is.null(attributes(json)) &&
      length(json) == 1L && !is.na(json) &&
      isTRUE(stringi::stri_enc_isutf8(json))
    if (!valid) {
      gx_aoi_abort_recipe(
        "AOI recipe JSON must be raw bytes or one valid UTF-8 string.",
        "gx_error_aoi_recipe_input"
      )
    }
    size <- nchar(enc2utf8(json), type = "bytes")
    if (!is.finite(size) || size > gx_aoi_recipe_max_json_bytes()) {
      gx_aoi_abort_recipe(
        "AOI recipe JSON exceeds its serialized-byte ceiling.",
        "gx_error_aoi_recipe_budget"
      )
    }
    json <- charToRaw(enc2utf8(json))
  } else if (!is.raw(json) || is.object(json) ||
             !is.null(attributes(json))) {
    gx_aoi_abort_recipe(
      "AOI recipe JSON must be raw bytes or one valid UTF-8 string.",
      "gx_error_aoi_recipe_input"
    )
  }

  if (length(json) > gx_aoi_recipe_max_json_bytes()) {
    gx_aoi_abort_recipe(
      "AOI recipe JSON exceeds its serialized-byte ceiling.",
      "gx_error_aoi_recipe_budget"
    )
  }
  utf8_bom <- length(json) >= 3L &&
    identical(as.integer(json[seq_len(3L)]), c(239L, 187L, 191L))
  utf16_bom <- length(json) >= 2L && (
    identical(as.integer(json[seq_len(2L)]), c(255L, 254L)) ||
      identical(as.integer(json[seq_len(2L)]), c(254L, 255L))
  )
  utf32be_bom <- length(json) >= 4L &&
    identical(as.integer(json[seq_len(4L)]), c(0L, 0L, 254L, 255L))
  if (utf8_bom || utf16_bom || utf32be_bom) {
    gx_aoi_abort_recipe(
      "AOI recipe JSON must be unmarked UTF-8.",
      "gx_error_aoi_recipe_encoding"
    )
  }
  if (any(as.integer(json) == 0L)) {
    gx_aoi_abort_recipe(
      "AOI recipe JSON contains a NUL byte.",
      "gx_error_aoi_recipe_syntax"
    )
  }
  json
}

gx_aoi_recipe_json_text <- function(json) {
  bytes <- gx_aoi_recipe_json_bytes(json)
  text <- tryCatch(
    rawToChar(bytes),
    error = function(cnd) NA_character_
  )
  valid <- if (is.na(text)) {
    NA_character_
  } else {
    iconv(text, from = "UTF-8", to = "UTF-8", sub = NA_character_)
  }
  if (length(valid) != 1L || is.na(valid)) {
    gx_aoi_abort_recipe(
      "AOI recipe JSON is not valid UTF-8.",
      "gx_error_aoi_recipe_encoding"
    )
  }
  valid <- enc2utf8(valid)
  if (!identical(charToRaw(valid), bytes)) {
    gx_aoi_abort_recipe(
      "AOI recipe JSON did not survive exact UTF-8 decoding.",
      "gx_error_aoi_recipe_encoding"
    )
  }
  valid
}

gx_aoi_recipe_json_preflight <- function(text) {
  bytes <- as.integer(charToRaw(text))
  stack <- integer()
  in_string <- FALSE
  structural <- 0
  structural_limit <- gx_aoi_recipe_max_structural_units()
  hex_value <- function(value) {
    if (value >= 48L && value <= 57L) return(value - 48L)
    if (value >= 65L && value <= 70L) return(value - 55L)
    if (value >= 97L && value <= 102L) return(value - 87L)
    NA_integer_
  }
  unicode_escape <- function(start) {
    end <- start + 3L
    if (end > length(bytes)) return(NA_integer_)
    digits <- vapply(bytes[start:end], hex_value, integer(1))
    if (anyNA(digits)) return(NA_integer_)
    sum(digits * c(4096L, 256L, 16L, 1L))
  }

  index <- 1L
  while (index <= length(bytes)) {
    byte <- bytes[[index]]
    if (in_string) {
      if (byte < 32L) {
        gx_aoi_abort_recipe(
          "AOI recipe JSON strings contain an unescaped control byte.",
          "gx_error_aoi_recipe_syntax"
        )
      }
      if (byte == 34L) {
        in_string <- FALSE
        index <- index + 1L
        next
      }
      if (byte != 92L) {
        index <- index + 1L
        next
      }

      if (index == length(bytes)) {
        gx_aoi_abort_recipe(
          "AOI recipe JSON contains an incomplete string escape.",
          "gx_error_aoi_recipe_syntax"
        )
      }
      escaped <- bytes[[index + 1L]]
      if (escaped %in% c(34L, 47L, 92L, 98L, 102L, 110L, 114L, 116L)) {
        index <- index + 2L
        next
      }
      if (escaped != 117L) {
        gx_aoi_abort_recipe(
          "AOI recipe JSON contains an invalid string escape.",
          "gx_error_aoi_recipe_syntax"
        )
      }

      codepoint <- unicode_escape(index + 2L)
      if (is.na(codepoint) || codepoint <= 31L) {
        gx_aoi_abort_recipe(
          "AOI recipe JSON contains an invalid or control Unicode escape.",
          "gx_error_aoi_recipe_syntax"
        )
      }
      index <- index + 6L
      if (codepoint >= 55296L && codepoint <= 56319L) {
        paired <- index + 5L <= length(bytes) &&
          bytes[[index]] == 92L && bytes[[index + 1L]] == 117L
        low <- if (paired) unicode_escape(index + 2L) else NA_integer_
        if (is.na(low) || low < 56320L || low > 57343L) {
          gx_aoi_abort_recipe(
            "AOI recipe JSON contains an unpaired Unicode surrogate.",
            "gx_error_aoi_recipe_syntax"
          )
        }
        index <- index + 6L
      } else if (codepoint >= 56320L && codepoint <= 57343L) {
        gx_aoi_abort_recipe(
          "AOI recipe JSON contains an unpaired Unicode surrogate.",
          "gx_error_aoi_recipe_syntax"
        )
      }
      next
    }
    if (byte == 34L) {
      in_string <- TRUE
    } else if (byte %in% c(91L, 123L)) {
      stack <- c(stack, byte)
      structural <- structural + 1
      if (length(stack) > gx_aoi_recipe_max_depth) {
        gx_aoi_abort_recipe(
          "AOI recipe JSON exceeds its nesting-depth ceiling.",
          "gx_error_aoi_recipe_budget"
        )
      }
    } else if (byte %in% c(93L, 125L)) {
      expected <- if (byte == 93L) 91L else 123L
      if (!length(stack) || utils::tail(stack, 1L) != expected) {
        gx_aoi_abort_recipe(
          "AOI recipe JSON has unbalanced delimiters.",
          "gx_error_aoi_recipe_syntax"
        )
      }
      stack <- utils::head(stack, -1L)
    } else if (byte %in% c(44L, 58L)) {
      structural <- structural + 1
    }
    if (structural > structural_limit) {
      gx_aoi_abort_recipe(
        "AOI recipe JSON exceeds its structural-member ceiling.",
        "gx_error_aoi_recipe_budget"
      )
    }
    index <- index + 1L
  }

  if (in_string || length(stack)) {
    gx_aoi_abort_recipe(
      "AOI recipe JSON is incomplete.",
      "gx_error_aoi_recipe_syntax"
    )
  }
  invisible(text)
}

gx_aoi_recipe_parse_json <- function(json) {
  text <- gx_aoi_recipe_json_text(json)
  gx_aoi_recipe_json_preflight(text)
  value <- tryCatch(
    jsonlite::parse_json(
      text,
      simplifyVector = FALSE,
      bigint_as_char = TRUE
    ),
    error = function(cnd) {
      gx_aoi_abort_recipe(
        "AOI recipe JSON could not be parsed.",
        "gx_error_aoi_recipe_syntax"
      )
    }
  )
  gx_aoi_recipe_assert_complexity(value)
  value
}

gx_aoi_recipe_position <- function(position, state) {
  if (is.list(position)) {
    position <- gx_aoi_recipe_plain_array(
      position,
      "Each AOI coordinate position",
      minimum = 2L
    )
    if (length(position) != 2L ||
        !all(vapply(position, function(value) {
          is.numeric(value) && !is.object(value) && length(value) == 1L &&
            is.null(attributes(value)) && !is.na(value) && is.finite(value)
        }, logical(1)))) {
      gx_aoi_abort_recipe(
        "Each AOI coordinate position must contain exactly two finite JSON numbers.",
        "gx_error_aoi_recipe_geometry"
      )
    }
    position <- unlist(position, recursive = FALSE, use.names = FALSE)
  } else {
    valid <- is.numeric(position) && !is.object(position) &&
      length(position) == 2L && is.null(attributes(position)) &&
      !anyNA(position) && all(is.finite(position))
    if (!isTRUE(valid)) {
      gx_aoi_abort_recipe(
        "Each AOI coordinate position must contain exactly two finite JSON numbers.",
        "gx_error_aoi_recipe_geometry"
      )
    }
  }

  if (state$coordinates >= gx_aoi_max_coordinates) {
    gx_aoi_abort_recipe(
      "AOI recipe geometry exceeds its coordinate-position ceiling.",
      "gx_error_aoi_recipe_budget"
    )
  }
  position <- unname(as.double(position))
  if (position[[1L]] < -180 || position[[1L]] > 180 ||
      position[[2L]] < -90 || position[[2L]] > 90) {
    gx_aoi_abort_recipe(
      "AOI recipe coordinates must remain within CRS84 bounds.",
      "gx_error_aoi_recipe_geometry"
    )
  }
  state$coordinates <- state$coordinates + 1L
  position
}

gx_aoi_recipe_ring <- function(ring, state) {
  ring <- gx_aoi_recipe_plain_array(
    ring,
    "AOI polygon rings",
    minimum = 4L
  )
  remaining <- gx_aoi_max_coordinates - state$coordinates
  if (length(ring) > remaining) {
    gx_aoi_abort_recipe(
      "AOI recipe geometry exceeds its coordinate-position ceiling.",
      "gx_error_aoi_recipe_budget"
    )
  }
  positions <- vector("list", length(ring))
  for (index in seq_along(ring)) {
    positions[[index]] <- gx_aoi_recipe_position(ring[[index]], state)
  }
  matrix(
    unlist(positions, recursive = FALSE, use.names = FALSE),
    ncol = 2L,
    byrow = TRUE,
    dimnames = list(NULL, NULL)
  )
}

gx_aoi_recipe_polygon <- function(polygon, state) {
  polygon <- gx_aoi_recipe_plain_array(
    polygon,
    "AOI polygon coordinates",
    minimum = 1L
  )
  remaining_rings <- floor(
    (gx_aoi_max_coordinates - state$coordinates) / 4L
  )
  if (length(polygon) > remaining_rings) {
    gx_aoi_abort_recipe(
      "AOI recipe geometry exceeds its coordinate-position ceiling.",
      "gx_error_aoi_recipe_budget"
    )
  }
  rings <- vector("list", length(polygon))
  for (index in seq_along(polygon)) {
    rings[[index]] <- gx_aoi_recipe_ring(polygon[[index]], state)
  }
  rings
}

gx_aoi_recipe_geometry <- function(value) {
  value <- gx_aoi_recipe_plain_object(
    value,
    c("type", "coordinates"),
    "AOI canonical GeoJSON"
  )
  type <- gx_aoi_recipe_string(value$type, "AOI GeoJSON type")
  state <- new.env(parent = emptyenv())
  state$coordinates <- 0L

  if (identical(type, "Polygon")) {
    polygon <- gx_aoi_recipe_polygon(value$coordinates, state)
    geometry <- tryCatch(
      sf::st_polygon(polygon),
      error = function(cnd) {
        gx_aoi_abort_recipe(
          "AOI recipe polygon coordinates could not be reconstructed.",
          "gx_error_aoi_recipe_geometry"
        )
      }
    )
    geojson <- list(
      type = "Polygon",
      coordinates = gx_aoi_geojson_polygon(polygon)
    )
  } else if (identical(type, "MultiPolygon")) {
    members <- gx_aoi_recipe_plain_array(
      value$coordinates,
      "AOI multipolygon coordinates",
      minimum = 1L
    )
    if (length(members) >
        floor((gx_aoi_max_coordinates - state$coordinates) / 4L)) {
      gx_aoi_abort_recipe(
        "AOI recipe geometry exceeds its coordinate-position ceiling.",
        "gx_error_aoi_recipe_budget"
      )
    }
    polygons <- vector("list", length(members))
    for (index in seq_along(members)) {
      polygons[[index]] <- gx_aoi_recipe_polygon(members[[index]], state)
    }
    geometry <- tryCatch(
      sf::st_multipolygon(polygons),
      error = function(cnd) {
        gx_aoi_abort_recipe(
          "AOI recipe multipolygon coordinates could not be reconstructed.",
          "gx_error_aoi_recipe_geometry"
        )
      }
    )
    geojson <- list(
      type = "MultiPolygon",
      coordinates = lapply(polygons, gx_aoi_geojson_polygon)
    )
  } else {
    gx_aoi_abort_recipe(
      "AOI recipe geometry type must be Polygon or MultiPolygon.",
      "gx_error_aoi_recipe_geometry"
    )
  }

  sfc <- tryCatch(
    sf::st_sfc(geometry, crs = gx_aoi_crs, precision = 0),
    error = function(cnd) {
      gx_aoi_abort_recipe(
        "AOI recipe geometry could not be assigned the declared CRS84 identity.",
        "gx_error_aoi_recipe_geometry"
      )
    }
  )
  list(geometry = sfc, geojson = geojson)
}

gx_aoi_from_recipe_inner <- function(recipe) {
  gx_aoi_recipe_assert_complexity(recipe)
  recipe <- gx_aoi_recipe_plain_object(
    recipe,
    c("contract_version", "aoi", "pipeline"),
    "AOI recipe"
  )
  contract_version <- gx_aoi_recipe_string(
    recipe$contract_version,
    "AOI recipe contract version"
  )
  if (!identical(contract_version, gx_aoi_contract_version)) {
    gx_aoi_abort_recipe(
      "AOI recipe contract version is unsupported.",
      "gx_error_aoi_recipe_contract"
    )
  }
  pipeline <- gx_aoi_recipe_plain_object(
    recipe$pipeline,
    c("start_stage", "end_stage"),
    "AOI recipe pipeline"
  )
  pipeline$start_stage <- gx_aoi_recipe_string(
    pipeline$start_stage,
    "AOI recipe start stage"
  )
  pipeline$end_stage <- gx_aoi_recipe_string(
    pipeline$end_stage,
    "AOI recipe end stage"
  )
  if (!identical(pipeline$start_stage, "aoi") ||
      !identical(pipeline$end_stage, "catalog")) {
    gx_aoi_abort_recipe(
      "AOI recipe pipeline must be exactly aoi to catalog.",
      "gx_error_aoi_recipe_contract"
    )
  }

  aoi_value <- gx_aoi_recipe_named_object(recipe[["aoi"]], "AOI recipe identity")
  if (!"kind" %in% names(aoi_value)) {
    gx_aoi_abort_recipe(
      "AOI recipe identity has an invalid object shape.",
      "gx_error_aoi_recipe_structure"
    )
  }
  kind <- gx_aoi_recipe_string(aoi_value[["kind"]], "AOI recipe kind")

  if (kind %in% c("huc", "county", "state")) {
    aoi <- gx_aoi_recipe_plain_object(
      aoi_value,
      c("kind", "identifier"),
      "Identifier AOI recipe"
    )
    identifier <- gx_aoi_recipe_string(
      aoi$identifier,
      "AOI recipe identifier"
    )
    normalized <- list(
      contract_version = gx_aoi_contract_version,
      aoi = list(kind = kind, identifier = identifier),
      pipeline = pipeline
    )
    result <- gx_aoi(identifier, type = kind)
  } else if (identical(kind, "sf")) {
    aoi <- gx_aoi_recipe_plain_object(
      aoi_value,
      c("kind", "canonical_geojson", "crs", "wkb_sha256"),
      "Spatial AOI recipe"
    )
    crs <- gx_aoi_recipe_string(aoi$crs, "AOI recipe CRS")
    hash <- gx_aoi_recipe_string(aoi$wkb_sha256, "AOI recipe WKB hash")
    if (!identical(crs, gx_aoi_crs) ||
        !grepl("^[a-f0-9]{64}\\z", hash, perl = TRUE)) {
      gx_aoi_abort_recipe(
        "Spatial AOI recipe CRS or WKB hash is invalid.",
        "gx_error_aoi_recipe_contract"
      )
    }
    reconstructed <- gx_aoi_recipe_geometry(aoi$canonical_geojson)
    normalized <- list(
      contract_version = gx_aoi_contract_version,
      aoi = list(
        kind = "sf",
        canonical_geojson = reconstructed$geojson,
        crs = gx_aoi_crs,
        wkb_sha256 = hash
      ),
      pipeline = pipeline
    )
    result <- gx_aoi(reconstructed$geometry, type = "sf")
  } else {
    gx_aoi_abort_recipe(
      "AOI recipe kind is not implemented at the offline hydration boundary.",
      "gx_error_aoi_recipe_contract"
    )
  }

  if (!identical(result$recipe, normalized)) {
    gx_aoi_abort_recipe(
      "AOI recipe is not the exact canonical representation of its reconstructed AOI.",
      "gx_error_aoi_recipe_canonical"
    )
  }
  result
}

# Internal M6b boundary. It hydrates only the exact AOI recipe fragment emitted
# by gx_aoi(); it does not read files or authorize catalog/replay execution.
gx_aoi_from_recipe_impl <- function(recipe) {
  tryCatch(
    gx_aoi_from_recipe_inner(recipe),
    gx_error_aoi_recipe = function(cnd) stop(cnd),
    error = function(cnd) {
      gx_aoi_abort_recipe(
        "AOI recipe could not be reconstructed without violating its contract.",
        "gx_error_aoi_recipe_contract"
      )
    }
  )
}

# Internal JSON boundary. Character input is JSON text, never a path or URL.
gx_aoi_from_recipe_json_impl <- function(json) {
  tryCatch(
    gx_aoi_from_recipe_impl(gx_aoi_recipe_parse_json(json)),
    gx_error_aoi_recipe = function(cnd) stop(cnd),
    error = function(cnd) {
      gx_aoi_abort_recipe(
        "AOI recipe JSON could not be decoded without violating its contract.",
        "gx_error_aoi_recipe_syntax"
      )
    }
  )
}
