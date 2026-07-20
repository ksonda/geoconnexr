gx_aoi_contract_version <- "1.0.0"
gx_aoi_crs <- "OGC:CRS84"
gx_aoi_max_coordinates <- 100000L
gx_aoi_max_serialized_bytes <- 8L * 1024L * 1024L
gx_aoi_coordinate_digits <- 9L
gx_aoi_half_grid_tolerance <- 1e-3

gx_aoi_state_codes <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
  "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
  "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
  "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
  "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
  "DC", "AS", "GU", "MP", "PR", "UM", "VI"
)

gx_aoi_abort_geometry <- function(message, call = rlang::caller_env()) {
  gx_abort(
    message,
    c("gx_error_aoi_geometry", "gx_error_aoi"),
    call = call
  )
}

gx_aoi_abort_contract <- function(message, call = rlang::caller_env()) {
  gx_abort(
    message,
    c("gx_error_aoi_contract", "gx_error_aoi"),
    call = call
  )
}

gx_aoi_abort_identifier <- function(message, call = rlang::caller_env()) {
  gx_abort(
    message,
    c("gx_error_identifier", "gx_error_aoi"),
    call = call
  )
}

gx_aoi_match_type <- function(type) {
  choices <- c("auto", "huc", "county", "state", "sf")
  if (!is.character(type) || length(type) != 1L || is.na(type) ||
      !type %in% choices) {
    gx_abort(
      "{.arg type} must be exactly one of {.or {.val {choices}}}.",
      "gx_error_aoi"
    )
  }
  type
}

gx_aoi_validate_identifier <- function(x, type) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x) ||
      !isTRUE(stringi::stri_enc_isutf8(x))) {
    gx_abort(
      "Identifier AOIs must contain one non-empty valid UTF-8 string.",
      "gx_error_aoi"
    )
  }

  if (identical(type, "huc")) {
    if (!grepl("^[0-9]+\\z", x, perl = TRUE) ||
        !nchar(x) %in% c(2L, 4L, 6L, 8L, 10L, 12L)) {
      gx_aoi_abort_identifier(
        "HUCs must contain 2, 4, 6, 8, 10, or 12 digits."
      )
    }
  } else if (identical(type, "county")) {
    if (!grepl("^[0-9]{5}\\z", x, perl = TRUE)) {
      gx_abort("County FIPS must contain exactly five digits.", "gx_error_aoi")
    }
  } else if (identical(type, "state")) {
    if (!x %in% gx_aoi_state_codes) {
      gx_abort(
        "State AOIs must use a recognized two-letter USPS state or territory code.",
        "gx_error_aoi"
      )
    }
  } else {
    gx_abort("Identifier AOIs have an unsupported type.", "gx_error_aoi")
  }
  invisible(x)
}

gx_aoi_point_compare <- function(x, y) {
  if (x[[1]] < y[[1]]) {
    return(-1L)
  }
  if (x[[1]] > y[[1]]) {
    return(1L)
  }
  if (x[[2]] < y[[2]]) {
    return(-1L)
  }
  if (x[[2]] > y[[2]]) {
    return(1L)
  }
  0L
}

# Booth's minimum-rotation algorithm over lexicographically ordered XY pairs.
gx_aoi_minimum_rotation <- function(x) {
  n <- nrow(x)
  if (n <= 1L) {
    return(x)
  }

  i <- 1L
  j <- 2L
  k <- 0L
  while (i <= n && j <= n && k < n) {
    left <- ((i + k - 1L) %% n) + 1L
    right <- ((j + k - 1L) %% n) + 1L
    comparison <- gx_aoi_point_compare(x[left, ], x[right, ])
    if (comparison == 0L) {
      k <- k + 1L
    } else if (comparison > 0L) {
      i <- i + k + 1L
      if (i == j) {
        i <- i + 1L
      }
      k <- 0L
    } else {
      j <- j + k + 1L
      if (i == j) {
        j <- j + 1L
      }
      k <- 0L
    }
  }

  start <- min(i, j)
  indexes <- c(seq.int(start, n), if (start > 1L) seq_len(start - 1L))
  x[indexes, , drop = FALSE]
}

gx_aoi_ring_signed_area <- function(open) {
  following <- c(seq.int(2L, nrow(open)), 1L)
  sum(
    open[, 1L] * open[following, 2L] -
      open[following, 1L] * open[, 2L]
  ) / 2
}

gx_aoi_quantize_coordinates <- function(x) {
  scale <- 10^gx_aoi_coordinate_digits
  scaled <- x * scale
  lower <- floor(scaled)
  fraction <- scaled - lower
  near_half <- abs(fraction - 0.5) <= gx_aoi_half_grid_tolerance
  scaled[near_half] <- lower[near_half] + 0.5
  floor(scaled + 0.5) / scale
}

gx_aoi_canonical_ring <- function(ring, exterior) {
  ring <- unname(as.matrix(ring))
  storage.mode(ring) <- "double"
  if (ncol(ring) != 2L || nrow(ring) < 4L || any(!is.finite(ring))) {
    gx_aoi_abort_geometry(
      "AOI polygon rings must contain at least four finite XY coordinate positions."
    )
  }
  if (!isTRUE(all(ring[1L, ] == ring[nrow(ring), ]))) {
    gx_aoi_abort_geometry("AOI polygon rings must be explicitly closed.")
  }

  if (any(ring[, 1L] < -180 | ring[, 1L] > 180) ||
      any(ring[, 2L] < -90 | ring[, 2L] > 90)) {
    gx_aoi_abort_geometry(
      "Transformed AOI coordinates must remain within CRS84 longitude and latitude bounds."
    )
  }

  ring <- gx_aoi_quantize_coordinates(ring)
  ring[ring == 0] <- 0
  open <- ring[-nrow(ring), , drop = FALSE]
  closed_longitude <- c(open[, 1L], open[1L, 1L])
  if (any(abs(diff(closed_longitude)) > 180)) {
    gx_aoi_abort_geometry(
      "AOI rings crossing the antimeridian require an explicit pre-cut geometry."
    )
  }
  area <- gx_aoi_ring_signed_area(open)
  if (!is.finite(area) || area == 0) {
    gx_aoi_abort_geometry(
      "AOI polygon rings must retain non-zero area on the canonical CRS84 grid."
    )
  }
  if ((isTRUE(exterior) && area < 0) || (!isTRUE(exterior) && area > 0)) {
    open <- open[rev(seq_len(nrow(open))), , drop = FALSE]
  }
  canonical <- gx_aoi_minimum_rotation(open)
  rbind(canonical, canonical[1L, , drop = FALSE])
}

gx_aoi_ring_key <- function(ring) {
  header <- writeBin(
    as.integer(c(nrow(ring), ncol(ring))),
    raw(),
    size = 4L,
    endian = "little"
  )
  coordinates <- writeBin(
    as.double(t(ring)),
    raw(),
    size = 8L,
    endian = "little"
  )
  digest::digest(c(header, coordinates), algo = "sha256", serialize = FALSE)
}

gx_aoi_polygon_key <- function(polygon) {
  header <- writeBin(as.integer(length(polygon)), raw(), size = 4L, endian = "little")
  members <- unlist(lapply(polygon, function(ring) {
    c(
      writeBin(as.integer(nrow(ring)), raw(), size = 4L, endian = "little"),
      writeBin(as.double(t(ring)), raw(), size = 8L, endian = "little")
    )
  }), use.names = FALSE)
  digest::digest(c(header, members), algo = "sha256", serialize = FALSE)
}

gx_aoi_canonical_polygon <- function(polygon) {
  if (!is.list(polygon) || !length(polygon)) {
    gx_aoi_abort_geometry("AOI polygons must contain an exterior ring.")
  }
  rings <- c(
    list(gx_aoi_canonical_ring(polygon[[1L]], exterior = TRUE)),
    lapply(polygon[-1L], gx_aoi_canonical_ring, exterior = FALSE)
  )
  if (length(rings) > 2L) {
    holes <- rings[-1L]
    keys <- vapply(holes, gx_aoi_ring_key, character(1))
    rings <- c(rings[1L], holes[order(keys, method = "radix")])
  }
  rings
}

gx_aoi_geojson_ring <- function(ring) {
  lapply(seq_len(nrow(ring)), function(i) unname(as.double(ring[i, ])))
}

gx_aoi_geojson_polygon <- function(polygon) {
  lapply(polygon, gx_aoi_geojson_ring)
}

gx_aoi_wkb <- function(geometry) {
  tryCatch(
    sf::st_as_binary(
      geometry,
      EWKB = FALSE,
      endian = "little",
      pureR = TRUE
    )[[1L]],
    error = function(cnd) {
      gx_aoi_abort_geometry("AOI geometry could not be serialized as canonical WKB.")
    }
  )
}

gx_aoi_coordinate_count <- function(geometry, geometry_type) {
  value <- tryCatch(unclass(geometry[[1L]]), error = function(cnd) NULL)
  polygons <- if (identical(geometry_type, "POLYGON")) list(value) else value
  if (!is.list(polygons) || !length(polygons)) {
    gx_aoi_abort_geometry("AOI geometry has an invalid polygon structure.")
  }

  total <- 0L
  for (polygon in polygons) {
    if (!is.list(polygon) || !length(polygon)) {
      gx_aoi_abort_geometry("AOI polygons must contain an exterior ring.")
    }
    for (ring in polygon) {
      if (!is.matrix(ring) || !is.numeric(ring) ||
          ncol(ring) != 2L || nrow(ring) < 4L) {
        gx_aoi_abort_geometry(
          "AOI polygon rings must contain at least four XY coordinate positions."
        )
      }
      positions <- nrow(ring)
      if (positions > gx_aoi_max_coordinates - total) {
        gx_aoi_abort_geometry(
          "Spatial AOIs may contain at most {gx_aoi_max_coordinates} coordinate positions."
        )
      }
      if (any(!is.finite(ring))) {
        gx_aoi_abort_geometry("AOI geometry must contain finite XY coordinates.")
      }
      total <- total + positions
    }
  }
  total
}

gx_aoi_validate_sfc_shape <- function(geometry) {
  if (!inherits(geometry, "sfc") || length(geometry) != 1L) {
    gx_aoi_abort_geometry("Spatial AOIs must contain exactly one geometry.")
  }

  crs <- tryCatch(sf::st_crs(geometry), error = function(cnd) NULL)
  if (is.null(crs) || isTRUE(is.na(crs))) {
    gx_aoi_abort_geometry("Spatial AOIs must declare a coordinate reference system.")
  }

  geometry_class <- tryCatch(class(geometry[[1L]]), error = function(cnd) character())
  dimension <- if (length(geometry_class)) geometry_class[[1L]] else NA_character_
  if (!identical(dimension, "XY")) {
    gx_aoi_abort_geometry("Spatial AOIs must be two-dimensional XY geometries.")
  }

  geometry_type <- tryCatch(
    as.character(sf::st_geometry_type(geometry, by_geometry = TRUE))[[1L]],
    error = function(cnd) NA_character_
  )
  if (!geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    gx_aoi_abort_geometry("Spatial AOIs must be POLYGON or MULTIPOLYGON geometries.")
  }

  empty <- tryCatch(
    isTRUE(sf::st_is_empty(geometry)[[1L]]),
    error = function(cnd) TRUE
  )
  if (empty) {
    gx_aoi_abort_geometry("Spatial AOIs may not be empty.")
  }

  gx_aoi_coordinate_count(geometry, geometry_type)

  valid <- tryCatch(
    suppressMessages(suppressWarnings(sf::st_is_valid(geometry)))[[1L]],
    error = function(cnd) FALSE
  )
  if (!isTRUE(valid)) {
    gx_aoi_abort_geometry(
      "Spatial AOIs must be valid; geoconnexr does not repair invalid geometry."
    )
  }
  invisible(geometry_type)
}

gx_aoi_transform_crs84_offline <- function(geometry) {
  network_enabled <- tryCatch(
    sf::sf_proj_network(),
    error = function(cnd) NA
  )
  if (!is.logical(network_enabled) || length(network_enabled) != 1L ||
      is.na(network_enabled)) {
    gx_aoi_abort_geometry("PROJ network state could not be verified before CRS transformation.")
  }

  if (isTRUE(network_enabled)) {
    network_url <- tryCatch(
      sf::sf_proj_network(enable = FALSE),
      error = function(cnd) NULL
    )
    network_disabled <- tryCatch(
      identical(sf::sf_proj_network(), FALSE),
      error = function(cnd) FALSE
    )
    if (is.null(network_url) || !network_disabled) {
      gx_aoi_abort_geometry("PROJ network access could not be disabled before CRS transformation.")
    }
    on.exit(
      tryCatch(
        sf::sf_proj_network(enable = TRUE, url = network_url),
        error = function(cnd) {
          gx_aoi_abort_geometry("PROJ network state could not be restored after CRS transformation.")
        }
      ),
      add = TRUE
    )
  }

  tryCatch(
    suppressMessages(suppressWarnings(sf::st_transform(geometry, gx_aoi_crs))),
    error = function(cnd) NULL
  )
}

gx_aoi_canonicalize_geometry <- function(x) {
  geometry <- if (inherits(x, "sf")) {
    tryCatch(sf::st_geometry(x), error = function(cnd) NULL)
  } else if (inherits(x, "sfc")) {
    x
  } else {
    gx_aoi_abort_geometry("{.arg x} must be an {.cls sf} or {.cls sfc} object for type {.val sf}.")
  }

  if (is.null(geometry)) {
    gx_aoi_abort_geometry("{.arg x} does not contain a readable spatial geometry column.")
  }
  source_type <- gx_aoi_validate_sfc_shape(geometry)
  geometry <- gx_aoi_transform_crs84_offline(geometry)
  if (is.null(geometry)) {
    gx_aoi_abort_geometry("Spatial AOI coordinates could not be transformed to OGC CRS84.")
  }
  transformed_type <- gx_aoi_validate_sfc_shape(geometry)
  if (!identical(source_type, transformed_type)) {
    gx_aoi_abort_geometry("Spatial AOI geometry type changed during CRS84 transformation.")
  }

  value <- unclass(geometry[[1L]])
  if (identical(transformed_type, "POLYGON")) {
    polygon <- gx_aoi_canonical_polygon(value)
    canonical_sfg <- sf::st_polygon(polygon)
    geojson <- list(
      type = "Polygon",
      coordinates = gx_aoi_geojson_polygon(polygon)
    )
  } else {
    polygons <- lapply(value, gx_aoi_canonical_polygon)
    if (length(polygons) > 1L) {
      keys <- vapply(polygons, gx_aoi_polygon_key, character(1))
      polygons <- polygons[order(keys, method = "radix")]
    }
    canonical_sfg <- sf::st_multipolygon(polygons)
    geojson <- list(
      type = "MultiPolygon",
      coordinates = lapply(polygons, gx_aoi_geojson_polygon)
    )
  }

  canonical <- sf::st_sfc(canonical_sfg, crs = gx_aoi_crs, precision = 0)
  gx_aoi_validate_sfc_shape(canonical)
  wkb <- gx_aoi_wkb(canonical)
  serialized <- as.character(jsonlite::toJSON(
    geojson,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  ))
  if (length(wkb) > gx_aoi_max_serialized_bytes ||
      nchar(serialized, type = "bytes") > gx_aoi_max_serialized_bytes) {
    gx_aoi_abort_geometry(
      "Canonical AOI GeoJSON and WKB must each fit within the 8 MiB recipe ceiling."
    )
  }

  list(
    geometry = canonical,
    geojson = geojson,
    wkb = wkb,
    wkb_sha256 = digest::digest(wkb, algo = "sha256", serialize = FALSE),
    coordinate_count = gx_aoi_coordinate_count(canonical, transformed_type)
  )
}

gx_aoi_recipe <- function(type, id = NULL, canonical = NULL) {
  aoi <- if (identical(type, "sf")) {
    list(
      kind = "sf",
      canonical_geojson = canonical$geojson,
      crs = gx_aoi_crs,
      wkb_sha256 = canonical$wkb_sha256
    )
  } else {
    list(kind = type, identifier = id)
  }
  list(
    contract_version = gx_aoi_contract_version,
    aoi = aoi,
    pipeline = list(start_stage = "aoi", end_stage = "catalog")
  )
}

new_gx_aoi <- function(type, id, recipe, geometry = NULL) {
  fields <- if (identical(type, "sf")) {
    list(
      contract_version = gx_aoi_contract_version,
      type = type,
      id = id,
      geometry = geometry,
      recipe = recipe
    )
  } else {
    list(
      contract_version = gx_aoi_contract_version,
      type = type,
      id = id,
      recipe = recipe
    )
  }
  object <- structure(fields, class = "gx_aoi")
  gx_validate_aoi(object)
  object
}

gx_validate_aoi <- function(x) {
  if (!is.list(x) || !identical(class(x), "gx_aoi")) {
    gx_aoi_abort_contract("AOI objects must have exactly class {.cls gx_aoi}.")
  }
  if (!is.character(x$contract_version) || length(x$contract_version) != 1L ||
      !identical(x$contract_version, gx_aoi_contract_version) ||
      !is.character(x$type) || length(x$type) != 1L || is.na(x$type) ||
      !x$type %in% c("huc", "county", "state", "sf") ||
      !is.character(x$id) || length(x$id) != 1L || is.na(x$id) || !nzchar(x$id)) {
    gx_aoi_abort_contract("AOI object scalar fields do not satisfy their contract.")
  }

  expected_names <- if (identical(x$type, "sf")) {
    c("contract_version", "type", "id", "geometry", "recipe")
  } else {
    c("contract_version", "type", "id", "recipe")
  }
  if (!identical(names(x), expected_names) || !is.list(x$recipe) ||
      !identical(names(x$recipe), c("contract_version", "aoi", "pipeline")) ||
      !identical(x$recipe$contract_version, gx_aoi_contract_version) ||
      !is.list(x$recipe$pipeline) ||
      !identical(names(x$recipe$pipeline), c("start_stage", "end_stage")) ||
      !identical(x$recipe$pipeline$start_stage, "aoi") ||
      !identical(x$recipe$pipeline$end_stage, "catalog")) {
    gx_aoi_abort_contract("AOI object and recipe fields do not satisfy their exact contract.")
  }

  if (!identical(x$type, "sf")) {
    if (!is.list(x$recipe$aoi) ||
        !identical(names(x$recipe$aoi), c("kind", "identifier")) ||
        !identical(x$recipe$aoi$kind, x$type) ||
        !identical(x$recipe$aoi$identifier, x$id)) {
      gx_aoi_abort_contract("Identifier AOI recipe fields are inconsistent.")
    }
    valid_identifier <- tryCatch(
      {
        gx_aoi_validate_identifier(x$id, x$type)
        TRUE
      },
      error = function(cnd) FALSE
    )
    if (!valid_identifier) {
      gx_aoi_abort_contract("Identifier AOI value does not satisfy its declared type.")
    }
    return(invisible(x))
  }

  geometry_crs <- tryCatch(
    sf::st_crs(x$geometry)$input,
    error = function(cnd) NA_character_
  )
  if (!isTRUE(stringi::stri_enc_isutf8(x$id)) ||
      !grepl("^[a-f0-9]{64}\\z", x$id, perl = TRUE) || !is.list(x$recipe$aoi) ||
      !identical(
        names(x$recipe$aoi),
        c("kind", "canonical_geojson", "crs", "wkb_sha256")
      ) ||
      !identical(x$recipe$aoi$kind, "sf") ||
      !identical(x$recipe$aoi$crs, gx_aoi_crs) ||
      !identical(x$recipe$aoi$wkb_sha256, x$id) ||
      !inherits(x$geometry, "sfc") ||
      !identical(geometry_crs, gx_aoi_crs)) {
    gx_aoi_abort_contract("Spatial AOI fields do not satisfy their exact contract.")
  }

  canonical <- gx_aoi_canonicalize_geometry(x$geometry)
  if (!identical(gx_aoi_wkb(x$geometry), canonical$wkb) ||
      !identical(canonical$wkb_sha256, x$id) ||
      !identical(canonical$geojson, x$recipe$aoi$canonical_geojson)) {
    gx_aoi_abort_contract("Spatial AOI geometry, GeoJSON, and WKB integrity hash disagree.")
  }
  invisible(x)
}

#' Define an area of interest
#'
#' Identifier AOIs accept HUCs, five-digit county FIPS codes, and recognized
#' two-letter state or territory abbreviations. Spatial AOIs accept exactly one
#' valid, non-empty, two-dimensional `POLYGON` or `MULTIPOLYGON` in an
#' [sf][sf::sf] or [sfc][sf::sfc] object with an explicit CRS.
#'
#' Spatial inputs are transformed to OGC CRS84 with PROJ networking disabled,
#' rounded to a declared nine-decimal-degree grid with a deterministic
#' half-grid tie rule, bounded to 100,000 coordinate positions and 8 MiB per
#' canonical representation, normalized to GeoJSON ring winding plus
#' deterministic ring start and hole/member order, and recorded as canonical
#' GeoJSON with a SHA-256 of portable little-endian WKB. Invalid or
#' grid-collapsed geometry is rejected rather than repaired. Antimeridian rings
#' must be explicitly pre-cut. This function performs no network or catalog
#' work; the recipe's pipeline fields describe the intended replay boundary.
#'
#' @param x One character identifier, or one polygonal `sf`/`sfc` geometry.
#' @param type Exactly one of `"auto"`, `"huc"`, `"county"`, `"state"`, or
#'   `"sf"`. Partial matching is not supported.
#'
#' @return An object of class `gx_aoi` containing a validated replay recipe.
#' @export
gx_aoi <- function(x, type = "auto") {
  type <- gx_aoi_match_type(type)
  spatial <- inherits(x, "sf") || inherits(x, "sfc")

  if (spatial) {
    if (!type %in% c("auto", "sf")) {
      gx_abort(
        "Spatial {.arg x} requires {.arg type} {.val auto} or {.val sf}.",
        "gx_error_aoi"
      )
    }
    canonical <- gx_aoi_canonicalize_geometry(x)
    recipe <- gx_aoi_recipe("sf", canonical = canonical)
    return(new_gx_aoi(
      "sf",
      canonical$wkb_sha256,
      recipe,
      geometry = canonical$geometry
    ))
  }
  if (identical(type, "sf")) {
    gx_aoi_abort_geometry(
      "{.arg x} must be an {.cls sf} or {.cls sfc} object for type {.val sf}."
    )
  }

  if (!is.character(x)) {
    gx_aoi_abort_identifier(
      "{.arg x} must be character so leading zeroes are preserved."
    )
  }
  if (length(x) != 1L || is.na(x) || !nzchar(x) ||
      !isTRUE(stringi::stri_enc_isutf8(x))) {
    gx_aoi_abort_identifier(
      "{.arg x} must contain exactly one non-missing valid UTF-8 AOI identifier."
    )
  }

  if (identical(type, "auto")) {
    if (grepl("^[0-9]+\\z", x, perl = TRUE) &&
        nchar(x) %in% c(2L, 4L, 6L, 8L, 10L, 12L)) {
      type <- "huc"
    } else if (grepl("^[0-9]{5}\\z", x, perl = TRUE)) {
      type <- "county"
    } else if (grepl("^[A-Za-z]{2}\\z", x, perl = TRUE)) {
      type <- "state"
    } else {
      gx_abort(
        "Could not infer the AOI type; supply {.arg type} explicitly.",
        "gx_error_aoi"
      )
    }
  }

  if (identical(type, "state")) {
    x <- toupper(x)
  }
  gx_aoi_validate_identifier(x, type)
  new_gx_aoi(type, x, gx_aoi_recipe(type, id = x))
}

#' @export
print.gx_aoi <- function(x, ...) {
  gx_validate_aoi(x)
  details <- if (identical(x$type, "sf")) {
    c(
      "* Geometry: {x$recipe$aoi$canonical_geojson$type}",
      "* CRS: {x$recipe$aoi$crs}",
      "* WKB SHA-256: {x$id}"
    )
  } else {
    "* Identifier: {x$id}"
  }
  cli::cli_inform(c(
    "<gx_aoi>",
    "* Type: {x$type}",
    details,
    "* Contract: {x$contract_version}"
  ))
  invisible(x)
}
