aoi_geometry_test_shell <- function(dx = 0, dy = 0) {
  ring <- rbind(
    c(-80, 35),
    c(-76, 35),
    c(-76, 39),
    c(-80, 39),
    c(-80, 35)
  )
  ring[, 1L] <- ring[, 1L] + dx
  ring[, 2L] <- ring[, 2L] + dy
  ring
}

aoi_geometry_test_holes <- function() {
  list(
    rbind(
      c(-79.5, 35.5),
      c(-78.75, 35.5),
      c(-78.75, 36.25),
      c(-79.5, 36.25),
      c(-79.5, 35.5)
    ),
    rbind(
      c(-77.5, 37.25),
      c(-76.75, 37.25),
      c(-76.75, 38.25),
      c(-77.5, 38.25),
      c(-77.5, 37.25)
    )
  )
}

aoi_geometry_test_sfc <- function(
    shell = aoi_geometry_test_shell(),
    holes = list(),
    crs = "OGC:CRS84") {
  sf::st_sfc(sf::st_polygon(c(list(shell), holes)), crs = crs)
}

aoi_geometry_test_rotate <- function(ring, start) {
  open <- ring[-nrow(ring), , drop = FALSE]
  stopifnot(start >= 1L, start <= nrow(open))
  indexes <- c(
    seq.int(start, nrow(open)),
    if (start > 1L) seq_len(start - 1L)
  )
  rotated <- open[indexes, , drop = FALSE]
  rbind(rotated, rotated[1L, , drop = FALSE])
}

aoi_geometry_test_reverse <- function(ring) {
  ring[rev(seq_len(nrow(ring))), , drop = FALSE]
}

aoi_geometry_test_multipolygon <- function(reverse_members = FALSE) {
  first <- unclass(sf::st_polygon(list(aoi_geometry_test_shell())))
  second <- unclass(sf::st_polygon(list(aoi_geometry_test_shell(dx = 8))))
  members <- list(first, second)
  if (reverse_members) {
    members <- rev(members)
  }
  sf::st_sfc(sf::st_multipolygon(members), crs = "OGC:CRS84")
}

aoi_geometry_test_wkb <- function(geometry) {
  sf::st_as_binary(
    geometry,
    EWKB = FALSE,
    endian = "little",
    pureR = TRUE
  )[[1L]]
}

test_that("spatial AOIs expose a canonical CRS84 WKB contract", {
  input <- aoi_geometry_test_sfc(holes = aoi_geometry_test_holes())

  auto <- gx_aoi(input)
  automatic <- gx_aoi(input, type = "auto")
  explicit <- gx_aoi(input, type = "sf")

  expect_s3_class(auto, "gx_aoi")
  expect_identical(auto, automatic)
  expect_identical(auto, explicit)
  expect_identical(auto$type, "sf")
  expect_match(auto$id, "^[a-f0-9]{64}$")
  expect_identical(
    names(auto),
    c("contract_version", "type", "id", "geometry", "recipe")
  )

  expect_s3_class(auto$geometry, "sfc")
  expect_length(auto$geometry, 1L)
  expect_identical(class(auto$geometry[[1L]])[[1L]], "XY")
  expect_identical(
    as.character(sf::st_geometry_type(auto$geometry, by_geometry = TRUE)),
    "POLYGON"
  )
  expect_identical(sf::st_crs(auto$geometry)$input, "OGC:CRS84")
  expect_true(isTRUE(sf::st_crs(auto$geometry) == sf::st_crs("OGC:CRS84")))

  expected_hash <- digest::digest(
    aoi_geometry_test_wkb(auto$geometry),
    algo = "sha256",
    serialize = FALSE
  )
  expect_identical(auto$id, expected_hash)
  expect_identical(auto$recipe$aoi$kind, "sf")
  expect_identical(auto$recipe$aoi$crs, "OGC:CRS84")
  expect_identical(auto$recipe$aoi$wkb_sha256, auto$id)
  expect_identical(auto$recipe$aoi$canonical_geojson$type, "Polygon")
  expect_type(auto$recipe$aoi$canonical_geojson$coordinates, "list")
})

test_that("sf and sfc inputs produce the same deterministic AOI", {
  sfc <- aoi_geometry_test_sfc(holes = aoi_geometry_test_holes())
  frame <- sf::st_sf(source_name = "ignored", geometry = sfc)

  from_sfc <- gx_aoi(sfc)
  from_sf <- gx_aoi(frame)

  expect_identical(from_sf$id, from_sfc$id)
  expect_identical(from_sf$recipe, from_sfc$recipe)
  expect_identical(
    aoi_geometry_test_wkb(from_sf$geometry),
    aoi_geometry_test_wkb(from_sfc$geometry)
  )
  expect_identical(gx_aoi(frame), from_sf)
})

test_that("polygon canonicalization ignores ring direction, start, and hole order", {
  shell <- aoi_geometry_test_shell()
  holes <- aoi_geometry_test_holes()
  baseline <- gx_aoi(aoi_geometry_test_sfc(shell, holes))

  variants <- list(
    aoi_geometry_test_sfc(
      aoi_geometry_test_rotate(shell, 3L),
      holes
    ),
    aoi_geometry_test_sfc(
      aoi_geometry_test_reverse(shell),
      lapply(holes, aoi_geometry_test_reverse)
    ),
    aoi_geometry_test_sfc(
      aoi_geometry_test_rotate(aoi_geometry_test_reverse(shell), 2L),
      list(
        aoi_geometry_test_rotate(aoi_geometry_test_reverse(holes[[2L]]), 3L),
        aoi_geometry_test_rotate(holes[[1L]], 2L)
      )
    )
  )

  for (variant in variants) {
    candidate <- gx_aoi(variant)
    expect_identical(candidate$id, baseline$id)
    expect_identical(
      candidate$recipe$aoi$canonical_geojson,
      baseline$recipe$aoi$canonical_geojson
    )
    expect_identical(
      aoi_geometry_test_wkb(candidate$geometry),
      aoi_geometry_test_wkb(baseline$geometry)
    )
  }

  coordinates <- baseline$recipe$aoi$canonical_geojson$coordinates
  signed_area <- function(ring) {
    matrix <- do.call(rbind, ring)
    open <- matrix[-nrow(matrix), , drop = FALSE]
    following <- c(seq.int(2L, nrow(open)), 1L)
    sum(
      open[, 1L] * open[following, 2L] -
        open[following, 1L] * open[, 2L]
    ) / 2
  }
  expect_gt(signed_area(coordinates[[1L]]), 0)
  expect_true(all(vapply(coordinates[-1L], signed_area, numeric(1)) < 0))
})

test_that("multipolygon canonicalization ignores member order", {
  baseline <- gx_aoi(aoi_geometry_test_multipolygon())
  reordered <- gx_aoi(aoi_geometry_test_multipolygon(reverse_members = TRUE))

  expect_identical(baseline$id, reordered$id)
  expect_identical(baseline$geometry, reordered$geometry)
  expect_identical(baseline$recipe, reordered$recipe)
  expect_identical(
    baseline$recipe$aoi$canonical_geojson$type,
    "MultiPolygon"
  )
})

test_that("projected polygon inputs are transformed to canonical CRS84", {
  geographic <- aoi_geometry_test_sfc()
  projected <- sf::st_transform(geographic, 3857)

  result <- gx_aoi(projected)

  expect_identical(sf::st_crs(result$geometry)$input, "OGC:CRS84")
  expect_identical(result$id, gx_aoi(geographic)$id)
  expect_identical(result$recipe, gx_aoi(geographic)$recipe)
  expect_equal(
    unname(sf::st_bbox(result$geometry)),
    unname(sf::st_bbox(geographic)),
    tolerance = 1e-8
  )
  expect_identical(result$id, digest::digest(
    aoi_geometry_test_wkb(result$geometry),
    algo = "sha256",
    serialize = FALSE
  ))
  expect_identical(gx_aoi(projected), result)
  expect_identical(geoconnexr:::gx_aoi_coordinate_digits, 9L)
})

test_that("half-grid projection noise has one deterministic quantization tie", {
  x <- -99.9999999995
  y <- 35.0000000005
  geographic <- aoi_geometry_test_sfc(rbind(
    c(x, y),
    c(x + 1, y),
    c(x + 1, y + 1),
    c(x, y + 1),
    c(x, y)
  ))
  projected <- sf::st_transform(geographic, 3857)

  expect_identical(gx_aoi(geographic)$id, gx_aoi(projected)$id)
  expect_identical(gx_aoi(geographic)$recipe, gx_aoi(projected)$recipe)
  expect_identical(geoconnexr:::gx_aoi_half_grid_tolerance, 1e-3)
})

test_that("spatial transformation disables and restores PROJ networking", {
  original <- sf::sf_proj_network()
  if (isTRUE(original)) {
    original_url <- sf::sf_proj_network(enable = FALSE)
    on.exit(sf::sf_proj_network(enable = TRUE, url = original_url), add = TRUE)
  } else {
    on.exit(sf::sf_proj_network(enable = FALSE), add = TRUE)
  }

  enabled_url <- sf::sf_proj_network(enable = TRUE)
  on.exit(sf::sf_proj_network(enable = FALSE), add = TRUE)
  expect_true(sf::sf_proj_network())

  result <- gx_aoi(sf::st_transform(aoi_geometry_test_sfc(), 3857))

  expect_s3_class(result, "gx_aoi")
  expect_true(sf::sf_proj_network())
  expect_type(enabled_url, "character")
})

test_that("AOI type matching is exact and spatial dispatch is explicit", {
  polygon <- aoi_geometry_test_sfc()

  for (bad_type in list(
    "a", "s", "hu", "co", "st", "SF", "unknown",
    c("auto", "sf"), NA_character_, 1L
  )) {
    expect_error(gx_aoi(polygon, type = bad_type), class = "gx_error_aoi")
  }
  expect_error(
    gx_aoi(polygon, type = "huc"),
    class = "gx_error_aoi"
  )
  expect_error(
    gx_aoi("02070010", type = "sf"),
    class = "gx_error_aoi_geometry"
  )
})

test_that("spatial AOIs require exactly one sf or sfc geometry", {
  one <- aoi_geometry_test_sfc()
  none <- sf::st_sfc(crs = "OGC:CRS84")
  two <- do.call(c, list(one, one))
  sf_none <- sf::st_sf(label = character(), geometry = none)
  sf_two <- sf::st_sf(label = c("a", "b"), geometry = two)

  for (bad in list(none, two, sf_none, sf_two)) {
    expect_error(gx_aoi(bad), class = "gx_error_aoi_geometry")
  }
  expect_error(gx_aoi(one[[1L]], type = "sf"), class = "gx_error_aoi_geometry")
})

test_that("spatial AOIs require polygonal finite XY geometry with a CRS", {
  shell <- aoi_geometry_test_shell()
  point <- sf::st_sfc(sf::st_point(c(-78, 37)), crs = "OGC:CRS84")
  line <- sf::st_sfc(
    sf::st_linestring(rbind(c(-79, 36), c(-78, 37))),
    crs = "OGC:CRS84"
  )
  collection <- sf::st_sfc(
    sf::st_geometrycollection(list(sf::st_polygon(list(shell)))),
    crs = "OGC:CRS84"
  )
  missing_crs <- sf::st_sfc(sf::st_polygon(list(shell)))
  xyz <- sf::st_sfc(
    sf::st_polygon(list(cbind(shell, z = 1)), dim = "XYZ"),
    crs = "OGC:CRS84"
  )
  xym <- sf::st_sfc(
    sf::st_polygon(list(cbind(shell, m = 1)), dim = "XYM"),
    crs = "OGC:CRS84"
  )
  xyzm <- sf::st_sfc(
    sf::st_polygon(list(cbind(shell, z = 1, m = 1)), dim = "XYZM"),
    crs = "OGC:CRS84"
  )

  for (bad in list(point, line, collection, missing_crs, xyz, xym, xyzm)) {
    expect_error(gx_aoi(bad), class = "gx_error_aoi_geometry")
  }
})

test_that("empty, invalid, non-finite, and out-of-bounds geometry is rejected", {
  empty <- sf::st_sfc(sf::st_polygon(), crs = "OGC:CRS84")
  bowtie <- rbind(
    c(-80, 35),
    c(-76, 39),
    c(-80, 39),
    c(-76, 35),
    c(-80, 35)
  )
  invalid <- aoi_geometry_test_sfc(bowtie)

  expect_error(gx_aoi(empty), class = "gx_error_aoi_geometry")
  expect_false(isTRUE(sf::st_is_valid(invalid)[[1L]]))
  expect_error(gx_aoi(invalid), class = "gx_error_aoi_geometry")

  for (value in c(Inf, NaN)) {
    geometry <- aoi_geometry_test_sfc()
    geometry[[1L]][[1L]][2L, 1L] <- value
    expect_error(gx_aoi(geometry), class = "gx_error_aoi_geometry")
  }

  bad_longitude <- aoi_geometry_test_sfc(
    aoi_geometry_test_shell(dx = 260)
  )
  bad_latitude <- aoi_geometry_test_sfc(
    aoi_geometry_test_shell(dy = 60)
  )
  slight_overrun <- aoi_geometry_test_sfc(rbind(
    c(179.5, 35),
    c(180 + 1e-10, 35),
    c(179.5, 36),
    c(179.5, 35)
  ))
  antimeridian <- aoi_geometry_test_sfc(rbind(
    c(170, 50),
    c(-170, 50),
    c(-170, 55),
    c(170, 55),
    c(170, 50)
  ))

  for (bad in list(
    bad_longitude,
    bad_latitude,
    slight_overrun,
    antimeridian
  )) {
    expect_error(gx_aoi(bad), class = "gx_error_aoi_geometry")
  }
})

test_that("spatial AOIs enforce coordinate and serialized-size ceilings", {
  expect_identical(geoconnexr:::gx_aoi_max_coordinates, 100000L)
  expect_identical(
    geoconnexr:::gx_aoi_max_serialized_bytes,
    8L * 1024L * 1024L
  )

  angles <- seq(0, 2 * pi, length.out = 100001L)[-100001L]
  open_ring <- cbind(
    -78 + 0.25 * cos(angles),
    37 + 0.25 * sin(angles)
  )
  coordinate_bomb <- aoi_geometry_test_sfc(
    rbind(open_ring, open_ring[1L, , drop = FALSE])
  )
  expect_error(gx_aoi(coordinate_bomb), class = "gx_error_aoi_geometry")

  testthat::local_mocked_bindings(
    gx_aoi_max_serialized_bytes = 1L,
    .package = "geoconnexr"
  )
  expect_error(
    gx_aoi(aoi_geometry_test_sfc()),
    class = "gx_error_aoi_geometry"
  )
})

test_that("spatial AOI recipes satisfy the recipe JSON Schema", {
  skip_if_not_installed("jsonvalidate")

  recipe <- gx_aoi(
    aoi_geometry_test_sfc(holes = aoi_geometry_test_holes())
  )$recipe
  schema <- system.file(
    "schema", "recipe-v1.json",
    package = "geoconnexr"
  )
  json <- jsonlite::toJSON(recipe, auto_unbox = TRUE, digits = NA)

  expect_true(nzchar(schema))
  expect_true(jsonvalidate::json_validate(json, schema, engine = "ajv"))

  validates <- function(value) {
    jsonvalidate::json_validate(
      jsonlite::toJSON(value, auto_unbox = TRUE, digits = NA),
      schema,
      engine = "ajv"
    )
  }

  missing_hash <- recipe
  missing_hash$aoi$wkb_sha256 <- NULL
  wrong_crs <- recipe
  wrong_crs$aoi$crs <- "EPSG:4326"
  point_geometry <- recipe
  point_geometry$aoi$canonical_geojson <- list(
    type = "Point",
    coordinates = c(-78, 37)
  )
  out_of_bounds <- recipe
  out_of_bounds$aoi$canonical_geojson$coordinates[[1L]][[1L]] <- c(181, 37)
  mainstem_basin <- gx_aoi("02070010")$recipe
  mainstem_basin$aoi$kind <- "mainstem_basin"
  point_upstream <- list(
    contract_version = "1.0.0",
    aoi = list(
      kind = "point_upstream",
      longitude = -78,
      latitude = 37,
      crs = "OGC:CRS84"
    ),
    pipeline = list(start_stage = "aoi", end_stage = "catalog")
  )
  malformed_identifiers <- list(
    list(kind = "huc", identifier = "x"),
    list(kind = "county", identifier = "1"),
    list(kind = "state", identifier = "ZZ"),
    list(
      kind = "huc",
      identifier = "02070010",
      canonical_uri = "https://example.com/huc/02070010"
    )
  )

  for (unsupported in list(
    missing_hash,
    wrong_crs,
    point_geometry,
    out_of_bounds,
    mainstem_basin,
    point_upstream
  )) {
    expect_false(validates(unsupported))
  }
  for (aoi in malformed_identifiers) {
    invalid <- gx_aoi("02070010")$recipe
    invalid$aoi <- aoi
    expect_false(validates(invalid))
  }
})

test_that("the internal AOI validator rejects contract and geometry tampering", {
  valid <- gx_aoi(
    aoi_geometry_test_sfc(holes = aoi_geometry_test_holes())
  )
  expect_invisible(geoconnexr:::gx_validate_aoi(valid))

  bad_id <- valid
  bad_id$id <- strrep("0", 64L)

  bad_hash <- valid
  bad_hash$recipe$aoi$wkb_sha256 <- strrep("f", 64L)

  bad_crs <- valid
  bad_crs$recipe$aoi$crs <- "EPSG:4326"

  bad_geojson <- valid
  bad_geojson$recipe$aoi$canonical_geojson$coordinates[[1L]][[1L]][[1L]] <- -79

  missing_hash <- valid
  missing_hash$recipe$aoi$wkb_sha256 <- NULL

  extra_field <- valid
  extra_field$unexpected <- TRUE

  shifted_geometry <- valid
  shifted_ring <- unclass(valid$geometry[[1L]])[[1L]]
  shifted_ring[, 1L] <- shifted_ring[, 1L] + 0.125
  shifted_geometry$geometry <- aoi_geometry_test_sfc(shifted_ring)

  noncanonical_geometry <- valid
  reversed <- aoi_geometry_test_reverse(
    unclass(valid$geometry[[1L]])[[1L]]
  )
  noncanonical_geometry$geometry <- aoi_geometry_test_sfc(reversed)

  for (tampered in list(
    bad_id,
    bad_hash,
    bad_crs,
    bad_geojson,
    missing_hash,
    extra_field,
    shifted_geometry,
    noncanonical_geometry
  )) {
    expect_error(
      geoconnexr:::gx_validate_aoi(tampered),
      class = "gx_error_aoi"
    )
  }
})

test_that("AOI construction is offline and identifier AOIs remain compatible", {
  calls <- 0L
  testthat::local_mocked_bindings(
    gx_http_request = function(...) {
      calls <<- calls + 1L
      stop("unexpected network request", call. = FALSE)
    },
    .package = "geoconnexr"
  )

  spatial <- gx_aoi(aoi_geometry_test_sfc())
  huc <- gx_aoi("02070010")
  county <- gx_aoi("37135")
  state <- gx_aoi("nc")

  expect_identical(calls, 0L)
  expect_identical(spatial$type, "sf")
  expect_identical(c(huc$type, huc$id), c("huc", "02070010"))
  expect_identical(c(county$type, county$id), c("county", "37135"))
  expect_identical(c(state$type, state$id), c("state", "NC"))
  expect_identical(huc$recipe$aoi, list(kind = "huc", identifier = "02070010"))
  expect_identical(
    county$recipe$aoi,
    list(kind = "county", identifier = "37135")
  )
  expect_identical(state$recipe$aoi, list(kind = "state", identifier = "NC"))
})

test_that("spatial AOIs print their canonical identity", {
  aoi <- gx_aoi(aoi_geometry_test_sfc())
  printed <- paste(
    capture.output(print(aoi), type = "message"),
    collapse = "\n"
  )

  expect_match(printed, "<gx_aoi>", fixed = TRUE)
  expect_match(printed, "Type: sf", fixed = TRUE)
  expect_match(printed, "Geometry: Polygon", fixed = TRUE)
  expect_match(printed, "CRS: OGC:CRS84", fixed = TRUE)
  expect_match(printed, aoi$id, fixed = TRUE)
  expect_match(printed, aoi$contract_version, fixed = TRUE)
})

test_that("all gx_aoi input failures inherit from gx_error_aoi", {
  expect_error(gx_aoi(2070010), class = "gx_error_aoi")
  expect_error(gx_aoi("123", type = "huc"), class = "gx_error_aoi")
  expect_error(gx_aoi("ZZ", type = "state"), class = "gx_error_aoi")
  expect_error(gx_aoi(character()), class = "gx_error_aoi")
  expect_error(
    gx_aoi(aoi_geometry_test_sfc(), type = "county"),
    class = "gx_error_aoi"
  )
})
