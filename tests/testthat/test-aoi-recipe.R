aoi_recipe_test_shell <- function(dx = 0, dy = 0) {
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

aoi_recipe_test_holes <- function() {
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

aoi_recipe_test_polygon <- function(
    shell = aoi_recipe_test_shell(),
    holes = list()) {
  sf::st_sfc(
    sf::st_polygon(c(list(shell), holes)),
    crs = "OGC:CRS84"
  )
}

aoi_recipe_test_multipolygon <- function() {
  first <- unclass(sf::st_polygon(list(aoi_recipe_test_shell())))
  second <- unclass(sf::st_polygon(list(aoi_recipe_test_shell(dx = 8))))
  sf::st_sfc(
    sf::st_multipolygon(list(second, first)),
    crs = "OGC:CRS84"
  )
}

aoi_recipe_test_clone <- function(x) {
  unserialize(serialize(x, NULL))
}

aoi_recipe_test_json <- function(recipe) {
  as.character(jsonlite::toJSON(
    recipe,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  ))
}

aoi_recipe_test_positions <- function(matrix) {
  lapply(seq_len(nrow(matrix)), function(index) {
    unname(as.double(matrix[index, ]))
  })
}

aoi_recipe_test_rotate_ring <- function(ring, start = 2L) {
  open <- ring[-length(ring)]
  indexes <- c(
    seq.int(start, length(open)),
    if (start > 1L) seq_len(start - 1L)
  )
  rotated <- open[indexes]
  c(rotated, list(rotated[[1L]]))
}

aoi_recipe_test_expect_error <- function(code) {
  condition <- tryCatch(code(), error = identity)
  expect_s3_class(condition, "gx_error_aoi_recipe")
  expect_s3_class(condition, "gx_error_aoi")
  invisible(condition)
}

test_that("identifier recipes round-trip without losing leading zeroes", {
  inputs <- list(
    list(value = "01", type = "huc"),
    list(value = "010203040506", type = "huc"),
    list(value = "01001", type = "county"),
    list(value = "nc", type = "state")
  )

  for (input in inputs) {
    expected <- gx_aoi(input$value, type = input$type)
    json <- aoi_recipe_test_json(expected$recipe)

    from_list <- gx_aoi_from_recipe_impl(expected$recipe)
    from_text <- gx_aoi_from_recipe_json_impl(json)
    from_raw <- gx_aoi_from_recipe_json_impl(charToRaw(json))

    expect_identical(from_list, expected)
    expect_identical(from_text, expected)
    expect_identical(from_raw, expected)
    expect_identical(from_list$id, expected$id)
    expect_identical(from_list$recipe, expected$recipe)
  }
})

test_that("polygon and multipolygon recipes reconstruct canonical CRS84 geometry", {
  inputs <- list(
    Polygon = aoi_recipe_test_polygon(holes = aoi_recipe_test_holes()),
    MultiPolygon = aoi_recipe_test_multipolygon()
  )

  for (geometry_type in names(inputs)) {
    expected <- gx_aoi(inputs[[geometry_type]])
    json <- aoi_recipe_test_json(expected$recipe)
    outputs <- list(
      gx_aoi_from_recipe_impl(expected$recipe),
      gx_aoi_from_recipe_json_impl(json),
      gx_aoi_from_recipe_json_impl(charToRaw(json))
    )

    for (output in outputs) {
      expect_identical(output, expected)
      expect_s3_class(output$geometry, "sfc")
      expect_length(output$geometry, 1L)
      expect_identical(sf::st_crs(output$geometry)$input, "OGC:CRS84")
      expect_identical(
        as.character(sf::st_geometry_type(output$geometry)),
        toupper(geometry_type)
      )
      expect_identical(output$id, output$recipe$aoi$wkb_sha256)
      expect_invisible(geoconnexr:::gx_validate_aoi(output))
    }
  }
})

test_that("JSON object order is irrelevant but canonical output order is restored", {
  expected <- gx_aoi(
    aoi_recipe_test_polygon(holes = aoi_recipe_test_holes())
  )
  recipe <- expected$recipe
  reordered <- list(
    pipeline = list(
      end_stage = recipe$pipeline$end_stage,
      start_stage = recipe$pipeline$start_stage
    ),
    aoi = list(
      wkb_sha256 = recipe$aoi$wkb_sha256,
      crs = recipe$aoi$crs,
      canonical_geojson = list(
        coordinates = recipe$aoi$canonical_geojson$coordinates,
        type = recipe$aoi$canonical_geojson$type
      ),
      kind = recipe$aoi$kind
    ),
    contract_version = recipe$contract_version
  )
  json <- paste0(" \n", aoi_recipe_test_json(reordered), "\t ")

  output <- gx_aoi_from_recipe_json_impl(charToRaw(json))

  expect_identical(output, expected)
  expect_identical(
    names(output$recipe),
    c("contract_version", "aoi", "pipeline")
  )
  expect_identical(
    names(output$recipe$aoi),
    c("kind", "canonical_geojson", "crs", "wkb_sha256")
  )
  expect_identical(
    names(output$recipe$aoi$canonical_geojson),
    c("type", "coordinates")
  )
  expect_identical(
    names(output$recipe$pipeline),
    c("start_stage", "end_stage")
  )
})

test_that("integer and double JSON coordinates produce identical WKB identity", {
  expected <- gx_aoi(aoi_recipe_test_polygon())
  integer_json <- aoi_recipe_test_json(expected$recipe)
  double_json <- gsub(
    "(-?[0-9]+)(?=[,\\]])",
    "\\1.0",
    integer_json,
    perl = TRUE
  )

  expect_false(identical(integer_json, double_json))
  from_integer <- gx_aoi_from_recipe_json_impl(integer_json)
  from_double <- gx_aoi_from_recipe_json_impl(double_json)
  expect_identical(from_integer, expected)
  expect_identical(from_double, expected)
  expect_identical(from_integer$id, from_double$id)
  expect_identical(from_integer$geometry, from_double$geometry)
})

test_that("list recipes require the exact AOI-only field contract", {
  baseline <- gx_aoi("01020304", type = "huc")$recipe
  mutations <- list(
    extra_root = function(x) {
      x$time <- NULL
      x$catalog <- list()
      x
    },
    missing_version = function(x) {
      x$contract_version <- NULL
      x
    },
    missing_aoi = function(x) {
      x$aoi <- NULL
      x
    },
    missing_pipeline = function(x) {
      x$pipeline <- NULL
      x
    },
    extra_aoi = function(x) {
      x$aoi$unexpected <- TRUE
      x
    },
    missing_identifier = function(x) {
      x$aoi$identifier <- NULL
      x
    },
    extra_pipeline = function(x) {
      x$pipeline$unexpected <- TRUE
      x
    },
    missing_pipeline_end = function(x) {
      x$pipeline$end_stage <- NULL
      x
    }
  )

  for (mutate in mutations) {
    recipe <- mutate(aoi_recipe_test_clone(baseline))
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_impl(recipe)
    })
  }

  for (invalid in list(NULL, TRUE, "recipe", unname(baseline))) {
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_impl(invalid)
    })
  }
})

test_that("changed version, pipeline, kind, and identifier are rejected", {
  baseline <- gx_aoi("01020304", type = "huc")$recipe
  mutations <- list(
    version = function(x) {
      x$contract_version <- "1.0.1"
      x
    },
    pipeline_start = function(x) {
      x$pipeline$start_stage <- "catalog"
      x
    },
    pipeline_end = function(x) {
      x$pipeline$end_stage <- "package"
      x
    },
    kind = function(x) {
      x$aoi$kind <- "county"
      x
    },
    identifier = function(x) {
      x$aoi$identifier <- "123"
      x
    }
  )

  for (mutate in mutations) {
    recipe <- mutate(aoi_recipe_test_clone(baseline))
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_impl(recipe)
    })
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_json_impl(aoi_recipe_test_json(recipe))
    })
  }
})

test_that("JSON rejects duplicate members at every recipe object boundary", {
  expected <- gx_aoi(aoi_recipe_test_polygon())
  json <- aoi_recipe_test_json(expected$recipe)
  duplicates <- c(
    sub(
      '"contract_version":"1.0.0"',
      '"contract_version":"1.0.0","contract_version":"1.0.0"',
      json,
      fixed = TRUE
    ),
    sub(
      '"aoi":{"kind":"sf"',
      '"aoi":{"kind":"sf","kind":"sf"',
      json,
      fixed = TRUE
    ),
    sub(
      '"canonical_geojson":{"type":"Polygon"',
      '"canonical_geojson":{"type":"Polygon","type":"Polygon"',
      json,
      fixed = TRUE
    ),
    sub(
      '"pipeline":{"start_stage":"aoi"',
      '"pipeline":{"start_stage":"aoi","start_stage":"aoi"',
      json,
      fixed = TRUE
    ),
    sub(
      '"contract_version":"1.0.0"',
      paste0(
        '"contract_version":"1.0.0",',
        '"contract_\\u0076ersion":"1.0.0"'
      ),
      json,
      fixed = TRUE
    )
  )

  expect_true(all(duplicates != json))
  for (duplicate in duplicates) {
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_json_impl(charToRaw(duplicate))
    })
  }
})

test_that("JSON rejects unknown and missing members rather than defaulting", {
  baseline <- gx_aoi(aoi_recipe_test_polygon())$recipe
  mutations <- list(
    unknown_root = function(x) {
      x$fetch <- list(enabled = FALSE)
      x
    },
    missing_root = function(x) {
      x$pipeline <- NULL
      x
    },
    unknown_aoi = function(x) {
      x$aoi$identifier <- "01"
      x
    },
    missing_aoi = function(x) {
      x$aoi$wkb_sha256 <- NULL
      x
    },
    unknown_geojson = function(x) {
      x$aoi$canonical_geojson$bbox <- c(-80, 35, -76, 39)
      x
    },
    missing_geojson = function(x) {
      x$aoi$canonical_geojson$type <- NULL
      x
    },
    unknown_pipeline = function(x) {
      x$pipeline$refresh <- FALSE
      x
    },
    missing_pipeline = function(x) {
      x$pipeline$start_stage <- NULL
      x
    }
  )

  for (mutate in mutations) {
    recipe <- mutate(aoi_recipe_test_clone(baseline))
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_json_impl(aoi_recipe_test_json(recipe))
    })
  }
})

test_that("spatial recipe hashes and canonical coordinates are integrity-bound", {
  baseline <- gx_aoi(
    aoi_recipe_test_polygon(holes = aoi_recipe_test_holes())
  )$recipe

  bad_hash <- aoi_recipe_test_clone(baseline)
  bad_hash$aoi$wkb_sha256 <- paste0(
    if (startsWith(bad_hash$aoi$wkb_sha256, "0")) "1" else "0",
    substring(bad_hash$aoi$wkb_sha256, 2L)
  )

  bad_coordinate <- aoi_recipe_test_clone(baseline)
  bad_coordinate$aoi$canonical_geojson$coordinates[[1L]][[2L]][[1L]] <-
    bad_coordinate$aoi$canonical_geojson$coordinates[[1L]][[2L]][[1L]] + 1e-9

  bad_crs <- aoi_recipe_test_clone(baseline)
  bad_crs$aoi$crs <- "EPSG:4326"

  bad_kind <- aoi_recipe_test_clone(baseline)
  bad_kind$aoi$kind <- "huc"

  for (recipe in list(bad_hash, bad_coordinate, bad_crs, bad_kind)) {
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_impl(recipe)
    })
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_json_impl(aoi_recipe_test_json(recipe))
    })
  }
})

test_that("logically equivalent but noncanonical polygon recipes are rejected", {
  baseline <- gx_aoi(
    aoi_recipe_test_polygon(holes = aoi_recipe_test_holes())
  )$recipe
  coordinates <- baseline$aoi$canonical_geojson$coordinates

  rotated <- aoi_recipe_test_clone(baseline)
  rotated$aoi$canonical_geojson$coordinates[[1L]] <-
    aoi_recipe_test_rotate_ring(coordinates[[1L]], 3L)

  reversed <- aoi_recipe_test_clone(baseline)
  reversed$aoi$canonical_geojson$coordinates[[1L]] <- rev(coordinates[[1L]])

  holes_reordered <- aoi_recipe_test_clone(baseline)
  holes_reordered$aoi$canonical_geojson$coordinates[c(2L, 3L)] <-
    holes_reordered$aoi$canonical_geojson$coordinates[c(3L, 2L)]

  off_grid <- aoi_recipe_test_clone(baseline)
  off_grid$aoi$canonical_geojson$coordinates[[1L]][[2L]][[1L]] <-
    off_grid$aoi$canonical_geojson$coordinates[[1L]][[2L]][[1L]] + 1e-10

  for (recipe in list(rotated, reversed, holes_reordered, off_grid)) {
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_impl(recipe)
    })
  }
})

test_that("logically equivalent but noncanonical multipolygon order is rejected", {
  baseline <- gx_aoi(aoi_recipe_test_multipolygon())$recipe
  reordered <- aoi_recipe_test_clone(baseline)
  reordered$aoi$canonical_geojson$coordinates <- rev(
    reordered$aoi$canonical_geojson$coordinates
  )

  aoi_recipe_test_expect_error(function() {
    gx_aoi_from_recipe_impl(reordered)
  })
  aoi_recipe_test_expect_error(function() {
    gx_aoi_from_recipe_json_impl(aoi_recipe_test_json(reordered))
  })
})

test_that("invalid, empty, non-finite, and malformed polygon coordinates fail closed", {
  baseline <- gx_aoi(aoi_recipe_test_polygon())$recipe
  make_recipe <- function(type = "Polygon", coordinates) {
    recipe <- aoi_recipe_test_clone(baseline)
    recipe$aoi$canonical_geojson$type <- type
    recipe$aoi$canonical_geojson$coordinates <- coordinates
    recipe
  }
  bowtie <- rbind(
    c(-80, 35), c(-76, 39), c(-80, 39), c(-76, 35), c(-80, 35)
  )
  unclosed <- aoi_recipe_test_shell()[-5L, , drop = FALSE]
  xyz <- lapply(aoi_recipe_test_positions(aoi_recipe_test_shell()), function(x) {
    c(x, 1)
  })
  non_finite <- aoi_recipe_test_positions(aoi_recipe_test_shell())
  non_finite[[2L]][[1L]] <- Inf

  invalid <- list(
    make_recipe(coordinates = list()),
    make_recipe(coordinates = list(aoi_recipe_test_positions(bowtie))),
    make_recipe(coordinates = list(aoi_recipe_test_positions(unclosed))),
    make_recipe(coordinates = list(xyz)),
    make_recipe(coordinates = list(non_finite)),
    make_recipe(type = "Point", coordinates = c(-78, 37)),
    make_recipe(type = "MultiPolygon", coordinates = list()),
    make_recipe(type = "MultiPolygon", coordinates = list(list()))
  )

  for (recipe in invalid) {
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_impl(recipe)
    })
  }
})

test_that("coordinate leaves reject overflow and every non-number JSON type", {
  baseline <- gx_aoi(aoi_recipe_test_polygon())$recipe
  baseline_json <- aoi_recipe_test_json(baseline)
  expect_true(grepl("[-80,35]", baseline_json, fixed = TRUE))

  for (token in c("1e309", "1234567890123456789012345678901234567890")) {
    hostile_json <- sub(
      "[-80,35]",
      paste0("[", token, ",35]"),
      baseline_json,
      fixed = TRUE
    )
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_json_impl(hostile_json)
    })
  }

  leaves <- list(
    string = list(-80, "35"),
    logical = list(-80, TRUE),
    null = list(-80, NULL)
  )
  for (leaf in leaves) {
    recipe <- aoi_recipe_test_clone(baseline)
    recipe$aoi$canonical_geojson$coordinates[[1L]][[1L]] <- leaf
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_impl(recipe)
    })
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_json_impl(aoi_recipe_test_json(recipe))
    })
  }
})

test_that("decoded R recipes reject data frames, matrices, and classed containers", {
  baseline <- gx_aoi(aoi_recipe_test_polygon())$recipe
  root_data_frame <- data.frame(
    contract_version = baseline$contract_version,
    aoi = I(list(baseline$aoi)),
    pipeline = I(list(baseline$pipeline)),
    check.names = FALSE
  )
  root_classed <- structure(
    aoi_recipe_test_clone(baseline),
    class = "hostile_recipe"
  )
  root_pairlist <- as.pairlist(aoi_recipe_test_clone(baseline))
  nested_aoi_pairlist <- aoi_recipe_test_clone(baseline)
  nested_aoi_pairlist$aoi <- as.pairlist(nested_aoi_pairlist$aoi)
  nested_pipeline_pairlist <- aoi_recipe_test_clone(baseline)
  nested_pipeline_pairlist$pipeline <- as.pairlist(
    nested_pipeline_pairlist$pipeline
  )
  matrix_position <- aoi_recipe_test_clone(baseline)
  matrix_position$aoi$canonical_geojson$coordinates[[1L]][[1L]] <-
    matrix(c(-80, 35), nrow = 1L)
  data_frame_position <- aoi_recipe_test_clone(baseline)
  data_frame_position$aoi$canonical_geojson$coordinates[[1L]][[1L]] <-
    data.frame(x = -80, y = 35)
  classed_position <- aoi_recipe_test_clone(baseline)
  classed_position$aoi$canonical_geojson$coordinates[[1L]][[1L]] <-
    structure(c(-80, 35), class = "hostile_position")

  for (recipe in list(
    root_data_frame,
    root_classed,
    root_pairlist,
    nested_aoi_pairlist,
    nested_pipeline_pairlist,
    matrix_position,
    data_frame_position,
    classed_position
  )) {
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_impl(recipe)
    })
  }
})

test_that("bounds, grid collapse, and antimeridian crossings are rejected", {
  baseline <- gx_aoi(aoi_recipe_test_polygon())$recipe
  with_ring <- function(matrix) {
    recipe <- aoi_recipe_test_clone(baseline)
    recipe$aoi$canonical_geojson$coordinates <- list(
      aoi_recipe_test_positions(matrix)
    )
    recipe
  }
  bad_longitude <- aoi_recipe_test_shell(dx = 260)
  bad_latitude <- aoi_recipe_test_shell(dy = 60)
  antimeridian <- rbind(
    c(179, 10), c(-179, 10), c(-179, 11), c(179, 11), c(179, 10)
  )
  grid_collapse <- rbind(
    c(-80, 35),
    c(-79.9999999996, 35),
    c(-79.9999999996, 35.0000000004),
    c(-80, 35.0000000004),
    c(-80, 35)
  )

  for (recipe in lapply(
    list(bad_longitude, bad_latitude, antimeridian, grid_collapse),
    with_ring
  )) {
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_impl(recipe)
    })
  }
})

test_that("JSON input is scalar text or raw bytes, never a path", {
  recipe <- gx_aoi("01020304", type = "huc")$recipe
  json <- aoi_recipe_test_json(recipe)
  path <- tempfile("aoi-recipe-", fileext = ".json")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  writeBin(charToRaw(json), path)

  invalid <- list(
    character(),
    c(json, json),
    NA_character_,
    factor(json),
    structure(json, names = "recipe"),
    matrix(json, nrow = 1L),
    structure(json, class = "hostile_json"),
    structure(charToRaw(json), names = rep("byte", nchar(json))),
    structure(charToRaw(json), class = "hostile_raw"),
    list(json),
    raw()
  )
  for (value in invalid) {
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_json_impl(value)
    })
  }

  aoi_recipe_test_expect_error(function() {
    gx_aoi_from_recipe_json_impl(path)
  })

  url_calls <- 0L
  testthat::local_mocked_bindings(
    url = function(...) {
      url_calls <<- url_calls + 1L
      rawConnection(charToRaw(json), open = "rb")
    },
    .package = "base"
  )
  aoi_recipe_test_expect_error(function() {
    gx_aoi_from_recipe_json_impl("https://example.invalid/aoi-recipe.json")
  })
  expect_identical(url_calls, 0L)
})

test_that("malformed, invalid UTF-8, controls, and every BOM are rejected", {
  valid <- charToRaw(aoi_recipe_test_json(
    gx_aoi("01020304", type = "huc")$recipe
  ))
  middle <- as.integer(length(valid) %/% 2L)
  invalid_utf8 <- c(charToRaw('{"x":"'), as.raw(0xff), charToRaw('"}'))
  nul <- c(valid[seq_len(middle)], as.raw(0x00), valid[-seq_len(middle)])
  boms <- list(
    utf8 = as.raw(c(0xef, 0xbb, 0xbf)),
    utf16_le = as.raw(c(0xff, 0xfe)),
    utf16_be = as.raw(c(0xfe, 0xff)),
    utf32_le = as.raw(c(0xff, 0xfe, 0x00, 0x00)),
    utf32_be = as.raw(c(0x00, 0x00, 0xfe, 0xff))
  )
  malformed <- list(
    charToRaw("{"),
    charToRaw("[]"),
    charToRaw("null"),
    charToRaw("42"),
    charToRaw(paste0(rawToChar(valid), " trailing")),
    invalid_utf8,
    nul,
    charToRaw('{"x":"\\u0000"}'),
    charToRaw('{"x":"\\uD800"}'),
    charToRaw('{"x":"\\uDC00"}')
  )
  malformed <- c(malformed, lapply(boms, c, valid))

  for (value in malformed) {
    aoi_recipe_test_expect_error(function() {
      gx_aoi_from_recipe_json_impl(value)
    })
  }
})

test_that("JSON nesting accepts depth seven and rejects depth eight", {
  expect_identical(geoconnexr:::gx_aoi_recipe_max_depth, 7L)
  depth_json <- function(depth) {
    paste0(strrep("[", depth), "0", strrep("]", depth))
  }

  depth_seven <- aoi_recipe_test_expect_error(function() {
    gx_aoi_from_recipe_json_impl(depth_json(7L))
  })
  expect_false(inherits(depth_seven, "gx_error_aoi_recipe_budget"))

  depth_eight <- tryCatch(
    geoconnexr:::gx_aoi_recipe_json_preflight(depth_json(8L)),
    error = identity
  )
  expect_s3_class(depth_eight, "gx_error_aoi_recipe_budget")
  aoi_recipe_test_expect_error(function() {
    gx_aoi_from_recipe_json_impl(depth_json(8L))
  })

  multipolygon <- gx_aoi(aoi_recipe_test_multipolygon())
  expect_identical(
    gx_aoi_from_recipe_json_impl(aoi_recipe_test_json(multipolygon$recipe)),
    multipolygon
  )
})

test_that("JSON member and byte bombs fail at bounded ceilings", {
  expect_equal(geoconnexr:::gx_aoi_recipe_max_members(), 350011)
  expect_equal(
    geoconnexr:::gx_aoi_recipe_max_structural_units(),
    350022
  )

  member_bomb <- paste0(
    '{"contract_version":"1.0.0","aoi":{"kind":"huc",',
    '"identifier":"01","members":[',
    paste(rep.int("0", 30L), collapse = ","),
    ']},"pipeline":{"start_stage":"aoi","end_stage":"catalog"}}'
  )
  testthat::local_mocked_bindings(
    gx_aoi_recipe_max_members = function() 16L,
    gx_aoi_recipe_max_structural_units = function() 1000L,
    .package = "geoconnexr"
  )
  member_budget <- tryCatch(
    geoconnexr:::gx_aoi_recipe_parse_json(charToRaw(member_bomb)),
    error = identity
  )
  expect_s3_class(member_budget, "gx_error_aoi_recipe_budget")
  aoi_recipe_test_expect_error(function() {
    gx_aoi_from_recipe_json_impl(charToRaw(member_bomb))
  })

  byte_bomb <- rep(
    as.raw(0x20),
    geoconnexr:::gx_aoi_recipe_max_json_bytes() + 1L
  )
  byte_budget <- tryCatch(
    geoconnexr:::gx_aoi_recipe_json_bytes(byte_bomb),
    error = identity
  )
  expect_s3_class(byte_budget, "gx_error_aoi_recipe_budget")
  aoi_recipe_test_expect_error(function() {
    gx_aoi_from_recipe_json_impl(byte_bomb)
  })
})

test_that("decoded spatial recipes reject the 100001st coordinate position", {
  expect_identical(geoconnexr:::gx_aoi_max_coordinates, 100000L)
  coordinate_bomb <- gx_aoi(aoi_recipe_test_polygon())$recipe
  coordinate_bomb$aoi$canonical_geojson$coordinates <- list(
    rep(
      list(c(-80, 35)),
      geoconnexr:::gx_aoi_max_coordinates + 1L
    )
  )

  testthat::local_mocked_bindings(
    gx_json_measure_complexity = function(...) list(exceeded = NA_character_),
    .package = "geoconnexr"
  )
  coordinate_budget <- tryCatch(
    geoconnexr:::gx_aoi_from_recipe_inner(coordinate_bomb),
    error = identity
  )
  expect_s3_class(coordinate_budget, "gx_error_aoi_recipe_budget")
  aoi_recipe_test_expect_error(function() {
    gx_aoi_from_recipe_impl(coordinate_bomb)
  })
})

test_that("recipe reconstruction never uses transport, graph, reference, or files", {
  state <- new.env(parent = emptyenv())
  state$calls <- character()
  blocked <- function(...) {
    state$calls <- c(state$calls, "external")
    stop("unexpected external access", call. = FALSE)
  }
  testthat::local_mocked_bindings(
    gx_http_request = blocked,
    gx_graph_execute_once = blocked,
    gx_ref_features = blocked,
    .package = "geoconnexr"
  )
  withr::local_options(list(
    geoconnexr.file_performer = blocked,
    geoconnexr.dns_resolver = blocked
  ))

  identifier <- gx_aoi("01020304", type = "huc")
  spatial <- gx_aoi(aoi_recipe_test_polygon())
  expect_identical(
    gx_aoi_from_recipe_impl(identifier$recipe),
    identifier
  )
  expect_identical(
    gx_aoi_from_recipe_json_impl(aoi_recipe_test_json(spatial$recipe)),
    spatial
  )
  expect_length(state$calls, 0L)
})

test_that("spatial recipe reconstruction restores PROJ network state", {
  expected <- gx_aoi(aoi_recipe_test_polygon())
  enabled <- TRUE
  network_url <- "https://cdn.proj.org"
  calls <- character()
  fake_proj_network <- function(enable = FALSE, url = character(0)) {
    if (missing(enable)) {
      calls <<- c(calls, "get")
      return(enabled)
    }
    if (isTRUE(enable)) {
      calls <<- c(calls, "enable")
      enabled <<- TRUE
      network_url <<- url
      return(invisible(network_url))
    }
    calls <<- c(calls, "disable")
    previous_url <- network_url
    enabled <<- FALSE
    previous_url
  }
  testthat::local_mocked_bindings(
    sf_proj_network = fake_proj_network,
    .package = "sf"
  )

  output <- gx_aoi_from_recipe_impl(expected$recipe)

  expect_identical(output, expected)
  expect_true(enabled)
  expect_identical(network_url, "https://cdn.proj.org")
  guard_calls <- c("get", "disable", "get", "enable")
  expect_gt(length(calls), 0L)
  expect_identical(length(calls) %% length(guard_calls), 0L)
  expect_identical(
    calls,
    rep(guard_calls, length(calls) %/% length(guard_calls))
  )

  calls <- character()
  enabled <- TRUE
  bad <- aoi_recipe_test_clone(expected$recipe)
  bad$aoi$wkb_sha256 <- strrep("0", 64L)
  aoi_recipe_test_expect_error(function() {
    gx_aoi_from_recipe_impl(bad)
  })
  expect_true(enabled)
  expect_gt(length(calls), 0L)
  expect_identical(length(calls) %% length(guard_calls), 0L)
  expect_identical(
    calls,
    rep(guard_calls, length(calls) %/% length(guard_calls))
  )
})

test_that("AOI recipe readers remain internal", {
  namespace <- asNamespace("geoconnexr")
  exports <- getNamespaceExports("geoconnexr")

  expect_true(exists(
    "gx_aoi_from_recipe_impl",
    envir = namespace,
    inherits = FALSE
  ))
  expect_true(exists(
    "gx_aoi_from_recipe_json_impl",
    envir = namespace,
    inherits = FALSE
  ))
  expect_false("gx_aoi_from_recipe_impl" %in% exports)
  expect_false("gx_aoi_from_recipe_json_impl" %in% exports)
})
