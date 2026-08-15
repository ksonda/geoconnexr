gx_huc12_intersection_test_queryables <- function(collection) {
  properties <- if (identical(collection, "hu12")) {
    list(
      geometry = list(
        format = "geometry-multipolygon",
        `x-ogc-role` = "primary-geometry"
      ),
      huc12 = list(type = "string", `x-ogc-role` = "id"),
      uri = list(type = "string")
    )
  } else {
    list(
      geometry = list(
        format = "geometry-linestring",
        `x-ogc-role` = "primary-geometry"
      ),
      id = list(type = "string", `x-ogc-role` = "id"),
      uri = list(type = "string"),
      outlet_drainagearea_sqkm = list(type = "number"),
      outlet_nhdpv2huc12 = list(type = "string"),
      superseded = list(type = "boolean"),
      new_mainstemid = list(type = "string")
    )
  }
  list(type = "object", properties = properties, additionalProperties = TRUE)
}

gx_huc12_intersection_test_huc <- function(huc12) {
  list(
    type = "Feature",
    id = huc12,
    properties = list(
      huc12 = huc12,
      uri = paste0("https://geoconnex.us/ref/huc12/", huc12)
    ),
    geometry = list(
      type = "MultiPolygon",
      coordinates = list(list(list(
        c(0, 0), c(1, 0), c(0, 1), c(0, 0)
      )))
    )
  )
}

gx_huc12_intersection_test_mainstem <- function(
    id,
    coordinates,
    outlet_huc12 = "",
    drainage_area = 1,
    superseded = FALSE,
    replacements = "") {
  list(
    type = "Feature",
    id = id,
    properties = list(
      id = id,
      uri = paste0("https://geoconnex.us/ref/mainstems/", id),
      outlet_drainagearea_sqkm = drainage_area,
      outlet_nhdpv2huc12 = if (nzchar(outlet_huc12)) {
        paste0("https://geoconnex.us/nhdplusv2/huc12/", outlet_huc12)
      } else {
        ""
      },
      superseded = superseded,
      new_mainstemid = replacements
    ),
    geometry = list(type = "LineString", coordinates = coordinates)
  )
}

gx_huc12_intersection_test_client <- function(hucs, candidates) {
  gx_cw_test_client(function(request, state) {
    parsed <- httr2::url_parse(request$url)
    if (endsWith(parsed$path, "/collections/hu12/queryables")) {
      return(gx_cw_test_response(
        request,
        body = gx_cw_test_json(gx_huc12_intersection_test_queryables("hu12")),
        content_type = "application/schema+json"
      ))
    }
    if (endsWith(parsed$path, "/collections/mainstems_v3/queryables")) {
      return(gx_cw_test_response(
        request,
        body = gx_cw_test_json(gx_huc12_intersection_test_queryables("mainstems_v3")),
        content_type = "application/schema+json"
      ))
    }
    if (endsWith(parsed$path, "/collections/hu12/items")) {
      requested <- parsed$query$huc12 %||% ""
      feature <- hucs[[requested]]
      features <- if (is.null(feature)) list() else list(feature)
      return(gx_cw_test_response(
        request,
        body = gx_cw_test_json(gx_cw_test_collection(features)),
        content_type = "application/geo+json"
      ))
    }
    if (endsWith(parsed$path, "/collections/mainstems_v3/items")) {
      return(gx_cw_test_response(
        request,
        body = gx_cw_test_json(gx_cw_test_collection(candidates)),
        content_type = "application/geo+json"
      ))
    }
    stop("Unexpected HUC12 intersection request: ", request$url, call. = FALSE)
  }, max_bytes = 1024L * 1024L, .local_envir = parent.frame())
}

gx_huc12_intersection_test_candidates <- function(huc12) {
  list(
    gx_huc12_intersection_test_mainstem(
      "4", list(c(0, 0.1), c(0.8, 0.1)), huc12, 80,
      superseded = TRUE,
      replacements = "['https://geoconnex.us/ref/mainstems/40']"
    ),
    gx_huc12_intersection_test_mainstem(
      "3", list(c(0, 0.4), c(0.5, 0.4)), drainage_area = 100
    ),
    gx_huc12_intersection_test_mainstem(
      "1", list(c(0, 0.2), c(0.7, 0.2)), huc12, 10
    ),
    gx_huc12_intersection_test_mainstem(
      "5", list(c(0.8, 0.8), c(0.9, 0.9)), drainage_area = 200
    ),
    gx_huc12_intersection_test_mainstem(
      "2", list(c(0, 0), c(0.9, 0.9)), drainage_area = 20
    )
  )
}

test_that("HUC12 intersections return all matches in disclosed rank order", {
  huc12 <- "010100020101"
  absent <- "010100020102"
  setup <- gx_huc12_intersection_test_client(
    setNames(list(gx_huc12_intersection_test_huc(huc12)), huc12),
    gx_huc12_intersection_test_candidates(huc12)
  )

  out <- gx_huc12_to_mainstem(
    c(huc12, absent, huc12),
    method = "intersects",
    client = setup$client
  )
  metadata <- attr(out, "gx_crosswalk")

  expect_s3_class(out, "gx_huc12_intersection_crosswalk")
  expect_identical(out$input_index, c(1L, 1L, 1L, 1L, 2L, 3L, 3L, 3L, 3L))
  expect_identical(
    out$mainstem_uri,
    c(
      "https://geoconnex.us/ref/mainstems/1",
      "https://geoconnex.us/ref/mainstems/2",
      "https://geoconnex.us/ref/mainstems/3",
      "https://geoconnex.us/ref/mainstems/4",
      NA_character_,
      "https://geoconnex.us/ref/mainstems/1",
      "https://geoconnex.us/ref/mainstems/2",
      "https://geoconnex.us/ref/mainstems/3",
      "https://geoconnex.us/ref/mainstems/4"
    )
  )
  expect_identical(out$match_index, c(1:4, NA_integer_, 1:4))
  expect_identical(out$status, c(rep("matched", 4), "not_found", rep("matched", 4)))
  expect_identical(out$outlet_huc12_match[c(1L, 4L)], c(TRUE, TRUE))
  expect_identical(out$mainstem_status[1:4], c(
    "current", "current", "current", "superseded"
  ))
  expect_identical(
    out$replacement_uris[[4L]],
    "https://geoconnex.us/ref/mainstems/40"
  )
  expect_true(all(diff(out$intersection_length_km[2:3]) < 0))
  expect_identical(metadata$candidate_count, 5L)
  expect_identical(metadata$unique_intersection_count, 4L)
  expect_identical(metadata$match_count, 8L)
  expect_identical(metadata$not_found_input_count, 1L)
  expect_identical(nrow(metadata$requests), 5L)
  expect_identical(length(setup$state$calls), 5L)
})

test_that("HUC12 intersection zero rows and client types fail cleanly", {
  huc12 <- "010100020101"
  setup <- gx_huc12_intersection_test_client(list(), list())

  out <- gx_huc12_to_mainstem(
    character(), method = "intersects", client = setup$client
  )
  expect_s3_class(out, "gx_huc12_intersection_crosswalk")
  expect_identical(nrow(out), 0L)
  expect_identical(length(setup$state$calls), 0L)

  nldi <- gx_client("nldi", cache = FALSE)
  expect_error(
    gx_huc12_to_mainstem(huc12, method = "intersects", client = nldi),
    class = "gx_error_crosswalk_input"
  )
})

test_that("HUC12 intersections enforce the candidate ceiling", {
  huc12 <- "010100020101"
  setup <- gx_huc12_intersection_test_client(
    setNames(list(gx_huc12_intersection_test_huc(huc12)), huc12),
    gx_huc12_intersection_test_candidates(huc12)
  )
  withr::local_options(list(geoconnexr.crosswalk_max_matches = 4L))

  expect_error(
    gx_huc12_to_mainstem(huc12, method = "intersects", client = setup$client),
    class = "gx_error_crosswalk_budget"
  )
})

test_that("HUC12 intersections reject contradictory mainstem identity", {
  huc12 <- "010100020101"
  candidate <- gx_huc12_intersection_test_mainstem(
    "1", list(c(0, 0.2), c(0.7, 0.2)), huc12
  )
  candidate$properties$uri <- "https://geoconnex.us/ref/mainstems/2"
  setup <- gx_huc12_intersection_test_client(
    setNames(list(gx_huc12_intersection_test_huc(huc12)), huc12),
    list(candidate)
  )

  expect_error(
    gx_huc12_to_mainstem(huc12, method = "intersects", client = setup$client),
    class = "gx_error_crosswalk_identity"
  )
})
