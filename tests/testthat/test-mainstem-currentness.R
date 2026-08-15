gx_currentness_test_clock <- function() {
  as.POSIXct("2026-08-15 12:00:00", tz = "UTC")
}

gx_currentness_test_dns <- function(host) {
  rep("93.184.216.34", length(host))
}

gx_currentness_test_json <- function(value) {
  charToRaw(as.character(jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", na = "null", digits = NA
  )))
}

gx_currentness_test_response <- function(request, body, status = 200L,
                                         content_type = "application/json") {
  list(
    status = as.integer(status),
    headers = list("Content-Type" = content_type),
    body = if (is.raw(body)) body else gx_currentness_test_json(body),
    url = request$url
  )
}

gx_currentness_test_queryables <- function() {
  list(
    type = "object",
    properties = list(
      geometry = list(
        format = "geometry-linestring",
        `x-ogc-role` = "primary-geometry"
      ),
      id = list(type = "string", `x-ogc-role` = "id"),
      uri = list(type = "string"),
      superseded = list(type = "boolean"),
      new_mainstemid = list(type = "string")
    ),
    additionalProperties = TRUE
  )
}

gx_currentness_test_feature <- function(id, superseded = FALSE,
                                        replacements = "") {
  uri <- paste0("https://geoconnex.us/ref/mainstems/", id)
  list(
    type = "Feature",
    id = id,
    properties = list(
      id = id,
      uri = uri,
      superseded = superseded,
      new_mainstemid = replacements
    ),
    geometry = list(
      type = "LineString",
      coordinates = list(c(-75.8, 39.0), c(-75.7, 39.1))
    )
  )
}

gx_currentness_test_client <- function(features, max_bytes = 1024L * 1024L) {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  performer <- function(request) {
    state$calls[[length(state$calls) + 1L]] <- request
    if (grepl("/collections/mainstems_v3/queryables$", request$url)) {
      return(gx_currentness_test_response(
        request,
        gx_currentness_test_queryables(),
        content_type = "application/schema+json"
      ))
    }
    path <- sub("[?].*$", "", request$url)
    id <- sub("^.*/collections/mainstems_v3/items/", "", path)
    if (identical(id, path) || is.null(features[[id]])) {
      stop("Unexpected currentness request: ", request$url, call. = FALSE)
    }
    gx_currentness_test_response(
      request,
      features[[id]],
      content_type = "application/geo+json"
    )
  }
  withr::local_options(
    list(
      geoconnexr.performer = performer,
      geoconnexr.dns_resolver = gx_currentness_test_dns,
      geoconnexr.clock = gx_currentness_test_clock,
      geoconnexr.cache_dir = withr::local_tempdir(),
      geoconnexr.offline = FALSE
    ),
    .local_envir = parent.frame()
  )
  list(
    client = gx_client(
      "reference", retries = 2L, max_bytes = max_bytes, cache = FALSE
    ),
    state = state
  )
}

test_that("mainstem currentness preserves status and every replacement", {
  one <- "https://geoconnex.us/ref/mainstems/147890"
  two <- "https://geoconnex.us/ref/mainstems/12726"
  three <- "https://geoconnex.us/ref/mainstems/200"
  four <- "https://geoconnex.us/ref/mainstems/300"
  setup <- gx_currentness_test_client(list(
    `147890` = gx_currentness_test_feature("147890"),
    `12726` = gx_currentness_test_feature(
      "12726", TRUE,
      "['https://geoconnex.us/ref/mainstems/147890']"
    ),
    `200` = gx_currentness_test_feature(
      "200", TRUE,
      paste0(
        "['https://geoconnex.us/ref/mainstems/201', ",
        "'https://geoconnex.us/ref/mainstems/202']"
      )
    ),
    `300` = gx_currentness_test_feature("300", TRUE)
  ))

  out <- gx_mainstem(c(one, two, three, four, one), client = setup$client)
  metadata <- attr(out, "gx_crosswalk")

  expect_s3_class(out, "gx_mainstem_currentness")
  expect_identical(out$input_index, c(1L, 2L, 3L, 3L, 4L, 5L))
  expect_identical(
    out$status,
    c(
      "current", "superseded", "superseded", "superseded",
      "superseded_unresolved", "current"
    )
  )
  expect_identical(
    out$replacement_uri,
    c(
      NA_character_, one,
      "https://geoconnex.us/ref/mainstems/201",
      "https://geoconnex.us/ref/mainstems/202",
      NA_character_, NA_character_
    )
  )
  expect_identical(out$replacement_index, c(NA_integer_, 1L, 1L, 2L, NA_integer_, NA_integer_))
  expect_identical(metadata$input_count, 5L)
  expect_identical(metadata$unique_input_count, 4L)
  expect_identical(metadata$current_input_count, 2L)
  expect_identical(metadata$superseded_input_count, 3L)
  expect_identical(metadata$replacement_count, 3L)
  expect_identical(nrow(metadata$requests), 8L)
  expect_identical(length(setup$state$calls), 8L)
  expect_identical(
    unique(out$diagnostics[[5L]]$code),
    "superseded_without_replacement"
  )
})

test_that("mainstem currentness validates replacement and state contracts", {
  invalid_replacements <- gx_currentness_test_client(list(
    `12726` = gx_currentness_test_feature(
      "12726", TRUE, "https://geoconnex.us/ref/mainstems/147890"
    )
  ))
  expect_error(
    gx_mainstem(
      "https://geoconnex.us/ref/mainstems/12726",
      client = invalid_replacements$client
    ),
    class = "gx_error_crosswalk_currentness_contract"
  )

  invalid_current <- gx_currentness_test_client(list(
    `147890` = gx_currentness_test_feature(
      "147890", FALSE,
      "['https://geoconnex.us/ref/mainstems/200']"
    )
  ))
  expect_error(
    gx_mainstem(
      "https://geoconnex.us/ref/mainstems/147890",
      client = invalid_current$client
    ),
    class = "gx_error_crosswalk_currentness_contract"
  )

  duplicate_replacements <- gx_currentness_test_client(list(
    `12726` = gx_currentness_test_feature(
      "12726", TRUE,
      paste0(
        "['https://geoconnex.us/ref/mainstems/147890', ",
        "'https://geoconnex.us/ref/mainstems/147890']"
      )
    )
  ))
  expect_error(
    gx_mainstem(
      "https://geoconnex.us/ref/mainstems/12726",
      client = duplicate_replacements$client
    ),
    class = "gx_error_crosswalk_currentness_contract"
  )
})

test_that("mainstem currentness validates inputs and budgets before transport", {
  setup <- gx_currentness_test_client(list(
    `147890` = gx_currentness_test_feature("147890")
  ))
  invalid <- list(
    147890,
    NA_character_,
    "",
    "http://geoconnex.us/ref/mainstems/147890",
    "https://geoconnex.us/ref/mainstems/0",
    "https://geoconnex.us/ref/mainstems/147890?x=1"
  )
  for (value in invalid) {
    expect_error(
      gx_mainstem(value, client = setup$client),
      class = "gx_error_crosswalk_input"
    )
  }
  expect_identical(length(setup$state$calls), 0L)

  out <- gx_mainstem(character(), client = setup$client)
  expect_s3_class(out, "gx_mainstem_currentness")
  expect_identical(nrow(out), 0L)
  expect_identical(length(setup$state$calls), 0L)

  withr::local_options(list(geoconnexr.crosswalk_max_requests = 3L))
  expect_error(
    gx_mainstem(
      "https://geoconnex.us/ref/mainstems/147890",
      client = setup$client
    ),
    class = "gx_error_crosswalk_budget"
  )
  expect_identical(length(setup$state$calls), 0L)
})

test_that("mainstem currentness rejects mismatched live identity", {
  feature <- gx_currentness_test_feature("147890")
  feature$properties$uri <- "https://geoconnex.us/ref/mainstems/147891"
  setup <- gx_currentness_test_client(list(`147890` = feature))

  expect_error(
    gx_mainstem(
      "https://geoconnex.us/ref/mainstems/147890",
      client = setup$client
    ),
    class = "gx_error_crosswalk_currentness_contract"
  )
})
