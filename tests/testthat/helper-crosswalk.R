gx_cw_test_clock <- function() {
  as.POSIXct("2026-07-13 22:00:00", tz = "UTC")
}

gx_cw_test_dns <- function(host) {
  rep("93.184.216.34", length(host))
}

gx_cw_test_fixture <- function(name) {
  path <- testthat::test_path("..", "fixtures", "crosswalk", name)
  readBin(path, what = "raw", n = file.info(path)$size)
}

gx_cw_test_json <- function(value) {
  charToRaw(as.character(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )))
}

gx_cw_test_response <- function(request, status = 200L, body = raw(),
                                content_type = "application/json",
                                headers = list()) {
  headers[["Content-Type"]] <- content_type
  list(
    status = as.integer(status),
    headers = headers,
    body = if (is.raw(body)) body else charToRaw(body),
    url = request$url
  )
}

gx_cw_test_router <- function(handler) {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  performer <- function(request) {
    state$calls[[length(state$calls) + 1L]] <- request
    handler(request, state)
  }
  list(performer = performer, state = state)
}

gx_cw_test_client <- function(handler, max_bytes = 1024L * 1024L,
                              retries = 0L,
                              .local_envir = parent.frame()) {
  routed <- gx_cw_test_router(handler)
  withr::local_options(
    list(
      geoconnexr.performer = routed$performer,
      geoconnexr.dns_resolver = gx_cw_test_dns,
      geoconnexr.clock = gx_cw_test_clock,
      geoconnexr.cache_dir = withr::local_tempdir(),
      geoconnexr.offline = FALSE
    ),
    .local_envir = .local_envir
  )
  list(
    client = gx_client(
      "reference",
      retries = retries,
      max_bytes = max_bytes,
      cache = FALSE
    ),
    state = routed$state
  )
}

gx_cw_test_feature <- function(id, provider_id,
                               uri = paste0("https://geoconnex.us/ref/gages/", id),
                               mainstem_uri = "https://geoconnex.us/ref/mainstems/1622734",
                               comid = 17789327) {
  properties <- list(
    id = id,
    uri = uri,
    provider_id = provider_id,
    nhdpv2_comid = comid,
    mainstem_uri = mainstem_uri
  )
  properties <- properties[!vapply(properties, is.null, logical(1))]
  list(
    type = "Feature",
    id = id,
    properties = properties,
    geometry = list(type = "Point", coordinates = c(-107, 35))
  )
}

gx_cw_test_collection <- function(features, matched = length(features),
                                  links = list()) {
  list(
    type = "FeatureCollection",
    features = features,
    numberMatched = matched,
    numberReturned = length(features),
    links = links
  )
}

gx_cw_test_handler <- function(items) {
  force(items)
  function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_cw_test_response(
        request,
        body = gx_cw_test_fixture("queryables-gages.min.json"),
        content_type = "application/schema+json"
      ))
    }
    parsed <- httr2::url_parse(request$url)
    provider_id <- parsed$query$provider_id %||% ""
    value <- items[[provider_id]]
    if (is.null(value)) value <- gx_cw_test_collection(list(), 0L)
    gx_cw_test_response(
      request,
      body = if (is.raw(value)) value else gx_cw_test_json(value),
      content_type = "application/geo+json"
    )
  }
}

gx_currentness_test_clock <- function() {
  as.POSIXct("2026-08-15 23:59:59", tz = "UTC")
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
