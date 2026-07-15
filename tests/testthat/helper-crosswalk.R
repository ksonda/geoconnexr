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
                              retries = 0L) {
  routed <- gx_cw_test_router(handler)
  withr::local_options(
    list(
      geoconnexr.performer = routed$performer,
      geoconnexr.dns_resolver = gx_cw_test_dns,
      geoconnexr.clock = gx_cw_test_clock,
      geoconnexr.cache_dir = withr::local_tempdir(),
      geoconnexr.offline = FALSE
    ),
    .local_envir = parent.frame()
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
