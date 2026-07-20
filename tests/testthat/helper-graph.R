gx_graph_fixture_path <- function(name) {
  testthat::test_path("..", "fixtures", "sparql", name)
}

gx_graph_fixture_raw <- function(name) {
  path <- gx_graph_fixture_path(name)
  readBin(path, what = "raw", n = file.info(path)$size[[1]])
}

gx_graph_test_parse <- function(body, ...) {
  defaults <- list(
    expected = "select",
    max_rows = 100L,
    max_variables = 100L,
    max_bound_terms = 1000L,
    max_links = 10L,
    max_members = 10000L,
    max_atomic_bytes = 1024L * 1024L,
    max_depth = 32L
  )
  args <- utils::modifyList(defaults, list(...))
  do.call(
    geoconnexr:::gx_graph_parse_results,
    c(list(body = body), args)
  )
}

gx_graph_test_execute <- function(query, client, ...) {
  defaults <- list(
    expected = "select",
    max_rows = 100L,
    max_variables = 100L,
    max_bound_terms = 1000L,
    max_links = 10L,
    max_requests = 4L,
    max_total_bytes = 1024L * 1024L,
    max_members = 10000L,
    max_atomic_bytes = 1024L * 1024L,
    max_depth = 32L
  )
  args <- utils::modifyList(defaults, list(...))
  do.call(
    geoconnexr:::gx_graph_execute_once,
    c(list(query = query, client = client), args)
  )
}

gx_graph_test_public_dns <- function(host) {
  rep("93.184.216.34", length(host))
}

gx_graph_test_clock <- function() {
  as.POSIXct("2026-07-14 12:00:00", tz = "UTC")
}

gx_graph_test_response <- function(body, status = 200L,
                                   content_type = "application/sparql-results+json",
                                   headers = list()) {
  force(body)
  force(status)
  force(content_type)
  force(headers)
  function(request) {
    response_headers <- headers
    if (!is.null(content_type)) {
      response_headers[["Content-Type"]] <- content_type
    }
    list(
      status = as.integer(status),
      headers = response_headers,
      body = body,
      url = request$url
    )
  }
}

gx_graph_test_script <- function(responses) {
  state <- new.env(parent = emptyenv())
  state$index <- 0L
  state$calls <- list()
  performer <- function(request) {
    state$index <- state$index + 1L
    state$calls[[state$index]] <- request
    if (state$index > length(responses)) {
      stop("Unexpected graph request in deterministic test.", call. = FALSE)
    }
    response <- responses[[state$index]]
    if (is.function(response)) response(request) else response
  }
  list(performer = performer, state = state)
}

gx_graph_test_seed_cache <- function(client, query, body) {
  query_raw <- charToRaw(enc2utf8(query))
  request_headers <- list(
    Accept = "application/sparql-results+json",
    `content-type` = "application/sparql-query",
    `accept-encoding` = "identity"
  )
  key <- geoconnexr:::gx_cache_key(
    client,
    method = "POST",
    url = client$base_url,
    headers = request_headers,
    body = query_raw
  )
  response <- list(
    status = 200L,
    headers = list(
      `content-type` = "application/sparql-results+json"
    ),
    body = body,
    url = geoconnexr:::gx_canonical_url(client$base_url),
    retrieved_at = geoconnexr:::gx_now(),
    body_sha256 = digest::digest(body, algo = "sha256", serialize = FALSE),
    bytes = length(body),
    from_cache = FALSE,
    cache_origin = "network"
  )
  backend <- geoconnexr:::gx_cache_backend(client$cache_dir)
  backend$set(key, list(
    cache_schema_version = geoconnexr:::.gx_cache_schema_version,
    request_id = key,
    response = response
  ))
  list(key = key, backend = backend, response = response)
}
