gx_jsonld_test_clock <- function() {
  as.POSIXct("2026-07-13 04:00:00", tz = "UTC")
}

gx_jsonld_test_dns <- function(host) {
  rep("93.184.216.34", length(host))
}

gx_jsonld_test_route_performer <- function(routes) {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  performer <- function(request) {
    state$calls[[length(state$calls) + 1L]] <- request
    key <- paste(request$method, request$url)
    route <- routes[[key]]
    if (is.null(route)) {
      stop("Unexpected request: ", key, call. = FALSE)
    }
    if (is.function(route)) {
      return(route(request))
    }
    route$url <- request$url
    route
  }
  list(performer = performer, state = state)
}

gx_jsonld_test_response <- function(status = 200L, headers = list(), body = raw()) {
  list(status = as.integer(status), headers = headers, body = body, url = "replaced-by-router")
}

gx_jsonld_test_client <- function(routes, max_bytes = 1024L * 1024L, retries = 0L, cache = FALSE) {
  routed <- gx_jsonld_test_route_performer(routes)
  withr::local_options(
    list(
      geoconnexr.performer = routed$performer,
      geoconnexr.dns_resolver = gx_jsonld_test_dns,
      geoconnexr.clock = gx_jsonld_test_clock,
      geoconnexr.cache_dir = withr::local_tempdir(),
      geoconnexr.offline = FALSE
    ),
    .local_envir = parent.frame()
  )
  list(
    client = gx_client("pid", retries = retries, max_bytes = max_bytes, cache = cache),
    state = routed$state
  )
}

gx_jsonld_fixture_raw <- function(...) {
  readBin(
    testthat::test_path("..", "fixtures", "jsonld", ...),
    what = "raw",
    n = file.info(testthat::test_path("..", "fixtures", "jsonld", ...))$size
  )
}

test_that("gx_jsonld negotiates direct JSON-LD through the bounded transport", {
  pid <- "https://geoconnex.us/ref/gages/test"
  landing <- "https://example.net/site/test"
  body <- charToRaw('{"@context":"https://schema.org","@id":"https://example.net/site/test","@type":"Place","name":"Test"}')
  setup <- gx_jsonld_test_client(list(
    "HEAD https://geoconnex.us/ref/gages/test" = gx_jsonld_test_response(303L, list(Location = landing)),
    "HEAD https://example.net/site/test" = gx_jsonld_test_response(200L, list(`Content-Type` = "text/html")),
    "GET https://example.net/site/test" = gx_jsonld_test_response(200L, list(`Content-Type` = "application/ld+json; charset=utf-8"), body)
  ))

  out <- gx_jsonld(pid, client = setup$client)

  expect_s3_class(out, "gx_jsonld")
  expect_equal(out$pid_uri, pid)
  expect_equal(out$source_url, landing)
  expect_equal(out$retrieval_mode, "negotiated")
  expect_equal(out$representation, "expanded")
  expect_equal(out$content_bytes, length(body))
  expect_match(out$content_sha256, "^[0-9a-f]{64}$")
  expect_equal(nrow(out$requests), 3L)
  expect_equal(length(setup$state$calls), 3L)
  get <- setup$state$calls[[3]]
  expect_match(get$headers[["accept"]], "application/ld\\+json")
  expect_identical(get$headers[["accept-encoding"]], "identity")
  expect_true(length(get$resolved_ip) >= 1L)
  expect_true(all(out$diagnostics$severity == "info"))
})

test_that("gx_jsonld uses embedded JSON-LD before an advertised alternate", {
  pid <- "https://geoconnex.us/id/embedded"
  landing <- "https://example.net/page/embedded"
  html <- gx_jsonld_fixture_raw("retrieval", "embedded-jsonld.html")
  setup <- gx_jsonld_test_client(list(
    "HEAD https://geoconnex.us/id/embedded" = gx_jsonld_test_response(303L, list(Location = landing)),
    "HEAD https://example.net/page/embedded" = gx_jsonld_test_response(200L),
    "GET https://example.net/page/embedded" = gx_jsonld_test_response(200L, list(`Content-Type` = "text/html; charset=utf-8"), html)
  ))

  out <- gx_jsonld(pid, as = "raw", client = setup$client)

  expect_equal(out$retrieval_mode, "embedded")
  expect_equal(out$document[["@id"]], "https://example.net/site/embedded")
  expect_equal(length(setup$state$calls), 3L)
  expect_contains(out$diagnostics$code, "empty_jsonld_script")
})

test_that("gx_jsonld follows one safe relative JSON-LD alternate", {
  pid <- "https://geoconnex.us/id/alternate"
  landing <- "https://example.net/page/alternate"
  html <- gx_jsonld_fixture_raw("retrieval", "alternate-jsonld.html")
  json <- charToRaw('{"@context":{"schema":"https://schema.org/"},"@id":"https://example.net/site/alternate","@type":"schema:Place"}')
  setup <- gx_jsonld_test_client(list(
    "HEAD https://geoconnex.us/id/alternate" = gx_jsonld_test_response(303L, list(Location = landing)),
    "HEAD https://example.net/page/alternate" = gx_jsonld_test_response(200L),
    "GET https://example.net/page/alternate" = gx_jsonld_test_response(200L, list(`Content-Type` = "text/html"), html),
    "GET https://example.net/page/alternate?f=jsonld" = gx_jsonld_test_response(200L, list(`Content-Type` = "application/ld+json"), json)
  ), retries = 3L)

  out <- gx_jsonld(pid, client = setup$client)

  expect_equal(out$retrieval_mode, "alternate")
  expect_equal(out$source_url, "https://example.net/page/alternate?f=jsonld")
  expect_equal(nrow(out$requests), 4L)
  expect_equal(length(setup$state$calls), 4L)
  expect_match(setup$state$calls[[4]]$headers[["accept"]], "application/ld\\+json")
  expect_true(all(vapply(setup$state$calls, `[[`, integer(1), "retries") == 0L))
  expect_contains(out$diagnostics$code, "retries_disabled")
  expect_false(grepl("f=jsonld", out$requests$url[[4]], fixed = TRUE))
})

test_that("gx_jsonld never fetches unsafe alternates", {
  pid <- "https://geoconnex.us/id/unsafe"
  landing <- "https://example.net/page/unsafe"
  setup <- gx_jsonld_test_client(list(
    "HEAD https://geoconnex.us/id/unsafe" = gx_jsonld_test_response(303L, list(Location = landing)),
    "HEAD https://example.net/page/unsafe" = gx_jsonld_test_response(200L),
    "GET https://example.net/page/unsafe" = gx_jsonld_test_response(
      200L,
      list(`Content-Type` = "text/html"),
      gx_jsonld_fixture_raw("retrieval", "unsafe-alternate.html")
    )
  ))

  expect_error(
    gx_jsonld(pid, client = setup$client),
    class = "gx_error_jsonld_missing"
  )
  expect_equal(length(setup$state$calls), 3L)
})

test_that("remote contexts are allowlisted without invoking the jsonld loader", {
  called <- character()
  testthat::local_mocked_bindings(
    download = function(url) {
      called <<- c(called, url)
      stop("Unexpected remote context download.", call. = FALSE)
    },
    .package = "jsonld"
  )
  known <- '{"@context":"https://schema.org","@id":"https://example.net/known","name":"Known"}'
  expect_no_error(gx_parse_location(known))
  expect_length(called, 0L)

  unknown <- gx_jsonld_fixture_raw("negative", "unknown-remote-context.json")
  expect_error(gx_parse_location(unknown), class = "gx_error_jsonld_context")
  expect_length(called, 0L)
  imported <- gx_jsonld_fixture_raw("negative", "context-import.json")
  expect_error(gx_parse_location(imported), class = "gx_error_jsonld_context")
  expect_length(called, 0L)
})

test_that("depth is checked before JSON-LD expansion", {
  withr::local_options(geoconnexr.jsonld_max_depth = 64L)
  at_limit <- paste0(strrep("[", 63L), "{}", strrep("]", 63L))
  over_limit <- paste0(strrep("[", 64L), "{}", strrep("]", 64L))
  expect_no_error(gx_parse_location(at_limit))
  expect_error(gx_parse_location(over_limit), class = "gx_error_jsonld_too_deep")
  expect_no_error(gx_parse_location('{"brackets":"[[[{{{","quote":"\\\""}'))
})

test_that("M2 network code does not bypass the shared transport", {
  namespace <- asNamespace("geoconnexr")
  object_names <- ls(namespace, all.names = TRUE)
  objects <- mget(object_names, envir = namespace, inherits = FALSE)
  direct <- object_names[vapply(objects, function(object) {
    is.function(object) && any(grepl(
      "httr2::request\\s*\\(|curl::curl_fetch",
      deparse(body(object))
    ))
  }, logical(1))]

  expect_identical(direct, "gx_default_performer")
})

test_that("request metadata redacts credentials and sensitive URLs bypass cache", {
  unsafe <- "https://user:secret@example.net/data?token=value&f=jsonld#access_token"
  redacted <- geoconnexr:::gx_redact_url(unsafe)

  expect_false(grepl("user|secret|value|jsonld|access_token", redacted))
  expect_false(geoconnexr:::gx_cache_allowed(list(), "https://example.net/data?token=value"))
  expect_false(geoconnexr:::gx_cache_allowed(list(), "https://example.net/data?api_key=value"))
  expect_false(geoconnexr:::gx_cache_allowed(list(), "https://example.net/data?X-Amz-Signature=value"))
  expect_false(geoconnexr:::gx_cache_allowed(list(), "https://example.net/data?format=json"))
  expect_true(geoconnexr:::gx_cache_allowed(list(), "https://example.net/data"))

  malformed <- c(
    "https://evil.invalid/context?=TOPSECRET",
    "https://evil.invalid/context?TOPSECRET"
  )
  sanitized <- vapply(malformed, geoconnexr:::gx_redact_url, character(1))
  expect_false(any(grepl("TOPSECRET", sanitized, fixed = TRUE)))
  expect_match(sanitized, "[?][[]redacted[]]", all = TRUE)

  message <- tryCatch(
    gx_parse_location('{"@context":"https://user:secret@example.net/context?token=value"}'),
    error = conditionMessage
  )
  expect_false(grepl("user|secret|value", message))
})

test_that("the pinned Schema.org context preserves IRI coercion", {
  prepared <- geoconnexr:::gx_prepare_jsonld(list(
    `@context` = "https://schema.org",
    url = "https://example.net/resource",
    name = "Resource"
  ))
  node <- prepared$expanded[[1]]

  expect_equal(
    node[["http://schema.org/url"]][[1]][["@id"]],
    "https://example.net/resource"
  )
  expect_equal(node[["http://schema.org/name"]][[1]][["@value"]], "Resource")
})

test_that("prefix repair respects JSON-LD scope and does not rewrite literals", {
  scoped <- list(
    `@context` = list(
      schema = "https://schema.org/",
      hyf = "https://www.opengis.net/def/schema/hy_features/hyf/",
      geo = "http://www.opengis.net/ont/geosparql#"
    ),
    `@id` = "https://example.net/site/scoped",
    `@type` = "hyf:HY_HydroLocation",
    `schema:name` = "gsp:literal-value",
    `gsp:hasGeometry` = list(`gsp:asWKT` = "POINT (1 2)"),
    `schema:subjectOf` = list(
      `@context` = list(gsp = "http://www.opengis.net/ont/geosparql#"),
      `@type` = "schema:Dataset",
      `schema:name` = "Nested"
    )
  )
  prepared <- geoconnexr:::gx_prepare_jsonld(scoped)
  location <- gx_parse_location(scoped)

  expect_contains(prepared$diagnostics$code, "known_prefix_repaired")
  expect_equal(location$geometry_wkt, "POINT (1 2)")
  expect_equal(location$name, "gsp:literal-value")
})

test_that("HTML parsing accepts bounded non-UTF-8 landing pages", {
  pid <- "https://geoconnex.us/id/latin1"
  landing <- "https://example.net/page/latin1"
  before <- charToRaw('<!doctype html><html><head><script type="application/ld+json">{"@context":{"schema":"https://schema.org/"},"@id":"https://example.net/site/latin1","@type":"schema:Place"}</script></head><body>')
  html <- c(before, as.raw(233L), charToRaw("</body></html>"))
  setup <- gx_jsonld_test_client(list(
    "HEAD https://geoconnex.us/id/latin1" = gx_jsonld_test_response(303L, list(Location = landing)),
    "HEAD https://example.net/page/latin1" = gx_jsonld_test_response(200L),
    "GET https://example.net/page/latin1" = gx_jsonld_test_response(200L, list(`Content-Type` = "text/html; charset=iso-8859-1"), html)
  ))

  out <- gx_jsonld(pid, as = "raw", client = setup$client)
  expect_equal(out$retrieval_mode, "embedded")
  expect_equal(out$document[["@id"]], "https://example.net/site/latin1")
})

test_that("JSON-LD input and expansion budgets fail with stable conditions", {
  withr::local_options(geoconnexr.jsonld_max_local_bytes = 10L)
  expect_error(gx_parse_location('{"long":"this is not parsed"}'), class = "gx_error_jsonld_too_large")

  deep <- list()
  for (i in seq_len(80L)) deep <- list(deep)
  withr::local_options(geoconnexr.jsonld_max_local_bytes = 1024L^2)
  expect_error(gx_parse_location(deep), class = "gx_error_jsonld_too_deep")

  withr::local_options(geoconnexr.jsonld_max_members = 2L)
  expect_error(gx_parse_location('{"a":1,"b":2,"c":3}'), class = "gx_error_jsonld_too_large")

  withr::local_options(
    geoconnexr.jsonld_max_members = 100L,
    geoconnexr.jsonld_max_context_bytes = 10L
  )
  expect_error(
    gx_parse_location('{"@context":{"long":"https://example.net/a/long/context/value"},"long":"x"}'),
    class = "gx_error_jsonld_too_large"
  )

  withr::local_options(
    geoconnexr.jsonld_max_context_bytes = 1024L,
    geoconnexr.jsonld_max_expanded_bytes = 5L
  )
  expect_error(
    gx_parse_location('{"https://schema.org/name":[{"@value":"expanded value"}]}'),
    class = "gx_error_jsonld_too_large"
  )

  withr::local_options(
    geoconnexr.jsonld_max_members = 2L,
    geoconnexr.jsonld_max_local_bytes = 1024L^2
  )
  expect_error(
    gx_parse_location(list(junk = rep("a", 100000L))),
    class = "gx_error_jsonld_too_large"
  )
})

test_that("untrusted parser details are not attached to public conditions", {
  error <- tryCatch(
    gx_parse_location('{"token":"TOPSECRET","broken":}'),
    error = identity
  )

  expect_s3_class(error, "gx_error_jsonld_syntax")
  expect_false(grepl("TOPSECRET", conditionMessage(error), fixed = TRUE))
  expect_false(grepl("TOPSECRET", paste(format(error), collapse = "\n"), fixed = TRUE))

  context_error <- tryCatch(
    gx_parse_location('{"@context":"https://evil.invalid/context?token=TOPSECRET"}'),
    error = identity
  )
  expect_s3_class(context_error, "gx_error_jsonld_context")
  expect_false(grepl("TOPSECRET", conditionMessage(context_error), fixed = TRUE))
  expect_false(grepl("TOPSECRET", paste(format(context_error), collapse = "\n"), fixed = TRUE))
})

test_that("bundled remote contexts are preflighted before repeated replacement", {
  source <- list(`@context` = as.list(rep("https://schema.org", 20L)))
  withr::local_options(
    geoconnexr.jsonld_max_members = 100000L,
    geoconnexr.jsonld_max_context_bytes = 200000L
  )

  expect_error(
    gx_parse_location(source),
    class = "gx_error_jsonld_too_large"
  )
})

test_that("retrieval request and cumulative byte budgets include PID resolution", {
  pid <- "https://geoconnex.us/id/budget"
  landing <- "https://example.net/page/budget"
  body <- charToRaw('{"@id":"https://example.net/site/budget"}')
  routes <- list(
    "HEAD https://geoconnex.us/id/budget" = gx_jsonld_test_response(303L, list(Location = landing)),
    "HEAD https://example.net/page/budget" = gx_jsonld_test_response(200L),
    "GET https://example.net/page/budget" = gx_jsonld_test_response(200L, list(`Content-Type` = "application/ld+json"), body)
  )
  requests <- gx_jsonld_test_client(routes)
  withr::local_options(geoconnexr.jsonld_max_requests = 2L)
  expect_error(gx_jsonld(pid, client = requests$client), class = "gx_error_jsonld_budget")
  expect_equal(length(requests$state$calls), 2L)

  bytes <- gx_jsonld_test_client(routes)
  withr::local_options(geoconnexr.jsonld_max_requests = 16L, geoconnexr.jsonld_total_bytes = length(body) - 1L)
  expect_error(gx_jsonld(pid, client = bytes$client), class = "gx_error_jsonld_budget")
  expect_equal(length(bytes$state$calls), 3L)
})

test_that("JSON-LD transport failures use redacted boundary conditions", {
  pid <- "https://geoconnex.us/id/transport"
  landing <- "https://example.net/page/transport?token=TOPSECRET"
  setup <- gx_jsonld_test_client(list(
    "HEAD https://geoconnex.us/id/transport" = gx_jsonld_test_response(
      303L, list(Location = landing)
    ),
    "HEAD https://example.net/page/transport?token=TOPSECRET" =
      gx_jsonld_test_response(200L),
    "GET https://example.net/page/transport?token=TOPSECRET" = function(request) {
      stop("transport failed for ", request$url)
    }
  ))

  error <- tryCatch(gx_jsonld(pid, client = setup$client), error = identity)

  expect_s3_class(error, "gx_error_jsonld_transport")
  expect_s3_class(error, "gx_error_jsonld")
  expect_false(grepl("TOPSECRET", conditionMessage(error), fixed = TRUE))
  expect_false(grepl("TOPSECRET", paste(format(error), collapse = "\n"), fixed = TRUE))
})

test_that("HTML JSON-LD candidates are bounded", {
  pid <- "https://geoconnex.us/id/many-scripts"
  landing <- "https://example.net/page/many-scripts"
  html <- charToRaw(paste0(
    "<html><head>",
    paste(rep('<script type="application/ld+json"></script>', 2L), collapse = ""),
    "</head></html>"
  ))
  setup <- gx_jsonld_test_client(list(
    "HEAD https://geoconnex.us/id/many-scripts" = gx_jsonld_test_response(303L, list(Location = landing)),
    "HEAD https://example.net/page/many-scripts" = gx_jsonld_test_response(200L),
    "GET https://example.net/page/many-scripts" = gx_jsonld_test_response(200L, list(`Content-Type` = "text/html"), html)
  ))
  withr::local_options(geoconnexr.jsonld_max_html_candidates = 1L)

  expect_error(gx_jsonld(pid, client = setup$client), class = "gx_error_jsonld_budget")
})
