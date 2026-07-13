gx_test_public_dns <- function(host) {
  rep("93.184.216.34", length(host))
}

gx_test_fixed_clock <- function() {
  as.POSIXct("2026-07-12 16:00:00", tz = "UTC")
}

gx_test_response <- function(status = 200L, headers = list(), body = raw(), url) {
  list(
    status = as.integer(status),
    headers = headers,
    body = body,
    url = url
  )
}

gx_test_scripted_performer <- function(responses) {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  state$index <- 0L

  performer <- function(request) {
    state$index <- state$index + 1L
    state$calls[[state$index]] <- request
    if (state$index > length(responses)) {
      stop("Unexpected network request in deterministic test.", call. = FALSE)
    }
    response <- responses[[state$index]]
    if (is.function(response)) {
      response(request)
    } else {
      response
    }
  }

  list(performer = performer, state = state)
}

gx_test_http_request <- function(client, method, url, accept = NULL, body = NULL) {
  fn <- geoconnexr:::gx_http_request
  fml <- names(formals(fn))
  args <- list(client = client, method = method, url = url)

  if (!is.null(accept)) {
    if ("headers" %in% fml) {
      args$headers <- list(Accept = accept)
    } else if ("accept" %in% fml) {
      args$accept <- accept
    }
  }
  if (!is.null(body) && "body" %in% fml) {
    args$body <- body
  }

  do.call(fn, args)
}

test_that("gx_client captures bounded deterministic policy", {
  cache_dir <- withr::local_tempdir()
  withr::local_options(list(
    geoconnexr.offline = FALSE,
    geoconnexr.cache_dir = cache_dir,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))

  client <- gx_client(
    "pid",
    timeout = 12,
    retries = 2L,
    max_bytes = 4096,
    cache = TRUE
  )

  expect_s3_class(client, "gx_client")
  expect_equal(client$endpoint, "pid")
  expect_equal(client$timeout, 12)
  expect_equal(client$retries, 2L)
  expect_equal(client$max_bytes, 4096)
  expect_true(client$cache)
  expect_false(client$offline)
  expect_equal(normalizePath(client$cache_dir, mustWork = FALSE),
               normalizePath(cache_dir, mustWork = FALSE))

  expect_error(gx_client("unknown"), class = "gx_error_client")
  expect_error(gx_client("pid", timeout = 0), class = "gx_error_client")
  expect_error(gx_client("pid", retries = -1L), class = "gx_error_client")
  expect_error(gx_client("pid", max_bytes = Inf), class = "gx_error_client")
  expect_error(gx_client("pid", retries = 2^31), class = "gx_error_client")
  expect_error(gx_client("pid", max_bytes = 2^31), class = "gx_error_client")
  expect_error(
    geoconnexr:::gx_http_request(
      client,
      "GET",
      "https://example.org/data",
      headers = list(Accept = "application/json", accept = "text/plain")
    ),
    class = "gx_error_client"
  )
})

test_that("target safety is distinct from URI syntax", {
  withr::local_options(list(
    geoconnexr.dns_resolver = function(host) {
      ifelse(host == "private.example", "10.1.2.3", "93.184.216.34")
    }
  ))

  expect_no_error(geoconnexr:::gx_assert_safe_url("https://example.org/data"))
  expect_error(
    geoconnexr:::gx_assert_safe_url("http://127.0.0.1/data"),
    class = "gx_error_unsafe_url"
  )
  expect_error(
    geoconnexr:::gx_assert_safe_url("http://169.254.169.254/latest/meta-data"),
    class = "gx_error_unsafe_url"
  )
  expect_error(
    geoconnexr:::gx_assert_safe_url("http://[::1]/data"),
    class = "gx_error_unsafe_url"
  )
  expect_error(
    geoconnexr:::gx_assert_safe_url("http://[fd00::1]/data"),
    class = "gx_error_unsafe_url"
  )
  expect_error(
    geoconnexr:::gx_assert_safe_url("http://[fe80::1]/data"),
    class = "gx_error_unsafe_url"
  )
  expect_error(
    geoconnexr:::gx_assert_safe_url("https://private.example/data"),
    class = "gx_error_unsafe_url"
  )
  expect_error(
    geoconnexr:::gx_assert_safe_url("https://user:secret@example.org/data"),
    class = "gx_error_unsafe_url"
  )
  expect_error(
    geoconnexr:::gx_assert_safe_url("https://[2606:4700:4700::1111]/data"),
    class = "gx_error_unsafe_url"
  )

  withr::local_options(geoconnexr.dns_resolver = function(host) NULL)
  expect_error(
    geoconnexr:::gx_assert_safe_url("https://unresolved.example/data"),
    class = "gx_error_unsafe_url"
  )
})

test_that("cache keys vary by representation and support offline reads", {
  cache_dir <- withr::local_tempdir()
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw('{"value":1}'),
      url = request$url
    ),
    function(request) gx_test_response(
      headers = list(`Content-Type` = "text/plain"),
      body = charToRaw("value=1"),
      url = request$url
    )
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))

  url <- "https://example.org/representation"
  online <- gx_client("reference", cache = TRUE, cache_dir = cache_dir)
  json <- gx_test_http_request(online, "GET", url, "application/json")
  json_again <- gx_test_http_request(online, "GET", url, "application/json")
  text <- gx_test_http_request(online, "GET", url, "text/plain")

  expect_equal(calls$state$index, 2L)
  expect_identical(json$body, json_again$body)
  expect_false(identical(json$body, text$body))
  expect_no_error(gx_cache_info(cache_dir = cache_dir))

  withr::local_options(geoconnexr.performer = function(request) {
    stop("Offline cache hit attempted the network.", call. = FALSE)
  })
  offline <- gx_client(
    "reference",
    cache = TRUE,
    offline = TRUE,
    cache_dir = cache_dir
  )
  cached <- gx_test_http_request(offline, "GET", url, "application/json")
  expect_identical(cached$body, json$body)
  expect_error(
    gx_test_http_request(
      offline,
      "GET",
      "https://example.org/not-cached",
      "application/json"
    ),
    class = "gx_error_offline_miss"
  )

  expect_no_error(gx_cache_clear(confirm = FALSE, cache_dir = cache_dir))
  expect_error(
    gx_test_http_request(offline, "GET", url, "application/json"),
    class = "gx_error_offline_miss"
  )
})

test_that("corrupt cache entries are discarded and replaced", {
  cache_dir <- withr::local_tempdir()
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      body = charToRaw("first"),
      url = request$url
    ),
    function(request) gx_test_response(
      body = charToRaw("replacement"),
      url = request$url
    )
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))
  client <- gx_client("reference", cache_dir = cache_dir)
  url <- "https://example.org/corrupt"

  first <- gx_test_http_request(client, "GET", url, "text/plain")
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  expect_length(cache_files, 1L)
  writeBin(charToRaw("not an RDS value"), cache_files[[1]])

  replacement <- gx_test_http_request(client, "GET", url, "text/plain")
  expect_identical(rawToChar(first$body), "first")
  expect_identical(rawToChar(replacement$body), "replacement")
  expect_equal(calls$state$index, 2L)
})

test_that("cache entries honor fixed freshness and current byte policy", {
  cache_dir <- withr::local_tempdir()
  now <- as.POSIXct("2026-07-12 16:00:00", tz = "UTC")
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(body = charToRaw("0123456789"), url = request$url),
    function(request) gx_test_response(body = charToRaw("replacement"), url = request$url)
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = function() now,
    geoconnexr.cache_max_age = 60
  ))
  url <- "https://example.org/freshness"
  online <- gx_client("reference", max_bytes = 20L, cache_dir = cache_dir)
  gx_test_http_request(online, "GET", url)

  now <- now + 61
  refreshed <- gx_test_http_request(online, "GET", url)
  expect_identical(rawToChar(refreshed$body), "replacement")
  expect_equal(calls$state$index, 2L)

  too_small <- gx_client(
    "reference",
    max_bytes = 1L,
    offline = TRUE,
    cache_dir = cache_dir
  )
  expect_error(
    gx_test_http_request(too_small, "GET", url),
    class = "gx_error_offline_miss"
  )
})

test_that("credentialed requests do not share or persist cache entries", {
  cache_dir <- withr::local_tempdir()
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(body = charToRaw("one"), url = request$url),
    function(request) gx_test_response(body = charToRaw("two"), url = request$url)
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))
  client <- gx_client("reference", cache_dir = cache_dir)
  request <- function() geoconnexr:::gx_http_request(
    client,
    "GET",
    "https://example.org/private",
    headers = list(Authorization = "Bearer secret")
  )

  expect_identical(rawToChar(request()$body), "one")
  expect_identical(rawToChar(request()$body), "two")
  expect_equal(calls$state$index, 2L)
  expect_equal(gx_cache_info(cache_dir)$entries, 0L)
})

test_that("cache clearing requires a package ownership marker", {
  cache_dir <- withr::local_tempdir()
  unrelated <- file.path(cache_dir, "important.rds")
  saveRDS(list(important = TRUE), unrelated)

  expect_error(
    gx_cache_clear(confirm = FALSE, cache_dir = cache_dir),
    class = "gx_error_cache_ownership"
  )
  expect_true(file.exists(unrelated))
  expect_error(
    gx_cache_clear(confirm = FALSE, cache_dir = NA_character_),
    class = "gx_error_client"
  )
})

test_that("response byte ceilings accept the boundary and reject overflow", {
  cache_dir <- withr::local_tempdir()
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(body = as.raw(1:4), url = request$url),
    function(request) gx_test_response(body = as.raw(1:5), url = request$url)
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))
  client <- gx_client("reference", max_bytes = 4L, cache = FALSE, cache_dir = cache_dir)

  boundary <- gx_test_http_request(
    client,
    "GET",
    "https://example.org/exactly-four"
  )
  expect_length(boundary$body, 4L)
  expect_error(
    gx_test_http_request(client, "GET", "https://example.org/five"),
    class = "gx_error_payload_too_large"
  )
})

test_that("compressed responses and transport failures use stable errors", {
  cache_dir <- withr::local_tempdir()
  compressed <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      headers = list(`Content-Encoding` = "gzip"),
      body = as.raw(c(31, 139)),
      url = request$url
    )
  ))
  withr::local_options(list(
    geoconnexr.performer = compressed$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))
  client <- gx_client("reference", cache = FALSE, cache_dir = cache_dir)
  expect_error(
    gx_test_http_request(client, "GET", "https://example.org/compressed"),
    class = "gx_error_content_encoding"
  )

  withr::local_options(geoconnexr.performer = function(request) stop("socket failed"))
  expect_error(
    gx_test_http_request(client, "GET", "https://example.org/transport"),
    class = "gx_error_transport"
  )
})

test_that("valid repeated response headers are preserved without rejection", {
  cache_dir <- withr::local_tempdir()
  repeated <- structure(
    list("a=1", "b=2", "application/json"),
    names = c("Set-Cookie", "set-cookie", "Content-Type")
  )
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      headers = repeated,
      body = charToRaw("{}"),
      url = request$url
    )
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))
  client <- gx_client("reference", cache = FALSE, cache_dir = cache_dir)

  response <- gx_test_http_request(client, "GET", "https://example.org/headers")

  expect_equal(
    geoconnexr:::gx_header(response$headers, "set-cookie"),
    "a=1, b=2"
  )
  expect_equal(response$status, 200L)
})

test_that("graph requests reject redirects and HTML responses", {
  cache_dir <- withr::local_tempdir()
  redirect <- gx_test_scripted_performer(list(
    gx_test_response(
      303L,
      headers = list(Location = "https://example.org/elsewhere"),
      url = "https://graph.geoconnex.us/"
    )
  ))
  withr::local_options(list(
    geoconnexr.performer = redirect$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))
  client <- gx_client("graph", cache = FALSE, cache_dir = cache_dir)
  expect_error(
    gx_test_http_request(
      client,
      "POST",
      "https://graph.geoconnex.us/",
      "application/sparql-results+json",
      charToRaw("SELECT * WHERE { ?s ?p ?o } LIMIT 1")
    ),
    class = "gx_error_redirect"
  )

  html <- gx_test_scripted_performer(list(
    gx_test_response(
      200L,
      headers = list(`Content-Type` = "text/html; charset=utf-8"),
      body = charToRaw("<html><body>proxy error</body></html>"),
      url = "https://graph.geoconnex.us/"
    )
  ))
  withr::local_options(geoconnexr.performer = html$performer)
  expect_error(
    gx_test_http_request(
      client,
      "POST",
      "https://graph.geoconnex.us/",
      "application/sparql-results+json",
      charToRaw("SELECT * WHERE { ?s ?p ?o } LIMIT 1")
    ),
    class = "gx_error_content_type"
  )
})
