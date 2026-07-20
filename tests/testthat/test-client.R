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
    min_interval = 0.25,
    max_bytes = 4096,
    cache = TRUE
  )

  expect_s3_class(client, "gx_client")
  expect_equal(client$endpoint, "pid")
  expect_equal(client$timeout, 12)
  expect_equal(client$retries, 2L)
  expect_equal(client$min_interval, 0.25)
  expect_equal(client$max_bytes, 4096)
  expect_true(client$cache)
  expect_false(client$offline)
  expect_equal(normalizePath(client$cache_dir, mustWork = FALSE),
               normalizePath(cache_dir, mustWork = FALSE))

  expect_error(gx_client("unknown"), class = "gx_error_client")
  expect_error(gx_client("pid", timeout = 0), class = "gx_error_client")
  expect_error(gx_client("pid", retries = -1L), class = "gx_error_client")
  expect_error(gx_client("pid", retries = 1001L), class = "gx_error_client")
  expect_error(gx_client("pid", min_interval = -1), class = "gx_error_client")
  expect_error(gx_client("pid", min_interval = Inf), class = "gx_error_client")
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

test_that("physical attempts reserve deterministic per-host throttle slots", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  now <- 0
  sleeps <- numeric()
  starts <- numeric()
  performer <- function(request) {
    starts <<- c(starts, now)
    gx_test_response(url = request$url)
  }
  withr::local_options(list(
    geoconnexr.performer = performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.throttle_clock = function() now,
    geoconnexr.throttle_sleep = function(seconds) {
      sleeps <<- c(sleeps, seconds)
      now <<- now + seconds
    }
  ))
  client <- gx_client(
    "reference", retries = 0L, min_interval = 2,
    cache = FALSE, cache_dir = withr::local_tempdir()
  )
  zero_interval_client <- gx_client(
    "reference", retries = 0L, min_interval = 0,
    cache = FALSE, cache_dir = client$cache_dir
  )

  first <- gx_test_http_request(client, "GET", "https://one.example/a")
  second <- gx_test_http_request(client, "GET", "https://two.example/a")
  third <- gx_test_http_request(
    zero_interval_client, "GET", "https://one.example/b"
  )

  expect_identical(starts, c(0, 0, 2))
  expect_identical(sleeps, 2)
  expect_identical(first$attempts$throttle_delay, 0)
  expect_identical(second$attempts$throttle_delay, 0)
  expect_identical(third$attempts$throttle_delay, 2)
  expect_identical(third$attempts$resolved_host, "one.example")
})

test_that("mixed client intervals use the larger adjacent host gap", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  now <- 0
  sleeps <- numeric()
  starts <- numeric()
  withr::local_options(list(
    geoconnexr.performer = function(request) {
      starts <<- c(starts, now)
      gx_test_response(url = request$url)
    },
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.throttle_clock = function() now,
    geoconnexr.throttle_sleep = function(seconds) {
      sleeps <<- c(sleeps, seconds)
      now <<- now + seconds
    }
  ))
  cache_dir <- withr::local_tempdir()
  zero_interval_client <- gx_client(
    "reference", retries = 0L, min_interval = 0,
    cache = FALSE, cache_dir = cache_dir
  )
  positive_interval_client <- gx_client(
    "reference", retries = 0L, min_interval = 2,
    cache = FALSE, cache_dir = cache_dir
  )

  zero_first <- gx_test_http_request(
    zero_interval_client, "GET", "https://reverse.example/a"
  )
  positive_second <- gx_test_http_request(
    positive_interval_client, "GET", "https://reverse.example/b"
  )
  zero_again <- gx_test_http_request(
    zero_interval_client, "GET", "https://zero.example/a"
  )
  zero_immediate <- gx_test_http_request(
    zero_interval_client, "GET", "https://zero.example/b"
  )

  expect_identical(starts, c(0, 2, 2, 2))
  expect_identical(sleeps, 2)
  expect_identical(zero_first$attempts$throttle_delay, 0)
  expect_identical(positive_second$attempts$throttle_delay, 2)
  expect_identical(zero_again$attempts$throttle_delay, 0)
  expect_identical(zero_immediate$attempts$throttle_delay, 0)
})

test_that("retry delay and host throttle compose without under-waiting", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  now <- 0
  retry_sleeps <- numeric()
  throttle_sleeps <- numeric()
  starts <- numeric()
  calls <- gx_test_scripted_performer(list(
    function(request) {
      starts <<- c(starts, now)
      gx_test_response(status = 503L, url = request$url)
    },
    function(request) {
      starts <<- c(starts, now)
      gx_test_response(url = request$url)
    }
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.retry_jitter = function(max_seconds) 1,
    geoconnexr.retry_sleep = function(seconds) {
      retry_sleeps <<- c(retry_sleeps, seconds)
      now <<- now + seconds
    },
    geoconnexr.throttle_clock = function() now,
    geoconnexr.throttle_sleep = function(seconds) {
      throttle_sleeps <<- c(throttle_sleeps, seconds)
      now <<- now + seconds
    }
  ))
  client <- gx_client(
    "reference", retries = 1L, min_interval = 2,
    cache = FALSE, cache_dir = withr::local_tempdir()
  )

  response <- gx_test_http_request(
    client, "GET", "https://example.org/retry-throttle"
  )

  expect_identical(starts, c(0, 2))
  expect_identical(retry_sleeps, 1)
  expect_identical(throttle_sleeps, 1)
  expect_identical(response$attempts$delay, c(1, NA_real_))
  expect_identical(response$attempts$throttle_delay, c(0, 1))
})

test_that("post-throttle DNS rejection commits one zero-byte attempt", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  now <- 0
  performer_calls <- 0L
  before_physical <- logical()
  after_rows <- geoconnexr:::gx_http_empty_attempts()
  control <- list(
    before = function(request, physical) {
      before_physical <<- c(before_physical, physical)
      request$max_bytes
    },
    after = function(attempt) {
      after_rows <<- rbind(after_rows, attempt)
      invisible(NULL)
    }
  )
  withr::local_options(list(
    geoconnexr.performer = function(request) {
      performer_calls <<- performer_calls + 1L
      gx_test_response(url = request$url)
    },
    geoconnexr.dns_resolver = function(host) "10.0.0.1",
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.throttle_clock = function() now,
    geoconnexr.throttle_sleep = function(seconds) now <<- now + seconds
  ))
  client <- gx_client(
    "reference", retries = 0L, min_interval = 1,
    cache = FALSE, cache_dir = withr::local_tempdir()
  )

  error <- expect_error(
    geoconnexr:::gx_http_request(
      client,
      "GET",
      "https://private.example/rejected",
      .attempt_control = control
    ),
    class = "gx_error_unsafe_url"
  )

  expect_identical(performer_calls, 0L)
  expect_identical(before_physical, TRUE)
  expect_identical(nrow(after_rows), 1L)
  expect_identical(error$attempt_count, 1L)
  expect_identical(error$retry_stopped, "unsafe_target")
  expect_identical(error$attempts, after_rows)
  expect_identical(error$attempts$status, NA_integer_)
  expect_identical(error$attempts$outcome, "policy_error")
  expect_identical(error$attempts$physical, TRUE)
  expect_identical(error$attempts$retryable, FALSE)
  expect_identical(error$attempts$error_code, "unsafe_url")
  expect_identical(error$attempts$bytes, 0)
  expect_identical(error$attempts$charged_bytes, 0)
  expect_identical(error$attempts$resolved_host, "private.example")
  expect_identical(error$attempts$resolved_ip, NA_character_)
})

test_that("injected non-advancing throttle time fails before dispatch", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  now <- 0
  sleeps <- numeric()
  calls <- 0L
  withr::local_options(list(
    geoconnexr.performer = function(request) {
      calls <<- calls + 1L
      gx_test_response(url = request$url)
    },
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.throttle_clock = function() now,
    geoconnexr.throttle_sleep = function(seconds) {
      sleeps <<- c(sleeps, seconds)
    }
  ))
  client <- gx_client(
    "reference", retries = 0L, min_interval = 1,
    cache = FALSE, cache_dir = withr::local_tempdir()
  )

  first <- gx_test_http_request(
    client, "GET", "https://stalled-clock.example/first"
  )
  error <- expect_error(
    gx_test_http_request(
      client, "GET", "https://stalled-clock.example/second"
    ),
    class = "gx_error_client"
  )

  expect_identical(first$attempt_count, 1L)
  expect_identical(sleeps, 1)
  expect_identical(calls, 1L)
  expect_match(conditionMessage(error), "did not advance", fixed = TRUE)
  expect_identical(error$retry_stopped, "throttle_error")
  expect_identical(nrow(error$attempts), 0L)
})

test_that("cache hits bypass host throttling and invalid sleepers fail closed", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  now <- 0
  sleeps <- numeric()
  calls <- 0L
  clock_calls <- 0L
  cache_dir <- withr::local_tempdir()
  withr::local_options(list(
    geoconnexr.performer = function(request) {
      calls <<- calls + 1L
      gx_test_response(url = request$url)
    },
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.throttle_clock = function() {
      clock_calls <<- clock_calls + 1L
      now
    },
    geoconnexr.throttle_sleep = function(seconds) {
      sleeps <<- c(sleeps, seconds)
    }
  ))
  client <- gx_client(
    "reference", retries = 0L, min_interval = 5,
    cache = TRUE, cache_dir = cache_dir
  )

  first <- gx_test_http_request(client, "GET", "https://example.org/cached")
  clock_calls_after_first <- clock_calls
  cached <- gx_test_http_request(client, "GET", "https://example.org/cached")

  expect_identical(first$attempt_count, 1L)
  expect_identical(cached$attempt_count, 0L)
  expect_identical(calls, 1L)
  expect_identical(clock_calls, clock_calls_after_first)
  expect_length(sleeps, 0L)

  error <- expect_error(
    gx_test_http_request(client, "GET", "https://example.org/not-cached"),
    class = "gx_error_client"
  )
  expect_identical(error$retry_stopped, "throttle_error")
  expect_identical(calls, 1L)
  expect_identical(nrow(error$attempts), 0L)

  token <- "TOPSECRET-throttle-clock"
  withr::local_options(
    geoconnexr.throttle_clock = function() stop(token, call. = FALSE)
  )
  clock_error <- expect_error(
    gx_test_http_request(client, "GET", "https://other.example/not-cached"),
    class = "gx_error_client"
  )
  expect_false(grepl(
    token,
    paste(c(conditionMessage(clock_error), format(clock_error)), collapse = "\n"),
    fixed = TRUE
  ))
  expect_identical(calls, 1L)
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

test_that("query URLs and private responses are never persisted", {
  cache_dir <- withr::local_tempdir()
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      headers = list(`Cache-Control` = "no-store"),
      body = charToRaw("no-store"),
      url = request$url
    ),
    function(request) gx_test_response(
      headers = list(`Set-Cookie` = "session=secret"),
      body = charToRaw("cookie"),
      url = request$url
    ),
    function(request) gx_test_response(
      body = charToRaw("query"),
      url = request$url
    )
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))
  client <- gx_client("reference", cache_dir = cache_dir)

  gx_test_http_request(client, "GET", "https://example.org/no-store")
  gx_test_http_request(client, "GET", "https://example.org/cookie")
  gx_test_http_request(client, "GET", "https://example.org/data?code=TOPSECRET")

  expect_equal(calls$state$index, 3L)
  expect_length(list.files(cache_dir, pattern = "[.]rds$"), 0L)
  expect_false(geoconnexr:::gx_response_cache_allowed(list(`Cache-Control` = "private")))
  expect_false(geoconnexr:::gx_response_cache_allowed(list(Pragma = "foo, no-cache")))
  expect_false(geoconnexr:::gx_response_cache_allowed(list(Vary = "Accept, *")))
  expect_false(geoconnexr:::gx_response_cache_allowed(list(
    Location = "https://provider.example/data?token=TOPSECRET"
  )))
})

test_that("redirect credentials are not persisted through response headers", {
  cache_dir <- withr::local_tempdir()
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      status = 303L,
      headers = list(Location = "https://provider.example/data?token=TOPSECRET"),
      url = request$url
    )
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))
  client <- gx_client("reference", cache_dir = cache_dir)

  response <- geoconnexr:::gx_http_request(
    client,
    "HEAD",
    "https://example.org/redirect",
    check_status = FALSE
  )

  expect_equal(response$status, 303L)
  expect_length(list.files(cache_dir, pattern = "[.]rds$"), 0L)
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

  withr::local_options(geoconnexr.performer = function(request) {
    stop("socket failed at https://example.org/data?token=TOPSECRET")
  })
  error <- tryCatch(
    gx_test_http_request(client, "GET", "https://example.org/transport"),
    error = identity
  )
  expect_s3_class(error, "gx_error_transport")
  expect_false(grepl("TOPSECRET", conditionMessage(error), fixed = TRUE))
  expect_false(grepl("TOPSECRET", paste(format(error), collapse = "\n"), fixed = TRUE))
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

test_that("package retries expose every physical attempt and honor Retry-After", {
  cache_dir <- withr::local_tempdir()
  dns_calls <- 0L
  sleeps <- numeric()
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      status = 429L,
      headers = list(
        `Retry-After` = "3",
        `Content-Type` = "text/plain"
      ),
      body = charToRaw("wait"),
      url = request$url
    ),
    function(request) gx_test_response(
      status = 200L,
      headers = list(`Content-Type` = "text/plain"),
      body = charToRaw("ok"),
      url = request$url
    )
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = function(host) {
      dns_calls <<- dns_calls + 1L
      "93.184.216.34"
    },
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.retry_jitter = function(max_seconds) 0,
    geoconnexr.retry_sleep = function(seconds) {
      sleeps <<- c(sleeps, seconds)
    }
  ))
  client <- gx_client(
    "reference", retries = 1L, max_bytes = 32L,
    cache = FALSE, cache_dir = cache_dir
  )

  response <- gx_test_http_request(
    client, "GET", "https://example.org/retry-after"
  )

  expect_identical(rawToChar(response$body), "ok")
  expect_identical(response$attempt_count, 2L)
  expect_identical(response$attempts$status, c(429L, 200L))
  expect_identical(response$attempts$delay, c(3, NA_real_))
  expect_identical(response$attempts$bytes, c(4, 2))
  expect_identical(sleeps, 3)
  expect_identical(dns_calls, 2L)
  expect_identical(calls$state$index, 2L)
  expect_true(all(vapply(calls$state$calls, `[[`, integer(1), "retries") == 0L))
  expect_length(unique(vapply(
    calls$state$calls, `[[`, character(1), "request_id"
  )), 1L)
})

test_that("Retry-After HTTP dates use the response Date reference", {
  cache_dir <- withr::local_tempdir()
  sleeps <- numeric()
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      status = 503L,
      headers = list(
        Date = "Mon, 13 Jul 2026 20:00:00 GMT",
        `Retry-After` = "Mon, 13 Jul 2026 20:00:07 GMT"
      ),
      body = raw(),
      url = request$url
    ),
    function(request) gx_test_response(url = request$url)
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = function() {
      as.POSIXct("2026-07-13 19:00:00", tz = "UTC")
    },
    geoconnexr.retry_jitter = function(max_seconds) 0,
    geoconnexr.retry_sleep = function(seconds) {
      sleeps <<- c(sleeps, seconds)
    }
  ))
  client <- gx_client(
    "reference", retries = 1L, cache = FALSE, cache_dir = cache_dir
  )

  response <- gx_test_http_request(client, "GET", "https://example.org/date")

  expect_identical(sleeps, 7)
  expect_identical(response$attempts$retry_after, c(7, NA_real_))
})

test_that("retry jitter uses bounded exponential maxima", {
  maxima <- numeric()
  sleeps <- numeric()
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(status = 503L, url = request$url),
    function(request) gx_test_response(status = 503L, url = request$url),
    function(request) gx_test_response(url = request$url)
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.retry_jitter = function(max_seconds) {
      maxima <<- c(maxima, max_seconds)
      max_seconds / 2
    },
    geoconnexr.retry_sleep = function(seconds) sleeps <<- c(sleeps, seconds)
  ))
  client <- gx_client(
    "reference", retries = 2L, cache = FALSE,
    cache_dir = withr::local_tempdir()
  )

  response <- gx_test_http_request(client, "GET", "https://example.org/jitter")

  expect_identical(maxima, c(1, 2))
  expect_identical(sleeps, c(0.5, 1))
  expect_identical(response$attempts$delay, c(0.5, 1, NA_real_))
})

test_that("retry status policy is explicit and exhaustion preserves metadata", {
  for (status in c(500L, 502L, 503L, 504L)) {
    calls <- gx_test_scripted_performer(list(
      function(request) gx_test_response(status = status, url = request$url),
      function(request) gx_test_response(url = request$url)
    ))
    withr::local_options(list(
      geoconnexr.performer = calls$performer,
      geoconnexr.dns_resolver = gx_test_public_dns,
      geoconnexr.clock = gx_test_fixed_clock,
      geoconnexr.retry_jitter = function(max_seconds) 0
    ))
    client <- gx_client(
      "reference", retries = 1L, cache = FALSE,
      cache_dir = withr::local_tempdir()
    )
    response <- gx_test_http_request(
      client, "GET", paste0("https://example.org/status-", status)
    )
    expect_identical(response$attempts$status, c(status, 200L))
    expect_identical(calls$state$index, 2L)
  }

  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(status = 501L, url = request$url)
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.retry_jitter = function(max_seconds) 0
  ))
  client <- gx_client(
    "reference", retries = 3L, cache = FALSE,
    cache_dir = withr::local_tempdir()
  )
  terminal <- geoconnexr:::gx_http_request(
    client, "GET", "https://example.org/not-transient", check_status = FALSE
  )
  expect_identical(terminal$status, 501L)
  expect_identical(terminal$attempt_count, 1L)
  expect_identical(calls$state$index, 1L)

  exhausted <- gx_test_scripted_performer(list(
    function(request) gx_test_response(status = 503L, url = request$url),
    function(request) gx_test_response(status = 503L, url = request$url)
  ))
  withr::local_options(geoconnexr.performer = exhausted$performer)
  client$retries <- 1L
  error <- expect_error(
    gx_test_http_request(client, "GET", "https://example.org/exhausted"),
    class = "gx_error_http"
  )
  expect_identical(error$status, 503L)
  expect_true(error$retry_exhausted)
  expect_identical(error$attempts$status, c(503L, 503L))
})

test_that("transport retries redact failures and conservatively charge unknown bytes", {
  token <- "TOPSECRET-transport-token"
  calls <- gx_test_scripted_performer(list(
    function(request) stop(paste("socket failed", token), call. = FALSE),
    function(request) gx_test_response(body = charToRaw("ok"), url = request$url)
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.retry_jitter = function(max_seconds) 0
  ))
  client <- gx_client(
    "reference", retries = 1L, max_bytes = 16L, cache = FALSE,
    cache_dir = withr::local_tempdir()
  )
  response <- gx_test_http_request(
    client, "GET", "https://example.org/transport?sig=hidden"
  )
  expect_identical(response$attempts$status, c(NA_integer_, 200L))
  expect_identical(response$attempts$charged_bytes, c(16, 2))
  expect_identical(response$attempts$url[[1]], "https://example.org/transport?[redacted]")

  failures <- gx_test_scripted_performer(list(
    function(request) stop(paste("first", token), call. = FALSE),
    function(request) stop(paste("second", token), call. = FALSE)
  ))
  withr::local_options(geoconnexr.performer = failures$performer)
  error <- expect_error(
    gx_test_http_request(
      client, "GET", "https://example.org/fail?sig=hidden"
    ),
    class = "gx_error_transport"
  )
  expect_identical(nrow(error$attempts), 2L)
  expect_true(error$retry_exhausted)
  rendered <- paste(c(conditionMessage(error), format(error)), collapse = "\n")
  expect_false(grepl(token, rendered, fixed = TRUE))
  expect_false(grepl("sig=hidden", rendered, fixed = TRUE))

  classed <- gx_test_scripted_performer(list(
    function(request) {
      geoconnexr:::gx_abort("Classed transport failure.", "gx_error_transport")
    },
    function(request) gx_test_response(body = charToRaw("ok"), url = request$url)
  ))
  withr::local_options(geoconnexr.performer = classed$performer)
  recovered <- gx_test_http_request(
    client, "GET", "https://example.org/classed-transport"
  )
  expect_identical(recovered$attempts$outcome, c("transport_error", "response"))
  expect_identical(classed$state$index, 2L)
})

test_that("user interrupts propagate without retry or wrapping", {
  calls <- 0L
  withr::local_options(list(
    geoconnexr.performer = function(request) {
      calls <<- calls + 1L
      rlang::interrupt()
    },
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))
  client <- gx_client(
    "reference", retries = 3L, cache = FALSE,
    cache_dir = withr::local_tempdir()
  )

  condition <- tryCatch(
    gx_test_http_request(client, "GET", "https://example.org/interrupted"),
    interrupt = identity
  )

  expect_s3_class(condition, "interrupt")
  expect_identical(calls, 1L)
})

test_that("DNS is revalidated before every retry and rebinding fails closed", {
  dns_calls <- 0L
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(status = 503L, url = request$url),
    function(request) gx_test_response(url = request$url)
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = function(host) {
      dns_calls <<- dns_calls + 1L
      if (dns_calls == 1L) "93.184.216.34" else "10.0.0.1"
    },
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.retry_jitter = function(max_seconds) 0
  ))
  client <- gx_client(
    "reference", retries = 1L, cache = FALSE,
    cache_dir = withr::local_tempdir()
  )
  error <- expect_error(
    gx_test_http_request(client, "GET", "https://example.org/rebind"),
    class = "gx_error_unsafe_url"
  )
  expect_identical(dns_calls, 2L)
  expect_identical(calls$state$index, 1L)
  expect_identical(error$attempt_count, 2L)
  expect_identical(error$attempts$attempt, 1:2)
  expect_identical(error$attempts$status, c(503L, NA_integer_))
  expect_identical(error$attempts$outcome, c("response", "policy_error"))
  expect_identical(error$attempts$physical, c(TRUE, TRUE))
  expect_identical(error$attempts$retryable, c(TRUE, FALSE))
  expect_identical(error$attempts$error_code, c(NA_character_, "unsafe_url"))
  expect_identical(error$attempts$bytes, c(0, 0))
  expect_identical(error$attempts$charged_bytes, c(0, 0))
  expect_identical(error$attempts$resolved_host, rep("example.org", 2L))
  expect_identical(
    error$attempts$resolved_ip,
    c("93.184.216.34", NA_character_)
  )
  expect_identical(error$retry_stopped, "unsafe_target")
})

test_that("only a terminal eligible response is cached after retries", {
  cache_dir <- withr::local_tempdir()
  dns_calls <- 0L
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      status = 503L, body = charToRaw("temporary"), url = request$url
    ),
    function(request) gx_test_response(
      status = 200L, body = charToRaw("final"), url = request$url
    )
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = function(host) {
      dns_calls <<- dns_calls + 1L
      gx_test_public_dns(host)
    },
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.retry_jitter = function(max_seconds) 0
  ))
  client <- gx_client(
    "reference", retries = 1L, cache = TRUE, cache_dir = cache_dir
  )
  first <- gx_test_http_request(client, "GET", "https://example.org/cache-retry")
  second <- gx_test_http_request(client, "GET", "https://example.org/cache-retry")

  expect_identical(rawToChar(first$body), "final")
  expect_identical(rawToChar(second$body), "final")
  expect_identical(first$attempt_count, 2L)
  expect_identical(second$attempt_count, 0L)
  expect_identical(nrow(second$attempts), 0L)
  expect_identical(calls$state$index, 2L)
  expect_identical(dns_calls, 2L)
  expect_identical(gx_cache_info(cache_dir)$entries, 1L)
})

test_that("retry policy callbacks and server delay ceilings fail safely", {
  calls <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      status = 503L,
      headers = list(`Retry-After` = "61"),
      url = request$url
    )
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock,
    geoconnexr.retry_jitter = function(max_seconds) 0,
    geoconnexr.retry_max_delay = 60
  ))
  client <- gx_client(
    "reference", retries = 2L, cache = FALSE,
    cache_dir = withr::local_tempdir()
  )
  response <- geoconnexr:::gx_http_request(
    client, "GET", "https://example.org/too-late", check_status = FALSE
  )
  expect_identical(response$status, 503L)
  expect_identical(response$attempt_count, 1L)
  expect_identical(response$retry_stopped, "retry_after_exceeds_limit")

  overflow <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      status = 503L,
      headers = list(`Retry-After` = strrep("9", 400L)),
      url = request$url
    )
  ))
  withr::local_options(geoconnexr.performer = overflow$performer)
  overflow_response <- geoconnexr:::gx_http_request(
    client, "GET", "https://example.org/overflow-delay", check_status = FALSE
  )
  expect_identical(overflow_response$attempt_count, 1L)
  expect_identical(overflow_response$retry_stopped, "retry_after_exceeds_limit")
  expect_identical(overflow_response$attempts$retry_after, Inf)

  duplicate <- gx_test_scripted_performer(list(
    function(request) gx_test_response(
      status = 503L,
      headers = structure(
        list(
          "Mon, 13 Jul 2026 20:00:10 GMT",
          "Mon, 13 Jul 2026 20:01:00 GMT"
        ),
        names = c("Retry-After", "Retry-After")
      ),
      url = request$url
    )
  ))
  withr::local_options(geoconnexr.performer = duplicate$performer)
  duplicate_response <- geoconnexr:::gx_http_request(
    client, "GET", "https://example.org/duplicate-delay", check_status = FALSE
  )
  expect_identical(duplicate_response$attempt_count, 1L)
  expect_identical(duplicate_response$retry_stopped, "retry_after_exceeds_limit")
  expect_identical(duplicate_response$attempts$retry_after, Inf)

  invalid <- gx_test_scripted_performer(list(
    function(request) gx_test_response(status = 503L, url = request$url)
  ))
  withr::local_options(list(
    geoconnexr.performer = invalid$performer,
    geoconnexr.retry_jitter = function(max_seconds) max_seconds + 1
  ))
  error <- expect_error(
    gx_test_http_request(client, "GET", "https://example.org/bad-jitter"),
    class = "gx_error_client"
  )
  expect_identical(error$attempts$status, 503L)
  expect_identical(error$retry_stopped, "retry_policy_error")
})

test_that("attempt controls shrink the hard byte ceiling before transport", {
  observed_max <- numeric()
  recorded <- geoconnexr:::gx_http_empty_attempts()
  calls <- gx_test_scripted_performer(list(
    function(request) {
      observed_max <<- c(observed_max, request$max_bytes)
      gx_test_response(body = charToRaw("abc"), url = request$url)
    }
  ))
  withr::local_options(list(
    geoconnexr.performer = calls$performer,
    geoconnexr.dns_resolver = gx_test_public_dns,
    geoconnexr.clock = gx_test_fixed_clock
  ))
  client <- gx_client(
    "reference", retries = 3L, max_bytes = 10L, cache = FALSE,
    cache_dir = withr::local_tempdir()
  )
  control <- list(
    before = function(request, physical) 2L,
    after = function(attempt) recorded <<- rbind(recorded, attempt)
  )

  error <- expect_error(
    geoconnexr:::gx_http_request(
      client,
      "GET",
      "https://example.org/two-byte-budget",
      .attempt_control = control
    ),
    class = "gx_error_payload_too_large"
  )

  expect_identical(observed_max, 2)
  expect_identical(calls$state$index, 1L)
  expect_identical(error$attempts$charged_bytes, 3)
  expect_identical(recorded$charged_bytes, 3)
})
