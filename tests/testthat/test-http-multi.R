gx_test_multi_request <- function(host, key, max_bytes = 100L) {
  list(
    method = "GET",
    url = paste0("https://", host, "/data"),
    headers = c("accept-encoding" = "identity"),
    body = raw(),
    timeout = 10,
    retries = 0L,
    max_bytes = as.integer(max_bytes),
    user_agent = "geoconnexr/test",
    request_id = key,
    resolved_host = host,
    resolved_port = "443",
    resolved_ip = character(),
    throttle_delay = 0
  )
}

gx_test_multi_adapter <- function(bodies) {
  state <- new.env(parent = emptyenv())
  state$pool_options <- NULL
  state$starts <- numeric()
  state$pending <- list()
  state$runs <- numeric()
  adapter <- list(
    new_pool = function(total_con, host_con, max_streams, multiplex) {
      state$pool_options <- list(
        total_con = total_con, host_con = host_con,
        max_streams = max_streams, multiplex = multiplex
      )
      pool <- new.env(parent = emptyenv())
      class(pool) <- "gx_test_multi_pool"
      pool
    },
    new_handle = function(...) {
      handle <- new.env(parent = emptyenv())
      class(handle) <- "gx_test_multi_handle"
      handle$options <- list()
      handle$finished <- FALSE
      handle$canceled <- FALSE
      handle
    },
    handle_setopt = function(handle, ..., .list = list()) {
      handle$options <- utils::modifyList(
        handle$options, c(list(...), .list)
      )
      invisible(handle)
    },
    multi_add = function(handle, done = NULL, fail = NULL, data = NULL,
                         pool = NULL) {
      state$starts <- c(
        state$starts,
        getOption("geoconnexr.test_multi_now", function() 0)()
      )
      state$pending[[length(state$pending) + 1L]] <- list(
        handle = handle, done = done, fail = fail, data = data
      )
      invisible(handle)
    },
    multi_run = function(timeout = Inf, poll = FALSE, pool = NULL) {
      state$runs <- c(state$runs, timeout)
      if (!is.infinite(timeout)) {
        return(list(success = 0L, error = 0L, pending = length(state$pending)))
      }
      order <- rev(seq_along(state$pending))
      for (i in order) {
        item <- state$pending[[i]]
        if (item$handle$finished || item$handle$canceled) next
        url <- item$handle$options$url
        body <- bodies[[url]]
        item$data(body, final = FALSE)
        within <- item$handle$options$progressfunction(
          c(length(body), length(body)), c(0, 0)
        )
        item$data(raw(), final = TRUE)
        item$handle$finished <- TRUE
        if (isTRUE(within)) {
          item$done(list(
            status_code = 200L,
            headers = list(`content-type` = "application/json"),
            url = url
          ))
        } else {
          item$fail("aborted by bounded progress callback")
        }
      }
      list(success = length(order), error = 0L, pending = 0L)
    },
    multi_list = function(pool = NULL) {
      active <- vapply(state$pending, function(item) {
        !item$handle$finished && !item$handle$canceled
      }, logical(1))
      lapply(state$pending[active], `[[`, "handle")
    },
    multi_cancel = function(handle) {
      handle$canceled <- TRUE
      invisible(NULL)
    },
    parse_headers = function(headers) headers
  )
  list(adapter = adapter, state = state)
}

test_that("curl multi performer starts bounded requests and returns completion order", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  now <- 0
  first_url <- "https://one.example/data"
  second_url <- "https://two.example/data"
  fake <- gx_test_multi_adapter(stats::setNames(
    list(charToRaw("one"), charToRaw("two")),
    c(first_url, second_url)
  ))
  withr::local_options(list(
    geoconnexr.multi_adapter = fake$adapter,
    geoconnexr.dns_resolver = function(host) "93.184.216.34",
    geoconnexr.throttle_clock = function() now,
    geoconnexr.throttle_sleep = function(seconds) now <<- now + seconds,
    geoconnexr.test_multi_now = function() now
  ))
  attempts <- list(
    list(
      token = 1L,
      request = gx_test_multi_request("one.example", strrep("a", 64)),
      min_interval = 0
    ),
    list(
      token = 2L,
      request = gx_test_multi_request("two.example", strrep("b", 64)),
      min_interval = 0
    )
  )

  events <- geoconnexr:::gx_default_multi_performer(
    attempts, max_active = 2L, max_per_host = 1L
  )
  expect_identical(vapply(events, `[[`, integer(1), "token"), c(2L, 1L))
  expect_identical(
    vapply(events, function(event) rawToChar(event$response$body), character(1)),
    c("two", "one")
  )
  expect_true(all(vapply(events, function(event) is.null(event$error), logical(1))))
  expect_identical(fake$state$starts, c(0, 0))
  expect_identical(
    fake$state$pool_options,
    list(total_con = 2L, host_con = 1L, max_streams = 1L, multiplex = FALSE)
  )
  expect_identical(fake$state$runs, c(0, 0, Inf))
  resolutions <- vapply(fake$state$pending, function(item) {
    item$handle$options$resolve
  }, character(1))
  expect_identical(
    resolutions,
    c("one.example:443:93.184.216.34", "two.example:443:93.184.216.34")
  )
})

test_that("curl multi performer enforces streaming ceilings", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  url <- "https://large.example/data"
  fake <- gx_test_multi_adapter(stats::setNames(
    list(charToRaw("too large")), url
  ))
  withr::local_options(list(
    geoconnexr.multi_adapter = fake$adapter,
    geoconnexr.dns_resolver = function(host) "93.184.216.34"
  ))
  attempts <- list(list(
    token = 1L,
    request = gx_test_multi_request(
      "large.example", strrep("c", 64), max_bytes = 3L
    ),
    min_interval = 0
  ))

  events <- geoconnexr:::gx_default_multi_performer(
    attempts, max_active = 1L, max_per_host = 1L
  )
  expect_length(events, 1L)
  expect_null(events[[1L]]$response)
  expect_s3_class(events[[1L]]$error, "gx_error_payload_too_large")
  expect_true(is.na(events[[1L]]$error$gx_bytes))
})

test_that("curl multi performer isolates a rejected DNS target", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  good_url <- "https://good.example/data"
  fake <- gx_test_multi_adapter(stats::setNames(
    list(charToRaw("good")), good_url
  ))
  withr::local_options(list(
    geoconnexr.multi_adapter = fake$adapter,
    geoconnexr.dns_resolver = function(host) {
      if (identical(host, "bad.example")) "127.0.0.1" else "93.184.216.34"
    }
  ))
  attempts <- list(
    list(
      token = 1L,
      request = gx_test_multi_request("bad.example", strrep("e", 64)),
      min_interval = 0
    ),
    list(
      token = 2L,
      request = gx_test_multi_request("good.example", strrep("f", 64)),
      min_interval = 0
    )
  )

  events <- geoconnexr:::gx_default_multi_performer(
    attempts, max_active = 2L, max_per_host = 1L
  )
  expect_identical(vapply(events, `[[`, integer(1), "token"), c(1L, 2L))
  expect_s3_class(events[[1L]]$error, "gx_error_unsafe_url")
  expect_identical(rawToChar(events[[2L]]$response$body), "good")
  expect_length(fake$state$pending, 1L)
})

test_that("curl multi performer rejects ambiguous attempt contracts", {
  request <- gx_test_multi_request("one.example", strrep("d", 64))
  request$retries <- 1L
  expect_error(
    geoconnexr:::gx_default_multi_performer(list(list(
      token = 1L, request = request, min_interval = 0
    ))),
    class = "gx_error_http_multi_contract"
  )

  request$retries <- 0L
  request$headers <- c("accept-encoding" = "gzip")
  expect_error(
    geoconnexr:::gx_default_multi_performer(list(list(
      token = 1L, request = request, min_interval = 0
    ))),
    class = "gx_error_http_multi_contract"
  )
})

test_that("multi execution coalesces and reuses compatible cache entries", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  cache_dir <- withr::local_tempdir()
  url <- "https://cache.example/data"
  fake <- gx_test_multi_adapter(stats::setNames(
    list(charToRaw("cached")), url
  ))
  withr::local_options(list(
    geoconnexr.multi_adapter = fake$adapter,
    geoconnexr.dns_resolver = function(host) "93.184.216.34",
    geoconnexr.clock = function() {
      as.POSIXct("2026-08-15 13:00:00", tz = "UTC")
    }
  ))
  client <- gx_client(
    "reference", retries = 0L, min_interval = 0, max_bytes = 100L,
    cache = TRUE, cache_dir = cache_dir
  )
  plans <- list(
    geoconnexr:::gx_http_multi_plan(client, url = url),
    geoconnexr:::gx_http_multi_plan(client, url = url)
  )

  results <- geoconnexr:::gx_http_multi_execute(
    plans, max_active = 2L, max_per_host = 2L,
    max_requests = 1L, max_bytes = 100
  )
  expect_s3_class(results, "gx_http_multi_results")
  expect_identical(
    vapply(results, `[[`, character(1), "origin"),
    c("network", "single_flight")
  )
  expect_identical(
    vapply(results, function(result) {
      rawToChar(result$response$body)
    }, character(1)),
    c("cached", "cached")
  )
  expect_length(fake$state$pending, 1L)
  expect_identical(results[[1L]]$response$attempt_count, 1L)

  offline <- gx_client(
    "reference", retries = 0L, min_interval = 0, max_bytes = 100L,
    cache = TRUE, offline = TRUE, cache_dir = cache_dir
  )
  cached <- geoconnexr:::gx_http_multi_execute(list(
    geoconnexr:::gx_http_multi_plan(offline, url = url)
  ))
  expect_identical(cached[[1L]]$origin, "offline_cache")
  expect_true(cached[[1L]]$response$from_cache)
  expect_identical(cached[[1L]]$response$attempt_count, 0L)
  expect_length(fake$state$pending, 1L)
})

test_that("multi execution keeps aggregate budget deferral explicit", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  urls <- c("https://one.example/data", "https://two.example/data")
  fake <- gx_test_multi_adapter(stats::setNames(
    list(charToRaw("one"), charToRaw("two")), urls
  ))
  withr::local_options(list(
    geoconnexr.multi_adapter = fake$adapter,
    geoconnexr.dns_resolver = function(host) "93.184.216.34"
  ))
  client <- gx_client(
    "reference", retries = 0L, min_interval = 0, max_bytes = 100L,
    cache = FALSE
  )
  plans <- lapply(urls, function(url) {
    geoconnexr:::gx_http_multi_plan(client, url = url)
  })

  results <- geoconnexr:::gx_http_multi_execute(
    plans, max_active = 2L, max_per_host = 1L,
    max_requests = 1L, max_bytes = 200
  )
  expect_identical(results[[1L]]$origin, "network")
  expect_identical(results[[2L]]$origin, "deferred_request_budget")
  expect_s3_class(
    results[[2L]]$error, "gx_error_http_multi_request_budget"
  )
  expect_length(fake$state$pending, 1L)

  byte_blocked <- geoconnexr:::gx_http_multi_execute(
    plans[1L], max_active = 1L, max_per_host = 1L,
    max_requests = 1L, max_bytes = 99
  )
  expect_identical(
    byte_blocked[[1L]]$origin, "deferred_byte_budget"
  )
  expect_s3_class(
    byte_blocked[[1L]]$error, "gx_error_http_multi_byte_budget"
  )
})
