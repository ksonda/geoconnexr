gx_pid_test_fixed_clock <- function() {
  as.POSIXct("2026-07-12 16:00:00", tz = "UTC")
}

gx_pid_test_public_dns <- function(host) {
  rep("93.184.216.34", length(host))
}

gx_pid_test_response <- function(status = 200L, headers = list(), body = raw(), url) {
  list(
    status = as.integer(status),
    headers = headers,
    body = body,
    url = url
  )
}

gx_pid_test_scripted_performer <- function(responses) {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  state$index <- 0L

  performer <- function(request) {
    state$index <- state$index + 1L
    state$calls[[state$index]] <- request
    if (state$index > length(responses)) {
      stop("Unexpected network request in deterministic PID test.", call. = FALSE)
    }
    response <- responses[[state$index]]
    if (is.function(response)) response(request) else response
  }

  list(performer = performer, state = state)
}

gx_pid_test_options <- function(performer, dns_resolver = gx_pid_test_public_dns) {
  withr::local_options(
    list(
      geoconnexr.performer = performer,
      geoconnexr.dns_resolver = dns_resolver,
      geoconnexr.clock = gx_pid_test_fixed_clock,
      geoconnexr.cache_dir = withr::local_tempdir(),
      geoconnexr.offline = FALSE
    ),
    .local_envir = parent.frame()
  )
}

test_that("gx_resolve has stable zero-row behavior", {
  out <- gx_resolve(character())

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_identical(
    names(out),
    c(
      "pid_uri", "initial_status", "final_status", "landing_url",
      "redirect_chain", "resolved_at", "problem_code"
    )
  )
  expect_type(out$pid_uri, "character")
  expect_type(out$initial_status, "integer")
  expect_type(out$final_status, "integer")
  expect_type(out$landing_url, "character")
  expect_type(out$redirect_chain, "list")
  expect_s3_class(out$resolved_at, "POSIXct")
  expect_type(out$problem_code, "character")
})

test_that("gx_resolve preserves PID identity and the full redirect chain", {
  pid <- "https://geoconnex.us/ref/mainstems/29559"
  landing <- "https://reference.geoconnex.us/collections/mainstems/items/29559"
  calls <- gx_pid_test_scripted_performer(list(
    function(request) gx_pid_test_response(
      303L,
      headers = list(Location = landing),
      url = request$url
    ),
    function(request) gx_pid_test_response(
      200L,
      headers = list(`Content-Type` = "application/geo+json"),
      body = charToRaw('{"type":"Feature"}'),
      url = request$url
    )
  ))
  gx_pid_test_options(calls$performer)

  out <- gx_resolve(pid)

  expect_equal(out$pid_uri, pid)
  expect_equal(out$initial_status, 303L)
  expect_equal(out$final_status, 200L)
  expect_equal(out$landing_url, landing)
  expect_identical(out$redirect_chain[[1]], c(pid, landing))
  expect_equal(out$resolved_at, gx_pid_test_fixed_clock())
  expect_true(is.na(out$problem_code))
  expect_equal(calls$state$index, 2L)
  expect_true(all(vapply(calls$state$calls, `[[`, character(1), "method") == "HEAD"))
})

test_that("gx_resolve falls back from rejected HEAD to a minimal GET", {
  pid <- "https://geoconnex.us/ref/gages/1000001"
  landing <- "https://reference.geoconnex.us/collections/gages/items/1000001"
  calls <- gx_pid_test_scripted_performer(list(
    function(request) gx_pid_test_response(405L, url = request$url),
    function(request) gx_pid_test_response(
      303L,
      headers = list(Location = landing),
      url = request$url
    ),
    function(request) gx_pid_test_response(
      200L,
      headers = list(`Content-Type` = "application/ld+json"),
      body = charToRaw('{"@id":"https://geoconnex.us/ref/gages/1000001"}'),
      url = request$url
    )
  ))
  gx_pid_test_options(calls$performer)

  out <- gx_resolve(pid)

  expect_equal(out$initial_status, 405L)
  expect_equal(out$final_status, 200L)
  expect_equal(out$landing_url, landing)
  expect_identical(out$redirect_chain[[1]], c(pid, landing))
  expect_identical(
    vapply(calls$state$calls, `[[`, character(1), "method"),
    c("HEAD", "GET", "GET")
  )
})

test_that("gx_resolve preserves duplicate input order", {
  one <- "https://geoconnex.us/ref/gages/1"
  two <- "https://geoconnex.us/ref/gages/2"
  inputs <- c(two, one, two)
  calls <- gx_pid_test_scripted_performer(lapply(inputs, function(uri) {
    function(request) gx_pid_test_response(
      200L,
      headers = list(`Content-Type` = "application/ld+json"),
      url = request$url
    )
  }))
  gx_pid_test_options(calls$performer)

  out <- gx_resolve(inputs)

  expect_identical(out$pid_uri, inputs)
  expect_equal(nrow(out), 3L)
  expect_identical(
    lapply(out$redirect_chain, identity),
    lapply(inputs, function(x) x)
  )
})

test_that("gx_resolve reports unsafe and malformed redirects without following", {
  pid <- "https://geoconnex.us/ref/gages/1000001"
  private <- gx_pid_test_scripted_performer(list(
    function(request) gx_pid_test_response(
      302L,
      headers = list(Location = "http://127.0.0.1/private"),
      url = request$url
    )
  ))
  gx_pid_test_options(private$performer)

  unsafe <- gx_resolve(pid)
  expect_equal(private$state$index, 1L)
  expect_match(unsafe$problem_code, "unsafe|redirect")
  expect_identical(unsafe$redirect_chain[[1]], pid)

  missing <- gx_pid_test_scripted_performer(list(
    function(request) gx_pid_test_response(302L, headers = list(), url = request$url)
  ))
  gx_pid_test_options(missing$performer)
  malformed <- gx_resolve(pid)
  expect_equal(missing$state$index, 1L)
  expect_match(malformed$problem_code, "redirect")
  expect_identical(malformed$redirect_chain[[1]], pid)
})

test_that("gx_resolve detects redirect loops deterministically", {
  pid <- "https://geoconnex.us/ref/gages/1000001"
  landing <- "https://reference.geoconnex.us/collections/gages/items/1000001"
  calls <- gx_pid_test_scripted_performer(list(
    function(request) gx_pid_test_response(
      302L,
      headers = list(Location = landing),
      url = request$url
    ),
    function(request) gx_pid_test_response(
      302L,
      headers = list(Location = pid),
      url = request$url
    )
  ))
  gx_pid_test_options(calls$performer)

  out <- gx_resolve(pid)

  expect_equal(calls$state$index, 2L)
  expect_match(out$problem_code, "loop|redirect")
  expect_identical(out$redirect_chain[[1]], c(pid, landing))
})

test_that("gx_resolve follows safe relative redirects", {
  pid <- "https://geoconnex.us/ref/gages/1000001"
  landing <- "https://geoconnex.us/landing/gage/1000001"
  calls <- gx_pid_test_scripted_performer(list(
    function(request) gx_pid_test_response(
      303L,
      headers = list(Location = "/landing/gage/1000001"),
      url = request$url
    ),
    function(request) gx_pid_test_response(200L, url = request$url)
  ))
  gx_pid_test_options(calls$performer)

  out <- gx_resolve(pid)

  expect_identical(out$redirect_chain[[1]], c(pid, landing))
  expect_equal(out$landing_url, landing)
  expect_true(is.na(out$problem_code))
})

test_that("gx_resolve honors an explicit PID client", {
  pid <- "https://geoconnex.us/ref/gages/1000001"
  calls <- gx_pid_test_scripted_performer(list(
    function(request) gx_pid_test_response(200L, url = request$url)
  ))
  gx_pid_test_options(calls$performer)
  client <- gx_client("pid", timeout = 7, retries = 0L, max_bytes = 128L)

  out <- gx_resolve(pid, client = client)

  expect_equal(out$final_status, 200L)
  expect_equal(calls$state$calls[[1]]$timeout, 7)
  expect_equal(calls$state$calls[[1]]$retries, 0L)
  expect_equal(calls$state$calls[[1]]$max_bytes, 128L)
  expect_true(length(calls$state$calls[[1]]$resolved_ip) >= 1L)
})

test_that("gx_resolve reports transport failures with a stable code", {
  pid <- "https://geoconnex.us/ref/gages/1000001"
  gx_pid_test_options(function(request) stop("socket failed"))

  out <- gx_resolve(pid)

  expect_equal(out$problem_code, "transport")
  expect_true(is.na(out$final_status))
})

test_that("gx_resolve can replay a cached redirect chain offline", {
  pid <- "https://geoconnex.us/ref/gages/1000001"
  landing <- "https://reference.geoconnex.us/collections/gages/items/1000001"
  cache_dir <- withr::local_tempdir()
  calls <- gx_pid_test_scripted_performer(list(
    function(request) gx_pid_test_response(
      303L,
      headers = list(Location = landing),
      url = request$url
    ),
    function(request) gx_pid_test_response(200L, url = request$url)
  ))
  gx_pid_test_options(calls$performer)
  online <- gx_client("pid", cache_dir = cache_dir)
  expect_true(is.na(gx_resolve(pid, client = online)$problem_code))

  withr::local_options(geoconnexr.performer = function(request) {
    stop("offline resolution attempted the network")
  })
  offline <- gx_client("pid", offline = TRUE, cache_dir = cache_dir)
  resolved <- gx_resolve(pid, client = offline)

  expect_equal(resolved$initial_status, 303L)
  expect_equal(resolved$final_status, 200L)
  expect_identical(resolved$redirect_chain[[1]], c(pid, landing))
  expect_true(is.na(resolved$problem_code))
  expect_equal(calls$state$index, 2L)
})

test_that("HEAD metadata does not count as a transferred response body", {
  pid <- "https://geoconnex.us/ref/gages/1000001"
  calls <- gx_pid_test_scripted_performer(list(
    function(request) gx_pid_test_response(
      200L,
      headers = list(
        `Content-Length` = "9999999",
        `Content-Encoding` = "gzip"
      ),
      body = raw(),
      url = request$url
    )
  ))
  gx_pid_test_options(calls$performer)
  client <- gx_client("pid", max_bytes = 10L, cache = FALSE)

  resolved <- gx_resolve(pid, client = client)

  expect_equal(resolved$initial_status, 200L)
  expect_equal(resolved$final_status, 200L)
  expect_equal(resolved$landing_url, pid)
  expect_true(is.na(resolved$problem_code))
})
