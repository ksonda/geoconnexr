gx_test_scheduler_jobs <- function(host, key, max_bytes = 100) {
  rows <- length(host)
  tibble::tibble(
    order = seq_len(rows),
    job_id = paste0("job-", seq_len(rows)),
    host = host,
    cache_key = key,
    max_bytes = rep(as.numeric(max_bytes), length.out = rows)
  )
}

test_that("scheduler admits deterministic total and per-host permits", {
  jobs <- gx_test_scheduler_jobs(
    c("a.example", "a.example", "b.example", "b.example"),
    vapply(1:4, function(i) strrep(as.character(i), 64), character(1)),
    40
  )
  state <- geoconnexr:::gx_http_scheduler_new(
    jobs, max_active = 2L, max_per_host = 1L,
    max_requests = 4L, max_bytes = 160
  )

  first <- geoconnexr:::gx_http_scheduler_dispatch(state)
  expect_identical(first$order, c(1L, 3L))
  expect_identical(first$token, c(1L, 2L))
  expect_identical(first$host, c("a.example", "b.example"))
  expect_identical(
    names(first), geoconnexr:::.gx_http_scheduler_reservation_columns
  )
  expect_equal(geoconnexr:::gx_http_scheduler_snapshot(state)$active, 2L)

  geoconnexr:::gx_http_scheduler_complete(
    state, first$token[[2L]], list(value = "third"), 10
  )
  expect_equal(nrow(geoconnexr:::gx_http_scheduler_collect(state)), 0L)
  second <- geoconnexr:::gx_http_scheduler_dispatch(state)
  expect_identical(second$order, 4L)
  geoconnexr:::gx_http_scheduler_complete(
    state, second$token, list(value = "fourth"), 10
  )
  expect_equal(nrow(geoconnexr:::gx_http_scheduler_collect(state)), 0L)

  geoconnexr:::gx_http_scheduler_complete(
    state, first$token[[1L]], list(value = "first"), 10
  )
  prefix <- geoconnexr:::gx_http_scheduler_collect(state)
  expect_identical(prefix$order, 1L)
  third <- geoconnexr:::gx_http_scheduler_dispatch(state)
  expect_identical(third$order, 2L)
  geoconnexr:::gx_http_scheduler_complete(
    state, third$token, list(value = "second"), 10
  )

  collected <- geoconnexr:::gx_http_scheduler_collect(state)
  expect_identical(collected$order, 1:4)
  expect_identical(collected$job_id, paste0("job-", 1:4))
  expect_true(all(collected$status == "completed"))
  expect_identical(
    vapply(collected$result, `[[`, character(1), "value"),
    c("first", "second", "third", "fourth")
  )
  snapshot <- geoconnexr:::gx_http_scheduler_snapshot(state)
  expect_identical(snapshot$active, 0L)
  expect_identical(snapshot$terminal, 4L)
  expect_identical(snapshot$requests_started, 4L)
  expect_equal(snapshot$bytes_reserved, 0)
  expect_equal(snapshot$bytes_consumed, 40)
})

test_that("scheduler coalesces compatible cache keys without extra budgets", {
  shared <- strrep("a", 64)
  jobs <- gx_test_scheduler_jobs(
    c("one.example", "one.example", "two.example"),
    c(shared, shared, strrep("b", 64)),
    c(50, 50, 30)
  )
  state <- geoconnexr:::gx_http_scheduler_new(
    jobs, max_active = 2L, max_per_host = 2L,
    max_requests = 2L, max_bytes = 80
  )

  launched <- geoconnexr:::gx_http_scheduler_dispatch(state)
  expect_identical(launched$order, c(1L, 3L))
  snapshot <- geoconnexr:::gx_http_scheduler_snapshot(state)
  expect_identical(snapshot$requests_started, 2L)
  expect_identical(snapshot$followers, 1L)
  expect_equal(snapshot$bytes_reserved, 80)

  shared_result <- list(status = 200L, body = charToRaw("same"))
  completion <- geoconnexr:::gx_http_scheduler_complete(
    state, launched$token[[1L]], shared_result, 4
  )
  expect_identical(completion$completed_orders, c(1L, 2L))
  expect_equal(completion$released_bytes, 46)
  geoconnexr:::gx_http_scheduler_complete(
    state, launched$token[[2L]], list(status = 204L), 0
  )

  collected <- geoconnexr:::gx_http_scheduler_collect(state)
  expect_identical(
    collected$result_origin,
    c("network", "single_flight", "network")
  )
  expect_identical(collected$result[[1L]], collected$result[[2L]])
  expect_identical(collected$result[[1L]], shared_result)
  expect_identical(
    geoconnexr:::gx_http_scheduler_snapshot(state)$requests_started, 2L
  )
})

test_that("scheduler reserves request and byte budgets atomically", {
  jobs <- gx_test_scheduler_jobs(
    rep("budget.example", 2),
    c(strrep("c", 64), strrep("d", 64)),
    100
  )
  state <- geoconnexr:::gx_http_scheduler_new(
    jobs, max_active = 2L, max_per_host = 2L,
    max_requests = 1L, max_bytes = 200
  )
  launched <- geoconnexr:::gx_http_scheduler_dispatch(state)
  expect_identical(launched$order, 1L)
  before <- geoconnexr:::gx_http_scheduler_snapshot(state)
  expect_error(
    geoconnexr:::gx_http_scheduler_complete(
      state, launched$token, list(status = 200L), 101
    ),
    class = "gx_error_http_scheduler_contract"
  )
  expect_identical(
    geoconnexr:::gx_http_scheduler_snapshot(state), before
  )

  geoconnexr:::gx_http_scheduler_complete(
    state, launched$token, list(status = 200L), 20
  )
  collected <- geoconnexr:::gx_http_scheduler_collect(state)
  expect_identical(
    collected$status, c("completed", "deferred_request_budget")
  )
  expect_identical(collected$result_origin, c("network", NA_character_))
  snapshot <- geoconnexr:::gx_http_scheduler_snapshot(state)
  expect_identical(snapshot$requests_started, 1L)
  expect_equal(snapshot$bytes_consumed, 20)
  expect_equal(snapshot$bytes_reserved, 0)
})

test_that("unused byte reservations return before later admission", {
  jobs <- gx_test_scheduler_jobs(
    c("one.example", "two.example"),
    c(strrep("e", 64), strrep("f", 64)),
    c(80, 60)
  )
  state <- geoconnexr:::gx_http_scheduler_new(
    jobs, max_active = 2L, max_per_host = 2L,
    max_requests = 2L, max_bytes = 100
  )

  first <- geoconnexr:::gx_http_scheduler_dispatch(state)
  expect_identical(first$order, 1L)
  expect_identical(
    geoconnexr:::gx_http_scheduler_snapshot(state)$queued, 1L
  )
  geoconnexr:::gx_http_scheduler_complete(
    state, first$token, list(ok = TRUE), 20
  )
  second <- geoconnexr:::gx_http_scheduler_dispatch(state)
  expect_identical(second$order, 2L)
  geoconnexr:::gx_http_scheduler_complete(
    state, second$token, list(ok = TRUE), 60
  )
  snapshot <- geoconnexr:::gx_http_scheduler_snapshot(state)
  expect_equal(snapshot$bytes_consumed, 80)
  expect_equal(snapshot$bytes_reserved, 0)

  impossible <- gx_test_scheduler_jobs(
    "large.example", strrep("0", 64), 101
  )
  blocked <- geoconnexr:::gx_http_scheduler_new(
    impossible, max_requests = 1L, max_bytes = 100
  )
  expect_equal(
    nrow(geoconnexr:::gx_http_scheduler_dispatch(blocked)), 0L
  )
  expect_identical(
    geoconnexr:::gx_http_scheduler_collect(blocked)$status,
    "deferred_byte_budget"
  )
})

test_that("scheduler rejects ambiguous or corrupt state", {
  jobs <- gx_test_scheduler_jobs(
    c("one.example", "one.example"),
    rep(strrep("1", 64), 2),
    c(10, 20)
  )
  expect_error(
    geoconnexr:::gx_http_scheduler_new(jobs),
    class = "gx_error_http_scheduler_contract"
  )

  jobs$cache_key[[2L]] <- strrep("2", 64)
  jobs$host[[2L]] <- "BAD HOST"
  expect_error(
    geoconnexr:::gx_http_scheduler_new(jobs),
    class = "gx_error_http_scheduler_contract"
  )

  valid <- gx_test_scheduler_jobs("one.example", strrep("3", 64), 10)
  one <- geoconnexr:::gx_http_scheduler_new(valid, max_active = 1L)
  expect_identical(
    geoconnexr:::gx_http_scheduler_snapshot(one)$max_per_host, 1L
  )
  null_launch <- geoconnexr:::gx_http_scheduler_dispatch(one)
  geoconnexr:::gx_http_scheduler_complete(
    one, null_launch$token, NULL, 0
  )
  expect_null(geoconnexr:::gx_http_scheduler_collect(one)$result[[1L]])

  state <- geoconnexr:::gx_http_scheduler_new(valid)
  state$bytes_reserved <- 1
  expect_error(
    geoconnexr:::gx_http_scheduler_dispatch(state),
    class = "gx_error_http_scheduler_contract"
  )

  state <- geoconnexr:::gx_http_scheduler_new(valid)
  state$token[[1L]] <- 9L
  expect_error(
    geoconnexr:::gx_http_scheduler_dispatch(state),
    class = "gx_error_http_scheduler_contract"
  )
})
