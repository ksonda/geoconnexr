.gx_http_scheduler_contract_version <- "0.1.0"
.gx_http_scheduler_max_safe_integer <- 9007199254740991
.gx_http_scheduler_job_columns <- c(
  "order", "job_id", "host", "cache_key", "max_bytes"
)
.gx_http_scheduler_reservation_columns <- c(
  "token", "order", "job_id", "host", "cache_key", "max_bytes"
)
.gx_http_scheduler_terminal_states <- c(
  "completed", "deferred_request_budget", "deferred_byte_budget"
)

gx_http_scheduler_abort <- function(message,
                                    class = "gx_error_http_scheduler") {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_http_scheduler", "gx_error_client"))
  )
}

gx_http_scheduler_whole_number <- function(x, name, minimum, maximum) {
  valid <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x == floor(x) && x >= minimum && x <= maximum
  if (!valid) {
    gx_http_scheduler_abort(
      paste("Internal scheduler", name, "is invalid."),
      "gx_error_http_scheduler_contract"
    )
  }
  as.double(x)
}

gx_http_scheduler_jobs <- function(jobs) {
  if (!is.data.frame(jobs) ||
      !identical(names(jobs), .gx_http_scheduler_job_columns)) {
    gx_http_scheduler_abort(
      "Internal scheduler jobs violate their exact table shape.",
      "gx_error_http_scheduler_contract"
    )
  }
  rows <- nrow(jobs)
  if (!is.numeric(jobs$order) || !is.character(jobs$job_id) ||
      !is.character(jobs$host) || !is.character(jobs$cache_key) ||
      !is.numeric(jobs$max_bytes) || anyNA(jobs)) {
    gx_http_scheduler_abort(
      "Internal scheduler jobs have invalid column types or missing values.",
      "gx_error_http_scheduler_contract"
    )
  }
  expected_order <- if (rows) seq_len(rows) else integer()
  valid_order <- identical(as.integer(jobs$order), expected_order) &&
    all(jobs$order == floor(jobs$order))
  valid_ids <- all(nzchar(jobs$job_id)) && !anyDuplicated(jobs$job_id)
  valid_hosts <- all(
    nzchar(jobs$host) &
      jobs$host == tolower(jobs$host) &
      !grepl("[[:space:]/:@]", jobs$host) &
      !startsWith(jobs$host, ".") & !endsWith(jobs$host, ".")
  )
  valid_keys <- all(grepl("^[0-9a-f]{64}$", jobs$cache_key))
  valid_bytes <- all(
    jobs$max_bytes == floor(jobs$max_bytes) & jobs$max_bytes >= 1 &
      jobs$max_bytes <= .Machine$integer.max
  )
  compatible_keys <- TRUE
  if (rows && anyDuplicated(jobs$cache_key)) {
    groups <- split(seq_len(rows), jobs$cache_key)
    compatible_keys <- all(vapply(groups, function(index) {
      length(unique(jobs$host[index])) == 1L &&
        length(unique(jobs$max_bytes[index])) == 1L
    }, logical(1)))
  }
  if (!valid_order || !valid_ids || !valid_hosts || !valid_keys ||
      !valid_bytes || !compatible_keys) {
    gx_http_scheduler_abort(
      paste(
        "Internal scheduler jobs must be ordered, uniquely identified,",
        "host-normalized, cache-keyed, byte-bounded, and compatible for",
        "single-flight sharing."
      ),
      "gx_error_http_scheduler_contract"
    )
  }
  tibble::as_tibble(jobs)
}

gx_http_scheduler_empty_reservations <- function() {
  tibble::tibble(
    token = integer(), order = integer(), job_id = character(),
    host = character(), cache_key = character(), max_bytes = numeric()
  )
}

gx_http_scheduler_new <- function(jobs, max_active = 4L,
                                  max_per_host = min(4L, max_active),
                                  max_requests = nrow(jobs),
                                  max_bytes = sum(jobs$max_bytes)) {
  jobs <- gx_http_scheduler_jobs(jobs)
  max_active <- gx_http_scheduler_whole_number(
    max_active, "max_active", 1, 1024
  )
  max_per_host <- gx_http_scheduler_whole_number(
    max_per_host, "max_per_host", 1, max_active
  )
  max_requests <- gx_http_scheduler_whole_number(
    max_requests, "max_requests", 0, .Machine$integer.max
  )
  max_bytes <- gx_http_scheduler_whole_number(
    max_bytes, "max_bytes", 0, .gx_http_scheduler_max_safe_integer
  )

  state <- new.env(parent = emptyenv())
  class(state) <- "gx_http_scheduler"
  state$contract_version <- .gx_http_scheduler_contract_version
  state$jobs <- jobs
  state$max_active <- as.integer(max_active)
  state$max_per_host <- as.integer(max_per_host)
  state$max_requests <- as.integer(max_requests)
  state$max_bytes <- max_bytes
  state$status <- rep("queued", nrow(jobs))
  state$token <- rep(NA_integer_, nrow(jobs))
  state$leader_order <- rep(NA_integer_, nrow(jobs))
  state$result_origin <- rep(NA_character_, nrow(jobs))
  state$results <- rep(list(NULL), nrow(jobs))
  state$requests_started <- 0L
  state$bytes_reserved <- 0
  state$bytes_consumed <- 0
  state$next_token <- 1L
  state
}

gx_http_scheduler_assert <- function(state) {
  if (!inherits(state, "gx_http_scheduler") || !is.environment(state) ||
      !identical(state$contract_version, .gx_http_scheduler_contract_version)) {
    gx_http_scheduler_abort(
      "Internal scheduler state has an invalid contract.",
      "gx_error_http_scheduler_contract"
    )
  }
  jobs <- gx_http_scheduler_jobs(state$jobs)
  rows <- nrow(jobs)
  vector_lengths <- vapply(
    c("status", "token", "leader_order", "result_origin", "results"),
    function(name) length(state[[name]]),
    integer(1)
  )
  valid_limits <- is.integer(state$max_active) &&
    is.integer(state$max_per_host) && is.integer(state$max_requests) &&
    length(state$max_active) == 1L && length(state$max_per_host) == 1L &&
    length(state$max_requests) == 1L && state$max_active >= 1L &&
    state$max_per_host >= 1L && state$max_per_host <= state$max_active &&
    state$max_requests >= 0L && is.numeric(state$max_bytes) &&
    length(state$max_bytes) == 1L && is.finite(state$max_bytes) &&
    state$max_bytes >= 0 &&
    state$max_bytes <= .gx_http_scheduler_max_safe_integer
  valid_accounting <- is.integer(state$requests_started) &&
    length(state$requests_started) == 1L && state$requests_started >= 0L &&
    state$requests_started <= state$max_requests &&
    is.numeric(state$bytes_reserved) && length(state$bytes_reserved) == 1L &&
    is.finite(state$bytes_reserved) && state$bytes_reserved >= 0 &&
    is.numeric(state$bytes_consumed) && length(state$bytes_consumed) == 1L &&
    is.finite(state$bytes_consumed) && state$bytes_consumed >= 0 &&
    state$bytes_reserved + state$bytes_consumed <= state$max_bytes &&
    is.integer(state$next_token) && length(state$next_token) == 1L &&
    state$next_token >= 1L
  allowed_states <- c(
    "queued", "active", "follower", .gx_http_scheduler_terminal_states
  )
  valid_vectors <- identical(unname(vector_lengths), rep(rows, 5L)) &&
    is.character(state$status) && all(state$status %in% allowed_states) &&
    is.integer(state$token) && is.integer(state$leader_order) &&
    is.character(state$result_origin) && is.list(state$results)
  if (!valid_limits || !valid_accounting || !valid_vectors) {
    gx_http_scheduler_abort(
      "Internal scheduler state is corrupt.",
      "gx_error_http_scheduler_contract"
    )
  }

  active <- which(state$status == "active")
  followers <- which(state$status == "follower")
  completed <- which(state$status == "completed")
  inactive <- which(!state$status %in% c("active", "follower", "completed"))
  pending <- which(state$status %in% c("queued", "active", "follower"))
  valid_inactive <- !length(inactive) || (
    all(is.na(state$token[inactive])) &&
      all(is.na(state$leader_order[inactive]))
  )
  valid_pending <- !length(pending) || (
    all(is.na(state$result_origin[pending])) &&
      all(vapply(state$results[pending], is.null, logical(1)))
  )
  valid_active <- length(active) <= state$max_active &&
    !anyNA(state$token[active]) && !anyDuplicated(state$token[active]) &&
    all(state$leader_order[active] == active)
  if (length(active)) {
    host_counts <- table(jobs$host[active])
    valid_active <- valid_active && all(host_counts <= state$max_per_host) &&
      identical(
        state$bytes_reserved,
        as.numeric(sum(jobs$max_bytes[active]))
      )
  } else {
    valid_active <- valid_active && identical(state$bytes_reserved, 0)
  }
  valid_followers <- !length(followers) || all(vapply(followers, function(i) {
    leader <- state$leader_order[[i]]
    !is.na(leader) && leader >= 1L && leader <= rows &&
      state$status[[leader]] == "active" &&
      identical(jobs$cache_key[[leader]], jobs$cache_key[[i]]) &&
      identical(state$token[[leader]], state$token[[i]])
  }, logical(1)))
  valid_completed <- !length(completed) || all(vapply(completed, function(i) {
    origin <- state$result_origin[[i]]
    leader <- state$leader_order[[i]]
    !is.na(state$token[[i]]) && !is.na(leader) && leader >= 1L &&
      leader <= rows && origin %in% c("network", "single_flight") &&
      if (identical(origin, "network")) {
        identical(leader, i)
      } else {
        state$status[[leader]] == "completed" &&
          identical(state$result_origin[[leader]], "network") &&
          identical(state$token[[leader]], state$token[[i]]) &&
          identical(jobs$cache_key[[leader]], jobs$cache_key[[i]])
      }
  }, logical(1)))
  accounted_requests <- length(active) + sum(
    state$status == "completed" & state$result_origin == "network",
    na.rm = TRUE
  )
  valid_request_accounting <-
    identical(state$requests_started, as.integer(accounted_requests)) &&
    identical(state$next_token, state$requests_started + 1L)
  if (!valid_inactive || !valid_pending || !valid_active ||
      !valid_followers || !valid_completed || !valid_request_accounting) {
    gx_http_scheduler_abort(
      "Internal scheduler permits or single-flight state are corrupt.",
      "gx_error_http_scheduler_contract"
    )
  }
  invisible(state)
}

gx_http_scheduler_active_key <- function(state, cache_key) {
  index <- which(
    state$status == "active" & state$jobs$cache_key == cache_key
  )
  if (length(index)) index[[1L]] else NA_integer_
}

gx_http_scheduler_dispatch <- function(state) {
  gx_http_scheduler_assert(state)
  launched <- gx_http_scheduler_empty_reservations()
  rows <- nrow(state$jobs)
  if (!rows) return(launched)

  for (i in seq_len(rows)) {
    if (!identical(state$status[[i]], "queued")) next

    leader <- gx_http_scheduler_active_key(
      state, state$jobs$cache_key[[i]]
    )
    if (!is.na(leader)) {
      state$status[[i]] <- "follower"
      state$token[[i]] <- state$token[[leader]]
      state$leader_order[[i]] <- leader
      next
    }

    active <- which(state$status == "active")
    if (length(active) >= state$max_active) next
    host_active <- sum(state$jobs$host[active] == state$jobs$host[[i]])
    if (host_active >= state$max_per_host) next

    if (state$requests_started >= state$max_requests) {
      state$status[[i]] <- "deferred_request_budget"
      next
    }
    requested_bytes <- state$jobs$max_bytes[[i]]
    available_bytes <- state$max_bytes - state$bytes_consumed -
      state$bytes_reserved
    if (requested_bytes > available_bytes) {
      if (!length(active)) state$status[[i]] <- "deferred_byte_budget"
      next
    }

    token <- state$next_token
    if (token == .Machine$integer.max) {
      gx_http_scheduler_abort(
        "Internal scheduler token space is exhausted.",
        "gx_error_http_scheduler_budget"
      )
    }
    state$status[[i]] <- "active"
    state$token[[i]] <- token
    state$leader_order[[i]] <- i
    state$requests_started <- state$requests_started + 1L
    state$bytes_reserved <- state$bytes_reserved + requested_bytes
    state$next_token <- token + 1L
    launched <- rbind(
      launched,
      tibble::tibble(
        token = token,
        order = as.integer(state$jobs$order[[i]]),
        job_id = state$jobs$job_id[[i]],
        host = state$jobs$host[[i]],
        cache_key = state$jobs$cache_key[[i]],
        max_bytes = as.numeric(requested_bytes)
      )
    )
  }
  gx_http_scheduler_assert(state)
  launched
}

gx_http_scheduler_complete <- function(state, token, result,
                                       charged_bytes) {
  gx_http_scheduler_assert(state)
  token <- gx_http_scheduler_whole_number(
    token, "token", 1, .Machine$integer.max
  )
  active <- which(state$status == "active" & state$token == token)
  if (length(active) != 1L) {
    gx_http_scheduler_abort(
      "Internal scheduler completion token is not active.",
      "gx_error_http_scheduler_contract"
    )
  }
  leader <- active[[1L]]
  reserved <- state$jobs$max_bytes[[leader]]
  charged_bytes <- gx_http_scheduler_whole_number(
    charged_bytes, "charged_bytes", 0, reserved
  )
  followers <- which(
    state$status == "follower" & state$leader_order == leader &
      state$token == token
  )
  completed <- c(leader, followers)

  next_reserved <- state$bytes_reserved - reserved
  next_consumed <- state$bytes_consumed + charged_bytes
  if (next_reserved < 0 ||
      next_reserved + next_consumed > state$max_bytes) {
    gx_http_scheduler_abort(
      "Internal scheduler completion would violate its byte budget.",
      "gx_error_http_scheduler_budget"
    )
  }

  state$bytes_reserved <- next_reserved
  state$bytes_consumed <- next_consumed
  state$status[completed] <- "completed"
  state$result_origin[[leader]] <- "network"
  state$results[leader] <- list(result)
  if (length(followers)) {
    state$result_origin[followers] <- "single_flight"
    for (i in followers) state$results[i] <- list(result)
  }
  gx_http_scheduler_assert(state)
  list(
    token = as.integer(token),
    leader_order = as.integer(leader),
    completed_orders = as.integer(completed),
    charged_bytes = as.numeric(charged_bytes),
    released_bytes = as.numeric(reserved - charged_bytes)
  )
}

gx_http_scheduler_collect <- function(state) {
  gx_http_scheduler_assert(state)
  rows <- nrow(state$jobs)
  if (!rows) {
    return(tibble::tibble(
      order = integer(), job_id = character(), status = character(),
      result_origin = character(), result = list()
    ))
  }
  terminal <- state$status %in% .gx_http_scheduler_terminal_states
  first_open <- which(!terminal)
  count <- if (length(first_open)) first_open[[1L]] - 1L else rows
  if (!count) {
    return(tibble::tibble(
      order = integer(), job_id = character(), status = character(),
      result_origin = character(), result = list()
    ))
  }
  index <- seq_len(count)
  tibble::tibble(
    order = as.integer(state$jobs$order[index]),
    job_id = state$jobs$job_id[index],
    status = state$status[index],
    result_origin = state$result_origin[index],
    result = state$results[index]
  )
}

gx_http_scheduler_snapshot <- function(state) {
  gx_http_scheduler_assert(state)
  active <- which(state$status == "active")
  tibble::tibble(
    max_active = state$max_active,
    max_per_host = state$max_per_host,
    max_requests = state$max_requests,
    max_bytes = state$max_bytes,
    active = as.integer(length(active)),
    queued = as.integer(sum(state$status == "queued")),
    followers = as.integer(sum(state$status == "follower")),
    terminal = as.integer(
      sum(state$status %in% .gx_http_scheduler_terminal_states)
    ),
    requests_started = state$requests_started,
    bytes_reserved = state$bytes_reserved,
    bytes_consumed = state$bytes_consumed
  )
}
