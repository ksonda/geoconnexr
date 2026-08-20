.gx_http_multi_max_attempts <- 1024L
.gx_http_multi_attempt_fields <- c("token", "request", "min_interval")
.gx_http_multi_event_fields <- c("token", "request", "response", "error")
.gx_http_multi_plan_fields <- c(
  "client", "request", "check_status", "response_validator",
  "cache_eligible"
)
.gx_http_multi_result_fields <- c(
  "order", "request_id", "origin", "response", "error"
)

gx_http_multi_abort <- function(message,
                                class = "gx_error_http_multi_contract") {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_http_multi", "gx_error_client"))
  )
}

gx_http_multi_adapter <- function() {
  adapter <- getOption("geoconnexr.multi_adapter")
  if (is.null(adapter)) {
    adapter <- list(
      new_pool = curl::new_pool,
      new_handle = curl::new_handle,
      handle_setopt = curl::handle_setopt,
      multi_add = curl::multi_add,
      multi_run = curl::multi_run,
      multi_list = curl::multi_list,
      multi_cancel = curl::multi_cancel,
      parse_headers = curl::parse_headers_list
    )
  }
  expected <- c(
    "new_pool", "new_handle", "handle_setopt", "multi_add", "multi_run",
    "multi_list", "multi_cancel", "parse_headers"
  )
  valid <- is.list(adapter) && identical(names(adapter), expected) &&
    all(vapply(adapter, is.function, logical(1)))
  if (!valid) {
    gx_http_multi_abort("The configured curl multi adapter is invalid.")
  }
  adapter
}

gx_http_multi_condition <- function(message, class, gx_bytes = NA_real_) {
  structure(
    list(message = message, call = NULL, gx_bytes = as.numeric(gx_bytes)),
    class = unique(c(class, "gx_error_http_multi", "gx_error", "error", "condition"))
  )
}

gx_http_multi_request <- function(request) {
  expected <- c(
    "method", "url", "headers", "body", "timeout", "retries",
    "max_bytes", "user_agent", "request_id", "resolved_host",
    "resolved_port", "resolved_ip", "throttle_delay"
  )
  valid <- is.list(request) && identical(names(request), expected) &&
    is.character(request$method) && length(request$method) == 1L &&
    request$method %in% c("GET", "HEAD", "POST") &&
    is.character(request$url) && length(request$url) == 1L &&
    !is.na(request$url) &&
    identical(request$url, gx_canonical_url(request$url)) &&
    is.character(request$headers) && !anyNA(request$headers) &&
    is.raw(request$body) && is.numeric(request$timeout) &&
    length(request$timeout) == 1L && is.finite(request$timeout) &&
    request$timeout > 0 && is.integer(request$retries) &&
    identical(request$retries, 0L) && is.integer(request$max_bytes) &&
    length(request$max_bytes) == 1L && request$max_bytes >= 1L &&
    is.character(request$user_agent) && length(request$user_agent) == 1L &&
    !is.na(request$user_agent) && nzchar(request$user_agent) &&
    is.character(request$request_id) && length(request$request_id) == 1L &&
    grepl("^[0-9a-f]{64}$", request$request_id) &&
    is.character(request$resolved_host) &&
    length(request$resolved_host) == 1L &&
    !is.na(request$resolved_host) && nzchar(request$resolved_host) &&
    is.character(request$resolved_port) &&
    length(request$resolved_port) == 1L &&
    !is.na(request$resolved_port) && nzchar(request$resolved_port) &&
    is.character(request$resolved_ip) && !anyNA(request$resolved_ip) &&
    is.numeric(request$throttle_delay) &&
    length(request$throttle_delay) == 1L &&
    is.finite(request$throttle_delay) && request$throttle_delay >= 0
  if (!valid) {
    gx_http_multi_abort("A curl multi request violates its exact contract.")
  }
  normalized <- gx_normalize_headers(request$headers)
  if (!identical(normalized, request$headers) ||
      !identical(gx_header(normalized, "accept-encoding"), "identity")) {
    gx_http_multi_abort(
      "A curl multi request has noncanonical or encoded headers."
    )
  }
  request
}

gx_http_multi_attempts <- function(attempts) {
  valid <- is.list(attempts) && !is.data.frame(attempts) &&
    length(attempts) >= 1L && length(attempts) <= .gx_http_multi_max_attempts
  if (!valid) {
    gx_http_multi_abort("Curl multi attempts must be one bounded list.")
  }
  tokens <- integer(length(attempts))
  for (i in seq_along(attempts)) {
    attempt <- attempts[[i]]
    valid_attempt <- is.list(attempt) &&
      identical(names(attempt), .gx_http_multi_attempt_fields) &&
      is.integer(attempt$token) && length(attempt$token) == 1L &&
      !is.na(attempt$token) && attempt$token >= 1L &&
      is.numeric(attempt$min_interval) &&
      length(attempt$min_interval) == 1L &&
      !is.na(attempt$min_interval) && is.finite(attempt$min_interval) &&
      attempt$min_interval >= 0 && attempt$min_interval <= 3600
    if (!valid_attempt) {
      gx_http_multi_abort("A curl multi attempt violates its exact contract.")
    }
    attempt$request <- gx_http_multi_request(attempt$request)
    if (!identical(attempt$request$resolved_host,
                   gx_safe_target(attempt$request$url, FALSE)$host) ||
        length(attempt$request$resolved_ip)) {
      gx_http_multi_abort(
        "A curl multi attempt must enter transport unresolved."
      )
    }
    attempts[[i]] <- attempt
    tokens[[i]] <- attempt$token
  }
  if (anyDuplicated(tokens)) {
    gx_http_multi_abort("Curl multi attempt tokens must be unique.")
  }
  attempts
}

gx_http_multi_limits <- function(max_active, max_per_host) {
  max_active <- gx_http_scheduler_whole_number(
    max_active, "max_active", 1, .gx_http_multi_max_attempts
  )
  max_per_host <- gx_http_scheduler_whole_number(
    max_per_host, "max_per_host", 1, max_active
  )
  list(
    max_active = as.integer(max_active),
    max_per_host = as.integer(max_per_host)
  )
}

gx_http_multi_event <- function(token, request, response = NULL,
                                error = NULL) {
  valid_error <- is.null(error) || inherits(error, "error")
  if ((is.null(response) && is.null(error)) ||
      (!is.null(response) && !is.null(error)) ||
      !valid_error) {
    gx_http_multi_abort("A curl multi completion event is invalid.")
  }
  structure(
    list(
      token = as.integer(token), request = request,
      response = response, error = error
    ),
    names = .gx_http_multi_event_fields
  )
}

gx_http_multi_body <- function(chunks) {
  if (!length(chunks)) raw() else do.call(c, chunks)
}

gx_http_multi_plan <- function(client, method = "GET", url = client$base_url,
                               headers = list(), body = NULL,
                               content_type = NULL, check_status = TRUE,
                               response_validator = NULL) {
  if (!inherits(client, "gx_client") || !identical(client$retries, 0L)) {
    gx_http_multi_abort(
      "Curl multi plans require a zero-retry client created by gx_client()."
    )
  }
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    gx_http_multi_abort("A curl multi plan requires one HTTP method.")
  }
  method <- toupper(method)
  if (!method %in% c("GET", "HEAD", "POST") ||
      (identical(client$endpoint, "graph") && !identical(method, "POST"))) {
    gx_http_multi_abort("The curl multi method violates endpoint policy.")
  }
  if (!is.logical(check_status) || length(check_status) != 1L ||
      is.na(check_status) ||
      (!is.null(response_validator) && !is.function(response_validator))) {
    gx_http_multi_abort("Curl multi response policy is invalid.")
  }
  target <- gx_safe_target(url, resolve_dns = FALSE)
  headers <- gx_normalize_headers(headers)
  if (!is.null(content_type)) {
    if (!is.character(content_type) || length(content_type) != 1L ||
        is.na(content_type)) {
      gx_http_multi_abort("Curl multi content type must be one string.")
    }
    headers["content-type"] <- content_type
  }
  if (!"accept-encoding" %in% names(headers)) {
    headers["accept-encoding"] <- "identity"
  } else if (!identical(tolower(headers[["accept-encoding"]]), "identity")) {
    gx_http_multi_abort("Curl multi plans require identity encoding.")
  }
  if (is.null(body)) {
    body <- raw()
  } else if (is.character(body) && length(body) == 1L && !is.na(body)) {
    body <- charToRaw(enc2utf8(body))
  } else if (!is.raw(body)) {
    gx_http_multi_abort("Curl multi request bodies must be raw or one string.")
  }
  key <- gx_cache_key(client, method, target$url, headers, body)
  request <- list(
    method = method,
    url = target$url,
    headers = headers,
    body = body,
    timeout = client$timeout,
    retries = 0L,
    max_bytes = client$max_bytes,
    user_agent = client$user_agent,
    request_id = key,
    resolved_host = target$host,
    resolved_port = target$port,
    resolved_ip = character(),
    throttle_delay = 0
  )
  gx_http_multi_request(request)
  structure(
    list(
      client = client,
      request = request,
      check_status = check_status,
      response_validator = response_validator,
      cache_eligible = isTRUE(client$cache) &&
        gx_cache_allowed(headers, target$url)
    ),
    names = .gx_http_multi_plan_fields,
    class = "gx_http_multi_plan"
  )
}

gx_http_multi_plan_validate <- function(plan) {
  valid <- inherits(plan, "gx_http_multi_plan") && is.list(plan) &&
    identical(names(plan), .gx_http_multi_plan_fields) &&
    inherits(plan$client, "gx_client") &&
    identical(plan$client$retries, 0L) &&
    is.logical(plan$check_status) && length(plan$check_status) == 1L &&
    !is.na(plan$check_status) &&
    (is.null(plan$response_validator) || is.function(plan$response_validator)) &&
    is.logical(plan$cache_eligible) && length(plan$cache_eligible) == 1L &&
    !is.na(plan$cache_eligible)
  if (!valid) gx_http_multi_abort("A curl multi plan is corrupt.")
  gx_http_multi_request(plan$request)
  expected_cache <- isTRUE(plan$client$cache) &&
    gx_cache_allowed(plan$request$headers, plan$request$url)
  if (!identical(plan$cache_eligible, expected_cache) ||
      !identical(
        plan$request$request_id,
        gx_cache_key(
          plan$client, plan$request$method, plan$request$url,
          plan$request$headers, plan$request$body
        )
      )) {
    gx_http_multi_abort("A curl multi plan no longer binds its request.")
  }
  invisible(plan)
}

gx_http_multi_error_with_attempts <- function(cnd, attempts,
                                              status = NULL) {
  if (!inherits(cnd, "error")) {
    cnd <- gx_http_multi_condition(
      "HTTP transport failed; underlying details were withheld.",
      "gx_error_transport"
    )
  }
  cnd$attempts <- attempts
  cnd$attempt_count <- as.integer(sum(attempts$physical %in% TRUE))
  cnd$retry_exhausted <- FALSE
  cnd$retry_stopped <- "concurrent_single_attempt"
  if (!is.null(status)) cnd$status <- as.integer(status)
  cnd
}

gx_http_multi_network_result <- function(event, plan, backend) {
  request <- gx_http_multi_request(event$request)
  if (!is.null(event$error)) {
    if (inherits(event$error, "gx_error_unsafe_url")) {
      attempt <- gx_http_attempt_row(
        request, attempt = 1L, outcome = "policy_error",
        physical = TRUE, retryable = FALSE,
        error_code = gx_http_error_code(event$error), bytes = 0,
        charged_bytes = 0, cache_origin = "network_rejected"
      )
    } else {
      attempt <- gx_http_attempt_from_error(
        event$error, request, attempt = 1L,
        retryable = inherits(event$error, "gx_error_transport"),
        retry_reason = if (inherits(event$error, "gx_error_transport")) {
          "transport_error"
        } else {
          "response_rejected"
        }
      )
    }
    error <- gx_http_multi_error_with_attempts(event$error, attempt)
    return(list(response = NULL, error = error, charged = attempt$charged_bytes[[1L]]))
  }

  response <- tryCatch(
    gx_validate_response(event$response, request, plan$client),
    error = identity
  )
  if (inherits(response, "error")) {
    attempt <- gx_http_attempt_from_error(
      response, request, attempt = 1L, raw_response = event$response,
      retry_reason = "response_rejected"
    )
    error <- gx_http_multi_error_with_attempts(
      response, attempt, status = attempt$status[[1L]]
    )
    return(list(response = NULL, error = error, charged = attempt$charged_bytes[[1L]]))
  }

  attempt <- gx_http_attempt_from_response(response, attempt = 1L)
  response$attempts <- attempt
  response$attempt_count <- 1L
  response$retry_exhausted <- FALSE
  response$retry_stopped <- NA_character_
  validation_error <- tryCatch({
    gx_validate_endpoint_response(response, plan$client, plan$check_status)
    if (!is.null(plan$response_validator)) {
      plan$response_validator(response)
    }
    NULL
  }, error = identity)
  if (!is.null(validation_error)) {
    error <- gx_http_multi_error_with_attempts(
      validation_error, attempt, status = response$status
    )
    return(list(response = NULL, error = error, charged = response$bytes))
  }

  cacheable <- response$status >= 200L && response$status < 400L &&
    gx_response_cache_allowed(response$headers)
  if (!is.null(backend) && cacheable) {
    cached_response <- response
    cached_response$request <- NULL
    cached_response$attempts <- NULL
    cached_response$attempt_count <- NULL
    cached_response$retry_exhausted <- NULL
    cached_response$retry_stopped <- NULL
    backend$set(request$request_id, list(
      cache_schema_version = .gx_cache_schema_version,
      request_id = request$request_id,
      response = cached_response
    ))
  }
  list(response = response, error = NULL, charged = response$bytes)
}

gx_http_multi_result <- function(order, request_id, origin,
                                 response = NULL, error = NULL) {
  structure(
    list(
      order = as.integer(order), request_id = request_id,
      origin = origin, response = response, error = error
    ),
    names = .gx_http_multi_result_fields
  )
}

gx_http_multi_cache_result <- function(plan, backend) {
  if (is.null(backend)) return(NULL)
  cached <- gx_cache_read(
    backend, plan$request$request_id, plan$client, plan$request
  )
  if (is.null(cached)) return(NULL)
  cached$from_cache <- TRUE
  cached$cache_origin <- if (plan$client$offline) {
    "offline_cache"
  } else {
    "fresh_cache"
  }
  cached$request <- plan$request
  cached$attempts <- gx_http_empty_attempts()
  cached$attempt_count <- 0L
  cached$retry_exhausted <- FALSE
  cached$retry_stopped <- NA_character_
  validation_error <- tryCatch({
    gx_validate_endpoint_response(cached, plan$client, plan$check_status)
    if (!is.null(plan$response_validator)) plan$response_validator(cached)
    NULL
  }, error = identity)
  if (is.null(validation_error)) return(cached)
  cache_invalid <- isTRUE(validation_error$cache_invalid) ||
    inherits(validation_error, c("gx_error_content_type", "gx_error_redirect"))
  if (cache_invalid) try(backend$remove(plan$request$request_id), silent = TRUE)
  if (plan$client$offline || !cache_invalid) {
    return(gx_http_multi_error_with_attempts(
      validation_error, gx_http_empty_attempts(), status = cached$status
    ))
  }
  NULL
}

gx_http_multi_events_validate <- function(events, launched) {
  valid <- is.list(events) && length(events) == nrow(launched)
  if (valid) {
    tokens <- vapply(events, function(event) {
      if (!is.list(event) || !identical(names(event), .gx_http_multi_event_fields) ||
          !is.integer(event$token) || length(event$token) != 1L ||
          is.na(event$token)) return(NA_integer_)
      exclusive <- xor(is.null(event$response), is.null(event$error))
      if (!exclusive || (!is.null(event$error) && !inherits(event$error, "error"))) {
        return(NA_integer_)
      }
      event$token
    }, integer(1))
    valid <- !anyNA(tokens) && !anyDuplicated(tokens) &&
      setequal(tokens, launched$token)
  }
  if (!valid) gx_http_multi_abort("Curl multi completion events are incomplete.")
  events
}

gx_http_multi_execute <- function(plans, max_active = 4L,
                                  max_per_host = min(4L, max_active),
                                  max_requests = length(plans),
                                  max_bytes = NULL) {
  if (!is.list(plans) || !length(plans) ||
      length(plans) > .gx_http_multi_max_attempts) {
    gx_http_multi_abort("Curl multi execution requires one bounded plan list.")
  }
  for (plan in plans) gx_http_multi_plan_validate(plan)
  if (is.null(max_bytes)) {
    max_bytes <- sum(vapply(
      plans,
      function(plan) plan$request$max_bytes,
      numeric(1)
    ))
  }
  limits <- gx_http_multi_limits(max_active, max_per_host)
  outputs <- rep(list(NULL), length(plans))
  backends <- rep(list(NULL), length(plans))
  network <- logical(length(plans))

  for (i in seq_along(plans)) {
    plan <- plans[[i]]
    backend <- if (plan$cache_eligible) {
      gx_cache_backend(plan$client$cache_dir)
    } else {
      NULL
    }
    backends[i] <- list(backend)
    cached <- gx_http_multi_cache_result(plan, backend)
    if (inherits(cached, "error")) {
      outputs[[i]] <- gx_http_multi_result(
        i, plan$request$request_id, "cache_error", error = cached
      )
    } else if (!is.null(cached)) {
      outputs[[i]] <- gx_http_multi_result(
        i, plan$request$request_id, cached$cache_origin,
        response = cached
      )
    } else if (plan$client$offline) {
      error <- gx_http_multi_condition(
        "Offline cache miss for a curl multi request.",
        "gx_error_offline_miss"
      )
      outputs[[i]] <- gx_http_multi_result(
        i, plan$request$request_id, "offline_miss", error = error
      )
    } else {
      network[[i]] <- TRUE
    }
  }

  indexes <- which(network)
  if (length(indexes)) {
    jobs <- tibble::tibble(
      order = seq_along(indexes),
      job_id = as.character(indexes),
      host = vapply(indexes, function(i) {
        plans[[i]]$request$resolved_host
      }, character(1)),
      cache_key = vapply(indexes, function(i) {
        plans[[i]]$request$request_id
      }, character(1)),
      max_bytes = vapply(indexes, function(i) {
        as.numeric(plans[[i]]$request$max_bytes)
      }, numeric(1))
    )
    scheduler <- gx_http_scheduler_new(
      jobs, max_active = limits$max_active,
      max_per_host = limits$max_per_host,
      max_requests = max_requests, max_bytes = max_bytes
    )
    performer <- getOption(
      "geoconnexr.multi_performer", gx_default_multi_performer
    )
    if (!is.function(performer)) {
      gx_http_multi_abort("The configured curl multi performer is invalid.")
    }
    repeat {
      launched <- gx_http_scheduler_dispatch(scheduler)
      if (!nrow(launched)) break
      attempts <- lapply(seq_len(nrow(launched)), function(row) {
        index <- as.integer(launched$job_id[[row]])
        list(
          token = launched$token[[row]],
          request = plans[[index]]$request,
          min_interval = plans[[index]]$client$min_interval
        )
      })
      events <- performer(
        attempts,
        max_active = limits$max_active,
        max_per_host = limits$max_per_host
      )
      events <- gx_http_multi_events_validate(events, launched)
      for (event in events) {
        row <- match(event$token, launched$token)
        index <- as.integer(launched$job_id[[row]])
        result <- gx_http_multi_network_result(
          event, plans[[index]], backends[[index]]
        )
        gx_http_scheduler_complete(
          scheduler, event$token, result, result$charged
        )
      }
    }
    collected <- gx_http_scheduler_collect(scheduler)
    if (nrow(collected) != length(indexes)) {
      gx_http_multi_abort("Curl multi scheduling ended before collection.")
    }
    for (row in seq_len(nrow(collected))) {
      index <- as.integer(collected$job_id[[row]])
      if (identical(collected$status[[row]], "completed")) {
        result <- collected$result[[row]]
        outputs[[index]] <- gx_http_multi_result(
          index, plans[[index]]$request$request_id,
          collected$result_origin[[row]],
          response = result$response, error = result$error
        )
      } else {
        error_class <- if (identical(
          collected$status[[row]], "deferred_request_budget"
        )) {
          "gx_error_http_multi_request_budget"
        } else {
          "gx_error_http_multi_byte_budget"
        }
        error <- gx_http_multi_condition(
          "Curl multi request was deferred by its aggregate budget.",
          error_class
        )
        outputs[[index]] <- gx_http_multi_result(
          index, plans[[index]]$request$request_id,
          collected$status[[row]], error = error
        )
      }
    }
  }

  if (any(vapply(outputs, is.null, logical(1)))) {
    gx_http_multi_abort("Curl multi execution did not reconcile every plan.")
  }
  structure(outputs, class = c("gx_http_multi_results", "list"))
}

gx_http_multi_handle <- function(request, adapter) {
  handle <- adapter$new_handle()
  options <- list(
    url = request$url,
    customrequest = request$method,
    followlocation = FALSE,
    maxredirs = 0L,
    maxfilesize = as.double(request$max_bytes),
    http_content_decoding = FALSE,
    noproxy = "*",
    timeout = request$timeout,
    connecttimeout = min(request$timeout, 30),
    useragent = request$user_agent,
    failonerror = FALSE,
    noprogress = FALSE
  )
  if (length(request$resolved_ip)) {
    options$resolve <- paste(
      request$resolved_host, request$resolved_port,
      request$resolved_ip[[1L]], sep = ":"
    )
  }
  if (identical(request$method, "HEAD")) options$nobody <- TRUE
  if (identical(request$method, "POST")) {
    options$postfields <- request$body
    options$postfieldsize <- length(request$body)
  }
  if (length(request$headers)) {
    options$httpheader <- c(
      paste0(names(request$headers), ": ", unname(request$headers)),
      "Expect:"
    )
  }
  adapter$handle_setopt(handle, .list = options)
  handle
}

gx_default_multi_performer <- function(attempts, max_active = 4L,
                                       max_per_host = 4L) {
  attempts <- gx_http_multi_attempts(attempts)
  limits <- gx_http_multi_limits(max_active, max_per_host)
  adapter <- gx_http_multi_adapter()
  pool <- adapter$new_pool(
    total_con = limits$max_active,
    host_con = limits$max_per_host,
    max_streams = 1L,
    multiplex = FALSE
  )
  events <- list()
  append_event <- function(event) {
    events[[length(events) + 1L]] <<- event
    invisible(NULL)
  }
  cleanup <- function() {
    active <- tryCatch(adapter$multi_list(pool = pool), error = function(cnd) list())
    if (length(active)) {
      for (handle in active) try(adapter$multi_cancel(handle), silent = TRUE)
    }
    invisible(NULL)
  }
  on.exit(cleanup(), add = TRUE)

  add_attempt <- function(attempt) {
      token <- attempt$token
      request <- attempt$request
      request$throttle_delay <- tryCatch(
        gx_http_throttle_wait(request$resolved_host, attempt$min_interval),
        error = identity
      )
      if (inherits(request$throttle_delay, "error")) {
        append_event(gx_http_multi_event(
          token, attempt$request, error = request$throttle_delay
        ))
        return(invisible(NULL))
      }
      resolved <- tryCatch(
        gx_safe_target(request$url, resolve_dns = TRUE),
        error = identity
      )
      if (inherits(resolved, "error")) {
        append_event(gx_http_multi_event(
          token, request, error = resolved
        ))
        return(invisible(NULL))
      }
      request$url <- resolved$url
      request$resolved_host <- resolved$host
      request$resolved_port <- resolved$port
      request$resolved_ip <- resolved$addresses
      request <- gx_http_multi_request(request)

      state <- new.env(parent = emptyenv())
      state$chunks <- list()
      state$bytes <- 0
      state$overflow <- FALSE
      state$finished <- FALSE
      handle <- gx_http_multi_handle(request, adapter)
      progress <- function(down, up) {
        valid <- is.numeric(down) && length(down) == 2L &&
          !anyNA(down) && all(is.finite(down)) && all(down >= 0)
        if (!valid) {
          state$overflow <- TRUE
          return(FALSE)
        }
        within <- down[[1L]] <= request$max_bytes &&
          down[[2L]] <= request$max_bytes
        if (!within) state$overflow <- TRUE
        within
      }
      adapter$handle_setopt(handle, progressfunction = progress)
      data <- function(chunk, final = FALSE) {
        if (isTRUE(final) || !length(chunk)) return(invisible(NULL))
        next_bytes <- state$bytes + length(chunk)
        if (!is.finite(next_bytes) || next_bytes > request$max_bytes) {
          state$overflow <- TRUE
          return(invisible(NULL))
        }
        state$bytes <- next_bytes
        state$chunks[[length(state$chunks) + 1L]] <- chunk
        invisible(NULL)
      }
      done <- function(raw_response) {
        if (state$finished) return(invisible(NULL))
        state$finished <- TRUE
        if (state$overflow) {
          error <- gx_http_multi_condition(
            "Response exceeded its hard streaming byte ceiling.",
            "gx_error_payload_too_large",
            NA_real_
          )
          append_event(gx_http_multi_event(token, request, error = error))
          return(invisible(NULL))
        }
        headers <- tryCatch(
          adapter$parse_headers(raw_response$headers),
          error = identity
        )
        if (inherits(headers, "error")) {
          append_event(gx_http_multi_event(token, request, error = headers))
          return(invisible(NULL))
        }
        response <- list(
          status = as.integer(raw_response$status_code),
          headers = headers,
          body = gx_http_multi_body(state$chunks),
          url = raw_response$url %||% request$url
        )
        append_event(gx_http_multi_event(
          token, request, response = response
        ))
        invisible(NULL)
      }
      fail <- function(message) {
        if (state$finished) return(invisible(NULL))
        state$finished <- TRUE
        error <- if (state$overflow) {
          gx_http_multi_condition(
            "Response exceeded its hard streaming byte ceiling.",
            "gx_error_payload_too_large",
            NA_real_
          )
        } else {
          gx_http_multi_condition(
            "HTTP transport failed; underlying details were withheld.",
            "gx_error_transport",
            NA_real_
          )
        }
        append_event(gx_http_multi_event(token, request, error = error))
        invisible(NULL)
      }
      adapter$multi_add(
        handle, done = done, fail = fail, data = data, pool = pool
      )
      adapter$multi_run(timeout = 0, poll = FALSE, pool = pool)
      invisible(NULL)
  }
  for (attempt in attempts) {
    add_attempt(attempt)
  }
  if (length(adapter$multi_list(pool = pool))) {
    adapter$multi_run(timeout = Inf, poll = FALSE, pool = pool)
  }
  cleanup()
  events
}
