gx_default_dns_resolver <- function(host) {
  curl::nslookup(host, multiple = TRUE, error = FALSE)
}

gx_path_is_symlink <- function(path) {
  link <- Sys.readlink(path)
  is.character(link) && length(link) == 1L && !is.na(link) && nzchar(link)
}

gx_is_ipv4 <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^[0-9]{1,3}(\\.[0-9]{1,3}){3}$", x)) {
    return(FALSE)
  }
  octets <- suppressWarnings(as.integer(strsplit(x, ".", fixed = TRUE)[[1]]))
  length(octets) == 4L && all(!is.na(octets)) && all(octets >= 0L & octets <= 255L)
}

gx_is_nonpublic_ipv4 <- function(x) {
  if (!gx_is_ipv4(x)) {
    return(FALSE)
  }
  o <- as.integer(strsplit(x, ".", fixed = TRUE)[[1]])
  o[[1]] == 0L ||
    o[[1]] == 10L ||
    o[[1]] == 127L ||
    (o[[1]] == 100L && o[[2]] >= 64L && o[[2]] <= 127L) ||
    (o[[1]] == 169L && o[[2]] == 254L) ||
    (o[[1]] == 172L && o[[2]] >= 16L && o[[2]] <= 31L) ||
    (o[[1]] == 192L && o[[2]] == 0L && o[[3]] %in% c(0L, 2L)) ||
    (o[[1]] == 192L && o[[2]] == 168L) ||
    (o[[1]] == 198L && o[[2]] %in% c(18L, 19L)) ||
    (o[[1]] == 198L && o[[2]] == 51L && o[[3]] == 100L) ||
    (o[[1]] == 203L && o[[2]] == 0L && o[[3]] == 113L) ||
    o[[1]] >= 224L
}

gx_is_nonpublic_ipv6 <- function(x) {
  x <- tolower(gsub("^\\[|\\]$", "", x))
  x <- sub("%.*$", "", x)
  if (!grepl(":", x, fixed = TRUE)) {
    return(FALSE)
  }
  if (grepl("^::ffff:", x)) {
    mapped <- sub("^::ffff:", "", x)
    return(!gx_is_ipv4(mapped) || gx_is_nonpublic_ipv4(mapped))
  }
  x %in% c("::", "::1") ||
    grepl("^(fc|fd)", x) ||
    grepl("^fe[89ab]", x) ||
    grepl("^ff", x) ||
    grepl("^2001:db8", x)
}

gx_canonical_url <- function(url) {
  parsed <- tryCatch(httr2::url_parse(url), error = function(cnd) NULL)
  if (is.null(parsed) || is.null(parsed$scheme) || is.null(parsed$hostname)) {
    gx_abort("URL must be an absolute HTTP(S) URL.", "gx_error_unsafe_url")
  }
  parsed$scheme <- tolower(parsed$scheme)
  parsed$hostname <- tolower(sub("\\.$", "", parsed$hostname))
  parsed$fragment <- NULL
  if ((identical(parsed$scheme, "https") && identical(parsed$port, "443")) ||
      (identical(parsed$scheme, "http") && identical(parsed$port, "80"))) {
    parsed$port <- NULL
  }
  httr2::url_build(parsed)
}

gx_safe_target <- function(url, resolve_dns = TRUE) {
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
    gx_abort("URL must be one non-empty string.", "gx_error_unsafe_url")
  }
  parsed <- tryCatch(httr2::url_parse(url), error = function(cnd) NULL)
  if (is.null(parsed) || !tolower(parsed$scheme %||% "") %in% c("http", "https") ||
      is.null(parsed$hostname) || !nzchar(parsed$hostname)) {
    gx_abort("Only absolute HTTP(S) URLs may be requested.", "gx_error_unsafe_url")
  }
  if (!is.null(parsed$username) || !is.null(parsed$password)) {
    gx_abort("URLs containing user information are not allowed.", "gx_error_unsafe_url")
  }

  host <- tolower(sub("\\.$", "", parsed$hostname))
  if (host == "localhost" || endsWith(host, ".localhost") ||
      gx_is_nonpublic_ipv4(host) || gx_is_nonpublic_ipv6(host)) {
    gx_abort("URL resolves to a non-public network target.", "gx_error_unsafe_url")
  }
  if (grepl(":", host, fixed = TRUE)) {
    gx_abort(
      "IPv6 URL literals are not supported by the current safety policy.",
      "gx_error_unsafe_url"
    )
  }

  addresses <- if (gx_is_ipv4(host)) host else character()
  if (resolve_dns && !gx_is_ipv4(host)) {
    resolver <- getOption("geoconnexr.dns_resolver", gx_default_dns_resolver)
    if (!is.function(resolver)) {
      gx_abort("The configured DNS resolver must be a function.", "gx_error_client")
    }
    addresses <- tryCatch(resolver(host), error = function(cnd) character())
    addresses <- as.character(addresses %||% character())
    address_shape_ok <- length(addresses) && !anyNA(addresses) &&
      all(vapply(addresses, function(ip) gx_is_ipv4(ip) || grepl(":", ip, fixed = TRUE), logical(1)))
    public_ipv4 <- addresses[vapply(addresses, gx_is_ipv4, logical(1))]
    if (!address_shape_ok || !length(public_ipv4) || any(vapply(addresses, function(ip) {
      gx_is_nonpublic_ipv4(ip) || gx_is_nonpublic_ipv6(ip)
    }, logical(1)))) {
      gx_abort(
        "URL did not provide a safe public IPv4 connection target.",
        "gx_error_unsafe_url"
      )
    }
    addresses <- public_ipv4
  }
  list(
    url = gx_canonical_url(url),
    host = host,
    port = parsed$port %||% if (tolower(parsed$scheme) == "https") "443" else "80",
    addresses = addresses
  )
}

gx_assert_safe_url <- function(url, resolve_dns = TRUE) {
  invisible(gx_safe_target(url, resolve_dns = resolve_dns)$url)
}

gx_normalize_headers <- function(headers, allow_duplicates = FALSE) {
  if (is.null(headers) || !length(headers)) {
    return(character())
  }
  if (is.list(headers)) {
    values <- vapply(headers, function(x) paste(as.character(x), collapse = ", "), character(1))
  } else if (is.character(headers)) {
    values <- headers
  } else {
    gx_abort("HTTP headers must be a named list or character vector.", "gx_error_client")
  }
  if (is.null(names(values)) || any(!nzchar(names(values))) || anyNA(values)) {
    gx_abort("HTTP headers must be fully named and non-missing.", "gx_error_client")
  }
  names(values) <- tolower(names(values))
  if (anyDuplicated(names(values))) {
    if (!allow_duplicates) {
      gx_abort("HTTP header names must be unique ignoring case.", "gx_error_client")
    }
    header_names <- unique(names(values))
    values <- vapply(
      header_names,
      function(name) paste(unname(values[names(values) == name]), collapse = ", "),
      character(1)
    )
  }
  values
}

gx_header <- function(headers, name) {
  headers <- gx_normalize_headers(headers, allow_duplicates = TRUE)
  value <- unname(headers[tolower(names(headers)) == tolower(name)])
  if (length(value)) value[[1]] else NULL
}

gx_cache_key <- function(client, method, url, headers, body) {
  selected <- gx_normalize_headers(headers)
  if (length(selected)) {
    selected <- selected[order(names(selected))]
  }
  payload <- paste(
    c(
      paste0("cache-schema:", .gx_cache_schema_version),
      paste0("endpoint-base:", gx_canonical_url(client$base_url)),
      paste0("method:", method),
      paste0("url:", gx_canonical_url(url)),
      paste0(names(selected), ":", selected),
      paste0("body-sha256:", digest::digest(body, algo = "sha256", serialize = FALSE))
    ),
    collapse = "\n"
  )
  digest::digest(payload, algo = "sha256", serialize = FALSE)
}

gx_sensitive_query <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url)) return(TRUE)
  before_fragment <- strsplit(url, "#", fixed = TRUE)[[1]][[1]]
  grepl("?", before_fragment, fixed = TRUE)
}

gx_redact_url <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url) ||
      stringi::stri_detect_regex(url, "[\\p{Z}\\p{Cc}\\p{Cf}\\p{Cs}]")) {
    return("<invalid-url>")
  }
  value <- enc2utf8(url)
  fragment_at <- regexpr("#", value, fixed = TRUE)[[1]]
  has_fragment <- fragment_at > 0L
  if (has_fragment) {
    value <- substr(value, 1L, fragment_at - 1L)
  }
  query_at <- regexpr("?", value, fixed = TRUE)[[1]]
  has_query <- query_at > 0L
  if (has_query) {
    value <- substr(value, 1L, query_at - 1L)
  }
  value <- sub(
    "^([A-Za-z][A-Za-z0-9+.-]*://)[^/?#]*@",
    "\\1",
    value,
    perl = TRUE
  )
  paste0(
    value,
    if (has_query) "?[redacted]" else "",
    if (has_fragment) "#[redacted]" else ""
  )
}

gx_cache_allowed <- function(headers, url) {
  sensitive <- c("authorization", "cookie", "proxy-authorization", "range")
  !any(names(gx_normalize_headers(headers)) %in% sensitive) && !gx_sensitive_query(url)
}

gx_response_cache_allowed <- function(headers) {
  normalized <- gx_normalize_headers(headers, allow_duplicates = TRUE)
  cache_control <- tolower(gx_header(normalized, "cache-control") %||% "")
  directives <- trimws(sub(
    "=.*$", "", strsplit(cache_control, ",", fixed = TRUE)[[1]]
  ))
  pragma <- tolower(trimws(strsplit(
    gx_header(normalized, "pragma") %||% "", ",", fixed = TRUE
  )[[1]]))
  vary <- trimws(strsplit(
    gx_header(normalized, "vary") %||% "", ",", fixed = TRUE
  )[[1]])
  location <- gx_header(normalized, "location")
  location_safe <- is.null(location) || (
    is.character(location) && length(location) == 1L && nzchar(location) &&
      !gx_sensitive_query(location) && !grepl("#", location, fixed = TRUE) &&
      !grepl("^([A-Za-z][A-Za-z0-9+.-]*:)?//[^/?#]*@", location, perl = TRUE) &&
      !stringi::stri_detect_regex(location, "[\\p{Z}\\p{Cc}\\p{Cf}\\p{Cs}]")
  )
  prohibited <- c("no-store", "no-cache", "private", "must-revalidate", "proxy-revalidate")
  !any(directives %in% prohibited) &&
    is.null(gx_header(normalized, "set-cookie")) &&
    !"no-cache" %in% pragma &&
    !"*" %in% vary &&
    location_safe
}

gx_http_retry_statuses <- function() {
  c(429L, 500L, 502L, 503L, 504L)
}

gx_http_empty_attempts <- function() {
  tibble::tibble(
    request_id = character(), attempt = integer(), method = character(),
    url = character(), resolved_host = character(), resolved_ip = character(),
    status = integer(), outcome = character(),
    physical = logical(), retryable = logical(), retry_reason = character(),
    error_code = character(), retry_after = numeric(), delay = numeric(),
    throttle_delay = numeric(),
    media_type = character(), bytes = numeric(), charged_bytes = numeric(),
    body_sha256 = character(), retrieved_at = as.POSIXct(character(), tz = "UTC"),
    cache_origin = character()
  )
}

gx_http_media_type <- function(headers) {
  value <- tryCatch(
    tolower(gx_header(headers, "content-type") %||% ""),
    error = function(cnd) ""
  )
  if (!length(value) || !nzchar(value)) return(NA_character_)
  trimws(strsplit(value, ";", fixed = TRUE)[[1]][[1]])
}

gx_http_error_code <- function(cnd) {
  classes <- class(cnd)
  match <- classes[startsWith(classes, "gx_error_")]
  if (length(match)) sub("^gx_error_", "", match[[1]]) else "transport_error"
}

gx_http_attempt_row <- function(request, attempt, status = NA_integer_,
                                outcome, physical = TRUE,
                                retryable = FALSE,
                                retry_reason = NA_character_,
                                error_code = NA_character_,
                                retry_after = NA_real_, delay = NA_real_,
                                throttle_delay = request$throttle_delay %||% 0,
                                media_type = NA_character_, bytes = NA_real_,
                                charged_bytes, body_sha256 = NA_character_,
                                retrieved_at = gx_now(),
                                cache_origin = "network") {
  tibble::tibble(
    request_id = request$request_id,
    attempt = as.integer(attempt),
    method = request$method,
    url = gx_redact_url(request$url),
    resolved_host = as.character(request$resolved_host %||% NA_character_),
    resolved_ip = if (length(request$resolved_ip)) {
      as.character(request$resolved_ip[[1]])
    } else {
      NA_character_
    },
    status = as.integer(status),
    outcome = as.character(outcome),
    physical = isTRUE(physical),
    retryable = isTRUE(retryable),
    retry_reason = as.character(retry_reason),
    error_code = as.character(error_code),
    retry_after = as.numeric(retry_after),
    delay = as.numeric(delay),
    throttle_delay = as.numeric(throttle_delay),
    media_type = as.character(media_type),
    bytes = as.numeric(bytes),
    charged_bytes = as.numeric(charged_bytes),
    body_sha256 = as.character(body_sha256),
    retrieved_at = as.POSIXct(retrieved_at, tz = "UTC"),
    cache_origin = as.character(cache_origin)
  )
}

gx_http_attempt_from_response <- function(response, attempt,
                                          retryable = FALSE,
                                          retry_reason = NA_character_,
                                          retry_after = NA_real_,
                                          delay = NA_real_,
                                          physical = TRUE) {
  gx_http_attempt_row(
    response$request,
    attempt = attempt,
    status = response$status,
    outcome = if (physical) "response" else "cache",
    physical = physical,
    retryable = retryable,
    retry_reason = retry_reason,
    retry_after = retry_after,
    delay = delay,
    media_type = gx_http_media_type(response$headers),
    bytes = response$bytes,
    charged_bytes = response$bytes,
    body_sha256 = response$body_sha256,
    retrieved_at = if (physical) response$retrieved_at else gx_now(),
    cache_origin = response$cache_origin
  )
}

gx_http_attempt_from_error <- function(cnd, request, attempt,
                                       raw_response = NULL,
                                       retryable = FALSE,
                                       retry_reason = NA_character_,
                                       retry_after = NA_real_,
                                       delay = NA_real_) {
  raw_body <- if (is.list(raw_response) && is.raw(raw_response$body)) {
    raw_response$body
  } else {
    NULL
  }
  known_bytes <- if (!is.null(raw_body)) {
    length(raw_body)
  } else {
    candidate <- cnd$gx_bytes %||% NA_real_
    if (is.numeric(candidate) && length(candidate) == 1L &&
        !is.na(candidate) && is.finite(candidate) && candidate >= 0) {
      as.numeric(candidate)
    } else {
      NA_real_
    }
  }
  charged <- if (is.na(known_bytes)) request$max_bytes else known_bytes
  status <- if (is.list(raw_response) && is.numeric(raw_response$status) &&
      length(raw_response$status) == 1L && !is.na(raw_response$status) &&
      is.finite(raw_response$status)) {
    as.integer(raw_response$status)
  } else if (is.numeric(cnd$status) && length(cnd$status) == 1L &&
      !is.na(cnd$status) && is.finite(cnd$status)) {
    as.integer(cnd$status)
  } else {
    NA_integer_
  }
  headers <- if (is.list(raw_response)) raw_response$headers %||% list() else list()
  gx_http_attempt_row(
    request,
    attempt = attempt,
    status = status,
    outcome = if (retryable) "transport_error" else "policy_error",
    retryable = retryable,
    retry_reason = retry_reason,
    error_code = gx_http_error_code(cnd),
    retry_after = retry_after,
    delay = delay,
    media_type = gx_http_media_type(headers),
    bytes = known_bytes,
    charged_bytes = charged,
    body_sha256 = if (is.null(raw_body)) {
      NA_character_
    } else {
      digest::digest(raw_body, algo = "sha256", serialize = FALSE)
    },
    cache_origin = if (retryable) "network_error" else "network_rejected"
  )
}

gx_http_attempt_request_row <- function(attempt) {
  required <- names(gx_http_empty_attempts())
  if (!is.data.frame(attempt) || nrow(attempt) != 1L ||
      !identical(names(attempt), required)) {
    gx_abort("HTTP attempt metadata violated its internal contract.", "gx_error_client")
  }
  tibble::tibble(
    request_id = attempt$request_id,
    method = attempt$method,
    url = attempt$url,
    status = attempt$status,
    media_type = attempt$media_type,
    bytes = as.integer(attempt$charged_bytes),
    body_sha256 = attempt$body_sha256,
    retrieved_at = attempt$retrieved_at,
    cache_origin = attempt$cache_origin
  )
}

gx_http_attach_attempts <- function(cnd, attempts,
                                    retry_exhausted = FALSE,
                                    retry_stopped = NA_character_,
                                    status = NULL) {
  cnd$attempts <- attempts
  cnd$attempt_count <- as.integer(nrow(attempts))
  cnd$retry_exhausted <- isTRUE(retry_exhausted)
  cnd$retry_stopped <- as.character(retry_stopped)
  if (!is.null(status)) cnd$status <- as.integer(status)
  stop(cnd)
}

gx_http_attempt_control <- function(control) {
  if (is.null(control)) return(NULL)
  valid <- is.list(control) && identical(names(control), c("before", "after")) &&
    is.function(control$before) && is.function(control$after)
  if (!valid) {
    gx_abort("Internal HTTP attempt control is invalid.", "gx_error_client")
  }
  control
}

gx_http_attempt_begin <- function(control, request, physical) {
  if (is.null(control)) return(request$max_bytes)
  ceiling <- control$before(request = request, physical = physical)
  if (is.null(ceiling)) return(request$max_bytes)
  ceiling <- gx_scalar_number(
    ceiling,
    "attempt byte ceiling",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  as.integer(min(request$max_bytes, ceiling))
}

gx_http_attempt_record <- function(control, attempt) {
  if (!is.null(control)) control$after(attempt)
  invisible(NULL)
}

gx_http_parse_date <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  day <- "(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)"
  weekday <- "(?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)"
  month <- "(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
  time <- "[0-9]{2}:[0-9]{2}:[0-9]{2}"
  valid_syntax <- grepl(
    paste0(
      "^(?:", day, ", [0-9]{2} ", month, " [0-9]{4} ", time, " GMT|",
      weekday, ", [0-9]{2}-", month, "-[0-9]{2} ", time, " GMT|",
      day, " ", month, " (?: [0-9]|[0-9]{2}) ", time, " [0-9]{4})$"
    ),
    trimws(value),
    perl = TRUE
  )
  if (!valid_syntax) return(as.POSIXct(NA, tz = "UTC"))
  parsed <- suppressWarnings(tryCatch(
    curl::parse_date(trimws(value)),
    error = function(cnd) as.POSIXct(NA, tz = "UTC")
  ))
  parsed <- as.POSIXct(parsed, tz = "UTC")
  if (length(parsed) != 1L || is.na(parsed)) {
    as.POSIXct(NA, tz = "UTC")
  } else {
    parsed
  }
}

gx_http_retry_after <- function(response) {
  value <- gx_header(response$headers, "retry-after")
  if (is.null(value)) return(NA_real_)
  value <- trimws(value)
  if (grepl("^[0-9]+$", value)) {
    seconds <- suppressWarnings(as.numeric(value))
    if (length(seconds) == 1L && !is.na(seconds)) return(seconds)
    return(Inf)
  }
  retry_time <- gx_http_parse_date(value)
  if (is.na(retry_time)) return(Inf)
  response_time <- gx_http_parse_date(gx_header(response$headers, "date") %||% "")
  if (is.na(response_time)) response_time <- gx_now()
  max(0, as.numeric(difftime(retry_time, response_time, units = "secs")))
}

gx_http_retry_policy <- function(response = NULL, retry_number) {
  max_delay <- gx_scalar_number(
    getOption("geoconnexr.retry_max_delay", 60),
    "geoconnexr.retry_max_delay",
    minimum = 0,
    maximum = 3600,
    integer = FALSE
  )
  retry_after <- if (is.null(response)) NA_real_ else gx_http_retry_after(response)
  if (!is.na(retry_after) && retry_after > max_delay) {
    return(list(
      retry = FALSE,
      retry_after = retry_after,
      delay = NA_real_,
      stopped = "retry_after_exceeds_limit"
    ))
  }
  exponent <- min(max(as.integer(retry_number) - 1L, 0L), 30L)
  jitter_max <- min(max_delay, 2^exponent)
  jitter <- getOption(
    "geoconnexr.retry_jitter",
    function(max_seconds) {
      if (max_seconds <= 0) 0 else stats::runif(1L, min = 0, max = max_seconds)
    }
  )
  if (!is.function(jitter)) {
    gx_abort("The configured retry jitter must be a function.", "gx_error_client")
  }
  value <- tryCatch(
    jitter(jitter_max),
    error = function(cnd) {
      gx_abort(
        "The configured retry jitter failed; underlying details were withheld.",
        "gx_error_client",
        .redact_trace = TRUE
      )
    }
  )
  valid <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.finite(value) && value >= 0 && value <= jitter_max
  if (!valid) {
    gx_abort(
      "The configured retry jitter returned an invalid delay.",
      "gx_error_client"
    )
  }
  delay <- if (is.na(retry_after)) value else max(retry_after, value)
  list(retry = TRUE, retry_after = retry_after, delay = delay, stopped = NA_character_)
}

gx_http_retry_sleep <- function(seconds) {
  if (seconds <= 0) return(invisible(NULL))
  sleeper <- getOption("geoconnexr.retry_sleep", Sys.sleep)
  if (!is.function(sleeper)) {
    gx_abort("The configured retry sleeper must be a function.", "gx_error_client")
  }
  tryCatch(
    sleeper(seconds),
    error = function(cnd) {
      gx_abort(
        "The configured retry sleeper failed; underlying details were withheld.",
        "gx_error_client",
        .redact_trace = TRUE
      )
    }
  )
  invisible(NULL)
}

.gx_http_throttle_state <- new.env(parent = emptyenv())

gx_http_throttle_reset <- function() {
  rm(list = ls(.gx_http_throttle_state, all.names = TRUE),
     envir = .gx_http_throttle_state)
  invisible(NULL)
}

gx_http_throttle_now <- function() {
  clock <- getOption(
    "geoconnexr.throttle_clock",
    function() unname(proc.time()[["elapsed"]])
  )
  if (!is.function(clock)) {
    gx_abort("The configured throttle clock must be a function.", "gx_error_client")
  }
  value <- tryCatch(
    clock(),
    error = function(cnd) {
      gx_abort(
        "The configured throttle clock failed; underlying details were withheld.",
        "gx_error_client",
        .redact_trace = TRUE
      )
    }
  )
  valid <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.finite(value) && value >= 0
  if (!valid) {
    gx_abort("The configured throttle clock returned an invalid time.", "gx_error_client")
  }
  as.double(value)
}

gx_http_throttle_sleep <- function(seconds) {
  if (!is.numeric(seconds) || length(seconds) != 1L || is.na(seconds) ||
      !is.finite(seconds) || seconds < 0) {
    gx_abort("Internal throttle delay is invalid.", "gx_error_client")
  }
  if (seconds <= 0) return(invisible(NULL))
  sleeper <- getOption("geoconnexr.throttle_sleep", Sys.sleep)
  if (!is.function(sleeper)) {
    gx_abort("The configured throttle sleeper must be a function.", "gx_error_client")
  }
  tryCatch(
    sleeper(seconds),
    error = function(cnd) {
      gx_abort(
        "The configured throttle sleeper failed; underlying details were withheld.",
        "gx_error_client",
        .redact_trace = TRUE
      )
    }
  )
  invisible(NULL)
}

gx_http_throttle_wait <- function(host, min_interval) {
  if (!is.numeric(min_interval) || length(min_interval) != 1L ||
      is.na(min_interval) || !is.finite(min_interval) || min_interval < 0) {
    gx_abort("Internal per-host throttle interval is invalid.", "gx_error_client")
  }
  if (!is.character(host) || length(host) != 1L || is.na(host) || !nzchar(host)) {
    gx_abort("Internal throttle hostname is invalid.", "gx_error_client")
  }
  pid <- as.character(Sys.getpid())
  state_pid <- get0(".pid", envir = .gx_http_throttle_state, inherits = FALSE)
  if (!identical(state_pid, pid)) {
    gx_http_throttle_reset()
    assign(".pid", pid, envir = .gx_http_throttle_state)
  }
  key <- paste0("host:", tolower(enc2utf8(host)))
  default_timing <- is.null(getOption("geoconnexr.throttle_clock")) &&
    is.null(getOption("geoconnexr.throttle_sleep"))
  started <- gx_http_throttle_now()
  current <- started
  prior <- get0(key, envir = .gx_http_throttle_state, inherits = FALSE)
  if (!is.null(prior)) {
    valid_prior <- is.list(prior) && identical(names(prior), c("started", "interval")) &&
      is.numeric(prior$started) && length(prior$started) == 1L &&
      !is.na(prior$started) && is.finite(prior$started) && prior$started >= 0 &&
      is.numeric(prior$interval) && length(prior$interval) == 1L &&
      !is.na(prior$interval) && is.finite(prior$interval) && prior$interval >= 0
    if (!valid_prior) {
      gx_abort("Internal per-host throttle state is invalid.", "gx_error_client")
    }
    next_at <- prior$started + max(prior$interval, min_interval)
    if (!is.finite(next_at)) {
      gx_abort("Internal per-host throttle state overflowed.", "gx_error_client")
    }
    tolerance <- 1e-9
    while (current + tolerance < next_at) {
      remaining <- next_at - current
      gx_http_throttle_sleep(remaining)
      updated <- gx_http_throttle_now()
      if (updated <= current) {
        if (!default_timing) {
          gx_abort(
            "The configured throttle sleeper did not advance its monotonic clock.",
            "gx_error_client"
          )
        }
        # proc.time() has platform-dependent resolution. Sys.sleep() completed,
        # so advance the internal reservation to the requested boundary when
        # the package defaults cannot observe a very small positive delay.
        current <- next_at
        break
      }
      current <- updated
    }
  }
  assign(
    key,
    list(started = current, interval = as.double(min_interval)),
    envir = .gx_http_throttle_state
  )
  max(0, current - started)
}

gx_default_performer <- function(request) {
  req <- httr2::request(request$url)
  req <- httr2::req_method(req, request$method)
  if (length(request$headers)) {
    req <- do.call(httr2::req_headers, c(list(req), as.list(request$headers)))
  }
  req <- httr2::req_user_agent(req, request$user_agent)
  req <- httr2::req_timeout(req, seconds = request$timeout)
  curl_options <- list(
    followlocation = FALSE,
    maxredirs = 0,
    maxfilesize = request$max_bytes,
    http_content_decoding = FALSE,
    noproxy = "*"
  )
  if (length(request$resolved_ip)) {
    curl_options$resolve <- paste(
      request$resolved_host,
      request$resolved_port,
      request$resolved_ip[[1]],
      sep = ":"
    )
  }
  req <- do.call(httr2::req_options, c(list(req), curl_options))
  if (length(request$body)) {
    req <- httr2::req_body_raw(
      req,
      body = request$body,
      type = gx_header(request$headers, "content-type") %||% "application/octet-stream"
    )
  }
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  response <- httr2::req_perform_connection(req)
  on.exit(close(response), add = TRUE)
  response_headers <- as.list(httr2::resp_headers(response))
  content_encoding <- tolower(gx_header(response_headers, "content-encoding") %||% "identity")
  if (!identical(request$method, "HEAD") &&
      !content_encoding %in% c("", "identity")) {
    gx_abort(
      "Compressed HTTP responses are rejected until bounded streaming decompression is available.",
      "gx_error_content_encoding"
    )
  }
  content_length <- suppressWarnings(
    as.numeric(gx_header(response_headers, "content-length") %||% NA)
  )
  if (!identical(request$method, "HEAD") &&
      !is.na(content_length) && content_length > request$max_bytes) {
    gx_abort(
      "Response exceeded the configured {format(request$max_bytes, big.mark = ',')} byte ceiling.",
      "gx_error_payload_too_large"
    )
  }

  chunks <- list()
  total <- 0
  chunk_kb <- max(1, min(64, ceiling(request$max_bytes / 1024)))
  while (!httr2::resp_stream_is_complete(response)) {
    chunk <- httr2::resp_stream_raw(response, kb = chunk_kb)
    if (!length(chunk)) {
      next
    }
    next_total <- total + as.double(length(chunk))
    if (!is.finite(next_total) || next_total > request$max_bytes) {
      gx_abort(
        "Response exceeded the configured {format(request$max_bytes, big.mark = ',')} byte ceiling.",
        "gx_error_payload_too_large"
      )
    }
    total <- next_total
    chunks[[length(chunks) + 1L]] <- chunk
  }
  body <- if (length(chunks)) do.call(c, chunks) else raw()
  list(
    status = httr2::resp_status(response),
    headers = response_headers,
    body = body,
    url = response$url %||% request$url
  )
}

gx_validate_response <- function(response, request, client) {
  valid <- is.list(response) && is.numeric(response$status) &&
    length(response$status) == 1L && !is.na(response$status) &&
    response$status == trunc(response$status) &&
    response$status >= 100 && response$status <= 599 &&
    is.raw(response$body) && is.character(response$url) &&
    length(response$url) == 1L && !is.na(response$url)
  if (!valid) {
    gx_abort("The HTTP performer returned an invalid response contract.", "gx_error_client")
  }
  headers <- gx_normalize_headers(response$headers %||% list(), allow_duplicates = TRUE)
  content_encoding <- tolower(gx_header(headers, "content-encoding") %||% "identity")
  if (!identical(request$method, "HEAD") &&
      !content_encoding %in% c("", "identity")) {
    gx_abort(
      "Compressed HTTP responses are rejected until bounded streaming decompression is available.",
      "gx_error_content_encoding"
    )
  }
  content_length <- suppressWarnings(as.numeric(gx_header(headers, "content-length") %||% NA))
  if ((!identical(request$method, "HEAD") &&
       !is.na(content_length) && content_length > request$max_bytes) ||
      length(response$body) > request$max_bytes) {
    gx_abort(
      "Response exceeded the configured {format(request$max_bytes, big.mark = ',')} byte ceiling.",
      "gx_error_payload_too_large"
    )
  }
  gx_assert_safe_url(response$url, resolve_dns = FALSE)
  if (!identical(gx_canonical_url(response$url), request$url)) {
    gx_abort("HTTP performer changed URL despite redirects being disabled.", "gx_error_redirect")
  }
  retrieved_at <- gx_now()
  list(
    status = as.integer(response$status),
    headers = headers,
    body = response$body,
    url = gx_canonical_url(response$url),
    retrieved_at = retrieved_at,
    body_sha256 = digest::digest(response$body, algo = "sha256", serialize = FALSE),
    bytes = length(response$body),
    from_cache = FALSE,
    cache_origin = "network",
    request = request
  )
}

gx_cache_read <- function(backend, key, client, request) {
  exists <- tryCatch(backend$exists(key), error = function(cnd) FALSE)
  if (!exists) {
    return(NULL)
  }
  entry <- tryCatch(backend$get(key), error = function(cnd) NULL)
  max_age <- getOption("geoconnexr.cache_max_age", 24 * 60 * 60)
  max_age_valid <- is.numeric(max_age) && length(max_age) == 1L &&
    !is.na(max_age) && max_age >= 0
  valid <- tryCatch({
    response <- entry$response
    headers <- gx_normalize_headers(response$headers, allow_duplicates = TRUE)
    now <- gx_now()
    age <- as.numeric(difftime(now, response$retrieved_at, units = "secs"))
    content_encoding <- tolower(gx_header(headers, "content-encoding") %||% "identity")
    content_length <- suppressWarnings(as.numeric(gx_header(headers, "content-length") %||% NA))
    is.list(entry) &&
      identical(entry$cache_schema_version, .gx_cache_schema_version) &&
      identical(entry$request_id, key) &&
      is.list(response) &&
      is.numeric(response$status) && length(response$status) == 1L &&
      response$status >= 200L && response$status < 400L &&
      is.raw(response$body) &&
      is.character(response$url) && length(response$url) == 1L &&
      identical(gx_canonical_url(response$url), request$url) &&
      inherits(response$retrieved_at, "POSIXct") && length(response$retrieved_at) == 1L &&
      is.numeric(response$bytes) && identical(as.integer(response$bytes), length(response$body)) &&
      identical(
        response$body_sha256,
        digest::digest(response$body, algo = "sha256", serialize = FALSE)
      ) &&
      length(response$body) <= client$max_bytes &&
      (identical(request$method, "HEAD") ||
       is.na(content_length) || content_length <= client$max_bytes) &&
      (identical(request$method, "HEAD") || content_encoding %in% c("", "identity")) &&
      gx_response_cache_allowed(headers) &&
      max_age_valid && !is.na(age) && age >= 0 && age <= max_age
  }, error = function(cnd) FALSE)
  if (!valid) {
    try(backend$remove(key), silent = TRUE)
    return(NULL)
  }
  entry$response
}

gx_validate_endpoint_response <- function(response, client, check_status) {
  if (identical(client$endpoint, "graph")) {
    if (response$status >= 300L && response$status < 400L) {
      gx_abort("Graph requests must not follow redirects.", "gx_error_redirect")
    }
    if (response$status >= 200L && response$status < 300L) {
      media_type <- gx_http_media_type(response$headers)
      if (!identical(media_type, "application/sparql-results+json")) {
        gx_abort(
          "Graph endpoint did not return SPARQL Results JSON.",
          "gx_error_content_type"
        )
      }
    }
  }
  if (check_status && (response$status < 200L || response$status >= 300L)) {
    gx_abort(
      "HTTP request failed with status {response$status}.",
      "gx_error_http"
    )
  }
  invisible(response)
}

gx_http_request <- function(client,
                            method = "GET",
                            url = client$base_url,
                            headers = list(),
                            body = NULL,
                            content_type = NULL,
                            check_status = TRUE,
                            .attempt_control = NULL,
                            .response_validator = NULL) {
  if (!inherits(client, "gx_client")) {
    gx_abort("{.arg client} must be created by {.fn gx_client}.", "gx_error_client")
  }
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    gx_abort("{.arg method} must be one HTTP method.", "gx_error_client")
  }
  method <- toupper(method)
  if (!method %in% c("GET", "HEAD", "POST")) {
    gx_abort("Only GET, HEAD, and POST are supported.", "gx_error_client")
  }
  if (identical(client$endpoint, "graph") && !identical(method, "POST")) {
    gx_abort("Graph clients accept POST requests only.", "gx_error_client")
  }
  if (!is.logical(check_status) || length(check_status) != 1L || is.na(check_status)) {
    gx_abort("{.arg check_status} must be one logical value.", "gx_error_client")
  }
  control <- gx_http_attempt_control(.attempt_control)
  if (!is.null(.response_validator) && !is.function(.response_validator)) {
    gx_abort("Internal HTTP response validator is invalid.", "gx_error_client")
  }
  response_validator <- .response_validator
  target <- gx_safe_target(url, resolve_dns = FALSE)
  headers <- gx_normalize_headers(headers)
  if (!is.null(content_type)) {
    if (!is.character(content_type) || length(content_type) != 1L || is.na(content_type)) {
      gx_abort("{.arg content_type} must be one string.", "gx_error_client")
    }
    headers["content-type"] <- content_type
  }
  if (!"accept-encoding" %in% names(headers)) {
    headers["accept-encoding"] <- "identity"
  } else if (!identical(tolower(headers[["accept-encoding"]]), "identity")) {
    gx_abort(
      "Only {.code Accept-Encoding: identity} is allowed until bounded decompression is available.",
      "gx_error_content_encoding"
    )
  }
  if (is.null(body)) {
    body <- raw()
  } else if (is.character(body) && length(body) == 1L && !is.na(body)) {
    body <- charToRaw(enc2utf8(body))
  } else if (!is.raw(body)) {
    gx_abort("{.arg body} must be NULL, raw, or one string.", "gx_error_client")
  }

  key <- gx_cache_key(client, method, url, headers, body)
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
  backend <- if (client$cache && gx_cache_allowed(headers, target$url)) {
    gx_cache_backend(client$cache_dir)
  } else {
    NULL
  }
  if (!is.null(backend)) {
    cached <- gx_cache_read(backend, key, client, request)
    if (!is.null(cached)) {
      cached$from_cache <- TRUE
      cached$cache_origin <- if (client$offline) "offline_cache" else "fresh_cache"
      cached$request <- request
      attempts <- gx_http_empty_attempts()
      tryCatch(
        gx_http_attempt_begin(control, request, physical = FALSE),
        error = function(cnd) gx_http_attach_attempts(cnd, attempts)
      )
      cache_row <- gx_http_attempt_from_response(
        cached,
        attempt = NA_integer_,
        physical = FALSE
      )
      tryCatch(
        gx_http_attempt_record(control, cache_row),
        error = function(cnd) gx_http_attach_attempts(cnd, attempts)
      )
      cached$attempts <- attempts
      cached$attempt_count <- 0L
      cached$retry_exhausted <- FALSE
      cached$retry_stopped <- NA_character_
      validation_error <- tryCatch({
        gx_validate_endpoint_response(cached, client, check_status)
        if (!is.null(response_validator)) response_validator(cached)
        NULL
      }, error = identity)
      if (is.null(validation_error)) return(cached)

      cache_invalid <- isTRUE(validation_error$cache_invalid) ||
        inherits(validation_error, c("gx_error_content_type", "gx_error_redirect"))
      if (cache_invalid) {
        try(backend$remove(key), silent = TRUE)
      }
      if (client$offline || !cache_invalid) {
        gx_http_attach_attempts(
          validation_error,
          attempts,
          status = cached$status
        )
      }
    }
  }
  if (client$offline) {
    gx_abort(
      "Offline cache miss for {method} {.url {gx_redact_url(gx_canonical_url(url))}}.",
      "gx_error_offline_miss"
    )
  }

  performer <- getOption("geoconnexr.performer", gx_default_performer)
  if (!is.function(performer)) {
    gx_abort("The configured HTTP performer must be a function.", "gx_error_client")
  }
  attempts <- gx_http_empty_attempts()
  attempt_index <- 0
  max_attempts <- as.double(client$retries) + 1
  record_attempt <- function(row) {
    attempts <<- rbind(attempts, row)
    tryCatch(
      gx_http_attempt_record(control, row),
      error = function(cnd) gx_http_attach_attempts(cnd, attempts)
    )
    invisible(NULL)
  }

  repeat {
    attempt_index <- attempt_index + 1
    attempt_request <- request
    attempt_request$max_bytes <- tryCatch(
      gx_http_attempt_begin(control, request, physical = TRUE),
      error = function(cnd) gx_http_attach_attempts(cnd, attempts)
    )
    attempt_request$throttle_delay <- tryCatch(
      gx_http_throttle_wait(
        attempt_request$resolved_host,
        client$min_interval %||% 0
      ),
      error = function(cnd) {
        gx_http_attach_attempts(
          cnd,
          attempts,
          retry_stopped = "throttle_error"
        )
      }
    )
    resolved <- tryCatch(
      gx_safe_target(request$url, resolve_dns = TRUE),
      error = function(cnd) {
        rejected <- gx_http_attempt_row(
          attempt_request,
          attempt = attempt_index,
          outcome = "policy_error",
          physical = TRUE,
          retryable = FALSE,
          error_code = gx_http_error_code(cnd),
          bytes = 0,
          charged_bytes = 0,
          cache_origin = "network_rejected"
        )
        record_attempt(rejected)
        gx_http_attach_attempts(
          cnd,
          attempts,
          retry_stopped = "unsafe_target"
        )
      }
    )
    attempt_request$url <- resolved$url
    attempt_request$resolved_host <- resolved$host
    attempt_request$resolved_port <- resolved$port
    attempt_request$resolved_ip <- resolved$addresses

    performed <- tryCatch(
      list(
        response = withCallingHandlers(
          performer(attempt_request),
          warning = function(cnd) invokeRestart("muffleWarning")
        ),
        error = NULL
      ),
      error = function(cnd) list(response = NULL, error = cnd)
    )

    if (!is.null(performed$error)) {
      cnd <- performed$error
      transport_failure <- !inherits(cnd, "gx_error") ||
        inherits(cnd, "gx_error_transport")
      has_retry <- transport_failure && attempt_index < max_attempts
      policy <- list(
        retry = FALSE,
        retry_after = NA_real_,
        delay = NA_real_,
        stopped = if (transport_failure) "retry_exhausted" else "policy_error"
      )
      policy_error <- NULL
      if (has_retry) {
        candidate <- tryCatch(
          gx_http_retry_policy(retry_number = attempt_index),
          error = identity
        )
        if (inherits(candidate, "error")) {
          policy_error <- candidate
          policy$stopped <- "retry_policy_error"
        } else {
          policy <- candidate
        }
      }
      row <- gx_http_attempt_from_error(
        cnd,
        attempt_request,
        attempt = attempt_index,
        retryable = transport_failure,
        retry_reason = if (transport_failure) "transport_error" else "policy_error",
        retry_after = policy$retry_after,
        delay = policy$delay
      )
      record_attempt(row)
      if (!is.null(policy_error)) {
        gx_http_attach_attempts(
          policy_error,
          attempts,
          retry_stopped = policy$stopped
        )
      }
      if (!transport_failure) {
        gx_http_attach_attempts(
          cnd,
          attempts,
          retry_stopped = policy$stopped,
          status = row$status[[1]]
        )
      }
      if (isTRUE(policy$retry)) {
        tryCatch(
          gx_http_retry_sleep(policy$delay),
          error = function(sleep_cnd) {
            gx_http_attach_attempts(
              sleep_cnd,
              attempts,
              retry_stopped = "retry_sleep_error"
            )
          }
        )
        next
      }
      gx_abort(
        "HTTP transport failed; underlying details were withheld.",
        "gx_error_transport",
        attempts = attempts,
        attempt_count = as.integer(nrow(attempts)),
        retry_exhausted = !has_retry,
        retry_stopped = policy$stopped,
        .redact_trace = TRUE
      )
    }

    raw_response <- performed$response
    response <- tryCatch(
      gx_validate_response(raw_response, attempt_request, client),
      error = identity
    )
    if (inherits(response, "error")) {
      row <- gx_http_attempt_from_error(
        response,
        attempt_request,
        attempt = attempt_index,
        raw_response = raw_response,
        retry_reason = "response_rejected"
      )
      record_attempt(row)
      gx_http_attach_attempts(
        response,
        attempts,
        retry_stopped = "response_rejected",
        status = row$status[[1]]
      )
    }

    retryable <- response$status %in% gx_http_retry_statuses()
    has_retry <- retryable && attempt_index < max_attempts
    policy <- list(
      retry = FALSE,
      retry_after = NA_real_,
      delay = NA_real_,
      stopped = if (retryable) "retry_exhausted" else NA_character_
    )
    policy_error <- NULL
    if (has_retry) {
      candidate <- tryCatch(
        gx_http_retry_policy(response, retry_number = attempt_index),
        error = identity
      )
      if (inherits(candidate, "error")) {
        policy_error <- candidate
        policy$stopped <- "retry_policy_error"
      } else {
        policy <- candidate
      }
    }
    row <- gx_http_attempt_from_response(
      response,
      attempt = attempt_index,
      retryable = retryable,
      retry_reason = if (retryable) paste0("http_", response$status) else NA_character_,
      retry_after = policy$retry_after,
      delay = policy$delay
    )
    record_attempt(row)
    if (!is.null(policy_error)) {
      gx_http_attach_attempts(
        policy_error,
        attempts,
        retry_stopped = policy$stopped,
        status = response$status
      )
    }
    if (isTRUE(policy$retry)) {
      tryCatch(
        gx_http_retry_sleep(policy$delay),
        error = function(cnd) {
          gx_http_attach_attempts(
            cnd,
            attempts,
            retry_stopped = "retry_sleep_error",
            status = response$status
          )
        }
      )
      next
    }

    response$attempts <- attempts
    response$attempt_count <- as.integer(nrow(attempts))
    response$retry_exhausted <- retryable && !has_retry
    response$retry_stopped <- policy$stopped
    tryCatch({
      gx_validate_endpoint_response(response, client, check_status)
      if (!is.null(response_validator)) response_validator(response)
    },
      error = function(cnd) {
        gx_http_attach_attempts(
          cnd,
          attempts,
          retry_exhausted = response$retry_exhausted,
          retry_stopped = response$retry_stopped,
          status = response$status
        )
      }
    )

    cacheable <- response$status >= 200L && response$status < 400L &&
      gx_response_cache_allowed(response$headers)
    if (!is.null(backend) && cacheable) {
      cached_response <- response
      cached_response$request <- NULL
      cached_response$attempts <- NULL
      cached_response$attempt_count <- NULL
      cached_response$retry_exhausted <- NULL
      cached_response$retry_stopped <- NULL
      backend$set(key, list(
        cache_schema_version = .gx_cache_schema_version,
        request_id = key,
        response = cached_response
      ))
    }
    return(response)
  }
}

gx_default_file_performer <- function(request, path) {
  handle <- curl::new_handle()
  options <- list(
    followlocation = FALSE,
    maxredirs = 0L,
    maxfilesize = as.double(request$max_bytes),
    http_content_decoding = FALSE,
    noproxy = "*",
    timeout = request$timeout,
    connecttimeout = min(request$timeout, 30),
    useragent = request$user_agent,
    failonerror = FALSE
  )
  if (length(request$headers)) {
    options$httpheader <- paste0(
      names(request$headers), ": ", unname(request$headers)
    )
  }
  if (length(request$resolved_ip)) {
    options$resolve <- paste(
      request$resolved_host,
      request$resolved_port,
      request$resolved_ip[[1]],
      sep = ":"
    )
  }
  do.call(curl::handle_setopt, c(list(handle = handle), options))
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  transferred <- 0
  overflow <- FALSE
  response <- tryCatch(
    withCallingHandlers(
      curl::curl_fetch_stream(
        request$url,
        function(chunk) {
          next_total <- transferred + length(chunk)
          if (!is.finite(next_total) || next_total > request$max_bytes) {
            overflow <<- TRUE
            stop("bounded file stream exceeded", call. = FALSE)
          }
          writeBin(chunk, connection, useBytes = TRUE)
          transferred <<- next_total
          invisible(NULL)
        },
        handle = handle
      ),
      warning = function(cnd) invokeRestart("muffleWarning")
    ),
    error = function(cnd) {
      if (overflow) {
        gx_abort(
          "The file response exceeded its hard streaming byte ceiling.",
          "gx_error_download",
          .redact_trace = TRUE
        )
      }
      if (inherits(cnd, "gx_error")) stop(cnd)
      gx_abort(
        "File transport failed; underlying details were withheld.",
        "gx_error_download",
        .redact_trace = TRUE
      )
    }
  )
  list(
    status = response$status_code,
    headers = curl::parse_headers_list(response$headers),
    url = response$url,
    path = path
  )
}

gx_validate_file_response <- function(response, request, path, max_bytes) {
  valid <- is.list(response) && is.numeric(response$status) &&
    length(response$status) == 1L && !is.na(response$status) &&
    response$status == trunc(response$status) &&
    response$status >= 100 && response$status <= 599 &&
    is.character(response$url) && length(response$url) == 1L &&
    !is.na(response$url)
  if (!valid) {
    gx_abort(
      "The file performer returned an invalid response contract.",
      "gx_error_download",
      .redact_trace = TRUE
    )
  }
  headers <- tryCatch(
    gx_normalize_headers(response$headers %||% list(), allow_duplicates = TRUE),
    gx_error = function(cnd) {
      gx_abort(
        "The file response contained invalid headers.",
        "gx_error_download",
        .redact_trace = TRUE
      )
    }
  )
  regular <- file.exists(path) && !dir.exists(path) &&
    !gx_path_is_symlink(path)
  info <- if (regular) file.info(path) else NULL
  bytes <- if (!is.null(info)) as.double(info$size[[1]]) else NA_real_
  if (!regular || !is.finite(bytes) || bytes < 0 || bytes > max_bytes) {
    gx_abort(
      "The downloaded file is missing, invalid, or exceeds its byte ceiling.",
      "gx_error_download",
      .redact_trace = TRUE
    )
  }
  content_encoding <- tolower(gx_header(headers, "content-encoding") %||% "identity")
  if (!content_encoding %in% c("", "identity")) {
    gx_abort(
      "Encoded file responses are not allowed by this download contract.",
      "gx_error_download",
      .redact_trace = TRUE
    )
  }
  length_header <- gx_header(headers, "content-length")
  if (!is.null(length_header)) {
    content_length <- suppressWarnings(as.double(length_header))
    if (length(content_length) != 1L || !is.finite(content_length) ||
        content_length < 0 || content_length != trunc(content_length) ||
        content_length != bytes) {
      gx_abort(
        "The file response Content-Length did not match the transferred bytes.",
        "gx_error_download",
        .redact_trace = TRUE
      )
    }
  }
  response_url <- tryCatch(
    gx_canonical_url(response$url),
    gx_error = function(cnd) NA_character_
  )
  if (is.na(response_url) || !identical(response_url, request$url)) {
    gx_abort(
      "The file performer changed URL despite redirects being disabled.",
      "gx_error_download",
      .redact_trace = TRUE
    )
  }
  list(
    status = as.integer(response$status),
    headers = headers,
    url = response_url,
    bytes = bytes,
    retrieved_at = gx_now(),
    request = request
  )
}

gx_http_download_file <- function(client, url, path, headers = list(),
                                  max_bytes = client$max_bytes,
                                  check_status = TRUE) {
  if (!inherits(client, "gx_client") || identical(client$endpoint, "graph")) {
    gx_abort(
      "{.arg client} must be a non-graph client created by {.fn gx_client}.",
      "gx_error_client"
    )
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !dir.exists(dirname(path)) || file.exists(path) ||
      dir.exists(path) || gx_path_is_symlink(path) ||
      gx_path_is_symlink(dirname(path))) {
    gx_abort(
      "{.arg path} must be a new file in an existing directory.",
      "gx_error_download",
      .redact_trace = TRUE
    )
  }
  max_bytes <- gx_scalar_number(
    max_bytes,
    "max_bytes",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  if (!is.logical(check_status) || length(check_status) != 1L ||
      is.na(check_status)) {
    gx_abort(
      "{.arg check_status} must be one non-missing logical value.",
      "gx_error_client"
    )
  }
  if (client$offline) {
    gx_abort(
      "Offline mode cannot satisfy an uncached file download.",
      "gx_error_offline_miss",
      .redact_trace = TRUE
    )
  }
  target <- tryCatch(
    gx_safe_target(url, resolve_dns = FALSE),
    gx_error = function(cnd) {
      gx_abort(
        "The file download target failed safety validation.",
        "gx_error_download",
        .redact_trace = TRUE
      )
    }
  )
  throttle_delay <- gx_http_throttle_wait(
    target$host,
    client$min_interval %||% 0
  )
  target <- tryCatch(
    gx_safe_target(target$url, resolve_dns = TRUE),
    gx_error = function(cnd) {
      gx_abort(
        "The file download target failed safety validation.",
        "gx_error_download",
        .redact_trace = TRUE
      )
    }
  )
  headers <- gx_normalize_headers(headers)
  if (!"accept-encoding" %in% names(headers)) {
    headers["accept-encoding"] <- "identity"
  } else if (!identical(tolower(headers[["accept-encoding"]]), "identity")) {
    gx_abort(
      "File downloads require {.code Accept-Encoding: identity}.",
      "gx_error_download",
      .redact_trace = TRUE
    )
  }
  request <- list(
    method = "GET",
    url = target$url,
    headers = headers,
    timeout = client$timeout,
    max_bytes = max_bytes,
    user_agent = client$user_agent,
    request_id = gx_cache_key(client, "GET", target$url, headers, raw()),
    resolved_host = target$host,
    resolved_port = target$port,
    resolved_ip = target$addresses,
    throttle_delay = throttle_delay
  )
  performer <- getOption("geoconnexr.file_performer", gx_default_file_performer)
  if (!is.function(performer)) {
    gx_abort(
      "The configured file performer must be a function.",
      "gx_error_client"
    )
  }
  keep <- FALSE
  on.exit(if (!keep && file.exists(path)) unlink(path, force = TRUE), add = TRUE)
  raw_response <- tryCatch(
    performer(request, path),
    error = function(cnd) {
      if (inherits(cnd, "gx_error")) stop(cnd)
      gx_abort(
        "File transport failed; underlying details were withheld.",
        "gx_error_download",
        .redact_trace = TRUE
      )
    }
  )
  response <- gx_validate_file_response(raw_response, request, path, max_bytes)
  if (check_status && (response$status < 200L || response$status >= 300L)) {
    gx_abort(
      "File request failed with HTTP status {response$status}.",
      "gx_error_download",
      status = response$status,
      .redact_trace = TRUE
    )
  }
  keep <- TRUE
  response
}
