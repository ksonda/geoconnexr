gx_default_dns_resolver <- function(host) {
  curl::nslookup(host, multiple = TRUE, error = FALSE)
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

gx_cache_allowed <- function(headers) {
  sensitive <- c("authorization", "cookie", "proxy-authorization", "range")
  !any(names(gx_normalize_headers(headers)) %in% sensitive)
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
  if (request$retries > 0L) {
    req <- httr2::req_retry(
      req,
      max_tries = request$retries + 1L,
      retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
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
  total <- 0L
  chunk_kb <- max(1, min(64, ceiling(request$max_bytes / 1024)))
  while (!httr2::resp_stream_is_complete(response)) {
    chunk <- httr2::resp_stream_raw(response, kb = chunk_kb)
    if (!length(chunk)) {
      next
    }
    total <- total + length(chunk)
    if (total > request$max_bytes) {
      gx_abort(
        "Response exceeded the configured {format(request$max_bytes, big.mark = ',')} byte ceiling.",
        "gx_error_payload_too_large"
      )
    }
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
       !is.na(content_length) && content_length > client$max_bytes) ||
      length(response$body) > client$max_bytes) {
    gx_abort(
      "Response exceeded the configured {format(client$max_bytes, big.mark = ',')} byte ceiling.",
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
    media_type <- tolower(gx_header(response$headers, "content-type") %||% "")
    if (response$status >= 200L && response$status < 300L &&
        grepl("text/html", media_type, fixed = TRUE)) {
      gx_abort("Graph endpoint returned HTML instead of a SPARQL result.", "gx_error_content_type")
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
                            check_status = TRUE) {
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
  target <- gx_safe_target(url, resolve_dns = !client$offline)
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
    retries = client$retries,
    max_bytes = client$max_bytes,
    user_agent = client$user_agent,
    request_id = key,
    resolved_host = target$host,
    resolved_port = target$port,
    resolved_ip = target$addresses
  )
  backend <- if (client$cache && gx_cache_allowed(headers)) {
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
      gx_validate_endpoint_response(cached, client, check_status)
      return(cached)
    }
  }
  if (client$offline) {
    gx_abort(
      "Offline cache miss for {method} {.url {gx_canonical_url(url)}}.",
      "gx_error_offline_miss"
    )
  }

  performer <- getOption("geoconnexr.performer", gx_default_performer)
  if (!is.function(performer)) {
    gx_abort("The configured HTTP performer must be a function.", "gx_error_client")
  }
  raw_response <- tryCatch(
    performer(request),
    error = function(cnd) {
      if (inherits(cnd, "gx_error")) {
        stop(cnd)
      }
      gx_abort("HTTP transport failed.", "gx_error_transport", parent = cnd)
    }
  )
  response <- gx_validate_response(raw_response, request, client)
  gx_validate_endpoint_response(response, client, check_status)

  cacheable <- response$status >= 200L && response$status < 400L
  if (!is.null(backend) && cacheable) {
    cached_response <- response
    cached_response$request <- NULL
    backend$set(key, list(
      cache_schema_version = .gx_cache_schema_version,
      request_id = key,
      response = cached_response
    ))
  }
  response
}
