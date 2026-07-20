gx_resolution_problem <- function(cnd, prefix = NULL) {
  classes <- class(cnd)
  matches <- classes[grepl("^gx_error_", classes)]
  code <- if (length(matches)) matches[[1]] else "gx_error"
  code <- sub("^gx_error_", "", code)
  if (!is.null(prefix)) paste(prefix, code, sep = "_") else code
}

gx_empty_resolution <- function() {
  tibble::tibble(
    pid_uri = character(),
    initial_status = integer(),
    final_status = integer(),
    landing_url = character(),
    redirect_chain = list(),
    resolved_at = as.POSIXct(character(), tz = "UTC"),
    problem_code = character()
  )
}

gx_resolve_row <- function(pid_uri, client, follow, max_redirects, budget = NULL) {
  method <- "HEAD"
  current <- gx_canonical_url(pid_uri)
  chain <- current
  visited <- current
  initial_status <- NA_integer_
  final_status <- NA_integer_
  landing_url <- NA_character_
  problem_code <- NA_character_
  redirects <- 0L
  accept <- "application/ld+json, text/html;q=0.9, */*;q=0.1"

  repeat {
    gx_jsonld_budget_begin(budget)
    response <- tryCatch(
      gx_http_request(
        client,
        method = method,
        url = current,
        headers = list(Accept = accept),
        check_status = FALSE
      ),
      error = function(cnd) cnd
    )
    if (inherits(response, "error")) {
      if (!is.null(budget) && inherits(response, "gx_error_jsonld_budget")) {
        stop(response)
      }
      problem_code <- gx_resolution_problem(response)
      break
    }
    gx_jsonld_budget_record(budget, response)
    if (is.na(initial_status)) {
      initial_status <- response$status
    }

    if (identical(method, "HEAD") && response$status %in% c(405L, 501L)) {
      method <- "GET"
      next
    }

    final_status <- response$status
    if (response$status < 300L || response$status >= 400L) {
      landing_url <- current
      if (response$status < 200L || response$status >= 300L) {
        problem_code <- paste0("http_", response$status)
      }
      break
    }

    location <- gx_header(response$headers, "location")
    if (is.null(location) || !nzchar(location)) {
      problem_code <- "redirect_missing_location"
      break
    }
    target <- tryCatch(
      httr2::url_modify_relative(current, location),
      error = function(cnd) NA_character_
    )
    if (is.na(target)) {
      problem_code <- "redirect_invalid_location"
      break
    }
    safety <- tryCatch(
      gx_assert_safe_url(target, resolve_dns = !client$offline),
      error = function(cnd) cnd
    )
    if (inherits(safety, "error")) {
      problem_code <- gx_resolution_problem(safety, "redirect")
      break
    }
    target <- gx_canonical_url(target)
    if (target %in% visited) {
      problem_code <- "redirect_loop"
      break
    }
    if (!follow) {
      landing_url <- target
      break
    }

    redirects <- redirects + 1L
    if (redirects > max_redirects) {
      problem_code <- "redirect_limit"
      break
    }
    chain <- c(chain, target)
    visited <- c(visited, target)
    current <- target
  }

  resolved_at <- gx_now()
  list(
    pid_uri = pid_uri,
    initial_status = initial_status,
    final_status = final_status,
    landing_url = landing_url,
    redirect_chain = chain,
    resolved_at = resolved_at,
    problem_code = problem_code
  )
}

#' Resolve Geoconnex persistent identifiers
#'
#' Resolution preserves each PID as the identity key while recording every
#' followed redirect. Each redirect target is checked before a request is
#' dispatched. A rejected `HEAD` request is retried as a minimal `GET`.
#'
#' @param uri Character vector of Geoconnex persistent identifiers.
#' @param follow Whether to follow safe redirects.
#' @param client A PID client created by [gx_client()].
#'
#' @return A tibble with one row per input PID and a list-column containing the
#'   redirect chain.
#' @export
gx_resolve <- function(uri, follow = TRUE, client = gx_client("pid")) {
  gx_resolve_impl(uri, follow = follow, client = client)
}

gx_resolve_impl <- function(uri, follow = TRUE, client = gx_client("pid"), budget = NULL) {
  if (!is.character(uri) || anyNA(uri) || any(!nzchar(uri))) {
    gx_abort(
      "{.arg uri} must be a character vector of non-missing HTTP(S) identifiers.",
      "gx_error_identifier"
    )
  }
  if (!is.logical(follow) || length(follow) != 1L || is.na(follow)) {
    gx_abort("{.arg follow} must be one logical value.", "gx_error_client")
  }
  if (!inherits(client, "gx_client") || !identical(client$endpoint, "pid")) {
    gx_abort("{.arg client} must be a PID client created by {.fn gx_client}.", "gx_error_client")
  }
  if (!length(uri)) {
    return(gx_empty_resolution())
  }
  invisible(lapply(uri, gx_assert_safe_url, resolve_dns = !client$offline))

  max_redirects <- getOption("geoconnexr.max_redirects", 10L)
  max_redirects <- gx_scalar_number(
    max_redirects,
    "geoconnexr.max_redirects",
    minimum = 0,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  rows <- lapply(
    uri,
    gx_resolve_row,
    client = client,
    follow = follow,
    max_redirects = max_redirects,
    budget = budget
  )

  tibble::tibble(
    pid_uri = vapply(rows, `[[`, character(1), "pid_uri"),
    initial_status = vapply(rows, `[[`, integer(1), "initial_status"),
    final_status = vapply(rows, `[[`, integer(1), "final_status"),
    landing_url = vapply(rows, `[[`, character(1), "landing_url"),
    redirect_chain = unname(lapply(rows, `[[`, "redirect_chain")),
    resolved_at = do.call(c, lapply(rows, `[[`, "resolved_at")),
    problem_code = vapply(rows, `[[`, character(1), "problem_code")
  )
}
