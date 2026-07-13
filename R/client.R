.gx_cache_schema_version <- "1"

gx_scalar_number <- function(x, name, minimum = 0, maximum = Inf, integer = FALSE) {
  valid <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x >= minimum && x <= maximum && (!integer || x == trunc(x))
  if (!valid) {
    gx_abort(
      "{.arg {name}} must be one finite {if (integer) 'whole ' else ''}number between {minimum} and {maximum}.",
      "gx_error_client"
    )
  }
  if (integer) as.integer(x) else as.numeric(x)
}

gx_default_cache_dir <- function() {
  getOption(
    "geoconnexr.cache_dir",
    tools::R_user_dir("geoconnexr", which = "cache")
  )
}

gx_now <- function() {
  clock <- getOption("geoconnexr.clock", Sys.time)
  if (!is.function(clock)) {
    gx_abort("The configured clock must be a function.", "gx_error_client")
  }
  now <- as.POSIXct(clock(), tz = "UTC")
  if (length(now) != 1L || is.na(now)) {
    gx_abort("The configured clock returned an invalid time.", "gx_error_client")
  }
  now
}

gx_validate_cache_dir <- function(cache_dir) {
  if (!is.character(cache_dir) || length(cache_dir) != 1L ||
      is.na(cache_dir) || !nzchar(cache_dir)) {
    gx_abort("{.arg cache_dir} must be one non-empty path.", "gx_error_client")
  }
  invisible(cache_dir)
}

gx_cache_marker <- function(cache_dir) {
  file.path(cache_dir, ".geoconnexr-cache")
}

gx_prepare_cache_dir <- function(cache_dir) {
  gx_validate_cache_dir(cache_dir)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(cache_dir)) {
    gx_abort("Cache directory could not be created.", "gx_error_cache")
  }
  marker <- gx_cache_marker(cache_dir)
  if (!file.exists(marker)) {
    existing <- list.files(cache_dir, all.files = TRUE, no.. = TRUE)
    if (length(existing)) {
      gx_abort(
        "Refusing to use an unmarked, non-empty directory as the package cache.",
        "gx_error_cache_ownership"
      )
    }
    writeLines(
      paste0("geoconnexr-cache-schema:", .gx_cache_schema_version),
      marker,
      useBytes = TRUE
    )
  }
  invisible(cache_dir)
}

gx_cache_backend <- function(cache_dir) {
  gx_prepare_cache_dir(cache_dir)
  cachem::cache_disk(
    dir = cache_dir,
    max_size = getOption("geoconnexr.cache_max_size", 1024^3),
    max_age = Inf,
    evict = "fifo",
    warn_ref_objects = FALSE
  )
}

#' Create a bounded Geoconnex protocol client
#'
#' A client records endpoint-specific request policy. Network calls remain
#' lazy: constructing a client never contacts a service or writes to its cache.
#'
#' @param endpoint One of `"graph"`, `"reference"`, or `"pid"`.
#' @param timeout Per-attempt timeout in seconds.
#' @param retries Number of retry attempts after the initial request.
#' @param max_bytes Maximum decoded response bytes.
#' @param cache Whether successful and redirect responses may use the package
#'   cache.
#' @param offline Whether requests must be satisfied from valid cache entries.
#' @param cache_dir Cache directory.
#'
#' @return An object of class `gx_client`.
#' @export
gx_client <- function(endpoint = c("graph", "reference", "pid"),
                      timeout = 30,
                      retries = 3L,
                      max_bytes = 2 * 1024^2,
                      cache = TRUE,
                      offline = getOption("geoconnexr.offline", FALSE),
                      cache_dir = gx_default_cache_dir()) {
  endpoint <- tryCatch(
    match.arg(endpoint),
    error = function(cnd) {
      gx_abort(
        "{.arg endpoint} must be one of 'graph', 'reference', or 'pid'.",
        "gx_error_client"
      )
    }
  )
  timeout <- gx_scalar_number(timeout, "timeout", minimum = .Machine$double.eps)
  retries <- gx_scalar_number(
    retries,
    "retries",
    minimum = 0,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  max_bytes <- gx_scalar_number(
    max_bytes,
    "max_bytes",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  if (!is.logical(cache) || length(cache) != 1L || is.na(cache) ||
      !is.logical(offline) || length(offline) != 1L || is.na(offline)) {
    gx_abort(
      "{.arg cache} and {.arg offline} must each be one non-missing logical value.",
      "gx_error_client"
    )
  }
  gx_validate_cache_dir(cache_dir)
  if (offline && !cache) {
    gx_abort("Offline clients require {.arg cache = TRUE}.", "gx_error_client")
  }

  structure(
    list(
      endpoint = endpoint,
      base_url = unname(gx_endpoints()[[endpoint]]),
      timeout = timeout,
      retries = retries,
      max_bytes = max_bytes,
      cache = cache,
      offline = offline,
      cache_dir = cache_dir,
      user_agent = paste0(
        "geoconnexr/",
        tryCatch(as.character(utils::packageVersion("geoconnexr")), error = function(cnd) "development")
      )
    ),
    class = "gx_client"
  )
}

#' @export
print.gx_client <- function(x, ...) {
  cli::cli_inform(c(
    "<gx_client>",
    "* Endpoint: {x$endpoint} ({x$base_url})",
    "* Offline: {x$offline}",
    "* Cache: {x$cache}",
    "* Response ceiling: {format(x$max_bytes, big.mark = ',')} bytes"
  ))
  invisible(x)
}

#' Inspect the package HTTP cache
#'
#' @param cache_dir Cache directory to inspect.
#'
#' @return A one-row tibble with the cache path, entry count, and disk size.
#' @export
gx_cache_info <- function(cache_dir = gx_default_cache_dir()) {
  gx_validate_cache_dir(cache_dir)
  if (!dir.exists(cache_dir)) {
    return(tibble::tibble(
      path = normalizePath(cache_dir, winslash = "/", mustWork = FALSE),
      entries = 0L,
      size_bytes = 0
    ))
  }

  backend <- gx_cache_backend(cache_dir)
  files <- list.files(
    cache_dir,
    all.files = TRUE,
    recursive = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  files <- files[file.exists(files) & !dir.exists(files)]
  size <- if (length(files)) sum(file.info(files)$size, na.rm = TRUE) else 0
  tibble::tibble(
    path = normalizePath(cache_dir, winslash = "/", mustWork = FALSE),
    entries = length(backend$keys()),
    size_bytes = as.numeric(size)
  )
}

#' Clear the package HTTP cache
#'
#' @param confirm Prompt before clearing when `TRUE`. Non-interactive callers
#'   must pass `FALSE` explicitly.
#' @param cache_dir Cache directory to clear.
#'
#' @return `TRUE` invisibly if the cache was cleared, otherwise `FALSE`.
#' @export
gx_cache_clear <- function(confirm = interactive(),
                           cache_dir = gx_default_cache_dir()) {
  if (!is.logical(confirm) || length(confirm) != 1L || is.na(confirm)) {
    gx_abort("{.arg confirm} must be one non-missing logical value.", "gx_error_client")
  }
  gx_validate_cache_dir(cache_dir)
  if (confirm) {
    if (!interactive()) {
      gx_abort(
        "Use {.code confirm = FALSE} to clear the cache non-interactively.",
        "gx_error_client"
      )
    }
    answer <- utils::askYesNo("Clear the geoconnexr HTTP cache?")
    if (!isTRUE(answer)) {
      return(invisible(FALSE))
    }
  }
  if (dir.exists(cache_dir)) {
    if (!file.exists(gx_cache_marker(cache_dir))) {
      gx_abort(
        "Refusing to clear a directory not marked as a geoconnexr cache.",
        "gx_error_cache_ownership"
      )
    }
    gx_cache_backend(cache_dir)$reset()
  }
  invisible(TRUE)
}
