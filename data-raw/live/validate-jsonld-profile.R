if (!identical(Sys.getenv("GEOCONNEXR_RUN_LIVE"), "true")) {
  stop("Set GEOCONNEXR_RUN_LIVE=true to run bounded live checks.", call. = FALSE)
}

for (package in c("httr2", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("This maintainer script requires ", package, ".", call. = FALSE)
  }
}

max_requests <- 2L
request_count <- 0L

bounded_get <- function(url, accept) {
  request_count <<- request_count + 1L
  stopifnot(request_count <= max_requests)

  request <- httr2::request(url)
  request <- httr2::req_headers(request, Accept = accept)
  request <- httr2::req_user_agent(request, "geoconnexr-p0-evidence/0.1")
  request <- httr2::req_timeout(request, seconds = 30)
  request <- httr2::req_options(
    request,
    maxfilesize = 2 * 1024 * 1024,
    maxredirs = 0
  )
  response <- httr2::req_perform(request)
  body <- httr2::resp_body_raw(response)
  stopifnot(length(body) <= 2 * 1024 * 1024)
  list(
    status = httr2::resp_status(response),
    media_type = httr2::resp_content_type(response),
    body = jsonlite::fromJSON(rawToChar(body), simplifyVector = FALSE)
  )
}

gage <- bounded_get(
  "https://reference.geoconnex.us/collections/gages/items/1000001",
  "application/ld+json"
)

cat(jsonlite::toJSON(
  list(
    checked_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    request_count = request_count,
    status = gage$status,
    media_type = gage$media_type,
    top_level_names = names(gage$body),
    note = "Inspect expansion separately; no response body is persisted."
  ),
  auto_unbox = TRUE,
  pretty = TRUE
), "\n")
