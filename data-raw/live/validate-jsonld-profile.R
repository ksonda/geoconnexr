if (!identical(Sys.getenv("GEOCONNEXR_RUN_LIVE"), "true")) {
  stop("Set GEOCONNEXR_RUN_LIVE=true to run bounded live checks.", call. = FALSE)
}

for (package in c("digest", "httr2", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("This maintainer script requires ", package, ".", call. = FALSE)
  }
}

profiles <- list(
  reference_gage = list(
    fixture_id = "reference-gage-1000001",
    pid = "https://geoconnex.us/ref/gages/1000001",
    url = "https://reference.geoconnex.us/collections/gages/items/1000001"
  ),
  reference_mainstem = list(
    fixture_id = "reference-mainstem-29559",
    pid = "https://geoconnex.us/ref/mainstems/29559",
    url = "https://reference.geoconnex.us/collections/mainstems/items/29559"
  ),
  virginia_wqp = list(
    fixture_id = "virginia-wqp-wmpo001",
    pid = "https://geoconnex.us/iow/wqp/21VASWCB-WMPO001",
    url = "https://sta.geoconnex.dev/collections/WQP/Things/items/%2721VASWCB-WMPO001%27"
  ),
  nevada_ndwr = list(
    fixture_id = "nevada-ndwr-222s12e6701dd1",
    pid = "https://geoconnex.us/ndwr/gages/222S12E6701DD1",
    url = "https://features.internetofwater.dev/collections/nv/gages/items/222S12E6701DD1"
  ),
  montana_mtdnrc = list(
    fixture_id = "montana-mtdnrc-sr002",
    pid = "https://geoconnex.us/mtdnrc/gages/SR002",
    url = "https://features.internetofwater.dev/collections/mt/gages/items/SR002"
  ),
  usda_nrcs_snotel = list(
    fixture_id = "usda-nrcs-snotel-301",
    pid = "https://geoconnex.us/wwdh/snotel/301",
    url = "https://api.wwdh.internetofwater.app/collections/snotel-edr/items/301"
  )
)

max_requests <- length(profiles)
request_count <- 0L
manifest <- jsonlite::fromJSON(
  file.path("tests", "fixtures", "jsonld", "manifest-v1.json"),
  simplifyVector = FALSE
)
manifest_entries <- stats::setNames(
  manifest$fixtures,
  vapply(manifest$fixtures, `[[`, character(1), "fixture_id")
)

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
  request <- httr2::req_error(request, is_error = function(response) FALSE)
  response <- httr2::req_perform(request)
  body <- httr2::resp_body_raw(response)
  stopifnot(length(body) <= 2 * 1024 * 1024)
  media_type <- httr2::resp_content_type(response)
  parsed <- if (!is.null(media_type) && grepl("json", media_type, ignore.case = TRUE)) {
    tryCatch(
      jsonlite::fromJSON(rawToChar(body), simplifyVector = FALSE),
      error = function(error) NULL
    )
  } else {
    NULL
  }
  valid_jsonld <-
    httr2::resp_status(response) >= 200L &&
    httr2::resp_status(response) < 300L &&
    !is.null(media_type) &&
    identical(tolower(media_type), "application/ld+json") &&
    !is.null(parsed)
  list(
    status = httr2::resp_status(response),
    media_type = media_type,
    bytes = length(body),
    sha256 = digest::digest(body, algo = "sha256", serialize = FALSE),
    top_level_names = names(parsed),
    valid_jsonld = valid_jsonld
  )
}

results <- lapply(profiles, function(profile) {
  response <- bounded_get(profile$url, "application/ld+json")
  expected <- manifest_entries[[profile$fixture_id]]$observed
  matches_manifest <-
    identical(response$status, as.integer(expected$status)) &&
    identical(response$media_type, expected$media_type) &&
    identical(response$bytes, as.integer(expected$original_bytes)) &&
    identical(response$sha256, expected$original_sha256) &&
    identical(profile$url, expected$final_url)
  c(
    list(
      fixture_id = profile$fixture_id,
      pid = profile$pid,
      url = profile$url,
      matches_manifest = matches_manifest
    ),
    response
  )
})

report <- list(
  checked_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  request_count = request_count,
  profiles = results,
  note = "Response bodies are not persisted; any service or manifest drift exits nonzero."
)
cat(jsonlite::toJSON(
  report,
  auto_unbox = TRUE,
  pretty = TRUE
), "\n")

if (!all(vapply(results, function(result) {
  isTRUE(result$valid_jsonld) && isTRUE(result$matches_manifest)
}, logical(1)))) {
  stop("One or more live JSON-LD profiles drifted from the recorded manifest.", call. = FALSE)
}
