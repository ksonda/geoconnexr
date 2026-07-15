.gx_reference_contract_version <- "0.1.0"

gx_ref_client <- function(client) {
  if (!inherits(client, "gx_client") || !identical(client$endpoint, "reference")) {
    gx_abort(
      "{.arg client} must be a reference client created by {.fn gx_client}.",
      "gx_error_reference_client"
    )
  }
  client
}

gx_ref_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    gx_abort("{.arg {name}} must be one non-missing logical value.", "gx_error_reference_input")
  }
  x
}

gx_ref_path_segment <- function(x, name) {
  utf8 <- is.character(x) && length(x) == 1L && !is.na(x) &&
    isTRUE(stringi::stri_enc_isutf8(x))
  forbidden <- if (utf8) {
    stringi::stri_detect_regex(x, "[\\p{Zl}\\p{Zp}\\p{Cc}\\p{Cf}\\p{Cs}]") ||
      stringi::stri_detect_fixed(x, "/") ||
      stringi::stri_detect_fixed(x, "\\") ||
      stringi::stri_detect_fixed(x, "%")
  } else {
    TRUE
  }
  valid <- utf8 && nzchar(x) && nchar(x, type = "bytes") <= 1024L &&
    !x %in% c(".", "..") && isFALSE(forbidden)
  if (!valid) {
    gx_abort(
      "{.arg {name}} must be one non-empty UTF-8 path identifier.",
      "gx_error_reference_input"
    )
  }
  utils::URLencode(enc2utf8(x), reserved = TRUE, repeated = TRUE)
}

gx_ref_url <- function(client, ...) {
  parts <- vapply(list(...), as.character, character(1))
  paste(c(sub("/+$", "", client$base_url), parts), collapse = "/")
}

gx_ref_require_query <- function(url, required_query = list()) {
  if (!length(required_query)) return(url)
  out <- url
  for (name in names(required_query)) {
    query <- httr2::url_parse(out)$query %||% list()
    positions <- which(names(query) == name)
    existing <- if (length(positions) == 1L) query[[positions]] else NULL
    required <- as.character(required_query[[name]])
    if (length(existing) == 1L && identical(as.character(existing), required)) next
    out <- do.call(
      httr2::url_modify_query,
      c(list(.url = out), stats::setNames(list(required), name))
    )
  }
  out
}

gx_ref_url_byte_limit <- function() {
  gx_scalar_number(
    getOption("geoconnexr.ref_max_url_bytes", 16384L),
    "geoconnexr.ref_max_url_bytes",
    minimum = 256,
    maximum = 1048576,
    integer = TRUE
  )
}

gx_ref_query_value_byte_limit <- function() {
  gx_scalar_number(
    getOption("geoconnexr.ref_max_query_value_bytes", 4096L),
    "geoconnexr.ref_max_query_value_bytes",
    minimum = 1,
    maximum = 1048576,
    integer = TRUE
  )
}

gx_ref_origin <- function(url) {
  parsed <- httr2::url_parse(gx_canonical_url(url))
  port <- parsed$port %||% if (identical(parsed$scheme, "https")) "443" else "80"
  paste0(tolower(parsed$scheme), "://", tolower(parsed$hostname), ":", port)
}

gx_ref_url_key <- function(url) {
  parsed <- httr2::url_parse(gx_canonical_url(url))
  query <- parsed$query %||% list()
  parsed$query <- NULL
  parsed$fragment <- NULL
  base <- httr2::url_build(parsed)
  if (!length(query)) return(base)
  values <- vapply(query, as.character, character(1))
  pairs <- paste0(
    utils::URLencode(names(query), reserved = TRUE, repeated = TRUE),
    "=",
    utils::URLencode(values, reserved = TRUE, repeated = TRUE)
  )
  paste0(base, "?", paste(sort(pairs), collapse = "&"))
}

gx_ref_assert_endpoint_url <- function(url, client) {
  if (nchar(enc2utf8(url), type = "bytes") > gx_ref_url_byte_limit()) {
    gx_abort(
      "Reference URL exceeds the configured byte budget.",
      "gx_error_reference_budget"
    )
  }
  gx_assert_safe_url(url, resolve_dns = FALSE)
  candidate <- httr2::url_parse(gx_canonical_url(url))
  base <- httr2::url_parse(gx_canonical_url(client$base_url))
  if (!identical(gx_ref_origin(url), gx_ref_origin(client$base_url))) {
    gx_abort(
      "Reference pagination and redirects must remain on the configured endpoint.",
      "gx_error_reference_endpoint"
    )
  }
  base_path <- sub("/+$", "", base$path %||% "")
  candidate_path <- candidate$path %||% ""
  if (nzchar(base_path) && !identical(base_path, "/") &&
      !identical(candidate_path, base_path) &&
      !startsWith(candidate_path, paste0(base_path, "/"))) {
    gx_abort(
      "Reference pagination escaped the configured endpoint path.",
      "gx_error_reference_endpoint"
    )
  }
  invisible(gx_canonical_url(url))
}

gx_ref_empty_requests <- function() {
  tibble::tibble(
    request_id = character(), method = character(), url = character(),
    status = integer(), media_type = character(), bytes = integer(),
    body_sha256 = character(), retrieved_at = as.POSIXct(character(), tz = "UTC"),
    cache_origin = character()
  )
}

gx_ref_request_row <- function(response) {
  tibble::tibble(
    request_id = response$request$request_id,
    method = response$request$method,
    url = gx_redact_url(response$request$url),
    status = response$status,
    media_type = gx_media_type(response$headers),
    bytes = as.integer(response$bytes),
    body_sha256 = response$body_sha256,
    retrieved_at = response$retrieved_at,
    cache_origin = response$cache_origin
  )
}

gx_ref_get <- function(url, client, accept, check_status = TRUE,
                       required_query = list(), max_requests = NULL,
                       max_total_bytes = NULL,
                       request_budget_scope = "reference",
                       byte_budget_scope = "reference") {
  gx_ref_client(client)
  current <- gx_canonical_url(gx_ref_require_query(url, required_query))
  gx_ref_assert_endpoint_url(current, client)
  visited <- current
  chain <- current
  ledger <- gx_ref_empty_requests()
  redirects <- 0L
  max_redirects <- gx_scalar_number(
    getOption("geoconnexr.max_redirects", 10L),
    "geoconnexr.max_redirects",
    minimum = 0,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  if (!is.null(max_requests)) {
    max_requests <- gx_scalar_number(
      max_requests,
      "max_requests",
      minimum = 1,
      maximum = 100000,
      integer = TRUE
    )
  }
  if (!is.null(max_total_bytes)) {
    max_total_bytes <- gx_scalar_number(
      max_total_bytes,
      "max_total_bytes",
      minimum = 1,
      maximum = .Machine$integer.max,
      integer = TRUE
    )
  }
  if (!request_budget_scope %in% c("reference", "injected") ||
      !byte_budget_scope %in% c("reference", "injected")) {
    gx_abort("Internal reference budget scope is invalid.", "gx_error_reference_client")
  }
  attempt_control <- list(
    before = function(request, physical) {
      if (!is.null(max_requests) && nrow(ledger) >= max_requests) {
        gx_abort(
          "Reference request exhausted its request-attempt budget.",
          "gx_error_reference_budget",
          budget_kind = "requests",
          budget_scope = request_budget_scope,
          requests = ledger
        )
      }
      if (is.null(max_total_bytes)) return(request$max_bytes)
      remaining_bytes <- max_total_bytes -
        sum(as.double(ledger$bytes), na.rm = TRUE)
      if (!is.finite(remaining_bytes) || remaining_bytes < 1) {
        gx_abort(
          "Reference request exhausted its cumulative-byte budget.",
          "gx_error_reference_budget",
          budget_kind = "bytes",
          budget_scope = byte_budget_scope,
          requests = ledger
        )
      }
      as.integer(min(as.double(request$max_bytes), floor(remaining_bytes)))
    },
    after = function(attempt) {
      ledger <<- rbind(ledger, gx_http_attempt_request_row(attempt))
      if (!is.null(max_total_bytes) &&
          sum(as.double(ledger$bytes), na.rm = TRUE) > max_total_bytes) {
        gx_abort(
          "Reference request exceeded its cumulative-byte budget.",
          "gx_error_reference_budget",
          budget_kind = "bytes",
          budget_scope = byte_budget_scope,
          requests = ledger
        )
      }
      invisible(NULL)
    }
  )

  repeat {
    response <- tryCatch(
      gx_http_request(
        client,
        method = "GET",
        url = current,
        headers = list(Accept = accept),
        check_status = FALSE,
        .attempt_control = attempt_control
      ),
      error = function(cnd) gx_ref_rethrow_with_requests(cnd, ledger)
    )
    if (response$status < 300L || response$status >= 400L) break

    location <- gx_header(response$headers, "location")
    if (is.null(location) || !nzchar(location)) {
      gx_abort(
        "Reference redirect omitted Location.",
        "gx_error_reference_redirect",
        requests = ledger
      )
    }
    target <- tryCatch(
      httr2::url_modify_relative(current, location),
      error = function(cnd) NA_character_
    )
    if (is.na(target)) {
      gx_abort(
        "Reference redirect Location is invalid.",
        "gx_error_reference_redirect",
        requests = ledger
      )
    }
    target <- gx_ref_require_query(target, required_query)
    tryCatch(
      gx_ref_assert_endpoint_url(target, client),
      error = function(cnd) gx_ref_rethrow_with_requests(cnd, ledger)
    )
    target <- gx_canonical_url(target)
    if (target %in% visited) {
      gx_abort(
        "Reference redirect loop detected.",
        "gx_error_reference_redirect",
        requests = ledger
      )
    }
    redirects <- redirects + 1L
    if (redirects > max_redirects) {
      gx_abort(
        "Reference redirect limit exceeded.",
        "gx_error_reference_redirect",
        requests = ledger
      )
    }
    current <- target
    visited <- c(visited, current)
    chain <- c(chain, current)
  }

  if (check_status && (response$status < 200L || response$status >= 300L)) {
    gx_abort(
      "Reference request failed with HTTP status {response$status}.",
      "gx_error_reference_http",
      status = response$status,
      requests = ledger
    )
  }
  list(response = response, requests = ledger, redirect_chain = chain)
}

gx_ref_assert_unique_object_members <- function(value) {
  stack <- list(value)
  while (length(stack)) {
    current <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    if (!is.list(current)) next
    if (!is.null(names(current)) && anyDuplicated(names(current))) {
      gx_abort(
        "Reference JSON contains duplicate object members.",
        "gx_error_reference_payload"
      )
    }
    children <- current[vapply(current, is.list, logical(1))]
    if (length(children)) stack <- c(stack, unname(children))
  }
  invisible(value)
}

gx_ref_json <- function(response, expected, require_object = TRUE) {
  media_type <- gx_media_type(response$headers)
  json_type <- media_type %in% c(
    "application/json", "application/geo+json", "application/schema+json",
    "application/problem+json", "application/ld+json"
  ) || endsWith(media_type, "+json")
  text <- tryCatch(
    gx_json_text(response$body),
    gx_error_jsonld_syntax = function(cnd) {
      gx_abort(
        "Reference endpoint returned malformed JSON.",
        "gx_error_reference_payload"
      )
    }
  )
  leading <- sub("^[[:space:]]+", "", text)
  if (!json_type && (!nzchar(media_type) || media_type %in% c("text/plain", "application/octet-stream")) &&
      (startsWith(leading, "{") || startsWith(leading, "["))) {
    json_type <- TRUE
  }
  if (!json_type) {
    gx_abort(
      "Reference endpoint did not return JSON for {expected}.",
      "gx_error_reference_content_type"
    )
  }
  tryCatch(
    gx_json_assert_depth(text),
    gx_error_jsonld_syntax = function(cnd) {
      gx_abort(
        "Reference endpoint returned malformed JSON.",
        "gx_error_reference_payload"
      )
    }
  )
  value <- tryCatch(
      jsonlite::fromJSON(text, simplifyVector = FALSE, bigint_as_char = TRUE),
    error = function(cnd) {
      gx_abort("Reference endpoint returned malformed JSON.", "gx_error_reference_payload")
    }
  )
  if (!is.list(value) || (require_object && is.null(names(value)))) {
    expected_root <- if (require_object) "a JSON object" else "JSON"
    gx_abort(
      "Reference {expected} payload must contain {expected_root}.",
      "gx_error_reference_payload"
    )
  }
  gx_ref_assert_unique_object_members(value)
  max_members <- gx_scalar_number(
    getOption("geoconnexr.ref_max_members", 250000L),
    "geoconnexr.ref_max_members",
    minimum = 1,
    maximum = 10000000,
    integer = TRUE
  )
  complexity <- gx_json_measure_complexity(value, max_members = max_members)
  if (identical(complexity$exceeded, "members")) {
    gx_abort(
      "Reference JSON exceeds the configured member budget.",
      "gx_error_reference_budget"
    )
  }
  value
}

gx_ref_scalar_text <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x)) return(default)
  value <- unlist(x, recursive = TRUE, use.names = FALSE)
  value <- value[!is.na(value)]
  if (!length(value)) default else as.character(value[[1]])
}

gx_ref_metadata <- function(requests, source_url, diagnostics = gx_empty_diagnostics(), ...) {
  list(
    contract_version = .gx_reference_contract_version,
    source_url = source_url,
    retrieved_at = if (nrow(requests)) max(requests$retrieved_at) else as.POSIXct(NA, tz = "UTC"),
    requests = requests,
    diagnostics = diagnostics,
    ...
  )
}

gx_ref_validate_metadata <- function(metadata) {
  request_names <- names(gx_ref_empty_requests())
  diagnostic_names <- names(gx_empty_diagnostics())
  valid <- is.list(metadata) &&
    identical(metadata$contract_version, .gx_reference_contract_version) &&
    is.character(metadata$source_url) && length(metadata$source_url) == 1L &&
    !is.na(metadata$source_url) &&
    inherits(metadata$retrieved_at, "POSIXct") &&
    length(metadata$retrieved_at) == 1L &&
    is.data.frame(metadata$requests) &&
    identical(names(metadata$requests), request_names) &&
    is.data.frame(metadata$diagnostics) &&
    identical(names(metadata$diagnostics), diagnostic_names)
  if (!valid) {
    gx_abort(
      "Reference metadata violates its contract.",
      "gx_error_reference_contract"
    )
  }
  invisible(metadata)
}

gx_ref_validate_collections <- function(x, metadata) {
  expected <- c(
    "contract_version", "collection_id", "title", "description",
    "item_type", "crs", "extent", "links", "raw"
  )
  character_columns <- c(
    "contract_version", "collection_id", "title", "description", "item_type"
  )
  list_columns <- c("crs", "extent", "links", "raw")
  valid <- is.data.frame(x) && identical(names(x), expected) &&
    all(vapply(x[character_columns], is.character, logical(1))) &&
    all(vapply(x[list_columns], is.list, logical(1))) &&
    !anyNA(x$contract_version) &&
    all(x$contract_version == .gx_reference_contract_version) &&
    !anyNA(x$collection_id) && !anyDuplicated(x$collection_id) &&
    is.numeric(metadata$collection_count) &&
    length(metadata$collection_count) == 1L && !is.na(metadata$collection_count) &&
    identical(as.integer(metadata$collection_count), as.integer(nrow(x)))
  if (!valid) {
    gx_abort(
      "Reference collections violate their table contract.",
      "gx_error_reference_contract"
    )
  }
  gx_ref_validate_metadata(metadata)
  invisible(x)
}

gx_ref_validate_queryables <- function(x, metadata) {
  expected <- c(
    "contract_version", "collection_id", "name", "json_types", "format",
    "title", "description", "enum", "schema"
  )
  character_columns <- c(
    "contract_version", "collection_id", "name", "format", "title",
    "description"
  )
  list_columns <- c("json_types", "enum", "schema")
  valid <- is.data.frame(x) && identical(names(x), expected) &&
    all(vapply(x[character_columns], is.character, logical(1))) &&
    all(vapply(x[list_columns], is.list, logical(1))) &&
    !anyNA(x$contract_version) &&
    all(x$contract_version == .gx_reference_contract_version) &&
    !anyNA(x$collection_id) && !anyNA(x$name) && !anyDuplicated(x$name) &&
    is.character(metadata$collection) && length(metadata$collection) == 1L &&
    !is.na(metadata$collection) &&
    (!nrow(x) || all(x$collection_id == metadata$collection))
  if (!valid) {
    gx_abort(
      "Reference queryables violate their table contract.",
      "gx_error_reference_contract"
    )
  }
  gx_ref_validate_metadata(metadata)
  invisible(x)
}

gx_ref_validate_features <- function(x, metadata) {
  valid <- inherits(x, "sf") &&
    all(c("contract_version", "feature_id") %in% names(x)) &&
    is.character(x$contract_version) && is.character(x$feature_id) &&
    !anyNA(x$contract_version) &&
    all(x$contract_version == .gx_reference_contract_version) &&
    is.character(metadata$collection) && length(metadata$collection) == 1L &&
    !is.na(metadata$collection) &&
    is.numeric(metadata$number_returned) &&
    length(metadata$number_returned) == 1L &&
    identical(as.integer(metadata$number_returned), as.integer(nrow(x)))
  if (!valid) {
    gx_abort(
      "Reference features violate their simple-feature contract.",
      "gx_error_reference_contract"
    )
  }
  gx_ref_validate_metadata(metadata)
  invisible(x)
}

gx_ref_validate_feature <- function(x, metadata) {
  gx_ref_validate_features(x, metadata)
  if (nrow(x) != 1L || !is.character(metadata$id) ||
      length(metadata$id) != 1L || is.na(metadata$id)) {
    gx_abort(
      "A reference feature must contain one identified row.",
      "gx_error_reference_contract"
    )
  }
  invisible(x)
}

gx_ref_new_table <- function(x, class_name, metadata) {
  switch(
    class_name,
    gx_ref_collections = gx_ref_validate_collections(x, metadata),
    gx_ref_queryables = gx_ref_validate_queryables(x, metadata),
    gx_ref_features = gx_ref_validate_features(x, metadata),
    gx_abort(
      "Unknown reference table class.",
      "gx_error_reference_contract"
    )
  )
  attr(x, "gx_reference") <- metadata
  class(x) <- c(class_name, class(x))
  x
}

gx_ref_print_table <- function(x, class_name, label) {
  original <- x
  metadata <- attr(x, "gx_reference")
  cli::cli_inform(c(
    "<{class_name}>",
    "* {label}: {nrow(x)}",
    "* Requests: {nrow(metadata$requests %||% gx_ref_empty_requests())}"
  ))
  class(x) <- setdiff(class(x), class_name)
  print(x)
  invisible(original)
}

#' Discover reference feature collections
#'
#' Retrieves the live OGC API Features collection inventory through the
#' bounded reference client. `refresh = TRUE` bypasses the package cache for
#' this call without mutating the supplied client.
#'
#' @param refresh Whether to bypass cached collection metadata.
#' @param client A reference client created by [gx_client()].
#'
#' @return A `gx_ref_collections` tibble with one row per advertised
#'   collection. Nested `crs`, `extent`, `links`, and `raw` values are retained
#'   as list-columns. Request and retrieval metadata are stored in the
#'   `gx_reference` attribute.
#' @export
gx_ref_collections <- function(refresh = FALSE, client = gx_client("reference")) {
  gx_ref_flag(refresh, "refresh")
  gx_ref_client(client)
  transport <- client
  if (refresh) transport$cache <- FALSE
  fetched <- gx_ref_get(
    gx_ref_url(transport, "collections"),
    transport,
    "application/json"
  )
  tryCatch({
  payload <- gx_ref_json(fetched$response, "collections")
  if (!"collections" %in% names(payload)) {
    gx_abort(
      "Collections payload omitted its collections member.",
      "gx_error_reference_payload"
    )
  }
  collections <- payload$collections
  if (!is.list(collections)) {
    gx_abort("Collections payload has an invalid collections member.", "gx_error_reference_payload")
  }
  if (length(collections) && !is.null(names(collections))) {
    gx_abort(
      "Collections payload must contain a JSON array.",
      "gx_error_reference_payload"
    )
  }
  valid <- vapply(collections, function(x) is.list(x) &&
    is.character(x$id) && length(x$id) == 1L && !is.na(x$id) && nzchar(x$id), logical(1))
  if (length(valid) && !all(valid)) {
    gx_abort("Collections payload contains an invalid collection.", "gx_error_reference_payload")
  }
  ids <- if (length(collections)) vapply(collections, function(x) as.character(x$id), character(1)) else character()
  if (anyDuplicated(ids)) {
    gx_abort("Collections payload contains duplicate collection identifiers.", "gx_error_reference_payload")
  }
  out <- tibble::tibble(
    contract_version = rep(.gx_reference_contract_version, length(collections)),
    collection_id = ids,
    title = vapply(collections, function(x) gx_ref_scalar_text(x$title), character(1)),
    description = vapply(collections, function(x) gx_ref_scalar_text(x$description), character(1)),
    item_type = vapply(collections, function(x) gx_ref_scalar_text(x$itemType, "feature"), character(1)),
    crs = unname(lapply(collections, function(x) as.character(unlist(x$crs %||% list(), use.names = FALSE)))),
    extent = unname(lapply(collections, function(x) x$extent %||% list())),
    links = unname(lapply(collections, function(x) x$links %||% list())),
    raw = unname(collections)
  )
  metadata <- gx_ref_metadata(
    fetched$requests,
    fetched$response$url,
    collection_count = nrow(out),
    refreshed = refresh
  )
  gx_ref_new_table(out, "gx_ref_collections", metadata)
  }, error = function(cnd) {
    gx_ref_rethrow_with_requests(cnd, fetched$requests)
  })
}

#' @export
print.gx_ref_collections <- function(x, ...) {
  gx_ref_print_table(x, "gx_ref_collections", "Collections")
}

gx_ref_schema_types <- function(schema) {
  if (!is.list(schema)) return(character())
  unique(as.character(unlist(schema$type %||% list(), recursive = TRUE, use.names = FALSE)))
}

#' Discover queryable collection properties
#'
#' Reads the collection's OGC queryables schema. The complete property schema
#' is retained so [gx_ref_features()] can validate simple equality filters
#' before sending them.
#'
#' @param collection One advertised collection identifier.
#' @param client A reference client created by [gx_client()].
#'
#' @return A `gx_ref_queryables` tibble. The `json_types`, `enum`, and `schema`
#'   columns are list-columns that preserve server-advertised JSON Schema data.
#' @export
gx_ref_queryables <- function(collection, client = gx_client("reference")) {
  gx_ref_queryables_impl(collection, client)
}

gx_ref_queryables_impl <- function(collection, client = gx_client("reference"),
                                   .max_requests = NULL,
                                   .max_total_bytes = NULL,
                                   .request_budget_scope = NULL,
                                   .byte_budget_scope = NULL) {
  gx_ref_client(client)
  request_budget_scope <- .request_budget_scope %||%
    if (is.null(.max_requests)) "reference" else "injected"
  byte_budget_scope <- .byte_budget_scope %||%
    if (is.null(.max_total_bytes)) "reference" else "injected"
  if (!request_budget_scope %in% c("reference", "injected") ||
      !byte_budget_scope %in% c("reference", "injected")) {
    gx_abort("Internal queryables budget scope is invalid.", "gx_error_reference_client")
  }
  collection_path <- gx_ref_path_segment(collection, "collection")
  fetched <- gx_ref_get(
    gx_ref_url(client, "collections", collection_path, "queryables"),
    client,
    "application/schema+json, application/json;q=0.9",
    max_requests = .max_requests,
    max_total_bytes = .max_total_bytes,
    request_budget_scope = request_budget_scope,
    byte_budget_scope = byte_budget_scope
  )
  tryCatch({
  payload <- gx_ref_json(fetched$response, "queryables")
  if (!identical(payload$type %||% "", "object")) {
    gx_abort("Queryables payload must be a JSON object schema.", "gx_error_reference_payload")
  }
  properties <- payload$properties
  if (is.null(properties)) properties <- list()
  valid_names <- is.list(properties) && !is.null(names(properties)) &&
    all(nzchar(names(properties))) && !anyDuplicated(names(properties))
  if (length(properties) && !valid_names) {
    gx_abort("Queryables payload has invalid property names.", "gx_error_reference_payload")
  }
  if (!length(properties)) properties <- stats::setNames(list(), character())
  schemas <- unname(properties)
  property_names <- names(properties)
  if (any(property_names %in% c("contract_version", "feature_id"))) {
    gx_abort(
      "Queryables payload uses a package-reserved property name.",
      "gx_error_reference_payload"
    )
  }
  if (length(schemas) && any(!vapply(schemas, is.list, logical(1)))) {
    gx_abort("Queryable property schemas must be JSON objects.", "gx_error_reference_payload")
  }
  roles <- vapply(schemas, gx_ref_queryable_role, character(1))
  identity_roles <- !is.na(roles) & roles == "id"
  valid_identity_types <- vapply(schemas, function(schema) {
    types <- setdiff(gx_ref_schema_types(schema), "null")
    length(types) == 1L && types %in% c("string", "integer", "number")
  }, logical(1))
  if (any(identity_roles & !valid_identity_types)) {
    gx_abort(
      "Identity queryables must advertise one string or numeric JSON type.",
      "gx_error_reference_payload"
    )
  }
  out <- tibble::tibble(
    contract_version = rep(.gx_reference_contract_version, length(schemas)),
    collection_id = rep(as.character(collection), length(schemas)),
    name = property_names,
    json_types = unname(lapply(schemas, gx_ref_schema_types)),
    format = vapply(schemas, function(x) gx_ref_scalar_text(x$format), character(1)),
    title = vapply(schemas, function(x) gx_ref_scalar_text(x$title), character(1)),
    description = vapply(schemas, function(x) gx_ref_scalar_text(x$description), character(1)),
    enum = unname(lapply(schemas, function(x) unlist(x$enum %||% list(), recursive = TRUE, use.names = FALSE))),
    schema = schemas
  )
  metadata <- gx_ref_metadata(
    fetched$requests,
    fetched$response$url,
    collection = as.character(collection),
    additional_properties = payload$additionalProperties %||% NA
  )
  gx_ref_new_table(out, "gx_ref_queryables", metadata)
  }, error = function(cnd) {
    gx_ref_rethrow_with_requests(cnd, fetched$requests)
  })
}

#' @export
print.gx_ref_queryables <- function(x, ...) {
  gx_ref_print_table(x, "gx_ref_queryables", "Queryable properties")
}

gx_ref_query_value <- function(value, schema, name, enforce_bytes = TRUE) {
  scalar <- length(value) == 1L && !is.list(value) && !is.null(value) && !is.na(value)
  if (!scalar) {
    gx_abort(
      "Reference filter {.field {name}} must be one non-missing scalar value.",
      "gx_error_reference_query"
    )
  }
  value_text <- as.character(value)
  if (!isTRUE(stringi::stri_enc_isutf8(value_text))) {
    gx_abort(
      "Reference filter {.field {name}} must contain valid UTF-8.",
      "gx_error_reference_query"
    )
  }
  if (enforce_bytes && nchar(enc2utf8(value_text), type = "bytes") >
      gx_ref_query_value_byte_limit()) {
    gx_abort(
      "Reference filter {.field {name}} exceeds the configured byte budget.",
      "gx_error_reference_budget"
    )
  }
  types <- setdiff(gx_ref_schema_types(schema), "null")
  matches <- if (!length(types)) {
    FALSE
  } else {
    any(vapply(types, function(type) {
      switch(
        type,
        string = is.character(value) || inherits(value, "Date") || inherits(value, "POSIXt"),
        integer = (is.numeric(value) && is.finite(value) && value == trunc(value)) ||
          (is.character(value) && grepl("^-?[0-9]+$", value)),
        number = (is.numeric(value) && is.finite(value)) ||
          (is.character(value) && grepl("^-?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$", value)),
        boolean = is.logical(value) ||
          (is.character(value) && tolower(value) %in% c("true", "false")),
        FALSE
      )
    }, logical(1)))
  }
  if (!matches) {
    advertised <- if (length(types)) paste(types, collapse = "/") else "scalar"
    gx_abort(
      "Reference filter {.field {name}} does not match advertised type {advertised}.",
      "gx_error_reference_query"
    )
  }
  enum <- unlist(schema$enum %||% list(), recursive = TRUE, use.names = FALSE)
  if (length(enum) && !as.character(value) %in% as.character(enum)) {
    gx_abort(
      "Reference filter {.field {name}} is outside its advertised enum.",
      "gx_error_reference_query"
    )
  }
  if (inherits(value, "POSIXt")) {
    return(format(as.POSIXct(value, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  if (inherits(value, "Date")) return(format(value, "%Y-%m-%d"))
  if (is.logical(value)) return(if (value) "true" else "false")
  if (is.numeric(value)) return(sprintf("%.17g", value))
  enc2utf8(as.character(value))
}

gx_ref_control_parameters <- function() {
  c(
    "f", "limit", "bbox", "bbox-crs", "datetime", "crs", "offset",
    "startindex", "filter", "filter-lang", "filter-crs", "sortby",
    "properties", "resulttype", "skipgeometry"
  )
}

gx_ref_preflight_query <- function(query) {
  if (is.null(query)) query <- list()
  if (!is.list(query)) {
    gx_abort("{.arg query} must be a named list.", "gx_error_reference_query")
  }
  if (!length(query)) return(list())
  if (is.null(names(query)) || anyNA(names(query)) ||
      any(!nzchar(names(query))) || anyDuplicated(names(query))) {
    gx_abort("{.arg query} must have unique, non-empty names.", "gx_error_reference_query")
  }
  if (!all(stringi::stri_enc_isutf8(names(query)))) {
    gx_abort(
      "Reference filter names must contain valid UTF-8.",
      "gx_error_reference_query"
    )
  }
  if (any(nchar(enc2utf8(names(query)), type = "bytes") > 256L)) {
    gx_abort(
      "Reference filter names cannot exceed 256 UTF-8 bytes.",
      "gx_error_reference_budget"
    )
  }
  if (any(tolower(names(query)) %in% gx_ref_control_parameters())) {
    gx_abort(
      "{.arg query} cannot override controlled OGC request parameters.",
      "gx_error_reference_query"
    )
  }
  limit <- gx_ref_query_value_byte_limit()
  for (name in names(query)) {
    value <- query[[name]]
    scalar <- length(value) == 1L && !is.list(value) &&
      !is.null(value) && !is.na(value)
    if (!scalar) {
      gx_abort(
        "Reference filter {.field {name}} must be one non-missing scalar value.",
        "gx_error_reference_query"
      )
    }
    value_text <- as.character(value)
    if (!isTRUE(stringi::stri_enc_isutf8(value_text))) {
      gx_abort(
        "Reference filter {.field {name}} must contain valid UTF-8.",
        "gx_error_reference_query"
      )
    }
    if (nchar(enc2utf8(value_text), type = "bytes") > limit) {
      gx_abort(
        "Reference filter {.field {name}} exceeds the configured byte budget.",
        "gx_error_reference_budget"
      )
    }
  }
  query
}

gx_ref_validate_query <- function(query, queryables) {
  query <- gx_ref_preflight_query(query)
  if (!length(query)) return(list())
  unknown <- setdiff(names(query), queryables$name)
  if (length(unknown)) {
    gx_abort(
      "Unknown queryable propert{?y/ies}: {paste(unknown, collapse = ', ')}.",
      "gx_error_reference_query"
    )
  }
  out <- vector("list", length(query))
  names(out) <- names(query)
  for (i in seq_along(query)) {
    row <- match(names(query)[[i]], queryables$name)
    role <- gx_ref_queryable_role(queryables$schema[[row]])
    if (role %in% c("primary-geometry", "primary-instant", "primary-interval")) {
      gx_abort(
        "Queryable {.field {names(query)[[i]]}} is not a simple equality property.",
        "gx_error_reference_query"
      )
    }
    out[[i]] <- gx_ref_query_value(query[[i]], queryables$schema[[row]], names(query)[[i]])
  }
  out
}

gx_ref_bbox <- function(bbox) {
  if (is.null(bbox)) return(NULL)
  valid <- is.numeric(bbox) && length(bbox) %in% c(4L, 6L) &&
    !anyNA(bbox) && all(is.finite(bbox))
  axes <- if (length(bbox) == 4L) 2L else 3L
  ordered_axes <- if (axes == 2L) 2L else c(2L, 3L)
  if (!valid || any(bbox[ordered_axes] > bbox[axes + ordered_axes])) {
    gx_abort(
      "{.arg bbox} must contain four or six finite min/max coordinates.",
      "gx_error_reference_query"
    )
  }
  paste(format(bbox, scientific = FALSE, trim = TRUE, digits = 15L), collapse = ",")
}

gx_ref_limit <- function(limit) {
  gx_scalar_number(
    limit,
    "limit",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
}

gx_ref_total_byte_limit <- function(client) {
  gx_scalar_number(
    getOption(
      "geoconnexr.ref_total_bytes",
      min(as.double(.Machine$integer.max), 8 * as.double(client$max_bytes))
    ),
    "geoconnexr.ref_total_bytes",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
}

gx_ref_assert_total_bytes <- function(requests, limit,
                                      budget_scope = "reference") {
  total <- if (nrow(requests)) sum(requests$bytes) else 0
  if (total > limit) {
    gx_abort(
      "Reference retrieval exceeded its cumulative byte budget.",
      "gx_error_reference_budget",
      budget_kind = "bytes",
      budget_scope = budget_scope,
      requests = requests
    )
  }
  as.numeric(total)
}

gx_ref_remaining_total_bytes <- function(requests, limit,
                                         budget_scope = "reference") {
  total <- gx_ref_assert_total_bytes(requests, limit, budget_scope)
  remaining <- as.double(limit) - total
  if (!is.finite(remaining) || remaining < 1) {
    gx_abort(
      "Reference retrieval exhausted its cumulative byte budget.",
      "gx_error_reference_budget",
      budget_kind = "bytes",
      budget_scope = budget_scope,
      requests = requests
    )
  }
  as.integer(floor(remaining))
}

gx_ref_link_objects <- function(links) {
  if (is.null(links)) return(list())
  if (!is.list(links)) return(list())
  if (!is.null(names(links)) && any(nzchar(names(links)))) list(links) else links
}

gx_ref_next_href <- function(payload) {
  links <- gx_ref_link_objects(payload$links)
  for (link in links) {
    if (!is.list(link)) next
    rel <- as.character(unlist(link$rel %||% list(), recursive = TRUE, use.names = FALSE))
    href <- gx_ref_scalar_text(link$href)
    if ("next" %in% rel && !is.na(href) && nzchar(href)) return(href)
  }
  NULL
}

gx_ref_count <- function(value) {
  if (is.null(value) || !length(value)) return(NA_integer_)
  value <- unlist(value, recursive = TRUE, use.names = FALSE)
  if (length(value) != 1L || is.logical(value)) return(NA_integer_)
  value <- suppressWarnings(as.numeric(value[[1]]))
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0 ||
      value != trunc(value) || value > .Machine$integer.max) {
    return(NA_integer_)
  }
  as.integer(value)
}

gx_ref_valid_feature <- function(feature) {
  if (!is.list(feature) || is.null(names(feature)) || anyDuplicated(names(feature))) {
    return(FALSE)
  }
  if (!all(c("type", "geometry", "properties") %in% names(feature))) {
    return(FALSE)
  }
  id <- feature$id
  id_valid <- is.null(id) ||
    ((is.character(id) || is.numeric(id)) && length(id) == 1L && !is.na(id) &&
      (!is.numeric(id) || is.finite(id)))
  properties <- feature$properties
  properties_valid <- is.null(properties) ||
    (is.list(properties) && (!length(properties) || !is.null(names(properties))) &&
      !anyDuplicated(names(properties)))
  geometry <- feature$geometry
  geometry_valid <- is.null(geometry) ||
    (is.list(geometry) && !is.null(names(geometry)) &&
      !anyDuplicated(names(geometry)))
  identical(feature$type %||% "", "Feature") && id_valid &&
    properties_valid && geometry_valid
}

gx_ref_feature_array <- function(payload) {
  if (!identical(payload$type %||% "", "FeatureCollection") || is.null(payload$features) ||
      !is.list(payload$features)) {
    gx_abort(
      "Reference items response is not a GeoJSON FeatureCollection.",
      "gx_error_reference_payload"
    )
  }
  features <- payload$features
  if (length(features) && !is.null(names(features)) && any(nzchar(names(features)))) {
    gx_abort("FeatureCollection features must be a JSON array.", "gx_error_reference_payload")
  }
  valid <- vapply(features, gx_ref_valid_feature, logical(1))
  if (length(valid) && !all(valid)) {
    gx_abort("FeatureCollection contains an invalid feature.", "gx_error_reference_payload")
  }
  features
}

gx_ref_identifier_name <- function(name) {
  name <- tolower(name)
  name == "id" || endsWith(name, "_id") ||
    grepl("(^|_)(comid|fips|reachcode|levelpathi|huc([0-9]+)?)(_|$)", name, perl = TRUE)
}

gx_ref_queryable_role <- function(schema) {
  gx_ref_scalar_text(schema[["x-ogc-role"]])
}

gx_ref_identity_queryables <- function(queryables) {
  roles <- vapply(queryables$schema, gx_ref_queryable_role, character(1))
  names <- queryables$name[!is.na(roles) & roles == "id"]
  unique(names)
}

gx_ref_queryable_prototype <- function(schema, identifier = FALSE) {
  if (identifier) return(character())
  types <- setdiff(gx_ref_schema_types(schema), "null")
  if (length(types) != 1L) return(list())
  switch(
    types,
    string = character(),
    integer = integer(),
    number = numeric(),
    boolean = logical(),
    list()
  )
}

gx_ref_typed_column <- function(x, prototype, name) {
  if (is.list(prototype)) return(x)
  converted <- NULL
  valid <- !is.list(x)
  if (valid && is.character(prototype)) {
    converted <- as.character(x)
  } else if (valid && is.integer(prototype)) {
    numeric <- suppressWarnings(as.numeric(as.character(x)))
    valid <- all(is.na(x) | (!is.na(numeric) & is.finite(numeric) &
      numeric == trunc(numeric) & abs(numeric) <= .Machine$integer.max))
    if (valid) converted <- as.integer(numeric)
  } else if (valid && is.double(prototype)) {
    numeric <- suppressWarnings(as.numeric(as.character(x)))
    valid <- all(is.na(x) | (!is.na(numeric) & is.finite(numeric)))
    if (valid) converted <- numeric
  } else if (valid && is.logical(prototype)) {
    if (is.logical(x)) {
      converted <- x
    } else if (is.character(x) && all(is.na(x) | tolower(x) %in% c("true", "false"))) {
      converted <- ifelse(is.na(x), NA, tolower(x) == "true")
    } else {
      valid <- FALSE
    }
  }
  if (!valid || is.null(converted) || length(converted) != length(x) ||
      any(!is.na(x) & is.na(converted))) {
    gx_abort(
      "Reference property {.field {name}} does not match its advertised type.",
      "gx_error_reference_payload"
    )
  }
  converted
}

gx_ref_apply_queryables <- function(out, queryables, identity_values = NULL) {
  geometry <- sf::st_geometry(out)
  properties <- sf::st_drop_geometry(out)
  identity_names <- gx_ref_identity_queryables(queryables)
  geometry_roles <- vapply(queryables$schema, gx_ref_queryable_role, character(1))
  property_queryables <- queryables$name[!geometry_roles %in% c("primary-geometry", "primary-instant", "primary-interval")]
  for (name in property_queryables) {
    row <- match(name, queryables$name)
    identifier <- name %in% identity_names || gx_ref_identifier_name(name)
    prototype <- gx_ref_queryable_prototype(queryables$schema[[row]], identifier)
    if (!name %in% names(properties)) {
      properties[[name]] <- if (is.list(prototype)) {
        rep(list(NULL), nrow(out))
      } else {
        rep(prototype[NA_integer_], nrow(out))
      }
    } else {
      properties[[name]] <- gx_ref_typed_column(properties[[name]], prototype, name)
    }
  }
  if (!is.null(identity_values)) {
    for (name in identity_names) properties[[name]] <- as.character(identity_values)
  }
  for (name in names(properties)) {
    if ((name %in% identity_names || gx_ref_identifier_name(name)) && !is.list(properties[[name]])) {
      properties[[name]] <- as.character(properties[[name]])
    }
  }
  base_names <- intersect(c("contract_version", "feature_id"), names(properties))
  advertised_names <- intersect(property_queryables, names(properties))
  extra_names <- setdiff(names(properties), c(base_names, advertised_names))
  properties <- properties[c(base_names, advertised_names, extra_names)]
  sf::st_sf(properties, geometry = geometry)
}

gx_ref_validate_raw_properties <- function(feature, queryables,
                                           feature_index = NA_integer_) {
  properties <- feature$properties %||% list()
  diagnostics <- gx_empty_diagnostics()
  roles <- vapply(queryables$schema, gx_ref_queryable_role, character(1))
  property_rows <- which(!roles %in% c(
    "primary-geometry", "primary-instant", "primary-interval"
  ))
  for (row in property_rows) {
    name <- queryables$name[[row]]
    if (!name %in% names(properties)) next
    value <- properties[[name]]
    types <- gx_ref_schema_types(queryables$schema[[row]])
    if (identical(roles[[row]], "id")) {
      scalar_id <- !is.list(value) && (is.character(value) || is.numeric(value)) &&
        length(value) == 1L && !is.na(value) &&
        (!is.numeric(value) || is.finite(value))
      if (!scalar_id) {
        gx_abort(
          "Reference identity property {.field {name}} must be one scalar value.",
          "gx_error_reference_payload"
        )
      }
    }
    if (!length(types)) next
    if (is.null(value)) {
      if (!"null" %in% types) {
        path <- if (is.na(feature_index)) {
          paste0("/properties/", name)
        } else {
          paste0("/features/", feature_index - 1L, "/properties/", name)
        }
        diagnostics <- gx_bind_diagnostics(
          diagnostics,
          gx_diagnostic(
            "warning", "unexpected_null_property", path,
            paste0("Property ", name, " was null despite its advertised type.")
          )
        )
      }
      next
    }
    if (is.list(value)) {
      object <- !is.null(names(value))
      compatible <- (object && "object" %in% types) ||
        (!object && "array" %in% types)
      if (compatible) next
      gx_abort(
        "Reference property {.field {name}} must be a scalar value.",
        "gx_error_reference_payload"
      )
    }
    primitive <- intersect(types, c("string", "integer", "number", "boolean"))
    if (!length(primitive)) next
    tryCatch(
      gx_ref_query_value(
        value,
        queryables$schema[[row]],
        name,
        enforce_bytes = FALSE
      ),
      error = function(cnd) {
        gx_abort(
          "Reference property {.field {name}} does not match its advertised type.",
          "gx_error_reference_payload"
        )
      }
    )
  }
  diagnostics
}

gx_ref_features_sf <- function(features, queryables) {
  raw_diagnostics <- lapply(seq_along(features), function(index) {
    gx_ref_validate_raw_properties(
      features[[index]], queryables, feature_index = index
    )
  })
  raw_diagnostics <- do.call(
    gx_bind_diagnostics,
    c(list(gx_empty_diagnostics()), raw_diagnostics)
  )
  read_features <- features
  order_marker <- "gx_internal_row_order_6f83c19a"
  occupied <- unique(c(
    queryables$name,
    unlist(lapply(features, function(feature) names(feature$properties %||% list())))
  ))
  while (order_marker %in% occupied) order_marker <- paste0(order_marker, "_")
  if (length(read_features)) {
    for (index in seq_along(read_features)) {
      properties <- read_features[[index]]$properties %||% list()
      properties[[order_marker]] <- as.integer(index)
      read_features[[index]]$properties <- properties
    }
  }
  payload <- list(type = "FeatureCollection", features = read_features)
  text <- gx_json_serialize(payload)
  out <- tryCatch(
    suppressWarnings(sf::st_read(text, quiet = TRUE, stringsAsFactors = FALSE)),
    error = function(cnd) {
      gx_abort("Reference GeoJSON could not be converted to simple features.", "gx_error_reference_geometry")
    }
  )
  if (nrow(out) != length(features)) {
    gx_abort("Reference GeoJSON conversion changed the feature count.", "gx_error_reference_geometry")
  }
  if (length(features)) {
    if (!order_marker %in% names(out)) {
      gx_abort("Reference GeoJSON conversion lost its row-order marker.", "gx_error_reference_geometry")
    }
    marker <- suppressWarnings(as.integer(out[[order_marker]]))
    if (anyNA(marker) || !setequal(marker, seq_along(features))) {
      gx_abort("Reference GeoJSON conversion changed feature row identities.", "gx_error_reference_geometry")
    }
    out <- out[match(seq_along(features), marker), , drop = FALSE]
    out[[order_marker]] <- NULL
  }
  feature_id <- if (length(features)) {
    vapply(features, function(feature) gx_ref_scalar_text(feature$id), character(1))
  } else {
    character()
  }
  geometry <- sf::st_geometry(out)
  properties <- sf::st_drop_geometry(out)
  nested_names <- queryables$name[vapply(queryables$schema, function(schema) {
    length(intersect(gx_ref_schema_types(schema), c("object", "array"))) > 0L
  }, logical(1))]
  for (name in nested_names) {
    values <- unname(lapply(features, function(feature) {
      source <- feature$properties %||% list()
      if (name %in% names(source)) source[[name]] else NULL
    }))
    properties[name] <- list(values)
  }
  protected <- c("contract_version", "feature_id")
  for (name in intersect(names(properties), protected)) {
    names(properties)[names(properties) == name] <- paste0("property_", name)
  }
  names(properties) <- make.unique(names(properties), sep = "_")
  data <- data.frame(
    contract_version = rep(.gx_reference_contract_version, length(features)),
    feature_id = feature_id,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out <- sf::st_sf(data, properties, geometry = geometry)
  out <- gx_ref_apply_queryables(out, queryables)
  row.names(out) <- seq_len(nrow(out))
  attr(out, "gx_reference_diagnostics") <- raw_diagnostics
  out
}

gx_ref_features_meta <- function(x) attr(x, "gx_reference")

#' Retrieve bounded reference features
#'
#' Validates simple property filters against the collection's advertised
#' queryables and follows same-endpoint `rel=next` links until the requested
#' row limit or a deterministic stop condition is reached.
#' Query-bearing responses follow the package privacy policy and are not
#' persisted, so offline filtered replay is not promised.
#'
#' @param collection One advertised collection identifier.
#' @param query Named list of simple equality filters. Every name and scalar
#'   value is checked against [gx_ref_queryables()].
#' @param bbox Optional numeric bounding box with four or six coordinates.
#' @param limit Positive overall row limit, not merely a page size.
#' @param allow_unbounded Explicit opt-in for a request without `query` or
#'   `bbox`; row, page, response-byte, and cumulative-byte ceilings still apply.
#' @param client A reference client created by [gx_client()].
#'
#' @section Budgets:
#' The per-response ceiling comes from `client$max_bytes`. Additional options
#' bound the workflow: `geoconnexr.ref_page_size` (default 1000),
#' `geoconnexr.ref_max_pages` (100), `geoconnexr.ref_total_bytes` (eight times
#' the client response ceiling), and `geoconnexr.ref_max_members` (250,000 JSON
#' members per response). Filter values default to 4,096 bytes through
#' `geoconnexr.ref_max_query_value_bytes`, and every reference URL defaults to
#' 16,384 bytes through `geoconnexr.ref_max_url_bytes`. Invalid option values
#' fail before an items request.
#'
#' @return A `gx_ref_features` simple-feature table. Top-level GeoJSON IDs are
#'   normalized to character `feature_id`, and hydrologic identifier property
#'   columns are also character. The `gx_reference` attribute records
#'   `truncated`, `stop_reason`, `number_matched`, page and byte counts,
#'   diagnostics, and the redacted request ledger.
#' @export
gx_ref_features <- function(collection, query = list(), bbox = NULL,
                            limit = 1000L, allow_unbounded = FALSE,
                            client = gx_client("reference")) {
  gx_ref_features_impl(
    collection = collection, query = query, bbox = bbox, limit = limit,
    allow_unbounded = allow_unbounded, client = client
  )
}

gx_ref_features_impl <- function(collection, query = list(), bbox = NULL,
                                 limit = 1000L, allow_unbounded = FALSE,
                                 client = gx_client("reference"),
                                 .queryables = NULL, .max_pages = NULL,
                                 .max_total_bytes = NULL,
                                 .max_requests = NULL,
                                 .byte_budget_scope = NULL) {
  gx_ref_client(client)
  collection_path <- gx_ref_path_segment(collection, "collection")
  limit <- gx_ref_limit(limit)
  allow_unbounded <- gx_ref_flag(allow_unbounded, "allow_unbounded")
  bbox_value <- gx_ref_bbox(bbox)
  if (is.null(query)) query <- list()
  if (!is.list(query)) {
    gx_abort("{.arg query} must be a named list.", "gx_error_reference_query")
  }
  query <- gx_ref_preflight_query(query)
  if (!length(query) && is.null(bbox_value) && !allow_unbounded) {
    gx_abort(
      "Unfiltered reference retrieval requires {.code allow_unbounded = TRUE}.",
      "gx_error_reference_unbounded"
    )
  }

  page_size <- gx_scalar_number(
    getOption("geoconnexr.ref_page_size", 1000L),
    "geoconnexr.ref_page_size",
    minimum = 1,
    maximum = 100000,
    integer = TRUE
  )
  max_pages <- gx_scalar_number(
    getOption("geoconnexr.ref_max_pages", 100L),
    "geoconnexr.ref_max_pages",
    minimum = 1,
    maximum = 100000,
    integer = TRUE
  )
  if (!is.null(.max_pages)) {
    max_pages <- min(max_pages, gx_scalar_number(
      .max_pages,
      ".max_pages",
      minimum = 1,
      maximum = 100000,
      integer = TRUE
    ))
  }
  max_total_bytes <- gx_ref_total_byte_limit(client)
  byte_budget_scope <- "reference"
  if (!is.null(.max_total_bytes)) {
    injected_total_bytes <- gx_scalar_number(
      .max_total_bytes,
      ".max_total_bytes",
      minimum = 1,
      maximum = .Machine$integer.max,
      integer = TRUE
    )
    if (injected_total_bytes <= max_total_bytes) {
      byte_budget_scope <- "injected"
    }
    max_total_bytes <- min(max_total_bytes, injected_total_bytes)
  }
  if (!is.null(.byte_budget_scope)) {
    if (!.byte_budget_scope %in% c("reference", "injected")) {
      gx_abort("Internal feature byte-budget scope is invalid.", "gx_error_reference_client")
    }
    byte_budget_scope <- .byte_budget_scope
  }
  if (!is.null(.max_requests)) {
    if (is.null(.queryables)) {
      gx_abort(
        "Internal request-attempt limits require prevalidated queryables.",
        "gx_error_reference_client"
      )
    }
    .max_requests <- gx_scalar_number(
      .max_requests,
      ".max_requests",
      minimum = 1,
      maximum = 100000,
      integer = TRUE
    )
  }

  if (is.null(.queryables)) {
    queryables <- gx_ref_queryables_impl(
      collection,
      client = client,
      .max_total_bytes = max_total_bytes,
      .byte_budget_scope = byte_budget_scope
    )
    queryable_requests <- gx_ref_features_meta(queryables)$requests
  } else {
    if (!inherits(.queryables, "gx_ref_queryables") ||
        any(.queryables$collection_id != as.character(collection))) {
      gx_abort("Internal queryables do not match the collection.", "gx_error_reference_client")
    }
    queryables <- .queryables
    queryable_requests <- gx_ref_empty_requests()
  }
  requests <- queryable_requests
  tryCatch({
  if (length(query)) {
    query <- gx_ref_validate_query(query, queryables)
  }
  params <- c(
    list(f = "json"),
    query,
    if (is.null(bbox_value)) list() else list(bbox = bbox_value),
    list(limit = as.character(min(limit, page_size)))
  )
  current <- do.call(
    httr2::url_modify_query,
    c(list(.url = gx_ref_url(client, "collections", collection_path, "items")), params)
  )
  current <- gx_canonical_url(current)
  gx_ref_assert_endpoint_url(current, client)
  seen <- gx_ref_url_key(current)
  total_bytes <- gx_ref_assert_total_bytes(
    requests,
    max_total_bytes,
    budget_scope = byte_budget_scope
  )
  features <- list()
  diagnostics <- gx_empty_diagnostics()
  number_matched <- NA_integer_
  number_matched_consistent <- TRUE
  pages <- 0L
  truncated <- FALSE
  stop_reason <- "no_next"
  source_url <- current

  repeat {
    if (pages >= max_pages) {
      truncated <- TRUE
      stop_reason <- "page_budget"
      break
    }
    remaining_requests <- if (is.null(.max_requests)) {
      NULL
    } else {
      .max_requests - nrow(requests)
    }
    if (!is.null(remaining_requests) && remaining_requests < 1L) {
      gx_abort(
        "Reference pagination exhausted its request-attempt budget.",
        "gx_error_reference_budget",
        budget_kind = "requests",
        budget_scope = "injected",
        requests = requests
      )
    }
    remaining_bytes <- max_total_bytes - sum(as.double(requests$bytes), na.rm = TRUE)
    if (!is.finite(remaining_bytes) || remaining_bytes < 1) {
      gx_abort(
        "Reference pagination exhausted its cumulative-byte budget.",
        "gx_error_reference_budget",
        budget_kind = "bytes",
        budget_scope = byte_budget_scope,
        requests = requests
      )
    }
    fetched <- gx_ref_get(
      current,
      client,
      "application/geo+json, application/json;q=0.9",
      required_query = list(f = "json"),
      max_requests = remaining_requests,
      max_total_bytes = remaining_bytes,
      request_budget_scope = if (is.null(.max_requests)) "reference" else "injected",
      byte_budget_scope = byte_budget_scope
    )
    pages <- pages + 1L
    requests <- rbind(requests, fetched$requests)
    total_bytes <- gx_ref_assert_total_bytes(
      requests,
      max_total_bytes,
      budget_scope = byte_budget_scope
    )
    source_url <- fetched$response$url
    payload <- gx_ref_json(fetched$response, "features")
    page_features <- gx_ref_feature_array(payload)
    page_matched <- gx_ref_count(payload$numberMatched)
    if (!is.null(payload$numberMatched) && is.na(page_matched)) {
      diagnostics <- gx_bind_diagnostics(
        diagnostics,
        gx_diagnostic("warning", "invalid_number_matched", "", "numberMatched was not one non-negative integer.")
      )
      number_matched_consistent <- FALSE
      number_matched <- NA_integer_
    } else if (!is.na(page_matched) && number_matched_consistent) {
      if (is.na(number_matched)) {
        number_matched <- page_matched
      } else if (!identical(number_matched, page_matched)) {
        diagnostics <- gx_bind_diagnostics(
          diagnostics,
          gx_diagnostic("warning", "number_matched_changed", "", "numberMatched changed between pages.")
        )
        number_matched_consistent <- FALSE
        number_matched <- NA_integer_
      }
    }
    number_returned <- gx_ref_count(payload$numberReturned)
    if (!is.null(payload$numberReturned) && is.na(number_returned)) {
      diagnostics <- gx_bind_diagnostics(
        diagnostics,
        gx_diagnostic("warning", "invalid_number_returned", "", "numberReturned was not one non-negative integer.")
      )
    }
    if (!is.na(number_returned) && number_returned != length(page_features)) {
      diagnostics <- gx_bind_diagnostics(
        diagnostics,
        gx_diagnostic("warning", "number_returned_mismatch", "", "numberReturned did not match the page feature count.")
      )
    }
    next_href <- gx_ref_next_href(payload)
    if (!length(page_features)) {
      if (!is.na(number_matched) && length(features) >= number_matched) {
        stop_reason <- "number_matched"
        truncated <- FALSE
      } else {
        stop_reason <- "empty_page"
        truncated <- (!is.null(next_href) || (!is.na(number_matched) && length(features) < number_matched))
      }
      break
    }

    previous_count <- length(features)
    remaining <- limit - length(features)
    take <- min(remaining, length(page_features))
    if (take > 0L) features <- c(features, page_features[seq_len(take)])
    page_overflow <- length(page_features) > take
    reached_limit <- length(features) >= limit
    if (!is.na(number_matched) && previous_count + length(page_features) > number_matched) {
      diagnostics <- gx_bind_diagnostics(
        diagnostics,
        gx_diagnostic("warning", "number_matched_too_small", "", "numberMatched was smaller than the returned feature count.")
      )
      number_matched_consistent <- FALSE
      number_matched <- NA_integer_
    }
    if (!is.na(number_matched) && length(features) >= number_matched) {
      stop_reason <- "number_matched"
      break
    }
    if (reached_limit) {
      stop_reason <- "limit"
      truncated <- page_overflow || !is.null(next_href) ||
        (!is.na(number_matched) && number_matched > length(features))
      break
    }
    if (is.null(next_href)) {
      stop_reason <- "no_next"
      if (!is.na(number_matched) && length(features) < number_matched) {
        truncated <- TRUE
        diagnostics <- gx_bind_diagnostics(
          diagnostics,
          gx_diagnostic("warning", "missing_next_link", "", "The response omitted a next link before numberMatched was reached.")
        )
      }
      break
    }
    next_url <- tryCatch(
      httr2::url_modify_relative(fetched$response$url, next_href),
      error = function(cnd) NA_character_
    )
    if (is.na(next_url)) {
      gx_abort("Reference next link is invalid.", "gx_error_reference_next")
    }
    next_url <- gx_ref_require_query(next_url, list(f = "json"))
    gx_ref_assert_endpoint_url(next_url, client)
    next_url <- gx_canonical_url(next_url)
    next_key <- gx_ref_url_key(next_url)
    if (next_key %in% seen) {
      truncated <- TRUE
      stop_reason <- "repeated_next"
      break
    }
    current <- next_url
    seen <- c(seen, next_key)
  }

  out <- gx_ref_features_sf(features, queryables)
  diagnostics <- gx_bind_diagnostics(
    diagnostics,
    attr(out, "gx_reference_diagnostics") %||% gx_empty_diagnostics()
  )
  attr(out, "gx_reference_diagnostics") <- NULL
  metadata <- gx_ref_metadata(
    requests,
    source_url,
    diagnostics,
    collection = as.character(collection),
    query = query,
    bbox = bbox,
    limit = limit,
    number_matched = number_matched,
    number_returned = nrow(out),
    pages = pages,
    bytes = as.numeric(total_bytes),
    truncated = truncated,
    complete = !truncated,
    retrieval_mode = "items",
    content_crs = sf::st_crs(out)$input %||% NA_character_,
    stop_reason = stop_reason
  )
  gx_ref_new_table(out, "gx_ref_features", metadata)
  }, error = function(cnd) {
    gx_ref_rethrow_with_requests(cnd, requests)
  })
}

#' @export
print.gx_ref_features <- function(x, ...) {
  original <- x
  metadata <- attr(x, "gx_reference")
  cli::cli_inform(c(
    "<gx_ref_features>",
    "* Collection: {metadata$collection}",
    "* Features: {nrow(x)}; pages: {metadata$pages}",
    "* Truncated: {metadata$truncated} ({metadata$stop_reason})"
  ))
  class(x) <- setdiff(class(x), "gx_ref_features")
  print(x, ...)
  invisible(original)
}

gx_ref_empty_attempts <- function() {
  tibble::tibble(
    stage = character(), status = integer(), code = character(),
    recoverable = logical()
  )
}

gx_ref_attempt <- function(stage, status = NA_integer_, code = NA_character_, recoverable = TRUE) {
  tibble::tibble(
    stage = as.character(stage), status = as.integer(status),
    code = as.character(code), recoverable = as.logical(recoverable)
  )
}

gx_ref_error_code <- function(cnd) {
  classes <- class(cnd)
  match <- classes[grepl("^gx_error_", classes)]
  if (length(match)) sub("^gx_error_", "", match[[1]]) else "error"
}

gx_ref_item_features <- function(payload) {
  if (identical(payload$type %||% "", "Feature")) {
    if (!gx_ref_valid_feature(payload)) {
      gx_abort("Reference item response contains an invalid feature.", "gx_error_reference_payload")
    }
    return(list(payload))
  }
  if (identical(payload$type %||% "", "FeatureCollection")) {
    features <- gx_ref_feature_array(payload)
    if (length(features) != 1L) {
      gx_abort(
        "Reference item response did not identify exactly one feature.",
        "gx_error_reference_ambiguous"
      )
    }
    matched <- gx_ref_count(payload$numberMatched)
    returned <- gx_ref_count(payload$numberReturned)
    invalid_count <- (!is.null(payload$numberMatched) && is.na(matched)) ||
      (!is.null(payload$numberReturned) && is.na(returned))
    if (invalid_count) {
      gx_abort(
        "Reference item response contains an invalid feature count.",
        "gx_error_reference_payload"
      )
    }
    if ((!is.na(matched) && matched != 1L) ||
        (!is.na(returned) && returned != 1L) ||
        !is.null(gx_ref_next_href(payload))) {
      gx_abort(
        "Reference item response may contain additional features.",
        "gx_error_reference_ambiguous"
      )
    }
    return(features)
  }
  gx_abort("Reference item response is not a GeoJSON feature.", "gx_error_reference_payload")
}

gx_ref_feature_matches_id <- function(feature, id, identity_names) {
  if (length(identity_names) != 1L) return(FALSE)
  top_level <- gx_ref_scalar_text(feature$id)
  properties <- feature$properties %||% list()
  property_name <- identity_names[[1]]
  property_present <- property_name %in% names(properties)
  property <- if (property_present) properties[[property_name]] else NULL
  property_valid <- !property_present ||
    ((!is.list(property) && (is.character(property) || is.numeric(property))) &&
      length(property) == 1L && !is.na(property) &&
      (!is.numeric(property) || is.finite(property)))
  property_values <- if (property_valid && property_present) {
    as.character(property)
  } else {
    character()
  }
  candidates <- c(if (is.na(top_level)) character() else top_level, property_values)
  property_valid && length(candidates) > 0L &&
    all(candidates == as.character(id))
}

gx_ref_sf_feature_matches_id <- function(x, row, id, identity_names) {
  if (length(identity_names) != 1L) return(FALSE)
  top_level <- x$feature_id[[row]]
  candidates <- if (is.na(top_level)) character() else as.character(top_level)
  identity_name <- identity_names[[1]]
  if (identity_name %in% names(x)) {
    property <- x[[identity_name]][[row]]
    property_values <- unlist(property, recursive = TRUE, use.names = FALSE)
    property_values <- as.character(property_values[!is.na(property_values)])
    if (length(property_values) > 1L) return(FALSE)
    candidates <- c(candidates, property_values)
  }
  length(candidates) > 0L && all(candidates == as.character(id))
}

gx_ref_direct_jsonld <- function(url, identity_uri, client,
                                 .max_total_bytes = NULL,
                                 .byte_budget_scope = NULL) {
  byte_budget_scope <- .byte_budget_scope %||%
    if (is.null(.max_total_bytes)) "reference" else "injected"
  fetched <- gx_ref_get(
    url,
    client,
    "application/ld+json, application/json;q=0.9",
    check_status = TRUE,
    max_total_bytes = .max_total_bytes,
    byte_budget_scope = byte_budget_scope
  )
  tryCatch({
  value <- gx_ref_json(
    fetched$response,
    "JSON-LD feature",
    require_object = FALSE
  )
  prepared <- gx_prepare_jsonld(value, base = fetched$response$url)
  text <- gx_json_serialize(value)
  document <- structure(
    list(
      contract_version = .gx_jsonld_contract_version,
      representation = "expanded",
      pid_uri = identity_uri,
      landing_url = fetched$response$url,
      source_url = fetched$response$url,
      media_type = gx_media_type(fetched$response$headers),
      retrieval_mode = "reference_negotiated",
      retrieved_at = fetched$response$retrieved_at,
      content_sha256 = digest::digest(charToRaw(enc2utf8(text)), algo = "sha256", serialize = FALSE),
      content_bytes = nchar(enc2utf8(text), type = "bytes"),
      response_sha256 = fetched$response$body_sha256,
      document = prepared$expanded,
      source_document = value,
      expanded = prepared$expanded,
      resolution = NULL,
      requests = fetched$requests,
      diagnostics = prepared$diagnostics
    ),
    class = "gx_jsonld"
  )
  list(document = document, fetched = fetched)
  }, error = function(cnd) {
    gx_ref_rethrow_with_requests(cnd, fetched$requests)
  })
}

gx_ref_wkt_sfc <- function(wkt, crs_uri = NA_character_) {
  if (is.na(wkt) || !nzchar(trimws(wkt))) {
    return(sf::st_sfc(sf::st_geometrycollection(), crs = 4326))
  }
  lexical <- sub("^<[^>]+>[[:space:]]*", "", trimws(wkt))
  crs <- if (!is.na(crs_uri) && grepl("(CRS84|/4326)$", crs_uri, ignore.case = TRUE)) 4326 else NA
  tryCatch(
    sf::st_as_sfc(lexical, crs = crs),
    error = function(cnd) {
      gx_abort("JSON-LD fallback contains invalid WKT geometry.", "gx_error_reference_geometry")
    }
  )
}

gx_ref_jsonld_feature_sf <- function(document, id) {
  locations <- gx_parse_location(document)
  if (nrow(locations)) {
    exact <- which(!is.na(locations$site_uri) & locations$site_uri == document$pid_uri)
    row <- if (length(exact)) exact[[1]] else 1L
    location <- locations[row, , drop = FALSE]
    geometry <- gx_ref_wkt_sfc(location$geometry_wkt[[1]], location$geometry_crs[[1]])
    location$geometry_wkt <- NULL
    location$geometry_crs <- NULL
    location$contract_version <- .gx_reference_contract_version
    location$feature_id <- as.character(id)
    location <- location[c("contract_version", "feature_id", setdiff(names(location), c("contract_version", "feature_id")))]
    out <- sf::st_sf(location, geometry = geometry)
    attr(out, "gx_source_identity") <- locations$site_uri[[row]]
    return(out)
  }

  nodes <- gx_merge_graph_nodes(gx_flatten_graph(document$expanded))
  exact <- which(vapply(nodes, function(node) {
    identical(as.character(node[["@id"]] %||% NA_character_), document$pid_uri)
  }, logical(1)))
  node <- if (length(exact)) nodes[[exact[[1]]]] else if (length(nodes)) nodes[[1]] else NULL
  if (is.null(node)) {
    gx_abort("JSON-LD fallback contains no feature node.", "gx_error_reference_payload")
  }
  geometries <- gx_prop(node, paste0(.gx_gsp, "hasGeometry"))
  geometry_node <- if (length(geometries)) geometries[[1]] else list()
  wkt <- gx_first_text(gx_prop(geometry_node, paste0(.gx_gsp, "asWKT")))
  crs_uri <- gx_first_iri(gx_prop(geometry_node, paste0(.gx_gsp, "crs")))
  geometry <- gx_ref_wkt_sfc(wkt, crs_uri)
  out <- tibble::tibble(
    contract_version = .gx_reference_contract_version,
    feature_id = as.character(id),
    name = gx_first_text(gx_prop(node, gx_schema_terms("name"))),
    description = gx_first_text(gx_prop(node, gx_schema_terms("description"))),
    rdf_types = list(gx_node_types(node)),
    source_url = document$source_url
  )
  out <- sf::st_sf(out, geometry = geometry)
  attr(out, "gx_source_identity") <- as.character(node[["@id"]] %||% NA_character_)
  out
}

gx_ref_new_feature <- function(x, metadata) {
  gx_ref_validate_feature(x, metadata)
  attr(x, "gx_reference") <- metadata
  class(x) <- unique(c("gx_ref_feature", "gx_ref_features", class(x)))
  x
}

gx_ref_can_fallback <- function(cnd, stage) {
  allowed <- c(
    "gx_error_reference_http", "gx_error_reference_content_type",
    "gx_error_reference_payload", "gx_error_reference_geometry",
    "gx_error_reference_identity", "gx_error_reference_ambiguous"
  )
  any(vapply(allowed, function(class) inherits(cnd, class), logical(1))) ||
    (identical(stage, "item") && inherits(cnd, "gx_error_payload_too_large")) ||
    (identical(stage, "filter") && inherits(cnd, "gx_error_reference_query"))
}

gx_ref_condition_requests <- function(cnd) {
  candidate <- cnd$requests
  required <- names(gx_ref_empty_requests())
  if (!is.data.frame(candidate) || !all(required %in% names(candidate))) {
    return(gx_ref_empty_requests())
  }
  tibble::as_tibble(candidate[required])
}

gx_ref_merge_requests <- function(x, y) {
  out <- rbind(x, y)
  row.names(out) <- NULL
  out
}

gx_ref_rethrow_with_requests <- function(cnd, requests) {
  if (inherits(cnd, "gx_error")) {
    condition_requests <- gx_ref_condition_requests(cnd)
    cnd$requests <- if (identical(as.data.frame(requests), as.data.frame(condition_requests))) {
      requests
    } else {
      gx_ref_merge_requests(requests, condition_requests)
    }
  }
  stop(cnd)
}

gx_ref_condition_status <- function(cnd) {
  status <- cnd$status
  if (is.numeric(status) && length(status) == 1L && !is.na(status)) {
    return(as.integer(status))
  }
  requests <- gx_ref_condition_requests(cnd)
  if (nrow(requests)) return(utils::tail(requests$status, 1L))
  NA_integer_
}

#' Retrieve one reference feature with compatible fallbacks
#'
#' Tries the OGC item route first, then a collection filter using the
#' queryable marked `x-ogc-role: id`, and finally direct JSON-LD negotiation on
#' the item URL. Every successful path verifies the requested identity. The
#' JSON-LD path is marked incomplete because it can expose fewer properties
#' than GeoJSON.
#'
#' @param collection One advertised collection identifier.
#' @param id One feature identifier. It is always returned as character.
#' @param client A reference client created by [gx_client()].
#'
#' @return A one-row `gx_ref_feature` simple-feature table. Its
#'   `gx_reference` attribute records `retrieval_mode`, `complete`, attempts,
#'   diagnostics, and a redacted ledger for every physical response or transport
#'   attempt in the workflow, plus cache retrievals. A stage that fails before
#'   receiving a response remains visible in `attempts` with a missing status.
#' @export
gx_ref_feature <- function(collection, id, client = gx_client("reference")) {
  gx_ref_client(client)
  max_total_bytes <- gx_ref_total_byte_limit(client)
  collection_path <- gx_ref_path_segment(collection, "collection")
  id_path <- gx_ref_path_segment(id, "id")
  id <- as.character(id)
  item_url <- gx_ref_url(client, "collections", collection_path, "items", id_path)
  item_json_url <- httr2::url_modify_query(item_url, f = "json")
  requests <- gx_ref_empty_requests()
  attempts <- gx_ref_empty_attempts()
  item_response_status <- NA_integer_
  jsonld_response_status <- NA_integer_
  diagnostics <- gx_empty_diagnostics()
  queryables_cache <- NULL
  probe_client <- client
  probe_client$retries <- 0L
  if (client$retries > 0L) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("info", "fallback_probe_retries_disabled", "", "Item and JSON-LD fallback probes disabled transport retries.")
    )
  }

  item_result <- tryCatch({
    fetched <- gx_ref_get(
      item_json_url,
      probe_client,
      "application/geo+json, application/json;q=0.9",
      check_status = FALSE,
      required_query = list(f = "json"),
      max_total_bytes = gx_ref_remaining_total_bytes(requests, max_total_bytes),
      byte_budget_scope = "reference"
    )
    item_response_status <- fetched$response$status
    requests <- rbind(requests, fetched$requests)
    gx_ref_assert_total_bytes(requests, max_total_bytes)
    if (fetched$response$status < 200L || fetched$response$status >= 300L) {
      attempts <- rbind(attempts, gx_ref_attempt("item", fetched$response$status, paste0("http_", fetched$response$status)))
      NULL
    } else {
      payload <- gx_ref_json(fetched$response, "feature")
      features <- gx_ref_item_features(payload)
      queryables <- gx_ref_queryables_impl(
        collection,
        client = client,
        .max_total_bytes = gx_ref_remaining_total_bytes(requests, max_total_bytes),
        .byte_budget_scope = "reference"
      )
      queryables_cache <- queryables
      requests <- rbind(requests, gx_ref_features_meta(queryables)$requests)
      gx_ref_assert_total_bytes(requests, max_total_bytes)
      identity_names <- gx_ref_identity_queryables(queryables)
      if (!gx_ref_feature_matches_id(features[[1]], id, identity_names)) {
        gx_abort(
          "Reference item identity did not match the request.",
          "gx_error_reference_identity",
          status = fetched$response$status
        )
      }
      out <- gx_ref_features_sf(features, queryables)
      diagnostics <- gx_bind_diagnostics(
        diagnostics,
        attr(out, "gx_reference_diagnostics") %||% gx_empty_diagnostics()
      )
      attr(out, "gx_reference_diagnostics") <- NULL
      out$feature_id <- id
      attempts <- rbind(attempts, gx_ref_attempt("item", fetched$response$status, NA_character_, FALSE))
      metadata <- gx_ref_metadata(
        requests, fetched$response$url, diagnostics,
        collection = as.character(collection), id = id, retrieval_mode = "item",
        complete = TRUE, truncated = FALSE, attempts = attempts,
        number_matched = 1L, number_returned = 1L, pages = 1L,
        bytes = sum(requests$bytes), stop_reason = "item"
      )
      gx_ref_new_feature(out, metadata)
    }
  }, error = function(cnd) {
    if (!inherits(cnd, "gx_error")) stop(cnd)
    if (!gx_ref_can_fallback(cnd, "item")) {
      gx_ref_rethrow_with_requests(cnd, requests)
    }
    requests <<- gx_ref_merge_requests(requests, gx_ref_condition_requests(cnd))
    gx_ref_assert_total_bytes(requests, max_total_bytes)
    status <- gx_ref_condition_status(cnd)
    if (is.na(status)) status <- item_response_status
    attempts <<- rbind(attempts, gx_ref_attempt(
      "item", status, gx_ref_error_code(cnd)
    ))
    NULL
  })
  if (!is.null(item_result)) return(item_result)

  filter_result <- tryCatch({
    queryables <- queryables_cache
    if (is.null(queryables)) {
      queryables <- gx_ref_queryables_impl(
        collection,
        client = client,
        .max_total_bytes = gx_ref_remaining_total_bytes(requests, max_total_bytes),
        .byte_budget_scope = "reference"
      )
      queryables_cache <- queryables
      requests <- rbind(requests, gx_ref_features_meta(queryables)$requests)
      gx_ref_assert_total_bytes(requests, max_total_bytes)
    }
    identity_names <- gx_ref_identity_queryables(queryables)
    if (length(identity_names) != 1L) {
      gx_abort(
        "Collection does not advertise exactly one identity queryable.",
        "gx_error_reference_identity"
      )
    }
    query <- stats::setNames(list(id), identity_names)
    candidates <- gx_ref_features_impl(
      collection,
      query = query,
      limit = 2L,
      client = client,
      .queryables = queryables,
      .max_total_bytes = gx_ref_remaining_total_bytes(requests, max_total_bytes),
      .byte_budget_scope = "reference"
    )
    candidate_meta <- gx_ref_features_meta(candidates)
    requests <- rbind(requests, candidate_meta$requests)
    gx_ref_assert_total_bytes(requests, max_total_bytes)
    exact <- which(vapply(seq_len(nrow(candidates)), function(row) {
      gx_ref_sf_feature_matches_id(candidates, row, id, identity_names)
    }, logical(1)))
    compatible <- nrow(candidates) == 1L && length(exact) == 1L &&
      !isTRUE(candidate_meta$truncated)
    if (!compatible) {
      class_name <- if (nrow(candidates) > 1L || length(exact) > 1L ||
          isTRUE(candidate_meta$truncated)) {
        "gx_error_reference_ambiguous"
      } else {
        "gx_error_reference_identity"
      }
      gx_abort(
        "Filtered reference lookup did not return one exact identity.",
        class_name,
        status = utils::tail(candidate_meta$requests$status, 1L)
      )
    }
    out <- candidates[exact[[1]], , drop = FALSE]
    out$feature_id <- id
    attempts <- rbind(attempts, gx_ref_attempt(
      "filter", utils::tail(candidate_meta$requests$status, 1L), NA_character_, FALSE
    ))
    metadata <- candidate_meta
    metadata$requests <- requests
    metadata$source_url <- candidate_meta$source_url
    metadata$retrieved_at <- max(requests$retrieved_at)
    metadata$id <- id
    metadata$retrieval_mode <- "filter"
    metadata$complete <- TRUE
    metadata$truncated <- FALSE
    metadata$attempts <- attempts
    metadata$number_matched <- 1L
    metadata$number_returned <- 1L
    metadata$stop_reason <- "filter"
    metadata$bytes <- sum(requests$bytes)
    gx_ref_new_feature(out, metadata)
  }, error = function(cnd) {
    if (!inherits(cnd, "gx_error")) stop(cnd)
    if (!gx_ref_can_fallback(cnd, "filter")) {
      gx_ref_rethrow_with_requests(cnd, requests)
    }
    requests <<- gx_ref_merge_requests(requests, gx_ref_condition_requests(cnd))
    gx_ref_assert_total_bytes(requests, max_total_bytes)
    attempts <<- rbind(attempts, gx_ref_attempt(
      "filter", gx_ref_condition_status(cnd), gx_ref_error_code(cnd)
    ))
    NULL
  })
  if (!is.null(filter_result)) return(filter_result)

  jsonld_result <- tryCatch({
    negotiated <- gx_ref_direct_jsonld(
      item_url,
      item_url,
      probe_client,
      .max_total_bytes = gx_ref_remaining_total_bytes(requests, max_total_bytes),
      .byte_budget_scope = "reference"
    )
    jsonld_response_status <- negotiated$fetched$response$status
    requests <- rbind(requests, negotiated$fetched$requests)
    gx_ref_assert_total_bytes(requests, max_total_bytes)
    out <- gx_ref_jsonld_feature_sf(negotiated$document, id)
    source_identity <- attr(out, "gx_source_identity")
    expected_pid <- paste0(
      sub("/+$", "", gx_endpoints()[["pid"]]),
      "/ref/", collection_path, "/", id_path
    )
    if (!is.character(source_identity) || length(source_identity) != 1L ||
        is.na(source_identity) || !source_identity %in% c(item_url, expected_pid)) {
      gx_abort(
        "JSON-LD fallback identity did not match the request.",
        "gx_error_reference_identity",
        status = negotiated$fetched$response$status
      )
    }
    if (is.null(queryables_cache)) {
      queryables_cache <- gx_ref_queryables_impl(
        collection,
        client = client,
        .max_total_bytes = gx_ref_remaining_total_bytes(requests, max_total_bytes),
        .byte_budget_scope = "reference"
      )
      requests <- rbind(requests, gx_ref_features_meta(queryables_cache)$requests)
      gx_ref_assert_total_bytes(requests, max_total_bytes)
    }
    identity_names <- gx_ref_identity_queryables(queryables_cache)
    if (length(identity_names) != 1L) {
      gx_abort(
        "Collection does not advertise exactly one identity queryable.",
        "gx_error_reference_identity",
        status = negotiated$fetched$response$status
      )
    }
    out <- gx_ref_apply_queryables(out, queryables_cache, identity_values = id)
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      negotiated$document$diagnostics,
      gx_diagnostic("warning", "jsonld_fallback_incomplete", "", "JSON-LD fallback may expose fewer properties than the GeoJSON feature.")
    )
    attempts <- rbind(attempts, gx_ref_attempt(
      "jsonld", negotiated$fetched$response$status, NA_character_, FALSE
    ))
    metadata <- gx_ref_metadata(
      requests, negotiated$fetched$response$url, diagnostics,
      collection = as.character(collection), id = id, retrieval_mode = "jsonld",
      complete = FALSE, truncated = FALSE, attempts = attempts,
      number_matched = 1L, number_returned = 1L, pages = 1L,
      bytes = sum(requests$bytes), stop_reason = "jsonld"
    )
    gx_ref_new_feature(out, metadata)
  }, error = function(cnd) {
    if (!inherits(cnd, "gx_error")) stop(cnd)
    if (!gx_ref_can_fallback(cnd, "jsonld")) {
      gx_ref_rethrow_with_requests(cnd, requests)
    }
    requests <<- gx_ref_merge_requests(requests, gx_ref_condition_requests(cnd))
    gx_ref_assert_total_bytes(requests, max_total_bytes)
    status <- gx_ref_condition_status(cnd)
    if (is.na(status)) status <- jsonld_response_status
    attempts <<- rbind(attempts, gx_ref_attempt(
      "jsonld", status, gx_ref_error_code(cnd)
    ))
    NULL
  })
  if (!is.null(jsonld_result)) return(jsonld_result)

  gx_abort(
    "Reference feature could not be retrieved by any compatible path.",
    "gx_error_reference_feature",
    attempts = attempts,
    requests = requests
  )
}

#' @export
print.gx_ref_feature <- function(x, ...) {
  original <- x
  metadata <- attr(x, "gx_reference")
  cli::cli_inform(c(
    "<gx_ref_feature>",
    "* Collection/ID: {metadata$collection}/{metadata$id}",
    "* Retrieval: {metadata$retrieval_mode}; complete: {metadata$complete}",
    "* Requests: {nrow(metadata$requests)}"
  ))
  class(x) <- setdiff(class(x), c("gx_ref_feature", "gx_ref_features"))
  print(x, ...)
  invisible(original)
}
