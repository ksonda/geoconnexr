.gx_csv_validated_response_contract_version <- "0.1.0"
.gx_csv_validated_response_max_headers <- 64L
.gx_csv_validated_response_max_header_name_bytes <- 256L
.gx_csv_validated_response_max_header_value_bytes <- 8L * 1024L
.gx_csv_validated_response_max_header_bytes <- 64L * 1024L
.gx_csv_validated_response_max_text_bytes <- 1L * 1024L^2

.gx_csv_validated_response_fields <- c(
  "contract_version", "request_plan", "body", "validation", "metadata"
)

.gx_csv_validated_response_candidate_fields <- c(
  "status", "headers", "body", "url"
)

.gx_csv_validated_response_validation_fields <- c(
  "validation_id", "logical_request_id", "intent_id", "reservation_id",
  "distribution_id", "status", "media_type", "content_encoding",
  "content_length_present", "content_length", "encoded_bytes",
  "decoded_bytes", "body_sha256", "validation_status"
)

.gx_csv_validated_response_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "response_candidate_validated", "provider_response_observed",
  "budgets_consumed", "parser_executed", "observation_origin",
  "non_replayable_reasons"
)

.gx_csv_validated_response_media_types <- c(
  "text/csv", "application/csv"
)

gx_csv_validated_response_abort <- function(
    message,
    class = "gx_error_csv_response_validation_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class,
      "gx_error_csv_response_validation",
      "gx_error_fetch_plan"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_csv_validated_response_exact_attributes <- function(x, expected) {
  observed <- names(attributes(x))
  is.character(observed) && !anyNA(observed) &&
    length(observed) == length(expected) && all(expected %in% observed)
}

gx_csv_validated_response_valid_scalar_text <- function(x, nonempty = TRUE) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    is.null(attributes(x)) && gx_fetch_plan_text_valid(
      x, allow_na = FALSE, nonempty = nonempty
    )
}

gx_csv_validated_response_request_row_impl <- function(
    request_plan, logical_request_id) {
  if (!gx_csv_validated_response_valid_scalar_text(logical_request_id) ||
      !gx_catalog_is_sha256(logical_request_id)) {
    gx_csv_validated_response_abort(
      "A direct-CSV response must select one valid logical request identity.",
      "gx_error_csv_response_validation_input"
    )
  }
  position <- which(
    request_plan$request_plans$logical_request_id == logical_request_id
  )
  if (length(position) != 1L) {
    gx_csv_validated_response_abort(
      "The selected logical request is not present exactly once.",
      "gx_error_csv_response_validation_input"
    )
  }
  unname(as.integer(position))
}

gx_csv_validated_response_target_impl <- function(request_plan, position) {
  request <- request_plan$request_plans[position, , drop = FALSE]
  distributions <- request_plan$intent_set$plan$distributions
  distribution_position <- match(
    request$distribution_id[[1L]], distributions$distribution_id
  )
  target <- tryCatch(
    gx_csv_get_intents_target_impl(
      distributions$distribution_url[[distribution_position]]
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (!is.list(target) || !identical(
    names(target), c("url", "redacted")
  ) || !identical(
    target$redacted, request$canonical_url_redacted[[1L]]
  )) {
    gx_csv_validated_response_abort(
      "The logical request target could not be re-derived safely.",
      "gx_error_csv_response_validation_input"
    )
  }
  target
}

gx_csv_validated_response_header_values_impl <- function(headers) {
  if (is.null(headers)) {
    return(character())
  }
  if (!is.character(headers) && !is.list(headers)) {
    gx_csv_validated_response_abort(
      "Response headers violate their exact bounded input shape.",
      "gx_error_csv_response_validation_headers"
    )
  }
  if (length(headers) > .gx_csv_validated_response_max_headers) {
    gx_csv_validated_response_abort(
      "Response headers exceed their count budget or have invalid names.",
      "gx_error_csv_response_validation_headers"
    )
  }
  if (!length(headers)) {
    if (!is.null(attributes(headers))) {
      gx_csv_validated_response_abort(
        "Response headers violate their exact bounded input shape.",
        "gx_error_csv_response_validation_headers"
      )
    }
    return(character())
  }
  header_names <- names(headers)
  if (is.null(header_names) || anyNA(header_names) ||
      any(!nzchar(header_names))) {
    gx_csv_validated_response_abort(
      "Response headers exceed their count budget or have invalid names.",
      "gx_error_csv_response_validation_headers"
    )
  }
  if (is.character(headers)) {
    if (!gx_csv_validated_response_exact_attributes(headers, "names")) {
      gx_csv_validated_response_abort(
        "Response headers violate their exact bounded input shape.",
        "gx_error_csv_response_validation_headers"
      )
    }
    values <- unname(headers)
  } else if (is.list(headers)) {
    if (!gx_csv_validated_response_exact_attributes(headers, "names") ||
        !all(vapply(headers, function(value) {
          is.character(value) && length(value) == 1L && !is.na(value) &&
            is.null(attributes(value))
        }, logical(1)))) {
      gx_csv_validated_response_abort(
        "Response headers violate their exact bounded input shape.",
        "gx_error_csv_response_validation_headers"
      )
    }
    values <- unname(vapply(headers, identity, character(1)))
  }
  byte_length <- function(value) {
    suppressWarnings(tryCatch(
      nchar(value, type = "bytes", allowNA = TRUE),
      error = function(cnd) NA_integer_,
      warning = function(cnd) NA_integer_
    ))
  }
  name_bytes <- byte_length(header_names)
  source_value_bytes <- byte_length(values)
  source_total_bytes <- sum(as.double(name_bytes)) +
    sum(as.double(source_value_bytes))
  if (anyNA(name_bytes) || anyNA(source_value_bytes) ||
      any(name_bytes > .gx_csv_validated_response_max_header_name_bytes) ||
      any(source_value_bytes >
        .gx_csv_validated_response_max_header_value_bytes) ||
      !is.finite(source_total_bytes) ||
      source_total_bytes > .gx_csv_validated_response_max_header_bytes) {
    gx_csv_validated_response_abort(
      "Response headers violate their text or byte budgets.",
      "gx_error_csv_response_validation_headers"
    )
  }
  valid_name_encoding <- vapply(header_names, function(value) {
    suppressWarnings(tryCatch(
      isTRUE(stringi::stri_enc_isutf8(value)),
      error = function(cnd) FALSE,
      warning = function(cnd) FALSE
    ))
  }, logical(1))
  valid_names <- rep.int(FALSE, length(header_names))
  if (any(valid_name_encoding)) {
    valid_names[valid_name_encoding] <- grepl(
      "^[!#$%&'*+.^_`|~0-9A-Za-z-]+$",
      header_names[valid_name_encoding],
      perl = TRUE
    )
  }
  value_valid <- vapply(values, function(value) {
    gx_fetch_plan_text_valid(value, allow_na = FALSE, nonempty = FALSE)
  }, logical(1))
  if (any(!valid_names) || !all(value_valid)) {
    gx_csv_validated_response_abort(
      "Response headers violate their text or byte budgets.",
      "gx_error_csv_response_validation_headers"
    )
  }
  value_bytes <- byte_length(enc2utf8(values))
  total_bytes <- sum(as.double(name_bytes)) + sum(as.double(value_bytes))
  if (anyNA(value_bytes) ||
      any(value_bytes > .gx_csv_validated_response_max_header_value_bytes) ||
      !is.finite(total_bytes) ||
      total_bytes > .gx_csv_validated_response_max_header_bytes) {
    gx_csv_validated_response_abort(
      "Response headers violate their text or byte budgets.",
      "gx_error_csv_response_validation_headers"
    )
  }
  names(values) <- tolower(header_names)
  values
}

gx_csv_validated_response_single_header_impl <- function(
    headers, name, required = FALSE, class) {
  positions <- which(names(headers) == name)
  if (length(positions) > 1L || (required && length(positions) != 1L)) {
    gx_csv_validated_response_abort(
      "A critical response header is missing or ambiguous.",
      class
    )
  }
  if (!length(positions)) return(NULL)
  unname(headers[[positions[[1L]]]])
}

gx_csv_validated_response_media_type_impl <- function(headers) {
  value <- gx_csv_validated_response_single_header_impl(
    headers, "content-type", required = TRUE,
    class = c(
      "gx_error_csv_response_validation_content_type",
      "gx_error_content_type"
    )
  )
  value <- trimws(value)
  if (!nzchar(value) || grepl(",", value, fixed = TRUE)) {
    gx_csv_validated_response_abort(
      "The response media type is not admitted by the logical request.",
      c(
        "gx_error_csv_response_validation_content_type",
        "gx_error_content_type"
      )
    )
  }
  media_type <- tolower(trimws(strsplit(
    value, ";", fixed = TRUE
  )[[1L]][[1L]]))
  if (!media_type %in%
      .gx_csv_validated_response_media_types) {
    gx_csv_validated_response_abort(
      "The response media type is not admitted by the logical request.",
      c(
        "gx_error_csv_response_validation_content_type",
        "gx_error_content_type"
      )
    )
  }
  unname(media_type)
}

gx_csv_validated_response_content_encoding_impl <- function(headers) {
  value <- gx_csv_validated_response_single_header_impl(
    headers, "content-encoding", required = FALSE,
    class = "gx_error_csv_response_validation_content_encoding"
  )
  if (is.null(value)) return("identity")
  value <- tolower(trimws(value))
  if (!identical(value, "identity")) {
    gx_csv_validated_response_abort(
      "The response content encoding is not the required identity encoding.",
      c(
        "gx_error_csv_response_validation_content_encoding",
        "gx_error_content_encoding"
      )
    )
  }
  "identity"
}

gx_csv_validated_response_content_length_impl <- function(headers, body_bytes) {
  value <- gx_csv_validated_response_single_header_impl(
    headers, "content-length", required = FALSE,
    class = "gx_error_csv_response_validation_content_length"
  )
  if (is.null(value)) {
    return(list(present = FALSE, value = NA_integer_))
  }
  value <- trimws(value)
  if (!nzchar(value) || !grepl("^[0-9]+$", value)) {
    gx_csv_validated_response_abort(
      "The response Content-Length is not a strict unsigned decimal.",
      "gx_error_csv_response_validation_content_length"
    )
  }
  canonical <- sub("^0+", "", value)
  if (!nzchar(canonical)) canonical <- "0"
  if (!identical(canonical, as.character(body_bytes))) {
    gx_csv_validated_response_abort(
      "The response Content-Length does not equal the exact raw body length.",
      "gx_error_csv_response_validation_content_length"
    )
  }
  list(present = TRUE, value = unname(as.integer(body_bytes)))
}

gx_csv_validated_response_body_bytes_impl <- function(body) {
  bytes <- as.double(length(body))
  if (!is.finite(bytes) || bytes < 0 || bytes != floor(bytes) ||
      bytes > .Machine$integer.max) {
    gx_csv_validated_response_abort(
      "The response body exceeds the supported in-memory byte range.",
      c(
        "gx_error_csv_response_validation_payload_too_large",
        "gx_error_payload_too_large"
      )
    )
  }
  unname(as.integer(bytes))
}

gx_csv_validated_response_candidate_impl <- function(candidate) {
  if (!is.list(candidate) ||
      !identical(names(candidate), .gx_csv_validated_response_candidate_fields) ||
      !gx_csv_validated_response_exact_attributes(candidate, "names")) {
    gx_csv_validated_response_abort(
      "A direct-CSV response candidate violates its exact input shape.",
      "gx_error_csv_response_validation_input"
    )
  }
  status <- candidate$status
  valid_status <- is.numeric(status) && length(status) == 1L &&
    !is.na(status) && is.finite(status) && status == floor(status) &&
    status >= 100 && status <= 599 && is.null(attributes(status))
  if (!valid_status) {
    gx_csv_validated_response_abort(
      "A response candidate has an invalid HTTP status.",
      "gx_error_csv_response_validation_status"
    )
  }
  if (!identical(as.integer(status), 200L)) {
    gx_csv_validated_response_abort(
      "The response status is not the required success status.",
      "gx_error_csv_response_validation_status"
    )
  }
  if (!is.raw(candidate$body) || !is.null(attributes(candidate$body))) {
    gx_csv_validated_response_abort(
      "A response candidate body must be one unclassed raw vector.",
      "gx_error_csv_response_validation_input"
    )
  }
  if (!gx_csv_validated_response_valid_scalar_text(candidate$url)) {
    gx_csv_validated_response_abort(
      "A response candidate URL violates its exact input shape.",
      "gx_error_csv_response_validation_url"
    )
  }
  headers <- gx_csv_validated_response_header_values_impl(candidate$headers)
  media_type <- gx_csv_validated_response_media_type_impl(headers)
  content_encoding <- gx_csv_validated_response_content_encoding_impl(headers)
  body_bytes <- gx_csv_validated_response_body_bytes_impl(candidate$body)
  content_length <- gx_csv_validated_response_content_length_impl(
    headers, body_bytes
  )
  list(
    status = 200L,
    body = candidate$body,
    url = candidate$url,
    media_type = media_type,
    content_encoding = content_encoding,
    content_length_present = content_length$present,
    content_length = content_length$value,
    body_bytes = body_bytes
  )
}

gx_csv_validated_response_assert_target_impl <- function(
    candidate_url, expected_target) {
  observed_target <- tryCatch(
    gx_csv_get_intents_target_impl(candidate_url),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (!is.list(observed_target) ||
      !identical(observed_target$url, expected_target$url)) {
    gx_csv_validated_response_abort(
      "The response target differs from the redirect-disabled logical request.",
      c(
        "gx_error_csv_response_validation_url",
        "gx_error_redirect"
      )
    )
  }
  invisible(observed_target)
}

gx_csv_validated_response_assert_body_limit_impl <- function(
    body_bytes, request) {
  limits <- c(
    request$max_encoded_bytes[[1L]],
    request$max_decoded_bytes[[1L]],
    request$response_byte_limit[[1L]]
  )
  if (any(as.double(body_bytes) > limits)) {
    gx_csv_validated_response_abort(
      "The response body exceeds a logical-request byte ceiling.",
      c(
        "gx_error_csv_response_validation_payload_too_large",
        "gx_error_payload_too_large"
      )
    )
  }
  invisible(body_bytes)
}

gx_csv_validated_response_validation_id_impl <- function(
    request, canonical_target, status, media_type, content_encoding,
    content_length_present, content_length, encoded_bytes, decoded_bytes,
    body_sha256) {
  gx_contract_hash(
    list(
      "logical_request_id", request$logical_request_id[[1L]],
      "intent_id", request$intent_id[[1L]],
      "reservation_id", request$reservation_id[[1L]],
      "distribution_id", request$distribution_id[[1L]],
      "canonical_target", canonical_target,
      "status", status,
      "media_type", media_type,
      "content_encoding", content_encoding,
      "content_length_present", content_length_present,
      "content_length", if (content_length_present) {
        gx_csv_request_plan_byte_hash_value(content_length)
      } else {
        "absent"
      },
      "encoded_bytes", gx_csv_request_plan_byte_hash_value(encoded_bytes),
      "decoded_bytes", gx_csv_request_plan_byte_hash_value(decoded_bytes),
      "body_sha256", body_sha256,
      "max_encoded_bytes", gx_csv_request_plan_byte_hash_value(
        request$max_encoded_bytes[[1L]]
      ),
      "max_decoded_bytes", gx_csv_request_plan_byte_hash_value(
        request$max_decoded_bytes[[1L]]
      ),
      "response_byte_limit", gx_csv_request_plan_byte_hash_value(
        request$response_byte_limit[[1L]]
      )
    ),
    namespace = "geoconnexr.csv-response-validation.v1",
    contract_version = .gx_csv_validated_response_contract_version
  )
}

gx_csv_validated_response_validation_impl <- function(
    request, target, candidate) {
  body_sha256 <- digest::digest(
    candidate$body, algo = "sha256", serialize = FALSE
  )
  validation_id <- gx_csv_validated_response_validation_id_impl(
    request = request,
    canonical_target = target$url,
    status = candidate$status,
    media_type = candidate$media_type,
    content_encoding = candidate$content_encoding,
    content_length_present = candidate$content_length_present,
    content_length = candidate$content_length,
    encoded_bytes = candidate$body_bytes,
    decoded_bytes = candidate$body_bytes,
    body_sha256 = body_sha256
  )
  list(
    validation_id = validation_id,
    logical_request_id = request$logical_request_id[[1L]],
    intent_id = request$intent_id[[1L]],
    reservation_id = request$reservation_id[[1L]],
    distribution_id = request$distribution_id[[1L]],
    status = candidate$status,
    media_type = candidate$media_type,
    content_encoding = candidate$content_encoding,
    content_length_present = candidate$content_length_present,
    content_length = candidate$content_length,
    encoded_bytes = candidate$body_bytes,
    decoded_bytes = candidate$body_bytes,
    body_sha256 = body_sha256,
    validation_status = "validated_caller_supplied"
  )
}

gx_csv_validated_response_reasons_impl <- function(request_plan) {
  reasons <- unique(c(
    setdiff(
      request_plan$metadata$non_replayable_reasons,
      "response_validator_unimplemented"
    ),
    "response_origin_unbound"
  ))
  reasons[gx_catalog_byte_order(reasons)]
}

gx_csv_validated_response_metadata_impl <- function(request_plan) {
  list(
    host_specific = FALSE,
    replayable = FALSE,
    execution_ready = FALSE,
    transport_authorized = FALSE,
    response_candidate_validated = TRUE,
    provider_response_observed = FALSE,
    budgets_consumed = FALSE,
    parser_executed = FALSE,
    observation_origin = "caller_supplied",
    non_replayable_reasons = gx_csv_validated_response_reasons_impl(
      request_plan
    )
  )
}

gx_csv_validated_response_new_impl <- function(
    request_plan, body, validation, metadata) {
  object <- structure(
    list(
      contract_version = .gx_csv_validated_response_contract_version,
      request_plan = request_plan,
      body = body,
      validation = validation,
      metadata = metadata
    ),
    class = "gx_csv_validated_response"
  )
  gx_csv_validated_response_validate_impl(object)
  object
}

gx_csv_validated_response_impl <- function(
    request_plan, logical_request_id = NULL, candidate = NULL) {
  valid_plan <- tryCatch({
    gx_csv_request_plan_validate_impl(request_plan)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid_plan) {
    gx_csv_validated_response_abort(
      "M7e construction requires a valid M7d direct-CSV request plan.",
      "gx_error_csv_response_validation_input"
    )
  }
  position <- gx_csv_validated_response_request_row_impl(
    request_plan, logical_request_id
  )
  request <- request_plan$request_plans[position, , drop = FALSE]
  target <- gx_csv_validated_response_target_impl(request_plan, position)
  candidate <- gx_csv_validated_response_candidate_impl(candidate)
  gx_csv_validated_response_assert_target_impl(candidate$url, target)
  gx_csv_validated_response_assert_body_limit_impl(
    candidate$body_bytes, request
  )
  validation <- gx_csv_validated_response_validation_impl(
    request, target, candidate
  )
  gx_csv_validated_response_new_impl(
    request_plan = request_plan,
    body = candidate$body,
    validation = validation,
    metadata = gx_csv_validated_response_metadata_impl(request_plan)
  )
}

gx_csv_validated_response_validate_validation <- function(
    validation, body, request_plan) {
  valid_shape <- is.list(validation) && identical(
    names(validation), .gx_csv_validated_response_validation_fields
  ) && gx_csv_validated_response_exact_attributes(validation, "names")
  if (!valid_shape) {
    gx_csv_validated_response_abort(
      "Validated direct-CSV response facts violate their exact shape."
    )
  }
  character_fields <- c(
    "validation_id", "logical_request_id", "intent_id", "reservation_id",
    "distribution_id", "media_type", "content_encoding", "body_sha256",
    "validation_status"
  )
  integer_fields <- c(
    "status", "content_length", "encoded_bytes", "decoded_bytes"
  )
  valid_characters <- all(vapply(
    validation[character_fields],
    gx_csv_validated_response_valid_scalar_text,
    logical(1)
  ))
  valid_integers <- all(vapply(validation[integer_fields], function(value) {
    is.integer(value) && length(value) == 1L && is.null(attributes(value))
  }, logical(1)))
  valid_present <- is.logical(validation$content_length_present) &&
    length(validation$content_length_present) == 1L &&
    !is.na(validation$content_length_present) &&
    is.null(attributes(validation$content_length_present))
  if (!valid_characters || !valid_integers || !valid_present ||
      !gx_catalog_is_sha256(validation$validation_id) ||
      !gx_catalog_is_sha256(validation$logical_request_id) ||
      !gx_catalog_is_sha256(validation$intent_id) ||
      !gx_catalog_is_sha256(validation$reservation_id) ||
      !gx_catalog_is_sha256(validation$distribution_id) ||
      !gx_catalog_is_sha256(validation$body_sha256)) {
    gx_csv_validated_response_abort(
      "Validated direct-CSV response facts have invalid types or identities."
    )
  }
  position <- gx_csv_validated_response_request_row_impl(
    request_plan, validation$logical_request_id
  )
  request <- request_plan$request_plans[position, , drop = FALSE]
  target <- gx_csv_validated_response_target_impl(request_plan, position)
  body_bytes <- gx_csv_validated_response_body_bytes_impl(body)
  expected_content_length <- if (validation$content_length_present) {
    body_bytes
  } else {
    NA_integer_
  }
  gx_csv_validated_response_assert_body_limit_impl(body_bytes, request)
  expected_body_sha256 <- digest::digest(
    body, algo = "sha256", serialize = FALSE
  )
  expected_id <- gx_csv_validated_response_validation_id_impl(
    request = request,
    canonical_target = target$url,
    status = validation$status,
    media_type = validation$media_type,
    content_encoding = validation$content_encoding,
    content_length_present = validation$content_length_present,
    content_length = validation$content_length,
    encoded_bytes = validation$encoded_bytes,
    decoded_bytes = validation$decoded_bytes,
    body_sha256 = validation$body_sha256
  )
  if (!identical(validation$intent_id, request$intent_id[[1L]]) ||
      !identical(validation$reservation_id, request$reservation_id[[1L]]) ||
      !identical(validation$distribution_id, request$distribution_id[[1L]]) ||
      !identical(validation$status, 200L) ||
      !validation$media_type %in% .gx_csv_validated_response_media_types ||
      !identical(validation$content_encoding, "identity") ||
      !identical(validation$content_length, expected_content_length) ||
      !identical(validation$encoded_bytes, body_bytes) ||
      !identical(validation$decoded_bytes, body_bytes) ||
      !identical(validation$body_sha256, expected_body_sha256) ||
      !identical(
        validation$validation_status, "validated_caller_supplied"
      ) || !identical(validation$validation_id, expected_id)) {
    gx_csv_validated_response_abort(
      "Validated response facts do not rebind to the request plan and body."
    )
  }
  invisible(validation)
}

gx_csv_validated_response_validate_metadata <- function(
    metadata, request_plan) {
  expected <- gx_csv_validated_response_metadata_impl(request_plan)
  if (!is.list(metadata) ||
      !identical(names(metadata), .gx_csv_validated_response_metadata_fields) ||
      !gx_csv_validated_response_exact_attributes(metadata, "names") ||
      !identical(metadata, expected) ||
      !all(vapply(metadata, function(value) {
        is.null(attributes(value))
      }, logical(1))) ||
      !is.character(metadata$non_replayable_reasons) ||
      !length(metadata$non_replayable_reasons) ||
      anyNA(metadata$non_replayable_reasons) ||
      anyDuplicated(metadata$non_replayable_reasons) ||
      !gx_catalog_byte_sorted(metadata$non_replayable_reasons) ||
      !gx_catalog_is_token(metadata$non_replayable_reasons)) {
    gx_csv_validated_response_abort(
      "Validated response metadata violates its exact non-executable contract."
    )
  }
  invisible(metadata)
}

gx_csv_validated_response_assert_text_budget <- function(x) {
  owned <- list(
    contract_version = x$contract_version,
    validation = x$validation,
    metadata = x$metadata
  )
  total <- gx_fetch_plan_text_total(
    owned, limit = .gx_csv_validated_response_max_text_bytes
  )
  if (!is.finite(total) ||
      total > .gx_csv_validated_response_max_text_bytes) {
    gx_csv_validated_response_abort(
      "Validated response text exceeds its aggregate byte budget.",
      "gx_error_csv_response_validation_budget"
    )
  }
  invisible(total)
}

gx_csv_validated_response_validate_body <- function(x) {
  if (!is.list(x) || !identical(class(x), "gx_csv_validated_response") ||
      !identical(names(x), .gx_csv_validated_response_fields) ||
      !gx_csv_validated_response_exact_attributes(x, c("names", "class")) ||
      !identical(
        x$contract_version, .gx_csv_validated_response_contract_version
      ) || !is.null(attributes(x$contract_version)) ||
      !is.raw(x$body) || !is.null(attributes(x$body))) {
    gx_csv_validated_response_abort(
      "Validated direct-CSV responses violate their exact top-level contract."
    )
  }
  gx_csv_request_plan_validate_impl(x$request_plan)
  gx_csv_validated_response_validate_validation(
    x$validation, x$body, x$request_plan
  )
  gx_csv_validated_response_validate_metadata(x$metadata, x$request_plan)
  gx_csv_validated_response_assert_text_budget(x)
  invisible(x)
}

gx_csv_validated_response_validate_impl <- function(x) {
  tryCatch(
    gx_csv_validated_response_validate_body(x),
    error = function(cnd) {
      if (inherits(cnd, "gx_error_csv_response_validation")) stop(cnd)
      gx_csv_validated_response_abort(
        "Direct-CSV response validation rejected a malformed object."
      )
    },
    warning = function(cnd) {
      gx_csv_validated_response_abort(
        "Direct-CSV response validation rejected a warning-producing object."
      )
    }
  )
}

#' @export
print.gx_csv_validated_response <- function(x, ...) {
  gx_csv_validated_response_validate_impl(x)
  validation <- x$validation
  cli::cli_inform(c(
    "<gx_csv_validated_response>",
    paste0(
      "* Caller-supplied response candidate: validated; bytes: ",
      "{validation$encoded_bytes}"
    ),
    paste0(
      "* Status: {validation$status}; media type: ",
      "{validation$media_type}; content encoding: ",
      "{validation$content_encoding}"
    ),
    paste0(
      "* Provider response observed: FALSE; budgets consumed: FALSE; ",
      "execution ready: FALSE"
    )
  ))
  invisible(x)
}
