.gx_graph_contract_version <- "0.1.0"
.gx_graph_query_byte_limit <- 256L * 1024L
.gx_graph_response_byte_limit <- 16L * 1024L^2
.gx_graph_scope_state <- new.env(parent = emptyenv())

gx_graph_abort <- function(message, class = "gx_error_graph", ...,
                           call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_graph")),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_graph_scalar_limit <- function(value, name, minimum = 1L,
                                  maximum = .Machine$integer.max) {
  valid <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.finite(value) && value == trunc(value) && value >= minimum &&
    value <= maximum
  if (!valid) {
    gx_graph_abort(
      "The internal graph limit {.arg {name}} is invalid.",
      "gx_error_graph_input"
    )
  }
  as.integer(value)
}

gx_graph_limits <- function(max_rows, max_variables, max_bound_terms, max_links,
                            max_requests, max_total_bytes, max_members,
                            max_atomic_bytes, max_depth) {
  list(
    max_rows = gx_graph_scalar_limit(max_rows, "max_rows", minimum = 0L),
    max_variables = gx_graph_scalar_limit(
      max_variables, "max_variables", minimum = 0L
    ),
    max_bound_terms = gx_graph_scalar_limit(
      max_bound_terms, "max_bound_terms", minimum = 0L
    ),
    max_links = gx_graph_scalar_limit(max_links, "max_links", minimum = 0L),
    max_requests = gx_graph_scalar_limit(max_requests, "max_requests"),
    max_total_bytes = gx_graph_scalar_limit(
      max_total_bytes, "max_total_bytes"
    ),
    max_members = gx_graph_scalar_limit(
      max_members, "max_members", maximum = 10000000L
    ),
    max_atomic_bytes = gx_graph_scalar_limit(
      max_atomic_bytes, "max_atomic_bytes"
    ),
    max_depth = gx_graph_scalar_limit(
      max_depth, "max_depth", maximum = 10000L
    )
  )
}

gx_graph_expected <- function(expected) {
  if (!is.character(expected) || length(expected) != 1L || is.na(expected) ||
      !expected %in% c("select", "ask")) {
    gx_graph_abort(
      "{.arg expected} must be exactly 'select' or 'ask'.",
      "gx_error_graph_input"
    )
  }
  expected
}

gx_graph_query_text <- function(query) {
  utf8 <- is.character(query) && length(query) == 1L && !is.na(query) &&
    isTRUE(stringi::stri_enc_isutf8(query))
  if (!utf8) {
    gx_graph_abort(
      "Graph query text must be one valid UTF-8 string.",
      "gx_error_graph_input"
    )
  }
  query <- enc2utf8(query)
  if (!nzchar(trimws(query))) {
    gx_graph_abort("Graph query text cannot be blank.", "gx_error_graph_input")
  }
  without_line_space <- gsub("[\t\r\n]", "", query, perl = TRUE)
  forbidden <- stringi::stri_detect_regex(
    without_line_space,
    "[\\p{Cc}\\p{Cf}\\p{Cs}]"
  )
  if (!isFALSE(forbidden)) {
    gx_graph_abort(
      "Graph query text contains unsupported control characters.",
      "gx_error_graph_input"
    )
  }
  bytes <- charToRaw(query)
  if (length(bytes) > .gx_graph_query_byte_limit) {
    gx_graph_abort(
      "Graph query text exceeds the 256 KiB input limit.",
      "gx_error_graph_input"
    )
  }
  list(text = query, raw = bytes)
}

gx_graph_empty_requests <- function() {
  tibble::tibble(
    request_id = character(), method = character(), url = character(),
    status = integer(), media_type = character(), bytes = integer(),
    body_sha256 = character(), retrieved_at = as.POSIXct(character(), tz = "UTC"),
    cache_origin = character()
  )
}

gx_graph_budget <- function(max_requests, max_total_bytes,
                            max_response_bytes = .gx_graph_response_byte_limit) {
  budget <- new.env(parent = emptyenv())
  budget$max_requests <- max_requests
  budget$max_total_bytes <- max_total_bytes
  budget$max_response_bytes <- max_response_bytes
  budget$ledger <- gx_graph_empty_requests()
  budget$control <- list(
    before = function(request, physical) {
      if (nrow(budget$ledger) >= budget$max_requests) {
        gx_graph_abort(
          "Graph retrieval exhausted its request-attempt budget.",
          "gx_error_graph_budget",
          budget_kind = "requests",
          requests = budget$ledger
        )
      }
      remaining <- budget$max_total_bytes -
        sum(as.double(budget$ledger$bytes), na.rm = TRUE)
      if (!is.finite(remaining) || remaining < 1) {
        gx_graph_abort(
          "Graph retrieval exhausted its cumulative-byte budget.",
          "gx_error_graph_budget",
          budget_kind = "bytes",
          requests = budget$ledger
        )
      }
      as.integer(min(
        as.double(request$max_bytes),
        floor(remaining),
        as.double(budget$max_response_bytes)
      ))
    },
    after = function(attempt) {
      budget$ledger <- rbind(
        budget$ledger,
        gx_http_attempt_request_row(attempt)
      )
      total <- sum(as.double(budget$ledger$bytes), na.rm = TRUE)
      if (!is.finite(total) || total > budget$max_total_bytes) {
        gx_graph_abort(
          "Graph retrieval exceeded its cumulative-byte budget.",
          "gx_error_graph_budget",
          budget_kind = "bytes",
          requests = budget$ledger
        )
      }
      invisible(NULL)
    }
  )
  budget
}

gx_graph_json_text <- function(body) {
  if (!is.raw(body)) {
    gx_graph_abort(
      "SPARQL results must be supplied as raw JSON bytes.",
      "gx_error_graph_payload"
    )
  }
  if (length(body) > .gx_graph_response_byte_limit) {
    gx_graph_abort(
      "SPARQL Results JSON exceeds the 16 MiB parser ceiling.",
      "gx_error_graph_budget",
      budget_kind = "raw_bytes"
    )
  }
  if (any(as.integer(body) == 0L)) {
    gx_graph_abort(
      "SPARQL Results JSON contains a NUL byte.",
      "gx_error_graph_payload"
    )
  }
  if (length(body) >= 3L &&
      identical(as.integer(body[1:3]), c(239L, 187L, 191L))) {
    body <- body[-(1:3)]
  }
  text <- tryCatch(
    rawToChar(body),
    error = function(cnd) {
      gx_graph_abort(
        "SPARQL results could not be decoded as UTF-8 JSON.",
        "gx_error_graph_payload"
      )
    }
  )
  valid <- iconv(text, from = "UTF-8", to = "UTF-8", sub = NA_character_)
  if (is.na(valid)) {
    gx_graph_abort(
      "SPARQL results are not valid UTF-8.",
      "gx_error_graph_payload"
    )
  }
  enc2utf8(valid)
}

gx_graph_json_preflight <- function(text, max_depth, max_members) {
  bytes <- as.integer(charToRaw(text))
  depth <- 0L
  in_string <- FALSE
  escaped <- FALSE
  structural_units <- 0
  # Every non-root container is itself a member of its parent, so opening
  # delimiters + object colons + separators are bounded by 3 * members + 1.
  structural_limit <- 3 * as.double(max_members) + 2
  for (byte in bytes) {
    if (in_string) {
      if (escaped) {
        escaped <- FALSE
      } else if (byte == 92L) {
        escaped <- TRUE
      } else if (byte == 34L) {
        in_string <- FALSE
      }
      next
    }
    if (byte == 34L) {
      in_string <- TRUE
    } else if (byte %in% c(91L, 123L)) {
      structural_units <- structural_units + 1
      depth <- depth + 1L
      if (depth > max_depth) {
        gx_graph_abort(
          "SPARQL Results JSON exceeds its nesting-depth budget.",
          "gx_error_graph_budget",
          budget_kind = "depth"
        )
      }
    } else if (byte %in% c(44L, 58L)) {
      structural_units <- structural_units + 1
    } else if (byte %in% c(93L, 125L)) {
      depth <- depth - 1L
      if (depth < 0L) {
        gx_graph_abort(
          "SPARQL Results JSON has unbalanced delimiters.",
          "gx_error_graph_payload"
        )
      }
    }
    if (structural_units > structural_limit) {
      gx_graph_abort(
        "SPARQL Results JSON exceeds its pre-parse structural budget.",
        "gx_error_graph_budget",
        budget_kind = "members"
      )
    }
  }
  if (in_string || depth != 0L) {
    gx_graph_abort(
      "SPARQL Results JSON is incomplete.",
      "gx_error_graph_payload"
    )
  }
  invisible(text)
}

gx_graph_assert_unique_members <- function(value) {
  stack <- list(value)
  while (length(stack)) {
    current <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    if (!is.list(current)) next
    object_names <- names(current)
    if (!is.null(object_names) && anyDuplicated(object_names)) {
      gx_graph_abort(
        "SPARQL Results JSON contains duplicate object members.",
        "gx_error_graph_payload"
      )
    }
    children <- current[vapply(current, is.list, logical(1))]
    if (length(children)) stack <- c(stack, unname(children))
  }
  invisible(value)
}

gx_graph_is_object <- function(value) {
  is.list(value) && !is.null(names(value)) && all(nzchar(names(value)))
}

gx_graph_is_array <- function(value) {
  is.list(value) && is.null(names(value))
}

gx_graph_is_string <- function(value, nonempty = FALSE, token = FALSE) {
  valid <- is.character(value) && length(value) == 1L && !is.na(value) &&
    isTRUE(stringi::stri_enc_isutf8(value)) &&
    (!nonempty || nzchar(value))
  if (!valid || !token) return(valid)
  !isTRUE(stringi::stri_detect_regex(
    value,
    "[\\p{Z}\\p{Cc}\\p{Cf}\\p{Cs}]"
  ))
}

gx_graph_is_absolute_iri <- function(value) {
  gx_graph_is_string(value, nonempty = TRUE, token = TRUE) &&
    grepl("^[A-Za-z][A-Za-z0-9+.-]*:", value) &&
    !grepl("[<>\"{}|^`\\\\]", value)
}

gx_graph_is_language_tag <- function(value) {
  gx_graph_is_string(value, nonempty = TRUE, token = TRUE) &&
    grepl("^[A-Za-z][A-Za-z0-9]{0,7}(-[A-Za-z0-9]{1,8})*$", value)
}

gx_graph_scope_id <- function(seed) {
  counter <- get0("counter", envir = .gx_graph_scope_state, inherits = FALSE,
                  ifnotfound = 0)
  counter <- as.double(counter) + 1
  assign("counter", counter, envir = .gx_graph_scope_state)
  digest::digest(
    list(
      seed = seed,
      pid = Sys.getpid(),
      counter = counter,
      timestamp = as.double(Sys.time()),
      elapsed = unname(proc.time()[["elapsed"]])
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

gx_graph_head <- function(head, expected, max_variables, max_links) {
  if (!gx_graph_is_object(head)) {
    gx_graph_abort(
      "SPARQL Results JSON head must be an object.",
      "gx_error_graph_payload"
    )
  }
  if ("version" %in% names(head)) {
    gx_graph_abort(
      "This graph substrate accepts SPARQL 1.1 results only.",
      "gx_error_graph_version"
    )
  }
  allowed <- if (identical(expected, "select")) c("vars", "link") else "link"
  if (length(setdiff(names(head), allowed))) {
    gx_graph_abort(
      "SPARQL Results JSON head contains unsupported members.",
      "gx_error_graph_payload"
    )
  }
  links <- character()
  if ("link" %in% names(head)) {
    link <- head[["link"]]
    if (!gx_graph_is_array(link)) {
      gx_graph_abort(
        "SPARQL result links must be a JSON array.",
        "gx_error_graph_payload"
      )
    }
    if (length(link) > max_links) {
      gx_graph_abort(
        "SPARQL result links exceed their count budget.",
        "gx_error_graph_budget",
        budget_kind = "links"
      )
    }
    valid_links <- all(vapply(
      link,
      gx_graph_is_absolute_iri,
      logical(1)
    ))
    if (!valid_links) {
      gx_graph_abort(
        "SPARQL result links must contain valid non-empty strings.",
        "gx_error_graph_payload"
      )
    }
    links <- vapply(link, identity, character(1), USE.NAMES = FALSE)
  }
  if (identical(expected, "ask")) {
    return(list(variables = character(), links = links))
  }
  if (!"vars" %in% names(head) || !gx_graph_is_array(head[["vars"]])) {
    gx_graph_abort(
      "SELECT results must declare an ordered variable array.",
      "gx_error_graph_payload"
    )
  }
  vars <- head[["vars"]]
  if (length(vars) > max_variables) {
    gx_graph_abort(
      "SELECT result variables exceed their count budget.",
      "gx_error_graph_budget",
      budget_kind = "variables"
    )
  }
  valid_vars <- all(vapply(
    vars,
    function(value) {
      gx_graph_is_string(value, nonempty = TRUE, token = TRUE) &&
        nchar(enc2utf8(value), type = "bytes") <= 256L
    },
    logical(1)
  ))
  if (!valid_vars) {
    gx_graph_abort(
      "SELECT result variables violate their finite string-array contract.",
      "gx_error_graph_payload"
    )
  }
  variables <- vapply(vars, identity, character(1), USE.NAMES = FALSE)
  if (anyDuplicated(variables)) {
    gx_graph_abort(
      "SELECT result variables must be unique.",
      "gx_error_graph_payload"
    )
  }
  list(variables = variables, links = links)
}

gx_graph_term <- function(term, scope_id) {
  if (!gx_graph_is_object(term)) {
    gx_graph_abort(
      "Each bound SPARQL value must be a term object.",
      "gx_error_graph_payload"
    )
  }
  allowed <- c("type", "value", "datatype", "xml:lang")
  if (!all(c("type", "value") %in% names(term)) ||
      length(setdiff(names(term), allowed))) {
    gx_graph_abort(
      "A SPARQL term violates the supported member contract.",
      "gx_error_graph_payload"
    )
  }
  type <- term[["type"]]
  value <- term[["value"]]
  if (!gx_graph_is_string(type, nonempty = TRUE, token = TRUE) ||
      !type %in% c("uri", "bnode", "literal") ||
      !gx_graph_is_string(value)) {
    gx_graph_abort(
      "A SPARQL term has an unsupported type or non-string value.",
      "gx_error_graph_payload"
    )
  }
  has_datatype <- "datatype" %in% names(term)
  has_language <- "xml:lang" %in% names(term)
  if ((has_datatype && has_language) ||
      (!identical(type, "literal") && (has_datatype || has_language))) {
    gx_graph_abort(
      "SPARQL term metadata is incompatible with its RDF term type.",
      "gx_error_graph_payload"
    )
  }
  datatype <- if (has_datatype) term[["datatype"]] else NA_character_
  language <- if (has_language) term[["xml:lang"]] else NA_character_
  if ((has_datatype &&
       !gx_graph_is_absolute_iri(datatype)) ||
      (has_language &&
       !gx_graph_is_language_tag(language)) ||
      (identical(type, "uri") && !gx_graph_is_absolute_iri(value)) ||
      (identical(type, "bnode") &&
       !gx_graph_is_string(value, nonempty = TRUE, token = TRUE))) {
    gx_graph_abort(
      "SPARQL term metadata contains an invalid RDF string.",
      "gx_error_graph_payload"
    )
  }
  list(
    term_type = type,
    value = value,
    datatype = datatype,
    language = language,
    bnode_scope = if (identical(type, "bnode")) scope_id else NA_character_
  )
}

gx_graph_empty_bindings <- function() {
  tibble::tibble(
    row = integer(), variable_index = integer(), variable = character(),
    term_type = character(), value = character(), datatype = character(),
    language = character(), bnode_scope = character()
  )
}

gx_graph_select <- function(root, head, limits, scope_id) {
  results <- root[["results"]]
  if (!gx_graph_is_object(results) ||
      !identical(names(results), "bindings") ||
      !gx_graph_is_array(results[["bindings"]])) {
    gx_graph_abort(
      "SELECT results must contain only a bindings array.",
      "gx_error_graph_payload"
    )
  }
  rows <- results[["bindings"]]
  if (length(rows) > limits$max_rows) {
    gx_graph_abort(
      "SELECT results exceed the configured row budget.",
      "gx_error_graph_budget",
      budget_kind = "rows"
    )
  }
  map_size <- as.integer(min(
    1000003,
    max(29, 2 * as.double(length(head$variables)))
  ))
  variable_map <- new.env(hash = TRUE, parent = emptyenv(), size = map_size)
  for (variable_index in seq_along(head$variables)) {
    assign(
      head$variables[[variable_index]],
      as.integer(variable_index),
      envir = variable_map
    )
  }
  bound_per_row <- numeric(length(rows))
  for (row_index in seq_along(rows)) {
    row <- rows[[row_index]]
    valid_names <- gx_graph_is_object(row) && all(vapply(
      names(row),
      exists,
      logical(1),
      envir = variable_map,
      inherits = FALSE
    ))
    if (!valid_names) {
      gx_graph_abort(
        "A SELECT binding row violates the declared variable contract.",
        "gx_error_graph_payload"
      )
    }
    bound_per_row[[row_index]] <- length(row)
  }
  bound_total <- sum(bound_per_row)
  if (!is.finite(bound_total) || bound_total > limits$max_bound_terms) {
    gx_graph_abort(
      "SELECT results exceed the bound-term budget.",
      "gx_error_graph_budget",
      budget_kind = "bound_terms"
    )
  }
  output_size <- as.integer(bound_total)
  output_row <- integer(output_size)
  output_variable_index <- integer(output_size)
  output_variable <- character(output_size)
  output_term_type <- character(output_size)
  output_value <- character(output_size)
  output_datatype <- rep(NA_character_, output_size)
  output_language <- rep(NA_character_, output_size)
  output_bnode_scope <- rep(NA_character_, output_size)
  bound_count <- 0L
  for (row_index in seq_along(rows)) {
    row <- rows[[row_index]]
    row_names <- names(row)
    if (!length(row_names)) next
    row_variable_indices <- vapply(
      row_names,
      get,
      integer(1),
      envir = variable_map,
      inherits = FALSE
    )
    row_order <- order(row_variable_indices)
    for (position in row_order) {
      variable <- row_names[[position]]
      variable_index <- row_variable_indices[[position]]
      bound_count <- bound_count + 1L
      term <- gx_graph_term(row[[variable]], scope_id)
      output_row[[bound_count]] <- as.integer(row_index)
      output_variable_index[[bound_count]] <- variable_index
      output_variable[[bound_count]] <- variable
      output_term_type[[bound_count]] <- term$term_type
      output_value[[bound_count]] <- term$value
      output_datatype[[bound_count]] <- term$datatype
      output_language[[bound_count]] <- term$language
      output_bnode_scope[[bound_count]] <- term$bnode_scope
    }
  }
  bindings <- if (!output_size) {
    gx_graph_empty_bindings()
  } else {
    tibble::tibble(
      row = output_row,
      variable_index = output_variable_index,
      variable = output_variable,
      term_type = output_term_type,
      value = output_value,
      datatype = output_datatype,
      language = output_language,
      bnode_scope = output_bnode_scope
    )
  }
  structure(
    list(
      contract_version = .gx_graph_contract_version,
      contract_status = "experimental",
      result_type = "select",
      variables = head$variables,
      row_count = as.integer(length(rows)),
      bindings = bindings,
      links = head$links
    ),
    class = c("gx_sparql_select", "gx_sparql_result")
  )
}

gx_graph_ask <- function(root, head) {
  value <- root[["boolean"]]
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    gx_graph_abort(
      "ASK results must contain one JSON boolean.",
      "gx_error_graph_payload"
    )
  }
  structure(
    list(
      contract_version = .gx_graph_contract_version,
      contract_status = "experimental",
      result_type = "ask",
      value = value,
      links = head$links
    ),
    class = c("gx_sparql_ask", "gx_sparql_result")
  )
}

gx_graph_parse_results <- function(body, expected, max_rows, max_variables,
                                   max_bound_terms, max_links, max_members,
                                   max_atomic_bytes, max_depth) {
  expected <- gx_graph_expected(expected)
  limits <- gx_graph_limits(
    max_rows = max_rows,
    max_variables = max_variables,
    max_bound_terms = max_bound_terms,
    max_links = max_links,
    max_requests = 1L,
    max_total_bytes = max(1L, length(body)),
    max_members = max_members,
    max_atomic_bytes = max_atomic_bytes,
    max_depth = max_depth
  )
  text <- gx_graph_json_text(body)
  gx_graph_json_preflight(text, limits$max_depth, limits$max_members)
  root <- tryCatch(
    jsonlite::fromJSON(
      text,
      simplifyVector = FALSE,
      bigint_as_char = TRUE
    ),
    error = function(cnd) {
      gx_graph_abort(
        "SPARQL Results JSON could not be parsed.",
        "gx_error_graph_payload"
      )
    }
  )
  complexity <- gx_json_measure_complexity(
    root,
    max_depth = limits$max_depth,
    max_members = limits$max_members,
    max_atomic_bytes = limits$max_atomic_bytes
  )
  if (!is.na(complexity$exceeded)) {
    gx_graph_abort(
      "SPARQL Results JSON exceeds its parsed-complexity budget.",
      "gx_error_graph_budget",
      budget_kind = complexity$exceeded
    )
  }
  gx_graph_assert_unique_members(root)
  if (!gx_graph_is_object(root) || !"head" %in% names(root)) {
    gx_graph_abort(
      "SPARQL Results JSON must be an object with a head member.",
      "gx_error_graph_payload"
    )
  }
  if ("version" %in% names(root)) {
    gx_graph_abort(
      "This graph substrate accepts SPARQL 1.1 results only.",
      "gx_error_graph_version"
    )
  }
  has_results <- "results" %in% names(root)
  has_boolean <- "boolean" %in% names(root)
  if (identical(has_results, has_boolean)) {
    gx_graph_abort(
      "SPARQL Results JSON must contain exactly one result form.",
      c("gx_error_graph_result_shape", "gx_error_graph_result_type")
    )
  }
  actual <- if (has_results) "select" else "ask"
  if (!identical(actual, expected)) {
    gx_graph_abort(
      "SPARQL result form does not match the expected query form.",
      c("gx_error_graph_expected_type", "gx_error_graph_result_type")
    )
  }
  head <- gx_graph_head(
    root[["head"]],
    expected = expected,
    max_variables = limits$max_variables,
    max_links = limits$max_links
  )
  body_sha256 <- digest::digest(body, algo = "sha256", serialize = FALSE)
  scope_id <- gx_graph_scope_id(body_sha256)
  result <- if (identical(expected, "select")) {
    gx_graph_select(root, head, limits, scope_id)
  } else {
    gx_graph_ask(root, head)
  }
  result$document_sha256 <- body_sha256
  result$complexity <- list(
    depth = complexity$depth,
    members = complexity$members,
    atomic_bytes = complexity$atomic_bytes
  )
  result
}

gx_graph_response_metadata <- function(response) {
  list(
    status = as.integer(response$status),
    media_type = gx_http_media_type(response$headers),
    bytes = as.integer(response$bytes),
    body_sha256 = response$body_sha256,
    retrieved_at = response$retrieved_at,
    cache_origin = response$cache_origin,
    from_cache = isTRUE(response$from_cache)
  )
}

gx_graph_attach_context <- function(cnd, requests, attempts, endpoint,
                                    response = NULL) {
  cnd$requests <- requests
  cnd$attempts <- attempts
  cnd$endpoint <- endpoint
  if (!is.null(response)) cnd$response <- response
  stop(cnd)
}

gx_graph_rethrow_http <- function(cnd, budget, endpoint) {
  attempts <- cnd$attempts %||% gx_http_empty_attempts()
  if (inherits(cnd, "gx_error_graph")) {
    gx_graph_attach_context(cnd, budget$ledger, attempts, endpoint)
  }
  class <- if (inherits(cnd, "gx_error_content_type")) {
    "gx_error_graph_content_type"
  } else if (inherits(cnd, c("gx_error_redirect", "gx_error_http"))) {
    "gx_error_graph_http"
  } else if (inherits(cnd, "gx_error_offline_miss")) {
    "gx_error_graph_offline"
  } else {
    "gx_error_graph_transport"
  }
  message <- switch(
    class,
    gx_error_graph_content_type = "Graph retrieval returned an unsupported media type.",
    gx_error_graph_http = "Graph retrieval failed at the HTTP boundary.",
    gx_error_graph_offline = "Graph retrieval was unavailable under offline policy.",
    "Graph retrieval failed at the bounded transport boundary."
  )
  gx_graph_abort(
    message,
    class,
    status = as.integer(cnd$status %||% NA_integer_),
    source_error = gx_http_error_code(cnd),
    requests = budget$ledger,
    attempts = attempts,
    endpoint = endpoint
  )
}

gx_graph_execute_once <- function(query, expected, client, max_rows,
                                  max_variables, max_bound_terms, max_links,
                                  max_requests, max_total_bytes, max_members,
                                  max_atomic_bytes, max_depth) {
  expected <- gx_graph_expected(expected)
  query <- gx_graph_query_text(query)
  if (!inherits(client, "gx_client") || !identical(client$endpoint, "graph")) {
    gx_graph_abort(
      "{.arg client} must be an explicit graph client.",
      "gx_error_graph_input"
    )
  }
  limits <- gx_graph_limits(
    max_rows = max_rows,
    max_variables = max_variables,
    max_bound_terms = max_bound_terms,
    max_links = max_links,
    max_requests = max_requests,
    max_total_bytes = max_total_bytes,
    max_members = max_members,
    max_atomic_bytes = max_atomic_bytes,
    max_depth = max_depth
  )
  limits$max_query_bytes <- .gx_graph_query_byte_limit
  limits$max_result_raw_bytes <- .gx_graph_response_byte_limit
  endpoint <- gx_redact_url(client$base_url)
  budget <- gx_graph_budget(
    limits$max_requests,
    limits$max_total_bytes,
    max_response_bytes = .gx_graph_response_byte_limit
  )
  parsed <- NULL
  response_validator <- function(response) {
    if (response$status < 200L || response$status >= 300L) {
      return(invisible(NULL))
    }
    candidate <- tryCatch(
      gx_graph_parse_results(
        response$body,
        expected = expected,
        max_rows = limits$max_rows,
        max_variables = limits$max_variables,
        max_bound_terms = limits$max_bound_terms,
        max_links = limits$max_links,
        max_members = limits$max_members,
        max_atomic_bytes = limits$max_atomic_bytes,
        max_depth = limits$max_depth
      ),
      error = identity
    )
    if (inherits(candidate, "error")) {
      if (inherits(candidate, "gx_error_graph")) {
        candidate$cache_invalid <- inherits(
          candidate,
          c("gx_error_graph_payload", "gx_error_graph_result_shape")
        )
        candidate$response <- gx_graph_response_metadata(response)
        stop(candidate)
      }
      gx_graph_abort(
        "SPARQL results failed bounded semantic validation.",
        "gx_error_graph_payload",
        cache_invalid = TRUE,
        response = gx_graph_response_metadata(response)
      )
    }
    parsed <<- candidate
    invisible(NULL)
  }
  response <- tryCatch(
    gx_http_request(
      client,
      method = "POST",
      url = client$base_url,
      headers = list(Accept = "application/sparql-results+json"),
      body = query$raw,
      content_type = "application/sparql-query",
      check_status = FALSE,
      .attempt_control = budget$control,
      .response_validator = response_validator
    ),
    error = function(cnd) gx_graph_rethrow_http(cnd, budget, endpoint)
  )
  attempts <- response$attempts %||% gx_http_empty_attempts()
  metadata <- gx_graph_response_metadata(response)
  if (response$status < 200L || response$status >= 300L) {
    gx_graph_abort(
      "Graph retrieval returned a non-success HTTP status.",
      "gx_error_graph_http",
      status = response$status,
      requests = budget$ledger,
      attempts = attempts,
      endpoint = endpoint,
      response = metadata
    )
  }
  if (!identical(metadata$media_type, "application/sparql-results+json")) {
    gx_graph_abort(
      "Graph retrieval did not return SPARQL Results JSON.",
      "gx_error_graph_content_type",
      requests = budget$ledger,
      attempts = attempts,
      endpoint = endpoint,
      response = metadata
    )
  }
  if (!inherits(parsed, "gx_sparql_result")) {
    gx_graph_abort(
      "Graph semantic validation did not produce a result contract.",
      "gx_error_graph_payload",
      requests = budget$ledger,
      attempts = attempts,
      endpoint = endpoint,
      response = metadata
    )
  }
  result <- parsed
  result$query_sha256 <- digest::digest(
    query$raw,
    algo = "sha256",
    serialize = FALSE
  )
  result$endpoint <- endpoint
  result$response <- metadata
  result$requests <- budget$ledger
  result$attempts <- attempts
  result$complete <- TRUE
  result$limits <- limits
  result
}
