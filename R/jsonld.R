.gx_jsonld_contract_version <- "0.1.0"
.gx_jsonld_registry_cache <- new.env(parent = emptyenv())

gx_json_atomic_bytes <- function(value) {
  if (!length(value)) return(0)
  if (is.character(value)) {
    bytes <- nchar(enc2utf8(value), type = "bytes", allowNA = TRUE)
    bytes[is.na(bytes)] <- 4
    return(sum(as.double(bytes)))
  }
  if (is.raw(value)) return(as.double(length(value)))
  as.double(length(value))
}

gx_json_measure_complexity <- function(value, max_depth = Inf,
                                       max_members = Inf,
                                       max_atomic_bytes = Inf) {
  members <- 0
  atomic_bytes <- 0
  deepest <- 0L
  stack <- list(list(value = value, depth = 1L))
  while (length(stack)) {
    item <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    x <- item$value
    if (!is.list(x)) next
    deepest <- max(deepest, item$depth)
    if (deepest > max_depth) {
      return(list(exceeded = "depth", members = members, atomic_bytes = atomic_bytes))
    }
    members <- members + length(x)
    if (members > max_members) {
      return(list(exceeded = "members", members = members, atomic_bytes = atomic_bytes))
    }
    object_names <- names(x)
    if (!is.null(object_names)) {
      atomic_bytes <- atomic_bytes + gx_json_atomic_bytes(object_names)
      if (atomic_bytes > max_atomic_bytes) {
        return(list(exceeded = "bytes", members = members, atomic_bytes = atomic_bytes))
      }
    }
    for (child in x) {
      if (is.list(child)) {
        stack[[length(stack) + 1L]] <- list(value = child, depth = item$depth + 1L)
      } else {
        if (length(child) > 1L) {
          if (item$depth + 1L > max_depth) {
            return(list(exceeded = "depth", members = members, atomic_bytes = atomic_bytes))
          }
          members <- members + length(child)
          if (members > max_members) {
            return(list(exceeded = "members", members = members, atomic_bytes = atomic_bytes))
          }
        }
        atomic_bytes <- atomic_bytes + gx_json_atomic_bytes(child)
        if (atomic_bytes > max_atomic_bytes) {
          return(list(exceeded = "bytes", members = members, atomic_bytes = atomic_bytes))
        }
      }
    }
  }
  list(
    exceeded = NA_character_, members = members,
    atomic_bytes = atomic_bytes, depth = deepest
  )
}

gx_context_asset_is_safe <- function(value) {
  stack <- list(value)
  while (length(stack)) {
    x <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    if (is.list(x)) {
      if (length(intersect(names(x) %||% character(), c("@context", "@import")))) {
        return(FALSE)
      }
      stack <- c(stack, x[vapply(x, is.list, logical(1))])
    } else if (is.character(x) &&
               any(nchar(x, type = "bytes", allowNA = TRUE) > 2048L, na.rm = TRUE)) {
      return(FALSE)
    }
  }
  TRUE
}

gx_jsonld_registry <- function() {
  if (exists("registry", envir = .gx_jsonld_registry_cache, inherits = FALSE)) {
    return(get("registry", envir = .gx_jsonld_registry_cache, inherits = FALSE))
  }
  path <- file.path(gx_asset_dir("jsonld"), "context-registry-v1.json")
  registry <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(cnd) NULL
  )
  valid <- is.list(registry) && identical(registry$registry_version, 1L) &&
    identical(registry$contract_version, .gx_jsonld_contract_version) &&
    is.list(registry$remote_contexts) && length(registry$remote_contexts) &&
    is.list(registry$known_prefixes) && length(registry$known_prefixes)
  if (!valid) {
    gx_abort("Bundled JSON-LD context registry is invalid.", "gx_error_asset")
  }
  asset_dir <- gx_asset_dir("jsonld")
  loaded_assets <- new.env(parent = emptyenv())
  registry$remote_contexts <- lapply(registry$remote_contexts, function(entry) {
    valid_entry <- is.list(entry) && identical(sort(names(entry)), c("asset", "sha256")) &&
      is.character(entry$asset) && length(entry$asset) == 1L &&
      identical(basename(entry$asset), entry$asset) &&
      is.character(entry$sha256) && length(entry$sha256) == 1L &&
      grepl("^[0-9a-f]{64}$", entry$sha256)
    if (!valid_entry) {
      gx_abort("Bundled JSON-LD context entry is invalid.", "gx_error_asset")
    }
    asset_key <- paste(entry$asset, entry$sha256, sep = ":")
    if (exists(asset_key, envir = loaded_assets, inherits = FALSE)) {
      return(get(asset_key, envir = loaded_assets, inherits = FALSE))
    }
    asset <- file.path(asset_dir, entry$asset)
    if (!file.exists(asset) ||
        !identical(digest::digest(file = asset, algo = "sha256", serialize = FALSE), entry$sha256)) {
      gx_abort("Bundled JSON-LD context asset failed integrity validation.", "gx_error_asset")
    }
    document <- tryCatch(
      jsonlite::fromJSON(asset, simplifyVector = FALSE),
      error = function(cnd) NULL
    )
    context <- if (is.list(document)) document[["@context"]] else NULL
    if (!is.list(context) || !gx_context_asset_is_safe(context)) {
      gx_abort("Bundled JSON-LD context asset is not a safe context object.", "gx_error_asset")
    }
    serialized <- gx_json_serialize(context)
    loaded <- list(
      value = context,
      bytes = nchar(serialized, type = "bytes"),
      members = gx_json_measure_complexity(context)$members
    )
    assign(asset_key, loaded, envir = loaded_assets)
    loaded
  })
  assign("registry", registry, envir = .gx_jsonld_registry_cache)
  registry
}

gx_json_text <- function(raw) {
  if (!is.raw(raw)) {
    gx_abort("JSON input must be raw bytes.", "gx_error_jsonld")
  }
  if (any(as.integer(raw) == 0L)) {
    gx_abort("JSON input contains a NUL byte.", "gx_error_jsonld_syntax")
  }
  if (length(raw) >= 3L && identical(as.integer(raw[1:3]), c(239L, 187L, 191L))) {
    raw <- raw[-(1:3)]
  }
  text <- rawToChar(raw)
  valid <- iconv(text, from = "UTF-8", to = "UTF-8", sub = NA_character_)
  if (is.na(valid)) {
    gx_abort("JSON input is not valid UTF-8.", "gx_error_jsonld_encoding")
  }
  enc2utf8(valid)
}

gx_json_assert_depth <- function(text) {
  max_depth <- gx_scalar_number(
    getOption("geoconnexr.jsonld_max_depth", 64L),
    "geoconnexr.jsonld_max_depth",
    minimum = 1,
    maximum = 10000,
    integer = TRUE
  )
  bytes <- as.integer(charToRaw(text))
  depth <- 0L
  in_string <- FALSE
  escaped <- FALSE
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
      depth <- depth + 1L
      if (depth > max_depth) {
        gx_abort(
          "JSON-LD nesting exceeds the configured depth limit of {max_depth}.",
          "gx_error_jsonld_too_deep"
        )
      }
    } else if (byte %in% c(93L, 125L)) {
      depth <- depth - 1L
      if (depth < 0L) {
        gx_abort("JSON input has unbalanced delimiters.", "gx_error_jsonld_syntax")
      }
    }
  }
  if (in_string || depth != 0L) {
    gx_abort("JSON input is incomplete.", "gx_error_jsonld_syntax")
  }
  invisible(text)
}

gx_json_parse <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    gx_abort("JSON text must be one non-missing string.", "gx_error_jsonld")
  }
  gx_json_assert_depth(text)
  value <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(cnd) {
      gx_abort("JSON-LD could not be parsed.", "gx_error_jsonld_syntax")
    }
  )
  if (!is.list(value)) {
    gx_abort("A JSON-LD document must contain an object or array.", "gx_error_jsonld_syntax")
  }
  value
}

gx_json_serialize <- function(value) {
  as.character(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  ))
}

gx_json_assert_complexity <- function(value, max_bytes = Inf) {
  max_depth <- gx_scalar_number(
    getOption("geoconnexr.jsonld_max_depth", 64L),
    "geoconnexr.jsonld_max_depth",
    minimum = 1,
    maximum = 10000,
    integer = TRUE
  )
  max_members <- gx_scalar_number(
    getOption("geoconnexr.jsonld_max_members", 10000L),
    "geoconnexr.jsonld_max_members",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  measured <- gx_json_measure_complexity(
    value,
    max_depth = max_depth,
    max_members = max_members,
    max_atomic_bytes = max_bytes
  )
  if (identical(measured$exceeded, "depth")) {
    gx_abort(
      "JSON-LD nesting exceeds the configured depth limit of {max_depth}.",
      "gx_error_jsonld_too_deep"
    )
  }
  if (identical(measured$exceeded, "members")) {
    gx_abort(
      "JSON-LD exceeds the configured object/array member limit of {max_members}.",
      "gx_error_jsonld_too_large"
    )
  }
  if (identical(measured$exceeded, "bytes")) {
    gx_abort(
      "JSON-LD atomic values exceed the configured input byte limit.",
      "gx_error_jsonld_too_large"
    )
  }
  measured$max_members <- max_members
  invisible(measured)
}

gx_context_diagnostic <- function(state, code, path, message, severity = "warning") {
  state$diagnostics <- gx_bind_diagnostics(
    state$diagnostics,
    gx_diagnostic(severity, code, path, message)
  )
  invisible(NULL)
}

gx_preflight_context_value <- function(value, path, registry, state) {
  if (is.null(value)) return(invisible(NULL))
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    entry <- registry$remote_contexts[[value]]
    if (is.null(entry)) {
      gx_abort(
        "JSON-LD context {.url {gx_redact_url(value)}} is not in the bundled allowlist.",
        "gx_error_jsonld_context"
      )
    }
    state$remote_context_bytes <- state$remote_context_bytes + entry$bytes
    state$members <- state$members + entry$members
    if (state$remote_context_bytes > state$max_context_bytes ||
        state$members > state$max_members) {
      gx_abort(
        "Bundled context replacement exceeds a configured processing limit.",
        "gx_error_jsonld_too_large"
      )
    }
    return(invisible(NULL))
  }
  if (!is.list(value)) {
    gx_abort("JSON-LD @context has an unsupported shape.", "gx_error_jsonld_context")
  }
  object <- !is.null(names(value)) && any(nzchar(names(value)))
  if (object) {
    if ("@import" %in% names(value)) {
      gx_abort("JSON-LD @import is disabled.", "gx_error_jsonld_context")
    }
    for (name in names(value)) {
      if (is.list(value[[name]])) {
        gx_preflight_contexts(value[[name]], paste0(path, "/", name), registry, state)
      }
    }
  } else {
    for (i in seq_along(value)) {
      gx_preflight_context_value(value[[i]], paste0(path, "/", i - 1L), registry, state)
    }
  }
  invisible(NULL)
}

gx_preflight_contexts <- function(value, path, registry, state) {
  if (!is.list(value)) return(invisible(NULL))
  object <- !is.null(names(value)) && any(nzchar(names(value)))
  if (object && "@context" %in% names(value)) {
    gx_preflight_context_value(
      value[["@context"]], paste0(path, "/@context"), registry, state
    )
  }
  for (i in seq_along(value)) {
    name <- if (object) names(value)[[i]] else as.character(i - 1L)
    if (!identical(name, "@context") && is.list(value[[i]])) {
      gx_preflight_contexts(value[[i]], paste0(path, "/", name), registry, state)
    }
  }
  invisible(NULL)
}

gx_source_identity_diagnostics <- function(value, path, state) {
  if (!is.list(value)) return(invisible(NULL))
  object <- !is.null(names(value)) && any(nzchar(names(value)))
  if (object && "@id" %in% names(value)) {
    ids <- value[["@id"]]
    if (is.character(ids)) {
      check <- !nzchar(ids) | grepl("^[A-Za-z][A-Za-z0-9+.-]*:", ids) |
        grepl("[[:space:][:cntrl:]]", ids)
      invalid <- ids[check & is.na(vapply(ids, gx_identity_iri, character(1)))]
      if (length(invalid)) {
        state$invalid_ids <- (state$invalid_ids %||% 0L) + length(invalid)
        if (state$invalid_ids > 64L) {
          gx_abort("JSON-LD contains too many malformed identity values.", "gx_error_jsonld_budget")
        }
        state$diagnostics <- gx_bind_diagnostics(
          state$diagnostics,
          gx_diagnostic("warning", "invalid_source_id", paste0(path, "/@id"), "Malformed @id value may be omitted by standards expansion.")
        )
      }
    }
  }
  for (i in seq_along(value)) {
    if (is.list(value[[i]])) {
      name <- if (object) names(value)[[i]] else as.character(i - 1L)
      gx_source_identity_diagnostics(value[[i]], paste0(path, "/", name), state)
    }
  }
  invisible(NULL)
}

gx_sanitize_context_value <- function(value, path, registry, state) {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    entry <- registry$remote_contexts[[value]]
    if (is.null(entry)) {
      gx_abort(
        "JSON-LD context {.url {gx_redact_url(value)}} is not in the bundled allowlist.",
        "gx_error_jsonld_context"
      )
    }
    gx_context_diagnostic(
      state,
      "remote_context_bundled",
      path,
      paste0("Replaced allowlisted remote context ", value, " with its bundled copy."),
      severity = "info"
    )
    return(entry$value)
  }
  if (!is.list(value)) {
    gx_abort("JSON-LD @context has an unsupported shape.", "gx_error_jsonld_context")
  }
  if (!is.null(names(value)) && any(nzchar(names(value)))) {
    if ("@import" %in% names(value)) {
      gx_abort("JSON-LD @import is disabled.", "gx_error_jsonld_context")
    }
    for (name in names(value)) {
      child <- value[[name]]
      if (is.list(child)) {
        value[[name]] <- gx_sanitize_contexts(
          child,
          paste0(path, "/", name),
          registry,
          state
        )
      }
    }
    strings <- unlist(value, recursive = TRUE, use.names = FALSE)
    strings <- as.character(strings[!is.na(strings)])
    if (length(strings) && any(nchar(strings, type = "bytes") > 2048L)) {
      gx_abort(
        "JSON-LD context contains an overlong mapping value.",
        "gx_error_jsonld_context"
      )
    }
    return(value)
  }
  lapply(seq_along(value), function(i) {
    gx_sanitize_context_value(value[[i]], paste0(path, "/", i - 1L), registry, state)
  })
}

gx_sanitize_contexts <- function(value, path, registry, state) {
  if (!is.list(value)) {
    return(value)
  }
  object <- !is.null(names(value)) && any(nzchar(names(value)))
  if (object && "@context" %in% names(value)) {
    sanitized <- gx_sanitize_context_value(
      value[["@context"]], paste0(path, "/@context"), registry, state
    )
    value["@context"] <- list(sanitized)
  }
  for (i in seq_along(value)) {
    name <- if (object) names(value)[[i]] else as.character(i - 1L)
    if (!identical(name, "@context") && is.list(value[[i]])) {
      value[[i]] <- gx_sanitize_contexts(
        value[[i]], paste0(path, "/", name), registry, state
      )
    }
  }
  value
}

gx_context_scope <- function(context, inherited) {
  entries <- if (is.null(context)) {
    list(NULL)
  } else if (is.list(context) && is.null(names(context))) {
    context
  } else {
    list(context)
  }
  scope <- inherited
  for (entry in entries) {
    if (is.null(entry)) {
      scope <- character()
    } else if (is.list(entry) && !is.null(names(entry))) {
      terms <- setdiff(names(entry), grep("^@", names(entry), value = TRUE))
      removed <- terms[vapply(entry[terms], is.null, logical(1))]
      scope <- setdiff(scope, removed)
      scope <- unique(c(scope, setdiff(terms, removed)))
    }
  }
  scope
}

gx_direct_prefixes <- function(value) {
  keys <- names(value) %||% character()
  keys <- keys[grepl("^[A-Za-z][A-Za-z0-9._-]*:", keys)]
  identifiers <- unlist(value[intersect(c("@id", "@type"), names(value) %||% character())], use.names = FALSE)
  identifiers <- as.character(identifiers[!is.na(identifiers)])
  identifiers <- identifiers[grepl("^[A-Za-z][A-Za-z0-9._-]*:", identifiers)]
  unique(c(sub(":.*$", "", keys), sub(":.*$", "", identifiers)))
}

gx_append_context <- function(value, additions) {
  if (!"@context" %in% (names(value) %||% character())) {
    value["@context"] <- list(additions)
    return(value)
  }
  context <- value[["@context"]]
  if (is.list(context) && !is.null(names(context)) && any(nzchar(names(context)))) {
    value["@context"] <- list(c(context, additions))
  } else {
    entries <- if (is.null(context)) list(NULL) else context
    value["@context"] <- list(c(entries, list(additions)))
  }
  value
}

gx_repair_prefixes <- function(value, inherited, path, registry, state) {
  if (!is.list(value)) return(value)
  object <- !is.null(names(value)) && any(nzchar(names(value)))
  if (!object) {
    return(lapply(seq_along(value), function(i) {
      gx_repair_prefixes(value[[i]], inherited, paste0(path, "/", i - 1L), registry, state)
    }))
  }
  scope <- inherited
  if ("@context" %in% names(value)) {
    scope <- gx_context_scope(value[["@context"]], scope)
  }
  used <- intersect(gx_direct_prefixes(value), names(registry$known_prefixes))
  missing <- setdiff(used, scope)
  if (length(missing)) {
    additions <- registry$known_prefixes[missing]
    value <- gx_append_context(value, additions)
    scope <- unique(c(scope, missing))
    for (prefix in missing) {
      gx_context_diagnostic(
        state,
        "known_prefix_repaired",
        paste0(path, "/@context"),
        paste0("Added bundled mapping for undeclared Geoconnex prefix ", prefix, ":.")
      )
    }
  }
  for (name in setdiff(names(value), "@context")) {
    if (is.list(value[[name]])) {
      value[[name]] <- gx_repair_prefixes(
        value[[name]], scope, paste0(path, "/", name), registry, state
      )
    }
  }
  value
}

gx_prepare_jsonld <- function(value, base = NULL) {
  complexity <- gx_json_assert_complexity(value)
  registry <- gx_jsonld_registry()
  state <- new.env(parent = emptyenv())
  state$diagnostics <- gx_empty_diagnostics()
  state$invalid_ids <- 0L
  state$members <- complexity$members
  state$max_members <- complexity$max_members
  state$remote_context_bytes <- 0
  state$max_context_bytes <- gx_scalar_number(
    getOption("geoconnexr.jsonld_max_context_bytes", 512L * 1024L),
    "geoconnexr.jsonld_max_context_bytes",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  gx_source_identity_diagnostics(value, "", state)
  gx_preflight_contexts(value, "", registry, state)
  value <- gx_sanitize_contexts(value, "", registry, state)

  value <- gx_repair_prefixes(value, character(), "", registry, state)

  contexts <- gx_find_property(value, "@context")
  context_bytes <- if (length(contexts)) {
    nchar(gx_json_serialize(contexts), type = "bytes")
  } else {
    0L
  }
  if (context_bytes > state$max_context_bytes) {
    gx_abort("JSON-LD contexts exceed the configured byte limit.", "gx_error_jsonld_too_large")
  }
  safe_text <- gx_json_serialize(value)
  options <- if (is.null(base)) list() else list(base = base)
  expanded_text <- tryCatch(
    jsonld::jsonld_expand(safe_text, options = options),
    error = function(cnd) {
      gx_abort("JSON-LD expansion failed.", "gx_error_jsonld_expand")
    }
  )
  max_expanded_bytes <- gx_scalar_number(
    getOption("geoconnexr.jsonld_max_expanded_bytes", 16L * 1024L^2),
    "geoconnexr.jsonld_max_expanded_bytes",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  if (nchar(as.character(expanded_text), type = "bytes") > max_expanded_bytes) {
    gx_abort("Expanded JSON-LD exceeds the configured byte limit.", "gx_error_jsonld_too_large")
  }
  expanded <- gx_json_parse(as.character(expanded_text))
  gx_json_assert_complexity(expanded)
  list(source = value, text = safe_text, expanded = expanded, diagnostics = state$diagnostics)
}

gx_media_type <- function(headers) {
  value <- tolower(gx_header(headers, "content-type") %||% "")
  if (!length(value) || !nzchar(value)) return("")
  trimws(strsplit(value, ";", fixed = TRUE)[[1]][[1]])
}

gx_jsonld_empty_requests <- function() {
  tibble::tibble(
    request_id = character(), method = character(), url = character(),
    status = integer(), media_type = character(), bytes = integer(),
    body_sha256 = character(), retrieved_at = as.POSIXct(character(), tz = "UTC"),
    cache_origin = character()
  )
}

gx_jsonld_attempt_control <- function(budget) {
  if (is.null(budget)) return(NULL)
  list(
    before = function(request, physical) {
      if (nrow(budget$ledger) >= budget$max_requests) {
        gx_abort(
          "JSON-LD retrieval exceeded its request-attempt budget.",
          "gx_error_jsonld_budget",
          budget_kind = "requests",
          requests = budget$ledger
        )
      }
      remaining <- budget$max_bytes -
        sum(as.double(budget$ledger$bytes), na.rm = TRUE)
      if (!is.finite(remaining) || remaining < 1) {
        gx_abort(
          "JSON-LD retrieval exceeded its cumulative byte budget.",
          "gx_error_jsonld_budget",
          budget_kind = "bytes",
          requests = budget$ledger
        )
      }
      as.integer(min(as.double(request$max_bytes), floor(remaining)))
    },
    after = function(attempt) {
      budget$ledger <- rbind(
        budget$ledger,
        gx_http_attempt_request_row(attempt)
      )
      budget$requests <- nrow(budget$ledger)
      budget$bytes <- sum(as.double(budget$ledger$bytes), na.rm = TRUE)
      if (budget$bytes > budget$max_bytes) {
        gx_abort(
          "JSON-LD retrieval exceeded its cumulative byte budget.",
          "gx_error_jsonld_budget",
          budget_kind = "bytes",
          requests = budget$ledger
        )
      }
      invisible(NULL)
    }
  )
}

gx_jsonld_follow_get <- function(url, client, accept, budget) {
  current <- gx_canonical_url(url)
  visited <- current
  chain <- current
  redirects <- 0L
  max_redirects <- gx_scalar_number(
    getOption("geoconnexr.max_redirects", 10L),
    "geoconnexr.max_redirects",
    minimum = 0,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  repeat {
    response <- tryCatch(
      gx_http_request(
        client,
        method = "GET",
        url = current,
        headers = list(Accept = accept),
        check_status = FALSE,
        .attempt_control = gx_jsonld_attempt_control(budget)
      ),
      error = function(cnd) {
        if (inherits(cnd, "gx_error_jsonld")) stop(cnd)
        gx_abort(
          "JSON-LD representation transport failed; underlying details were withheld.",
          "gx_error_jsonld_transport",
          requests = if (is.null(budget)) gx_jsonld_empty_requests() else budget$ledger,
          attempts = cnd$attempts %||% gx_http_empty_attempts()
        )
      }
    )
    if (response$status < 300L || response$status >= 400L) {
      if (response$status < 200L || response$status >= 300L) {
        gx_abort(
          "JSON-LD retrieval failed with HTTP status {response$status}.",
          "gx_error_jsonld_http",
          status = response$status,
          requests = if (is.null(budget)) gx_jsonld_empty_requests() else budget$ledger
        )
      }
      return(list(response = response, chain = chain))
    }
    location <- gx_header(response$headers, "location")
    if (is.null(location) || !nzchar(location)) {
      gx_abort("JSON-LD redirect omitted Location.", "gx_error_jsonld_redirect")
    }
    target <- tryCatch(httr2::url_modify_relative(current, location), error = function(cnd) NA_character_)
    if (is.na(target)) {
      gx_abort("JSON-LD redirect Location is invalid.", "gx_error_jsonld_redirect")
    }
    safe <- tryCatch({
      gx_assert_safe_url(target, resolve_dns = FALSE)
      TRUE
    }, error = function(cnd) FALSE)
    if (!safe) {
      gx_abort("JSON-LD redirect target is unsafe.", "gx_error_jsonld_redirect")
    }
    target <- gx_canonical_url(target)
    if (target %in% visited) {
      gx_abort("JSON-LD redirect loop detected.", "gx_error_jsonld_redirect")
    }
    redirects <- redirects + 1L
    if (redirects > max_redirects) {
      gx_abort("JSON-LD redirect limit exceeded.", "gx_error_jsonld_redirect")
    }
    current <- target
    visited <- c(visited, current)
    chain <- c(chain, current)
  }
}

gx_html_jsonld <- function(body, base_url, client, budget) {
  document <- tryCatch(
    xml2::read_html(body, options = c("RECOVER", "NOERROR", "NOWARNING", "NONET")),
    error = function(cnd) {
      gx_abort("HTML landing page could not be parsed.", "gx_error_jsonld_html")
    }
  )
  diagnostics <- gx_empty_diagnostics()
  scripts <- xml2::xml_find_all(document, ".//*[local-name()='script']")
  script_types <- tolower(trimws(xml2::xml_attr(scripts, "type") %||% character()))
  scripts <- scripts[!is.na(script_types) & script_types == "application/ld+json"]
  max_candidates <- gx_scalar_number(
    getOption("geoconnexr.jsonld_max_html_candidates", 64L),
    "geoconnexr.jsonld_max_html_candidates",
    minimum = 1,
    maximum = 10000,
    integer = TRUE
  )
  if (length(scripts) > max_candidates) {
    gx_abort("HTML landing page exceeds the JSON-LD candidate budget.", "gx_error_jsonld_budget")
  }
  parsed <- list()
  if (length(scripts)) {
    for (i in seq_along(scripts)) {
      text <- xml2::xml_text(scripts[[i]], trim = FALSE)
      if (!nzchar(trimws(text))) {
        diagnostics <- gx_bind_diagnostics(
          diagnostics,
          gx_diagnostic("warning", "empty_jsonld_script", paste0("/html/script/", i), "Ignored an empty JSON-LD script block.")
        )
        next
      }
      value <- tryCatch(gx_json_parse(text), error = function(cnd) cnd)
      if (inherits(value, "error")) {
        diagnostics <- gx_bind_diagnostics(
          diagnostics,
          gx_diagnostic("warning", "invalid_jsonld_script", paste0("/html/script/", i), "Ignored a JSON-LD script block containing invalid or over-deep JSON.")
        )
      } else {
        parsed[[length(parsed) + 1L]] <- value
      }
    }
  }
  if (length(parsed)) {
    graph <- unlist(lapply(parsed, function(x) {
      if (is.null(names(x)) || !any(nzchar(names(x)))) x else list(x)
    }), recursive = FALSE)
    value <- if (length(graph) == 1L) graph[[1]] else list(`@graph` = graph)
    return(list(value = value, response = NULL, diagnostics = diagnostics, discovery = "embedded"))
  }

  links <- xml2::xml_find_all(document, ".//*[local-name()='link']")
  rel <- tolower(xml2::xml_attr(links, "rel") %||% character())
  type <- tolower(trimws(xml2::xml_attr(links, "type") %||% character()))
  type <- sub(";.*$", "", type)
  href <- xml2::xml_attr(links, "href")
  alternate <- !is.na(rel) & grepl("(^|[[:space:]])alternate([[:space:]]|$)", rel) &
    !is.na(type) & type %in% c("application/ld+json", "application/json") &
    !is.na(href) & nzchar(href)
  candidates <- href[alternate]
  if (length(candidates) > max_candidates) {
    gx_abort("HTML landing page exceeds the JSON-LD alternate budget.", "gx_error_jsonld_budget")
  }
  if (!length(candidates)) {
    gx_abort("HTML landing page contains no usable JSON-LD script or alternate link.", "gx_error_jsonld_missing")
  }
  target <- NULL
  for (candidate in candidates) {
    resolved <- tryCatch(httr2::url_modify_relative(base_url, candidate), error = function(cnd) NA_character_)
    safe <- tryCatch({
      if (is.na(resolved)) stop("invalid")
      gx_assert_safe_url(resolved, resolve_dns = FALSE)
      TRUE
    }, error = function(cnd) FALSE)
    if (safe) {
      target <- resolved
      break
    }
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "unsafe_jsonld_alternate", "/html/link", "Ignored an unsafe JSON-LD alternate URL.")
    )
  }
  if (is.null(target)) {
    gx_abort("HTML landing page has no safe JSON-LD alternate link.", "gx_error_jsonld_missing")
  }
  fetched <- gx_jsonld_follow_get(
    target,
    client,
    "application/ld+json, application/json;q=0.9",
    budget
  )
  list(value = NULL, response = fetched$response, diagnostics = diagnostics, discovery = "alternate")
}

gx_jsonld_decode_response <- function(response, client, budget, allow_html = TRUE) {
  media_type <- gx_media_type(response$headers)
  is_json <- media_type %in% c("application/ld+json", "application/json", "application/geo+json") ||
    endsWith(media_type, "+json")
  is_html <- media_type %in% c("text/html", "application/xhtml+xml")
  diagnostics <- gx_empty_diagnostics()
  text <- NULL
  sniffable <- !nzchar(media_type) || media_type %in% c("application/octet-stream", "text/plain")
  if (!is_json && !is_html && sniffable) {
    text <- gx_json_text(response$body)
    leading <- sub("^[[:space:]]+", "", text)
    if (startsWith(leading, "{") || startsWith(leading, "[")) {
      is_json <- TRUE
      diagnostics <- gx_diagnostic("warning", "sniffed_json", "", "Parsed JSON despite an unrecognized Content-Type header.")
    } else if (grepl("^<(html|!doctype[[:space:]]+html)", tolower(leading))) {
      is_html <- TRUE
      diagnostics <- gx_diagnostic("warning", "sniffed_html", "", "Parsed HTML despite an unrecognized Content-Type header.")
    }
  }
  if (is_json) {
    if (is.null(text)) text <- gx_json_text(response$body)
    return(list(
      value = gx_json_parse(text),
      text = text,
      response = response,
      diagnostics = diagnostics,
      discovery = "negotiated"
    ))
  }
  if (!is_html) {
    gx_abort("Response is neither JSON-LD nor HTML.", "gx_error_jsonld_content_type")
  }
  if (!allow_html) {
    gx_abort("A JSON-LD alternate returned HTML.", "gx_error_jsonld_content_type")
  }
  html <- gx_html_jsonld(response$body, response$url, client, budget)
  diagnostics <- gx_bind_diagnostics(diagnostics, html$diagnostics)
  if (is.null(html$response)) {
    return(list(
      value = html$value,
      text = gx_json_serialize(html$value),
      response = response,
      diagnostics = diagnostics,
      discovery = html$discovery
    ))
  }
  alternate <- gx_jsonld_decode_response(html$response, client, budget, allow_html = FALSE)
  if (identical(alternate$discovery, "negotiated")) {
    alternate$discovery <- "alternate"
  }
  alternate$diagnostics <- gx_bind_diagnostics(diagnostics, alternate$diagnostics)
  alternate
}

#' Retrieve bounded Geoconnex JSON-LD
#'
#' Resolves one identifier, negotiates JSON-LD, and falls back to embedded
#' JSON-LD or one advertised JSON-LD alternate on an HTML landing page. Every
#' network request uses the bounded package transport. Remote contexts are
#' replaced only from the bundled allowlist before standards-based expansion;
#' arbitrary context loading is disabled.
#'
#' @param uri One HTTP(S) identifier.
#' @param as Representation exposed in `document`: standards-expanded, parsed
#'   source JSON, or source JSON text. The object always retains both parsed
#'   source and expanded forms for the profile parsers.
#' @param client A PID client created by [gx_client()].
#'
#' @return A `gx_jsonld` list with these fields:
#'
#' - `contract_version` and `representation` identify the experimental output
#'   contract and the form selected in `document`.
#' - `pid_uri`, `landing_url`, and `source_url` preserve identifier, redirect,
#'   and representation provenance. `source_url` is exact and can contain query
#'   credentials; do not write it to logs without redaction.
#' - `media_type`, `retrieval_mode`, `retrieved_at`, `content_sha256`,
#'   `content_bytes`, and `response_sha256` describe the selected source.
#' - `document`, `source_document`, and `expanded` contain the selected form,
#'   parsed source JSON-LD, and standards-expanded JSON-LD, respectively.
#' - `resolution` is the [gx_resolve()] row, `requests` is a request-attempt
#'   ledger with redacted URLs, and `diagnostics` has fixed columns `severity`,
#'   `code`, `path`, `message`, and `recoverable`.
#'
#' @section Processing boundary:
#' Parsers receive already-bounded bytes and cannot make network requests.
#' HTML is parsed with `NONET`; only inline scripts and one safe advertised
#' alternate is considered. Package-owned retries record every physical attempt
#' in the same ledger and cumulative budget used by PID resolution, landing
#' retrieval, and alternate discovery. The `as` argument changes only
#' `document`, not validation or expansion.
#'
#' @section Safety limits:
#' Options provide fail-closed ceilings. Defaults are 16 request attempts or
#' cache retrievals; twice the client's per-response limit in cumulative
#' response bytes; 2 MiB of local
#' JSON; depth 64; 10,000 serialized members; 512 KiB of contexts; 16 MiB of
#' expanded JSON; and 64 HTML candidates. Profile parsing additionally limits
#' one identity to 64 defining fragments and dataset output to 10,000 rows.
#' Invalid limit values fail rather than disabling a ceiling.
#'
#' All retrieval, parsing, context, and budget failures inherit from
#' `gx_error_jsonld`. See [gx_parse_location()] and [gx_parse_datasets()] for
#' tolerant profile-level diagnostics.
#' @export
gx_jsonld <- function(uri, as = c("expanded", "raw", "text"), client = gx_client("pid")) {
  if (!is.character(uri) || length(uri) != 1L || is.na(uri) || !nzchar(uri)) {
    gx_abort("{.arg uri} must be one non-empty HTTP(S) identifier.", "gx_error_identifier")
  }
  as <- tryCatch(match.arg(as), error = function(cnd) {
    gx_abort("{.arg as} must be 'expanded', 'raw', or 'text'.", "gx_error_jsonld")
  })
  if (!inherits(client, "gx_client") || !identical(client$endpoint, "pid")) {
    gx_abort("{.arg client} must be a PID client created by {.fn gx_client}.", "gx_error_client")
  }
  transport_client <- client
  budget <- new.env(parent = emptyenv())
  budget$requests <- 0L
  budget$bytes <- 0
  budget$max_requests <- gx_scalar_number(
    getOption("geoconnexr.jsonld_max_requests", 16L),
    "geoconnexr.jsonld_max_requests",
    minimum = 1,
    maximum = 1000,
    integer = TRUE
  )
  budget$max_bytes <- gx_scalar_number(
    getOption("geoconnexr.jsonld_total_bytes", 2 * transport_client$max_bytes),
    "geoconnexr.jsonld_total_bytes",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  budget$ledger <- gx_jsonld_empty_requests()
  tryCatch({
  resolution <- gx_resolve_impl(uri, client = transport_client, budget = budget)
  if (nrow(resolution) != 1L || !is.na(resolution$problem_code[[1]]) ||
      is.na(resolution$landing_url[[1]])) {
    code <- resolution$problem_code[[1]] %||% "unknown"
    gx_abort(
      "PID resolution failed before JSON-LD retrieval ({code}).",
      "gx_error_jsonld_resolution",
      requests = budget$ledger
    )
  }

  fetched <- gx_jsonld_follow_get(
    resolution$landing_url[[1]],
    transport_client,
    "application/ld+json, application/json;q=0.95, text/html;q=0.8",
    budget
  )
  decoded <- gx_jsonld_decode_response(fetched$response, transport_client, budget)
  prepared <- gx_prepare_jsonld(decoded$value, base = decoded$response$url)
  diagnostics <- gx_bind_diagnostics(decoded$diagnostics, prepared$diagnostics)
  document <- switch(as, expanded = prepared$expanded, raw = decoded$value, text = decoded$text)
  structure(
    list(
      contract_version = .gx_jsonld_contract_version,
      representation = as,
      pid_uri = uri,
      landing_url = resolution$landing_url[[1]],
      source_url = decoded$response$url,
      media_type = gx_media_type(decoded$response$headers),
      retrieval_mode = decoded$discovery,
      retrieved_at = decoded$response$retrieved_at,
      content_sha256 = digest::digest(charToRaw(enc2utf8(decoded$text)), algo = "sha256", serialize = FALSE),
      content_bytes = length(charToRaw(enc2utf8(decoded$text))),
      response_sha256 = decoded$response$body_sha256,
      document = document,
      source_document = decoded$value,
      expanded = prepared$expanded,
      resolution = resolution,
      requests = budget$ledger,
      diagnostics = diagnostics
    ),
    class = "gx_jsonld"
  )
  }, error = function(cnd) {
    if (inherits(cnd, "gx_error")) cnd$requests <- budget$ledger
    stop(cnd)
  })
}

#' @export
print.gx_jsonld <- function(x, ...) {
  cli::cli_inform(c(
    "<gx_jsonld>",
    "* PID: {gx_redact_url(x$pid_uri)}",
    "* Source: {gx_redact_url(x$source_url)} ({x$retrieval_mode})",
    "* Representation: {x$representation}",
    "* Requests: {nrow(x$requests)}; diagnostics: {nrow(x$diagnostics)}"
  ))
  invisible(x)
}
