gx_snapshot_manifest_name <- "manifest.json"
gx_snapshot_max_manifest_bytes <- 16 * 1024^2
gx_snapshot_max_depth <- 8L
gx_snapshot_max_members <- 650000
gx_snapshot_max_structural_units <- 1950002
gx_snapshot_max_requests <- 10000L
gx_snapshot_max_resources <- 10000L
gx_snapshot_max_resource_bytes <- 1024^3
gx_snapshot_max_tree_entries <- 50000L
gx_snapshot_max_path_bytes <- 1024L
gx_snapshot_max_component_bytes <- 255L
gx_snapshot_max_path_depth <- 16L
gx_snapshot_max_roles <- 16L
gx_snapshot_max_safe_integer <- 9007199254740991

gx_snapshot_abort <- function(
    message,
    subclass = "gx_error_snapshot_contract",
    call = rlang::caller_env()) {
  gx_abort(
    message,
    c(subclass, "gx_error_snapshot"),
    call = call,
    .redact_trace = TRUE
  )
}

gx_snapshot_plain_string <- function(x, nonempty = FALSE) {
  valid <- is.character(x) && !is.object(x) && length(x) == 1L &&
    is.null(attributes(x)) && !is.na(x) &&
    isTRUE(stringi::stri_enc_isutf8(x)) &&
    !isTRUE(stringi::stri_detect_regex(x, "\\p{Cc}")) &&
    (!nonempty || nzchar(x))
  if (!isTRUE(valid)) {
    gx_snapshot_abort(
      "A snapshot manifest string does not satisfy its declared contract.",
      "gx_error_snapshot_manifest"
    )
  }
  enc2utf8(x)
}

gx_snapshot_plain_boolean <- function(x) {
  valid <- is.logical(x) && !is.object(x) && length(x) == 1L &&
    is.null(attributes(x)) && !is.na(x)
  if (!isTRUE(valid)) {
    gx_snapshot_abort(
      "A snapshot manifest boolean does not satisfy its declared contract.",
      "gx_error_snapshot_manifest"
    )
  }
  x
}

gx_snapshot_plain_number <- function(
    x,
    minimum = -Inf,
    maximum = Inf,
    integer = FALSE) {
  valid <- is.numeric(x) && !is.object(x) && length(x) == 1L &&
    is.null(attributes(x)) && !is.na(x) && is.finite(x) &&
    x >= minimum && x <= maximum &&
    (!integer || (x == trunc(x) && abs(x) <= gx_snapshot_max_safe_integer))
  if (!isTRUE(valid)) {
    gx_snapshot_abort(
      "A snapshot manifest number does not satisfy its declared contract.",
      "gx_error_snapshot_manifest"
    )
  }
  if (integer && x <= .Machine$integer.max && x >= -.Machine$integer.max) {
    return(as.integer(x))
  }
  as.numeric(x)
}

gx_snapshot_plain_object <- function(x, allowed = NULL, required = character()) {
  attrs <- attributes(x)
  object_names <- attr(x, "names", exact = TRUE)
  valid <- typeof(x) == "list" && !is.object(x) && is.list(attrs) &&
    identical(names(attrs), "names") &&
    is.character(object_names) && !is.object(object_names) &&
    is.null(attributes(object_names)) &&
    length(object_names) == length(x) && !anyNA(object_names) &&
    all(stringi::stri_enc_isutf8(object_names)) &&
    !any(stringi::stri_detect_regex(object_names, "\\p{Cc}")) &&
    !anyDuplicated(object_names)
  if (!isTRUE(valid) ||
      (!is.null(allowed) && any(!object_names %in% allowed)) ||
      any(!required %in% object_names)) {
    gx_snapshot_abort(
      "A snapshot manifest object does not have its exact declared members.",
      "gx_error_snapshot_manifest"
    )
  }
  x
}

gx_snapshot_plain_array <- function(x, minimum = 0L, maximum = Inf) {
  valid <- typeof(x) == "list" && !is.object(x) &&
    is.null(attributes(x)) && length(x) >= minimum && length(x) <= maximum
  if (!isTRUE(valid)) {
    gx_snapshot_abort(
      "A snapshot manifest array exceeds or violates its declared contract.",
      "gx_error_snapshot_manifest"
    )
  }
  x
}

gx_snapshot_object_order <- function(x, order) {
  x[order[order %in% names(x)]]
}

gx_snapshot_nullable <- function(x, validator, ...) {
  if (is.null(x)) return(NULL)
  validator(x, ...)
}

gx_snapshot_sha256 <- function(x) {
  x <- gx_snapshot_plain_string(x, nonempty = TRUE)
  if (!grepl("^[a-f0-9]{64}\\z", x, perl = TRUE)) {
    gx_snapshot_abort(
      "A snapshot manifest digest is not a lowercase SHA-256 value.",
      "gx_error_snapshot_manifest"
    )
  }
  x
}

gx_snapshot_datetime <- function(x) {
  x <- gx_snapshot_plain_string(x, nonempty = TRUE)
  pattern <- paste0(
    "^([0-9]{4})-(0[1-9]|1[0-2])-([0-2][0-9]|3[01])",
    "[Tt]([01][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])",
    "(?:\\.[0-9]+)?(?:[Zz]|[+-](?:[01][0-9]|2[0-3]):[0-5][0-9])\\z"
  )
  match <- regexec(pattern, x, perl = TRUE)
  pieces <- regmatches(x, match)[[1L]]
  valid <- length(pieces) == 7L
  if (valid) {
    year <- as.integer(pieces[[2L]])
    month <- as.integer(pieces[[3L]])
    day <- as.integer(pieces[[4L]])
    leap <- (year %% 4L == 0L && year %% 100L != 0L) || year %% 400L == 0L
    month_days <- c(31L, if (leap) 29L else 28L, 31L, 30L, 31L, 30L,
                    31L, 31L, 30L, 31L, 30L, 31L)
    valid <- day <= month_days[[month]]
  }
  if (!isTRUE(valid)) {
    gx_snapshot_abort(
      "A snapshot manifest date-time is not a valid RFC 3339 value.",
      "gx_error_snapshot_manifest"
    )
  }
  x
}

gx_snapshot_uri <- function(x) {
  x <- gx_snapshot_plain_string(x, nonempty = TRUE)
  bytes <- as.integer(charToRaw(x))
  valid <- all(bytes >= 33L & bytes <= 126L) &&
    !any(bytes %in% c(34L, 60L, 62L, 92L, 94L, 96L, 123L, 124L, 125L)) &&
    !grepl("%(?![0-9A-Fa-f]{2})", x, perl = TRUE)
  match <- regexec(
    "^([A-Za-z][A-Za-z0-9+.-]*):(.*)\\z",
    x,
    perl = TRUE
  )
  pieces <- regmatches(x, match)[[1L]]
  valid <- isTRUE(valid) && length(pieces) == 3L && nzchar(pieces[[3L]])
  if (isTRUE(valid)) {
    remainder <- pieces[[3L]]
    hashes <- gregexpr("#", remainder, fixed = TRUE)[[1L]]
    hash_count <- if (length(hashes) == 1L && hashes[[1L]] < 0L) {
      0L
    } else {
      length(hashes)
    }
    valid <- hash_count <= 1L
    before_fragment <- remainder
    fragment <- NULL
    if (isTRUE(valid) && hash_count == 1L) {
      position <- hashes[[1L]]
      before_fragment <- substr(remainder, 1L, position - 1L)
      fragment <- substring(remainder, position + 1L)
    }

    query_position <- regexpr("?", before_fragment, fixed = TRUE)[[1L]]
    hierarchy <- before_fragment
    query <- NULL
    if (query_position >= 0L) {
      hierarchy <- substr(before_fragment, 1L, query_position - 1L)
      query <- substring(before_fragment, query_position + 1L)
    }
    valid <- isTRUE(valid) && nzchar(hierarchy)

    path <- hierarchy
    authority <- startsWith(hierarchy, "//")
    authority_text <- NULL
    if (authority) {
      after_slashes <- substring(hierarchy, 3L)
      slash <- regexpr("/", after_slashes, fixed = TRUE)[[1L]]
      authority_text <- if (slash < 0L) {
        after_slashes
      } else {
        substr(after_slashes, 1L, slash - 1L)
      }
      path <- if (slash < 0L) "" else substring(after_slashes, slash)
    }
    pchar <- "(?:[A-Za-z0-9._~!$&'()*+,;=:@-]|%[0-9A-Fa-f]{2})"
    path_valid <- grepl(
      paste0("^(?:", pchar, "|/)*\\z"),
      path,
      perl = TRUE
    )
    suffix_pattern <- paste0("^(?:", pchar, "|[/?])*\\z")
    query_valid <- is.null(query) || grepl(suffix_pattern, query, perl = TRUE)
    fragment_valid <- is.null(fragment) ||
      grepl(suffix_pattern, fragment, perl = TRUE)
    reg_name_char <- "(?:[A-Za-z0-9._~!$&'()*+,;=-]|%[0-9A-Fa-f]{2})"
    userinfo_char <- "(?:[A-Za-z0-9._~!$&'()*+,;=:-]|%[0-9A-Fa-f]{2})"
    ipvfuture <- paste0(
      "[Vv][0-9A-Fa-f]+\\.",
      "[A-Za-z0-9._~!$&'()*+,;=:-]+"
    )
    ip_literal <- paste0(
      "\\[(?:[0-9A-Fa-f:.]+|",
      ipvfuture,
      ")\\]"
    )
    authority_pattern <- paste0(
      "^(?:", userinfo_char, "*@)?",
      "(?:", ip_literal, "|", reg_name_char, "*)",
      "(?::[0-9]*)?\\z"
    )
    authority_valid <- is.null(authority_text) ||
      grepl(authority_pattern, authority_text, perl = TRUE)
    valid <- isTRUE(valid) && path_valid && query_valid && fragment_valid &&
      authority_valid

    ipvfuture_authority <- authority && grepl(
      paste0(ip_literal, "(?::[0-9]*)?\\z"),
      authority_text,
      perl = TRUE
    ) && grepl(
      paste0("\\[", ipvfuture, "\\]"),
      authority_text,
      perl = TRUE
    )
    parse_authority <- authority && nzchar(authority_text) &&
      !ipvfuture_authority
    if (isTRUE(valid) && parse_authority) {
      parsed <- tryCatch(
        curl::curl_parse_url(x),
        warning = function(cnd) NULL,
        error = function(cnd) NULL
      )
      valid <- !is.null(parsed)
    }
  }
  if (!isTRUE(valid)) {
    gx_snapshot_abort(
      "A snapshot manifest URI does not satisfy its absolute URI contract.",
      "gx_error_snapshot_manifest"
    )
  }
  x
}

gx_snapshot_string_array <- function(
    x,
    allowed = NULL,
    nonempty = TRUE,
    minimum = 0L,
    maximum = Inf) {
  x <- gx_snapshot_plain_array(x, minimum = minimum, maximum = maximum)
  out <- vapply(
    x,
    gx_snapshot_plain_string,
    character(1),
    nonempty = nonempty
  )
  if (anyDuplicated(out) || (!is.null(allowed) && any(!out %in% allowed))) {
    gx_snapshot_abort(
      "A snapshot manifest string array is ambiguous or contains an invalid value.",
      "gx_error_snapshot_manifest"
    )
  }
  unname(as.list(out))
}

gx_snapshot_json_bytes <- function(bytes) {
  if (!is.raw(bytes) || is.object(bytes) || !is.null(attributes(bytes)) ||
      length(bytes) > gx_snapshot_max_manifest_bytes) {
    gx_snapshot_abort(
      "The snapshot manifest exceeds its serialized-byte ceiling.",
      "gx_error_snapshot_budget"
    )
  }
  prefix <- as.integer(utils::head(bytes, 4L))
  has_prefix <- function(value) {
    length(prefix) >= length(value) && identical(prefix[seq_along(value)], value)
  }
  marked <- has_prefix(c(239L, 187L, 191L)) ||
    has_prefix(c(255L, 254L)) || has_prefix(c(254L, 255L)) ||
    has_prefix(c(0L, 0L, 254L, 255L)) ||
    has_prefix(c(255L, 254L, 0L, 0L))
  if (marked) {
    gx_snapshot_abort(
      "The snapshot manifest must be unmarked UTF-8 JSON.",
      "gx_error_snapshot_encoding"
    )
  }
  if (any(as.integer(bytes) == 0L)) {
    gx_snapshot_abort(
      "The snapshot manifest contains a NUL byte.",
      "gx_error_snapshot_syntax"
    )
  }
  text <- tryCatch(rawToChar(bytes), error = function(cnd) NA_character_)
  decoded <- if (length(text) == 1L && !is.na(text)) {
    iconv(text, from = "UTF-8", to = "UTF-8", sub = NA_character_)
  } else {
    NA_character_
  }
  if (length(decoded) != 1L || is.na(decoded)) {
    gx_snapshot_abort(
      "The snapshot manifest is not valid UTF-8.",
      "gx_error_snapshot_encoding"
    )
  }
  decoded <- enc2utf8(decoded)
  if (!identical(charToRaw(decoded), bytes)) {
    gx_snapshot_abort(
      "The snapshot manifest did not survive exact UTF-8 decoding.",
      "gx_error_snapshot_encoding"
    )
  }
  decoded
}

gx_snapshot_json_preflight <- function(text) {
  bytes <- as.integer(charToRaw(text))
  stack <- integer()
  in_string <- FALSE
  structural <- 0
  hex_value <- function(value) {
    if (value >= 48L && value <= 57L) return(value - 48L)
    if (value >= 65L && value <= 70L) return(value - 55L)
    if (value >= 97L && value <= 102L) return(value - 87L)
    NA_integer_
  }
  unicode_escape <- function(start) {
    end <- start + 3L
    if (end > length(bytes)) return(NA_integer_)
    digits <- vapply(bytes[start:end], hex_value, integer(1))
    if (anyNA(digits)) return(NA_integer_)
    sum(digits * c(4096L, 256L, 16L, 1L))
  }

  index <- 1L
  while (index <= length(bytes)) {
    byte <- bytes[[index]]
    if (in_string) {
      if (byte < 32L) {
        gx_snapshot_abort(
          "The snapshot manifest contains an unescaped control byte.",
          "gx_error_snapshot_syntax"
        )
      }
      if (byte == 34L) {
        in_string <- FALSE
        index <- index + 1L
        next
      }
      if (byte != 92L) {
        index <- index + 1L
        next
      }
      if (index == length(bytes)) {
        gx_snapshot_abort(
          "The snapshot manifest contains an incomplete string escape.",
          "gx_error_snapshot_syntax"
        )
      }
      escaped <- bytes[[index + 1L]]
      if (escaped %in% c(34L, 47L, 92L)) {
        index <- index + 2L
        next
      }
      if (escaped != 117L) {
        gx_snapshot_abort(
          "The snapshot manifest contains an invalid string escape.",
          "gx_error_snapshot_syntax"
        )
      }
      codepoint <- unicode_escape(index + 2L)
      if (is.na(codepoint) || codepoint <= 31L) {
        gx_snapshot_abort(
          "The snapshot manifest contains an invalid or control Unicode escape.",
          "gx_error_snapshot_syntax"
        )
      }
      index <- index + 6L
      if (codepoint >= 55296L && codepoint <= 56319L) {
        paired <- index + 5L <= length(bytes) &&
          bytes[[index]] == 92L && bytes[[index + 1L]] == 117L
        low <- if (paired) unicode_escape(index + 2L) else NA_integer_
        if (is.na(low) || low < 56320L || low > 57343L) {
          gx_snapshot_abort(
            "The snapshot manifest contains an unpaired Unicode surrogate.",
            "gx_error_snapshot_syntax"
          )
        }
        index <- index + 6L
      } else if (codepoint >= 56320L && codepoint <= 57343L) {
        gx_snapshot_abort(
          "The snapshot manifest contains an unpaired Unicode surrogate.",
          "gx_error_snapshot_syntax"
        )
      }
      next
    }

    if (byte == 34L) {
      in_string <- TRUE
    } else if (byte %in% c(91L, 123L)) {
      stack <- c(stack, byte)
      structural <- structural + 1
      if (length(stack) > gx_snapshot_max_depth) {
        gx_snapshot_abort(
          "The snapshot manifest exceeds its nesting-depth ceiling.",
          "gx_error_snapshot_budget"
        )
      }
    } else if (byte %in% c(93L, 125L)) {
      expected <- if (byte == 93L) 91L else 123L
      if (!length(stack) || utils::tail(stack, 1L) != expected) {
        gx_snapshot_abort(
          "The snapshot manifest contains unbalanced delimiters.",
          "gx_error_snapshot_syntax"
        )
      }
      stack <- utils::head(stack, -1L)
      structural <- structural + 1
    } else if (byte %in% c(44L, 58L)) {
      structural <- structural + 1
    }
    if (structural > gx_snapshot_max_structural_units) {
      gx_snapshot_abort(
        "The snapshot manifest exceeds its structural-unit ceiling.",
        "gx_error_snapshot_budget"
      )
    }
    index <- index + 1L
  }

  if (in_string || length(stack)) {
    gx_snapshot_abort(
      "The snapshot manifest JSON is incomplete.",
      "gx_error_snapshot_syntax"
    )
  }
  invisible(text)
}

gx_snapshot_json_assert_complexity <- function(value) {
  members <- 0
  stack <- list(list(value = value, depth = 1L))
  while (length(stack)) {
    item <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    current <- item$value
    if (typeof(current) != "list") {
      valid <- !is.object(current) && is.null(attributes(current)) &&
        typeof(current) %in% c("NULL", "logical", "integer", "double", "character") &&
        length(current) <= 1L &&
        (is.null(current) || (!is.na(current) &&
          (typeof(current) != "double" || is.finite(current)) &&
          (typeof(current) != "character" ||
            (isTRUE(stringi::stri_enc_isutf8(current)) &&
              !isTRUE(stringi::stri_detect_regex(current, "\\p{Cc}"))))))
      if (!isTRUE(valid)) {
        gx_snapshot_abort(
          "The snapshot manifest contains a non-JSON scalar value.",
          "gx_error_snapshot_syntax"
        )
      }
      next
    }

    if (is.object(current)) {
      gx_snapshot_abort(
        "The snapshot manifest contains a classed decoded value.",
        "gx_error_snapshot_syntax"
      )
    }
    attrs <- attributes(current)
    attrs_valid <- is.null(attrs) || (
      is.list(attrs) && length(attrs) == 1L && identical(names(attrs), "names")
    )
    if (!isTRUE(attrs_valid)) {
      gx_snapshot_abort(
        "The snapshot manifest contains an attributed decoded value.",
        "gx_error_snapshot_syntax"
      )
    }
    object_names <- if (is.null(attrs)) NULL else attrs[[1L]]
    names_valid <- is.null(object_names) || (
      is.character(object_names) && !is.object(object_names) &&
        is.null(attributes(object_names)) &&
        length(object_names) == length(current) && !anyNA(object_names) &&
        all(stringi::stri_enc_isutf8(object_names)) &&
        !any(stringi::stri_detect_regex(object_names, "\\p{Cc}"))
    )
    if (!isTRUE(names_valid) ||
        (!is.null(object_names) && anyDuplicated(object_names))) {
      gx_snapshot_abort(
        "The snapshot manifest contains duplicate or invalid decoded object members.",
        "gx_error_snapshot_syntax"
      )
    }

    if (item$depth > gx_snapshot_max_depth) {
      gx_snapshot_abort(
        "The snapshot manifest exceeds its decoded nesting-depth ceiling.",
        "gx_error_snapshot_budget"
      )
    }
    members <- members + length(current)
    if (members > gx_snapshot_max_members) {
      gx_snapshot_abort(
        "The snapshot manifest exceeds its decoded-member ceiling.",
        "gx_error_snapshot_budget"
      )
    }
    for (index in seq_along(current)) {
      child <- current[[index]]
      if (typeof(child) == "list") {
        stack[[length(stack) + 1L]] <- list(
          value = child,
          depth = item$depth + 1L
        )
      } else {
        stack[[length(stack) + 1L]] <- list(value = child, depth = item$depth)
      }
    }
  }
  invisible(value)
}

gx_snapshot_parse_json <- function(bytes) {
  text <- gx_snapshot_json_bytes(bytes)
  gx_snapshot_json_preflight(text)
  value <- tryCatch(
    jsonlite::parse_json(
      text,
      simplifyVector = FALSE,
      bigint_as_char = FALSE
    ),
    error = function(cnd) {
      gx_snapshot_abort(
        "The snapshot manifest could not be parsed as strict JSON.",
        "gx_error_snapshot_syntax"
      )
    }
  )
  gx_snapshot_json_assert_complexity(value)
  value
}

gx_snapshot_normalize_json <- function(x) {
  if (typeof(x) != "list") return(x)
  if (is.null(attributes(x))) {
    return(lapply(x, gx_snapshot_normalize_json))
  }
  keys <- names(x)
  keys <- keys[order(enc2utf8(keys), method = "radix")]
  out <- x[keys]
  for (key in keys) out[key] <- list(gx_snapshot_normalize_json(out[[key]]))
  out
}

gx_snapshot_validate_pipeline <- function(x) {
  fields <- c("start_stage", "end_stage")
  x <- gx_snapshot_plain_object(x, fields, fields)
  start <- gx_snapshot_plain_string(x$start_stage, nonempty = TRUE)
  end <- gx_snapshot_plain_string(x$end_stage, nonempty = TRUE)
  allowed <- list(
    aoi = c("catalog", "fetched", "harmonized", "package"),
    catalog = c("fetched", "harmonized", "package"),
    fetched = c("harmonized", "package"),
    harmonized = "package"
  )
  if (!start %in% names(allowed) || !end %in% allowed[[start]]) {
    gx_snapshot_abort(
      "The snapshot recipe pipeline is not a supported forward stage range.",
      "gx_error_snapshot_recipe"
    )
  }
  list(start_stage = start, end_stage = end)
}

gx_snapshot_validate_time <- function(x) {
  if (is.null(x)) return(NULL)
  fields <- c("start", "end")
  x <- gx_snapshot_plain_object(x, fields, fields)
  x["start"] <- list(gx_snapshot_nullable(x$start, gx_snapshot_datetime))
  x["end"] <- list(gx_snapshot_nullable(x$end, gx_snapshot_datetime))
  gx_snapshot_object_order(x, fields)
}

gx_snapshot_validate_catalog <- function(x) {
  fields <- c("include", "providers", "variables")
  x <- gx_snapshot_plain_object(x, fields)
  if ("include" %in% names(x)) {
    x["include"] <- list(gx_snapshot_string_array(
      x$include,
      allowed = c("sites", "datasets", "reference"),
      nonempty = TRUE
    ))
  }
  for (field in intersect(c("providers", "variables"), names(x))) {
    x[field] <- list(gx_snapshot_string_array(x[[field]], nonempty = TRUE))
  }
  gx_snapshot_object_order(x, fields)
}

gx_snapshot_validate_fetch <- function(x) {
  fields <- c(
    "enabled", "max_datasets", "max_requests", "max_encoded_bytes",
    "max_decoded_bytes", "handler_order"
  )
  x <- gx_snapshot_plain_object(x, fields)
  if ("enabled" %in% names(x)) {
    x["enabled"] <- list(gx_snapshot_plain_boolean(x$enabled))
  }
  for (field in intersect(
    c("max_datasets", "max_requests", "max_encoded_bytes", "max_decoded_bytes"),
    names(x)
  )) {
    x[field] <- list(gx_snapshot_plain_number(
      x[[field]],
      minimum = 0,
      maximum = gx_snapshot_max_safe_integer,
      integer = TRUE
    ))
  }
  if ("handler_order" %in% names(x)) {
    x["handler_order"] <- list(gx_snapshot_string_array(
      x$handler_order,
      nonempty = TRUE
    ))
  }
  gx_snapshot_object_order(x, fields)
}

gx_snapshot_validate_harmonize <- function(x) {
  fields <- c("enabled", "target_units")
  x <- gx_snapshot_plain_object(x, fields)
  if ("enabled" %in% names(x)) {
    x["enabled"] <- list(gx_snapshot_plain_boolean(x$enabled))
  }
  if ("target_units" %in% names(x)) {
    units <- gx_snapshot_plain_object(x$target_units)
    keys <- names(units)
    keys <- keys[order(enc2utf8(keys), method = "radix")]
    units <- units[keys]
    for (key in keys) units[key] <- list(gx_snapshot_uri(units[[key]]))
    x["target_units"] <- list(units)
  }
  gx_snapshot_object_order(x, fields)
}

gx_snapshot_validate_output <- function(x) {
  fields <- c("timeseries", "keep_raw", "report")
  x <- gx_snapshot_plain_object(x, fields)
  if ("timeseries" %in% names(x)) {
    value <- gx_snapshot_plain_string(x$timeseries, nonempty = TRUE)
    if (!value %in% c("csv", "parquet")) {
      gx_snapshot_abort(
        "The snapshot recipe output format is unsupported.",
        "gx_error_snapshot_recipe"
      )
    }
    x["timeseries"] <- list(value)
  }
  for (field in intersect(c("keep_raw", "report"), names(x))) {
    x[field] <- list(gx_snapshot_plain_boolean(x[[field]]))
  }
  gx_snapshot_object_order(x, fields)
}

gx_snapshot_validate_recipe <- function(x) {
  fields <- c(
    "contract_version", "aoi", "pipeline", "time", "catalog", "fetch",
    "harmonize", "output"
  )
  x <- gx_snapshot_plain_object(
    x,
    fields,
    c("contract_version", "aoi", "pipeline")
  )
  contract <- gx_snapshot_plain_string(x$contract_version, nonempty = TRUE)
  if (!identical(contract, "1.0.0")) {
    gx_snapshot_abort(
      "The snapshot recipe contract version is unsupported.",
      "gx_error_snapshot_recipe"
    )
  }
  pipeline <- gx_snapshot_validate_pipeline(x$pipeline)
  fragment <- list(
    contract_version = contract,
    aoi = x$aoi,
    pipeline = list(start_stage = "aoi", end_stage = "catalog")
  )
  aoi <- tryCatch(
    gx_aoi_from_recipe_impl(fragment),
    gx_error_aoi_recipe = function(cnd) {
      gx_snapshot_abort(
        "The snapshot recipe AOI failed offline hydration.",
        "gx_error_snapshot_recipe"
      )
    },
    error = function(cnd) {
      gx_snapshot_abort(
        "The snapshot recipe AOI could not be validated offline.",
        "gx_error_snapshot_recipe"
      )
    }
  )
  x["contract_version"] <- list(contract)
  x["aoi"] <- list(aoi$recipe$aoi)
  x["pipeline"] <- list(pipeline)
  if ("time" %in% names(x)) {
    x["time"] <- list(gx_snapshot_validate_time(x$time))
  }
  if ("catalog" %in% names(x)) {
    x["catalog"] <- list(gx_snapshot_validate_catalog(x$catalog))
  }
  if ("fetch" %in% names(x)) {
    x["fetch"] <- list(gx_snapshot_validate_fetch(x$fetch))
  }
  if ("harmonize" %in% names(x)) {
    x["harmonize"] <- list(gx_snapshot_validate_harmonize(x$harmonize))
  }
  if ("output" %in% names(x)) {
    x["output"] <- list(gx_snapshot_validate_output(x$output))
  }
  list(recipe = gx_snapshot_object_order(x, fields), aoi = aoi)
}

gx_snapshot_validate_package <- function(x) {
  fields <- c("name", "version")
  x <- gx_snapshot_plain_object(x, fields, fields)
  name <- gx_snapshot_plain_string(x$name, nonempty = TRUE)
  if (!identical(name, "geoconnexr")) {
    gx_snapshot_abort(
      "The snapshot package identity is unsupported.",
      "gx_error_snapshot_manifest"
    )
  }
  list(name = name, version = gx_snapshot_plain_string(x$version, nonempty = TRUE))
}

gx_snapshot_validate_replay <- function(x) {
  fields <- c("replayable", "non_replayable_reasons", "handler_versions")
  x <- gx_snapshot_plain_object(x, fields, fields)
  reasons <- gx_snapshot_string_array(x$non_replayable_reasons, nonempty = FALSE)
  handlers <- gx_snapshot_plain_array(x$handler_versions)
  handler_fields <- c("handler", "implementation_id", "version", "replayable")
  normalized <- vector("list", length(handlers))
  for (index in seq_along(handlers)) {
    item <- gx_snapshot_plain_object(handlers[[index]], handler_fields, handler_fields)
    normalized[[index]] <- list(
      handler = gx_snapshot_plain_string(item$handler, nonempty = TRUE),
      implementation_id = gx_snapshot_plain_string(
        item$implementation_id,
        nonempty = TRUE
      ),
      version = gx_snapshot_plain_string(item$version, nonempty = TRUE),
      replayable = gx_snapshot_plain_boolean(item$replayable)
    )
  }
  list(
    replayable = gx_snapshot_plain_boolean(x$replayable),
    non_replayable_reasons = reasons,
    handler_versions = normalized
  )
}

gx_snapshot_validate_endpoints <- function(x) {
  x <- gx_snapshot_plain_object(x)
  if (!length(x)) {
    gx_snapshot_abort(
      "The snapshot manifest must declare at least one endpoint.",
      "gx_error_snapshot_manifest"
    )
  }
  keys <- names(x)
  if (any(!nzchar(keys))) {
    gx_snapshot_abort(
      "Snapshot endpoint names must be non-empty.",
      "gx_error_snapshot_manifest"
    )
  }
  keys <- keys[order(enc2utf8(keys), method = "radix")]
  x <- x[keys]
  for (key in keys) x[key] <- list(gx_snapshot_uri(x[[key]]))
  x
}

gx_snapshot_validate_hydrologic_vintage <- function(x) {
  fields <- c("reference_collection", "vintage", "migration_policy")
  x <- gx_snapshot_plain_object(x, fields)
  for (field in names(x)) {
    x[field] <- list(gx_snapshot_plain_string(x[[field]], nonempty = FALSE))
  }
  gx_snapshot_object_order(x, fields)
}

gx_snapshot_validate_asset_hashes <- function(x) {
  fields <- c("queries", "handler_registry", "vocabulary")
  x <- gx_snapshot_plain_object(x, fields, fields)
  list(
    queries = gx_snapshot_sha256(x$queries),
    handler_registry = gx_snapshot_sha256(x$handler_registry),
    vocabulary = gx_snapshot_sha256(x$vocabulary)
  )
}

gx_snapshot_validate_request <- function(x) {
  fields <- c(
    "request_id", "stage", "method", "canonical_url_redacted",
    "request_hash", "body_hash", "final_url", "response_status",
    "response_media_type", "encoded_bytes", "decoded_bytes", "content_hash",
    "etag", "last_modified", "retrieved_at", "elapsed_ms", "cache_origin",
    "error_code"
  )
  required <- c(
    "request_id", "stage", "method", "canonical_url_redacted",
    "request_hash", "retrieved_at", "cache_origin"
  )
  x <- gx_snapshot_plain_object(x, fields, required)
  x["request_id"] <- list(gx_snapshot_plain_string(x$request_id, nonempty = TRUE))
  x["stage"] <- list(gx_snapshot_plain_string(x$stage, nonempty = TRUE))
  method <- gx_snapshot_plain_string(x$method, nonempty = TRUE)
  if (!method %in% c("GET", "HEAD", "POST")) {
    gx_snapshot_abort(
      "A snapshot request method is unsupported.",
      "gx_error_snapshot_request"
    )
  }
  x["method"] <- list(method)
  x["canonical_url_redacted"] <- list(gx_snapshot_plain_string(
    x$canonical_url_redacted,
    nonempty = TRUE
  ))
  x["request_hash"] <- list(gx_snapshot_sha256(x$request_hash))
  if ("body_hash" %in% names(x)) {
    x["body_hash"] <- list(gx_snapshot_nullable(x$body_hash, gx_snapshot_sha256))
  }
  if ("final_url" %in% names(x)) {
    x["final_url"] <- list(gx_snapshot_nullable(x$final_url, gx_snapshot_uri))
  }
  if ("response_status" %in% names(x)) {
    x["response_status"] <- list(gx_snapshot_nullable(
      x$response_status,
      gx_snapshot_plain_number,
      minimum = 100,
      maximum = 599,
      integer = TRUE
    ))
  }
  for (field in intersect(
    c("response_media_type", "etag", "last_modified", "error_code"),
    names(x)
  )) {
    x[field] <- list(gx_snapshot_nullable(
      x[[field]],
      gx_snapshot_plain_string,
      nonempty = FALSE
    ))
  }
  for (field in intersect(c("encoded_bytes", "decoded_bytes"), names(x))) {
    x[field] <- list(gx_snapshot_nullable(
      x[[field]],
      gx_snapshot_plain_number,
      minimum = 0,
      maximum = gx_snapshot_max_resource_bytes,
      integer = TRUE
    ))
  }
  if ("content_hash" %in% names(x)) {
    x["content_hash"] <- list(gx_snapshot_nullable(
      x$content_hash,
      gx_snapshot_sha256
    ))
  }
  x["retrieved_at"] <- list(gx_snapshot_datetime(x$retrieved_at))
  if ("elapsed_ms" %in% names(x)) {
    x["elapsed_ms"] <- list(gx_snapshot_nullable(
      x$elapsed_ms,
      gx_snapshot_plain_number,
      minimum = 0
    ))
  }
  cache <- gx_snapshot_plain_string(x$cache_origin, nonempty = TRUE)
  if (!cache %in% c("network", "fresh_cache", "offline_cache")) {
    gx_snapshot_abort(
      "A snapshot request cache origin is unsupported.",
      "gx_error_snapshot_request"
    )
  }
  x["cache_origin"] <- list(cache)
  gx_snapshot_object_order(x, fields)
}

gx_snapshot_validate_requests <- function(x) {
  x <- gx_snapshot_plain_array(x, maximum = gx_snapshot_max_requests)
  out <- lapply(x, gx_snapshot_validate_request)
  ids <- vapply(out, `[[`, character(1), "request_id")
  if (anyDuplicated(ids)) {
    gx_snapshot_abort(
      "The embedded snapshot request ledger contains duplicate request identities.",
      "gx_error_snapshot_request"
    )
  }
  out
}

gx_snapshot_path <- function(x) {
  x <- gx_snapshot_plain_string(x, nonempty = TRUE)
  bytes <- as.integer(charToRaw(x))
  if (length(bytes) > gx_snapshot_max_path_bytes ||
      any(bytes < 32L | bytes > 126L) || any(bytes %in% c(58L, 92L)) ||
      startsWith(x, "/") || endsWith(x, "/")) {
    gx_snapshot_abort(
      "A snapshot resource path violates its portable relative-path contract.",
      "gx_error_snapshot_path"
    )
  }
  components <- strsplit(x, "/", fixed = TRUE)[[1L]]
  portable <- "^[A-Za-z0-9_-](?:[A-Za-z0-9._-]*[A-Za-z0-9_-])?\\z"
  reserved <- "^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(?:\\..*)?\\z"
  invalid <- !length(components) || length(components) > gx_snapshot_max_path_depth ||
    any(!nzchar(components)) || any(components %in% c(".", "..")) ||
    any(nchar(components, type = "bytes") > gx_snapshot_max_component_bytes) ||
    any(!grepl(portable, components, perl = TRUE)) ||
    any(grepl("[. ]\\z", components, perl = TRUE)) ||
    any(grepl(reserved, components, ignore.case = TRUE, perl = TRUE))
  if (invalid) {
    gx_snapshot_abort(
      "A snapshot resource path contains an unsafe or non-portable component.",
      "gx_error_snapshot_path"
    )
  }
  x
}

gx_snapshot_ascii_fold <- function(x) {
  chartr(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    "abcdefghijklmnopqrstuvwxyz",
    x
  )
}

gx_snapshot_validate_resource <- function(x) {
  fields <- c(
    "path", "media_type", "bytes", "sha256", "required", "roles",
    "source_uri", "license_uri"
  )
  required <- c("path", "media_type", "bytes", "sha256", "required", "roles")
  x <- gx_snapshot_plain_object(x, fields, required)
  x["path"] <- list(gx_snapshot_path(x$path))
  x["media_type"] <- list(gx_snapshot_plain_string(x$media_type, nonempty = TRUE))
  x["bytes"] <- list(gx_snapshot_plain_number(
    x$bytes,
    minimum = 0,
    maximum = gx_snapshot_max_resource_bytes,
    integer = TRUE
  ))
  x["sha256"] <- list(gx_snapshot_sha256(x$sha256))
  x["required"] <- list(gx_snapshot_plain_boolean(x$required))
  x["roles"] <- list(gx_snapshot_string_array(
    x$roles,
    nonempty = TRUE,
    minimum = 1L,
    maximum = gx_snapshot_max_roles
  ))
  for (field in intersect(c("source_uri", "license_uri"), names(x))) {
    x[field] <- list(gx_snapshot_nullable(x[[field]], gx_snapshot_uri))
  }
  gx_snapshot_object_order(x, fields)
}

gx_snapshot_assert_resource_paths <- function(resources) {
  paths <- vapply(resources, `[[`, character(1), "path")
  folded <- gx_snapshot_ascii_fold(paths)
  if (any(folded == gx_snapshot_ascii_fold(gx_snapshot_manifest_name)) ||
      anyDuplicated(paths) || anyDuplicated(folded)) {
    gx_snapshot_abort(
      "Snapshot resource paths are duplicated, aliased, or self-referential.",
      "gx_error_snapshot_path"
    )
  }
  order <- order(folded, method = "radix")
  folded <- folded[order]
  if (length(folded) > 1L) {
    for (index in seq_len(length(folded) - 1L)) {
      if (startsWith(folded[[index + 1L]], paste0(folded[[index]], "/"))) {
        gx_snapshot_abort(
          "Snapshot resource paths contain a file-directory prefix collision.",
          "gx_error_snapshot_path"
        )
      }
    }
  }
  total <- sum(vapply(resources, function(x) as.numeric(x$bytes), numeric(1)))
  if (!is.finite(total) || total > gx_snapshot_max_resource_bytes) {
    gx_snapshot_abort(
      "Snapshot resources exceed the aggregate stored-byte ceiling.",
      "gx_error_snapshot_budget"
    )
  }
  order(paths, method = "radix")
}

gx_snapshot_validate_resources <- function(x) {
  x <- gx_snapshot_plain_array(
    x,
    minimum = 1L,
    maximum = gx_snapshot_max_resources
  )
  resources <- lapply(x, gx_snapshot_validate_resource)
  resources[gx_snapshot_assert_resource_paths(resources)]
}

gx_snapshot_validate_completeness <- function(x) {
  x <- gx_snapshot_plain_array(x, minimum = 1L)
  fields <- c("stage", "status", "truncated", "reason")
  required <- c("stage", "status", "truncated")
  lapply(x, function(item) {
    item <- gx_snapshot_plain_object(item, fields, required)
    item["stage"] <- list(gx_snapshot_plain_string(item$stage, nonempty = TRUE))
    status <- gx_snapshot_plain_string(item$status, nonempty = TRUE)
    if (!status %in% c("complete", "partial", "not_run", "unknown")) {
      gx_snapshot_abort(
        "A snapshot completeness status is unsupported.",
        "gx_error_snapshot_manifest"
      )
    }
    item["status"] <- list(status)
    item["truncated"] <- list(gx_snapshot_plain_boolean(item$truncated))
    if ("reason" %in% names(item)) {
      item["reason"] <- list(gx_snapshot_plain_string(item$reason, nonempty = FALSE))
    }
    gx_snapshot_object_order(item, fields)
  })
}

gx_snapshot_validate_source_licenses <- function(x) {
  x <- gx_snapshot_plain_array(x)
  fields <- c("source_uri", "license_uri", "status")
  required <- c("source_uri", "status")
  lapply(x, function(item) {
    item <- gx_snapshot_plain_object(item, fields, required)
    item["source_uri"] <- list(gx_snapshot_uri(item$source_uri))
    if ("license_uri" %in% names(item)) {
      item["license_uri"] <- list(gx_snapshot_nullable(
        item$license_uri,
        gx_snapshot_uri
      ))
    }
    status <- gx_snapshot_plain_string(item$status, nonempty = TRUE)
    if (!status %in% c("declared", "missing", "unknown")) {
      gx_snapshot_abort(
        "A snapshot source-license status is unsupported.",
        "gx_error_snapshot_manifest"
      )
    }
    item["status"] <- list(status)
    gx_snapshot_object_order(item, fields)
  })
}

gx_snapshot_validate_session <- function(x) {
  fields <- c("r_version", "platform", "locale", "packages")
  required <- c("r_version", "platform", "locale")
  x <- gx_snapshot_plain_object(x, fields, required)
  for (field in required) {
    x[field] <- list(gx_snapshot_plain_string(x[[field]], nonempty = FALSE))
  }
  if ("packages" %in% names(x)) {
    package_fields <- c("package", "version")
    packages <- gx_snapshot_plain_array(x$packages)
    x["packages"] <- list(lapply(packages, function(item) {
      item <- gx_snapshot_plain_object(item, package_fields, package_fields)
      list(
        package = gx_snapshot_plain_string(item$package, nonempty = FALSE),
        version = gx_snapshot_plain_string(item$version, nonempty = FALSE)
      )
    }))
  }
  gx_snapshot_object_order(x, fields)
}

gx_snapshot_validate_manifest <- function(x) {
  fields <- c(
    "contract_version", "manifest_version", "package", "created_at", "recipe",
    "replay", "effective_options", "endpoints", "hydrologic_vintage",
    "asset_hashes", "requests", "resources", "completeness",
    "source_licenses", "session"
  )
  required <- c(
    "contract_version", "manifest_version", "package", "created_at", "recipe",
    "replay", "endpoints", "asset_hashes", "requests", "resources",
    "completeness", "session"
  )
  x <- gx_snapshot_plain_object(x, fields, required)
  contract <- gx_snapshot_plain_string(x$contract_version, nonempty = TRUE)
  version <- gx_snapshot_plain_string(x$manifest_version, nonempty = TRUE)
  if (!identical(contract, "1.0.0") || !identical(version, "1.0.0")) {
    gx_snapshot_abort(
      "The snapshot manifest contract version is unsupported.",
      "gx_error_snapshot_manifest"
    )
  }
  recipe <- gx_snapshot_validate_recipe(x$recipe)
  x["contract_version"] <- list(contract)
  x["manifest_version"] <- list(version)
  x["package"] <- list(gx_snapshot_validate_package(x$package))
  x["created_at"] <- list(gx_snapshot_datetime(x$created_at))
  x["recipe"] <- list(recipe$recipe)
  x["replay"] <- list(gx_snapshot_validate_replay(x$replay))
  if ("effective_options" %in% names(x)) {
    options <- gx_snapshot_plain_object(x$effective_options)
    x["effective_options"] <- list(gx_snapshot_normalize_json(options))
  }
  x["endpoints"] <- list(gx_snapshot_validate_endpoints(x$endpoints))
  if ("hydrologic_vintage" %in% names(x)) {
    x["hydrologic_vintage"] <- list(gx_snapshot_validate_hydrologic_vintage(
      x$hydrologic_vintage
    ))
  }
  x["asset_hashes"] <- list(gx_snapshot_validate_asset_hashes(x$asset_hashes))
  x["requests"] <- list(gx_snapshot_validate_requests(x$requests))
  x["resources"] <- list(gx_snapshot_validate_resources(x$resources))
  x["completeness"] <- list(gx_snapshot_validate_completeness(x$completeness))
  if ("source_licenses" %in% names(x)) {
    x["source_licenses"] <- list(gx_snapshot_validate_source_licenses(
      x$source_licenses
    ))
  }
  x["session"] <- list(gx_snapshot_validate_session(x$session))
  list(manifest = gx_snapshot_object_order(x, fields), aoi = recipe$aoi)
}

gx_snapshot_fs_info <- function(path) {
  info <- tryCatch(
    fs::file_info(path),
    warning = function(cnd) NULL,
    error = function(cnd) NULL
  )
  if (is.null(info) || nrow(info) != 1L) {
    gx_snapshot_abort(
      "Snapshot filesystem metadata could not be inspected safely.",
      "gx_error_snapshot_io"
    )
  }
  info
}

gx_snapshot_readlink <- function(path) {
  target <- tryCatch(
    Sys.readlink(path),
    warning = function(cnd) NA_character_,
    error = function(cnd) NA_character_
  )
  if (length(target) != 1L || is.na(target)) {
    gx_snapshot_abort(
      "A snapshot filesystem entry could not be checked for links.",
      "gx_error_snapshot_io"
    )
  }
  target
}

gx_snapshot_assert_fs_type <- function(path, type) {
  target <- gx_snapshot_readlink(path)
  info <- gx_snapshot_fs_info(path)
  observed <- as.character(info$type[[1L]])
  hard_links <- as.numeric(info$hard_links[[1L]])
  unsafe_links <- identical(type, "file") &&
    (!is.finite(hard_links) || hard_links != 1)
  if (nzchar(target) || is.na(observed) || !identical(observed, type) ||
      unsafe_links) {
    gx_snapshot_abort(
      "A snapshot filesystem entry has an unsafe type or is a symbolic link.",
      "gx_error_snapshot_tree"
    )
  }
  info
}

gx_snapshot_info_signature <- function(info) {
  list(
    type = as.character(info$type[[1L]]),
    size = as.numeric(info$size[[1L]]),
    permissions = as.character(info$permissions[[1L]]),
    modification_time = as.numeric(info$modification_time[[1L]]),
    change_time = as.numeric(info$change_time[[1L]]),
    device_id = as.numeric(info$device_id[[1L]]),
    inode = as.numeric(info$inode[[1L]]),
    hard_links = as.numeric(info$hard_links[[1L]])
  )
}

gx_snapshot_hash_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

gx_snapshot_assert_same_info <- function(before, after) {
  if (!identical(
    gx_snapshot_info_signature(before),
    gx_snapshot_info_signature(after)
  )) {
    gx_snapshot_abort(
      "A snapshot filesystem entry changed during verification.",
      "gx_error_snapshot_mutation"
    )
  }
  invisible(after)
}

gx_snapshot_root <- function(dir) {
  valid <- is.character(dir) && !is.object(dir) && length(dir) == 1L &&
    is.null(attributes(dir)) && !is.na(dir) && nzchar(dir) &&
    isTRUE(stringi::stri_enc_isutf8(dir))
  if (!isTRUE(valid)) {
    gx_snapshot_abort(
      "Snapshot verification requires one existing literal directory.",
      "gx_error_snapshot_input"
    )
  }
  absolute <- tryCatch(
    fs::path_abs(path.expand(dir)),
    warning = function(cnd) NULL,
    error = function(cnd) NULL
  )
  if (is.null(absolute) || length(absolute) != 1L) {
    gx_snapshot_abort(
      "The snapshot root could not be resolved safely.",
      "gx_error_snapshot_input"
    )
  }
  gx_snapshot_assert_fs_type(as.character(absolute), "directory")
  root <- tryCatch(
    normalizePath(as.character(absolute), winslash = "/", mustWork = TRUE),
    warning = function(cnd) NA_character_,
    error = function(cnd) NA_character_
  )
  if (length(root) != 1L || is.na(root) || !nzchar(root)) {
    gx_snapshot_abort(
      "The snapshot root could not be normalized safely.",
      "gx_error_snapshot_input"
    )
  }
  list(path = root, info = gx_snapshot_assert_fs_type(root, "directory"))
}

gx_snapshot_read_manifest <- function(root) {
  path <- file.path(root, gx_snapshot_manifest_name)
  before <- gx_snapshot_assert_fs_type(path, "file")
  size <- as.numeric(before$size[[1L]])
  if (!is.finite(size) || size < 0 || size > gx_snapshot_max_manifest_bytes ||
      size != trunc(size)) {
    gx_snapshot_abort(
      "The snapshot manifest exceeds its file-byte ceiling.",
      "gx_error_snapshot_budget"
    )
  }
  bytes <- tryCatch(
    readBin(path, what = "raw", n = as.integer(size + 1)),
    warning = function(cnd) NULL,
    error = function(cnd) NULL
  )
  after <- gx_snapshot_assert_fs_type(path, "file")
  gx_snapshot_assert_same_info(before, after)
  if (is.null(bytes) || length(bytes) != size) {
    gx_snapshot_abort(
      "The snapshot manifest could not be read as exact bounded bytes.",
      "gx_error_snapshot_io"
    )
  }
  list(
    path = path,
    bytes = bytes,
    info = after,
    sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE)
  )
}

gx_snapshot_allowed_directories <- function(paths) {
  split_paths <- strsplit(paths, "/", fixed = TRUE)
  counts <- pmax(lengths(split_paths) - 1L, 0L)
  out <- character(sum(counts))
  position <- 0L
  for (components in split_paths) {
    if (length(components) > 1L) {
      for (depth in seq_len(length(components) - 1L)) {
        position <- position + 1L
        out[[position]] <- paste(
          components[seq_len(depth)],
          collapse = "/"
        )
      }
    }
  }
  unique(out)
}

gx_snapshot_tree_inventory <- function(root, resource_paths) {
  allowed_directories <- gx_snapshot_allowed_directories(resource_paths)
  stack <- list(list(full = root, relative = "", depth = 0L))
  rows <- list()
  count <- 0L
  while (length(stack)) {
    item <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    directory_info <- gx_snapshot_assert_fs_type(item$full, "directory")
    children <- tryCatch(
      fs::dir_ls(
        item$full,
        all = TRUE,
        recurse = FALSE,
        type = "any",
        fail = TRUE
      ),
      warning = function(cnd) NULL,
      error = function(cnd) NULL
    )
    gx_snapshot_assert_same_info(
      directory_info,
      gx_snapshot_assert_fs_type(item$full, "directory")
    )
    if (is.null(children)) {
      gx_snapshot_abort(
        "The snapshot file tree could not be enumerated safely.",
        "gx_error_snapshot_io"
      )
    }
    children <- as.character(fs::path_file(children))
    children <- children[order(enc2utf8(children), method = "radix")]
    for (name in children) {
      relative <- if (nzchar(item$relative)) {
        paste(item$relative, name, sep = "/")
      } else {
        name
      }
      relative <- gx_snapshot_path(relative)
      depth <- length(strsplit(relative, "/", fixed = TRUE)[[1L]])
      if (depth > gx_snapshot_max_path_depth) {
        gx_snapshot_abort(
          "The snapshot file tree exceeds its depth ceiling.",
          "gx_error_snapshot_budget"
        )
      }
      count <- count + 1L
      if (count > gx_snapshot_max_tree_entries) {
        gx_snapshot_abort(
          "The snapshot file tree exceeds its entry ceiling.",
          "gx_error_snapshot_budget"
        )
      }
      full <- file.path(item$full, name)
      target <- gx_snapshot_readlink(full)
      info <- gx_snapshot_fs_info(full)
      type <- as.character(info$type[[1L]])
      if (nzchar(target) || is.na(type) || !type %in% c("file", "directory")) {
        gx_snapshot_abort(
          "The snapshot file tree contains a link or special entry.",
          "gx_error_snapshot_tree"
        )
      }
      if (identical(type, "directory") && !relative %in% allowed_directories) {
        gx_snapshot_abort(
          "The snapshot file tree contains an undeclared directory.",
          "gx_error_snapshot_tree"
        )
      }
      if (identical(type, "file") &&
          !relative %in% c(gx_snapshot_manifest_name, resource_paths)) {
        gx_snapshot_abort(
          "The snapshot file tree contains an undeclared file.",
          "gx_error_snapshot_tree"
        )
      }
      rows[[length(rows) + 1L]] <- list(
        path = relative,
        type = type,
        info = info
      )
      if (identical(type, "directory")) {
        stack[[length(stack) + 1L]] <- list(
          full = full,
          relative = relative,
          depth = depth
        )
      }
    }
  }
  paths <- vapply(rows, `[[`, character(1), "path")
  if (anyDuplicated(gx_snapshot_ascii_fold(paths))) {
    gx_snapshot_abort(
      "The snapshot file tree contains case-folding path aliases.",
      "gx_error_snapshot_tree"
    )
  }
  root_info <- gx_snapshot_assert_fs_type(root, "directory")
  order <- order(paths, method = "radix")
  list(root_info = root_info, entries = rows[order])
}

gx_snapshot_inventory_signature <- function(inventory) {
  list(
    root = gx_snapshot_info_signature(inventory$root_info),
    entries = lapply(inventory$entries, function(item) {
      list(
        path = item$path,
        type = item$type,
        info = gx_snapshot_info_signature(item$info)
      )
    })
  )
}

gx_snapshot_verify_resources <- function(root, resources, inventory) {
  inventory_paths <- vapply(inventory$entries, `[[`, character(1), "path")
  inventory_types <- vapply(inventory$entries, `[[`, character(1), "type")
  rows <- vector("list", length(resources))
  for (index in seq_along(resources)) {
    resource <- resources[[index]]
    position <- match(resource$path, inventory_paths)
    present <- !is.na(position)
    if (present && !identical(inventory_types[[position]], "file")) {
      gx_snapshot_abort(
        "A declared snapshot resource is not a regular file.",
        "gx_error_snapshot_resource"
      )
    }
    if (!present && isTRUE(resource$required)) {
      gx_snapshot_abort(
        "A required snapshot resource is absent.",
        "gx_error_snapshot_resource"
      )
    }

    actual_bytes <- NA_real_
    actual_sha256 <- NA_character_
    status <- "missing_optional"
    if (present) {
      full <- file.path(root, resource$path)
      before <- gx_snapshot_assert_fs_type(full, "file")
      gx_snapshot_assert_same_info(inventory$entries[[position]]$info, before)
      actual_bytes <- as.numeric(before$size[[1L]])
      if (!is.finite(actual_bytes) || actual_bytes != as.numeric(resource$bytes)) {
        gx_snapshot_abort(
          "A snapshot resource has an unexpected byte count.",
          "gx_error_snapshot_resource"
        )
      }
      actual_sha256 <- tryCatch(
        gx_snapshot_hash_file(full),
        warning = function(cnd) NA_character_,
        error = function(cnd) NA_character_
      )
      after <- gx_snapshot_assert_fs_type(full, "file")
      gx_snapshot_assert_same_info(before, after)
      if (is.na(actual_sha256) || !identical(actual_sha256, resource$sha256)) {
        gx_snapshot_abort(
          "A snapshot resource failed SHA-256 verification.",
          "gx_error_snapshot_resource"
        )
      }
      status <- "verified"
    }
    rows[[index]] <- tibble::tibble(
      path = resource$path,
      media_type = resource$media_type,
      expected_bytes = as.numeric(resource$bytes),
      expected_sha256 = resource$sha256,
      required = resource$required,
      roles = list(unname(unlist(resource$roles, use.names = FALSE))),
      present = present,
      actual_bytes = actual_bytes,
      actual_sha256 = actual_sha256,
      status = status
    )
  }
  do.call(rbind, rows)
}

# Internal M9a boundary. It verifies one closed snapshot directory without
# executing its recipe, parsing requests.csv, opening URLs, or writing files.
gx_snapshot_verify_impl <- function(dir) {
  tryCatch(
    {
      root <- gx_snapshot_root(dir)
      manifest_file <- gx_snapshot_read_manifest(root$path)
      decoded <- gx_snapshot_parse_json(manifest_file$bytes)
      validated <- gx_snapshot_validate_manifest(decoded)
      resource_paths <- vapply(
        validated$manifest$resources,
        `[[`,
        character(1),
        "path"
      )
      before <- gx_snapshot_tree_inventory(root$path, resource_paths)
      gx_snapshot_assert_same_info(root$info, before$root_info)
      manifest_position <- match(
        gx_snapshot_manifest_name,
        vapply(before$entries, `[[`, character(1), "path")
      )
      if (is.na(manifest_position)) {
        gx_snapshot_abort(
          "The fixed snapshot manifest disappeared during verification.",
          "gx_error_snapshot_mutation"
        )
      }
      gx_snapshot_assert_same_info(
        manifest_file$info,
        before$entries[[manifest_position]]$info
      )
      resources <- gx_snapshot_verify_resources(
        root$path,
        validated$manifest$resources,
        before
      )
      after <- gx_snapshot_tree_inventory(root$path, resource_paths)
      if (!identical(
        gx_snapshot_inventory_signature(before),
        gx_snapshot_inventory_signature(after)
      )) {
        gx_snapshot_abort(
          "The snapshot file tree changed during verification.",
          "gx_error_snapshot_mutation"
        )
      }
      list(
        contract_version = "1.0.0",
        mode = "offline_snapshot_verification",
        manifest = validated$manifest,
        aoi = validated$aoi,
        resources = tibble::as_tibble(resources),
        status = if (any(resources$status == "missing_optional")) {
          "verified_with_optional_absences"
        } else {
          "verified"
        },
        request_count = length(validated$manifest$requests),
        request_ledger_status = "shape_validated",
        manifest_sha256 = manifest_file$sha256,
        verified_at = gx_now()
      )
    },
    error = function(cnd) {
      if (inherits(cnd, "gx_error_snapshot")) stop(cnd)
      gx_snapshot_abort(
        "Snapshot verification failed closed at its offline boundary.",
        "gx_error_snapshot_contract"
      )
    }
  )
}
