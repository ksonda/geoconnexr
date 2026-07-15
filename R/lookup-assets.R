.gx_lookup_data_schema_version <- "1"
.gx_lookup_receipt_version <- 1L

gx_lookup_abort <- function(message, subclass, ...) {
  gx_abort(
    message,
    c(subclass, "gx_error_crosswalk_lookup", "gx_error_asset"),
    ...,
    .redact_trace = TRUE
  )
}

gx_lookup_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    gx_lookup_abort(
      "{.arg {name}} must be one non-missing logical value.",
      "gx_error_crosswalk_lookup_input"
    )
  }
  x
}

gx_lookup_string <- function(x, name) {
  valid <- is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x) &&
    isTRUE(stringi::stri_enc_isutf8(x)) &&
    !stringi::stri_detect_regex(x, "[\\p{Cc}\\p{Cf}\\p{Cs}]")
  if (!valid) {
    gx_lookup_abort(
      "{.arg {name}} must be one non-empty UTF-8 string.",
      "gx_error_crosswalk_lookup_input"
    )
  }
  x
}

gx_lookup_https_url <- function(x) {
  parsed <- tryCatch(httr2::url_parse(x), error = function(cnd) NULL)
  is.list(parsed) && identical(tolower(parsed$scheme %||% ""), "https") &&
    is.character(parsed$hostname) && length(parsed$hostname) == 1L &&
    nzchar(parsed$hostname) && is.null(parsed$username) &&
    is.null(parsed$password) && is.null(parsed$query) &&
    is.null(parsed$fragment)
}

gx_mainstem_lookup_registry <- function() {
  path <- file.path(
    gx_asset_dir("mainstem-lookups"),
    "registry-v1.json"
  )
  registry <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE, bigint_as_char = TRUE),
    error = function(cnd) {
      gx_lookup_abort(
        "The mainstem lookup registry could not be parsed.",
        "gx_error_crosswalk_lookup_registry"
      )
    }
  )
  if (!is.list(registry) ||
      !identical(names(registry), c("registry_version", "lookups")) ||
      !is.numeric(registry$registry_version) ||
      length(registry$registry_version) != 1L ||
      registry$registry_version != 1 ||
      !is.list(registry$lookups) || !length(registry$lookups)) {
    gx_lookup_abort(
      "The mainstem lookup registry has an invalid top-level contract.",
      "gx_error_crosswalk_lookup_registry"
    )
  }
  required <- c(
    "lookup_id", "release", "tag_commit", "asset_id", "asset_name",
    "source_url", "release_url", "repository_url", "allowed_hosts",
    "media_types", "encoding", "line_ending", "final_newline", "columns",
    "rows", "bytes", "sha256", "forward_cardinality", "active_state",
    "license", "known_answers", "known_absent"
  )
  lookups <- lapply(registry$lookups, function(spec) {
    if (!is.list(spec) || !identical(names(spec), required)) {
      gx_lookup_abort(
        "A mainstem lookup registry entry has an invalid shape.",
        "gx_error_crosswalk_lookup_registry"
      )
    }
    scalar_names <- c(
      "lookup_id", "release", "tag_commit", "asset_id", "asset_name",
      "source_url", "release_url", "repository_url", "encoding",
      "line_ending", "sha256", "forward_cardinality", "active_state",
      "license"
    )
    scalars_ok <- all(vapply(spec[scalar_names], function(value) {
      is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
    }, logical(1)))
    allowed_hosts <- unlist(spec$allowed_hosts, use.names = FALSE)
    media_types <- unlist(spec$media_types, use.names = FALSE)
    columns <- unlist(spec$columns, use.names = FALSE)
    known_absent <- unlist(spec$known_absent, use.names = FALSE)
    numbers_ok <- all(vapply(c("rows", "bytes"), function(name) {
      value <- spec[[name]]
      is.numeric(value) && length(value) == 1L && !is.na(value) &&
        is.finite(value) && value >= 1 && value == trunc(value) &&
        value <= .Machine$integer.max
    }, logical(1)))
    known_answers <- spec$known_answers
    answers_ok <- is.list(known_answers) && length(known_answers) &&
      all(vapply(known_answers, function(answer) {
        is.list(answer) && identical(names(answer), c("comid", "mainstem_uri")) &&
          is.character(answer$comid) && length(answer$comid) == 1L &&
          grepl("^[1-9][0-9]{0,9}\\z", answer$comid, perl = TRUE) &&
          is.character(answer$mainstem_uri) &&
          length(answer$mainstem_uri) == 1L &&
          gx_crosswalk_valid_mainstem_uri(answer$mainstem_uri, allow_na = FALSE)
      }, logical(1)))
    urls_ok <- all(vapply(
      spec[c("source_url", "release_url", "repository_url")],
      gx_lookup_https_url,
      logical(1)
    ))
    host <- tryCatch(
      tolower(httr2::url_parse(spec$source_url)$hostname),
      error = function(cnd) NA_character_
    )
    valid <- scalars_ok && numbers_ok && answers_ok && urls_ok &&
      grepl("^[0-9a-f]{40}$", spec$tag_commit) &&
      grepl("^[0-9]+$", spec$asset_id) &&
      grepl("^[A-Za-z0-9._-]+$", spec$asset_name) &&
      identical(basename(spec$asset_name), spec$asset_name) &&
      is.character(allowed_hosts) && length(allowed_hosts) >= 2L &&
      !anyNA(allowed_hosts) && !anyDuplicated(allowed_hosts) &&
      all(grepl("^[a-z0-9.-]+$", allowed_hosts)) && host %in% allowed_hosts &&
      is.character(media_types) && length(media_types) && !anyNA(media_types) &&
      identical(columns, c("uri", "comid")) &&
      identical(spec$encoding, "UTF-8") &&
      identical(spec$line_ending, "LF") &&
      is.logical(spec$final_newline) && length(spec$final_newline) == 1L &&
      isTRUE(spec$final_newline) &&
      grepl("^[0-9a-f]{64}$", spec$sha256) &&
      spec$forward_cardinality %in% c("zero_or_one", "zero_or_many") &&
      identical(spec$license, "CC0-1.0") &&
      is.character(known_absent) && length(known_absent) &&
      !anyNA(known_absent) &&
      all(grepl("^[1-9][0-9]{0,9}\\z", known_absent, perl = TRUE))
    if (!valid) {
      gx_lookup_abort(
        "A mainstem lookup registry entry failed validation.",
        "gx_error_crosswalk_lookup_registry"
      )
    }
    spec$registry_version <- as.integer(registry$registry_version)
    spec$allowed_hosts <- allowed_hosts
    spec$media_types <- media_types
    spec$columns <- columns
    spec$known_absent <- known_absent
    spec$rows <- as.integer(spec$rows)
    spec$bytes <- as.integer(spec$bytes)
    spec$known_answers <- tibble::tibble(
      comid = vapply(known_answers, `[[`, character(1), "comid"),
      mainstem_uri = vapply(known_answers, `[[`, character(1), "mainstem_uri")
    )
    spec
  })
  ids <- vapply(lookups, `[[`, character(1), "lookup_id")
  releases <- vapply(lookups, `[[`, character(1), "release")
  if (anyDuplicated(ids) || anyDuplicated(releases)) {
    gx_lookup_abort(
      "Mainstem lookup IDs and releases must be unique.",
      "gx_error_crosswalk_lookup_registry"
    )
  }
  lookups
}

gx_mainstem_lookup_spec <- function(version = "v3.2") {
  version <- gx_lookup_string(version, "version")
  registry <- gx_mainstem_lookup_registry()
  releases <- vapply(registry, `[[`, character(1), "release")
  match <- which(releases == version)
  if (length(match) != 1L) {
    gx_lookup_abort(
      "No pinned mainstem lookup is registered for release {.val {version}}.",
      "gx_error_crosswalk_lookup_registry"
    )
  }
  registry[[match]]
}

gx_lookup_validate_data_dir <- function(data_dir) {
  data_dir <- gx_lookup_string(data_dir, "data_dir")
  data_dir <- path.expand(data_dir)
  if (file.exists(data_dir) && !dir.exists(data_dir)) {
    gx_lookup_abort(
      "{.arg data_dir} exists but is not a directory.",
      "gx_error_crosswalk_lookup_io"
    )
  }
  if (dir.exists(data_dir) && gx_path_is_symlink(data_dir)) {
    gx_lookup_abort(
      "A symbolic-link data directory is not allowed for lookup installation.",
      "gx_error_crosswalk_lookup_ownership"
    )
  }
  data_dir
}

gx_lookup_data_marker <- function(data_dir) {
  file.path(data_dir, ".geoconnexr-data")
}

gx_lookup_prepare_data_dir <- function(data_dir) {
  data_dir <- gx_lookup_validate_data_dir(data_dir)
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(data_dir)) {
    gx_lookup_abort(
      "The package data directory could not be created.",
      "gx_error_crosswalk_lookup_io"
    )
  }
  marker <- gx_lookup_data_marker(data_dir)
  expected <- paste0("geoconnexr-data-schema:", .gx_lookup_data_schema_version)
  if (!file.exists(marker)) {
    existing <- list.files(data_dir, all.files = TRUE, no.. = TRUE)
    if (length(existing)) {
      gx_lookup_abort(
        "Refusing to use an unmarked, non-empty package data directory.",
        "gx_error_crosswalk_lookup_ownership"
      )
    }
    tryCatch(
      writeLines(expected, marker, useBytes = TRUE),
      error = function(cnd) {
        gx_lookup_abort(
          "The package data-directory marker could not be written.",
          "gx_error_crosswalk_lookup_io"
        )
      }
    )
  } else {
    observed <- tryCatch(readLines(marker, warn = FALSE), error = function(cnd) character())
    if (!identical(observed, expected) || gx_path_is_symlink(marker)) {
      gx_lookup_abort(
        "The package data-directory marker is invalid.",
        "gx_error_crosswalk_lookup_ownership"
      )
    }
  }
  data_dir
}

gx_mainstem_lookup_parent <- function(data_dir, create = FALSE) {
  data_dir <- if (create) {
    gx_lookup_prepare_data_dir(data_dir)
  } else {
    gx_lookup_validate_data_dir(data_dir)
  }
  parent <- file.path(data_dir, "mainstem-lookups")
  if ((file.exists(parent) || dir.exists(parent)) &&
      (!dir.exists(parent) || gx_path_is_symlink(parent))) {
    gx_lookup_abort(
      "The mainstem lookup parent is not a regular owned directory.",
      "gx_error_crosswalk_lookup_ownership"
    )
  }
  if (create && !dir.exists(parent)) {
    dir.create(parent, showWarnings = FALSE)
  }
  if (create && !dir.exists(parent)) {
    gx_lookup_abort(
      "The mainstem lookup directory could not be created.",
      "gx_error_crosswalk_lookup_io"
    )
  }
  parent
}

gx_mainstem_lookup_install_dir <- function(spec, data_dir) {
  file.path(
    gx_mainstem_lookup_parent(data_dir),
    paste0(spec$lookup_id, "-", substr(spec$sha256, 1L, 12L))
  )
}

gx_mainstem_lookup_asset_path <- function(spec, data_dir) {
  file.path(gx_mainstem_lookup_install_dir(spec, data_dir), spec$asset_name)
}

gx_mainstem_lookup_receipt_path <- function(spec, data_dir) {
  file.path(gx_mainstem_lookup_install_dir(spec, data_dir), "receipt-v1.json")
}

gx_lookup_iso_time <- function(x) {
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}

gx_lookup_parse_time <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  value <- suppressWarnings(as.POSIXct(
    x,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  ))
  if (length(value) != 1L || is.na(value)) as.POSIXct(NA, tz = "UTC") else value
}

gx_mainstem_lookup_write_receipt <- function(path, spec, source, redirect_hosts) {
  receipt <- list(
    receipt_version = .gx_lookup_receipt_version,
    lookup_id = spec$lookup_id,
    registry_version = spec$registry_version,
    release = spec$release,
    tag_commit = spec$tag_commit,
    asset_id = spec$asset_id,
    asset_name = spec$asset_name,
    asset_bytes = spec$bytes,
    asset_sha256 = spec$sha256,
    installed_at = gx_lookup_iso_time(gx_now()),
    source = source,
    source_url = spec$source_url,
    redirect_hosts = as.list(unique(redirect_hosts)),
    parser_contract = "csv-uri-comid-v1"
  )
  tryCatch(
    jsonlite::write_json(
      receipt,
      path,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    ),
    error = function(cnd) {
      gx_lookup_abort(
        "The mainstem lookup receipt could not be written.",
        "gx_error_crosswalk_lookup_io"
      )
    }
  )
  invisible(receipt)
}

gx_mainstem_lookup_read_receipt <- function(path, spec) {
  receipt <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE, bigint_as_char = TRUE),
    error = function(cnd) NULL
  )
  required <- c(
    "receipt_version", "lookup_id", "registry_version", "release",
    "tag_commit", "asset_id", "asset_name", "asset_bytes", "asset_sha256",
    "installed_at", "source", "source_url", "redirect_hosts",
    "parser_contract"
  )
  scalar_string <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value)
  }
  scalar_integer <- function(value, expected) {
    is.numeric(value) && length(value) == 1L && !is.na(value) &&
      is.finite(value) && value == trunc(value) &&
      value >= 0 && value <= .Machine$integer.max &&
      identical(as.integer(value), expected)
  }
  scalar_fields <- setdiff(
    required,
    c("receipt_version", "registry_version", "asset_bytes", "redirect_hosts")
  )
  shape_ok <- is.list(receipt) && identical(names(receipt), required) &&
    all(vapply(receipt[scalar_fields], scalar_string, logical(1))) &&
    is.list(receipt$redirect_hosts) &&
    is.null(names(receipt$redirect_hosts)) &&
    all(vapply(receipt$redirect_hosts, scalar_string, logical(1)))
  if (!isTRUE(shape_ok)) {
    gx_lookup_abort(
      "The installed mainstem lookup receipt is missing or invalid.",
      "gx_error_crosswalk_lookup_integrity"
    )
  }
  installed_at <- gx_lookup_parse_time(receipt$installed_at)
  hosts <- if (length(receipt$redirect_hosts)) {
    vapply(receipt$redirect_hosts, identity, character(1))
  } else {
    character()
  }
  source_host <- tolower(httr2::url_parse(spec$source_url)$hostname)
  source_chain_ok <- if (identical(receipt$source, "local_import")) {
    !length(hosts)
  } else if (identical(receipt$source, "release_download")) {
    length(hosts) >= 1L && identical(hosts[[1]], source_host)
  } else {
    FALSE
  }
  valid <- scalar_integer(
    receipt$receipt_version,
    .gx_lookup_receipt_version
  ) &&
    identical(receipt$lookup_id, spec$lookup_id) &&
    scalar_integer(receipt$registry_version, spec$registry_version) &&
    identical(receipt$release, spec$release) &&
    identical(receipt$tag_commit, spec$tag_commit) &&
    identical(receipt$asset_id, spec$asset_id) &&
    identical(receipt$asset_name, spec$asset_name) &&
    scalar_integer(receipt$asset_bytes, spec$bytes) &&
    identical(receipt$asset_sha256, spec$sha256) &&
    !is.na(installed_at) &&
    source_chain_ok &&
    identical(receipt$source_url, spec$source_url) &&
    is.character(hosts) && !anyNA(hosts) && !anyDuplicated(hosts) &&
    all(hosts %in% spec$allowed_hosts) &&
    identical(receipt$parser_contract, "csv-uri-comid-v1")
  if (!isTRUE(valid)) {
    gx_lookup_abort(
      "The installed mainstem lookup receipt is missing or invalid.",
      "gx_error_crosswalk_lookup_integrity"
    )
  }
  receipt$receipt_version <- as.integer(receipt$receipt_version)
  receipt$registry_version <- as.integer(receipt$registry_version)
  receipt$asset_bytes <- as.integer(receipt$asset_bytes)
  receipt$installed_at <- installed_at
  receipt$redirect_hosts <- hosts
  receipt
}

gx_mainstem_lookup_chunk_rows <- function() {
  gx_scalar_number(
    getOption("geoconnexr.lookup_chunk_rows", 50000L),
    "geoconnexr.lookup_chunk_rows",
    minimum = 1,
    maximum = 250000,
    integer = TRUE
  )
}

gx_mainstem_lookup_scan <- function(path, spec, targets = character(),
                                    target_field = "comid",
                                    max_matches = .Machine$integer.max) {
  targets <- unique(as.character(targets))
  if (!is.character(target_field) || length(target_field) != 1L ||
      is.na(target_field) || !target_field %in% c("comid", "uri")) {
    gx_lookup_abort(
      "The mainstem lookup target field is invalid.",
      "gx_error_crosswalk_lookup_contract"
    )
  }
  max_matches <- gx_scalar_number(
    max_matches,
    "max_matches",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  known_targets <- spec$known_answers$comid
  known_comids <- unique(c(known_targets, spec$known_absent))
  connection <- tryCatch(
    file(path, open = "rt", encoding = "UTF-8"),
    error = function(cnd) NULL
  )
  if (is.null(connection)) {
    gx_lookup_abort(
      "The mainstem lookup file could not be opened.",
      "gx_error_crosswalk_lookup_io"
    )
  }
  on.exit(close(connection), add = TRUE)
  header <- tryCatch(
    readLines(connection, n = 1L, warn = FALSE),
    error = function(cnd) character()
  )
  if (!identical(header, paste(spec$columns, collapse = ","))) {
    gx_lookup_abort(
      "The mainstem lookup CSV header does not match its pinned schema.",
      "gx_error_crosswalk_lookup_schema"
    )
  }
  rows <- 0L
  found <- list()
  repeat {
    lines <- tryCatch(
      readLines(connection, n = gx_mainstem_lookup_chunk_rows(), warn = FALSE),
      error = function(cnd) {
        gx_lookup_abort(
          "The mainstem lookup CSV could not be read.",
          "gx_error_crosswalk_lookup_io"
        )
      }
    )
    if (!length(lines)) break
    rows <- rows + length(lines)
    if (rows > spec$rows) {
      gx_lookup_abort(
        "The mainstem lookup CSV exceeds its pinned row count.",
        "gx_error_crosswalk_lookup_schema"
      )
    }
    utf8 <- tryCatch(
      stringi::stri_enc_isutf8(lines),
      error = function(cnd) rep(FALSE, length(lines))
    )
    comma_count <- stringi::stri_count_fixed(lines, ",")
    pieces <- stringi::stri_split_fixed(
      lines,
      ",",
      n = 2L,
      omit_empty = FALSE,
      simplify = TRUE
    )
    shape_ok <- is.matrix(pieces) && nrow(pieces) == length(lines) &&
      ncol(pieces) == 2L
    if (!shape_ok || anyNA(utf8) || !all(utf8) || any(comma_count != 1L)) {
      gx_lookup_abort(
        "The mainstem lookup CSV contains malformed or non-UTF-8 rows.",
        "gx_error_crosswalk_lookup_schema"
      )
    }
    uri <- pieces[, 1L]
    comid <- pieces[, 2L]
    valid_uri <- stringi::stri_detect_regex(
      uri,
      "^https://geoconnex[.]us/ref/mainstems/[1-9][0-9]*\\z"
    )
    uri_bytes <- rep(Inf, length(uri))
    valid_encoding <- !is.na(utf8) & utf8
    uri_bytes[valid_encoding] <- nchar(
      enc2utf8(uri[valid_encoding]),
      type = "bytes"
    )
    valid_uri <- valid_uri & uri_bytes <= 256L
    valid_comid <- stringi::stri_detect_regex(
      comid,
      "^[1-9][0-9]{0,9}\\z"
    )
    if (anyNA(valid_uri) || anyNA(valid_comid) ||
        !all(valid_uri) || !all(valid_comid)) {
      gx_lookup_abort(
        "The mainstem lookup CSV contains invalid URI or COMID values.",
        "gx_error_crosswalk_lookup_schema"
      )
    }
    requested_hit <- if (identical(target_field, "comid")) {
      comid %in% targets
    } else {
      uri %in% targets
    }
    hit <- requested_hit | comid %in% known_comids
    if (any(hit)) {
      found[[length(found) + 1L]] <- tibble::tibble(
        uri = uri[hit],
        comid = comid[hit]
      )
      found_rows <- do.call(rbind, found)
      row_key <- paste(found_rows$comid, found_rows$uri, sep = "\r")
      if (anyDuplicated(row_key)) {
        gx_lookup_abort(
          "The mainstem lookup contains a duplicate identifier mapping row.",
          "gx_error_crosswalk_lookup_integrity"
        )
      }
      requested_matches <- if (identical(target_field, "comid")) {
        sum(found_rows$comid %in% targets)
      } else {
        sum(found_rows$uri %in% targets)
      }
      if (requested_matches > max_matches) {
        gx_lookup_abort(
          "The mainstem lookup exceeded its match budget while scanning.",
          "gx_error_crosswalk_budget"
        )
      }
    }
  }
  if (rows != spec$rows) {
    gx_lookup_abort(
      "The mainstem lookup CSV does not match its pinned row count.",
      "gx_error_crosswalk_lookup_schema"
    )
  }
  final_byte <- tryCatch({
    binary <- file(path, open = "rb")
    on.exit(close(binary), add = TRUE)
    seek(binary, where = -1, origin = "end")
    readBin(binary, what = "raw", n = 1L)
  }, error = function(cnd) raw())
  if (!identical(final_byte, as.raw(0x0a))) {
    gx_lookup_abort(
      "The mainstem lookup CSV is missing its pinned final newline.",
      "gx_error_crosswalk_lookup_schema"
    )
  }
  found <- if (length(found)) {
    tibble::as_tibble(do.call(rbind, found))
  } else {
    tibble::tibble(uri = character(), comid = character())
  }
  for (index in seq_len(nrow(spec$known_answers))) {
    expected <- spec$known_answers[index, , drop = FALSE]
    observed <- found$uri[found$comid == expected$comid]
    if (!identical(observed, expected$mainstem_uri)) {
      gx_lookup_abort(
        "The mainstem lookup failed a pinned known-answer check.",
        "gx_error_crosswalk_lookup_integrity"
      )
    }
  }
  if (any(found$comid %in% spec$known_absent)) {
    gx_lookup_abort(
      "The mainstem lookup failed a pinned known-absence check.",
      "gx_error_crosswalk_lookup_integrity"
    )
  }
  requested <- if (identical(target_field, "comid")) {
    found[found$comid %in% targets, , drop = FALSE]
  } else {
    found[found$uri %in% targets, , drop = FALSE]
  }
  requested <- if (identical(target_field, "comid")) {
    requested[order(requested$comid, requested$uri, method = "radix"), , drop = FALSE]
  } else {
    requested[order(requested$uri, requested$comid, method = "radix"), , drop = FALSE]
  }
  if (identical(target_field, "comid") &&
      identical(spec$forward_cardinality, "zero_or_one") &&
      anyDuplicated(requested$comid)) {
    gx_lookup_abort(
      "The mainstem lookup violated its pinned forward-cardinality contract.",
      "gx_error_crosswalk_lookup_integrity"
    )
  }
  tibble::as_tibble(requested)
}

gx_mainstem_lookup_verify <- function(path, spec, parse = TRUE,
                                      require_receipt = TRUE) {
  regular <- file.exists(path) && !dir.exists(path) &&
    !gx_path_is_symlink(path) && !gx_path_is_symlink(dirname(path))
  info <- if (regular) file.info(path) else NULL
  bytes <- if (!is.null(info)) as.double(info$size[[1]]) else NA_real_
  if (!regular || !is.finite(bytes) || bytes != spec$bytes) {
    gx_lookup_abort(
      "The mainstem lookup file is missing or has the wrong byte count.",
      "gx_error_crosswalk_lookup_integrity"
    )
  }
  sha256 <- tryCatch(
    digest::digest(file = path, algo = "sha256", serialize = FALSE),
    error = function(cnd) NA_character_
  )
  if (!identical(sha256, spec$sha256)) {
    gx_lookup_abort(
      "The mainstem lookup file failed SHA-256 verification.",
      "gx_error_crosswalk_lookup_integrity"
    )
  }
  if (isTRUE(parse)) {
    gx_mainstem_lookup_scan(path, spec)
  }
  receipt <- NULL
  if (isTRUE(require_receipt)) {
    receipt <- gx_mainstem_lookup_read_receipt(
      file.path(dirname(path), "receipt-v1.json"),
      spec
    )
  }
  list(
    path = path,
    bytes = bytes,
    sha256 = sha256,
    verified_at = gx_now(),
    receipt = receipt
  )
}

gx_mainstem_download_response_limit <- function() {
  gx_scalar_number(
    getOption("geoconnexr.asset_max_redirects", 5L),
    "geoconnexr.asset_max_redirects",
    minimum = 0,
    maximum = 10,
    integer = TRUE
  )
}

gx_mainstem_download_timeout <- function() {
  gx_scalar_number(
    getOption("geoconnexr.asset_download_timeout", 600),
    "geoconnexr.asset_download_timeout",
    minimum = 1,
    maximum = 3600,
    integer = FALSE
  )
}

gx_mainstem_lookup_download <- function(path, spec) {
  client <- gx_client(
    "pid",
    timeout = gx_mainstem_download_timeout(),
    retries = 0L,
    max_bytes = spec$bytes,
    cache = FALSE,
    offline = FALSE
  )
  current <- gx_canonical_url(spec$source_url)
  visited <- current
  redirects <- 0L
  max_redirects <- gx_mainstem_download_response_limit()
  ledger <- tibble::tibble(
    host = character(),
    status = integer(),
    bytes = numeric(),
    retrieved_at = as.POSIXct(character(), tz = "UTC")
  )
  repeat {
    parsed <- httr2::url_parse(current)
    host <- tolower(parsed$hostname)
    if (!identical(tolower(parsed$scheme), "https") ||
        !host %in% spec$allowed_hosts) {
      gx_lookup_abort(
        "The mainstem lookup download left its pinned HTTPS host allowlist.",
        "gx_error_crosswalk_download"
      )
    }
    hop_limit <- if (identical(host, spec$allowed_hosts[[1]])) {
      min(spec$bytes, 1024L^2)
    } else {
      spec$bytes
    }
    hop_path <- tempfile(pattern = ".hop-", tmpdir = dirname(path))
    response <- tryCatch(
      gx_http_download_file(
        client,
        current,
        hop_path,
        headers = list(Accept = paste(spec$media_types, collapse = ", ")),
        max_bytes = hop_limit,
        check_status = FALSE
      ),
      error = function(cnd) {
        if (file.exists(hop_path)) unlink(hop_path, force = TRUE)
        gx_lookup_abort(
          "The mainstem lookup download failed its bounded transport contract.",
          "gx_error_crosswalk_download"
        )
      }
    )
    ledger <- rbind(
      ledger,
      tibble::tibble(
        host = host,
        status = response$status,
        bytes = response$bytes,
        retrieved_at = response$retrieved_at
      )
    )
    if (response$status >= 300L && response$status < 400L) {
      location <- gx_header(response$headers, "location")
      unlink(hop_path, force = TRUE)
      if (is.null(location) || !nzchar(location)) {
        gx_lookup_abort(
          "The mainstem lookup redirect omitted Location.",
          "gx_error_crosswalk_download"
        )
      }
      target <- tryCatch(
        httr2::url_modify_relative(current, location),
        error = function(cnd) NA_character_
      )
      target <- tryCatch(gx_canonical_url(target), error = function(cnd) NA_character_)
      if (is.na(target) || target %in% visited) {
        gx_lookup_abort(
          "The mainstem lookup redirect was invalid or repeated.",
          "gx_error_crosswalk_download"
        )
      }
      redirects <- redirects + 1L
      if (redirects > max_redirects) {
        gx_lookup_abort(
          "The mainstem lookup exceeded its redirect budget.",
          "gx_error_crosswalk_download"
        )
      }
      current <- target
      visited <- c(visited, current)
      next
    }
    media_type <- tolower(strsplit(
      gx_header(response$headers, "content-type") %||% "",
      ";",
      fixed = TRUE
    )[[1]][[1]])
    valid_final <- response$status == 200L && response$bytes == spec$bytes &&
      media_type %in% spec$media_types
    if (!valid_final) {
      unlink(hop_path, force = TRUE)
      gx_lookup_abort(
        "The mainstem lookup download returned an unexpected final response.",
        "gx_error_crosswalk_download"
      )
    }
    if (!file.rename(hop_path, path)) {
      unlink(hop_path, force = TRUE)
      gx_lookup_abort(
        "The verified download could not be moved into its staging path.",
        "gx_error_crosswalk_lookup_io"
      )
    }
    return(ledger)
  }
}

#' Inspect an installed mainstem lookup
#'
#' Verifies the content-addressed optional `ref_rivers` lookup used by the
#' experimental M4 crosswalk substrate. Inspection never downloads or repairs
#' data. A missing or invalid installation is reported in the returned row.
#' The lookup contains mainstems that were non-superseded when v3.2 was
#' generated; current reference-service state is deliberately not checked.
#'
#' @param version Pinned lookup release. Currently only `"v3.2"` is
#'   registered.
#' @param data_dir Persistent package data directory. This is separate from the
#'   HTTP cache managed by [gx_cache_info()].
#'
#' @return A one-row tibble describing availability, integrity, pinned and
#'   observed provenance, release-state semantics, and verification time.
#' @export
gx_mainstem_lookup_info <- function(version = "v3.2",
                                    data_dir = gx_default_data_dir()) {
  spec <- gx_mainstem_lookup_spec(version)
  data_dir <- gx_lookup_validate_data_dir(data_dir)
  path <- gx_mainstem_lookup_asset_path(spec, data_dir)
  available <- file.exists(path) && !dir.exists(path)
  verified <- FALSE
  verification <- NULL
  if (available) {
    verification <- tryCatch(
      gx_mainstem_lookup_verify(path, spec, parse = TRUE, require_receipt = TRUE),
      error = function(cnd) NULL
    )
    verified <- !is.null(verification)
  }
  actual_bytes <- if (available) as.double(file.info(path)$size[[1]]) else NA_real_
  actual_sha256 <- if (verified) verification$sha256 else NA_character_
  installed_at <- if (verified) {
    verification$receipt$installed_at
  } else {
    as.POSIXct(NA, tz = "UTC")
  }
  verified_at <- if (verified) {
    verification$verified_at
  } else {
    as.POSIXct(NA, tz = "UTC")
  }
  cache_origin <- if (!available) {
    "missing"
  } else if (!verified) {
    "invalid"
  } else {
    verification$receipt$source
  }
  tibble::tibble(
    lookup_id = spec$lookup_id,
    release = spec$release,
    tag_commit = spec$tag_commit,
    asset_id = spec$asset_id,
    available = available,
    verified = verified,
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    expected_bytes = as.double(spec$bytes),
    expected_sha256 = spec$sha256,
    bytes = actual_bytes,
    sha256 = actual_sha256,
    installed_at = installed_at,
    verified_at = verified_at,
    source_url = spec$source_url,
    license = spec$license,
    active_state = spec$active_state,
    currentness_policy = "not_checked",
    cache_origin = cache_origin
  )
}

gx_lookup_rename <- function(from, to) {
  file.rename(from, to)
}

gx_mainstem_lookup_swap <- function(stage_dir, final_dir) {
  parent <- dirname(final_dir)
  backup <- tempfile(pattern = ".backup-", tmpdir = parent)
  if ((file.exists(final_dir) || dir.exists(final_dir)) &&
      (!dir.exists(final_dir) || gx_path_is_symlink(final_dir))) {
    gx_lookup_abort(
      "The existing mainstem lookup path is not a regular owned directory.",
      "gx_error_crosswalk_lookup_ownership"
    )
  }
  had_existing <- dir.exists(final_dir)
  if (had_existing && !isTRUE(gx_lookup_rename(final_dir, backup))) {
    gx_lookup_abort(
      "The existing mainstem lookup could not be staged for replacement.",
      "gx_error_crosswalk_lookup_io"
    )
  }
  installed <- isTRUE(gx_lookup_rename(stage_dir, final_dir))
  if (!installed) {
    if (had_existing) {
      restored <- isTRUE(gx_lookup_rename(backup, final_dir))
      if (!restored) {
        recovery_path <- normalizePath(
          backup,
          winslash = "/",
          mustWork = FALSE
        )
        gx_lookup_abort(
          paste(
            "The mainstem lookup could not be installed or rolled back;",
            "the previous installation was retained at a recovery path."
          ),
          "gx_error_crosswalk_lookup_io",
          rollback_restored = FALSE,
          recovery_path = recovery_path
        )
      }
    }
    gx_lookup_abort(
      "The mainstem lookup could not be installed atomically.",
      "gx_error_crosswalk_lookup_io",
      rollback_restored = had_existing
    )
  }
  if (had_existing && dir.exists(backup)) {
    unlink(backup, recursive = TRUE, force = TRUE)
  }
  invisible(final_dir)
}

#' Explicitly install the pinned mainstem lookup
#'
#' Installs the optional `ref_rivers` NHDPlusV2 COMID lookup into a separate,
#' package-owned data directory. This is the only geoconnexr operation that
#' downloads the disclosed 120,422,425-byte v3.2 asset; crosswalk calls never
#' install or refresh it implicitly. `source = "file"` imports the same pinned
#' bytes from a local file and is suitable for air-gapped use.
#'
#' The transfer is streamed to disk with redirects disabled at the transport
#' layer and validated hop by hop. The exact byte count, SHA-256 digest, CSV
#' schema, row count, known answers, and provenance receipt must all verify
#' before an atomic replacement. Included mainstems were non-superseded when
#' v3.2 was generated; installation does not check current service state.
#'
#' @param source Either `"release"` for the pinned upstream release asset or
#'   `"file"` for a local import.
#' @param file Local CSV path required by `source = "file"`; otherwise `NULL`.
#' @param version Pinned lookup release. Currently only `"v3.2"` is
#'   registered.
#' @param force Replace an existing installation after the replacement has
#'   fully verified.
#' @param confirm Prompt before writing or downloading when `TRUE`.
#'   Non-interactive callers must pass `FALSE` explicitly if they set this
#'   argument to `TRUE` through shared configuration.
#' @param offline Whether network access is prohibited. Local imports remain
#'   available offline.
#' @param data_dir Persistent package data directory, separate from the HTTP
#'   cache.
#'
#' @return The verified one-row result from [gx_mainstem_lookup_info()].
#' @export
gx_mainstem_lookup_install <- function(
    source = c("release", "file"),
    file = NULL,
    version = "v3.2",
    force = FALSE,
    confirm = interactive(),
    offline = getOption("geoconnexr.offline", FALSE),
    data_dir = gx_default_data_dir()) {
  source <- tryCatch(
    match.arg(source),
    error = function(cnd) {
      gx_lookup_abort(
        "{.arg source} must be either 'release' or 'file'.",
        "gx_error_crosswalk_lookup_input"
      )
    }
  )
  force <- gx_lookup_flag(force, "force")
  confirm <- gx_lookup_flag(confirm, "confirm")
  offline <- gx_lookup_flag(offline, "offline")
  spec <- gx_mainstem_lookup_spec(version)
  data_dir <- gx_lookup_validate_data_dir(data_dir)
  if (identical(source, "release") && !is.null(file)) {
    gx_lookup_abort(
      "{.arg file} must be NULL when {.arg source = 'release'}.",
      "gx_error_crosswalk_lookup_input"
    )
  }
  if (identical(source, "file")) {
    file <- gx_lookup_string(file, "file")
    file <- path.expand(file)
    if (!file.exists(file) || dir.exists(file) || gx_path_is_symlink(file)) {
      gx_lookup_abort(
        "{.arg file} must identify a regular local file.",
        "gx_error_crosswalk_lookup_input"
      )
    }
  }
  current <- gx_mainstem_lookup_info(version, data_dir)
  if (isTRUE(current$verified[[1]]) && !force) return(current)
  if (isTRUE(current$available[[1]]) && !isTRUE(current$verified[[1]]) && !force) {
    gx_lookup_abort(
      "The existing lookup failed integrity checks; use {.arg force = TRUE} to replace it explicitly.",
      "gx_error_crosswalk_lookup_integrity"
    )
  }
  if (identical(source, "release") && offline) {
    gx_lookup_abort(
      "Offline mode cannot install a missing mainstem lookup from the release; use {.arg source = 'file'}.",
      "gx_error_crosswalk_lookup_offline"
    )
  }
  if (confirm) {
    if (!interactive()) {
      gx_lookup_abort(
        "Use {.arg confirm = FALSE} for non-interactive lookup installation.",
        "gx_error_crosswalk_lookup_input"
      )
    }
    question <- if (identical(source, "release")) {
      paste0(
        "Download and install the pinned ",
        format(spec$bytes, big.mark = ","),
        "-byte mainstem lookup?"
      )
    } else {
      "Import and install the pinned mainstem lookup?"
    }
    if (!isTRUE(utils::askYesNo(question))) return(current)
  }
  parent <- gx_mainstem_lookup_parent(data_dir, create = TRUE)
  final_dir <- gx_mainstem_lookup_install_dir(spec, data_dir)
  stage_dir <- tempfile(pattern = ".install-", tmpdir = parent)
  if (!dir.create(stage_dir, showWarnings = FALSE)) {
    gx_lookup_abort(
      "A staging directory for the mainstem lookup could not be created.",
      "gx_error_crosswalk_lookup_io"
    )
  }
  keep_stage <- FALSE
  on.exit(
    if (!keep_stage && dir.exists(stage_dir)) {
      unlink(stage_dir, recursive = TRUE, force = TRUE)
    },
    add = TRUE
  )
  stage_asset <- file.path(stage_dir, spec$asset_name)
  redirect_hosts <- character()
  receipt_source <- if (identical(source, "release")) {
    ledger <- gx_mainstem_lookup_download(stage_asset, spec)
    redirect_hosts <- unique(ledger$host)
    "release_download"
  } else {
    observed_size <- as.double(file.info(file)$size[[1]])
    if (!is.finite(observed_size) || observed_size != spec$bytes ||
        !file.copy(file, stage_asset, overwrite = FALSE, copy.mode = FALSE,
                   copy.date = FALSE)) {
      gx_lookup_abort(
        "The local lookup could not be copied or has the wrong byte count.",
        "gx_error_crosswalk_lookup_integrity"
      )
    }
    "local_import"
  }
  gx_mainstem_lookup_verify(
    stage_asset,
    spec,
    parse = TRUE,
    require_receipt = FALSE
  )
  gx_mainstem_lookup_write_receipt(
    file.path(stage_dir, "receipt-v1.json"),
    spec,
    receipt_source,
    redirect_hosts
  )
  gx_mainstem_lookup_read_receipt(
    file.path(stage_dir, "receipt-v1.json"),
    spec
  )
  gx_mainstem_lookup_swap(stage_dir, final_dir)
  keep_stage <- TRUE
  result <- gx_mainstem_lookup_info(version, data_dir)
  if (!isTRUE(result$verified[[1]])) {
    gx_lookup_abort(
      "The installed mainstem lookup failed final verification.",
      "gx_error_crosswalk_lookup_integrity"
    )
  }
  result
}

gx_mainstem_lookup_require <- function(version = "v3.2",
                                       data_dir = gx_default_data_dir()) {
  spec <- gx_mainstem_lookup_spec(version)
  path <- gx_mainstem_lookup_asset_path(spec, data_dir)
  if (!file.exists(path)) {
    gx_lookup_abort(
      "The pinned mainstem lookup is not installed; run {.fn gx_mainstem_lookup_install} explicitly.",
      "gx_error_crosswalk_lookup_missing"
    )
  }
  verification <- gx_mainstem_lookup_verify(
    path,
    spec,
    parse = FALSE,
    require_receipt = TRUE
  )
  list(spec = spec, verification = verification)
}
