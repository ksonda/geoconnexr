.gx_query_manifest_version <- 2L
.gx_query_manifest_contract_version <- "0.2.0"
.gx_query_manifest_max_bytes <- 1048576L
.gx_query_template_max_bytes <- 262144L

gx_query_asset_dir <- function() {
  path <- system.file("queries", package = "geoconnexr")
  if (!nzchar(path)) {
    gx_abort("Bundled query assets could not be located.", "gx_error_query_manifest")
  }
  path
}

gx_query_exact_mapping <- function(x, expected) {
  is.list(x) && !is.data.frame(x) && !is.null(names(x)) &&
    length(x) == length(expected) && !anyNA(names(x)) &&
    all(nzchar(names(x))) && !anyDuplicated(names(x)) &&
    setequal(names(x), expected)
}

gx_query_scalar_character <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x) &&
    isTRUE(stringi::stri_enc_isutf8(x))
}

gx_query_scalar_logical <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

gx_query_whole_number <- function(x, minimum = -.Machine$integer.max,
                                  maximum = .Machine$integer.max) {
  is.numeric(x) && !is.logical(x) && length(x) == 1L && !is.na(x) &&
    is.finite(x) && x == trunc(x) && x >= minimum && x <= maximum
}

gx_query_read_utf8_asset <- function(path, maximum_bytes, label,
                                     require_final_lf = TRUE) {
  if (!gx_query_scalar_character(path) || !file.exists(path)) {
    gx_abort(
      "The bundled {label} asset is missing.",
      "gx_error_query_manifest"
    )
  }

  link <- tryCatch(Sys.readlink(path), error = function(e) NA_character_)
  if (length(link) != 1L || (!is.na(link) && nzchar(link))) {
    gx_abort(
      "The bundled {label} asset must be a regular non-symlink file.",
      "gx_error_query_manifest"
    )
  }

  info <- tryCatch(file.info(path), error = function(e) NULL)
  if (is.null(info) || nrow(info) != 1L || isTRUE(info$isdir[[1]]) ||
      is.na(info$size[[1]]) || !gx_query_whole_number(
        info$size[[1]], 1L, maximum_bytes
      )) {
    gx_abort(
      "The bundled {label} asset has an invalid byte size.",
      "gx_error_query_manifest"
    )
  }

  size <- as.integer(info$size[[1]])
  bytes <- tryCatch(
    readBin(path, what = "raw", n = size),
    error = function(e) NULL
  )
  if (is.null(bytes) || length(bytes) != size) {
    gx_abort(
      "The bundled {label} asset could not be read completely.",
      "gx_error_query_manifest"
    )
  }

  byte_values <- as.integer(bytes)
  has_bom <- length(bytes) >= 3L && identical(
    bytes[seq_len(3L)], as.raw(c(0xef, 0xbb, 0xbf))
  )
  bad_ascii <- byte_values < 9L |
    (byte_values > 10L & byte_values < 32L) |
    byte_values == 127L
  if (has_bom || any(bad_ascii) ||
      (require_final_lf && !identical(bytes[[length(bytes)]], as.raw(0x0a)))) {
    gx_abort(
      "The bundled {label} asset must be BOM-free UTF-8 text with LF endings.",
      "gx_error_query_manifest"
    )
  }

  text <- tryCatch(rawToChar(bytes), error = function(e) NA_character_)
  if (!gx_query_scalar_character(text)) {
    gx_abort(
      "The bundled {label} asset is not valid UTF-8 text.",
      "gx_error_query_manifest"
    )
  }

  list(
    bytes = bytes,
    size = size,
    sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE),
    text = text
  )
}

gx_read_query_manifest <- function() {
  path <- file.path(gx_query_asset_dir(), "manifest.yml")
  asset <- gx_query_read_utf8_asset(
    path,
    maximum_bytes = .gx_query_manifest_max_bytes,
    label = "query manifest"
  )

  unsafe_yaml <- grepl(
    "(^|[[:space:]\\[\\{,])[*&!][A-Za-z0-9_-]*",
    asset$text,
    perl = TRUE
  ) || grepl("(^|[[:space:]])<<[[:space:]]*:", asset$text, perl = TRUE)
  if (unsafe_yaml) {
    gx_abort(
      "The bundled query manifest may not use YAML aliases, tags, or merges.",
      "gx_error_query_manifest"
    )
  }

  manifest <- tryCatch(
    yaml::yaml.load(
      asset$text,
      eval.expr = FALSE,
      merge.warning = TRUE,
      error.label = "bundled query manifest"
    ),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(manifest)) {
    gx_abort(
      "The bundled query manifest is not valid, unambiguous YAML.",
      "gx_error_query_manifest"
    )
  }
  gx_validate_query_manifest(manifest, dirname(path))
}

gx_query_validate_token_vector <- function(x, label, allow_empty = FALSE) {
  valid <- is.character(x) && !anyNA(x) &&
    (allow_empty || length(x) > 0L) &&
    all(nzchar(x)) && !anyDuplicated(x) &&
    all(stringi::stri_enc_isutf8(x)) &&
    all(grepl("^[A-Za-z_][A-Za-z0-9_]*$", x))
  if (!valid) {
    gx_abort(
      "Query manifest {label} must contain unique variable names.",
      "gx_error_query_manifest"
    )
  }
  unname(enc2utf8(x))
}

gx_query_validate_string_vector <- function(x, label, allowed = NULL) {
  valid <- is.character(x) && length(x) > 0L && !anyNA(x) &&
    all(nzchar(x)) && !anyDuplicated(x) &&
    all(stringi::stri_enc_isutf8(x))
  if (valid && !is.null(allowed)) {
    valid <- all(x %in% allowed)
  }
  if (!valid) {
    gx_abort(
      "Query manifest {label} contains invalid values.",
      "gx_error_query_manifest"
    )
  }
  unname(enc2utf8(x))
}

gx_query_validate_parameter <- function(spec, name, defaults) {
  if (!is.list(spec) || is.null(names(spec)) || anyDuplicated(names(spec)) ||
      !gx_query_scalar_character(spec$type %||% "")) {
    gx_abort(
      "Query parameter {.val {name}} has an invalid contract.",
      "gx_error_query_manifest"
    )
  }

  type <- spec$type
  expected <- switch(
    type,
    http_iri = c("type", "required", "maximum_bytes"),
    http_iri_list = c(
      "type", "required", "minimum_items", "maximum_items",
      "item_maximum_bytes", "encoded_maximum_bytes", "unique_items", "sort"
    ),
    integer = c("type", "required", "minimum", "maximum"),
    crs84_wkt_literal = c(
      "type", "required", "maximum_bytes", "geometry_types", "allow_empty"
    ),
    literal = c("type", "required", "maximum_bytes"),
    datetime = c("type", "required"),
    character()
  )
  if (!length(expected) || !gx_query_exact_mapping(spec, expected) ||
      !isTRUE(spec$required)) {
    gx_abort(
      "Query parameter {.val {name}} has unsupported or extra fields.",
      "gx_error_query_manifest"
    )
  }

  if (identical(type, "http_iri")) {
    if (!gx_query_whole_number(spec$maximum_bytes, 1L, 8192L)) {
      gx_abort(
        "HTTP IRI parameters must have a bounded byte ceiling.",
        "gx_error_query_manifest"
      )
    }
    spec$maximum_bytes <- as.integer(spec$maximum_bytes)
  } else if (identical(type, "http_iri_list")) {
    numeric_fields <- c(
      "minimum_items", "maximum_items", "item_maximum_bytes",
      "encoded_maximum_bytes"
    )
    valid_numbers <- all(vapply(
      spec[numeric_fields],
      gx_query_whole_number,
      logical(1),
      minimum = 1L,
      maximum = 65536L
    ))
    if (!valid_numbers || spec$minimum_items > spec$maximum_items ||
        spec$maximum_items > defaults$http_iri_list_max_items ||
        spec$item_maximum_bytes > 8192L ||
        spec$encoded_maximum_bytes > 65536L ||
        !isTRUE(spec$unique_items) || !identical(spec$sort, "bytewise")) {
      gx_abort(
        "HTTP IRI-list parameters have invalid cardinality or byte ceilings.",
        "gx_error_query_manifest"
      )
    }
    for (field in numeric_fields) {
      spec[[field]] <- as.integer(spec[[field]])
    }
  } else if (identical(type, "integer")) {
    if (!gx_query_whole_number(spec$minimum) ||
        !gx_query_whole_number(spec$maximum) ||
        spec$minimum > spec$maximum) {
      gx_abort(
        "Integer query parameters require ordered finite whole-number bounds.",
        "gx_error_query_manifest"
      )
    }
    spec$minimum <- as.integer(spec$minimum)
    spec$maximum <- as.integer(spec$maximum)
  } else if (identical(type, "crs84_wkt_literal")) {
    if (!gx_query_whole_number(spec$maximum_bytes, 1L, 131072L) ||
        !identical(spec$allow_empty, FALSE)) {
      gx_abort(
        "CRS84 WKT parameters require explicit geometry and byte bounds.",
        "gx_error_query_manifest"
      )
    }
    spec$geometry_types <- gx_query_validate_string_vector(
      spec$geometry_types,
      "geometry types",
      allowed = c("POLYGON", "MULTIPOLYGON")
    )
    if (!identical(spec$geometry_types, c("POLYGON", "MULTIPOLYGON"))) {
      gx_abort(
        "CRS84 WKT parameters must support polygon and multipolygon AOIs.",
        "gx_error_query_manifest"
      )
    }
    spec$maximum_bytes <- as.integer(spec$maximum_bytes)
  } else if (identical(type, "literal")) {
    if (!gx_query_whole_number(spec$maximum_bytes, 1L, 131072L)) {
      gx_abort(
        "Literal query parameters require a bounded byte ceiling.",
        "gx_error_query_manifest"
      )
    }
    spec$maximum_bytes <- as.integer(spec$maximum_bytes)
  }

  spec
}

gx_query_fixed_hits <- function(text, needle) {
  hits <- gregexpr(needle, text, fixed = TRUE)[[1]]
  if (identical(hits[[1]], -1L)) integer() else hits
}

gx_query_regex_hits <- function(text, pattern) {
  hits <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (identical(hits[[1]], -1L)) integer() else hits
}

gx_query_template_slots <- function(query) {
  canonical <- "\\{\\{[A-Za-z][A-Za-z0-9_]*\\}\\}"
  matches <- regmatches(query, gregexpr(canonical, query, perl = TRUE))[[1]]
  matches <- if (identical(matches, character(0))) character() else matches
  remainder <- gsub(canonical, "", query, perl = TRUE)
  if (grepl("{{", remainder, fixed = TRUE) ||
      grepl("}}", remainder, fixed = TRUE)) {
    gx_abort(
      "Query template contains malformed or unmatched doubled-brace syntax.",
      "gx_error_query_manifest"
    )
  }
  if (!length(matches)) {
    return(character())
  }
  unname(substring(matches, 3L, nchar(matches) - 2L))
}

gx_query_extract_projection <- function(query) {
  select_hits <- gx_query_regex_hits(query, "(?i)\\bSELECT\\b")
  if (length(select_hits) != 1L || !grepl(
    paste0(
      "(?is)\\A(?:[[:space:]]*PREFIX[[:space:]]+",
      "[A-Za-z][A-Za-z0-9_-]*:[[:space:]]*<[^>]+>[[:space:]]*)*SELECT\\b"
    ),
    query,
    perl = TRUE
  )) {
    gx_abort(
      "Query assets must contain exactly one reviewed top-level SELECT.",
      "gx_error_query_manifest"
    )
  }
  if (grepl(
    "(?i)\\b(INSERT|DELETE|LOAD|CLEAR|CREATE|DROP|COPY|MOVE|ADD|WITH)\\b",
    query,
    perl = TRUE
  )) {
    gx_abort(
      "Query assets may not contain SPARQL update keywords.",
      "gx_error_query_manifest"
    )
  }

  match <- regexec(
    "(?is)\\bSELECT\\b[[:space:]]+(?:DISTINCT[[:space:]]+|REDUCED[[:space:]]+)?(.*?)\\bWHERE\\b",
    query,
    perl = TRUE
  )
  parts <- regmatches(query, match)[[1]]
  if (length(parts) != 2L || grepl("*", parts[[2]], fixed = TRUE)) {
    gx_abort(
      "Query assets must use an explicit SELECT projection.",
      "gx_error_query_manifest"
    )
  }
  projection <- parts[[2]]
  if (grepl("[\"'<>#{}]", projection, perl = TRUE)) {
    gx_abort(
      "Query SELECT projections use unsupported expression syntax.",
      "gx_error_query_manifest"
    )
  }

  chars <- strsplit(projection, "", fixed = TRUE)[[1]]
  depth_before <- integer(length(chars))
  depth <- 0L
  for (i in seq_along(chars)) {
    depth_before[[i]] <- depth
    if (identical(chars[[i]], "(")) {
      depth <- depth + 1L
    } else if (identical(chars[[i]], ")")) {
      depth <- depth - 1L
      if (depth < 0L) {
        gx_abort(
          "Query SELECT projection parentheses are unbalanced.",
          "gx_error_query_manifest"
        )
      }
    }
  }
  if (depth != 0L) {
    gx_abort(
      "Query SELECT projection parentheses are unbalanced.",
      "gx_error_query_manifest"
    )
  }

  variable_pattern <- "\\?[A-Za-z_][A-Za-z0-9_]*"
  variable_hits <- gx_query_regex_hits(projection, variable_pattern)
  variable_matches <- if (length(variable_hits)) {
    regmatches(projection, gregexpr(variable_pattern, projection, perl = TRUE))[[1]]
  } else {
    character()
  }
  direct <- if (length(variable_hits)) {
    keep <- depth_before[variable_hits] == 0L
    data.frame(
      position = variable_hits[keep],
      variable = substring(variable_matches[keep], 2L),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(position = integer(), variable = character())
  }

  alias_pattern <- "(?i)\\bAS[[:space:]]+\\?[A-Za-z_][A-Za-z0-9_]*"
  alias_hits <- gx_query_regex_hits(projection, alias_pattern)
  aliases <- if (length(alias_hits)) {
    alias_matches <- regmatches(
      projection,
      gregexpr(alias_pattern, projection, perl = TRUE)
    )[[1]]
    data.frame(
      position = alias_hits,
      variable = sub("(?i)^.*\\?", "", alias_matches, perl = TRUE),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(position = integer(), variable = character())
  }

  projected <- rbind(direct, aliases)
  if (!nrow(projected)) {
    gx_abort(
      "Query SELECT projection contains no supported variables.",
      "gx_error_query_manifest"
    )
  }
  projected <- projected[order(projected$position), , drop = FALSE]
  variables <- unname(projected$variable)
  if (anyDuplicated(variables)) {
    gx_abort(
      "Query SELECT projection contains duplicate output variables.",
      "gx_error_query_manifest"
    )
  }
  variables
}

gx_query_extract_order <- function(query) {
  order_hits <- gx_query_regex_hits(query, "(?i)\\bORDER[[:space:]]+BY\\b")
  if (length(order_hits) != 1L) {
    gx_abort(
      "Query assets must contain exactly one ORDER BY clause.",
      "gx_error_query_manifest"
    )
  }
  match <- regexec(
    "(?is)\\bORDER[[:space:]]+BY\\b(.*?)\\bLIMIT\\b",
    query,
    perl = TRUE
  )
  parts <- regmatches(query, match)[[1]]
  if (length(parts) != 2L) {
    gx_abort(
      "Query ORDER BY metadata could not be inspected.",
      "gx_error_query_manifest"
    )
  }
  clause <- parts[[2]]
  pattern <- "\\?[A-Za-z_][A-Za-z0-9_]*"
  matches <- regmatches(clause, gregexpr(pattern, clause, perl = TRUE))[[1]]
  matches <- if (identical(matches, character(0))) character() else matches
  remainder <- trimws(gsub(pattern, "", clause, perl = TRUE))
  if (!length(matches) || nzchar(remainder)) {
    gx_abort(
      "Query ORDER BY clauses must be ascending bare-variable lists.",
      "gx_error_query_manifest"
    )
  }
  variables <- unname(substring(matches, 2L))
  if (anyDuplicated(variables)) {
    gx_abort(
      "Query ORDER BY clauses may not repeat variables.",
      "gx_error_query_manifest"
    )
  }
  variables
}

gx_query_validate_reviewed_text <- function(query, spec, name) {
  slots <- gx_query_template_slots(query)
  if (!setequal(unique(slots), names(spec$parameters))) {
    gx_abort(
      "Template {.val {name}} slots do not match its parameter contract.",
      "gx_error_query_manifest"
    )
  }
  if (length(gx_query_fixed_hits(query, "{{limit}}")) != 1L ||
      length(gx_query_fixed_hits(query, "{{offset}}")) != 1L ||
      !grepl(
        "(?is)\\bLIMIT[[:space:]]+\\{\\{limit\\}\\}[[:space:]]+OFFSET[[:space:]]+\\{\\{offset\\}\\}[[:space:]]*\\z",
        query,
        perl = TRUE
      )) {
    gx_abort(
      "Query assets require one terminal LIMIT/OFFSET slot pair.",
      "gx_error_query_manifest"
    )
  }

  projected <- gx_query_extract_projection(query)
  if (!identical(projected, spec$result_variables)) {
    gx_abort(
      "Template {.val {name}} result variables do not match its SELECT projection.",
      "gx_error_query_manifest"
    )
  }
  ordered <- gx_query_extract_order(query)
  if (!identical(ordered, spec$order$variables)) {
    gx_abort(
      "Template {.val {name}} order metadata does not match its ORDER BY clause.",
      "gx_error_query_manifest"
    )
  }

  if (identical(spec$result_key$uniqueness, "distinct_projection") &&
      !grepl("(?i)\\bSELECT[[:space:]]+DISTINCT\\b", query, perl = TRUE)) {
    gx_abort(
      "Distinct-projection result keys require SELECT DISTINCT.",
      "gx_error_query_manifest"
    )
  }
  if (identical(spec$result_key$uniqueness, "group_by") &&
      !grepl("(?i)\\bGROUP[[:space:]]+BY\\b", query, perl = TRUE)) {
    gx_abort(
      "Grouped result keys require an explicit GROUP BY clause.",
      "gx_error_query_manifest"
    )
  }
  invisible(query)
}

gx_query_validate_template <- function(spec, name, asset_dir, defaults) {
  required <- c(
    "file", "stored_bytes", "stored_sha256", "query_type", "parameters",
    "result_variables", "required_result_variables", "order", "result_key",
    "pagination", "row_budget"
  )
  if (!gx_query_exact_mapping(spec, required)) {
    gx_abort(
      "Template {.val {name}} has missing or unsupported fields.",
      "gx_error_query_manifest"
    )
  }
  if (!gx_query_scalar_character(spec$file) ||
      !grepl("^[A-Za-z0-9_-]+[.]rq$", spec$file) ||
      !identical(basename(spec$file), spec$file) ||
      !gx_query_whole_number(
        spec$stored_bytes, 1L, .gx_query_template_max_bytes
      ) ||
      !gx_query_scalar_character(spec$stored_sha256) ||
      !grepl("^[0-9a-f]{64}$", spec$stored_sha256) ||
      !identical(spec$query_type, "select") ||
      !gx_query_whole_number(spec$row_budget, 1L, .Machine$integer.max)) {
    gx_abort(
      "Template {.val {name}} has invalid identity, integrity, or budget metadata.",
      "gx_error_query_manifest"
    )
  }
  spec$stored_bytes <- as.integer(spec$stored_bytes)
  spec$row_budget <- as.integer(spec$row_budget)

  if (!is.list(spec$parameters) || !length(spec$parameters) ||
      is.null(names(spec$parameters)) || anyNA(names(spec$parameters)) ||
      any(!nzchar(names(spec$parameters))) || anyDuplicated(names(spec$parameters)) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", names(spec$parameters)))) {
    gx_abort(
      "Template {.val {name}} parameters must be a unique named mapping.",
      "gx_error_query_manifest"
    )
  }
  spec$parameters <- lapply(
    names(spec$parameters),
    function(parameter) gx_query_validate_parameter(
      spec$parameters[[parameter]], parameter, defaults
    )
  ) |> stats::setNames(names(spec$parameters))

  spec$result_variables <- gx_query_validate_token_vector(
    spec$result_variables,
    "result variables"
  )
  spec$required_result_variables <- gx_query_validate_token_vector(
    spec$required_result_variables,
    "required result variables"
  )
  if (!all(spec$required_result_variables %in% spec$result_variables)) {
    gx_abort(
      "Required result variables must be selected result variables.",
      "gx_error_query_manifest"
    )
  }

  order_fields <- c(
    "variables", "direction", "covers_result_variables", "total",
    "stable_across_requests"
  )
  if (!gx_query_exact_mapping(spec$order, order_fields) ||
      !identical(spec$order$direction, "ascending") ||
      !gx_query_scalar_logical(spec$order$covers_result_variables) ||
      !gx_query_scalar_logical(spec$order$total) ||
      !gx_query_scalar_logical(spec$order$stable_across_requests)) {
    gx_abort(
      "Template order metadata has an invalid contract.",
      "gx_error_query_manifest"
    )
  }
  spec$order$variables <- gx_query_validate_token_vector(
    spec$order$variables,
    "ORDER BY variables"
  )
  covers <- setequal(spec$order$variables, spec$result_variables)
  if (!all(spec$order$variables %in% spec$result_variables) ||
      !identical(spec$order$covers_result_variables, covers)) {
    gx_abort(
      "Template order metadata misstates its result-variable coverage.",
      "gx_error_query_manifest"
    )
  }

  key_fields <- c(
    "variables", "uniqueness", "scope", "stable_across_requests"
  )
  if (!gx_query_exact_mapping(spec$result_key, key_fields) ||
      !spec$result_key$uniqueness %in% c("distinct_projection", "group_by") ||
      !spec$result_key$scope %in% c("result_document", "query_binding") ||
      !gx_query_scalar_logical(spec$result_key$stable_across_requests)) {
    gx_abort(
      "Template result-key metadata has an invalid contract.",
      "gx_error_query_manifest"
    )
  }
  spec$result_key$variables <- gx_query_validate_token_vector(
    spec$result_key$variables,
    "result-key variables"
  )
  if (!all(spec$result_key$variables %in% spec$result_variables) ||
      !all(spec$result_key$variables %in% spec$order$variables) ||
      (identical(spec$result_key$uniqueness, "distinct_projection") &&
       !setequal(spec$result_key$variables, spec$result_variables))) {
    gx_abort(
      "Template result-key variables do not match their uniqueness contract.",
      "gx_error_query_manifest"
    )
  }

  pagination_fields <- c("enabled", "candidate_strategy", "blockers")
  blockers <- c(
    "rdf_term_order_is_not_total", "blank_nodes_are_document_scoped",
    "graph_has_no_snapshot_contract", "single_aggregate_result"
  )
  if (!gx_query_exact_mapping(spec$pagination, pagination_fields) ||
      !identical(spec$pagination$enabled, FALSE) ||
      !spec$pagination$candidate_strategy %in% c("offset", "none")) {
    gx_abort(
      "Template pagination metadata must remain explicitly disabled.",
      "gx_error_query_manifest"
    )
  }
  spec$pagination$blockers <- gx_query_validate_string_vector(
    spec$pagination$blockers,
    "pagination blockers",
    allowed = blockers
  )
  if (!"graph_has_no_snapshot_contract" %in% spec$pagination$blockers ||
      (!isTRUE(spec$order$total) &&
       !"rdf_term_order_is_not_total" %in% spec$pagination$blockers)) {
    gx_abort(
      "Template pagination blockers do not reflect its ordering contract.",
      "gx_error_query_manifest"
    )
  }

  offset_blockers <- c(
    "rdf_term_order_is_not_total", "blank_nodes_are_document_scoped",
    "graph_has_no_snapshot_contract"
  )
  aggregate_blockers <- c(
    "single_aggregate_result", "graph_has_no_snapshot_contract"
  )
  if (identical(spec$pagination$candidate_strategy, "offset")) {
    safe_facts <- identical(spec$order$total, FALSE) &&
      identical(spec$order$stable_across_requests, FALSE) &&
      identical(spec$result_key$uniqueness, "distinct_projection") &&
      identical(spec$result_key$scope, "result_document") &&
      identical(spec$result_key$stable_across_requests, FALSE) &&
      setequal(spec$pagination$blockers, offset_blockers)
  } else {
    safe_facts <- identical(spec$order$total, TRUE) &&
      identical(spec$order$stable_across_requests, TRUE) &&
      identical(spec$result_key$uniqueness, "group_by") &&
      identical(spec$result_key$scope, "query_binding") &&
      identical(spec$result_key$stable_across_requests, TRUE) &&
      setequal(spec$pagination$blockers, aggregate_blockers) &&
      identical(spec$row_budget, 1L)
  }
  if (!safe_facts) {
    gx_abort(
      "Template ordering, key, and pagination safety facts are inconsistent.",
      "gx_error_query_manifest"
    )
  }

  if (!all(c("limit", "offset") %in% names(spec$parameters)) ||
      !identical(spec$parameters$limit$type, "integer") ||
      !identical(spec$parameters$offset$type, "integer") ||
      spec$parameters$limit$minimum < 1L ||
      spec$parameters$limit$maximum > defaults$max_page_size ||
      spec$parameters$limit$maximum > spec$row_budget ||
      spec$parameters$offset$minimum < 0L ||
      spec$parameters$offset$maximum >= spec$row_budget) {
    gx_abort(
      "Template slice parameters do not satisfy their finite row budget.",
      "gx_error_query_manifest"
    )
  }
  if (identical(spec$pagination$candidate_strategy, "none") &&
      (!identical(spec$parameters$limit$minimum, 1L) ||
       !identical(spec$parameters$limit$maximum, 1L) ||
       !identical(spec$parameters$offset$minimum, 0L) ||
       !identical(spec$parameters$offset$maximum, 0L))) {
    gx_abort(
      "Non-pageable aggregate templates require a fixed one-row slice.",
      "gx_error_query_manifest"
    )
  }
  iri_lists <- Filter(
    function(x) identical(x$type, "http_iri_list"),
    spec$parameters
  )
  if (length(iri_lists) && any(vapply(
    iri_lists,
    function(x) x$maximum_items > defaults$http_iri_list_max_items,
    logical(1)
  ))) {
    gx_abort(
      "Template IRI-list parameters exceed the manifest default ceiling.",
      "gx_error_query_manifest"
    )
  }

  asset <- gx_query_read_utf8_asset(
    file.path(asset_dir, spec$file),
    maximum_bytes = .gx_query_template_max_bytes,
    label = paste("query template", name)
  )
  if (!identical(asset$size, spec$stored_bytes) ||
      !identical(asset$sha256, spec$stored_sha256)) {
    gx_abort(
      "Template {.val {name}} does not match its stored byte contract.",
      "gx_error_query_manifest"
    )
  }
  gx_query_validate_reviewed_text(asset$text, spec, name)
  spec
}

gx_validate_query_manifest <- function(manifest, asset_dir) {
  root_fields <- c(
    "version", "contract_version", "runtime", "integrity", "endpoint_policy",
    "defaults", "templates"
  )
  if (!gx_query_exact_mapping(manifest, root_fields) ||
      !identical(manifest$version, .gx_query_manifest_version) ||
      !identical(
        manifest$contract_version,
        .gx_query_manifest_contract_version
      )) {
    gx_abort(
      "Query manifest has an unsupported root contract.",
      "gx_error_query_manifest"
    )
  }

  runtime_fields <- c(
    "render_enabled", "execution_enabled", "pagination_enabled",
    "chunking_enabled", "gate"
  )
  if (!gx_query_exact_mapping(manifest$runtime, runtime_fields) ||
      !identical(manifest$runtime$render_enabled, TRUE) ||
      !identical(manifest$runtime$execution_enabled, FALSE) ||
      !identical(manifest$runtime$pagination_enabled, FALSE) ||
      !identical(manifest$runtime$chunking_enabled, FALSE) ||
      !identical(manifest$runtime$gate, "ADR-0004")) {
    gx_abort(
      "Query manifest runtime capabilities must remain render-only.",
      "gx_error_query_manifest"
    )
  }
  if (!gx_query_exact_mapping(manifest$integrity, c("algorithm", "scope")) ||
      !identical(manifest$integrity$algorithm, "sha256") ||
      !identical(manifest$integrity$scope, "exact_stored_bytes")) {
    gx_abort(
      "Query manifest integrity metadata is unsupported.",
      "gx_error_query_manifest"
    )
  }

  policy_fields <- c(
    "method", "content_type", "accept", "follow_redirects",
    "reject_content_types"
  )
  policy <- manifest$endpoint_policy
  if (!gx_query_exact_mapping(policy, policy_fields) ||
      !identical(policy$method, "POST") ||
      !identical(policy$content_type, "application/sparql-query") ||
      !identical(policy$accept, "application/sparql-results+json") ||
      !identical(policy$follow_redirects, FALSE) ||
      !identical(unname(policy$reject_content_types), "text/html")) {
    gx_abort(
      "Query manifest endpoint policy may not weaken package HTTP policy.",
      "gx_error_query_manifest"
    )
  }

  default_fields <- c(
    "page_size", "max_page_size", "http_iri_list_max_items"
  )
  defaults <- manifest$defaults
  if (!gx_query_exact_mapping(defaults, default_fields) ||
      !gx_query_whole_number(defaults$page_size, 1L, 1000L) ||
      !gx_query_whole_number(defaults$max_page_size, 1L, 1000L) ||
      !gx_query_whole_number(defaults$http_iri_list_max_items, 1L, 200L) ||
      defaults$page_size > defaults$max_page_size) {
    gx_abort(
      "Query manifest defaults require ordered finite ceilings.",
      "gx_error_query_manifest"
    )
  }
  for (field in default_fields) {
    defaults[[field]] <- as.integer(defaults[[field]])
  }
  manifest$defaults <- defaults

  if (!gx_query_scalar_character(asset_dir) || !dir.exists(asset_dir) ||
      !is.list(manifest$templates) || !length(manifest$templates) ||
      is.null(names(manifest$templates)) || anyNA(names(manifest$templates)) ||
      any(!grepl("^[a-z][a-z0-9_]*$", names(manifest$templates))) ||
      anyDuplicated(names(manifest$templates))) {
    gx_abort(
      "Query manifest must contain uniquely named templates and an asset directory.",
      "gx_error_query_manifest"
    )
  }

  manifest$templates <- lapply(
    names(manifest$templates),
    function(name) gx_query_validate_template(
      manifest$templates[[name]], name, asset_dir, defaults
    )
  ) |> stats::setNames(names(manifest$templates))

  files <- vapply(manifest$templates, `[[`, character(1), "file")
  shipped <- tryCatch(
    list.files(asset_dir, pattern = "[.]rq$", full.names = FALSE),
    error = function(e) character()
  )
  if (anyDuplicated(files) || !setequal(files, shipped)) {
    gx_abort(
      "Every shipped .rq asset must be declared exactly once.",
      "gx_error_query_manifest"
    )
  }
  manifest
}

#' List bundled, typed SPARQL templates
#'
#' Returns the render-only named-query manifest. The `order`, `result_key`, and
#' `pagination` list columns distinguish reviewed query facts from unproven
#' cross-request stability. `pagination[[i]]$enabled` is currently always
#' `FALSE`; listing a candidate strategy does not authorize paging.
#'
#' @return A tibble with one row per bundled template. It includes exact source
#'   byte/hash pins, ordered result-variable contracts, render-only runtime
#'   flags, and list columns for parameters, ordering, result keys, and blocked
#'   pagination metadata.
#' @export
gx_templates <- function() {
  manifest <- gx_read_query_manifest()
  specs <- manifest$templates
  count <- length(specs)
  tibble::tibble(
    contract_version = rep(manifest$contract_version, count),
    render_enabled = rep(manifest$runtime$render_enabled, count),
    execution_enabled = rep(manifest$runtime$execution_enabled, count),
    pagination_enabled = rep(manifest$runtime$pagination_enabled, count),
    chunking_enabled = rep(manifest$runtime$chunking_enabled, count),
    gate = rep(manifest$runtime$gate, count),
    name = names(specs),
    file = vapply(specs, `[[`, character(1), "file"),
    stored_bytes = as.integer(vapply(specs, `[[`, numeric(1), "stored_bytes")),
    stored_sha256 = vapply(specs, `[[`, character(1), "stored_sha256"),
    query_type = vapply(specs, `[[`, character(1), "query_type"),
    parameters = unname(lapply(specs, `[[`, "parameters")),
    result_variables = unname(lapply(specs, `[[`, "result_variables")),
    required_result_variables = unname(lapply(
      specs, `[[`, "required_result_variables"
    )),
    order = unname(lapply(specs, `[[`, "order")),
    result_key = unname(lapply(specs, `[[`, "result_key")),
    pagination = unname(lapply(specs, `[[`, "pagination")),
    row_budget = as.integer(vapply(specs, `[[`, numeric(1), "row_budget"))
  )
}

#' Render a bundled SPARQL template safely
#'
#' Parameters are encoded according to the exact-byte-pinned bundled manifest.
#' Raw string interpolation is deliberately unsupported. Rendering is local:
#' this function does not execute, paginate, or chunk a query.
#'
#' @param template Name returned by [gx_templates()].
#' @param params A named list containing exactly the template parameters.
#'
#' @return A length-one UTF-8 SPARQL query retaining the template's final LF.
#' @export
gx_render_query <- function(template, params) {
  if (!gx_query_scalar_character(template)) {
    gx_abort("{.arg template} must be one template name.", "gx_error_query")
  }
  if (!is.list(params) || is.null(names(params)) || anyNA(names(params)) ||
      any(!nzchar(names(params)))) {
    gx_abort(
      "{.arg params} must be a fully named list.",
      "gx_error_query_parameter"
    )
  }

  manifest <- gx_read_query_manifest()
  spec <- manifest$templates[[template]]
  if (is.null(spec)) {
    gx_abort("Unknown query template {.val {template}}.", "gx_error_query")
  }
  required <- names(spec$parameters)
  if (!setequal(names(params), required) || anyDuplicated(names(params))) {
    gx_abort(
      "Query parameters must match the template contract exactly.",
      "gx_error_query_parameter"
    )
  }

  if (all(c("limit", "offset") %in% names(params))) {
    limit <- params$limit
    offset <- params$offset
    gx_encode_integer(limit, spec$parameters$limit)
    gx_encode_integer(offset, spec$parameters$offset)
    requested_end <- as.double(limit) + as.double(offset)
    if (!is.finite(requested_end) || requested_end > as.double(spec$row_budget)) {
      gx_abort(
        "Requested slice exceeds the template row budget of {spec$row_budget}.",
        "gx_error_query_parameter"
      )
    }
  }

  asset <- gx_query_read_utf8_asset(
    file.path(gx_query_asset_dir(), spec$file),
    maximum_bytes = .gx_query_template_max_bytes,
    label = paste("query template", template)
  )
  if (!identical(asset$size, spec$stored_bytes) ||
      !identical(asset$sha256, spec$stored_sha256)) {
    gx_abort(
      "Query template bytes changed after manifest validation.",
      "gx_error_query_manifest"
    )
  }

  query <- asset$text
  for (name in required) {
    encoded <- gx_encode_parameter(params[[name]], spec$parameters[[name]])
    query <- stringi::stri_replace_all_fixed(
      query,
      paste0("{{", name, "}}"),
      encoded
    )
  }
  if (grepl("\\{\\{[A-Za-z][A-Za-z0-9_]*\\}\\}", query, perl = TRUE)) {
    gx_abort("Rendered query contains unresolved slots.", "gx_error_query")
  }
  if (!isTRUE(stringi::stri_enc_isutf8(query)) ||
      nchar(enc2utf8(query), type = "bytes") > .gx_query_template_max_bytes) {
    gx_abort("Rendered queries may not exceed 256 KiB.", "gx_error_query")
  }
  enc2utf8(query)
}
