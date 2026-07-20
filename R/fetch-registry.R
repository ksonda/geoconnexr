.gx_handler_registry_contract_version <- "0.1.0"
.gx_handler_registry_version <- 1L
.gx_handler_registry_max_asset_bytes <- 1048576L
.gx_handler_registry_max_handlers <- 64L
.gx_handler_registry_max_scalar_bytes <- 16384L
.gx_handler_registry_max_text_bytes <- 2097152L

.gx_handler_registry_allowed_facts <- c(
  "access_url", "media_type", "conforms_to"
)
.gx_handler_registry_allowed_packages <- c(
  "dataRetrieval", "edr4r", "readr"
)
.gx_handler_registry_protocol <- c("probe", "plan", "fetch", "normalize")
.gx_handler_registry_fields <- c(
  "contract_version", "registry_version", "evaluation", "protocol",
  "allowed_fact_names", "portable_sha256", "implementations_sha256",
  "handlers"
)
.gx_handler_registry_handler_columns <- c(
  "id", "precedence", "lifecycle", "outcome", "classifier",
  "implementation_id", "availability", "package", "minimum_version",
  "missing_package_status", "planning_metadata", "payload_class"
)
.gx_handler_registry_planning_fields <- c(
  "functions", "function", "query_semantics", "lifecycle", "warning",
  "plan_must_record"
)

gx_handler_registry_abort <- function(
    message,
    class = "gx_error_fetch_plan_registry_contract") {
  gx_abort(
    message,
    class = unique(c(class, "gx_error_handler_registry", "gx_error_fetch_plan")),
    call = NULL,
    .redact_trace = TRUE
  )
}

gx_handler_registry_asset_dir <- function() {
  path <- system.file("handlers", package = "geoconnexr")
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !dir.exists(path) || gx_path_is_symlink(path)) {
    gx_handler_registry_abort(
      "Bundled handler registry assets could not be located safely.",
      "gx_error_fetch_plan_registry_asset"
    )
  }
  path
}

gx_handler_registry_exact_mapping <- function(x, expected) {
  is.list(x) && !is.object(x) && !is.null(names(x)) &&
    identical(names(x), expected)
}

gx_handler_registry_scalar_text <- function(
    x, allow_empty = FALSE, maximum_bytes = .gx_handler_registry_max_scalar_bytes) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      (!allow_empty && !nzchar(x))) {
    return(FALSE)
  }
  valid_utf8 <- tryCatch(
    isTRUE(stringi::stri_enc_isutf8(x)),
    error = function(cnd) FALSE,
    warning = function(cnd) FALSE
  )
  bytes <- suppressWarnings(tryCatch(
    nchar(enc2utf8(x), type = "bytes", allowNA = TRUE),
    error = function(cnd) NA_integer_
  ))
  controls <- tryCatch(
    isTRUE(stringi::stri_detect_regex(x, "[\\p{Cc}\\p{Cf}\\p{Cs}]")),
    error = function(cnd) TRUE,
    warning = function(cnd) TRUE
  )
  valid_utf8 && length(bytes) == 1L && !is.na(bytes) &&
    bytes <= maximum_bytes && !controls
}

gx_handler_registry_text_total <- function(x, limit) {
  total <- 0
  pending <- list(x)
  while (length(pending)) {
    value <- pending[[length(pending)]]
    pending[[length(pending)]] <- NULL
    if (is.character(value)) {
      for (item in value) {
        if (is.na(item)) next
        bytes <- suppressWarnings(tryCatch(
          nchar(enc2utf8(item), type = "bytes", allowNA = TRUE),
          error = function(cnd) NA_integer_
        ))
        if (length(bytes) != 1L || is.na(bytes) || !is.finite(bytes) ||
            bytes > limit - total) {
          return(Inf)
        }
        total <- total + as.double(bytes)
      }
    } else if (is.list(value)) {
      pending <- c(pending, unname(value))
    }
  }
  total
}

gx_handler_registry_assert_text_budget <- function(x) {
  total <- gx_handler_registry_text_total(
    x, .gx_handler_registry_max_text_bytes
  )
  if (!is.finite(total) || total > .gx_handler_registry_max_text_bytes) {
    gx_handler_registry_abort(
      "Handler registry text exceeds its fixed aggregate byte budget.",
      "gx_error_fetch_plan_registry_budget"
    )
  }
  invisible(total)
}

gx_handler_registry_whole_number <- function(
    x, minimum = 0L, maximum = .Machine$integer.max) {
  is.numeric(x) && !is.logical(x) && length(x) == 1L && !is.na(x) &&
    is.finite(x) && x == trunc(x) && x >= minimum && x <= maximum
}

gx_handler_registry_string_array <- function(
    x, label, maximum_length = 256L, allow_empty = FALSE) {
  if (is.character(x)) {
    value <- unname(x)
  } else if (is.list(x) && !is.object(x) && is.null(names(x)) &&
             all(vapply(x, function(item) {
               is.character(item) && length(item) == 1L && !is.na(item)
             }, logical(1)))) {
    value <- unname(vapply(x, identity, character(1)))
  } else {
    gx_handler_registry_abort(
      "A handler registry string array has an invalid representation."
    )
  }
  if ((!allow_empty && !length(value)) || length(value) > maximum_length ||
      anyNA(value) || anyDuplicated(value) ||
      any(!vapply(value, gx_handler_registry_scalar_text, logical(1)))) {
    gx_handler_registry_abort(
      "A handler registry string array violates its bounded value contract."
    )
  }
  value
}

gx_handler_registry_read_asset <- function(path, label) {
  if (!gx_handler_registry_scalar_text(path) || !file.exists(path) ||
      gx_path_is_symlink(path)) {
    gx_handler_registry_abort(
      "A bundled handler registry asset is missing or unsafe.",
      "gx_error_fetch_plan_registry_asset"
    )
  }
  info <- tryCatch(
    withCallingHandlers(
      fs::file_info(path, follow = FALSE),
      warning = function(cnd) invokeRestart("muffleWarning")
    ),
    error = function(cnd) NULL
  )
  size <- if (!is.null(info) && nrow(info) == 1L) {
    suppressWarnings(as.double(info$size[[1L]]))
  } else {
    NA_real_
  }
  regular <- !is.null(info) && nrow(info) == 1L &&
    identical(as.character(info$type[[1L]]), "file")
  if (!regular || !gx_handler_registry_whole_number(
    size, minimum = 1L, maximum = .gx_handler_registry_max_asset_bytes
  )) {
    gx_handler_registry_abort(
      "A bundled handler registry asset must be a bounded regular file.",
      "gx_error_fetch_plan_registry_asset"
    )
  }

  bytes <- tryCatch(
    withCallingHandlers(
      readBin(path, what = "raw", n = as.integer(size) + 1L),
      warning = function(cnd) invokeRestart("muffleWarning")
    ),
    error = function(cnd) NULL
  )
  if (is.null(bytes) || !is.raw(bytes) || length(bytes) != as.integer(size)) {
    gx_handler_registry_abort(
      "A bundled handler registry asset could not be read completely.",
      "gx_error_fetch_plan_registry_asset"
    )
  }

  values <- as.integer(bytes)
  has_bom <- length(bytes) >= 3L && identical(
    bytes[seq_len(3L)], as.raw(c(0xef, 0xbb, 0xbf))
  )
  invalid_control <- values < 9L | (values > 10L & values < 32L) |
    values == 127L
  if (has_bom || any(invalid_control) || any(values == 13L) ||
      !identical(bytes[[length(bytes)]], as.raw(0x0a))) {
    gx_handler_registry_abort(
      "Bundled handler registry assets must be BOM-free UTF-8 with LF endings.",
      "gx_error_fetch_plan_registry_asset"
    )
  }
  text <- tryCatch(rawToChar(bytes), error = function(cnd) NA_character_)
  exact_utf8 <- is.character(text) && length(text) == 1L && !is.na(text) &&
    tryCatch(isTRUE(stringi::stri_enc_isutf8(text)), error = function(cnd) FALSE) &&
    tryCatch(
      identical(charToRaw(enc2utf8(text)), bytes),
      error = function(cnd) FALSE
    )
  if (!exact_utf8) {
    gx_handler_registry_abort(
      "A bundled handler registry asset is not exact UTF-8 text.",
      "gx_error_fetch_plan_registry_asset"
    )
  }
  list(
    bytes = bytes,
    text = text,
    sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE)
  )
}

gx_handler_registry_parse_yaml <- function(asset) {
  unsafe <- grepl(
    "(^|[[:space:]\\[\\{,])[*&!][A-Za-z0-9_-]*",
    asset$text,
    perl = TRUE
  ) || grepl("(^|[[:space:]])<<[[:space:]]*:", asset$text, perl = TRUE)
  if (unsafe) {
    gx_handler_registry_abort(
      "The portable handler registry may not use YAML aliases, tags, or merges.",
      "gx_error_fetch_plan_registry_parse"
    )
  }
  parsed <- tryCatch(
    yaml::yaml.load(
      asset$text,
      eval.expr = FALSE,
      merge.warning = TRUE,
      error.label = "bundled handler registry"
    ),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(parsed)) {
    gx_handler_registry_abort(
      "The portable handler registry is not valid unambiguous YAML.",
      "gx_error_fetch_plan_registry_parse"
    )
  }
  parsed
}

gx_handler_registry_parse_json <- function(asset) {
  parsed <- tryCatch(
    jsonlite::fromJSON(asset$text, simplifyVector = FALSE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(parsed)) {
    gx_handler_registry_abort(
      "The R handler implementation registry is not valid strict JSON.",
      "gx_error_fetch_plan_registry_parse"
    )
  }
  parsed
}

gx_handler_registry_validate_regex <- function(value) {
  if (!gx_handler_registry_scalar_text(value, maximum_bytes = 4096L)) {
    gx_handler_registry_abort("A handler classifier regex is invalid.")
  }
  valid <- tryCatch(
    {
      grepl(value, "", perl = TRUE)
      TRUE
    },
    error = function(cnd) FALSE,
    warning = function(cnd) FALSE
  )
  if (!valid) {
    gx_handler_registry_abort("A handler classifier regex does not compile.")
  }
  value
}

gx_handler_registry_validate_predicate <- function(x, allowed_facts) {
  if (!is.list(x) || is.object(x) || is.null(names(x)) ||
      !names(x)[[1L]] %in% "fact" || length(x) != 3L ||
      !identical(names(x)[[2L]], "operator") ||
      !identical(names(x)[[3L]], if (identical(x$operator, "regex")) {
        "value"
      } else {
        "values"
      }) || !gx_handler_registry_scalar_text(x$fact) ||
      !x$fact %in% allowed_facts || !gx_handler_registry_scalar_text(x$operator)) {
    gx_handler_registry_abort("A handler classifier predicate has an invalid exact shape.")
  }
  if (identical(x$operator, "regex")) {
    gx_handler_registry_validate_regex(x$value)
  } else {
    if (!x$operator %in% c("scheme_in", "contains_any", "equals_any")) {
      gx_handler_registry_abort("A handler classifier operator is unsupported.")
    }
    values <- gx_handler_registry_string_array(x$values, "predicate values")
    if (identical(x$operator, "scheme_in") &&
        any(!grepl("^[A-Za-z][A-Za-z0-9+.-]*$", values))) {
      gx_handler_registry_abort("A scheme classifier contains an invalid scheme token.")
    }
  }
  compatible <- switch(
    x$operator,
    scheme_in = identical(x$fact, "access_url"),
    regex = identical(x$fact, "access_url"),
    contains_any = identical(x$fact, "conforms_to"),
    equals_any = identical(x$fact, "media_type"),
    FALSE
  )
  if (!compatible) {
    gx_handler_registry_abort(
      "A handler classifier operator is incompatible with its fact."
    )
  }
  invisible(x)
}

gx_handler_registry_validate_classifier <- function(x, allowed_facts) {
  if (!is.list(x) || is.object(x) || !length(x) || is.null(names(x)) ||
      anyNA(names(x)) || anyDuplicated(names(x))) {
    gx_handler_registry_abort("A handler classifier has an invalid mapping.")
  }
  if (identical(names(x), "always")) {
    if (!isTRUE(x$always)) {
      gx_handler_registry_abort("An always classifier must contain exactly true.")
    }
    return(invisible(x))
  }
  expected <- c("all", "any")
  if (!identical(names(x), expected[expected %in% names(x)])) {
    gx_handler_registry_abort("A handler classifier supports only canonical all/any groups.")
  }
  for (group in unname(x)) {
    if (!is.list(group) || is.object(group) || !length(group) ||
        !is.null(names(group)) || length(group) > 64L) {
      gx_handler_registry_abort("A handler classifier group violates its row budget.")
    }
    invisible(lapply(
      group, gx_handler_registry_validate_predicate, allowed_facts = allowed_facts
    ))
  }
  invisible(x)
}

gx_handler_registry_validate_portable_handler <- function(x, allowed_facts) {
  allowed <- c("id", "precedence", "lifecycle", "outcome", "classifier")
  if (!is.list(x) || is.object(x) || is.null(names(x)) ||
      !all(c("id", "precedence", "classifier") %in% names(x)) ||
      !identical(names(x), allowed[allowed %in% names(x)]) ||
      !gx_handler_registry_scalar_text(x$id) ||
      !grepl("^[a-z][a-z0-9_]*$", x$id) ||
      !gx_handler_registry_whole_number(x$precedence)) {
    gx_handler_registry_abort("A portable handler entry has an invalid exact shape.")
  }
  lifecycle <- x$lifecycle %||% "active"
  outcome <- x$outcome %||% "fetch"
  if (!gx_handler_registry_scalar_text(lifecycle) ||
      !lifecycle %in% c("active", "deprecated") ||
      !gx_handler_registry_scalar_text(outcome) ||
      !outcome %in% c("fetch", "reference_only") ||
      ("outcome" %in% names(x) && !identical(outcome, "reference_only"))) {
    gx_handler_registry_abort("A portable handler lifecycle or outcome is invalid.")
  }
  gx_handler_registry_validate_classifier(x$classifier, allowed_facts)
  list(
    id = x$id,
    precedence = as.integer(x$precedence),
    lifecycle = lifecycle,
    outcome = outcome,
    classifier = x$classifier
  )
}

gx_handler_registry_validate_portable <- function(x) {
  expected <- c(
    "version", "contract_version", "evaluation", "allowed_fact_names",
    "handlers"
  )
  if (!gx_handler_registry_exact_mapping(x, expected) ||
      !identical(x$version, .gx_handler_registry_version) ||
      !identical(x$contract_version, .gx_handler_registry_contract_version) ||
      !identical(x$evaluation, "first_match_wins")) {
    gx_handler_registry_abort("The portable handler registry root is invalid.")
  }
  allowed_facts <- gx_handler_registry_string_array(
    x$allowed_fact_names, "allowed fact names", maximum_length = 16L
  )
  if (!identical(allowed_facts, .gx_handler_registry_allowed_facts) ||
      !is.list(x$handlers) || is.object(x$handlers) ||
      !is.null(names(x$handlers)) || !length(x$handlers) ||
      length(x$handlers) > .gx_handler_registry_max_handlers) {
    gx_handler_registry_abort("The portable handler registry facts or rows are invalid.")
  }
  handlers <- lapply(
    x$handlers,
    gx_handler_registry_validate_portable_handler,
    allowed_facts = allowed_facts
  )
  ids <- vapply(handlers, `[[`, character(1), "id")
  precedence <- vapply(handlers, `[[`, integer(1), "precedence")
  outcomes <- vapply(handlers, `[[`, character(1), "outcome")
  fallbacks <- vapply(
    handlers,
    function(handler) identical(names(handler$classifier), "always"),
    logical(1)
  )
  final <- length(handlers)
  if (anyDuplicated(ids) || anyDuplicated(precedence) ||
      (length(precedence) > 1L && any(diff(precedence) <= 0L)) ||
      !identical(ids[[final]], "unknown") || !fallbacks[[final]] ||
      any(fallbacks[-final]) || !identical(outcomes[[final]], "reference_only") ||
      any(outcomes[-final] != "fetch")) {
    gx_handler_registry_abort(
      "Handler IDs, precedence, outcomes, or the final fallback are invalid.",
      "gx_error_fetch_plan_registry_mismatch"
    )
  }
  list(
    registry_version = .gx_handler_registry_version,
    contract_version = x$contract_version,
    evaluation = x$evaluation,
    allowed_fact_names = allowed_facts,
    handlers = handlers
  )
}

gx_handler_registry_validate_named_functions <- function(x) {
  if (!is.list(x) || is.object(x) || !length(x) || is.null(names(x)) ||
      anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x)) ||
      length(x) > 64L) {
    gx_handler_registry_abort("An implementation function mapping is invalid.")
  }
  valid <- vapply(x, function(value) {
    gx_handler_registry_scalar_text(value) &&
      grepl("^[A-Za-z.][A-Za-z0-9._]*$", value)
  }, logical(1))
  if (any(!valid) || any(!grepl("^[a-z][a-z0-9_]*$", names(x)))) {
    gx_handler_registry_abort("An implementation function mapping is invalid.")
  }
  x
}

gx_handler_registry_validate_implementation <- function(x) {
  allowed <- c(
    "implementation_id", "availability", "package", "minimum_version",
    "functions", "function", "query_semantics", "lifecycle", "warning",
    "plan_must_record", "missing_package_status", "payload_class", "outcome"
  )
  if (!is.list(x) || is.object(x) || is.null(names(x)) ||
      !all(c("implementation_id", "availability", "package") %in% names(x)) ||
      !identical(names(x), allowed[allowed %in% names(x)]) ||
      !gx_handler_registry_scalar_text(x$implementation_id) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._:-]*$", x$implementation_id) ||
      !gx_handler_registry_scalar_text(x$availability) ||
      !x$availability %in% c("planned", "classifier_only")) {
    gx_handler_registry_abort("An R handler implementation has an invalid exact shape.")
  }
  package <- if (is.null(x$package)) NA_character_ else x$package
  if (length(package) != 1L || (!is.na(package) &&
      (!gx_handler_registry_scalar_text(package) ||
       !grepl("^[A-Za-z][A-Za-z0-9.]*$", package) ||
       !package %in% .gx_handler_registry_allowed_packages))) {
    gx_handler_registry_abort("An R handler package requirement is invalid.")
  }
  minimum_version <- x$minimum_version %||% NA_character_
  if (length(minimum_version) != 1L || (!is.na(minimum_version) &&
      (!gx_handler_registry_scalar_text(minimum_version) || is.na(package) ||
       !grepl("^[0-9]+(?:\\.[0-9]+)+(?:[-.][A-Za-z0-9]+)*$", minimum_version)))) {
    gx_handler_registry_abort("An R handler minimum package version is invalid.")
  }
  missing <- x$missing_package_status %||% NA_character_
  if (length(missing) != 1L ||
      (!is.na(package) && !identical(missing, "skipped_missing_pkg")) ||
      (is.na(package) && !is.na(missing))) {
    gx_handler_registry_abort("An R handler missing-package status is invalid.")
  }

  has_function <- "function" %in% names(x)
  has_functions <- "functions" %in% names(x)
  if (has_function == has_functions) {
    if (!identical(x$availability, "classifier_only") ||
        has_function || has_functions) {
      gx_handler_registry_abort("Executable handler metadata must name its planned functions.")
    }
  }
  if (has_function && (!gx_handler_registry_scalar_text(x[["function"]]) ||
      !grepl("^[A-Za-z.][A-Za-z0-9._]*$", x[["function"]]))) {
    gx_handler_registry_abort("An implementation function name is invalid.")
  }
  if (has_functions) gx_handler_registry_validate_named_functions(x$functions)

  for (name in intersect(c("query_semantics", "plan_must_record"), names(x))) {
    values <- gx_handler_registry_string_array(
      x[[name]], name, maximum_length = 64L
    )
    if (any(!grepl("^[a-z][a-z0-9_]*$", values))) {
      gx_handler_registry_abort("Implementation planning keys are invalid.")
    }
    x[[name]] <- values
  }
  for (name in intersect(c("lifecycle", "warning"), names(x))) {
    if (!gx_handler_registry_scalar_text(x[[name]])) {
      gx_handler_registry_abort("Implementation lifecycle metadata is invalid.")
    }
  }
  if (("lifecycle" %in% names(x)) != ("warning" %in% names(x))) {
    gx_handler_registry_abort(
      "Implementation deprecation lifecycle and warning metadata must be paired."
    )
  }
  payload_class <- x$payload_class %||% NA_character_
  if (length(payload_class) != 1L || (!is.na(payload_class) &&
      (!gx_handler_registry_scalar_text(payload_class) ||
       !grepl("^[A-Za-z][A-Za-z0-9._]*$", payload_class)))) {
    gx_handler_registry_abort("An implementation payload class is invalid.")
  }
  outcome <- x$outcome %||% "fetch"
  if (!gx_handler_registry_scalar_text(outcome) ||
      !outcome %in% c("fetch", "reference_only") ||
      ("outcome" %in% names(x) && !identical(outcome, "reference_only"))) {
    gx_handler_registry_abort("An implementation outcome is invalid.")
  }
  if (identical(x$availability, "planned") && (!has_function && !has_functions ||
      !identical(outcome, "fetch"))) {
    gx_handler_registry_abort("Every executable implementation must remain planned.")
  }
  if (identical(x$availability, "classifier_only") &&
      (!is.na(package) || !identical(outcome, "reference_only") ||
       has_function || has_functions || !is.na(payload_class))) {
    gx_handler_registry_abort("Classifier-only implementation metadata is executable.")
  }

  planning <- x[intersect(.gx_handler_registry_planning_fields, names(x))]
  if (!length(planning)) planning <- list()
  if ("functions" %in% names(planning)) {
    planning$functions <- gx_handler_registry_validate_named_functions(
      planning$functions
    )
  }
  list(
    implementation_id = x$implementation_id,
    availability = x$availability,
    package = package,
    minimum_version = minimum_version,
    missing_package_status = missing,
    planning_metadata = planning,
    payload_class = payload_class,
    outcome = outcome
  )
}

gx_handler_registry_validate_review <- function(x, implementations) {
  if (!gx_handler_registry_exact_mapping(
    x, c("date", "edr4r", "dataRetrieval")
  ) || !gx_handler_registry_scalar_text(x$date) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x$date)) {
    gx_handler_registry_abort("Implementation review metadata has an invalid root.")
  }
  parsed_date <- suppressWarnings(tryCatch(as.Date(x$date), error = function(cnd) NA))
  if (length(parsed_date) != 1L || is.na(parsed_date) ||
      !identical(format(parsed_date, "%Y-%m-%d"), x$date) ||
      !gx_handler_registry_exact_mapping(
        x$edr4r, c("minimum_version", "reference")
      ) || !gx_handler_registry_scalar_text(x$edr4r$minimum_version) ||
      !gx_is_http_uri(x$edr4r$reference) ||
      !gx_handler_registry_exact_mapping(
        x$dataRetrieval,
        c("current_functions", "legacy_functions", "reference")
      ) || !gx_is_http_uri(x$dataRetrieval$reference)) {
    gx_handler_registry_abort("Implementation review metadata is invalid.")
  }
  current <- gx_handler_registry_string_array(
    x$dataRetrieval$current_functions, "current reviewed functions"
  )
  legacy <- gx_handler_registry_string_array(
    x$dataRetrieval$legacy_functions, "legacy reviewed functions"
  )
  implementation_functions <- vapply(implementations, function(value) {
    if ("function" %in% names(value)) value[["function"]] else NA_character_
  }, character(1))
  implementation_packages <- vapply(implementations, function(value) {
    value$package %||% NA_character_
  }, character(1))
  data_retrieval_functions <- implementation_functions[
    !is.na(implementation_packages) & implementation_packages == "dataRetrieval"
  ]
  edr_versions <- vapply(implementations, function(value) {
    if (identical(value$package, "edr4r")) {
      value$minimum_version %||% NA_character_
    } else {
      NA_character_
    }
  }, character(1))
  if (length(intersect(current, legacy)) ||
      !all(c(current, legacy) %in% data_retrieval_functions) ||
      sum(!is.na(edr_versions)) != 1L ||
      !identical(edr_versions[!is.na(edr_versions)][[1L]],
                 x$edr4r$minimum_version)) {
    gx_handler_registry_abort(
      "Implementation review metadata disagrees with implementation entries.",
      "gx_error_fetch_plan_registry_mismatch"
    )
  }
  invisible(x)
}

gx_handler_registry_validate_lifecycle_binding <- function(
    lifecycle, planning_metadata) {
  has_lifecycle <- "lifecycle" %in% names(planning_metadata)
  has_warning <- "warning" %in% names(planning_metadata)
  valid <- if (identical(lifecycle, "deprecated")) {
    has_lifecycle && has_warning &&
      identical(
        planning_metadata$lifecycle,
        "deprecated_compatibility"
      )
  } else {
    !has_lifecycle && !has_warning
  }
  if (!valid) {
    gx_handler_registry_abort(
      "Portable and R handler lifecycle metadata disagree.",
      "gx_error_fetch_plan_registry_mismatch"
    )
  }
  invisible(planning_metadata)
}

gx_handler_registry_validate_implementations <- function(x, portable) {
  expected <- c(
    "version", "contract_version", "runtime_status", "protocol",
    "implementations", "checked"
  )
  if (!gx_handler_registry_exact_mapping(x, expected) ||
      !identical(x$version, .gx_handler_registry_version) ||
      !identical(x$contract_version, .gx_handler_registry_contract_version) ||
      !identical(
        x$runtime_status,
        "metadata_only; implementations are planned and not exported in the P0 scaffold"
      )) {
    gx_handler_registry_abort("The R implementation registry root is invalid.")
  }
  protocol <- gx_handler_registry_string_array(
    x$protocol, "handler protocol", maximum_length = 8L
  )
  ids <- vapply(portable$handlers, `[[`, character(1), "id")
  if (!identical(protocol, .gx_handler_registry_protocol) ||
      !is.list(x$implementations) || is.object(x$implementations) ||
      !identical(names(x$implementations), ids)) {
    gx_handler_registry_abort(
      "Portable and R implementation registries do not have one-to-one IDs.",
      "gx_error_fetch_plan_registry_mismatch"
    )
  }
  implementations <- lapply(
    x$implementations, gx_handler_registry_validate_implementation
  )
  implementation_ids <- unname(vapply(
    implementations, `[[`, character(1), "implementation_id"
  ))
  outcomes <- unname(vapply(implementations, `[[`, character(1), "outcome"))
  portable_outcomes <- vapply(
    portable$handlers, `[[`, character(1), "outcome"
  )
  availability <- unname(vapply(
    implementations, `[[`, character(1), "availability"
  ))
  final <- length(ids)
  if (anyDuplicated(implementation_ids) ||
      !identical(outcomes, portable_outcomes) ||
      any(availability[-final] != "planned") ||
      !identical(availability[[final]], "classifier_only")) {
    gx_handler_registry_abort(
      "Implementation identities, outcomes, or availability are inconsistent.",
      "gx_error_fetch_plan_registry_mismatch"
    )
  }
  invisible(Map(
    function(portable_handler, implementation) {
      gx_handler_registry_validate_lifecycle_binding(
        portable_handler$lifecycle,
        implementation$planning_metadata
      )
    },
    portable$handlers,
    implementations
  ))
  gx_handler_registry_validate_review(x$checked, x$implementations)
  list(protocol = protocol, implementations = implementations)
}

gx_handler_registry_validate_planning_metadata <- function(x, availability) {
  expected_names <- .gx_handler_registry_planning_fields[
    .gx_handler_registry_planning_fields %in% names(x)
  ]
  valid_names <- if (!length(x)) {
    is.null(names(x))
  } else {
    identical(names(x), expected_names)
  }
  if (!is.list(x) || is.object(x) || !valid_names) {
    gx_handler_registry_abort("Handler planning metadata has an invalid exact shape.")
  }
  has_function <- "function" %in% names(x)
  has_functions <- "functions" %in% names(x)
  if (identical(availability, "planned") && has_function == has_functions) {
    gx_handler_registry_abort("A planned handler must name exactly one function form.")
  }
  if (identical(availability, "classifier_only") && length(x)) {
    gx_handler_registry_abort("A classifier-only handler cannot contain planning metadata.")
  }
  if (has_function && (!gx_handler_registry_scalar_text(x[["function"]]) ||
      !grepl("^[A-Za-z.][A-Za-z0-9._]*$", x[["function"]]))) {
    gx_handler_registry_abort("A planned handler function name is invalid.")
  }
  if (has_functions) gx_handler_registry_validate_named_functions(x$functions)
  for (name in intersect(c("query_semantics", "plan_must_record"), names(x))) {
    values <- gx_handler_registry_string_array(x[[name]], name, maximum_length = 64L)
    if (any(!grepl("^[a-z][a-z0-9_]*$", values))) {
      gx_handler_registry_abort("Handler planning keys are invalid.")
    }
  }
  for (name in intersect(c("lifecycle", "warning"), names(x))) {
    if (!gx_handler_registry_scalar_text(x[[name]])) {
      gx_handler_registry_abort("Handler planning text is invalid.")
    }
  }
  if (("lifecycle" %in% names(x)) != ("warning" %in% names(x))) {
    gx_handler_registry_abort(
      "Handler deprecation lifecycle and warning metadata must be paired."
    )
  }
  invisible(x)
}

gx_handler_registry_validate_impl <- function(x) {
  if (!is.list(x) || !identical(class(x), "gx_handler_registry") ||
      !identical(names(x), .gx_handler_registry_fields) ||
      !identical(x$contract_version, .gx_handler_registry_contract_version) ||
      !identical(x$registry_version, .gx_handler_registry_version) ||
      !identical(x$evaluation, "first_match_wins") ||
      !identical(x$protocol, .gx_handler_registry_protocol) ||
      !identical(x$allowed_fact_names, .gx_handler_registry_allowed_facts) ||
      !gx_handler_registry_scalar_text(x$portable_sha256) ||
      !grepl("^[0-9a-f]{64}$", x$portable_sha256) ||
      !gx_handler_registry_scalar_text(x$implementations_sha256) ||
      !grepl("^[0-9a-f]{64}$", x$implementations_sha256)) {
    gx_handler_registry_abort("A handler registry object violates its exact root contract.")
  }
  gx_handler_registry_assert_text_budget(x)
  handlers <- x$handlers
  if (!inherits(handlers, "tbl_df") ||
      !identical(class(handlers), c("tbl_df", "tbl", "data.frame")) ||
      !identical(names(handlers), .gx_handler_registry_handler_columns) ||
      !nrow(handlers) || nrow(handlers) > .gx_handler_registry_max_handlers ||
      !is.character(handlers$id) || !is.integer(handlers$precedence) ||
      !is.character(handlers$lifecycle) || !is.character(handlers$outcome) ||
      !is.list(handlers$classifier) ||
      !is.character(handlers$implementation_id) ||
      !is.character(handlers$availability) || !is.character(handlers$package) ||
      !is.character(handlers$minimum_version) ||
      !is.character(handlers$missing_package_status) ||
      !is.list(handlers$planning_metadata) ||
      !is.character(handlers$payload_class)) {
    gx_handler_registry_abort("Handler registry rows violate their exact tibble contract.")
  }
  n <- nrow(handlers)
  scalar_columns <- setdiff(
    .gx_handler_registry_handler_columns, c("classifier", "planning_metadata")
  )
  if (any(vapply(handlers[scalar_columns], length, integer(1)) != n) ||
      anyNA(handlers$id) || anyNA(handlers$precedence) ||
      anyNA(handlers$lifecycle) || anyNA(handlers$outcome) ||
      anyNA(handlers$implementation_id) || anyNA(handlers$availability) ||
      anyDuplicated(handlers$id) || anyDuplicated(handlers$precedence) ||
      anyDuplicated(handlers$implementation_id) ||
      (n > 1L && any(diff(handlers$precedence) <= 0L)) ||
      any(!vapply(handlers$id, gx_handler_registry_scalar_text, logical(1))) ||
      any(!grepl("^[a-z][a-z0-9_]*$", handlers$id)) ||
      any(!handlers$lifecycle %in% c("active", "deprecated")) ||
      any(!handlers$outcome %in% c("fetch", "reference_only")) ||
      any(!handlers$availability %in% c("planned", "classifier_only"))) {
    gx_handler_registry_abort("Handler registry row identities or states are invalid.")
  }
  final <- n
  if (!identical(handlers$id[[final]], "unknown") ||
      !identical(handlers$outcome[[final]], "reference_only") ||
      !identical(handlers$availability[[final]], "classifier_only") ||
      any(handlers$outcome[-final] != "fetch") ||
      any(handlers$availability[-final] != "planned")) {
    gx_handler_registry_abort(
      "The final reference-only fallback or planned handler states are invalid.",
      "gx_error_fetch_plan_registry_mismatch"
    )
  }
  for (i in seq_len(n)) {
    gx_handler_registry_validate_classifier(
      handlers$classifier[[i]], x$allowed_fact_names
    )
    fallback <- identical(names(handlers$classifier[[i]]), "always")
    if (fallback != (i == final)) {
      gx_handler_registry_abort("Only the final unknown handler may always match.")
    }
    package <- handlers$package[[i]]
    minimum <- handlers$minimum_version[[i]]
    missing <- handlers$missing_package_status[[i]]
    payload <- handlers$payload_class[[i]]
    if ((!is.na(package) && (!gx_handler_registry_scalar_text(package) ||
        !grepl("^[A-Za-z][A-Za-z0-9.]*$", package) ||
        !package %in% .gx_handler_registry_allowed_packages)) ||
        (!is.na(minimum) && (!gx_handler_registry_scalar_text(minimum) ||
         is.na(package) ||
         !grepl("^[0-9]+(?:\\.[0-9]+)+(?:[-.][A-Za-z0-9]+)*$", minimum))) ||
        (!is.na(package) && !identical(missing, "skipped_missing_pkg")) ||
        (is.na(package) && !is.na(missing)) ||
        (!is.na(payload) && (!gx_handler_registry_scalar_text(payload) ||
         !grepl("^[A-Za-z][A-Za-z0-9._]*$", payload))) ||
        !gx_handler_registry_scalar_text(handlers$implementation_id[[i]]) ||
        !grepl("^[A-Za-z0-9][A-Za-z0-9._:-]*$",
               handlers$implementation_id[[i]])) {
      gx_handler_registry_abort("Handler implementation columns are invalid.")
    }
    gx_handler_registry_validate_planning_metadata(
      handlers$planning_metadata[[i]], handlers$availability[[i]]
    )
    gx_handler_registry_validate_lifecycle_binding(
      handlers$lifecycle[[i]], handlers$planning_metadata[[i]]
    )
  }
  gx_handler_registry_assert_text_budget(x)
  invisible(x)
}

gx_handler_registry_load_impl <- function(
    asset_dir = gx_handler_registry_asset_dir()) {
  if (!gx_handler_registry_scalar_text(asset_dir) || !dir.exists(asset_dir) ||
      gx_path_is_symlink(asset_dir)) {
    gx_handler_registry_abort(
      "The bundled handler registry directory is missing or unsafe.",
      "gx_error_fetch_plan_registry_asset"
    )
  }
  portable_asset <- gx_handler_registry_read_asset(
    file.path(asset_dir, "registry.yml"), "portable handler registry"
  )
  implementations_asset <- gx_handler_registry_read_asset(
    file.path(asset_dir, "implementations-r.json"), "R implementation registry"
  )
  portable <- gx_handler_registry_validate_portable(
    gx_handler_registry_parse_yaml(portable_asset)
  )
  implementations <- gx_handler_registry_validate_implementations(
    gx_handler_registry_parse_json(implementations_asset), portable
  )
  portable_handlers <- portable$handlers
  runtime_handlers <- implementations$implementations
  handlers <- tibble::tibble(
    id = vapply(portable_handlers, `[[`, character(1), "id"),
    precedence = vapply(portable_handlers, `[[`, integer(1), "precedence"),
    lifecycle = vapply(portable_handlers, `[[`, character(1), "lifecycle"),
    outcome = vapply(portable_handlers, `[[`, character(1), "outcome"),
    classifier = unname(lapply(portable_handlers, `[[`, "classifier")),
    implementation_id = unname(vapply(
      runtime_handlers, `[[`, character(1), "implementation_id"
    )),
    availability = unname(vapply(
      runtime_handlers, `[[`, character(1), "availability"
    )),
    package = unname(vapply(runtime_handlers, `[[`, character(1), "package")),
    minimum_version = unname(vapply(
      runtime_handlers, `[[`, character(1), "minimum_version"
    )),
    missing_package_status = unname(vapply(
      runtime_handlers, `[[`, character(1), "missing_package_status"
    )),
    planning_metadata = unname(lapply(
      runtime_handlers, `[[`, "planning_metadata"
    )),
    payload_class = unname(vapply(
      runtime_handlers, `[[`, character(1), "payload_class"
    ))
  )
  registry <- structure(
    list(
      contract_version = portable$contract_version,
      registry_version = portable$registry_version,
      evaluation = portable$evaluation,
      protocol = implementations$protocol,
      allowed_fact_names = portable$allowed_fact_names,
      portable_sha256 = portable_asset$sha256,
      implementations_sha256 = implementations_asset$sha256,
      handlers = handlers
    ),
    class = "gx_handler_registry"
  )
  gx_handler_registry_validate_impl(registry)
  registry
}
