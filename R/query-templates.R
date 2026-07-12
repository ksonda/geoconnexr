gx_query_asset_dir <- function() {
  path <- system.file("queries", package = "geoconnexr")
  if (!nzchar(path)) {
    gx_abort("Bundled query assets could not be located.", "gx_error_query_manifest")
  }
  path
}

gx_read_query_manifest <- function() {
  path <- file.path(gx_query_asset_dir(), "manifest.yml")
  manifest <- yaml::read_yaml(path)
  gx_validate_query_manifest(manifest, dirname(path))
}

gx_validate_query_manifest <- function(manifest, asset_dir) {
  if (!is.list(manifest) || !is.list(manifest$templates) ||
      !length(manifest$templates) || is.null(names(manifest$templates))) {
    gx_abort("Query manifest must contain a named templates mapping.", "gx_error_query_manifest")
  }

  allowed_types <- c("uri", "uri_list", "integer", "wkt")
  required <- c(
    "file", "query_type", "parameters", "stable_order",
    "page_strategy", "result_key", "row_budget"
  )

  for (name in names(manifest$templates)) {
    spec <- manifest$templates[[name]]
    missing <- setdiff(required, names(spec))
    if (length(missing)) {
      gx_abort(
        "Template {.val {name}} is missing: {paste(missing, collapse = ', ')}.",
        "gx_error_query_manifest"
      )
    }
    if (!identical(spec$query_type, "select")) {
      gx_abort("Only named SELECT templates are pageable.", "gx_error_query_manifest")
    }
    if (!is.character(spec$file) || length(spec$file) != 1L ||
        basename(spec$file) != spec$file || !grepl("\\.rq$", spec$file)) {
      gx_abort("Template file paths must be local .rq basenames.", "gx_error_query_manifest")
    }
    path <- file.path(asset_dir, spec$file)
    if (!file.exists(path)) {
      gx_abort("Query template file {.file {spec$file}} is missing.", "gx_error_query_manifest")
    }
    if (!is.list(spec$parameters) || is.null(names(spec$parameters))) {
      gx_abort("Template parameters must be a named mapping.", "gx_error_query_manifest")
    }
    types <- vapply(spec$parameters, function(x) x$type %||% "", character(1))
    if (any(!types %in% allowed_types)) {
      gx_abort("Template contains an unsupported parameter type.", "gx_error_query_manifest")
    }
    if (!length(spec$stable_order) || !length(spec$result_key) ||
        !is.numeric(spec$row_budget) || spec$row_budget < 1) {
      gx_abort("Template ordering, result key, and row budget must be explicit.", "gx_error_query_manifest")
    }

    query <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    hits <- regmatches(query, gregexpr("\\{\\{[A-Za-z][A-Za-z0-9_]*\\}\\}", query))[[1]]
    slots <- if (identical(hits, character(0))) character() else unique(substring(hits, 3L, nchar(hits) - 2L))
    if (!setequal(slots, names(spec$parameters))) {
      gx_abort(
        "Template {.val {name}} slots do not match its parameter contract.",
        "gx_error_query_manifest"
      )
    }
  }
  manifest
}

#' List bundled, typed SPARQL templates
#'
#' @return A tibble with one row per bundled template. List columns preserve
#'   parameter, ordering, and result-key metadata.
#' @export
gx_templates <- function() {
  manifest <- gx_read_query_manifest()
  specs <- manifest$templates
  tibble::tibble(
    name = names(specs),
    file = vapply(specs, `[[`, character(1), "file"),
    query_type = vapply(specs, `[[`, character(1), "query_type"),
    parameters = unname(lapply(specs, `[[`, "parameters")),
    stable_order = unname(lapply(specs, `[[`, "stable_order")),
    page_strategy = vapply(specs, `[[`, character(1), "page_strategy"),
    result_key = unname(lapply(specs, `[[`, "result_key")),
    row_budget = as.integer(vapply(specs, `[[`, numeric(1), "row_budget"))
  )
}

#' Render a bundled SPARQL template safely
#'
#' Parameters are encoded according to the bundled manifest. Raw string
#' interpolation is deliberately unsupported.
#'
#' @param template Name returned by [gx_templates()].
#' @param params A named list containing exactly the template parameters.
#'
#' @return A length-one character SPARQL query.
#' @export
gx_render_query <- function(template, params) {
  if (!is.character(template) || length(template) != 1L || is.na(template)) {
    gx_abort("{.arg template} must be one template name.", "gx_error_query")
  }
  if (!is.list(params) || is.null(names(params)) || any(!nzchar(names(params)))) {
    gx_abort("{.arg params} must be a fully named list.", "gx_error_query_parameter")
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
    if (!is.numeric(limit) || !is.numeric(offset) || length(limit) != 1L ||
        length(offset) != 1L || is.na(limit) || is.na(offset) ||
        limit + offset > spec$row_budget) {
      gx_abort(
        "Requested page exceeds the template row budget of {spec$row_budget}.",
        "gx_error_query_parameter"
      )
    }
  }

  path <- file.path(gx_query_asset_dir(), spec$file)
  query <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (name in required) {
    encoded <- gx_encode_parameter(params[[name]], spec$parameters[[name]])
    query <- gsub(paste0("{{", name, "}}"), encoded, query, fixed = TRUE)
  }
  if (grepl("\\{\\{[^}]+\\}\\}", query)) {
    gx_abort("Rendered query contains unresolved slots.", "gx_error_query")
  }
  if (nchar(enc2utf8(query), type = "bytes") > 262144L) {
    gx_abort("Rendered queries may not exceed 256 KiB.", "gx_error_query")
  }
  query
}
