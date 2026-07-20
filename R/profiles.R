.gx_schema_https <- "https://schema.org/"
.gx_schema_http <- "http://schema.org/"
.gx_hyf <- "https://www.opengis.net/def/schema/hy_features/hyf/"
.gx_gsp <- "http://www.opengis.net/ont/geosparql#"
.gx_skos <- "http://www.w3.org/2004/02/skos/core#"
.gx_loctype <- "https://www.opengis.net/def/schema/hy_features/hyf/HY_HydroLocationType/"

gx_schema_terms <- function(term) {
  paste0(c(.gx_schema_https, .gx_schema_http), term)
}

gx_as_nodes <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  if (!is.list(x)) {
    return(as.list(x))
  }
  if (is.null(names(x)) || !any(nzchar(names(x)))) x else list(x)
}

gx_prop <- function(node, keys) {
  if (!is.list(node) || is.null(names(node))) {
    return(list())
  }
  out <- list()
  for (key in keys) {
    if (key %in% names(node)) {
      out <- c(out, gx_as_nodes(node[[key]]))
    }
  }
  out
}

gx_node_types <- function(node) {
  types <- node[["@type"]] %||% character()
  unique(as.character(unlist(types, use.names = FALSE)))
}

gx_scalar_texts <- function(value, include_id = FALSE) {
  out <- character()
  walk <- function(x) {
    if (is.null(x)) {
      return(invisible(NULL))
    }
    if (is.atomic(x) && !is.list(x)) {
      values <- as.character(x)
      out <<- c(out, values[!is.na(values)])
      return(invisible(NULL))
    }
    if (!is.list(x)) {
      return(invisible(NULL))
    }
    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      if ("@value" %in% names(x)) {
        walk(x[["@value"]])
      } else if (include_id && "@id" %in% names(x)) {
        walk(x[["@id"]])
      }
      return(invisible(NULL))
    }
    invisible(lapply(x, walk))
  }
  walk(value)
  unique(out)
}

gx_first_text <- function(value, include_id = FALSE) {
  values <- gx_scalar_texts(value, include_id = include_id)
  if (length(values)) values[[1]] else NA_character_
}

gx_first_iri <- function(value) {
  values <- gx_scalar_texts(value, include_id = TRUE)
  values <- values[grepl("^[A-Za-z][A-Za-z0-9+.-]*:", values)]
  values <- values[!is.na(vapply(values, gx_identity_iri, character(1)))]
  if (length(values)) values[[1]] else NA_character_
}

gx_find_property <- function(value, keys) {
  found <- list()
  walk <- function(x) {
    if (!is.list(x)) {
      return(invisible(NULL))
    }
    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      for (key in intersect(keys, names(x))) {
        found <<- c(found, gx_as_nodes(x[[key]]))
      }
    }
    invisible(lapply(x, walk))
  }
  walk(value)
  found
}

gx_flatten_graph <- function(expanded) {
  nodes <- list()
  walk <- function(x) {
    for (node in gx_as_nodes(x)) {
      if (!is.list(node)) {
        next
      }
      graph <- node[["@graph"]]
      remainder <- node[setdiff(names(node) %||% character(), "@graph")]
      if (length(remainder)) {
        nodes[[length(nodes) + 1L]] <<- remainder
      }
      if (!is.null(graph)) {
        walk(graph)
      } else if (!length(remainder)) {
        nodes[[length(nodes) + 1L]] <<- node
      }
    }
    invisible(NULL)
  }
  walk(expanded)
  nodes
}

gx_max_id_fragments <- function() {
  gx_scalar_number(
    getOption("geoconnexr.jsonld_max_id_fragments", 64L),
    "geoconnexr.jsonld_max_id_fragments",
    minimum = 1,
    maximum = 10000,
    integer = TRUE
  )
}

gx_node_bucket_add <- function(buckets, id, node, max_fragments) {
  bucket <- if (exists(id, envir = buckets, inherits = FALSE)) {
    get(id, envir = buckets, inherits = FALSE)
  } else {
    new.env(parent = emptyenv())
  }
  if (length(node) > 1L) {
    count <- length(bucket$definitions %||% list()) + 1L
    if (count > max_fragments) {
      gx_abort("JSON-LD repeats one node identity beyond the fragment budget.", "gx_error_parser_budget")
    }
    bucket$definitions <- c(bucket$definitions %||% list(), list(node))
  } else {
    bucket$reference_seen <- TRUE
  }
  assign(id, bucket, envir = buckets)
  invisible(NULL)
}

gx_merge_node_fragments <- function(id, bucket) {
  fragments <- bucket$definitions %||% list()
  if (!length(fragments)) return(list(`@id` = id))
  values <- list()
  property_order <- character()
  for (fragment in fragments) {
    for (name in setdiff(names(fragment) %||% character(), "@id")) {
      if (!name %in% property_order) property_order <- c(property_order, name)
      values[[name]] <- c(values[[name]] %||% list(), gx_as_nodes(fragment[[name]]))
    }
  }
  out <- list(`@id` = id)
  for (name in property_order) {
    property_values <- values[[name]]
    keys <- vapply(property_values, gx_json_serialize, character(1))
    out[[name]] <- property_values[!duplicated(keys)]
  }
  out
}

gx_merge_graph_nodes <- function(nodes) {
  positions <- new.env(parent = emptyenv())
  buckets <- new.env(parent = emptyenv())
  max_fragments <- gx_max_id_fragments()
  out <- list()
  for (node in nodes) {
    id <- if (is.list(node)) node[["@id"]] else NULL
    if (is.character(id) && length(id) == 1L && nzchar(id)) {
      gx_node_bucket_add(buckets, id, node, max_fragments)
      if (!exists(id, envir = positions, inherits = FALSE)) {
        out[[length(out) + 1L]] <- id
        assign(id, length(out), envir = positions)
      }
    } else {
      out[[length(out) + 1L]] <- node
    }
  }
  for (id in ls(positions, all.names = TRUE)) {
    position <- get(id, envir = positions, inherits = FALSE)
    out[[position]] <- gx_merge_node_fragments(id, get(id, envir = buckets, inherits = FALSE))
  }
  out
}

gx_node_index <- function(value) {
  index <- new.env(parent = emptyenv())
  buckets <- new.env(parent = emptyenv())
  max_fragments <- gx_max_id_fragments()
  walk <- function(x) {
    if (!is.list(x)) {
      return(invisible(NULL))
    }
    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      id <- x[["@id"]]
      if (is.character(id) && length(id) == 1L && nzchar(id)) {
        gx_node_bucket_add(buckets, id, x, max_fragments)
      }
    }
    invisible(lapply(x, walk))
  }
  walk(value)
  for (id in ls(buckets, all.names = TRUE)) {
    assign(
      id,
      gx_merge_node_fragments(id, get(id, envir = buckets, inherits = FALSE)),
      envir = index
    )
  }
  index
}

gx_dereference <- function(value, index) {
  nodes <- gx_as_nodes(value)
  lapply(nodes, function(node) {
    if (is.list(node) && identical(sort(names(node)), "@id")) {
      id <- node[["@id"]]
      if (is.character(id) && exists(id, envir = index, inherits = FALSE)) {
        return(get(id, envir = index, inherits = FALSE))
      }
    }
    node
  })
}

gx_profile_input <- function(x) {
  if (inherits(x, "gx_jsonld")) {
    return(list(
      expanded = x$expanded,
      source = x$source_document,
      pid_uri = x$pid_uri,
      landing_url = x$landing_url,
      source_url = x$source_url,
      diagnostics = x$diagnostics
    ))
  }
  max_bytes <- gx_scalar_number(
    getOption("geoconnexr.jsonld_max_local_bytes", 2L * 1024L^2),
    "geoconnexr.jsonld_max_local_bytes",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  if (is.raw(x)) {
    input_bytes <- length(x)
    if (input_bytes > max_bytes) {
      gx_abort("Local JSON-LD input exceeds the configured byte limit.", "gx_error_jsonld_too_large")
    }
    source <- gx_json_parse(gx_json_text(x))
  } else if (is.character(x) && length(x) == 1L && !is.na(x)) {
    input_bytes <- nchar(enc2utf8(x), type = "bytes")
    if (input_bytes > max_bytes) {
      gx_abort("Local JSON-LD input exceeds the configured byte limit.", "gx_error_jsonld_too_large")
    }
    source <- gx_json_parse(x)
  } else if (is.list(x)) {
    gx_json_assert_complexity(x, max_bytes = max_bytes)
    input_bytes <- nchar(gx_json_serialize(x), type = "bytes")
    source <- x
  } else {
    gx_abort(
      "{.arg x} must be a gx_jsonld object, JSON string, raw JSON, or parsed JSON-LD list.",
      "gx_error_jsonld_input"
    )
  }
  if (input_bytes > max_bytes) {
    gx_abort("Local JSON-LD input exceeds the configured byte limit.", "gx_error_jsonld_too_large")
  }
  prepared <- gx_prepare_jsonld(source)
  list(
    expanded = prepared$expanded,
    source = source,
    pid_uri = NA_character_,
    landing_url = NA_character_,
    source_url = NA_character_,
    diagnostics = prepared$diagnostics
  )
}

gx_location_candidates <- function(nodes) {
  hydrometric_types <- c(
    paste0(.gx_hyf, "HY_HydrometricFeature"),
    paste0(.gx_hyf, "HY_HydroLocation")
  )
  vapply(nodes, function(node) {
    types <- gx_node_types(node)
    is_hydrometric <- any(types %in% hydrometric_types)
    has_position <- length(gx_prop(node, paste0(.gx_hyf, "referencedPosition"))) > 0L
    is_place <- any(types %in% gx_schema_terms("Place"))
    has_datasets <- length(gx_prop(node, gx_schema_terms("subjectOf"))) > 0L
    is_hydrometric || has_position || (is_place && has_datasets) ||
      gx_generic_state_gage_place(node, types)
  }, logical(1))
}

gx_has_wkt_geometry <- function(node) {
  geometries <- gx_prop(node, paste0(.gx_gsp, "hasGeometry"))
  wkts <- unlist(lapply(geometries, function(geometry) {
    gx_scalar_texts(gx_prop(geometry, paste0(.gx_gsp, "asWKT")))
  }), use.names = FALSE)
  any(!is.na(wkts) & nzchar(trimws(wkts)))
}

gx_generic_state_gage_place <- function(node, types = gx_node_types(node)) {
  id <- node[["@id"]]
  is.character(id) && length(id) == 1L && !is.na(id) &&
    grepl(
      "^https://geoconnex[.]us/(cdss|mtdnrc|ndwr|nednr|wyseo)/gages/[^/?#]+$",
      id
    ) &&
    any(types %in% gx_schema_terms("Place")) &&
    all(types %in% gx_schema_terms("Place")) &&
    gx_has_wkt_geometry(node)
}

gx_provider_fields <- function(node) {
  providers <- gx_prop(node, gx_schema_terms("provider"))
  if (!length(providers)) {
    return(list(uri = NA_character_, name = NA_character_, url = NA_character_))
  }
  provider <- providers[[1]]
  uri <- gx_first_iri(provider)
  name <- gx_first_text(gx_prop(provider, gx_schema_terms("name")))
  url <- gx_first_iri(gx_prop(provider, gx_schema_terms("url")))
  list(uri = uri, name = name, url = url)
}

gx_conflict_diagnostics <- function(node, fields, path) {
  diagnostics <- gx_empty_diagnostics()
  for (field in names(fields)) {
    values <- unique(gx_scalar_texts(gx_prop(node, fields[[field]]), include_id = TRUE))
    if (length(values) > 1L) {
      diagnostics <- gx_bind_diagnostics(
        diagnostics,
        gx_diagnostic(
          "warning",
          paste0("contradictory_", field),
          path,
          paste0("Multiple distinct ", gsub("_", " ", field), " values were present; the first was selected.")
        )
      )
    }
  }
  diagnostics
}

gx_provider_diagnostics <- function(node, path) {
  providers <- gx_prop(node, gx_schema_terms("provider"))
  if (length(providers) <= 1L) return(gx_empty_diagnostics())
  signatures <- vapply(providers, function(provider) {
    fields <- list(
      gx_first_iri(provider),
      gx_first_text(gx_prop(provider, gx_schema_terms("name"))),
      gx_first_iri(gx_prop(provider, gx_schema_terms("url")))
    )
    paste(vapply(fields, function(x) if (is.na(x)) "<NA>" else x, character(1)), collapse = "|")
  }, character(1))
  if (length(unique(signatures)) <= 1L) return(gx_empty_diagnostics())
  gx_diagnostic(
    "warning", "contradictory_provider", path,
    "Multiple distinct providers were present; the first was selected."
  )
}

gx_location_row <- function(node, input, path) {
  diagnostics <- gx_empty_diagnostics()
  site_uri <- as.character(node[["@id"]] %||% NA_character_)
  if (!is.na(site_uri) && is.na(gx_identity_iri(site_uri))) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "invalid_site_uri", path, "Location @id is not a valid absolute identity IRI.")
    )
  }
  diagnostics <- gx_bind_diagnostics(
    diagnostics,
    gx_conflict_diagnostics(
      node,
      list(
        location_name = gx_schema_terms("name"),
        location_description = gx_schema_terms("description"),
        literal_location_type = paste0(.gx_hyf, "HY_HydroLocationType")
      ),
      path
    ),
    gx_provider_diagnostics(node, path)
  )
  types <- gx_node_types(node)
  hydrometric_types <- paste0(.gx_hyf, c("HY_HydrometricFeature", "HY_HydroLocation"))
  if (gx_generic_state_gage_place(node, types)) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic(
        "warning", "generic_place_geometry", path,
        "Accepted a generic schema:Place in a reviewed state-gage PID namespace because it carries GeoSPARQL WKT."
      )
    )
  }
  generic <- c(
    gx_schema_terms("Place"),
    hydrometric_types
  )
  profile_types <- setdiff(types, generic)
  preferred_types <- profile_types[startsWith(profile_types, .gx_loctype)]
  if (length(preferred_types) > 1L) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "contradictory_location_type", path, "Multiple location-type IRIs were present; the first was selected.")
    )
  }
  site_type <- if (length(preferred_types)) preferred_types[[1]] else NA_character_

  literal_types <- gx_scalar_texts(gx_prop(node, paste0(.gx_hyf, "HY_HydroLocationType")))
  if (length(literal_types)) {
    site_type <- literal_types[[1]]
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic(
        "warning", "literal_location_type", paste0(path, "/HY_HydroLocationType"),
        "Retained a literal hydro-location type that is not an IRI."
      )
    )
  }

  geometries <- gx_prop(node, paste0(.gx_gsp, "hasGeometry"))
  wkts <- unique(unlist(lapply(
    geometries,
    function(geometry) gx_scalar_texts(gx_prop(geometry, paste0(.gx_gsp, "asWKT")))
  ), use.names = FALSE))
  crs <- unique(unlist(lapply(
    geometries,
    function(geometry) gx_scalar_texts(gx_prop(geometry, paste0(.gx_gsp, "crs")), include_id = TRUE)
  ), use.names = FALSE))
  if (!length(wkts)) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "missing_geometry", path, "Location has no GeoSPARQL WKT geometry.")
    )
  } else if (length(wkts) > 1L) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "multiple_geometries", path, "Location has multiple distinct WKT geometries; the first was selected.")
    )
  }

  positions <- gx_prop(node, paste0(.gx_hyf, "referencedPosition"))
  linear_elements <- unique(gx_scalar_texts(
    gx_find_property(positions, paste0(.gx_hyf, "linearElement")),
    include_id = TRUE
  ))
  linear_elements <- linear_elements[grepl("^[A-Za-z][A-Za-z0-9+.-]*:", linear_elements)]
  canonical_elements <- vapply(linear_elements, gx_identity_iri, character(1))
  is_mainstem <- !is.na(canonical_elements) & grepl(
    "^https://geoconnex[.]us/ref/mainstems/[^/?#]+$",
    canonical_elements
  )
  mainstems <- canonical_elements[is_mainstem]
  unsupported <- linear_elements[!is_mainstem]
  if (length(unsupported)) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "unsupported_linear_element", path, "Ignored a referenced linear element that is not a Geoconnex mainstem PID.")
    )
  }
  if (length(mainstems) > 1L) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "multiple_mainstems", path, "Location references multiple mainstems; the first was selected.")
    )
  }
  provider <- gx_provider_fields(node)
  list(
    contract_version = .gx_jsonld_contract_version,
    site_uri = site_uri,
    name = gx_first_text(gx_prop(node, gx_schema_terms("name"))),
    description = gx_first_text(gx_prop(node, gx_schema_terms("description"))),
    site_type = site_type,
    rdf_types = types,
    provider_uri = provider$uri,
    provider_name = provider$name,
    provider_url = provider$url,
    mainstem_uri = if (length(mainstems)) mainstems[[1]] else NA_character_,
    geometry_wkt = if (length(wkts)) wkts[[1]] else NA_character_,
    geometry_crs = if (length(crs)) crs[[1]] else NA_character_,
    landing_url = input$landing_url,
    source_url = input$source_url,
    diagnostics = diagnostics
  )
}

.gx_location_columns <- c(
  "contract_version", "site_uri", "name", "description", "site_type",
  "rdf_types", "provider_uri", "provider_name", "provider_url",
  "mainstem_uri", "geometry_wkt", "geometry_crs", "landing_url",
  "source_url", "diagnostics"
)

.gx_dataset_columns <- c(
  "contract_version", "site_uri", "dataset_id", "distribution_id",
  "variable_id", "dataset_uri", "dataset_name", "dataset_description",
  "temporal_coverage", "temporal_start", "temporal_end", "variable_uri",
  "variable_name", "unit_uri", "unit_label", "measurement_technique",
  "distribution_url", "media_type", "conforms_to", "provider_uri",
  "provider_name", "provider_url", "license", "access_rights", "handler_id",
  "fetchable", "source_url", "diagnostics"
)

gx_is_diagnostics <- function(x) {
  inherits(x, "data.frame") &&
    identical(names(x), c("severity", "code", "path", "message", "recoverable")) &&
    all(vapply(x[c("severity", "code", "path", "message")], is.character, logical(1))) &&
    is.logical(x$recoverable)
}

gx_new_profile_table <- function(data, diagnostics, class_name, columns,
                                 list_columns, posixct_columns = character(),
                                 logical_columns = character()) {
  if (!inherits(data, "tbl_df") || !identical(names(data), columns) ||
      !gx_is_diagnostics(diagnostics)) {
    gx_abort("Internal profile-table contract validation failed.", "gx_error_internal")
  }
  character_columns <- setdiff(columns, c(list_columns, posixct_columns, logical_columns))
  valid <- all(vapply(data[character_columns], is.character, logical(1))) &&
    all(vapply(data[list_columns], is.list, logical(1))) &&
    all(vapply(data[posixct_columns], inherits, logical(1), what = "POSIXct")) &&
    all(vapply(data[logical_columns], is.logical, logical(1))) &&
    all(vapply(data$diagnostics, gx_is_diagnostics, logical(1)))
  if (!valid) {
    gx_abort("Internal profile-column type validation failed.", "gx_error_internal")
  }
  attr(data, "diagnostics") <- diagnostics
  class(data) <- c(class_name, class(data))
  data
}

gx_new_location <- function(data, diagnostics) {
  gx_new_profile_table(
    data, diagnostics, "gx_location", .gx_location_columns,
    list_columns = c("rdf_types", "diagnostics")
  )
}

gx_new_datasets <- function(data, diagnostics) {
  gx_new_profile_table(
    data, diagnostics, "gx_datasets", .gx_dataset_columns,
    list_columns = c("conforms_to", "diagnostics"),
    posixct_columns = c("temporal_start", "temporal_end"),
    logical_columns = "fetchable"
  )
}

gx_profile_print_data <- function(x, class_name) {
  out <- x
  url_columns <- grep("(_uri|_url)$|^license$", names(out), value = TRUE)
  for (column in url_columns) {
    is_url <- !is.na(out[[column]]) & grepl("^https?://", out[[column]], ignore.case = TRUE)
    out[[column]][is_url] <- vapply(out[[column]][is_url], gx_redact_url, character(1))
  }
  class(out) <- setdiff(class(out), class_name)
  out
}

gx_empty_locations <- function(diagnostics = gx_empty_diagnostics()) {
  out <- tibble::tibble(
    contract_version = character(), site_uri = character(), name = character(),
    description = character(), site_type = character(), rdf_types = list(),
    provider_uri = character(), provider_name = character(), provider_url = character(),
    mainstem_uri = character(), geometry_wkt = character(), geometry_crs = character(),
    landing_url = character(), source_url = character(), diagnostics = list()
  )
  gx_new_location(out, diagnostics)
}

#' Parse Geoconnex monitoring-location profiles
#'
#' Extracts documented and known production Geoconnex location shapes from
#' safely expanded JSON-LD. A generic `schema:Place` is accepted only for
#' reviewed state-gage PID namespaces when it carries nonempty GeoSPARQL WKT;
#' this emits a warning-level `generic_place_geometry` diagnostic. WKT remains
#' text at this protocol layer.
#'
#' @param x A [gx_jsonld()] object, parsed JSON-LD list, JSON string, or raw
#'   JSON bytes.
#' @param strict Whether any warning/error diagnostic should abort after
#'   deterministic parsing.
#'
#' @return A `gx_location` tibble with one row per supported location. Its exact
#'   columns are `contract_version`, `site_uri`, `name`, `description`,
#'   `site_type`, `rdf_types`, `provider_uri`, `provider_name`, `provider_url`,
#'   `mainstem_uri`, `geometry_wkt`, `geometry_crs`, `landing_url`, `source_url`,
#'   and `diagnostics`. All are character except the two list-columns
#'   `rdf_types` and `diagnostics`. Zero-row results preserve those types.
#'
#' @section Diagnostics and provenance:
#' Each row's `diagnostics` entry has fixed character columns `severity`, `code`,
#' `path`, and `message`, plus logical `recoverable`. The table's `diagnostics`
#' attribute includes document-level and row-level diagnostics, including for a
#' zero-row result. With `strict = TRUE`, any warning or error is reported as a
#' `gx_error_parser_strict` after deterministic parsing. The table retains exact
#' provenance URLs; its print method redacts query values and credentials.
#'
#' This P0 contract is provisional: WKT remains text, duplicate relationships
#' remain diagnostic, and no canonical-site or typed-geometry contract is yet
#' asserted.
#' @export
gx_parse_location <- function(x, strict = FALSE) {
  input <- gx_profile_input(x)
  nodes <- gx_merge_graph_nodes(gx_flatten_graph(input$expanded))
  keep <- gx_location_candidates(nodes)
  if (!any(keep)) {
    diagnostics <- gx_bind_diagnostics(
      input$diagnostics,
      gx_diagnostic("warning", "no_location_node", "", "Document contains no supported monitoring-location node.")
    )
    gx_strict_diagnostics(diagnostics, strict, "location")
    return(gx_empty_locations(diagnostics))
  }
  selected <- nodes[keep]
  rows <- lapply(seq_along(selected), function(i) {
    gx_location_row(selected[[i]], input, paste0("/expanded/", i - 1L))
  })
  out <- tibble::tibble(
    contract_version = vapply(rows, `[[`, character(1), "contract_version"),
    site_uri = vapply(rows, `[[`, character(1), "site_uri"),
    name = vapply(rows, `[[`, character(1), "name"),
    description = vapply(rows, `[[`, character(1), "description"),
    site_type = vapply(rows, `[[`, character(1), "site_type"),
    rdf_types = unname(lapply(rows, `[[`, "rdf_types")),
    provider_uri = vapply(rows, `[[`, character(1), "provider_uri"),
    provider_name = vapply(rows, `[[`, character(1), "provider_name"),
    provider_url = vapply(rows, `[[`, character(1), "provider_url"),
    mainstem_uri = vapply(rows, `[[`, character(1), "mainstem_uri"),
    geometry_wkt = vapply(rows, `[[`, character(1), "geometry_wkt"),
    geometry_crs = vapply(rows, `[[`, character(1), "geometry_crs"),
    landing_url = vapply(rows, `[[`, character(1), "landing_url"),
    source_url = vapply(rows, `[[`, character(1), "source_url"),
    diagnostics = unname(lapply(rows, `[[`, "diagnostics"))
  )
  diagnostics <- gx_bind_diagnostics(input$diagnostics, do.call(gx_bind_diagnostics, lapply(rows, `[[`, "diagnostics")))
  diagnostics <- unique(diagnostics)
  out <- gx_new_location(out, diagnostics)
  gx_strict_diagnostics(diagnostics, strict, "location")
  out
}

#' @export
print.gx_location <- function(x, ...) {
  cli::cli_inform("<gx_location> {nrow(x)} row{?s}; {nrow(attr(x, 'diagnostics'))} diagnostic{?s}")
  print(gx_profile_print_data(x, "gx_location"), ...)
  invisible(x)
}

gx_identity_iri <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl("^[A-Za-z][A-Za-z0-9+.-]*:.+$", value) ||
      stringi::stri_detect_regex(value, "[\\p{Z}\\p{Cc}\\p{Cf}\\p{Cs}]") ||
      !stringi::stri_enc_isutf8(value)) {
    return(NA_character_)
  }
  scheme <- sub(":.*$", "", value)
  canonical_scheme <- tolower(scheme)
  if (!canonical_scheme %in% c("http", "https")) {
    return(paste0(canonical_scheme, substring(value, nchar(scheme) + 1L)))
  }
  parsed <- tryCatch(httr2::url_parse(value), error = function(cnd) NULL)
  if (is.null(parsed) || is.null(parsed$scheme)) {
    return(NA_character_)
  }
  if (is.null(parsed$hostname) || !nzchar(parsed$hostname) ||
      !is.null(parsed$username) || !is.null(parsed$password)) {
    return(NA_character_)
  }
  parsed$scheme <- canonical_scheme
  if (!is.null(parsed$hostname)) {
    parsed$hostname <- tolower(sub("\\.$", "", parsed$hostname))
    if ((identical(parsed$scheme, "https") && identical(parsed$port, "443")) ||
        (identical(parsed$scheme, "http") && identical(parsed$port, "80"))) {
      parsed$port <- NULL
    }
  }
  tryCatch(httr2::url_build(parsed), error = function(cnd) enc2utf8(value))
}

gx_normalize_label <- function(value) {
  if (is.na(value)) return(NA_character_)
  value <- stringi::stri_trans_nfc(enc2utf8(value))
  value <- gsub("^[ \\t\\r\\n]+|[ \\t\\r\\n]+$", "", value, perl = TRUE)
  value <- gsub("[ \\t\\r\\n]+", " ", value, perl = TRUE)
  stringi::stri_trans_nfc(stringi::stri_trans_casefold(value))
}

gx_normalize_media_type <- function(value) {
  if (is.na(value) || !nzchar(trimws(value))) return(NA_character_)
  tolower(trimws(sub(";.*$", "", value)))
}

gx_parse_temporal <- function(value, path) {
  diagnostics <- gx_empty_diagnostics()
  start <- end <- as.POSIXct(NA, tz = "UTC")
  if (is.na(value) || !nzchar(value)) {
    return(list(start = start, end = end, diagnostics = diagnostics))
  }
  parts <- strsplit(value, "/", fixed = TRUE)[[1]]
  if (length(parts) != 2L) {
    diagnostics <- gx_diagnostic("warning", "invalid_temporal_coverage", path, "Temporal coverage is not a start/end interval.")
    return(list(start = start, end = end, diagnostics = diagnostics))
  }
  parse_one <- function(text, endpoint) {
    if (!nzchar(text) || text %in% c("..", "\u2026")) {
      return(as.POSIXct(NA, tz = "UTC"))
    }
    normalized <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", text)
    format <- if (grepl("Z$", normalized)) {
      "%Y-%m-%dT%H:%M:%OSZ"
    } else if (grepl("[+-][0-9]{4}$", normalized)) {
      "%Y-%m-%dT%H:%M:%OS%z"
    } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", normalized)) {
      "%Y-%m-%d"
    } else {
      "%Y-%m-%dT%H:%M:%OS"
    }
    parsed <- tryCatch({
      value <- strptime(normalized, format = format, tz = "UTC")
      as.POSIXct(value, tz = "UTC")
    }, error = function(cnd) as.POSIXct(NA, tz = "UTC"))
    if (is.na(parsed)) {
      diagnostics <<- gx_bind_diagnostics(
        diagnostics,
        gx_diagnostic("warning", "invalid_temporal_endpoint", paste0(path, "/", endpoint), "Temporal interval endpoint could not be parsed as an ISO date-time.")
      )
    }
    parsed
  }
  start <- parse_one(parts[[1]], "start")
  end <- parse_one(parts[[2]], "end")
  list(start = start, end = end, diagnostics = diagnostics)
}

gx_dataset_id <- function(site_uri, dataset_uri, provider, dataset_name, diagnostics, path) {
  site <- gx_identity_iri(site_uri)
  dataset <- gx_identity_iri(dataset_uri)
  if (!is.na(dataset_uri) && is.na(dataset)) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "invalid_dataset_uri", path, "Dataset URI is malformed; fallback identity fields were used when available.")
    )
  }
  if (!is.na(site_uri) && is.na(site)) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "invalid_site_uri", path, "Site URI is malformed and cannot support a stable dataset ID.")
    )
  }
  if (!is.na(site) && !is.na(dataset)) {
    return(list(
      id = gx_contract_hash(
        list("uri", site, dataset),
        "geoconnexr.dataset-id.v1",
        .gx_jsonld_contract_version
      ),
      diagnostics = diagnostics
    ))
  }
  provider_identity <- gx_identity_iri(provider$uri)
  if (is.na(provider_identity)) provider_identity <- gx_normalize_label(provider$name)
  name <- gx_normalize_label(dataset_name)
  if (is.na(site) || is.na(provider_identity) || is.na(name) || !nzchar(name)) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "dataset_identity_incomplete", path, "Dataset lacks enough identity fields for a stable fallback ID.")
    )
    return(list(id = NA_character_, diagnostics = diagnostics))
  }
  list(
    id = gx_contract_hash(
      list("fallback", site, provider_identity, name),
      "geoconnexr.dataset-id.v1",
      .gx_jsonld_contract_version
    ),
    diagnostics = diagnostics
  )
}

gx_distribution_fields <- function(node) {
  if (is.null(node)) {
    return(list(url = NA_character_, media_type = NA_character_, conforms_to = character()))
  }
  list(
    url = gx_first_text(gx_prop(node, c(gx_schema_terms("contentUrl"), gx_schema_terms("accessURL"), gx_schema_terms("url"))), include_id = TRUE),
    media_type = gx_first_text(gx_prop(node, gx_schema_terms("encodingFormat"))),
    conforms_to = gx_scalar_texts(gx_prop(node, gx_schema_terms("conformsTo")), include_id = TRUE)
  )
}

gx_variable_fields <- function(node) {
  if (is.null(node)) {
    return(list(uri = NA_character_, name = NA_character_, unit_uri = NA_character_, unit_label = NA_character_, technique = NA_character_))
  }
  matches <- gx_prop(node, paste0(.gx_skos, c("exactMatch", "broadMatch", "closeMatch")))
  uri <- gx_first_iri(node)
  if (is.na(uri)) uri <- gx_first_iri(matches)
  unit_values <- gx_prop(node, c(gx_schema_terms("unitCode"), "http://qudt.org/schema/qudt/unit"))
  list(
    uri = uri,
    name = gx_first_text(gx_prop(node, gx_schema_terms("name"))),
    unit_uri = gx_first_iri(unit_values),
    unit_label = gx_first_text(gx_prop(node, gx_schema_terms("unitText"))),
    technique = gx_first_text(gx_prop(node, gx_schema_terms("measurementTechnique")), include_id = TRUE)
  )
}

gx_dataset_rows <- function(dataset, site, input, index, dataset_number, budget) {
  path <- paste0("/dataset/", dataset_number - 1L)
  diagnostics <- gx_bind_diagnostics(
    gx_conflict_diagnostics(
      dataset,
      list(
        dataset_name = gx_schema_terms("name"),
        dataset_description = gx_schema_terms("description"),
        temporal_coverage = gx_schema_terms("temporalCoverage"),
        license = gx_schema_terms("license"),
        access_rights = gx_schema_terms("accessRights")
      ),
      path
    ),
    gx_provider_diagnostics(dataset, path)
  )
  dataset_types <- gx_node_types(dataset)
  if (!any(dataset_types %in% gx_schema_terms("Dataset"))) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "unexpected_subject_type", path, "subjectOf entry is not typed as schema:Dataset but was retained tolerantly.")
    )
  } else if (length(setdiff(dataset_types, gx_schema_terms("Dataset")))) {
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "contradictory_dataset_type", path, "Dataset entry also declares additional RDF types; all types were retained.")
    )
  }
  site_provider <- gx_provider_fields(site)
  provider <- gx_provider_fields(dataset)
  if (all(is.na(unlist(provider, use.names = FALSE)))) provider <- site_provider
  dataset_uri <- as.character(dataset[["@id"]] %||% NA_character_)
  dataset_name <- gx_first_text(gx_prop(dataset, gx_schema_terms("name")))
  id_result <- gx_dataset_id(
    as.character(site[["@id"]] %||% NA_character_), dataset_uri,
    provider, dataset_name, diagnostics, path
  )
  diagnostics <- id_result$diagnostics
  temporal_coverage <- gx_first_text(gx_prop(dataset, gx_schema_terms("temporalCoverage")))
  temporal <- gx_parse_temporal(temporal_coverage, paste0(path, "/temporalCoverage"))
  diagnostics <- gx_bind_diagnostics(diagnostics, temporal$diagnostics)

  distributions <- gx_dereference(gx_prop(dataset, gx_schema_terms("distribution")), index)
  variables <- gx_dereference(gx_prop(dataset, gx_schema_terms("variableMeasured")), index)
  if (!length(distributions)) {
    distributions <- list(NULL)
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "missing_distribution", path, "Dataset has no described distribution.")
    )
  }
  if (!length(variables)) {
    variables <- list(NULL)
    diagnostics <- gx_bind_diagnostics(
      diagnostics,
      gx_diagnostic("warning", "missing_variable", path, "Dataset has no described variable.")
    )
  }

  product_rows <- as.double(length(distributions)) * as.double(length(variables))
  if (!is.finite(product_rows) || budget$rows + product_rows > budget$max_rows) {
    gx_abort(
      "Dataset profile exceeds the configured output-row budget of {budget$max_rows}.",
      "gx_error_parser_budget"
    )
  }
  budget$rows <- budget$rows + product_rows

  rows <- list()
  for (distribution_number in seq_along(distributions)) {
    distribution <- gx_distribution_fields(distributions[[distribution_number]])
    media_type <- gx_normalize_media_type(distribution$media_type)
    canonical_url <- NA_character_
    handler <- "unknown"
    fetchable <- FALSE
    row_diagnostics <- diagnostics
    if (!is.null(distributions[[distribution_number]])) {
      row_diagnostics <- gx_bind_diagnostics(
        row_diagnostics,
        gx_conflict_diagnostics(
          distributions[[distribution_number]],
          list(
            distribution_url = c(gx_schema_terms("contentUrl"), gx_schema_terms("accessURL"), gx_schema_terms("url")),
            media_type = gx_schema_terms("encodingFormat")
          ),
          paste0(path, "/distribution/", distribution_number - 1L)
        )
      )
    }
    if (!is.na(distribution$url)) {
      safe <- tryCatch({
        gx_assert_safe_url(distribution$url, resolve_dns = FALSE)
        TRUE
      }, error = function(cnd) FALSE)
      if (safe) {
        canonical_url <- gx_canonical_url(distribution$url)
        handler <- tryCatch(
          gx_classify_distribution(
            canonical_url,
            if (is.na(media_type)) NULL else media_type,
            distribution$conforms_to
          ),
          error = function(cnd) {
            if (inherits(cnd, "gx_error_classifier")) "unknown" else stop(cnd)
          }
        )
        fetchable <- !identical(handler, "unknown")
      } else {
        row_diagnostics <- gx_bind_diagnostics(
          row_diagnostics,
          gx_diagnostic("warning", "unsafe_distribution_url", paste0(path, "/distribution/", distribution_number - 1L), "Distribution URL is not a safe absolute HTTP(S) target.")
        )
      }
    }
    distribution_id <- if (!is.na(id_result$id) && !is.na(canonical_url)) {
      gx_contract_hash(
        list(id_result$id, canonical_url, media_type),
        "geoconnexr.distribution-id.v1",
        .gx_jsonld_contract_version
      )
    } else {
      NA_character_
    }
    for (variable_number in seq_along(variables)) {
      variable <- gx_variable_fields(variables[[variable_number]])
      variable_uri <- gx_identity_iri(variable$uri)
      variable_id <- variable_uri
      if (is.na(variable_id) && !is.na(variable$name)) {
        provider_identity <- gx_identity_iri(provider$uri)
        if (is.na(provider_identity)) provider_identity <- gx_normalize_label(provider$name)
        label <- gx_normalize_label(variable$name)
        if (!is.na(provider_identity) && !is.na(label) && nzchar(label)) {
          variable_id <- gx_contract_hash(
            list(provider_identity, label),
            "geoconnexr.variable-id.v1",
            .gx_jsonld_contract_version
          )
        }
      }
      variable_diagnostics <- row_diagnostics
      if (!is.na(variable$uri) && is.na(variable_uri)) {
        variable_diagnostics <- gx_bind_diagnostics(
          variable_diagnostics,
          gx_diagnostic("warning", "invalid_variable_uri", paste0(path, "/variable/", variable_number - 1L), "Variable URI is malformed; fallback identity fields were used when available.")
        )
      }
      if (!is.null(variables[[variable_number]])) {
        variable_diagnostics <- gx_bind_diagnostics(
          variable_diagnostics,
          gx_conflict_diagnostics(
            variables[[variable_number]],
            list(
              variable_name = gx_schema_terms("name"),
              unit_label = gx_schema_terms("unitText"),
              measurement_technique = gx_schema_terms("measurementTechnique"),
              variable_mapping = paste0(.gx_skos, c("exactMatch", "broadMatch", "closeMatch"))
            ),
            paste0(path, "/variable/", variable_number - 1L)
          )
        )
      }
      if (!is.null(variables[[variable_number]]) && is.na(variable_id)) {
        variable_diagnostics <- gx_bind_diagnostics(
          variable_diagnostics,
          gx_diagnostic("warning", "variable_identity_incomplete", paste0(path, "/variable/", variable_number - 1L), "Variable lacks enough identity fields for a stable ID.")
        )
      }
      rows[[length(rows) + 1L]] <- list(
        contract_version = .gx_jsonld_contract_version,
        site_uri = as.character(site[["@id"]] %||% NA_character_),
        dataset_id = id_result$id,
        distribution_id = distribution_id,
        variable_id = variable_id,
        dataset_uri = dataset_uri,
        dataset_name = dataset_name,
        dataset_description = gx_first_text(gx_prop(dataset, gx_schema_terms("description"))),
        temporal_coverage = temporal_coverage,
        temporal_start = temporal$start,
        temporal_end = temporal$end,
        variable_uri = variable_uri,
        variable_name = variable$name,
        unit_uri = gx_identity_iri(variable$unit_uri),
        unit_label = variable$unit_label,
        measurement_technique = variable$technique,
        distribution_url = distribution$url,
        media_type = media_type,
        conforms_to = distribution$conforms_to,
        provider_uri = provider$uri,
        provider_name = provider$name,
        provider_url = provider$url,
        license = gx_first_text(gx_prop(dataset, gx_schema_terms("license")), include_id = TRUE),
        access_rights = gx_first_text(gx_prop(dataset, gx_schema_terms("accessRights")), include_id = TRUE),
        handler_id = handler,
        fetchable = fetchable,
        source_url = input$source_url,
        diagnostics = variable_diagnostics
      )
    }
  }
  rows
}

gx_empty_datasets <- function(diagnostics = gx_empty_diagnostics()) {
  out <- tibble::tibble(
    contract_version = character(), site_uri = character(), dataset_id = character(),
    distribution_id = character(), variable_id = character(), dataset_uri = character(),
    dataset_name = character(), dataset_description = character(), temporal_coverage = character(),
    temporal_start = as.POSIXct(character(), tz = "UTC"), temporal_end = as.POSIXct(character(), tz = "UTC"),
    variable_uri = character(), variable_name = character(), unit_uri = character(),
    unit_label = character(), measurement_technique = character(), distribution_url = character(),
    media_type = character(), conforms_to = list(), provider_uri = character(),
    provider_name = character(), provider_url = character(), license = character(),
    access_rights = character(), handler_id = character(), fetchable = logical(),
    source_url = character(), diagnostics = list()
  )
  gx_new_datasets(out, diagnostics)
}

#' Parse datasets described by Geoconnex location profiles
#'
#' Produces one row per dataset by distribution by variable. A missing
#' distribution or variable contributes one explicit `NA` dimension so an
#' incomplete real-world dataset is not silently discarded.
#'
#' @inheritParams gx_parse_location
#'
#' @return A `gx_datasets` tibble with stable identifiers, classifier facts,
#'   provenance, and structured diagnostics. Its exact columns are
#'   `contract_version`, `site_uri`, `dataset_id`, `distribution_id`,
#'   `variable_id`, `dataset_uri`, `dataset_name`, `dataset_description`,
#'   `temporal_coverage`, `temporal_start`, `temporal_end`, `variable_uri`,
#'   `variable_name`, `unit_uri`, `unit_label`, `measurement_technique`,
#'   `distribution_url`, `media_type`, `conforms_to`, `provider_uri`,
#'   `provider_name`, `provider_url`, `license`, `access_rights`, `handler_id`,
#'   `fetchable`, `source_url`, and `diagnostics`. Temporal columns are UTC
#'   `POSIXct`, `fetchable` is logical, `conforms_to` and `diagnostics` are list
#'   columns, and all others are character. Zero-row results preserve types.
#'
#' Dataset identifiers follow the installed language-neutral identity contract.
#' Absolute HTTP(S) and opaque IRIs preserve semantic fragments; label fallback
#' uses UTF-8 NFC and Unicode default case folding. Unsafe distribution URLs are
#' retained as provenance but never marked fetchable. `gx_error_parser_budget`
#' is raised before a distribution-by-variable product exceeds the configured
#' row ceiling. Diagnostics and strict-mode behavior match [gx_parse_location()].
#' @export
gx_parse_datasets <- function(x, strict = FALSE) {
  input <- gx_profile_input(x)
  nodes <- gx_merge_graph_nodes(gx_flatten_graph(input$expanded))
  locations <- nodes[gx_location_candidates(nodes)]
  if (!length(locations)) {
    input$diagnostics <- gx_bind_diagnostics(
      input$diagnostics,
      gx_diagnostic("warning", "no_location_node", "", "Document contains no supported monitoring-location node.")
    )
  }
  index <- gx_node_index(input$expanded)
  budget <- new.env(parent = emptyenv())
  budget$rows <- 0
  budget$max_rows <- gx_scalar_number(
    getOption("geoconnexr.max_dataset_rows", 10000L),
    "geoconnexr.max_dataset_rows",
    minimum = 1,
    maximum = .Machine$integer.max,
    integer = TRUE
  )
  rows <- list()
  dataset_number <- 0L
  for (site in locations) {
    datasets <- gx_dereference(gx_prop(site, gx_schema_terms("subjectOf")), index)
    for (dataset in datasets) {
      dataset_number <- dataset_number + 1L
      rows <- c(rows, gx_dataset_rows(dataset, site, input, index, dataset_number, budget))
    }
  }
  if (!length(rows)) {
    if (length(locations)) {
      input$diagnostics <- gx_bind_diagnostics(
        input$diagnostics,
        gx_diagnostic("warning", "no_dataset_node", "", "Location profile describes no dataset through subjectOf.")
      )
    }
    gx_strict_diagnostics(input$diagnostics, strict, "dataset")
    return(gx_empty_datasets(input$diagnostics))
  }
  char_field <- function(name) vapply(rows, `[[`, character(1), name)
  out <- tibble::tibble(
    contract_version = char_field("contract_version"), site_uri = char_field("site_uri"),
    dataset_id = char_field("dataset_id"), distribution_id = char_field("distribution_id"),
    variable_id = char_field("variable_id"), dataset_uri = char_field("dataset_uri"),
    dataset_name = char_field("dataset_name"), dataset_description = char_field("dataset_description"),
    temporal_coverage = char_field("temporal_coverage"),
    temporal_start = do.call(c, lapply(rows, `[[`, "temporal_start")),
    temporal_end = do.call(c, lapply(rows, `[[`, "temporal_end")),
    variable_uri = char_field("variable_uri"), variable_name = char_field("variable_name"),
    unit_uri = char_field("unit_uri"), unit_label = char_field("unit_label"),
    measurement_technique = char_field("measurement_technique"),
    distribution_url = char_field("distribution_url"), media_type = char_field("media_type"),
    conforms_to = unname(lapply(rows, `[[`, "conforms_to")),
    provider_uri = char_field("provider_uri"), provider_name = char_field("provider_name"),
    provider_url = char_field("provider_url"), license = char_field("license"),
    access_rights = char_field("access_rights"), handler_id = char_field("handler_id"),
    fetchable = vapply(rows, `[[`, logical(1), "fetchable"), source_url = char_field("source_url"),
    diagnostics = unname(lapply(rows, `[[`, "diagnostics"))
  )
  diagnostics <- unique(gx_bind_diagnostics(
    input$diagnostics,
    do.call(gx_bind_diagnostics, lapply(rows, `[[`, "diagnostics"))
  ))
  out <- gx_new_datasets(out, diagnostics)
  gx_strict_diagnostics(diagnostics, strict, "dataset")
  out
}

#' @export
print.gx_datasets <- function(x, ...) {
  cli::cli_inform("<gx_datasets> {nrow(x)} row{?s}; {nrow(attr(x, 'diagnostics'))} diagnostic{?s}")
  print(gx_profile_print_data(x, "gx_datasets"), ...)
  invisible(x)
}
