.gx_publisher_profile_version <- "1.0.0"
.gx_publisher_max_sitemap_urls <- 50000L
.gx_publisher_max_sitemap_bytes <- as.integer(50L * 1024L^2)
.gx_publisher_profile_iri <-
  "https://ksonda.github.io/geoconnexr/profiles/publisher-v1"
.gx_publisher_vocab <- "https://ksonda.github.io/geoconnexr/vocab/"
.gx_publisher_allowed_location_literals <- c("Unknown", "hydrometricStation")
.gx_publisher_profile_cache <- new.env(parent = emptyenv())

gx_publisher_abort <- function(message, subclass = "gx_error_publisher_input",
                               call = rlang::caller_env()) {
  gx_abort(
    message,
    c(subclass, "gx_error_publisher"),
    call = call,
    .redact_trace = TRUE
  )
}

gx_publisher_profile_impl <- function() {
  if (exists("profile", envir = .gx_publisher_profile_cache, inherits = FALSE)) {
    return(get("profile", envir = .gx_publisher_profile_cache, inherits = FALSE))
  }
  path <- file.path(gx_asset_dir("jsonld"), "publisher-profile-v1.json")
  profile <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(cnd) NULL
  )
  expected_names <- c(
    "profile_version", "profile_iri", "input_contract", "context",
    "approved_location_type_literals", "validation_finding_columns",
    "sitemap"
  )
  finding_columns <- c(
    "severity", "json_pointer", "rule_id", "profile_version", "message",
    "suggested_fix"
  )
  literals <- if (is.list(profile$approved_location_type_literals)) {
    unlist(profile$approved_location_type_literals, use.names = FALSE)
  } else {
    NULL
  }
  columns <- if (is.list(profile$validation_finding_columns)) {
    unlist(profile$validation_finding_columns, use.names = FALSE)
  } else {
    NULL
  }
  valid <- is.list(profile) && identical(names(profile), expected_names) &&
    identical(profile$profile_version, .gx_publisher_profile_version) &&
    identical(profile$profile_iri, .gx_publisher_profile_iri) &&
    identical(profile$input_contract, "geoconnexr.catalog/0.1.0") &&
    identical(profile$context, list(
      schema = .gx_schema_https,
      hyf = .gx_hyf,
      geo = .gx_gsp,
      gx = .gx_publisher_vocab
    )) &&
    identical(literals, .gx_publisher_allowed_location_literals) &&
    identical(columns, finding_columns) &&
    is.list(profile$sitemap) &&
    identical(names(profile$sitemap), c(
      "max_urls", "max_uncompressed_bytes", "max_uri_bytes"
    )) &&
    identical(profile$sitemap$max_urls, .gx_publisher_max_sitemap_urls) &&
    identical(
      profile$sitemap$max_uncompressed_bytes,
      .gx_publisher_max_sitemap_bytes
    ) &&
    identical(profile$sitemap$max_uri_bytes, 2047L)
  if (!isTRUE(valid)) {
    gx_publisher_abort(
      "The bundled publisher profile asset is invalid.",
      "gx_error_publisher_asset"
    )
  }
  profile$approved_location_type_literals <- literals
  profile$validation_finding_columns <- columns
  assign("profile", profile, envir = .gx_publisher_profile_cache)
  profile
}

#' Return the Geoconnex publisher JSON-LD context
#'
#' The context is a local, network-free mapping for publisher profile 1.0.0.
#' A fresh copy is returned on every call.
#'
#' @return A named JSON-LD context list.
#' @export
gx_context <- function() {
  unserialize(serialize(gx_publisher_profile_impl()$context, NULL))
}

gx_publisher_provider_impl <- function(provider) {
  if (!is.list(provider) || !identical(names(provider), c("uri", "name", "url")) ||
      !all(lengths(provider) == 1L) ||
      !all(vapply(provider, is.character, logical(1))) ||
      anyNA(unlist(provider, use.names = FALSE)) ||
      !all(nzchar(unlist(provider, use.names = FALSE)))) {
    gx_publisher_abort(
      "{.arg provider} must be a list with nonempty character fields {.field uri}, {.field name}, and {.field url}."
    )
  }
  uri <- gx_identity_iri(provider$uri)
  url <- gx_identity_iri(provider$url)
  if (is.na(uri) || !identical(uri, provider$uri) ||
      is.na(url) || !identical(url, provider$url) ||
      !grepl("^https?://", url, ignore.case = TRUE) ||
      !gx_catalog_text_valid(provider$name, allow_na = FALSE, nonempty = TRUE)) {
    gx_publisher_abort(
      "Publisher provider fields must contain a canonical IRI, a safe name, and a canonical HTTP(S) URL."
    )
  }
  unclass(provider)
}

gx_publisher_assert_provider_alignment <- function(x, provider, label) {
  fields <- c(uri = "provider_uri", name = "provider_name", url = "provider_url")
  for (name in names(fields)) {
    values <- x[[fields[[name]]]]
    present <- !is.na(values)
    if (any(present & values != provider[[name]])) {
      gx_publisher_abort(
        "{label} contain provider fields that disagree with {.arg provider}.",
        "gx_error_publisher_provider"
      )
    }
  }
  invisible(x)
}

gx_publisher_context_impl <- function(context) {
  if (!is.list(context) || is.null(names(context)) || anyNA(names(context)) ||
      any(!nzchar(names(context))) || anyDuplicated(names(context)) ||
      any(names(context) %in% c("@base", "@import", "@vocab"))) {
    gx_publisher_abort(
      "{.arg context} must be a named, local JSON-LD context without base, import, or default-vocabulary directives."
    )
  }
  measured <- gx_json_measure_complexity(
    context,
    max_depth = 16L,
    max_members = 128L,
    max_atomic_bytes = 16L * 1024L
  )
  if (!is.na(measured$exceeded) || !gx_context_asset_is_safe(context)) {
    gx_publisher_abort(
      "{.arg context} exceeds the publisher profile's local context limits."
    )
  }
  required <- gx_context()
  if (!all(names(required) %in% names(context)) ||
      any(vapply(names(required), function(name) {
        !identical(context[[name]], required[[name]])
      }, logical(1)))) {
    gx_publisher_abort(
      "{.arg context} must retain the profile's schema, hyf, geo, and gx prefix mappings."
    )
  }
  context
}

gx_publisher_value <- function(value) {
  if (length(value) != 1L || is.na(value)) NULL else unname(value)
}

gx_publisher_provider_node <- function(provider) {
  list(
    `@id` = provider$uri,
    `schema:name` = provider$name,
    `schema:url` = provider$url
  )
}

gx_publisher_assert_constant <- function(rows, columns, label) {
  for (column in columns) {
    values <- rows[[column]]
    keys <- if (is.list(values)) {
      vapply(values, gx_json_serialize, character(1))
    } else if (inherits(values, "POSIXct")) {
      ifelse(is.na(values), "<NA>", format(values, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
    } else {
      ifelse(is.na(values), "<NA>", enc2utf8(as.character(values)))
    }
    if (length(unique(keys)) > 1L) {
      gx_publisher_abort(
        "{label} disagree in {.field {column}}.",
        "gx_error_publisher_cardinality"
      )
    }
  }
  invisible(rows)
}

gx_publisher_group_key <- function(rows, id, fallback) {
  value <- rows[[id]]
  ifelse(
    is.na(value),
    vapply(seq_len(nrow(rows)), function(i) {
      gx_contract_hash(
        lapply(fallback, function(column) rows[[column]][[i]]),
        paste0("geoconnexr.publisher.", id, ".fallback.v1"),
        .gx_publisher_profile_version
      )
    }, character(1)),
    value
  )
}

gx_publisher_distribution_node <- function(rows) {
  gx_publisher_assert_constant(
    rows,
    c("distribution_url", "media_type", "conforms_to"),
    "Rows for one distribution"
  )
  row <- rows[1L, , drop = FALSE]
  node <- list(`@type` = "schema:DataDownload")
  url <- gx_publisher_value(row$distribution_url[[1L]])
  media_type <- gx_publisher_value(row$media_type[[1L]])
  if (!is.null(url)) node[["schema:contentUrl"]] <- url
  if (!is.null(media_type)) node[["schema:encodingFormat"]] <- media_type
  conforms <- row$conforms_to[[1L]]
  if (length(conforms)) node[["schema:conformsTo"]] <- as.list(conforms)
  node
}

gx_publisher_variable_node <- function(rows) {
  gx_publisher_assert_constant(
    rows,
    c("variable_id", "variable_uri", "variable_name", "unit_uri",
      "unit_label", "measurement_technique"),
    "Rows for one variable"
  )
  row <- rows[1L, , drop = FALSE]
  node <- list()
  variable_uri <- gx_publisher_value(row$variable_uri[[1L]])
  variable_id <- gx_publisher_value(row$variable_id[[1L]])
  if (is.null(variable_uri) && !is.null(variable_id) &&
      !is.na(gx_identity_iri(variable_id))) {
    variable_uri <- variable_id
  }
  if (!is.null(variable_uri)) node[["@id"]] <- variable_uri
  fields <- c(
    "schema:name" = "variable_name",
    "schema:unitCode" = "unit_uri",
    "schema:unitText" = "unit_label",
    "schema:measurementTechnique" = "measurement_technique"
  )
  for (term in names(fields)) {
    value <- gx_publisher_value(row[[fields[[term]]]][[1L]])
    if (!is.null(value)) node[[term]] <- value
  }
  node
}

gx_publisher_dataset_node <- function(rows, provider) {
  invariant <- c(
    "site_uri", "dataset_id", "dataset_uri", "dataset_name",
    "dataset_description", "temporal_coverage", "temporal_start",
    "temporal_end", "provider_uri", "provider_name", "provider_url",
    "license", "access_rights"
  )
  gx_publisher_assert_constant(rows, invariant, "Rows for one dataset")

  distribution_key <- gx_publisher_group_key(
    rows, "distribution_id", c("distribution_url", "media_type")
  )
  variable_key <- gx_publisher_group_key(
    rows, "variable_id", c("variable_uri", "variable_name")
  )
  observed <- unique(paste(distribution_key, variable_key, sep = "\r"))
  expected <- as.vector(outer(
    unique(distribution_key), unique(variable_key), paste, sep = "\r"
  ))
  if (!setequal(observed, expected)) {
    gx_publisher_abort(
      "Rows for one dataset must form a complete distribution by variable product.",
      "gx_error_publisher_cardinality"
    )
  }

  row <- rows[1L, , drop = FALSE]
  node <- list(`@type` = "schema:Dataset")
  dataset_uri <- gx_publisher_value(row$dataset_uri[[1L]])
  if (!is.null(dataset_uri)) node[["@id"]] <- dataset_uri
  fields <- c(
    "schema:name" = "dataset_name",
    "schema:description" = "dataset_description",
    "schema:temporalCoverage" = "temporal_coverage",
    "schema:license" = "license",
    "schema:accessRights" = "access_rights"
  )
  for (term in names(fields)) {
    value <- gx_publisher_value(row[[fields[[term]]]][[1L]])
    if (!is.null(value)) node[[term]] <- value
  }
  node[["schema:provider"]] <- gx_publisher_provider_node(provider)
  distribution_order <- gx_catalog_byte_order(unique(distribution_key))
  distribution_ids <- unique(distribution_key)[distribution_order]
  node[["schema:distribution"]] <- lapply(distribution_ids, function(key) {
    gx_publisher_distribution_node(rows[distribution_key == key, , drop = FALSE])
  })
  variable_order <- gx_catalog_byte_order(unique(variable_key))
  variable_ids <- unique(variable_key)[variable_order]
  node[["schema:variableMeasured"]] <- lapply(variable_ids, function(key) {
    gx_publisher_variable_node(rows[variable_key == key, , drop = FALSE])
  })
  node
}

gx_publisher_site_node <- function(site, datasets, provider) {
  location_type <- site$site_type[[1L]]
  node_types <- "hyf:HY_HydrometricFeature"
  node <- list(`@id` = site$site_uri[[1L]])
  if (!is.na(location_type)) {
    canonical_type <- gx_identity_iri(location_type)
    if (!is.na(canonical_type) && identical(canonical_type, location_type)) {
      node_types <- c(node_types, location_type)
    } else if (location_type %in%
               gx_publisher_profile_impl()$approved_location_type_literals) {
      node[["hyf:HY_HydroLocationType"]] <- location_type
    } else {
      gx_publisher_abort(
        "A site location type is neither a canonical IRI nor a profile-approved literal.",
        "gx_error_publisher_location_type"
      )
    }
  }
  node[["@type"]] <- as.list(node_types)
  for (mapping in list(
    c("schema:name", "name"),
    c("schema:description", "description")
  )) {
    value <- gx_publisher_value(site[[mapping[[2L]]]][[1L]])
    if (!is.null(value)) node[[mapping[[1L]]]] <- value
  }
  node[["schema:provider"]] <- gx_publisher_provider_node(provider)

  geometry <- sf::st_geometry(site)[[1L]]
  if (!isTRUE(sf::st_is_empty(geometry))) {
    node[["geo:hasGeometry"]] <- list(
      `@type` = "geo:Geometry",
      `geo:asWKT` = sf::st_as_text(geometry, digits = 15),
      `geo:crs` = "http://www.opengis.net/def/crs/OGC/1.3/CRS84"
    )
  }
  mainstem <- gx_publisher_value(site$mainstem_uri[[1L]])
  if (!is.null(mainstem)) {
    node[["hyf:referencedPosition"]] <- list(
      `@type` = "hyf:HY_IndirectPosition",
      `hyf:linearElement` = list(`@id` = mainstem)
    )
  }
  if (nrow(datasets)) {
    dataset_key <- gx_publisher_group_key(
      datasets, "dataset_id", c("site_uri", "dataset_uri", "dataset_name")
    )
    keys <- unique(dataset_key)
    keys <- keys[gx_catalog_byte_order(keys)]
    node[["schema:subjectOf"]] <- lapply(keys, function(key) {
      gx_publisher_dataset_node(datasets[dataset_key == key, , drop = FALSE], provider)
    })
  }
  node
}

#' Build a versioned Geoconnex publisher profile
#'
#' Builds deterministic JSON-LD from the exact site and dataset tables owned by
#' the catalog 0.1.0 contract. The function performs no network access. Dataset
#' rows for one dataset must form a complete distribution by variable product,
#' because JSON-LD represents those two dimensions independently.
#'
#' @param sites An exact catalog 0.1.0 `sf` sites table.
#' @param datasets `NULL` or an exact catalog 0.1.0 datasets table whose site
#'   identities are present in `sites`.
#' @param provider A list with exact fields `uri`, `name`, and `url`. Present
#'   provider values in the input tables must agree with this publisher.
#' @param context A local JSON-LD context. It must retain the mappings returned
#'   by `gx_context()`.
#'
#' @return A parsed JSON-LD document with profile version 1.0.0.
#' @export
gx_jsonld_build <- function(sites, datasets = NULL, provider,
                            context = gx_context()) {
  gx_catalog_validate_sites(sites)
  if (is.null(datasets)) datasets <- gx_catalog_empty_datasets()
  gx_catalog_validate_datasets(datasets, sites)
  if (!nrow(sites)) {
    gx_publisher_abort("{.arg sites} must contain at least one site.")
  }
  provider <- gx_publisher_provider_impl(provider)
  context <- gx_publisher_context_impl(context)
  gx_publisher_assert_provider_alignment(sites, provider, "Site rows")
  gx_publisher_assert_provider_alignment(datasets, provider, "Dataset rows")

  site_order <- gx_catalog_byte_order(sites$site_uri)
  graph <- lapply(site_order, function(i) {
    site_datasets <- datasets[datasets$site_uri == sites$site_uri[[i]], , drop = FALSE]
    gx_publisher_site_node(sites[i, , drop = FALSE], site_datasets, provider)
  })
  document <- list(
    `@context` = context,
    `gx:profile` = .gx_publisher_profile_iri,
    `gx:profileVersion` = .gx_publisher_profile_version,
    `@graph` = graph
  )
  gx_json_assert_complexity(document, max_bytes = .gx_catalog_max_text_bytes)
  validation <- gx_jsonld_validate(document)
  if (!isTRUE(attr(validation, "valid"))) {
    gx_publisher_abort(
      "The generated publisher profile failed its own validation contract.",
      "gx_error_publisher_internal"
    )
  }
  document
}

gx_publisher_empty_findings <- function() {
  out <- tibble::tibble(
    severity = character(),
    json_pointer = character(),
    rule_id = character(),
    profile_version = character(),
    message = character(),
    suggested_fix = character()
  )
  class(out) <- c("gx_jsonld_validation", class(out))
  attr(out, "valid") <- TRUE
  out
}

gx_publisher_finding <- function(severity, pointer, rule_id, message, fix) {
  tibble::tibble(
    severity = severity,
    json_pointer = pointer,
    rule_id = rule_id,
    profile_version = .gx_publisher_profile_version,
    message = message,
    suggested_fix = fix
  )
}

gx_publisher_finish_findings <- function(findings) {
  if (!length(findings)) return(gx_publisher_empty_findings())
  out <- do.call(rbind, findings)
  out <- unique(out)
  rank <- match(out$severity, c("error", "warning", "info"))
  out <- out[order(rank, out$json_pointer, out$rule_id, method = "radix"), , drop = FALSE]
  row.names(out) <- NULL
  class(out) <- c("gx_jsonld_validation", class(out))
  attr(out, "valid") <- !any(out$severity == "error")
  out
}

gx_publisher_source_impl <- function(x) {
  if (inherits(x, "gx_jsonld")) return(x$source_document)
  if (is.raw(x)) return(gx_json_parse(gx_json_text(x)))
  if (is.character(x) && length(x) == 1L && !is.na(x)) return(gx_json_parse(x))
  if (is.list(x)) return(x)
  gx_publisher_abort(
    "{.arg x} must be a gx_jsonld object, JSON string, raw JSON, or parsed JSON-LD list."
  )
}

gx_publisher_profile_value <- function(source) {
  direct <- source[["gx:profileVersion"]]
  if (!is.null(direct)) return(gx_first_text(direct))
  gx_first_text(gx_find_property(
    source, paste0(.gx_publisher_vocab, "profileVersion")
  ))
}

gx_publisher_profile_iri_value <- function(source) {
  direct <- source[["gx:profile"]]
  if (!is.null(direct)) return(gx_first_text(direct, include_id = TRUE))
  gx_first_text(gx_find_property(
    source, paste0(.gx_publisher_vocab, "profile")
  ), include_id = TRUE)
}

#' Validate a Geoconnex publisher JSON-LD profile
#'
#' Validation is local and bounded. Findings use stable rule identifiers and
#' JSON pointers. Warnings do not make a document invalid; error findings do.
#'
#' @param x A [gx_jsonld()] object, JSON string, raw JSON, or parsed JSON-LD
#'   list.
#'
#' @return A `gx_jsonld_validation` tibble with `severity`, `json_pointer`,
#'   `rule_id`, `profile_version`, `message`, and `suggested_fix`. Its `valid`
#'   attribute is true when no error finding is present.
#' @export
gx_jsonld_validate <- function(x) {
  findings <- list()
  add <- function(severity, pointer, rule_id, message, fix) {
    findings[[length(findings) + 1L]] <<- gx_publisher_finding(
      severity, pointer, rule_id, message, fix
    )
  }
  source <- tryCatch(
    gx_publisher_source_impl(x),
    error = function(cnd) cnd
  )
  if (inherits(source, "condition")) {
    add(
      "error", "", "document.parse",
      "The input could not be parsed as bounded JSON-LD.",
      "Provide one UTF-8 JSON-LD object within the configured parser limits."
    )
    return(gx_publisher_finish_findings(findings))
  }
  version <- gx_publisher_profile_value(source)
  if (!identical(version, .gx_publisher_profile_version)) {
    add(
      "error", "/gx:profileVersion", "profile.version",
      "The document does not declare publisher profile version 1.0.0.",
      "Set gx:profileVersion to 1.0.0 and retain the gx prefix mapping."
    )
  }
  profile <- gx_publisher_profile_iri_value(source)
  if (!identical(profile, .gx_publisher_profile_iri)) {
    add(
      "error", "/gx:profile", "profile.identifier",
      "The document does not declare the publisher-v1 profile IRI.",
      paste0("Set gx:profile to ", .gx_publisher_profile_iri, ".")
    )
  }

  parsed <- tryCatch(
    list(
      locations = gx_parse_location(source),
      datasets = gx_parse_datasets(source)
    ),
    error = function(cnd) cnd
  )
  if (inherits(parsed, "condition")) {
    add(
      "error", "", "document.jsonld",
      "The document could not be expanded and parsed under the local JSON-LD safety policy.",
      "Use a local context based on gx_context() and remove unsupported remote context references."
    )
    return(gx_publisher_finish_findings(findings))
  }
  locations <- parsed$locations
  datasets <- parsed$datasets
  diagnostics <- unique(gx_bind_diagnostics(
    attr(locations, "diagnostics"), attr(datasets, "diagnostics")
  ))
  ignored_codes <- c("literal_location_type", "no_dataset_node")
  diagnostics <- diagnostics[!diagnostics$code %in% ignored_codes, , drop = FALSE]
  if (nrow(diagnostics)) {
    for (i in seq_len(nrow(diagnostics))) {
      severity <- if (diagnostics$severity[[i]] == "error") "error" else "warning"
      add(
        severity,
        diagnostics$path[[i]],
        paste0("parser.", diagnostics$code[[i]]),
        diagnostics$message[[i]],
        "Correct the referenced profile value or remove the unsupported value."
      )
    }
  }
  if (!nrow(locations)) {
    add(
      "error", "/@graph", "site.required",
      "The profile contains no supported monitoring location.",
      "Add at least one hydrometric feature with a canonical @id."
    )
    return(gx_publisher_finish_findings(findings))
  }
  for (i in seq_len(nrow(locations))) {
    pointer <- paste0("/@graph/", i - 1L)
    site_uri <- gx_identity_iri(locations$site_uri[[i]])
    if (is.na(site_uri) || !identical(site_uri, locations$site_uri[[i]])) {
      add(
        "error", paste0(pointer, "/@id"), "site.identifier",
        "A site does not have a canonical identity IRI.",
        "Set @id to one canonical absolute IRI."
      )
    }
    location_type <- locations$site_type[[i]]
    type_is_iri <- !is.na(location_type) &&
      identical(gx_identity_iri(location_type), location_type)
    type_is_literal <- !is.na(location_type) &&
      location_type %in% .gx_publisher_allowed_location_literals
    if (is.na(location_type)) {
      add(
        "warning", paste0(pointer, "/@type"), "site.location_type_missing",
        "The site does not declare a hydro-location type.",
        "Add a canonical location-type IRI or a profile-approved literal."
      )
    } else if (!type_is_iri && !type_is_literal) {
      add(
        "error", paste0(pointer, "/hyf:HY_HydroLocationType"),
        "site.location_type",
        "The site location type is not a canonical IRI or an approved literal.",
        "Use a canonical location-type IRI, hydrometricStation, or Unknown."
      )
    }
    if (is.na(locations$provider_uri[[i]])) {
      add(
        "error", paste0(pointer, "/schema:provider"), "site.provider",
        "The site does not identify its provider.",
        "Add a provider object with a canonical @id, name, and URL."
      )
    }
    if (is.na(locations$provider_name[[i]])) {
      add(
        "error", paste0(pointer, "/schema:provider/schema:name"),
        "site.provider_name",
        "The site provider does not have a name.",
        "Add a nonempty schema:name to the provider object."
      )
    }
    if (is.na(locations$provider_url[[i]])) {
      add(
        "error", paste0(pointer, "/schema:provider/schema:url"),
        "site.provider_url",
        "The site provider does not have a canonical URL.",
        "Add a canonical HTTP(S) schema:url to the provider object."
      )
    }
    if (is.na(locations$name[[i]])) {
      add(
        "warning", paste0(pointer, "/schema:name"), "site.name",
        "The site has no name.",
        "Add a concise schema:name value."
      )
    }
    wkt <- locations$geometry_wkt[[i]]
    if (!is.na(wkt)) {
      geometry_ok <- tryCatch({
        geometry <- sf::st_as_sfc(wkt, crs = gx_aoi_crs)
        inherits(geometry, "sfc_POINT") && !sf::st_is_empty(geometry)[[1L]]
      }, error = function(cnd) FALSE)
      if (!isTRUE(geometry_ok)) {
        add(
          "error", paste0(pointer, "/geo:hasGeometry/geo:asWKT"),
          "site.geometry",
          "Site geometry is not a nonempty WKT point.",
          "Publish one POINT geometry in OGC:CRS84."
        )
      }
    }
  }
  if (nrow(datasets)) {
    missing_distribution <- is.na(datasets$distribution_url)
    for (i in which(missing_distribution)) {
      add(
        "warning", paste0("/dataset/", i - 1L, "/schema:distribution"),
        "dataset.distribution",
        "A dataset row has no distribution URL.",
        "Add a distribution with a canonical HTTP(S) content URL."
      )
    }
    missing_variable <- is.na(datasets$variable_id)
    for (i in which(missing_variable)) {
      add(
        "warning", paste0("/dataset/", i - 1L, "/schema:variableMeasured"),
        "dataset.variable",
        "A dataset row has no stable variable identity.",
        "Add a canonical variable IRI or enough provider and label data for a stable fallback identity."
      )
    }
  }
  gx_publisher_finish_findings(findings)
}

#' @export
print.gx_jsonld_validation <- function(x, ...) {
  cli::cli_inform(
    "<gx_jsonld_validation> {nrow(x)} finding{?s}; valid: {isTRUE(attr(x, 'valid'))}"
  )
  print(structure(x, class = setdiff(class(x), "gx_jsonld_validation")), ...)
  invisible(x)
}

gx_publisher_sitemap_bytes <- function(uris) {
  root <- xml2::xml_new_root(
    "urlset",
    xmlns = "http://www.sitemaps.org/schemas/sitemap/0.9"
  )
  for (uri in uris) {
    entry <- xml2::xml_add_child(root, "url")
    xml2::xml_add_child(entry, "loc", uri)
  }
  charToRaw(as.character(root))
}

gx_publisher_sitemap_validate_uris <- function(uris) {
  limits <- gx_publisher_profile_impl()$sitemap
  if (!is.character(uris) || is.object(uris) || !length(uris) || anyNA(uris) ||
      any(!nzchar(uris)) || anyDuplicated(uris) ||
      length(uris) > limits$max_urls) {
    gx_publisher_abort(
      "{.arg uris} must contain 1 to 50,000 unique, non-missing character values."
    )
  }
  valid <- vapply(uris, function(uri) {
    canonical <- gx_identity_iri(uri)
    !is.na(canonical) && identical(canonical, uri) &&
      grepl("^https?://", uri, ignore.case = TRUE) &&
      nchar(enc2utf8(uri), type = "bytes") <= limits$max_uri_bytes
  }, logical(1))
  if (!all(valid)) {
    gx_publisher_abort("Every sitemap URI must be a canonical HTTP(S) URI.")
  }
  uris[gx_catalog_byte_order(uris)]
}

#' Write a bounded XML sitemap
#'
#' Writes one deterministic `sitemap.xml` through a private sibling staging
#' directory and publishes it only to an absent destination directory.
#'
#' @param uris One to 50,000 unique canonical HTTP(S) URIs.
#' @param dir A new destination directory whose parent already exists.
#'
#' @return A `gx_sitemap` object with the normalized directory and file paths,
#'   URL count, byte size, and SHA-256 digest.
#' @export
gx_sitemap <- function(uris, dir) {
  uris <- gx_publisher_sitemap_validate_uris(uris)
  bytes <- gx_publisher_sitemap_bytes(uris)
  if (length(bytes) > gx_publisher_profile_impl()$sitemap$max_uncompressed_bytes) {
    gx_publisher_abort(
      "The sitemap exceeds the 50 MiB uncompressed byte limit.",
      "gx_error_publisher_budget"
    )
  }
  destination <- tryCatch(
    gx_snapshot_writer_scalar_path(dir),
    error = function(cnd) cnd
  )
  if (inherits(destination, "condition")) {
    gx_publisher_abort(
      "The sitemap destination path is invalid.",
      "gx_error_publisher_path"
    )
  }
  if (gx_snapshot_writer_entry_exists(destination$target)) {
    gx_publisher_abort(
      "The sitemap destination already exists; overwrite is not supported.",
      "gx_error_publisher_path"
    )
  }
  stage <- tempfile(pattern = ".gx-sitemap-stage-", tmpdir = destination$parent)
  cleanup <- TRUE
  on.exit({
    if (cleanup && gx_snapshot_writer_entry_exists(stage)) {
      try(gx_snapshot_writer_cleanup_stage(stage), silent = TRUE)
    }
  }, add = TRUE)
  if (!dir.create(stage, mode = "0700", showWarnings = FALSE)) {
    gx_publisher_abort(
      "The sitemap staging directory could not be created.",
      "gx_error_publisher_io"
    )
  }
  staged_file <- file.path(stage, "sitemap.xml")
  tryCatch(
    gx_snapshot_writer_write_raw(staged_file, bytes),
    error = function(cnd) gx_publisher_abort(
      "The staged sitemap could not be written.",
      "gx_error_publisher_io"
    )
  )
  staged_hash <- digest::digest(
    file = staged_file, algo = "sha256", serialize = FALSE
  )
  if (!identical(readBin(staged_file, "raw", n = length(bytes)), bytes)) {
    gx_publisher_abort(
      "The staged sitemap failed byte verification.",
      "gx_error_publisher_io"
    )
  }
  destination$parent_info <- gx_snapshot_assert_fs_type(
    destination$parent, "directory"
  )
  if (gx_snapshot_writer_entry_exists(destination$target) ||
      !isTRUE(gx_snapshot_writer_rename(stage, destination$target))) {
    gx_publisher_abort(
      "The sitemap destination changed or could not be exposed atomically.",
      "gx_error_publisher_io"
    )
  }
  cleanup <- FALSE
  final_file <- file.path(destination$target, "sitemap.xml")
  final_hash <- digest::digest(file = final_file, algo = "sha256", serialize = FALSE)
  final_info <- gx_snapshot_assert_fs_type(final_file, "file")
  if (!identical(final_hash, staged_hash) ||
      !identical(as.double(final_info$size[[1L]]), as.double(length(bytes)))) {
    gx_publisher_abort(
      "The published sitemap failed final verification.",
      "gx_error_publisher_io"
    )
  }
  path <- normalizePath(destination$target, winslash = "/", mustWork = TRUE)
  structure(
    list(
      contract_version = .gx_publisher_profile_version,
      path = path,
      file = file.path(path, "sitemap.xml"),
      url_count = as.integer(length(uris)),
      bytes = as.double(length(bytes)),
      sha256 = final_hash
    ),
    class = "gx_sitemap"
  )
}

#' @export
print.gx_sitemap <- function(x, ...) {
  cli::cli_inform(c(
    "<gx_sitemap>",
    "* URLs: {x$url_count}",
    "* Bytes: {x$bytes}",
    "* File: {.file {x$file}}"
  ))
  invisible(x)
}
