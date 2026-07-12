gx_asset_dir <- function(name) {
  path <- system.file(name, package = "geoconnexr")
  if (!nzchar(path)) {
    gx_abort(
      "Bundled asset directory {.file {name}} could not be located.",
      "gx_error_asset"
    )
  }
  path
}

gx_validate_predicate <- function(predicate, allowed_facts) {
  if (!is.list(predicate) ||
      !identical(sort(names(predicate)), sort(unique(names(predicate)))) ||
      !is.character(predicate$fact) || length(predicate$fact) != 1L ||
      !predicate$fact %in% allowed_facts ||
      !is.character(predicate$operator) || length(predicate$operator) != 1L) {
    gx_abort("Handler predicate has an invalid fact or operator.", "gx_error_asset")
  }
  operator <- predicate$operator
  scalar_operator <- identical(operator, "regex")
  vector_operator <- operator %in% c("scheme_in", "contains_any", "equals_any")
  if ((!scalar_operator && !vector_operator) ||
      (scalar_operator && (!identical(names(predicate), c("fact", "operator", "value")) ||
        !is.character(predicate$value) || length(predicate$value) != 1L)) ||
      (vector_operator && (!identical(names(predicate), c("fact", "operator", "values")) ||
        !is.character(predicate$values) || !length(predicate$values)))) {
    gx_abort("Handler predicate arguments do not match its operator.", "gx_error_asset")
  }
  invisible(predicate)
}

gx_validate_classifier <- function(classifier, allowed_facts) {
  if (!is.list(classifier) || !length(classifier)) {
    gx_abort("Handler classifier must be a non-empty mapping.", "gx_error_asset")
  }
  if (!is.null(classifier$always)) {
    if (!identical(names(classifier), "always") || !isTRUE(classifier$always)) {
      gx_abort("An always classifier cannot contain other predicates.", "gx_error_asset")
    }
    return(invisible(classifier))
  }
  if (any(!names(classifier) %in% c("all", "any"))) {
    gx_abort("Classifier supports only all/any predicate groups.", "gx_error_asset")
  }
  groups <- unname(classifier)
  if (any(!vapply(groups, is.list, logical(1))) || any(!lengths(groups))) {
    gx_abort("Classifier predicate groups cannot be empty.", "gx_error_asset")
  }
  invisible(lapply(unlist(groups, recursive = FALSE), gx_validate_predicate, allowed_facts))
}

#' List portable distribution classifiers
#'
#' Returns the language-neutral, first-match classifier registry. Runtime R
#' implementations are deliberately kept in a separate asset so another
#' language can reuse these facts without inheriting R package names.
#'
#' @return A tibble with one row per classifier, ordered by precedence.
#' @export
gx_handlers <- function() {
  path <- file.path(gx_asset_dir("handlers"), "registry.yml")
  registry <- yaml::read_yaml(path)
  handlers <- registry$handlers
  if (!is.list(handlers) || !length(handlers) ||
      !identical(registry$evaluation, "first_match_wins")) {
    gx_abort("Handler registry has an invalid top-level contract.", "gx_error_asset")
  }

  ids <- vapply(handlers, function(x) x$id %||% "", character(1))
  precedence <- vapply(handlers, function(x) as.numeric(x$precedence %||% NA), numeric(1))
  if (any(!nzchar(ids)) || anyDuplicated(ids) || any(!is.finite(precedence)) ||
      anyDuplicated(precedence)) {
    gx_abort("Handler IDs and precedence values must be unique.", "gx_error_asset")
  }
  ord <- order(precedence)
  handlers <- handlers[ord]
  ids <- ids[ord]
  precedence <- precedence[ord]
  if (!identical(ids[[length(ids)]], "unknown")) {
    gx_abort("The unknown classifier must be the final fallback.", "gx_error_asset")
  }
  allowed_facts <- unlist(registry$allowed_fact_names, use.names = FALSE)
  invisible(lapply(handlers, function(x) gx_validate_classifier(x$classifier, allowed_facts)))

  tibble::tibble(
    id = ids,
    precedence = as.integer(precedence),
    lifecycle = vapply(handlers, function(x) x$lifecycle %||% "active", character(1)),
    outcome = vapply(handlers, function(x) x$outcome %||% "fetch", character(1)),
    classifier = unname(lapply(handlers, `[[`, "classifier"))
  )
}

gx_predicate_matches <- function(predicate, facts) {
  value <- facts[[predicate$fact]]
  if (is.null(value) || !length(value) || all(is.na(value))) {
    return(FALSE)
  }
  switch(
    predicate$operator,
    scheme_in = {
      scheme <- sub(":.*$", "", as.character(value[[1]]))
      tolower(scheme) %in% tolower(unlist(predicate$values, use.names = FALSE))
    },
    contains_any = any(as.character(value) %in% unlist(predicate$values, use.names = FALSE)),
    equals_any = any(as.character(value) %in% unlist(predicate$values, use.names = FALSE)),
    regex = any(grepl(predicate$value, as.character(value), perl = TRUE)),
    FALSE
  )
}

gx_classifier_matches <- function(classifier, facts) {
  if (isTRUE(classifier$always)) {
    return(TRUE)
  }
  all_ok <- if (is.null(classifier$all)) TRUE else
    all(vapply(classifier$all, gx_predicate_matches, logical(1), facts = facts))
  any_ok <- if (is.null(classifier$any)) TRUE else
    any(vapply(classifier$any, gx_predicate_matches, logical(1), facts = facts))
  all_ok && any_ok
}

#' Classify a described distribution
#'
#' Applies the portable first-match classifier registry. Classification does
#' not fetch or trust the supplied URL; request safety is enforced separately
#' by the future fetch planner and transport layer.
#'
#' @param access_url One absolute HTTP(S) URL.
#' @param media_type Optional media type.
#' @param conforms_to Optional character vector of advertised conformance URIs.
#'
#' @return One handler ID.
#' @export
gx_classify_distribution <- function(access_url, media_type = NULL, conforms_to = character()) {
  if (!gx_is_http_uri(access_url)) {
    gx_abort("{.arg access_url} must be a safe absolute HTTP(S) URI.", "gx_error_classifier")
  }
  if (!is.null(media_type) &&
      (!is.character(media_type) || length(media_type) != 1L || is.na(media_type))) {
    gx_abort("{.arg media_type} must be NULL or one string.", "gx_error_classifier")
  }
  if (!is.character(conforms_to) || anyNA(conforms_to)) {
    gx_abort("{.arg conforms_to} must be a character vector.", "gx_error_classifier")
  }

  registry <- gx_handlers()
  facts <- list(
    access_url = access_url,
    media_type = media_type,
    conforms_to = conforms_to
  )
  matched <- vapply(registry$classifier, gx_classifier_matches, logical(1), facts = facts)
  if (!any(matched)) {
    gx_abort("Handler registry did not provide a fallback.", "gx_error_asset")
  }
  registry$id[[which(matched)[[1]]]]
}

#' Read reviewed unit conversion rules
#'
#' Rules are directed affine transforms using
#' `converted_value = original_value * scale + offset`.
#'
#' @return A tibble of versioned conversion rules.
#' @export
gx_unit_conversions <- function() {
  path <- file.path(gx_asset_dir("vocab"), "unit-conversions-v1.csv")
  rules <- utils::read.csv(
    path,
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  required <- c(
    "rule_id", "from_unit_uri", "to_unit_uri", "scale", "offset",
    "dimension", "source_uri", "review_date", "status"
  )
  if (!identical(names(rules), required) || !nrow(rules) ||
      any(!nzchar(rules$rule_id)) || anyDuplicated(rules$rule_id)) {
    gx_abort("Unit conversion asset has an invalid contract.", "gx_error_asset")
  }
  scale <- suppressWarnings(as.numeric(rules$scale))
  offset <- suppressWarnings(as.numeric(rules$offset))
  if (any(!is.finite(scale)) || any(!is.finite(offset)) ||
      any(!vapply(rules$from_unit_uri, gx_is_http_uri, logical(1))) ||
      any(!vapply(rules$to_unit_uri, gx_is_http_uri, logical(1)))) {
    gx_abort("Unit conversion rules contain invalid numeric values or URIs.", "gx_error_asset")
  }
  rules$scale <- scale
  rules$offset <- offset
  tibble::as_tibble(rules)
}
