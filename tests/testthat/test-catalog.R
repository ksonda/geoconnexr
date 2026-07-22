catalog_test_hash <- function(x) {
  digest::digest(enc2utf8(x), algo = "sha256", serialize = FALSE)
}

catalog_test_aoi <- function() {
  ring <- rbind(
    c(-80, 35), c(-75, 35), c(-75, 40), c(-80, 40), c(-80, 35)
  )
  gx_aoi(sf::st_sfc(sf::st_polygon(list(ring)), crs = "OGC:CRS84"))
}

catalog_test_sites <- function(n = 2L, empty_geometry = FALSE) {
  n <- as.integer(n)
  if (!n) return(gx_catalog_empty_sites())
  index <- seq_len(n)
  geometry <- lapply(index, function(i) {
    if (empty_geometry) sf::st_point() else sf::st_point(c(-79 + i / 100, 36 + i / 100))
  })
  sf::st_sf(
    tibble::tibble(
      contract_version = rep("0.1.0", n),
      site_uri = sprintf("https://example.org/site/%05d", index),
      name = sprintf("Site %05d", index),
      description = rep(NA_character_, n),
      site_type = rep("hydrometricStation", n),
      provider_id = rep("fixture-provider", n),
      provider_uri = rep("https://example.org/provider/fixture", n),
      provider_name = rep("Fixture Provider", n),
      provider_url = rep("https://example.org/provider", n),
      mainstem_uri = rep(NA_character_, n),
      landing_url = sprintf("https://example.org/landing/%05d", index),
      source_url = sprintf("https://example.org/profile/%05d", index)
    ),
    geometry = sf::st_sfc(geometry, crs = "OGC:CRS84")
  )
}

catalog_test_datasets <- function(sites, n = nrow(sites)) {
  n <- as.integer(n)
  if (!n) return(gx_catalog_empty_datasets())
  stopifnot(nrow(sites) > 0L)
  index <- seq_len(n)
  site_index <- ((index - 1L) %% nrow(sites)) + 1L
  tibble::tibble(
    contract_version = rep("0.1.0", n),
    site_uri = sites$site_uri[site_index],
    dataset_id = vapply(sprintf("dataset-%05d", index), catalog_test_hash, character(1)),
    distribution_id = vapply(sprintf("distribution-%05d", index), catalog_test_hash, character(1)),
    variable_id = vapply(sprintf("variable-%05d", index), catalog_test_hash, character(1)),
    dataset_uri = sprintf("https://example.org/dataset/%05d", index),
    dataset_name = sprintf("Dataset %05d", index),
    dataset_description = rep(NA_character_, n),
    temporal_coverage = rep("2024-01-01T00:00:00Z/2024-12-31T00:00:00Z", n),
    temporal_start = as.POSIXct(rep("2024-01-01 00:00:00", n), tz = "UTC"),
    temporal_end = as.POSIXct(rep("2024-12-31 00:00:00", n), tz = "UTC"),
    variable_uri = sprintf("https://example.org/variable/%05d", index),
    variable_name = sprintf("Variable %05d", index),
    unit_uri = rep("https://qudt.org/vocab/unit/M3-PER-SEC", n),
    unit_label = rep("m3/s", n),
    measurement_technique = rep(NA_character_, n),
    distribution_url = sprintf("https://example.org/data/%05d.csv", index),
    media_type = rep("text/csv", n),
    conforms_to = rep(list(c(
      "https://example.org/spec/a", "https://example.org/spec/b"
    )), n),
    provider_uri = rep("https://example.org/provider/fixture", n),
    provider_name = rep("Fixture Provider", n),
    provider_url = rep("https://example.org/provider", n),
    license = rep("https://creativecommons.org/publicdomain/zero/1.0/", n),
    access_rights = rep("public", n),
    handler_id = rep("csv", n),
    fetchable = rep(TRUE, n),
    source_url = sprintf("https://example.org/profile/%05d", site_index)
  )
}

catalog_test_problems <- function(n = 1L) {
  n <- as.integer(n)
  if (!n) return(gx_catalog_empty_problems())
  index <- seq_len(n)
  tibble::tibble(
    stage = rep("datasets", n),
    source_uri = sprintf("https://example.org/profile/%05d", index),
    path = sprintf("/datasets/%d", index - 1L),
    code = sprintf("fixture_warning_%05d", index),
    severity = rep("warning", n),
    message = sprintf("Fixture warning %05d", index),
    recoverable = rep(TRUE, n),
    occurred_at = as.POSIXct(rep("2025-01-02 03:04:05", n), tz = "UTC")
  )
}

catalog_test_requests <- function(n = 2L) {
  n <- as.integer(n)
  if (!n) return(gx_catalog_empty_requests())
  index <- seq_len(n)
  urls <- sprintf("https://example.org/request/%05d", index)
  tibble::tibble(
    request_id = sprintf("fixture-request-%05d", index),
    stage = rep(c("sites", "datasets"), length.out = n),
    method = rep("GET", n),
    canonical_url_redacted = urls,
    request_hash = vapply(paste("GET", urls), catalog_test_hash, character(1)),
    body_hash = rep(NA_character_, n),
    final_url = urls,
    response_status = rep(200L, n),
    response_media_type = rep("application/json", n),
    encoded_bytes = as.integer(100L + index),
    decoded_bytes = as.integer(200L + index),
    content_hash = vapply(sprintf("body-%05d", index), catalog_test_hash, character(1)),
    etag = sprintf("\"fixture-%05d\"", index),
    last_modified = rep(NA_character_, n),
    retrieved_at = as.POSIXct(rep("2025-01-02 03:04:05", n), tz = "UTC"),
    elapsed_ms = as.double(index),
    cache_origin = rep("network", n),
    error_code = rep(NA_character_, n)
  )
}

catalog_test_completeness <- function(include, sites, datasets, reference = list()) {
  output_count <- vapply(include, function(stage) {
    switch(
      stage,
      sites = nrow(sites),
      datasets = nrow(datasets),
      reference = sum(vapply(reference, function(x) {
        if (is.data.frame(x)) nrow(x) else 0L
      }, integer(1))),
      0L
    )
  }, integer(1))
  tibble::tibble(
    stage = include,
    status = rep("complete", length(include)),
    truncated = rep(FALSE, length(include)),
    input_count = output_count,
    attempted_count = output_count,
    succeeded_count = output_count,
    failed_count = rep(0L, length(include)),
    skipped_count = rep(0L, length(include)),
    output_count = output_count,
    reason = rep(NA_character_, length(include))
  )
}

catalog_test_metadata <- function(sites, datasets, reference, problems, requests,
                                  include = c("sites", "datasets", "reference"),
                                  completeness = NULL) {
  if (is.null(completeness)) {
    completeness <- catalog_test_completeness(include, sites, datasets, reference)
  }
  reference_features <- sum(vapply(reference, function(x) {
    if (is.data.frame(x)) nrow(x) else 0L
  }, integer(1)))
  list(
    created_at = as.POSIXct("2025-01-02 03:04:05", tz = "UTC"),
    selection = list(
      include = include,
      providers = character(),
      variables = character()
    ),
    completeness = completeness,
    counts = list(
      sites = as.integer(nrow(sites)),
      datasets = as.integer(nrow(datasets)),
      reference_layers = as.integer(length(reference)),
      reference_features = as.integer(reference_features),
      problems = as.integer(nrow(problems)),
      requests = as.integer(nrow(requests))
    ),
    endpoints = c(
      graph = "https://example.org/sparql",
      reference = "https://example.org/reference"
    ),
    hydrologic_vintage = list(
      reference_collection = NA_character_,
      vintage = NA_character_,
      migration_policy = "not_checked"
    ),
    source_contracts = c(
      aoi = "1.0.0",
      catalog = "0.1.0"
    )
  )
}

catalog_test_catalog <- function(n_sites = 2L, n_datasets = n_sites,
                                 n_problems = 1L, n_requests = 2L,
                                 include = c("sites", "datasets", "reference")) {
  aoi <- catalog_test_aoi()
  sites <- catalog_test_sites(n_sites)
  datasets <- catalog_test_datasets(sites, n_datasets)
  reference <- list()
  problems <- catalog_test_problems(n_problems)
  requests <- catalog_test_requests(n_requests)
  metadata <- catalog_test_metadata(
    sites, datasets, reference, problems, requests, include
  )
  gx_catalog_new_impl(
    aoi = aoi,
    sites = sites,
    datasets = datasets,
    reference = reference,
    problems = problems,
    requests = requests,
    metadata = metadata
  )
}

test_that("catalog empty helpers have exact stable table contracts", {
  sites <- gx_catalog_empty_sites()
  datasets <- gx_catalog_empty_datasets()
  problems <- gx_catalog_empty_problems()
  requests <- gx_catalog_empty_requests()
  completeness <- gx_catalog_empty_completeness()

  expect_s3_class(sites, "sf")
  expect_equal(nrow(sites), 0L)
  expect_identical(names(sites), c(
    "contract_version", "site_uri", "name", "description", "site_type",
    "provider_id", "provider_uri", "provider_name", "provider_url",
    "mainstem_uri", "landing_url", "source_url", "geometry"
  ))
  expect_identical(as.character(sf::st_geometry_type(sites)), character())
  expect_identical(sf::st_crs(sites)$input, "OGC:CRS84")

  expect_equal(nrow(datasets), 0L)
  expect_identical(names(datasets), c(
    "contract_version", "site_uri", "dataset_id", "distribution_id",
    "variable_id", "dataset_uri", "dataset_name", "dataset_description",
    "temporal_coverage", "temporal_start", "temporal_end", "variable_uri",
    "variable_name", "unit_uri", "unit_label", "measurement_technique",
    "distribution_url", "media_type", "conforms_to", "provider_uri",
    "provider_name", "provider_url", "license", "access_rights", "handler_id",
    "fetchable", "source_url"
  ))
  expect_s3_class(datasets$temporal_start, "POSIXct")
  expect_s3_class(datasets$temporal_end, "POSIXct")
  expect_type(datasets$conforms_to, "list")
  expect_type(datasets$fetchable, "logical")

  expect_identical(names(problems), c(
    "stage", "source_uri", "path", "code", "severity", "message",
    "recoverable", "occurred_at"
  ))
  expect_s3_class(problems$occurred_at, "POSIXct")
  expect_identical(names(requests), c(
    "request_id", "stage", "method", "canonical_url_redacted",
    "request_hash", "body_hash", "final_url", "response_status",
    "response_media_type", "encoded_bytes", "decoded_bytes", "content_hash",
    "etag", "last_modified", "retrieved_at", "elapsed_ms", "cache_origin",
    "error_code"
  ))
  expect_s3_class(requests$retrieved_at, "POSIXct")
  expect_identical(names(completeness), c(
    "stage", "status", "truncated", "input_count", "attempted_count",
    "succeeded_count", "failed_count", "skipped_count", "output_count",
    "reason"
  ))
  expect_true(all(vapply(
    completeness[c(
      "input_count", "attempted_count", "succeeded_count", "failed_count",
      "skipped_count", "output_count"
    )],
    is.integer,
    logical(1)
  )))
})

test_that("zero-row and populated catalogs validate with exact top-level shape", {
  zero <- catalog_test_catalog(
    n_sites = 0L, n_datasets = 0L, n_problems = 0L, n_requests = 0L
  )
  populated <- catalog_test_catalog()

  for (x in list(zero, populated)) {
    expect_identical(class(x), "gx_catalog")
    expect_identical(names(x), c(
      "contract_version", "aoi", "sites", "datasets", "reference",
      "problems", "requests", "metadata"
    ))
    expect_identical(x$contract_version, "0.1.0")
    expect_identical(gx_catalog_validate_impl(x), invisible(x))
  }
  expect_equal(nrow(zero$sites), 0L)
  expect_equal(nrow(zero$datasets), 0L)
  expect_equal(unname(unlist(zero$metadata$counts)), rep(0L, 6L))
})

test_that("catalog construction requires a valid gx_aoi", {
  x <- catalog_test_catalog()
  bad_class <- x$aoi
  class(bad_class) <- "list"
  bad_digest <- x$aoi
  bad_digest$id <- strrep("0", 64L)

  expect_error(
    gx_catalog_new_impl(
      bad_class, x$sites, x$datasets, x$reference, x$problems, x$requests,
      x$metadata
    ),
    class = "gx_error_catalog_input"
  )
  expect_error(
    gx_catalog_new_impl(
      bad_digest, x$sites, x$datasets, x$reference, x$problems, x$requests,
      x$metadata
    ),
    class = "gx_error_catalog_input"
  )
})

test_that("site identity, point geometry, and CRS are enforced", {
  x <- catalog_test_catalog()

  duplicate <- x
  duplicate$sites$site_uri[[2L]] <- duplicate$sites$site_uri[[1L]]
  expect_error(gx_catalog_validate_impl(duplicate), class = "gx_error_catalog_contract")

  bad_uri <- x
  bad_uri$sites$site_uri[[1L]] <- "not a URI"
  expect_error(gx_catalog_validate_impl(bad_uri), class = "gx_error_catalog_contract")

  noncanonical_uri <- x
  noncanonical_uri$sites$site_uri[[1L]] <- "HTTPS://EXAMPLE.ORG.:443/site/00001"
  expect_error(
    gx_catalog_validate_impl(noncanonical_uri),
    class = "gx_error_catalog_contract"
  )

  line <- catalog_test_catalog(n_sites = 1L, n_datasets = 1L)
  sf::st_geometry(line$sites) <- sf::st_sfc(
    sf::st_linestring(rbind(c(-79, 36), c(-78, 37))),
    crs = "OGC:CRS84"
  )
  expect_error(gx_catalog_validate_impl(line), class = "gx_error_catalog_geometry")

  wrong_crs <- x
  suppressWarnings(sf::st_crs(wrong_crs$sites) <- sf::st_crs(3857))
  expect_error(
    gx_catalog_validate_impl(wrong_crs),
    class = "gx_error_catalog_geometry"
  )

  empty_sites <- catalog_test_sites(1L, empty_geometry = TRUE)
  empty_datasets <- gx_catalog_empty_datasets()
  metadata <- catalog_test_metadata(
    empty_sites, empty_datasets, list(), gx_catalog_empty_problems(),
    gx_catalog_empty_requests(), include = "sites"
  )
  expect_silent(gx_catalog_new_impl(
    catalog_test_aoi(), empty_sites, empty_datasets, list(),
    gx_catalog_empty_problems(), gx_catalog_empty_requests(), metadata
  ))
})

test_that("dataset foreign keys and identity triples are closed", {
  x <- catalog_test_catalog()

  orphan <- x
  orphan$datasets$site_uri[[1L]] <- "https://example.org/site/missing"
  expect_error(gx_catalog_validate_impl(orphan), class = "gx_error_catalog_contract")

  for (field in c("dataset_id", "distribution_id", "variable_id")) {
    invalid <- x
    invalid$datasets[[field]][[1L]] <- "ABC"
    expect_error(
      gx_catalog_validate_impl(invalid),
      class = "gx_error_catalog_contract",
      info = field
    )
  }

  duplicate <- x
  duplicate$datasets[2L, c("dataset_id", "distribution_id", "variable_id")] <-
    duplicate$datasets[1L, c("dataset_id", "distribution_id", "variable_id")]
  expect_error(gx_catalog_validate_impl(duplicate), class = "gx_error_catalog_contract")
})

test_that("dataset temporal and fetchability invariants fail closed", {
  x <- catalog_test_catalog()

  reverse_time <- x
  reverse_time$datasets$temporal_start[[1L]] <- as.POSIXct(
    "2025-01-01 00:00:00", tz = "UTC"
  )
  expect_error(
    gx_catalog_validate_impl(reverse_time),
    class = "gx_error_catalog_contract"
  )

  non_utc <- x
  attr(non_utc$datasets$temporal_start, "tzone") <- "America/New_York"
  expect_error(gx_catalog_validate_impl(non_utc), class = "gx_error_catalog_contract")

  for (field in c("distribution_url", "handler_id")) {
    unavailable <- x
    unavailable$datasets[[field]][[1L]] <- NA_character_
    expect_error(
      gx_catalog_validate_impl(unavailable),
      class = "gx_error_catalog_contract",
      info = field
    )
  }

  visible_but_not_fetchable <- x
  visible_but_not_fetchable$datasets$fetchable[[1L]] <- FALSE
  visible_but_not_fetchable$datasets$distribution_url[[1L]] <- NA_character_
  visible_but_not_fetchable$datasets$handler_id[[1L]] <- "unknown"
  expect_identical(
    gx_catalog_validate_impl(visible_but_not_fetchable),
    invisible(visible_but_not_fetchable)
  )
})

test_that("dataset conforms_to values remain canonical bounded URI arrays", {
  x <- catalog_test_catalog()

  not_a_list <- x
  not_a_list$datasets$conforms_to <- rep("https://example.org/spec/a", nrow(x$datasets))
  expect_error(gx_catalog_validate_impl(not_a_list), class = "gx_error_catalog_contract")

  invalid_uri <- x
  invalid_uri$datasets$conforms_to[[1L]] <- "not a URI"
  expect_error(gx_catalog_validate_impl(invalid_uri), class = "gx_error_catalog_contract")

  duplicate <- x
  duplicate$datasets$conforms_to[[1L]] <- rep("https://example.org/spec/a", 2L)
  expect_error(gx_catalog_validate_impl(duplicate), class = "gx_error_catalog_contract")

  over_row_budget <- x
  over_row_budget$datasets$conforms_to[[1L]] <- sprintf(
    "https://example.org/spec/%03d", seq_len(65L)
  )
  expect_error(
    gx_catalog_validate_impl(over_row_budget),
    class = "gx_error_catalog_budget"
  )
})

test_that("diagnostics convert to timestamped recoverable catalog problems", {
  diagnostics <- gx_diagnostic(
    "warning", "missing_distribution", "/dataset/0",
    "The distribution is absent.", recoverable = TRUE
  )
  occurred_at <- as.POSIXct("2025-02-03 04:05:06", tz = "UTC")
  out <- gx_catalog_problems_from_diagnostics_impl(
    diagnostics,
    stage = "datasets",
    source_uri = "https://example.org/profile/00001",
    occurred_at = occurred_at
  )

  expect_identical(names(out), names(gx_catalog_empty_problems()))
  expect_equal(nrow(out), 1L)
  expect_identical(out$stage, "datasets")
  expect_identical(out$source_uri, "https://example.org/profile/00001")
  expect_identical(out$code, "missing_distribution")
  expect_true(out$recoverable)
  expect_identical(out$occurred_at, occurred_at)
  expect_identical(
    gx_catalog_problems_from_diagnostics_impl(
      gx_empty_diagnostics(), "datasets", occurred_at = occurred_at
    ),
    gx_catalog_empty_problems()
  )
})

test_that("problems and completeness jointly describe only recoverable omissions", {
  x <- catalog_test_catalog()

  fatal <- x
  fatal$problems$recoverable[[1L]] <- FALSE
  expect_error(gx_catalog_validate_impl(fatal), class = "gx_error_catalog_contract")

  bad_severity <- x
  bad_severity$problems$severity[[1L]] <- "fatal"
  expect_error(
    gx_catalog_validate_impl(bad_severity),
    class = "gx_error_catalog_contract"
  )

  contradictory <- x
  contradictory$problems$severity[[1L]] <- "error"
  expect_error(
    gx_catalog_validate_impl(contradictory),
    class = "gx_error_catalog_completeness"
  )

  partial <- x
  row <- which(partial$metadata$completeness$stage == "datasets")
  partial$metadata$completeness$status[[row]] <- "partial"
  partial$metadata$completeness$truncated[[row]] <- TRUE
  partial$metadata$completeness$input_count[[row]] <- 3L
  partial$metadata$completeness$attempted_count[[row]] <- 3L
  partial$metadata$completeness$succeeded_count[[row]] <- 2L
  partial$metadata$completeness$failed_count[[row]] <- 1L
  partial$metadata$completeness$reason[[row]] <- "Provider result was truncated."
  expect_identical(gx_catalog_validate_impl(partial), invisible(partial))
})

test_that("completeness equations, selected stages, and output counts reconcile", {
  x <- catalog_test_catalog()

  equation <- x
  equation$metadata$completeness$attempted_count[[1L]] <-
    equation$metadata$completeness$attempted_count[[1L]] + 1L
  expect_error(
    gx_catalog_validate_impl(equation),
    class = "gx_error_catalog_completeness"
  )

  missing_stage <- x
  missing_stage$metadata$completeness <-
    missing_stage$metadata$completeness[-1L, , drop = FALSE]
  expect_error(
    gx_catalog_validate_impl(missing_stage),
    class = "gx_error_catalog_completeness"
  )

  wrong_output <- x
  row <- which(wrong_output$metadata$completeness$stage == "datasets")
  wrong_output$metadata$completeness$output_count[[row]] <- 999L
  expect_error(
    gx_catalog_validate_impl(wrong_output),
    class = "gx_error_catalog_completeness"
  )

  no_reason <- x
  row <- which(no_reason$metadata$completeness$stage == "datasets")
  no_reason$metadata$completeness$status[[row]] <- "partial"
  no_reason$metadata$completeness$truncated[[row]] <- TRUE
  expect_error(
    gx_catalog_validate_impl(no_reason),
    class = "gx_error_catalog_completeness"
  )
})

test_that("selection and catalog metadata are exact and canonical", {
  x <- catalog_test_catalog()

  missing_sites <- x
  missing_sites$metadata$selection$include <- "datasets"
  expect_error(
    gx_catalog_validate_impl(missing_sites),
    class = "gx_error_catalog_contract"
  )

  unsorted_filter <- x
  unsorted_filter$metadata$selection$providers <- c("z_provider", "a_provider")
  expect_error(
    gx_catalog_validate_impl(unsorted_filter),
    class = "gx_error_catalog_contract"
  )

  wrong_count <- x
  wrong_count$metadata$counts$sites <- wrong_count$metadata$counts$sites + 1L
  expect_error(gx_catalog_validate_impl(wrong_count), class = "gx_error_catalog_contract")

  unsafe_endpoint <- x
  unsafe_endpoint$metadata$endpoints[[1L]] <- "https://user:secret@example.org/"
  expect_error(
    gx_catalog_validate_impl(unsafe_endpoint),
    class = "gx_error_catalog_contract"
  )

  wrong_contract <- x
  wrong_contract$metadata$source_contracts[["catalog"]] <- "9.9.9"
  expect_error(
    gx_catalog_validate_impl(wrong_contract),
    class = "gx_error_catalog_contract"
  )
})

test_that("request ledgers enforce identity, safety, types, and unique attempts", {
  x <- catalog_test_catalog()

  duplicate <- x
  duplicate$requests$request_id[[2L]] <- duplicate$requests$request_id[[1L]]
  expect_error(gx_catalog_validate_impl(duplicate), class = "gx_error_catalog_contract")

  bad_hash <- x
  bad_hash$requests$request_hash[[1L]] <- strrep("G", 64L)
  expect_error(gx_catalog_validate_impl(bad_hash), class = "gx_error_catalog_contract")

  bad_method <- x
  bad_method$requests$method[[1L]] <- "TRACE"
  expect_error(gx_catalog_validate_impl(bad_method), class = "gx_error_catalog_contract")

  credential <- x
  credential$requests$canonical_url_redacted[[1L]] <-
    "https://user:secret@example.org/data?token=secret"
  expect_error(gx_catalog_validate_impl(credential), class = "gx_error_catalog_contract")

  negative_bytes <- x
  negative_bytes$requests$encoded_bytes[[1L]] <- -1L
  expect_error(
    gx_catalog_validate_impl(negative_bytes),
    class = "gx_error_catalog_contract"
  )

  bad_cache <- x
  bad_cache$requests$cache_origin[[1L]] <- "magic"
  expect_error(gx_catalog_validate_impl(bad_cache), class = "gx_error_catalog_contract")
})

test_that("representative catalog row and scalar budgets fail closed", {
  expect_error(
    catalog_test_catalog(
      n_sites = 10001L, n_datasets = 0L, n_problems = 0L, n_requests = 0L,
      include = "sites"
    ),
    class = "gx_error_catalog_budget"
  )

  scalar <- catalog_test_catalog()
  scalar$sites$name[[1L]] <- strrep("x", 16L * 1024L + 1L)
  expect_error(gx_catalog_validate_impl(scalar), class = "gx_error_catalog_budget")
})

test_that("catalog export views are deterministic under input permutation", {
  x <- catalog_test_catalog(
    n_sites = 3L, n_datasets = 3L, n_problems = 2L, n_requests = 3L
  )
  permuted <- gx_catalog_new_impl(
    aoi = x$aoi,
    sites = x$sites[rev(seq_len(nrow(x$sites))), , drop = FALSE],
    datasets = x$datasets[rev(seq_len(nrow(x$datasets))), , drop = FALSE],
    reference = x$reference,
    problems = x$problems[rev(seq_len(nrow(x$problems))), , drop = FALSE],
    requests = x$requests[rev(seq_len(nrow(x$requests))), , drop = FALSE],
    metadata = x$metadata
  )

  baseline_views <- gx_catalog_export_views_impl(x)
  permuted_views <- gx_catalog_export_views_impl(permuted)
  expect_identical(permuted_views, baseline_views)
  expect_s3_class(baseline_views$sites, "tbl_df")
  expect_false(inherits(baseline_views$sites, "sf"))
  expect_false("geometry" %in% names(baseline_views$sites))
  expect_true("geometry_wkt" %in% names(baseline_views$sites))
  expect_true(all(grepl("^POINT ", baseline_views$sites$geometry_wkt)))
  expect_identical(
    baseline_views$sites$site_uri,
    sort(x$sites$site_uri, method = "radix")
  )
  expect_type(baseline_views$datasets$conforms_to, "character")
  expect_true(all(vapply(
    baseline_views$datasets$conforms_to,
    jsonlite::validate,
    logical(1)
  )))
  expect_identical(
    baseline_views$datasets$conforms_to[[1L]],
    '["https://example.org/spec/a","https://example.org/spec/b"]'
  )
})

test_that("catalog export views redact HTTP-bearing copies without mutation", {
  x <- catalog_test_catalog()
  secrets <- c(
    site = "site-secret", dataset = "dataset-secret",
    conforms = "conforms-secret", fragment = "private-fragment"
  )
  x$sites$provider_url[[1L]] <- paste0(
    "https://example.org/provider?token=", secrets[["site"]],
    "#", secrets[["fragment"]]
  )
  x$sites$source_url[[1L]] <- paste0(
    "https://example.org/profile/00001?token=", secrets[["site"]]
  )
  x$datasets$distribution_url[[1L]] <- paste0(
    "https://example.org/data/00001.csv?token=", secrets[["dataset"]]
  )
  x$datasets$source_url[[1L]] <- paste0(
    "https://example.org/profile/00001?token=", secrets[["dataset"]]
  )
  x$datasets$conforms_to[[1L]] <- paste0(
    "https://example.org/spec/a?token=", secrets[["conforms"]]
  )
  expect_identical(gx_catalog_validate_impl(x), invisible(x))

  views <- gx_catalog_export_views_impl(x)
  exported_text <- c(
    unlist(views$sites[vapply(views$sites, is.character, logical(1))],
      use.names = FALSE
    ),
    unlist(views$datasets[vapply(views$datasets, is.character, logical(1))],
      use.names = FALSE
    )
  )
  for (secret in secrets) {
    expect_false(any(grepl(secret, exported_text, fixed = TRUE)), info = secret)
  }
  expect_true(grepl(secrets[["site"]], x$sites$source_url[[1L]], fixed = TRUE))
  expect_true(grepl(
    secrets[["dataset"]], x$datasets$distribution_url[[1L]], fixed = TRUE
  ))
})

test_that("the M6c constructor internals remain private", {
  internal <- c(
    "gx_catalog_empty_sites", "gx_catalog_empty_datasets",
    "gx_catalog_empty_problems", "gx_catalog_empty_requests",
    "gx_catalog_empty_completeness",
    "gx_catalog_new_impl", "gx_catalog_validate_impl",
    "gx_catalog_export_views_impl", "gx_catalog_problems_from_diagnostics_impl"
  )
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(internal %in% exports))
  expect_true("gx_catalog" %in% exports)
})
