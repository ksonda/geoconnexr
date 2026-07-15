catalog_hardening_hash <- function(x) {
  digest::digest(enc2utf8(x), algo = "sha256", serialize = FALSE)
}

catalog_hardening_sites <- function(n = 2L) {
  index <- seq_len(as.integer(n))
  geometry <- sf::st_sfc(
    lapply(index, function(i) sf::st_point(c(-79 + i / 100, 36 + i / 100))),
    crs = "OGC:CRS84"
  )
  sf::st_sf(
    tibble::tibble(
      contract_version = rep("0.1.0", n),
      site_uri = sprintf("https://example.org/site/%02d", index),
      name = sprintf("Site %02d", index),
      description = rep(NA_character_, n),
      site_type = rep("hydrometricStation", n),
      provider_id = rep("fixture_provider", n),
      provider_uri = rep("https://example.org/provider", n),
      provider_name = rep("Fixture Provider", n),
      provider_url = rep("https://example.org/provider", n),
      mainstem_uri = rep(NA_character_, n),
      landing_url = rep("https://example.org/landing", n),
      source_url = rep("https://example.org/profile", n)
    ),
    geometry = geometry
  )
}

catalog_hardening_datasets <- function(sites) {
  n <- nrow(sites)
  index <- seq_len(n)
  tibble::tibble(
    contract_version = rep("0.1.0", n),
    site_uri = sites$site_uri,
    dataset_id = vapply(paste0("dataset-", index), catalog_hardening_hash,
      character(1)
    ),
    distribution_id = vapply(
      paste0("distribution-", index), catalog_hardening_hash, character(1)
    ),
    variable_id = vapply(paste0("variable-", index), catalog_hardening_hash,
      character(1)
    ),
    dataset_uri = sprintf("https://example.org/dataset/%02d", index),
    dataset_name = sprintf("Dataset %02d", index),
    dataset_description = rep(NA_character_, n),
    temporal_coverage = rep("2024-01-01/2024-12-31", n),
    temporal_start = as.POSIXct(rep("2024-01-01 00:00:00", n), tz = "UTC"),
    temporal_end = as.POSIXct(rep("2024-12-31 00:00:00", n), tz = "UTC"),
    variable_uri = sprintf("https://example.org/variable/%02d", index),
    variable_name = sprintf("Variable %02d", index),
    unit_uri = rep("https://qudt.org/vocab/unit/M3-PER-SEC", n),
    unit_label = rep("m3/s", n),
    measurement_technique = rep(NA_character_, n),
    distribution_url = sprintf("https://example.org/data/%02d.csv", index),
    media_type = rep("text/csv", n),
    conforms_to = rep(list(character()), n),
    provider_uri = rep("https://example.org/provider", n),
    provider_name = rep("Fixture Provider", n),
    provider_url = rep("https://example.org/provider", n),
    license = rep(NA_character_, n),
    access_rights = rep("public", n),
    handler_id = rep("csv", n),
    fetchable = rep(TRUE, n),
    source_url = rep("https://example.org/profile", n)
  )
}

catalog_hardening_completeness <- function(n) {
  count <- rep(as.integer(n), 2L)
  tibble::tibble(
    stage = c("sites", "datasets"),
    status = rep("complete", 2L),
    truncated = rep(FALSE, 2L),
    input_count = count,
    attempted_count = count,
    succeeded_count = count,
    failed_count = c(0L, 0L),
    skipped_count = c(0L, 0L),
    output_count = count,
    reason = c(NA_character_, NA_character_)
  )
}

catalog_hardening_fixture <- function(n = 2L) {
  sites <- catalog_hardening_sites(n)
  datasets <- catalog_hardening_datasets(sites)
  problems <- gx_catalog_empty_problems()
  requests <- gx_catalog_empty_requests()
  metadata <- list(
    created_at = as.POSIXct("2026-07-15 12:00:00", tz = "UTC"),
    selection = list(
      include = c("sites", "datasets"),
      providers = character(),
      variables = character()
    ),
    completeness = catalog_hardening_completeness(n),
    counts = list(
      sites = as.integer(n),
      datasets = as.integer(n),
      reference_layers = 0L,
      reference_features = 0L,
      problems = 0L,
      requests = 0L
    ),
    endpoints = c(graph = "https://example.org/sparql"),
    hydrologic_vintage = list(
      reference_collection = NA_character_,
      vintage = NA_character_,
      migration_policy = "not_checked"
    ),
    source_contracts = c(aoi = "1.0.0", catalog = "0.1.0")
  )
  gx_catalog_new_impl(
    aoi = gx_aoi("02070010"),
    sites = sites,
    datasets = datasets,
    reference = list(),
    problems = problems,
    requests = requests,
    metadata = metadata
  )
}

catalog_hardening_export_text <- function(view) {
  values <- unlist(view[vapply(view, is.character, logical(1))],
    use.names = FALSE
  )
  paste(values[!is.na(values)], collapse = "\n")
}

test_that("generic URI secrets are absent from catalog and snapshot-safe views", {
  catalog <- catalog_hardening_fixture(1L)
  catalog$datasets$variable_id[[1L]] <- paste0(
    "ftp://user:VARIABLE_SECRET@example.org/id?token=VARIABLE_SECRET"
  )
  catalog$datasets$conforms_to[[1L]] <- c(
    "ftp://user:CONFORMS_SECRET@example.org/spec?token=CONFORMS_SECRET#private"
  )
  catalog$datasets$measurement_technique[[1L]] <-
    "ftp://user:TECHNIQUE_SECRET@example.org/method?token=TECHNIQUE_SECRET"
  catalog$datasets$access_rights[[1L]] <-
    "ftp://user:ACCESS_SECRET@example.org/policy?token=ACCESS_SECRET"
  expect_identical(gx_catalog_validate_impl(catalog), invisible(catalog))

  exported <- gx_catalog_export_views_impl(catalog)$datasets
  snapshot_safe <- gx_snapshot_writer_redact_view(
    gx_snapshot_writer_character_view(exported)
  )
  exported_text <- catalog_hardening_export_text(exported)
  snapshot_text <- catalog_hardening_export_text(snapshot_safe)

  secrets <- c(
    "VARIABLE_SECRET", "CONFORMS_SECRET", "TECHNIQUE_SECRET",
    "ACCESS_SECRET", "user:", "#private"
  )
  expect_false(any(vapply(
    secrets, grepl, logical(1), x = exported_text, fixed = TRUE
  )))
  expect_false(any(vapply(
    secrets, grepl, logical(1), x = snapshot_text, fixed = TRUE
  )))
  expect_true("variable_id_sha256" %in% names(exported))
  if ("variable_id_sha256" %in% names(exported)) {
    expect_match(exported$variable_id_sha256, "^[a-f0-9]{64}$")
  }
  expect_true(grepl(
    "VARIABLE_SECRET", catalog$datasets$variable_id[[1L]], fixed = TRUE
  ))
  expect_true(grepl(
    "CONFORMS_SECRET", catalog$datasets$conforms_to[[1L]], fixed = TRUE
  ))
})

test_that("redacted site identities retain an unambiguous exported join key", {
  catalog <- catalog_hardening_fixture(2L)
  raw_keys <- c("IDENTITY_ALPHA_SECRET", "IDENTITY_BETA_SECRET")
  catalog$sites$site_uri <- paste0(
    "https://example.org/site?identity=", raw_keys
  )
  catalog$datasets$site_uri <- catalog$sites$site_uri
  expect_identical(gx_catalog_validate_impl(catalog), invisible(catalog))

  views <- gx_catalog_export_views_impl(catalog)
  all_text <- paste(
    catalog_hardening_export_text(views$sites),
    catalog_hardening_export_text(views$datasets),
    collapse = "\n"
  )
  for (key in raw_keys) {
    expect_false(grepl(key, all_text, fixed = TRUE), info = key)
  }

  has_fingerprint <- "site_uri_sha256" %in% names(views$sites) &&
    "site_uri_sha256" %in% names(views$datasets)
  expect_true(has_fingerprint)
  if (!has_fingerprint) return(invisible(NULL))

  expect_match(views$sites$site_uri_sha256, "^[a-f0-9]{64}$")
  expect_match(views$datasets$site_uri_sha256, "^[a-f0-9]{64}$")
  expect_identical(anyDuplicated(views$sites$site_uri_sha256), 0L)
  positions <- match(
    views$datasets$site_uri_sha256,
    views$sites$site_uri_sha256
  )
  expect_false(anyNA(positions))
  expect_identical(
    sort(tabulate(positions, nbins = nrow(views$sites))),
    rep(1L, nrow(views$sites))
  )
})

test_that("completeness overflow is a warning-free typed contract error", {
  completeness <- gx_catalog_empty_completeness()
  completeness[1L, ] <- list(
    "sites", "partial", FALSE,
    .Machine$integer.max, .Machine$integer.max, .Machine$integer.max,
    0L, 1L, 0L, "Count reconciliation overflowed."
  )

  condition <- NULL
  expect_no_warning({
    condition <- tryCatch(
      gx_catalog_validate_completeness(completeness),
      error = identity
    )
  })
  expect_s3_class(condition, "gx_error_catalog_completeness")
  expect_false(inherits(condition, "simpleError"))
})

test_that("aggregate text budgets abort before table URI and geometry validation", {
  catalog <- catalog_hardening_fixture(1L)
  payload <- strrep("x", .gx_catalog_max_scalar_bytes)
  n <- as.integer(
    floor(.gx_catalog_max_text_bytes / .gx_catalog_max_scalar_bytes) + 1L
  )
  catalog$problems <- tibble::tibble(
    stage = rep("datasets", n),
    source_uri = rep(NA_character_, n),
    path = rep("", n),
    code = rep("oversized_catalog", n),
    severity = rep("warning", n),
    message = rep(payload, n),
    recoverable = rep(TRUE, n),
    occurred_at = as.POSIXct(rep("2026-07-15 12:00:00", n), tz = "UTC")
  )
  catalog$metadata$counts$problems <- n

  late_calls <- character()
  blocked <- function(stage) {
    force(stage)
    function(...) {
      late_calls <<- c(late_calls, stage)
      stop(paste("late validator reached", stage), call. = FALSE)
    }
  }
  condition <- testthat::with_mocked_bindings(
    tryCatch(gx_catalog_validate_impl(catalog), error = identity),
    gx_catalog_validate_sites = blocked("sites"),
    gx_catalog_validate_datasets = blocked("datasets"),
    gx_catalog_validate_problems = blocked("problems"),
    gx_catalog_validate_requests = blocked("requests"),
    gx_catalog_validate_metadata = blocked("metadata"),
    .package = "geoconnexr"
  )

  expect_s3_class(condition, "gx_error_catalog_budget")
  expect_length(late_calls, 0L)
})

test_that("early text accounting preserves typed errors for malformed tables", {
  catalog <- catalog_hardening_fixture(1L)
  catalog$datasets <- "malformed"
  condition <- tryCatch(gx_catalog_validate_impl(catalog), error = identity)
  expect_s3_class(condition, "gx_error_catalog_contract")
  expect_false(inherits(condition, "simpleError"))
})

test_that("forged table classes fail through typed shape guards", {
  cases <- list(
    sites = list(
      columns = .gx_catalog_site_columns,
      class = "sf",
      validate = function(x) gx_catalog_validate_sites(x)
    ),
    datasets = list(
      columns = .gx_catalog_dataset_columns,
      class = "tbl_df",
      validate = function(x) {
        gx_catalog_validate_datasets(x, gx_catalog_empty_sites())
      }
    ),
    problems = list(
      columns = .gx_catalog_problem_columns,
      class = "tbl_df",
      validate = function(x) gx_catalog_validate_problems(x)
    ),
    requests = list(
      columns = .gx_catalog_request_columns,
      class = "tbl_df",
      validate = function(x) gx_catalog_validate_requests(x)
    ),
    completeness = list(
      columns = .gx_catalog_completeness_columns,
      class = "tbl_df",
      validate = function(x) gx_catalog_validate_completeness(x)
    )
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    forged <- structure(
      setNames(vector("list", length(case$columns)), case$columns),
      class = case$class
    )
    condition <- NULL
    expect_no_warning({
      condition <- tryCatch(case$validate(forged), error = identity)
    })
    expect_s3_class(condition, "gx_error_catalog")
    expect_false(inherits(condition, "simpleError"))
  }
})
