fetch_plan_test_hash <- function(value) {
  vapply(value, function(x) {
    digest::digest(enc2utf8(x), algo = "sha256", serialize = FALSE)
  }, character(1), USE.NAMES = FALSE)
}

fetch_plan_test_time <- function(start = "2025-06-01 00:00:00",
                                 end = "2025-06-30 23:59:59") {
  as.POSIXct(c(start, end), tz = "UTC")
}

fetch_plan_test_now <- function() {
  as.POSIXct("2026-07-15 12:34:56", tz = "UTC")
}

fetch_plan_test_aoi <- function() {
  ring <- rbind(
    c(-80, 35), c(-75, 35), c(-75, 40), c(-80, 40), c(-80, 35)
  )
  gx_aoi(sf::st_sfc(sf::st_polygon(list(ring)), crs = "OGC:CRS84"))
}

fetch_plan_test_sites <- function(n = 7L) {
  n <- as.integer(n)
  if (!n) return(gx_catalog_empty_sites())
  index <- seq_len(n)
  geometry <- sf::st_sfc(
    lapply(index, function(i) sf::st_point(c(-79 + i / 100, 36 + i / 100))),
    crs = "OGC:CRS84"
  )
  sf::st_sf(
    tibble::tibble(
      contract_version = rep("0.1.0", n),
      site_uri = sprintf("https://example.org/site/%02d", index),
      name = sprintf("Plan site %02d", index),
      description = rep(NA_character_, n),
      site_type = rep("hydrometricStation", n),
      provider_id = rep("fixture-provider", n),
      provider_uri = rep("https://example.org/provider/fixture", n),
      provider_name = rep("Fixture Provider", n),
      provider_url = rep("https://example.org/provider", n),
      mainstem_uri = rep(NA_character_, n),
      landing_url = sprintf("https://example.org/landing/%02d", index),
      source_url = sprintf("https://example.org/profile/%02d", index)
    ),
    geometry = geometry
  )
}

fetch_plan_test_dataset_row <- function(
    site_uri,
    dataset_key,
    distribution_key,
    variable_key,
    provider_key,
    distribution_url,
    media_type = "text/csv",
    handler_id = "csv",
    fetchable = TRUE,
    temporal_start = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    temporal_end = as.POSIXct("2025-12-31 23:59:59", tz = "UTC"),
    provider_uri = paste0("https://example.org/provider/", provider_key),
    provider_name = paste("Provider", toupper(provider_key)),
    conforms_to = character()) {
  tibble::tibble(
    contract_version = "0.1.0",
    site_uri = site_uri,
    dataset_id = if (is.na(dataset_key)) NA_character_ else
      fetch_plan_test_hash(paste0("dataset:", dataset_key)),
    distribution_id = if (is.na(distribution_key)) NA_character_ else
      fetch_plan_test_hash(paste0("distribution:", distribution_key)),
    variable_id = if (is.na(variable_key)) NA_character_ else
      fetch_plan_test_hash(paste0("variable:", variable_key)),
    dataset_uri = if (is.na(dataset_key)) NA_character_ else
      paste0("https://example.org/dataset/", dataset_key),
    dataset_name = if (is.na(dataset_key)) NA_character_ else
      paste("Dataset", dataset_key),
    dataset_description = NA_character_,
    temporal_coverage = if (is.na(temporal_start) && is.na(temporal_end)) {
      NA_character_
    } else {
      paste(
        if (is.na(temporal_start)) ".." else
          format(temporal_start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        if (is.na(temporal_end)) ".." else
          format(temporal_end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        sep = "/"
      )
    },
    temporal_start = as.POSIXct(temporal_start, tz = "UTC"),
    temporal_end = as.POSIXct(temporal_end, tz = "UTC"),
    variable_uri = if (is.na(variable_key)) NA_character_ else
      paste0("https://example.org/variable/", variable_key),
    variable_name = if (is.na(variable_key)) NA_character_ else
      paste("Variable", variable_key),
    unit_uri = "https://qudt.org/vocab/unit/M3-PER-SEC",
    unit_label = "m3/s",
    measurement_technique = NA_character_,
    distribution_url = distribution_url,
    media_type = media_type,
    conforms_to = list(conforms_to),
    provider_uri = provider_uri,
    provider_name = provider_name,
    provider_url = if (is.na(provider_key)) NA_character_ else
      paste0("https://example.org/provider/", provider_key),
    license = "https://creativecommons.org/publicdomain/zero/1.0/",
    access_rights = "public",
    handler_id = handler_id,
    fetchable = fetchable,
    source_url = "https://example.org/profile/source"
  )
}

fetch_plan_test_datasets <- function(sites) {
  utc_na <- as.POSIXct(NA, tz = "UTC")
  rows <- list(
    fetch_plan_test_dataset_row(
      sites$site_uri[[1L]], "alpha", "alpha", "flow", "alpha",
      "https://example.org/data/alpha.csv"
    ),
    fetch_plan_test_dataset_row(
      sites$site_uri[[1L]], "alpha", "alpha", "temperature", "alpha",
      "https://example.org/data/alpha.csv"
    ),
    fetch_plan_test_dataset_row(
      sites$site_uri[[2L]], "outside", "outside", "flow", "alpha",
      "https://example.org/data/outside.csv",
      temporal_start = as.POSIXct("2023-01-01 00:00:00", tz = "UTC"),
      temporal_end = as.POSIXct("2023-12-31 23:59:59", tz = "UTC")
    ),
    fetch_plan_test_dataset_row(
      sites$site_uri[[3L]], "beta", "beta", "flow", "beta",
      "https://example.org/data/beta.csv",
      temporal_start = as.POSIXct("2025-06-15 00:00:00", tz = "UTC"),
      temporal_end = as.POSIXct("2025-07-15 00:00:00", tz = "UTC")
    ),
    fetch_plan_test_dataset_row(
      sites$site_uri[[4L]], "gamma", "gamma", "flow", "gamma",
      "https://example.org/data/gamma.csv",
      temporal_start = utc_na, temporal_end = utc_na
    ),
    fetch_plan_test_dataset_row(
      sites$site_uri[[5L]], "reference", "reference", "flow", "delta",
      "https://example.org/data/reference.bin",
      media_type = "application/octet-stream", handler_id = "unknown",
      fetchable = FALSE, temporal_start = utc_na, temporal_end = utc_na
    ),
    fetch_plan_test_dataset_row(
      sites$site_uri[[6L]], "disabled", "disabled", "flow", "epsilon",
      "https://example.org/data/disabled.csv",
      fetchable = FALSE, temporal_start = utc_na, temporal_end = utc_na,
      provider_uri = NA_character_, provider_name = "Provider AARDVARK"
    ),
    fetch_plan_test_dataset_row(
      sites$site_uri[[7L]], NA_character_, NA_character_, "unplannable", NA_character_,
      NA_character_, media_type = NA_character_, handler_id = "unknown",
      fetchable = FALSE, temporal_start = utc_na, temporal_end = utc_na,
      provider_uri = NA_character_, provider_name = NA_character_
    )
  )
  tibble::as_tibble(do.call(rbind, rows))
}

fetch_plan_test_completeness <- function(sites, datasets,
                                         status = "complete",
                                         truncated = FALSE) {
  dataset_rows <- nrow(datasets)
  partial <- !identical(status, "complete") || isTRUE(truncated)
  tibble::tibble(
    stage = c("sites", "datasets", "reference"),
    status = c("complete", status, "complete"),
    truncated = c(FALSE, truncated, FALSE),
    input_count = as.integer(c(
      nrow(sites), dataset_rows + if (partial) 1L else 0L, 0L
    )),
    attempted_count = as.integer(c(nrow(sites), dataset_rows, 0L)),
    succeeded_count = as.integer(c(nrow(sites), dataset_rows, 0L)),
    failed_count = c(0L, 0L, 0L),
    skipped_count = c(0L, if (partial) 1L else 0L, 0L),
    output_count = as.integer(c(nrow(sites), dataset_rows, 0L)),
    reason = c(
      NA_character_,
      if (partial) "The dataset catalog is intentionally incomplete." else NA_character_,
      NA_character_
    )
  )
}

fetch_plan_test_metadata <- function(sites, datasets,
                                     status = "complete",
                                     truncated = FALSE) {
  list(
    created_at = as.POSIXct("2026-07-14 10:00:00", tz = "UTC"),
    selection = list(
      include = c("sites", "datasets", "reference"),
      providers = character(),
      variables = character()
    ),
    completeness = fetch_plan_test_completeness(
      sites, datasets, status = status, truncated = truncated
    ),
    counts = list(
      sites = as.integer(nrow(sites)),
      datasets = as.integer(nrow(datasets)),
      reference_layers = 0L,
      reference_features = 0L,
      problems = 0L,
      requests = 0L
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
    source_contracts = c(aoi = "1.0.0", catalog = "0.1.0")
  )
}

fetch_plan_test_catalog <- function(populated = TRUE,
                                    status = "complete",
                                    truncated = FALSE) {
  sites <- fetch_plan_test_sites(if (populated) 7L else 0L)
  datasets <- if (populated) {
    fetch_plan_test_datasets(sites)
  } else {
    gx_catalog_empty_datasets()
  }
  gx_catalog_new_impl(
    aoi = fetch_plan_test_aoi(),
    sites = sites,
    datasets = datasets,
    reference = list(),
    problems = gx_catalog_empty_problems(),
    requests = gx_catalog_empty_requests(),
    metadata = fetch_plan_test_metadata(
      sites, datasets, status = status, truncated = truncated
    )
  )
}

fetch_plan_test_build <- function(catalog = fetch_plan_test_catalog(),
                                  time = fetch_plan_test_time(),
                                  max_datasets = 2L,
                                  max_requests = 17L,
                                  max_encoded_bytes = 123456,
                                  max_decoded_bytes = 654321) {
  gx_fetch_plan_impl(
    catalog = catalog,
    time = time,
    max_datasets = max_datasets,
    max_requests = max_requests,
    max_encoded_bytes = max_encoded_bytes,
    max_decoded_bytes = max_decoded_bytes,
    now = fetch_plan_test_now
  )
}

fetch_plan_test_clone <- function(x) {
  unserialize(serialize(x, NULL))
}
