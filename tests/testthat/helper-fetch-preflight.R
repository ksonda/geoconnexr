fetch_preflight_test_now <- function() {
  as.POSIXct("2026-07-15 15:45:00", tz = "UTC")
}

fetch_preflight_test_handler_specs <- function() {
  list(
    edr = list(
      url = "https://example.org/collections/streamflow/cube",
      media_type = "application/prs.coverage+json",
      fetchable = TRUE
    ),
    usgs_waterdata_continuous = list(
      url = paste0(
        "https://api.waterdata.usgs.gov/ogcapi/beta/collections/",
        "continuous/items"
      ),
      media_type = "application/geo+json",
      fetchable = TRUE
    ),
    usgs_waterdata_daily = list(
      url = paste0(
        "https://api.waterdata.usgs.gov/ogcapi/beta/collections/",
        "daily/items"
      ),
      media_type = "application/geo+json",
      fetchable = TRUE
    ),
    nwis_legacy_iv = list(
      url = "https://waterservices.usgs.gov/nwis/iv/",
      media_type = "application/json",
      fetchable = TRUE
    ),
    nwis_legacy_dv = list(
      url = "https://waterservices.usgs.gov/nwis/dv/",
      media_type = "application/json",
      fetchable = TRUE
    ),
    wqp = list(
      url = paste0(
        "https://www.waterqualitydata.us/data/Result/search?",
        "siteid=fixture"
      ),
      media_type = "text/csv",
      fetchable = TRUE
    ),
    ogc_api_features = list(
      url = "https://reference.geoconnex.us/collections/gages/items",
      media_type = "application/geo+json",
      fetchable = TRUE
    ),
    csv = list(
      url = "https://example.org/data/fixture.csv",
      media_type = "text/csv",
      fetchable = TRUE
    ),
    unknown = list(
      url = "https://example.org/data/reference.bin",
      media_type = "application/octet-stream",
      fetchable = FALSE
    )
  )
}

fetch_preflight_test_catalog <- function(handler_ids) {
  if (!is.character(handler_ids) || anyNA(handler_ids) ||
      anyDuplicated(handler_ids)) {
    stop("Test handler IDs must be unique character values.", call. = FALSE)
  }
  specs <- fetch_preflight_test_handler_specs()
  if (any(!handler_ids %in% names(specs))) {
    stop("A requested test handler has no fixture specification.", call. = FALSE)
  }
  sites <- fetch_plan_test_sites(length(handler_ids))
  if (!length(handler_ids)) {
    datasets <- gx_catalog_empty_datasets()
  } else {
    rows <- Map(function(handler_id, index) {
      spec <- specs[[handler_id]]
      fetch_plan_test_dataset_row(
        site_uri = sites$site_uri[[index]],
        dataset_key = paste0("preflight-", index, "-", handler_id),
        distribution_key = paste0("preflight-", index, "-", handler_id),
        variable_key = paste0("preflight-", index),
        provider_key = sprintf("preflight-%02d", index),
        distribution_url = spec$url,
        media_type = spec$media_type,
        handler_id = handler_id,
        fetchable = spec$fetchable
      )
    }, handler_ids, seq_along(handler_ids))
    datasets <- tibble::as_tibble(do.call(rbind, rows))
  }
  gx_catalog_new_impl(
    aoi = fetch_plan_test_aoi(),
    sites = sites,
    datasets = datasets,
    reference = list(),
    problems = gx_catalog_empty_problems(),
    requests = gx_catalog_empty_requests(),
    metadata = fetch_plan_test_metadata(sites, datasets)
  )
}

fetch_preflight_test_plan <- function(handler_ids, max_datasets = NULL) {
  if (is.null(max_datasets)) {
    max_datasets <- sum(handler_ids != "unknown")
  }
  fetch_plan_test_build(
    catalog = fetch_preflight_test_catalog(handler_ids),
    max_datasets = as.integer(max_datasets)
  )
}

fetch_preflight_test_resolver <- function(versions = character(), calls = NULL) {
  if (is.null(calls)) {
    calls <- new.env(parent = emptyenv())
  }
  calls$packages <- character()
  force(versions)
  function(package) {
    calls$packages <- c(calls$packages, package)
    position <- match(package, names(versions))
    if (is.na(position)) NA_character_ else unname(versions[[position]])
  }
}

fetch_preflight_test_build <- function(
    plan = fetch_plan_test_build(),
    versions = c(readr = "2.1.6"),
    calls = NULL,
    resolver = NULL) {
  if (is.null(resolver)) {
    resolver <- fetch_preflight_test_resolver(versions, calls)
  }
  gx_fetch_preflight_impl(
    plan,
    version_resolver = resolver,
    now = fetch_preflight_test_now
  )
}

fetch_preflight_test_clone <- function(x) {
  unserialize(serialize(x, NULL))
}

fetch_preflight_test_error <- function(expr) {
  tryCatch(expr, error = identity)
}
