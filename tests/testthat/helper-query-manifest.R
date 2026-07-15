gx_query_test_fixture_path <- function(name) {
  testthat::test_path("..", "fixtures", "sparql", name)
}

gx_query_test_contract <- function() {
  jsonlite::fromJSON(
    gx_query_test_fixture_path("query-template-contract-v2.json"),
    simplifyVector = FALSE
  )
}

gx_query_test_manifest <- function() {
  yaml::read_yaml(
    system.file("queries", "manifest.yml", package = "geoconnexr")
  )
}

gx_query_test_schema_path <- function() {
  system.file(
    "schema", "query-manifest-v2.json", package = "geoconnexr"
  )
}

gx_query_test_manifest_json <- function(manifest) {
  if (!is.null(manifest$endpoint_policy$reject_content_types)) {
    manifest$endpoint_policy$reject_content_types <- as.list(
      manifest$endpoint_policy$reject_content_types
    )
  }
  if (is.list(manifest$templates)) {
    manifest$templates <- lapply(manifest$templates, function(spec) {
      for (field in c("result_variables", "required_result_variables")) {
        if (!is.null(spec[[field]])) {
          spec[[field]] <- as.list(spec[[field]])
        }
      }
      if (!is.null(spec$order$variables)) {
        spec$order$variables <- as.list(spec$order$variables)
      }
      if (!is.null(spec$result_key$variables)) {
        spec$result_key$variables <- as.list(spec$result_key$variables)
      }
      if (!is.null(spec$pagination$blockers)) {
        spec$pagination$blockers <- as.list(spec$pagination$blockers)
      }
      if (is.list(spec$parameters)) {
        spec$parameters <- lapply(spec$parameters, function(parameter) {
          if (!is.null(parameter$geometry_types)) {
            parameter$geometry_types <- as.list(parameter$geometry_types)
          }
          parameter
        })
      }
      spec
    })
  }
  jsonlite::toJSON(
    manifest,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
}

gx_query_test_clone <- function(x) {
  unserialize(serialize(x, connection = NULL))
}

gx_query_test_asset_dir <- function(.local_envir = parent.frame()) {
  asset_dir <- withr::local_tempdir(
    pattern = "gx-query-assets-",
    .local_envir = .local_envir
  )
  source_dir <- system.file("queries", package = "geoconnexr")
  sources <- list.files(
    source_dir,
    pattern = "[.]rq$",
    full.names = TRUE
  )
  copied <- file.copy(sources, asset_dir, copy.mode = FALSE)
  if (!length(copied) || !all(copied)) {
    stop("Failed to prepare isolated query-template assets.", call. = FALSE)
  }
  asset_dir
}

gx_query_test_bundle_dir <- function(.local_envir = parent.frame()) {
  bundle_dir <- withr::local_tempdir(
    pattern = "gx-query-bundle-",
    .local_envir = .local_envir
  )
  source_dir <- system.file("queries", package = "geoconnexr")
  sources <- c(
    file.path(source_dir, "manifest.yml"),
    list.files(source_dir, pattern = "[.]rq$", full.names = TRUE)
  )
  copied <- file.copy(sources, bundle_dir, copy.mode = FALSE)
  if (!length(copied) || !all(copied)) {
    stop("Failed to prepare an isolated query bundle.", call. = FALSE)
  }
  bundle_dir
}

gx_query_test_rehash <- function(manifest, asset_dir, template) {
  path <- file.path(asset_dir, manifest$templates[[template]]$file)
  manifest$templates[[template]]$stored_bytes <- as.integer(file.info(path)$size)
  manifest$templates[[template]]$stored_sha256 <- digest::digest(
    file = path,
    algo = "sha256",
    serialize = FALSE
  )
  manifest
}

gx_query_test_valid_params <- function() {
  list(
    sites_on_mainstem = list(
      mainstem_uri = "https://geoconnex.us/ref/mainstems/1622734",
      limit = 100L,
      offset = 0L
    ),
    sites_in_aoi = list(
      aoi_wkt = "POLYGON((-108 35,-107 35,-107 36,-108 36,-108 35))",
      limit = 50L,
      offset = 0L
    ),
    datasets_for_sites = list(
      site_uris = c(
        "https://example.org/site/1",
        "https://example.org/site/2"
      ),
      limit = 100L,
      offset = 0L
    ),
    datasets_by_variable = list(
      variable_uri = "https://example.org/variable/discharge",
      limit = 100L,
      offset = 0L
    ),
    sites_by_provider = list(
      provider_uri = "https://example.org/provider/usgs",
      limit = 100L,
      offset = 0L
    ),
    provider_coverage = list(
      provider_uri = "https://example.org/provider/usgs",
      limit = 1L,
      offset = 0L
    )
  )
}

gx_query_test_structural_mutations <- function() {
  list(
    root_extra = function(x) {
      x$unexpected <- TRUE
      x
    },
    root_missing_version = function(x) {
      x$version <- NULL
      x
    },
    root_version_wrong = function(x) {
      x$version <- 1L
      x
    },
    contract_version_wrong = function(x) {
      x$contract_version <- "0.1.0"
      x
    },
    runtime_execution_enabled = function(x) {
      x$runtime$execution_enabled <- TRUE
      x
    },
    runtime_render_disabled = function(x) {
      x$runtime$render_enabled <- FALSE
      x
    },
    runtime_pagination_enabled = function(x) {
      x$runtime$pagination_enabled <- TRUE
      x
    },
    runtime_chunking_enabled = function(x) {
      x$runtime$chunking_enabled <- TRUE
      x
    },
    runtime_gate_bad = function(x) {
      x$runtime$gate <- "none"
      x
    },
    runtime_extra = function(x) {
      x$runtime$unexpected <- TRUE
      x
    },
    integrity_algorithm_bad = function(x) {
      x$integrity$algorithm <- "sha1"
      x
    },
    integrity_scope_bad = function(x) {
      x$integrity$scope <- "normalized_text"
      x
    },
    integrity_extra = function(x) {
      x$integrity$unexpected <- TRUE
      x
    },
    policy_extra = function(x) {
      x$endpoint_policy$unexpected <- TRUE
      x
    },
    policy_missing_accept = function(x) {
      x$endpoint_policy$accept <- NULL
      x
    },
    policy_method_bad = function(x) {
      x$endpoint_policy$method <- "GET"
      x
    },
    defaults_extra = function(x) {
      x$defaults$unexpected <- 1L
      x
    },
    defaults_missing_page_size = function(x) {
      x$defaults$page_size <- NULL
      x
    },
    defaults_page_size_zero = function(x) {
      x$defaults$page_size <- 0L
      x
    },
    template_name_bad = function(x) {
      names(x$templates)[1L] <- "1bad"
      x
    },
    template_extra = function(x) {
      x$templates$sites_on_mainstem$unexpected <- TRUE
      x
    },
    template_missing_file = function(x) {
      x$templates$sites_on_mainstem$file <- NULL
      x
    },
    template_file_traversal = function(x) {
      x$templates$sites_on_mainstem$file <- "../sites_on_mainstem.rq"
      x
    },
    template_query_type_bad = function(x) {
      x$templates$sites_on_mainstem$query_type <- "ask"
      x
    },
    template_bytes_fractional = function(x) {
      x$templates$sites_on_mainstem$stored_bytes <- 1.5
      x
    },
    template_hash_uppercase = function(x) {
      x$templates$sites_on_mainstem$stored_sha256 <- toupper(
        x$templates$sites_on_mainstem$stored_sha256
      )
      x
    },
    result_variables_missing = function(x) {
      x$templates$sites_on_mainstem$result_variables <- NULL
      x
    },
    result_variables_duplicate = function(x) {
      x$templates$sites_on_mainstem$result_variables <- c(
        x$templates$sites_on_mainstem$result_variables,
        "site"
      )
      x
    },
    result_variable_name_bad = function(x) {
      x$templates$sites_on_mainstem$result_variables[[1L]] <- "?site"
      x
    },
    required_result_variables_duplicate = function(x) {
      x$templates$sites_on_mainstem$required_result_variables <- c("site", "site")
      x
    },
    order_variables_duplicate = function(x) {
      x$templates$sites_on_mainstem$order$variables <- c(
        x$templates$sites_on_mainstem$order$variables,
        "site"
      )
      x
    },
    order_extra = function(x) {
      x$templates$sites_on_mainstem$order$unexpected <- TRUE
      x
    },
    order_missing_total = function(x) {
      x$templates$sites_on_mainstem$order$total <- NULL
      x
    },
    order_direction_bad = function(x) {
      x$templates$sites_on_mainstem$order$direction <- "descending"
      x
    },
    result_key_extra = function(x) {
      x$templates$sites_on_mainstem$result_key$unexpected <- TRUE
      x
    },
    result_key_missing_scope = function(x) {
      x$templates$sites_on_mainstem$result_key$scope <- NULL
      x
    },
    pagination_enabled = function(x) {
      x$templates$sites_on_mainstem$pagination$enabled <- TRUE
      x
    },
    pagination_missing_blockers = function(x) {
      x$templates$sites_on_mainstem$pagination$blockers <- NULL
      x
    },
    pagination_strategy_bad = function(x) {
      x$templates$sites_on_mainstem$pagination$candidate_strategy <- "cursor"
      x
    },
    pagination_blocker_bad = function(x) {
      x$templates$sites_on_mainstem$pagination$blockers[[1L]] <- "unknown"
      x
    },
    pagination_blocker_duplicate = function(x) {
      x$templates$sites_on_mainstem$pagination$blockers <- c(
        x$templates$sites_on_mainstem$pagination$blockers,
        x$templates$sites_on_mainstem$pagination$blockers[[1L]]
      )
      x
    },
    row_budget_fractional = function(x) {
      x$templates$sites_on_mainstem$row_budget <- 1.5
      x
    },
    result_key_name_bad = function(x) {
      x$templates$sites_on_mainstem$result_key$variables[[1L]] <- "?site"
      x
    },
    parameter_extra_key = function(x) {
      x$templates$sites_on_mainstem$parameters$mainstem_uri$minimum <- 0L
      x
    },
    parameter_missing_required = function(x) {
      x$templates$sites_on_mainstem$parameters$mainstem_uri$required <- NULL
      x
    },
    parameter_required_false = function(x) {
      x$templates$sites_on_mainstem$parameters$mainstem_uri$required <- FALSE
      x
    },
    http_iri_maximum_bytes_fractional = function(x) {
      x$templates$sites_on_mainstem$parameters$mainstem_uri$maximum_bytes <- 1.5
      x
    },
    uri_list_minimum_zero = function(x) {
      x$templates$datasets_for_sites$parameters$site_uris$minimum_items <- 0L
      x
    },
    integer_constraint_fractional = function(x) {
      x$templates$sites_on_mainstem$parameters$limit$maximum <- 1.5
      x
    },
    integer_irrelevant_constraint = function(x) {
      x$templates$sites_on_mainstem$parameters$limit$maximum_items <- 2L
      x
    },
    uri_list_unique_items_false = function(x) {
      x$templates$datasets_for_sites$parameters$site_uris$unique_items <- FALSE
      x
    },
    uri_list_sort_bad = function(x) {
      x$templates$datasets_for_sites$parameters$site_uris$sort <- "input"
      x
    },
    wkt_geometry_type_bad = function(x) {
      x$templates$sites_in_aoi$parameters$aoi_wkt$geometry_types[[1L]] <- "POINT"
      x
    },
    wkt_allow_empty_true = function(x) {
      x$templates$sites_in_aoi$parameters$aoi_wkt$allow_empty <- TRUE
      x
    }
  )
}
