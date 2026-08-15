writer_test_hash <- function(x) {
  digest::digest(enc2utf8(x), algo = "sha256", serialize = FALSE)
}

writer_test_clock <- function() {
  as.POSIXct("2026-07-15 12:34:56", tz = "UTC")
}

test_that("snapshot timestamps preserve rounded microseconds exactly", {
  stamp <- as.POSIXct("2026-08-15 04:27:54", tz = "UTC") + 0.613437
  text <- gx_snapshot_writer_time(stamp)
  parsed <- gx_snapshot_catalog_view_time_impl(
    text, "test", allow_blank = FALSE
  )
  rebound <- gx_snapshot_time_text_impl(parsed)

  expect_identical(text, "2026-08-15T04:27:54.613437Z")
  expect_identical(rebound, text)
})

writer_test_aoi <- function() {
  gx_aoi("02070010", type = "huc")
}

writer_test_sites <- function(n = 1L, sensitive = FALSE) {
  n <- as.integer(n)
  if (!n) return(gx_catalog_empty_sites())
  index <- seq_len(n)
  suffix <- sprintf("%03d", index)
  site_uri <- paste0("https://example.org/site/", suffix)
  source_url <- paste0("https://example.org/profile/", suffix)
  if (sensitive) {
    site_uri <- paste0(site_uri, "?token=SECRET_SITE_", suffix)
    source_url <- paste0(source_url, "?key=SECRET_SOURCE_", suffix)
  }
  geometry <- lapply(index, function(i) {
    sf::st_point(c(-79 + i / 8, 36 + i / 8))
  })
  sf::st_sf(
    tibble::tibble(
      contract_version = rep("0.1.0", n),
      site_uri = site_uri,
      name = paste0("Caf\u00e9, \"quoted\" site ", suffix),
      description = rep("UTF-8 snowman \u2603", n),
      site_type = rep("hydrometricStation", n),
      provider_id = paste0("provider-", suffix),
      provider_uri = rep("https://example.org/provider/fixture", n),
      provider_name = rep("Fixture Provider", n),
      provider_url = rep("https://example.org/provider", n),
      mainstem_uri = rep(NA_character_, n),
      landing_url = paste0("https://example.org/landing/", suffix),
      source_url = source_url
    ),
    geometry = sf::st_sfc(geometry, crs = "OGC:CRS84")
  )
}

writer_test_datasets <- function(sites, sensitive = FALSE) {
  n <- nrow(sites)
  if (!n) return(gx_catalog_empty_datasets())
  index <- seq_len(n)
  suffix <- sprintf("%03d", index)
  distribution_url <- paste0("https://example.org/data/", suffix, ".csv")
  source_url <- paste0("https://example.org/profile/", suffix)
  conforms <- rep(list(c(
    "https://example.org/spec/a", "https://example.org/spec/b"
  )), n)
  license <- rep("https://creativecommons.org/publicdomain/zero/1.0/", n)
  if (sensitive) {
    distribution_url <- paste0(
      distribution_url, "?token=SECRET_DISTRIBUTION_", suffix
    )
    source_url <- paste0(source_url, "?key=SECRET_DATASET_SOURCE_", suffix)
    conforms <- lapply(suffix, function(id) {
      paste0("https://example.org/spec/a?token=SECRET_CONFORMS_", id)
    })
    license <- paste0(
      "https://example.org/license?token=SECRET_LICENSE_", suffix
    )
  }
  tibble::tibble(
    contract_version = rep("0.1.0", n),
    site_uri = sites$site_uri,
    dataset_id = vapply(paste0("dataset-", suffix), writer_test_hash, character(1)),
    distribution_id = vapply(
      paste0("distribution-", suffix), writer_test_hash, character(1)
    ),
    variable_id = vapply(paste0("variable-", suffix), writer_test_hash, character(1)),
    dataset_uri = paste0("https://example.org/dataset/", suffix),
    dataset_name = paste0("Dataset, \"quoted\" ", suffix),
    dataset_description = rep("D\u00e9bit journalier", n),
    temporal_coverage = rep(
      "2024-01-01T00:00:00Z/2024-12-31T00:00:00Z", n
    ),
    temporal_start = as.POSIXct(rep("2024-01-01 00:00:00", n), tz = "UTC"),
    temporal_end = as.POSIXct(rep("2024-12-31 00:00:00", n), tz = "UTC"),
    variable_uri = paste0("https://example.org/variable/", suffix),
    variable_name = paste0("Discharge ", suffix),
    unit_uri = rep("https://qudt.org/vocab/unit/M3-PER-SEC", n),
    unit_label = rep("m3/s", n),
    measurement_technique = rep(NA_character_, n),
    distribution_url = distribution_url,
    media_type = rep("text/csv", n),
    conforms_to = conforms,
    provider_uri = rep("https://example.org/provider/fixture", n),
    provider_name = rep("Fixture Provider", n),
    provider_url = rep("https://example.org/provider", n),
    license = license,
    access_rights = rep("public", n),
    handler_id = rep("csv", n),
    fetchable = rep(TRUE, n),
    source_url = source_url
  )
}

writer_test_problems <- function(n = 1L) {
  n <- as.integer(n)
  if (!n) return(gx_catalog_empty_problems())
  index <- seq_len(n)
  tibble::tibble(
    stage = rep("datasets", n),
    source_uri = paste0("https://example.org/problem/", index),
    path = paste0("/datasets/", index - 1L),
    code = paste0("fixture_warning_", index),
    severity = rep("warning", n),
    message = paste0("Recoverable, \"quoted\" warning ", index),
    recoverable = rep(TRUE, n),
    occurred_at = as.POSIXct(rep("2026-07-15 11:00:00", n), tz = "UTC")
  )
}

writer_test_requests <- function(n = 1L) {
  n <- as.integer(n)
  if (!n) return(gx_catalog_empty_requests())
  index <- seq_len(n)
  urls <- paste0("https://example.org/request/", index)
  tibble::tibble(
    request_id = paste0("writer-request-", index),
    stage = rep(c("sites", "datasets"), length.out = n),
    method = rep("GET", n),
    canonical_url_redacted = urls,
    request_hash = vapply(paste("GET", urls), writer_test_hash, character(1)),
    body_hash = rep(NA_character_, n),
    final_url = urls,
    response_status = rep(200L, n),
    response_media_type = rep("application/json", n),
    encoded_bytes = as.integer(100 + index),
    decoded_bytes = as.integer(200 + index),
    content_hash = vapply(paste0("body-", index), writer_test_hash, character(1)),
    etag = paste0("\"fixture-", index, "\""),
    last_modified = rep(NA_character_, n),
    retrieved_at = as.POSIXct(rep("2026-07-15 11:30:00", n), tz = "UTC"),
    elapsed_ms = as.double(index) + 0.25,
    cache_origin = rep("network", n),
    error_code = rep(NA_character_, n)
  )
}

writer_test_completeness <- function(sites, datasets) {
  output <- c(nrow(sites), nrow(datasets), 0L)
  tibble::tibble(
    stage = c("sites", "datasets", "reference"),
    status = rep("complete", 3L),
    truncated = rep(FALSE, 3L),
    input_count = as.integer(output),
    attempted_count = as.integer(output),
    succeeded_count = as.integer(output),
    failed_count = rep(0L, 3L),
    skipped_count = rep(0L, 3L),
    output_count = as.integer(output),
    reason = rep(NA_character_, 3L)
  )
}

writer_test_catalog <- function(n = 1L, sensitive = FALSE) {
  sites <- writer_test_sites(n, sensitive = sensitive)
  datasets <- writer_test_datasets(sites, sensitive = sensitive)
  problems <- writer_test_problems(if (n) n else 0L)
  requests <- writer_test_requests(if (n) n else 0L)
  metadata <- list(
    created_at = as.POSIXct("2026-07-15 10:00:00", tz = "UTC"),
    selection = list(
      include = c("sites", "datasets", "reference"),
      providers = character(), variables = character()
    ),
    completeness = writer_test_completeness(sites, datasets),
    counts = list(
      sites = as.integer(nrow(sites)),
      datasets = as.integer(nrow(datasets)),
      reference_layers = 0L, reference_features = 0L,
      problems = as.integer(nrow(problems)),
      requests = as.integer(nrow(requests))
    ),
    endpoints = c(
      graph = "https://example.org/sparql",
      reference = "https://example.org/reference"
    ),
    hydrologic_vintage = list(
      reference_collection = NA_character_, vintage = NA_character_,
      migration_policy = "not_checked"
    ),
    source_contracts = c(aoi = "1.0.0", catalog = "0.1.0")
  )
  gx_catalog_new_impl(
    writer_test_aoi(), sites, datasets, list(), problems, requests, metadata
  )
}

writer_test_copy_catalog <- function(x, reverse = FALSE) {
  indexes <- function(n) if (reverse) rev(seq_len(n)) else seq_len(n)
  sites <- x$sites[indexes(nrow(x$sites)), , drop = FALSE]
  datasets <- x$datasets[indexes(nrow(x$datasets)), , drop = FALSE]
  problems <- x$problems[indexes(nrow(x$problems)), , drop = FALSE]
  requests <- x$requests[indexes(nrow(x$requests)), , drop = FALSE]
  gx_catalog_new_impl(
    x$aoi, sites, datasets, list(), problems, requests, x$metadata
  )
}

writer_test_parent <- function(.env = parent.frame()) {
  withr::local_tempdir(pattern = "gx-snapshot-write-", .local_envir = .env)
}

writer_test_read_raw <- function(path) {
  readBin(path, "raw", n = file.info(path)$size[[1L]])
}

writer_test_read_text <- function(path) {
  text <- rawToChar(writer_test_read_raw(path))
  Encoding(text) <- "UTF-8"
  text
}

writer_test_manifest <- function(dir) {
  jsonlite::fromJSON(
    file.path(dir, "manifest.json"), simplifyVector = FALSE
  )
}

writer_test_stage_paths <- function(parent) {
  list.files(
    parent, pattern = "^[.]gx-snapshot-stage-", all.files = TRUE,
    full.names = TRUE
  )
}

writer_test_expect_error <- function(code, subclass = NULL) {
  condition <- tryCatch(code(), error = identity)
  expect_s3_class(condition, "gx_error_snapshot_write")
  if (!is.null(subclass)) expect_s3_class(condition, subclass)
  invisible(condition)
}

writer_test_csv <- function(path) {
  utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character", na.strings = character()
  )
}

test_that("empty and populated catalogs write closed snapshots that M9a verifies", {
  testthat::local_mocked_bindings(gx_now = writer_test_clock, .package = "geoconnexr")
  for (n in c(0L, 1L)) {
    parent <- writer_test_parent()
    target <- file.path(parent, paste0("snapshot-", n))
    output <- gx_snapshot_write_catalog_impl(writer_test_catalog(n), target)

    expect_identical(output$status, "written")
    expect_identical(output$mode, "catalog_snapshot_write")
    expect_identical(output$verification$status, "verified")
    expect_identical(gx_snapshot_verify_impl(target)$status, "verified")
    files <- sort(list.files(target, recursive = TRUE), method = "radix")
    expect_identical(files, sort(c(
      "catalog/sites.csv", "catalog/datasets.csv", "catalog/problems.csv",
      "manifest.json", "requests.csv"
    ), method = "radix"))
    expect_length(writer_test_stage_paths(parent), 0L)
  }
})

test_that("manifest declares exactly four required resources and no replay", {
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(), target)
  manifest <- writer_test_manifest(target)
  paths <- vapply(manifest$resources, `[[`, character(1), "path")

  expect_identical(sort(paths, method = "radix"), sort(unname(
    gx_snapshot_writer_paths
  ), method = "radix"))
  expect_length(paths, 4L)
  expect_true(all(vapply(manifest$resources, `[[`, logical(1), "required")))
  expect_false(manifest$replay$replayable)
  expect_identical(
    unlist(manifest$replay$non_replayable_reasons, use.names = FALSE),
    "catalog_only_writer_v0_1"
  )
  expect_identical(manifest$recipe$pipeline$end_stage, "catalog")
})

test_that("CSV serialization is quote-all UTF-8 LF with WKT and JSON arrays", {
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(sensitive = TRUE), target)
  paths <- file.path(target, unname(gx_snapshot_writer_paths))
  names(paths) <- names(gx_snapshot_writer_paths)
  texts <- vapply(paths, writer_test_read_text, character(1))
  expect_true(all(stringi::stri_enc_isutf8(texts)))

  expect_true(all(endsWith(texts, "\n")))
  expect_false(any(grepl("\r", texts, fixed = TRUE)))
  expect_true(all(startsWith(texts, "\"")))
  expect_match(texts[["sites"]], "geometry_wkt", fixed = TRUE)
  expect_match(texts[["sites"]], "POINT (-78.875 36.125)", fixed = TRUE)
  expect_match(texts[["sites"]], "Caf\u00e9, \"\"quoted\"\"", fixed = TRUE)
  expect_match(texts[["datasets"]], "D\u00e9bit journalier", fixed = TRUE)
  expect_match(texts[["datasets"]], "[\"\"https://example.org/spec/a?[redacted]\"\"]", fixed = TRUE)
  has_bom <- vapply(paths, function(path) {
    bytes <- writer_test_read_raw(path)
    length(bytes) >= 3L && identical(
      bytes[seq_len(3L)], as.raw(c(0xef, 0xbb, 0xbf))
    )
  }, logical(1))
  expect_false(any(has_bom))
})

test_that("URL secrets and user information never reach snapshot bytes", {
  view <- data.frame(
    source_url = "https://user:TOPSECRET@example.org/path?token=TOPSECRET",
    stringsAsFactors = FALSE
  )
  expect_identical(
    gx_snapshot_writer_redact_view(view)$source_url,
    "https://example.org/path?[redacted]"
  )

  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  catalog <- writer_test_catalog(sensitive = TRUE)
  catalog$metadata$selection$providers <-
    "https://example.org/provider?token=SECRET_SELECTION"
  catalog$metadata$selection$variables <-
    "https://example.org/variable?token=SECRET_VARIABLE"
  catalog$requests$canonical_url_redacted <-
    "https://example.org/request?[redacted]"
  catalog$requests$final_url <- "https://example.org/request?[redacted]"
  gx_catalog_validate_impl(catalog)
  gx_snapshot_write_catalog_impl(catalog, target)
  files <- list.files(target, recursive = TRUE, full.names = TRUE)
  bytes <- paste(vapply(files, function(path) {
    writer_test_read_text(path)
  }, character(1)), collapse = "\n")
  for (secret in c(
    "SECRET_SITE", "SECRET_SOURCE", "SECRET_DISTRIBUTION",
    "SECRET_DATASET_SOURCE", "SECRET_CONFORMS", "SECRET_LICENSE",
    "SECRET_SELECTION", "SECRET_VARIABLE", "TOPSECRET", "user:"
  )) {
    expect_false(grepl(secret, bytes, fixed = TRUE), info = secret)
  }
  expect_match(bytes, "?[redacted]", fixed = TRUE)
  expect_null(writer_test_manifest(target)$requests[[1L]]$final_url)
})

test_that("embedded request ledger and requests.csv are exactly bound", {
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(n = 2L), target)
  manifest <- writer_test_manifest(target)
  csv <- writer_test_csv(file.path(target, "requests.csv"))

  expect_equal(nrow(csv), length(manifest$requests))
  expect_identical(names(csv), gx_snapshot_writer_request_fields)
  for (i in seq_along(manifest$requests)) {
    for (field in gx_snapshot_writer_request_fields) {
      value <- manifest$requests[[i]][[field]]
      expected <- if (is.null(value)) {
        ""
      } else if (is.logical(value)) {
        if (value) "true" else "false"
      } else {
        as.character(value)
      }
      expect_identical(csv[[field]][[i]], expected, info = field)
    }
  }
  resource <- manifest$resources[[match(
    "requests.csv", vapply(manifest$resources, `[[`, character(1), "path")
  )]]
  expect_equal(resource$bytes, file.info(file.path(target, "requests.csv"))$size)
  expect_identical(resource$sha256, digest::digest(
    file = file.path(target, "requests.csv"), algo = "sha256", serialize = FALSE
  ))
})

test_that("M9e loads canonical request exports as exact typed ledgers", {
  testthat::local_mocked_bindings(
    gx_now = writer_test_clock,
    .package = "geoconnexr"
  )
  for (n in c(0L, 2L)) {
    parent <- writer_test_parent()
    target <- file.path(parent, paste0("snapshot-", n))
    catalog <- writer_test_catalog(n)
    gx_snapshot_write_catalog_impl(catalog, target)

    output <- gx_snapshot_load_requests_impl(target)

    expect_s3_class(output, "gx_snapshot_request_export")
    expect_identical(class(output), "gx_snapshot_request_export")
    expect_identical(output$contract_version, "0.1.0")
    expect_identical(output$mode, "catalog_request_export")
    expect_identical(output$status, "loaded_and_bound")
    expect_identical(output$request_count, n)
    expect_identical(
      output$path,
      normalizePath(target, winslash = "/", mustWork = TRUE)
    )
    expected_requests <- catalog$requests
    expected_requests$request_hash <- unname(expected_requests$request_hash)
    expected_requests$content_hash <- unname(expected_requests$content_hash)
    expect_identical(output$requests, expected_requests)
    expect_match(output$manifest_sha256, "^[0-9a-f]{64}$")
    expect_match(output$resource_sha256, "^[0-9a-f]{64}$")
    expect_match(output$export_id, "^[0-9a-f]{64}$")
    expect_identical(
      gx_snapshot_request_export_validate_impl(output),
      invisible(output)
    )
  }
})

test_that("M9e rejects noncanonical request bytes despite valid tree hashes", {
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(), target)
  request_path <- file.path(target, "requests.csv")
  modified <- c(writer_test_read_raw(request_path), charToRaw("\n"))
  writeBin(modified, request_path)

  manifest <- writer_test_manifest(target)
  paths <- vapply(manifest$resources, `[[`, character(1), "path")
  position <- match("requests.csv", paths)
  manifest$resources[[position]]$bytes <- length(modified)
  manifest$resources[[position]]$sha256 <- digest::digest(
    modified,
    algo = "sha256",
    serialize = FALSE
  )
  writeBin(
    gx_snapshot_writer_json_bytes(manifest),
    file.path(target, "manifest.json")
  )

  expect_identical(gx_snapshot_verify_impl(target)$status, "verified")
  expect_error(
    gx_snapshot_load_requests_impl(target),
    class = "gx_error_snapshot_request_load_binding"
  )
})

test_that("M9e accepts only the fixed catalog request-export profile", {
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(), target)
  manifest <- writer_test_manifest(target)
  manifest$effective_options$serialization$request_export <- "future-profile"
  writeBin(
    gx_snapshot_writer_json_bytes(manifest),
    file.path(target, "manifest.json")
  )

  expect_identical(gx_snapshot_verify_impl(target)$status, "verified")
  expect_error(
    gx_snapshot_load_requests_impl(target),
    class = "gx_error_snapshot_request_load_profile"
  )
})

test_that("M9e evidence and loading budgets fail closed", {
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(), target)
  output <- gx_snapshot_load_requests_impl(target)

  forged <- unserialize(serialize(output, NULL))
  forged$requests$cache_origin[[1L]] <- "offline_cache"
  expect_error(
    gx_snapshot_request_export_validate_impl(forged),
    class = "gx_error_snapshot_request_load_evidence"
  )

  request_size <- file.info(file.path(target, "requests.csv"))$size[[1L]]
  expect_error(
    testthat::with_mocked_bindings(
      gx_snapshot_load_requests_impl(target),
      gx_snapshot_request_load_max_bytes = request_size - 1,
      .package = "geoconnexr"
    ),
    class = "gx_error_snapshot_request_load_budget"
  )
})

test_that("M9e request loading is offline and read-only", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or write seam", call. = FALSE)
  }
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(), target)

  expect_no_error(testthat::with_mocked_bindings(
    gx_snapshot_load_requests_impl(target),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_snapshot_writer_write_csv = blocked,
    gx_snapshot_writer_write_raw = blocked,
    gx_snapshot_writer_unlink = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9f publishes exact read-only request-ledger evidence", {
  testthat::local_mocked_bindings(
    gx_now = writer_test_clock,
    .package = "geoconnexr"
  )
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  catalog <- writer_test_catalog(n = 2L)
  gx_snapshot_write_catalog_impl(catalog, target)

  output <- gx_snapshot_requests(target)

  expect_s3_class(output, "gx_snapshot_request_export")
  expect_identical(output$status, "loaded_and_bound")
  expect_identical(output$request_count, 2L)
  expect_identical(names(output$requests), .gx_catalog_request_columns)
  expect_identical(
    gx_snapshot_request_export_validate_impl(output),
    invisible(output)
  )
  expect_identical(
    output$manifest_sha256,
    gx_snapshot_verify(target)$manifest_sha256
  )
  printed <- capture.output(print(output), type = "message")
  expect_true(any(grepl(
    "canonical CSV bytes to typed manifest ledger",
    printed,
    fixed = TRUE
  )))

  forged <- unserialize(serialize(output, NULL))
  forged$path <- dirname(forged$path)
  expect_error(
    gx_snapshot_request_export_validate_impl(forged),
    class = "gx_error_snapshot_request_load_evidence"
  )
})

test_that("M9g loads exact redacted catalog CSV character tables", {
  testthat::local_mocked_bindings(
    gx_now = writer_test_clock,
    .package = "geoconnexr"
  )
  for (n in c(0L, 2L)) {
    parent <- writer_test_parent()
    target <- file.path(parent, paste0("snapshot-", n))
    catalog <- writer_test_catalog(n)
    gx_snapshot_write_catalog_impl(catalog, target)
    views <- gx_catalog_export_views_impl(catalog)

    for (resource in c("sites", "datasets", "problems")) {
      output <- gx_snapshot_load_catalog_csv_impl(target, resource)
      expected <- gx_snapshot_writer_redact_view(
        gx_snapshot_writer_character_view(views[[resource]])
      )
      expected <- lapply(expected, function(column) {
        column <- as.character(column)
        column[is.na(column)] <- ""
        unname(column)
      })
      expected <- as.data.frame(
        expected,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      expected <- tibble::as_tibble(expected)

      expect_s3_class(output, "gx_snapshot_catalog_csv")
      expect_identical(output$resource, resource)
      expect_identical(output$status, "loaded_and_bound")
      expect_identical(output$table, expected)
      expect_identical(output$rows, as.integer(nrow(expected)))
      expect_identical(output$columns, as.integer(ncol(expected)))
      expect_match(output$manifest_sha256, "^[0-9a-f]{64}$")
      expect_match(output$resource_sha256, "^[0-9a-f]{64}$")
      expect_match(output$table_id, "^[0-9a-f]{64}$")
      expect_identical(
        gx_snapshot_catalog_csv_validate_impl(output),
        invisible(output)
      )
    }
  }
})

test_that("M9g parser accepts only canonical quote-all UTF-8 LF bytes", {
  expected <- c("left", "right")
  canonical <- charToRaw("\"left\",\"right\"\n\"a\",\"b\"\n")
  parsed <- gx_snapshot_csv_parse_impl(canonical, expected, 1L)
  expect_identical(parsed, tibble::tibble(left = "a", right = "b"))
  expect_identical(gx_snapshot_csv_bytes_impl(parsed), canonical)

  cases <- list(
    unquoted = charToRaw("left,right\n\"a\",\"b\"\n"),
    crlf = charToRaw("\"left\",\"right\"\r\n\"a\",\"b\"\r\n"),
    extra_blank = c(canonical, charToRaw("\n")),
    bom = c(as.raw(c(0xef, 0xbb, 0xbf)), canonical),
    wrong_header = charToRaw("\"right\",\"left\"\n\"b\",\"a\"\n")
  )
  for (name in names(cases)) {
    expect_error(
      gx_snapshot_csv_parse_impl(cases[[name]], expected, 1L),
      class = "gx_error_snapshot_csv_load",
      info = name
    )
  }
})

test_that("M9g rejects rehashed noncanonical catalog CSV bytes", {
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(), target)
  resource_path <- file.path(target, "catalog", "sites.csv")
  modified <- c(writer_test_read_raw(resource_path), charToRaw("\n"))
  writeBin(modified, resource_path)

  manifest <- writer_test_manifest(target)
  paths <- vapply(manifest$resources, `[[`, character(1), "path")
  position <- match("catalog/sites.csv", paths)
  manifest$resources[[position]]$bytes <- length(modified)
  manifest$resources[[position]]$sha256 <- digest::digest(
    modified,
    algo = "sha256",
    serialize = FALSE
  )
  writeBin(
    gx_snapshot_writer_json_bytes(manifest),
    file.path(target, "manifest.json")
  )

  expect_identical(gx_snapshot_verify_impl(target)$status, "verified")
  expect_error(
    gx_snapshot_load_catalog_csv_impl(target, "sites"),
    class = "gx_error_snapshot_csv_load"
  )
})

test_that("M9g profile, evidence, and budgets fail closed", {
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(), target)
  output <- gx_snapshot_load_catalog_csv_impl(target, "datasets")

  forged <- unserialize(serialize(output, NULL))
  forged$table$dataset_name[[1L]] <- "forged"
  expect_error(
    gx_snapshot_catalog_csv_validate_impl(forged),
    class = "gx_error_snapshot_csv_load_evidence"
  )
  expect_error(
    gx_snapshot_load_catalog_csv_impl(target, "requests"),
    class = "gx_error_snapshot_csv_load_input"
  )

  resource_size <- file.info(
    file.path(target, "catalog", "datasets.csv")
  )$size[[1L]]
  expect_error(
    testthat::with_mocked_bindings(
      gx_snapshot_load_catalog_csv_impl(target, "datasets"),
      gx_snapshot_csv_load_max_bytes = resource_size - 1,
      .package = "geoconnexr"
    ),
    class = "gx_error_snapshot_csv_load_budget"
  )
})

test_that("M9g catalog CSV loading is offline and read-only", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or write seam", call. = FALSE)
  }
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(), target)

  expect_no_error(testthat::with_mocked_bindings(
    gx_snapshot_load_catalog_csv_impl(target, "problems"),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_snapshot_writer_write_csv = blocked,
    gx_snapshot_writer_write_raw = blocked,
    gx_snapshot_writer_unlink = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9h derives exact typed redacted catalog views", {
  testthat::local_mocked_bindings(
    gx_now = writer_test_clock,
    .package = "geoconnexr"
  )
  for (n in c(0L, 2L)) {
    parent <- writer_test_parent()
    target <- file.path(parent, paste0("snapshot-", n))
    gx_snapshot_write_catalog_impl(writer_test_catalog(n), target)

    output <- gx_snapshot_load_catalog_view_impl(target)

    expect_s3_class(output, "gx_snapshot_catalog_view")
    expect_identical(output$contract_version, "0.1.0")
    expect_identical(output$mode, "redacted_catalog_view")
    expect_identical(output$status, "loaded_and_verified")
    expect_s3_class(output$verification, "gx_snapshot_verification")
    expect_s3_class(output$sites, "sf")
    expect_s3_class(sf::st_geometry(output$sites), "sfc_POINT")
    expect_s3_class(output$datasets, "tbl_df")
    expect_s3_class(output$problems, "tbl_df")
    expect_s3_class(output$requests, "tbl_df")
    expect_true(inherits(output$datasets$temporal_start, "POSIXct"))
    expect_true(inherits(output$datasets$temporal_end, "POSIXct"))
    expect_true(is.list(output$datasets$conforms_to))
    expect_true(is.logical(output$datasets$fetchable))
    expect_true(is.logical(output$problems$recoverable))
    expect_true(inherits(output$problems$occurred_at, "POSIXct"))
    expect_identical(output$metadata$counts, list(
      sites = n,
      datasets = n,
      problems = n,
      requests = n
    ))
    expect_identical(output$metadata$blank_cells, "preserved_as_empty_strings")
    expect_identical(
      output$metadata$identity_policy,
      "redacted_values_not_reconstructed"
    )
    expect_false(output$metadata$replayable)
    expect_match(output$view_id, "^[0-9a-f]{64}$")
    expect_identical(
      gx_snapshot_catalog_view_validate_impl(output),
      invisible(output)
    )
  }
})

test_that("M9h preserves redaction and does not reconstruct discarded identities", {
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(
    writer_test_catalog(sensitive = TRUE),
    target
  )

  output <- gx_snapshot_load_catalog_view_impl(target)

  expect_match(output$sites$site_uri[[1L]], "?[redacted]", fixed = TRUE)
  expect_match(
    output$datasets$distribution_url[[1L]],
    "?[redacted]",
    fixed = TRUE
  )
  expect_true(any(grepl(
    "?[redacted]",
    output$datasets$conforms_to[[1L]],
    fixed = TRUE
  )))
  expect_identical(output$sites$mainstem_uri[[1L]], "")
  expect_identical(output$datasets$measurement_technique[[1L]], "")
  collect_text <- function(x) {
    if (is.character(x)) return(x)
    if (!is.list(x)) return(character())
    unlist(lapply(x, collect_text), use.names = FALSE)
  }
  expect_false(any(grepl(
    "SECRET_",
    collect_text(output),
    fixed = TRUE
  )))
})

test_that("M9h rejects canonical CSV with invalid typed field values", {
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(), target)
  evidence <- gx_snapshot_load_catalog_csv_impl(target, "datasets")
  table <- evidence$table
  table$fetchable[[1L]] <- "yes"
  modified <- gx_snapshot_csv_bytes_impl(table)
  resource_path <- file.path(target, "catalog", "datasets.csv")
  writeBin(modified, resource_path)

  manifest <- writer_test_manifest(target)
  paths <- vapply(manifest$resources, `[[`, character(1), "path")
  position <- match("catalog/datasets.csv", paths)
  manifest$resources[[position]]$bytes <- length(modified)
  manifest$resources[[position]]$sha256 <- digest::digest(
    modified,
    algo = "sha256",
    serialize = FALSE
  )
  writeBin(
    gx_snapshot_writer_json_bytes(manifest),
    file.path(target, "manifest.json")
  )

  expect_s3_class(
    gx_snapshot_load_catalog_csv_impl(target, "datasets"),
    "gx_snapshot_catalog_csv"
  )
  expect_error(
    gx_snapshot_load_catalog_view_impl(target),
    class = "gx_error_snapshot_catalog_view_type"
  )
})

test_that("M9h combined evidence forgery fails closed", {
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(), target)
  output <- gx_snapshot_load_catalog_view_impl(target)

  mutations <- list(
    typed = function(x) {
      x$datasets$dataset_name[[1L]] <- "forged"
      x
    },
    raw = function(x) {
      x$resource_evidence$datasets$table$dataset_name[[1L]] <- "forged"
      x
    },
    request = function(x) {
      x$requests$cache_origin[[1L]] <- "offline_cache"
      x
    },
    metadata = function(x) {
      x$metadata$replayable <- TRUE
      x
    },
    identity = function(x) {
      x$view_id <- paste(rep("0", 64L), collapse = "")
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(output, NULL)))
    expect_error(
      gx_snapshot_catalog_view_validate_impl(forged),
      class = "gx_error_snapshot_catalog_view",
      info = name
    )
  }
})

test_that("M9h typed redacted-view loading is offline and read-only", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or write seam", call. = FALSE)
  }
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(), target)

  expect_no_error(testthat::with_mocked_bindings(
    gx_snapshot_load_catalog_view_impl(target),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_snapshot_writer_write_csv = blocked,
    gx_snapshot_writer_write_raw = blocked,
    gx_snapshot_writer_unlink = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("M9i publicly exposes the exact typed redacted catalog view", {
  testthat::local_mocked_bindings(
    gx_now = writer_test_clock,
    .package = "geoconnexr"
  )
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("blocked external or write seam", call. = FALSE)
  }
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  gx_snapshot_write_catalog_impl(writer_test_catalog(2L), target)

  output <- testthat::with_mocked_bindings(
    gx_snapshot_catalog_view(target),
    gx_http_request = blocked,
    gx_default_dns_resolver = blocked,
    gx_snapshot_writer_write_csv = blocked,
    gx_snapshot_writer_write_raw = blocked,
    gx_snapshot_writer_unlink = blocked,
    .package = "geoconnexr"
  )

  expect_identical(calls, 0L)
  expect_s3_class(output, "gx_snapshot_catalog_view")
  expect_identical(output$status, "loaded_and_verified")
  expect_s3_class(output$sites, "sf")
  expect_s3_class(output$datasets, "tbl_df")
  expect_s3_class(output$problems, "tbl_df")
  expect_s3_class(output$requests, "tbl_df")
  expect_identical(
    output$verification$manifest_sha256,
    gx_snapshot_verify(target)$manifest_sha256
  )
  expect_identical(output$metadata$counts, list(
    sites = 2L,
    datasets = 2L,
    problems = 2L,
    requests = 2L
  ))
  expect_identical(
    gx_snapshot_catalog_view_validate_impl(output),
    invisible(output)
  )
  expect_message(print(output), "typed redacted snapshot view", fixed = TRUE)
  expect_message(print(output), "not gx_catalog", fixed = TRUE)
  expect_message(print(output), "non-replayable", fixed = TRUE)

  forged_path <- unserialize(serialize(output, NULL))
  forged_path$path <- paste0(forged_path$path, "-forged")
  expect_error(
    gx_snapshot_catalog_view_validate_impl(forged_path),
    class = "gx_error_snapshot_catalog_view"
  )
  forged_metadata <- unserialize(serialize(output, NULL))
  forged_metadata$metadata$replayable <- TRUE
  expect_error(
    gx_snapshot_catalog_view_validate_impl(forged_metadata),
    class = "gx_error_snapshot_catalog_view"
  )
})

test_that("resource bytes are deterministic for permuted equivalent catalogs", {
  testthat::local_mocked_bindings(gx_now = writer_test_clock, .package = "geoconnexr")
  catalog <- writer_test_catalog(n = 2L)
  permuted <- writer_test_copy_catalog(catalog, reverse = TRUE)
  parent <- writer_test_parent()
  left <- file.path(parent, "left")
  right <- file.path(parent, "right")
  gx_snapshot_write_catalog_impl(catalog, left)
  gx_snapshot_write_catalog_impl(permuted, right)

  for (path in c(unname(gx_snapshot_writer_paths), gx_snapshot_manifest_name)) {
    expect_identical(
      writer_test_read_raw(file.path(left, path)),
      writer_test_read_raw(file.path(right, path)),
      info = path
    )
  }
})

test_that("existing targets and unsafe parents fail without overwrite", {
  catalog <- writer_test_catalog(0L)
  makers <- list(
    file = function(path) writeBin(charToRaw("owned"), path),
    directory = function(path) dir.create(path)
  )
  for (name in names(makers)) {
    parent <- writer_test_parent()
    target <- file.path(parent, "snapshot")
    makers[[name]](target)
    writer_test_expect_error(
      function() gx_snapshot_write_catalog_impl(catalog, target),
      "gx_error_snapshot_write_exists"
    )
    expect_equal(length(writer_test_stage_paths(parent)), 0L, info = name)
  }

  if (.Platform$OS.type != "windows") {
    for (dangling in c(FALSE, TRUE)) {
      parent <- writer_test_parent()
      target <- file.path(parent, "snapshot")
      source <- file.path(parent, if (dangling) "missing" else "source")
      if (!dangling) writeBin(charToRaw("owned"), source)
      if (!isTRUE(file.symlink(source, target))) skip("symlink creation unavailable")
      writer_test_expect_error(
        function() gx_snapshot_write_catalog_impl(catalog, target),
        "gx_error_snapshot_write_exists"
      )
    }

    root <- writer_test_parent()
    real_parent <- file.path(root, "real-parent")
    linked_parent <- file.path(root, "linked-parent")
    dir.create(real_parent)
    if (isTRUE(file.symlink(real_parent, linked_parent))) {
      writer_test_expect_error(function() {
        gx_snapshot_write_catalog_impl(catalog, file.path(linked_parent, "snapshot"))
      })
    }
  }

  parent <- writer_test_parent()
  missing_parent <- file.path(parent, "missing")
  writer_test_expect_error(function() {
    gx_snapshot_write_catalog_impl(catalog, file.path(missing_parent, "snapshot"))
  })
  expect_false(dir.exists(missing_parent))
})

test_that("staging failures clean temporary and destination paths", {
  catalog <- writer_test_catalog()
  cases <- list(
    csv = list(gx_snapshot_writer_write_csv = function(...) {
      gx_snapshot_writer_abort("injected CSV failure", "gx_error_snapshot_write_io")
    }),
    raw = list(gx_snapshot_writer_write_raw = function(...) {
      gx_snapshot_writer_abort("injected raw failure", "gx_error_snapshot_write_io")
    }),
    rename = list(gx_snapshot_writer_rename = function(...) FALSE),
    verify_stage = list(gx_snapshot_verify_impl = function(...) {
      gx_snapshot_writer_abort("injected verify failure", "gx_error_snapshot_write_io")
    })
  )
  for (name in names(cases)) {
    parent <- writer_test_parent()
    target <- file.path(parent, paste0("snapshot-", name))
    condition <- testthat::with_mocked_bindings(
      writer_test_expect_error(function() {
        gx_snapshot_write_catalog_impl(catalog, target)
      }),
      !!!cases[[name]],
      .package = "geoconnexr"
    )
    expect_s3_class(condition, "gx_error_snapshot_write")
    expect_false(file.exists(target) || dir.exists(target), info = name)
    expect_equal(length(writer_test_stage_paths(parent)), 0L, info = name)
  }

  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot-final-verify")
  real_verify <- gx_snapshot_verify_impl
  calls <- 0L
  testthat::with_mocked_bindings(
    writer_test_expect_error(function() {
      gx_snapshot_write_catalog_impl(catalog, target)
    }),
    gx_snapshot_verify_impl = function(dir) {
      calls <<- calls + 1L
      if (calls == 2L) {
        gx_snapshot_writer_abort(
          "injected final verification failure", "gx_error_snapshot_write_io"
        )
      }
      real_verify(dir)
    },
    .package = "geoconnexr"
  )
  expect_identical(calls, 2L)
  # Ownership ends at atomic exposure. A post-rename failure may reflect an
  # external replacement, so the writer must not recursively delete target.
  expect_true(dir.exists(target))
  expect_identical(real_verify(target)$status, "verified")
  expect_length(writer_test_stage_paths(parent), 0L)
})

test_that("rename and cleanup warnings never disclose filesystem paths", {
  catalog <- writer_test_catalog()

  rename_parent <- writer_test_parent()
  rename_target <- file.path(rename_parent, "snapshot-rename-warning")
  observed_warnings <- character()
  rename_condition <- withCallingHandlers(
    testthat::with_mocked_bindings(
      writer_test_expect_error(function() {
        gx_snapshot_write_catalog_impl(catalog, rename_target)
      }),
      gx_snapshot_writer_file_rename = function(from, to) {
        warning(paste("LEAK_RENAME", from, to), call. = FALSE)
        FALSE
      },
      .package = "geoconnexr"
    ),
    warning = function(cnd) {
      observed_warnings <<- c(observed_warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(observed_warnings, 0L)
  expect_s3_class(rename_condition, "gx_error_snapshot_write_io")
  expect_false(grepl(rename_parent, conditionMessage(rename_condition), fixed = TRUE))
  expect_false(grepl(rename_target, conditionMessage(rename_condition), fixed = TRUE))
  expect_false(file.exists(rename_target) || dir.exists(rename_target))
  expect_length(writer_test_stage_paths(rename_parent), 0L)

  cleanup_parent <- writer_test_parent()
  cleanup_target <- file.path(cleanup_parent, "snapshot-cleanup-warning")
  observed_warnings <- character()
  cleanup_condition <- withCallingHandlers(
    testthat::with_mocked_bindings(
      writer_test_expect_error(function() {
        gx_snapshot_write_catalog_impl(catalog, cleanup_target)
      }),
      gx_snapshot_writer_write_csv = function(...) {
        gx_snapshot_writer_abort(
          "injected pre-publication failure",
          "gx_error_snapshot_write_io"
        )
      },
      gx_snapshot_writer_unlink = function(path) {
        warning(paste("LEAK_CLEANUP", path), call. = FALSE)
        1L
      },
      .package = "geoconnexr"
    ),
    warning = function(cnd) {
      observed_warnings <<- c(observed_warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(observed_warnings, 0L)
  expect_s3_class(cleanup_condition, "gx_error_snapshot_write_cleanup")
  expect_false(grepl(cleanup_parent, conditionMessage(cleanup_condition), fixed = TRUE))
  expect_false(grepl(cleanup_target, conditionMessage(cleanup_condition), fixed = TRUE))
  expect_false(file.exists(cleanup_target) || dir.exists(cleanup_target))
  residue <- writer_test_stage_paths(cleanup_parent)
  expect_length(residue, 1L)
  unlink(residue, recursive = TRUE, force = TRUE)
})

test_that("writer revalidates mutated catalog objects before staging", {
  catalog <- writer_test_catalog()
  catalog$datasets$site_uri[[1L]] <- "https://example.org/site/missing"
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  writer_test_expect_error(function() gx_snapshot_write_catalog_impl(catalog, target))
  expect_false(file.exists(target) || dir.exists(target))
  expect_length(writer_test_stage_paths(parent), 0L)
})

test_that("catalog snapshot writing performs no network discovery", {
  calls <- 0L
  blocked <- function(...) {
    calls <<- calls + 1L
    stop("network boundary invoked", call. = FALSE)
  }
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")
  expect_no_error(testthat::with_mocked_bindings(
    gx_snapshot(writer_test_catalog(), target),
    gx_http_request = blocked,
    gx_graph_execute_once = blocked,
    gx_ref_features_impl = blocked,
    gx_jsonld_follow_get = blocked,
    gx_default_dns_resolver = blocked,
    .package = "geoconnexr"
  ))
  expect_identical(calls, 0L)
})

test_that("public catalog snapshots return exact verified evidence", {
  testthat::local_mocked_bindings(
    gx_now = writer_test_clock,
    .package = "geoconnexr"
  )
  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot")

  output <- gx_snapshot(writer_test_catalog(n = 2L), target)

  expect_s3_class(output, "gx_snapshot")
  expect_identical(class(output), "gx_snapshot")
  expect_identical(output$contract_version, "0.1.0")
  expect_identical(output$mode, "catalog_only_snapshot")
  expect_identical(output$status, "written_and_verified")
  expect_identical(
    output$path,
    normalizePath(target, winslash = "/", mustWork = TRUE)
  )
  expect_s3_class(output$verification, "gx_snapshot_verification")
  expect_identical(output$verification$status, "verified")
  expect_identical(output$metadata, list(
    scope = "catalog_only_v1",
    creation_only = TRUE,
    catalog_contract_version = "0.1.0",
    resources = 4L,
    requests = 2L,
    overwrite = FALSE,
    fetch = FALSE,
    harmonize = FALSE,
    report = FALSE,
    frictionless = FALSE,
    replayable = FALSE
  ))
  expect_match(output$snapshot_id, "^[0-9a-f]{64}$")
  expect_identical(gx_snapshot_validate_impl(output), invisible(output))
  expect_identical(
    output$verification$manifest_sha256,
    gx_snapshot_verify(target)$manifest_sha256
  )
  printed <- capture.output(print(output), type = "message")
  expect_true(any(grepl(
    "catalog-only; creation-only; non-replayable",
    printed,
    fixed = TRUE
  )))

  mutations <- list(
    verification = function(x) {
      x$verification$resources$actual_sha256[[1L]] <-
        paste(rep("0", 64L), collapse = "")
      x
    },
    metadata = function(x) {
      x$metadata$frictionless <- TRUE
      x
    },
    identity = function(x) {
      x$snapshot_id <- paste(rep("0", 64L), collapse = "")
      x
    },
    status = function(x) {
      x$status <- "forged"
      x
    }
  )
  for (name in names(mutations)) {
    forged <- mutations[[name]](unserialize(serialize(output, NULL)))
    expect_error(
      gx_snapshot_validate_impl(forged),
      class = "gx_error_snapshot",
      info = name
    )
  }
})

test_that("public catalog snapshots reject scope expansion before writing", {
  catalog <- writer_test_catalog()
  cases <- list(
    fetch = list(fetch = TRUE),
    report = list(report = TRUE),
    overwrite = list(overwrite = TRUE)
  )
  for (name in names(cases)) {
    parent <- writer_test_parent()
    target <- file.path(parent, paste0("snapshot-", name))
    arguments <- c(list(x = catalog, dir = target), cases[[name]])
    expect_error(
      do.call(gx_snapshot, arguments),
      class = "gx_error_snapshot_scope",
      info = name
    )
    expect_false(file.exists(target) || dir.exists(target), info = name)
    expect_length(writer_test_stage_paths(parent), 0L)
  }

  parent <- writer_test_parent()
  target <- file.path(parent, "snapshot-invalid")
  expect_error(
    gx_snapshot(list(), target),
    class = "gx_error_snapshot_input"
  )
  expect_false(file.exists(target) || dir.exists(target))
  expect_error(
    gx_snapshot(catalog, target, fetch = NA),
    class = "gx_error_snapshot_input"
  )
  expect_false(file.exists(target) || dir.exists(target))
})

test_that("public snapshot wrappers preserve internal boundaries", {
  exports <- getNamespaceExports("geoconnexr")
  expect_true(all(c(
    "gx_snapshot", "gx_snapshot_verify", "gx_snapshot_requests",
    "gx_snapshot_catalog_view"
  ) %in% exports))
  expect_false("gx_snapshot_write_catalog_impl" %in% exports)
  expect_false("gx_snapshot_verify_impl" %in% exports)
  expect_false("gx_catalog_export_views_impl" %in% exports)
  expect_false("gx_snapshot_validate_impl" %in% exports)
  expect_false("gx_snapshot_load_requests_impl" %in% exports)
  expect_false("gx_snapshot_load_catalog_csv_impl" %in% exports)
  expect_false("gx_snapshot_load_catalog_view_impl" %in% exports)
})
