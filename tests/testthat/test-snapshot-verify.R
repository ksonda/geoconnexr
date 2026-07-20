snapshot_test_fixture_dir <- function() {
  testthat::test_path(
    "..", "fixtures", "snapshot", "catalog-only-v1"
  )
}

snapshot_test_copy <- function(.env = parent.frame()) {
  parent <- withr::local_tempdir(
    pattern = "gx-snapshot-verify-",
    .local_envir = .env
  )
  target <- file.path(parent, "snapshot")
  fs::dir_copy(snapshot_test_fixture_dir(), target)
  target
}

snapshot_test_manifest_path <- function(dir) {
  file.path(dir, "manifest.json")
}

snapshot_test_read_raw <- function(path) {
  size <- file.info(path)$size[[1L]]
  readBin(path, what = "raw", n = size)
}

snapshot_test_read_manifest <- function(dir) {
  jsonlite::fromJSON(
    snapshot_test_manifest_path(dir),
    simplifyVector = FALSE
  )
}

snapshot_test_write_raw <- function(dir, bytes) {
  writeBin(bytes, snapshot_test_manifest_path(dir))
  invisible(dir)
}

snapshot_test_write_manifest <- function(dir, manifest) {
  json <- jsonlite::toJSON(
    manifest,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = TRUE
  )
  snapshot_test_write_raw(dir, charToRaw(enc2utf8(as.character(json))))
}

snapshot_test_clone <- function(x) {
  unserialize(serialize(x, NULL))
}

snapshot_test_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

snapshot_test_resource_index <- function(manifest, path) {
  paths <- vapply(manifest$resources, `[[`, character(1), "path")
  match(path, paths)
}

snapshot_test_rebind_resource <- function(manifest, dir, path) {
  index <- snapshot_test_resource_index(manifest, path)
  stopifnot(!is.na(index))
  components <- strsplit(path, "/", fixed = TRUE)[[1L]]
  resource_path <- do.call(file.path, c(list(dir), as.list(components)))
  manifest$resources[[index]]$bytes <- as.integer(file.info(resource_path)$size)
  manifest$resources[[index]]$sha256 <- snapshot_test_sha256(resource_path)
  manifest
}

snapshot_test_expect_error <- function(code, subclass = NULL) {
  condition <- tryCatch(code(), error = identity)
  expect_s3_class(condition, "gx_error_snapshot")
  if (!is.null(subclass)) {
    expect_s3_class(condition, subclass)
  }
  invisible(condition)
}

snapshot_test_expect_manifest_mutation <- function(
    mutate,
    subclass = NULL,
    .env = parent.frame()) {
  dir <- snapshot_test_copy(.env = .env)
  manifest <- snapshot_test_read_manifest(dir)
  snapshot_test_write_manifest(dir, mutate(snapshot_test_clone(manifest)))
  snapshot_test_expect_error(
    function() gx_snapshot_verify_impl(dir),
    subclass = subclass
  )
}

snapshot_test_replace_text <- function(bytes, pattern, replacement) {
  text <- rawToChar(bytes)
  changed <- sub(pattern, replacement, text, fixed = TRUE)
  stopifnot(!identical(text, changed))
  charToRaw(changed)
}

snapshot_test_polygon_aoi <- function() {
  ring <- rbind(
    c(-80, 35),
    c(-76, 35),
    c(-76, 39),
    c(-80, 39),
    c(-80, 35)
  )
  geometry <- sf::st_sfc(
    sf::st_polygon(list(ring)),
    crs = "OGC:CRS84"
  )
  gx_aoi(geometry)
}

snapshot_test_tree_state <- function(dir) {
  relative <- list.files(
    dir,
    all.files = TRUE,
    full.names = FALSE,
    recursive = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  relative <- sort(relative, method = "radix")
  paths <- file.path(dir, relative)
  info <- fs::file_info(paths, follow = FALSE)
  regular <- as.character(info$type) == "file"
  hashes <- rep(NA_character_, length(paths))
  hashes[regular] <- vapply(paths[regular], snapshot_test_sha256, character(1))
  list(
    relative = relative,
    type = as.character(info$type),
    size = as.numeric(info$size),
    modification_time = as.numeric(info$modification_time),
    hashes = hashes
  )
}

snapshot_test_block_external_work <- function(.env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  blocked <- function(...) {
    state$calls <- state$calls + 1L
    stop("snapshot verification attempted external work", call. = FALSE)
  }
  withr::local_options(
    list(
      geoconnexr.performer = blocked,
      geoconnexr.file_performer = blocked,
      geoconnexr.dns_resolver = blocked,
      geoconnexr.offline = TRUE
    ),
    .local_envir = .env
  )
  testthat::local_mocked_bindings(
    gx_http_request = blocked,
    gx_parse_datasets = blocked,
    gx_parse_location = blocked,
    .package = "geoconnexr",
    .env = .env
  )
  state
}

test_that("identifier snapshots verify exact bytes into deterministic evidence", {
  dir <- snapshot_test_copy()
  verified_at <- as.POSIXct("2026-07-14 12:34:56", tz = "UTC")
  withr::local_options(list(geoconnexr.clock = function() verified_at))

  manifest_raw <- snapshot_test_read_raw(snapshot_test_manifest_path(dir))
  manifest <- snapshot_test_read_manifest(dir)
  output <- gx_snapshot_verify_impl(dir)

  required_fields <- c(
    "contract_version", "mode", "status", "manifest_sha256", "manifest",
    "aoi", "resources", "request_count", "request_ledger_status",
    "verified_at"
  )
  expect_true(all(required_fields %in% names(output)))
  expect_type(output, "list")
  expect_identical(output$contract_version, "1.0.0")
  expect_identical(output$mode, "offline_snapshot_verification")
  expect_identical(output$status, "verified")
  expect_identical(
    output$manifest_sha256,
    digest::digest(manifest_raw, algo = "sha256", serialize = FALSE)
  )
  expect_identical(
    names(output$manifest),
    c(
      "contract_version", "manifest_version", "package", "created_at",
      "recipe", "replay", "endpoints", "hydrologic_vintage",
      "asset_hashes", "requests", "resources", "completeness",
      "source_licenses", "session"
    )
  )
  expect_identical(output$manifest$contract_version, manifest$contract_version)
  expect_identical(output$manifest$manifest_version, manifest$manifest_version)
  expect_identical(output$aoi, gx_aoi("02070010", type = "huc"))
  expect_identical(output$request_count, 1L)
  expect_identical(output$request_ledger_status, "shape_validated")
  expect_s3_class(output$verified_at, "POSIXct")
  expect_length(output$verified_at, 1L)
  expect_identical(output$verified_at, verified_at)

  expect_s3_class(output$resources, "tbl_df")
  expect_identical(
    names(output$resources),
    c(
      "path", "media_type", "expected_bytes", "expected_sha256",
      "required", "roles", "present", "actual_bytes", "actual_sha256",
      "status"
    )
  )
  expected_paths <- c("catalog/datasets.csv", "requests.csv")
  expect_identical(output$resources$path, expected_paths)
  expect_equal(output$resources$expected_bytes, c(42, 65))
  expect_identical(
    output$resources$expected_sha256,
    unname(vapply(
      file.path(dir, expected_paths),
      snapshot_test_sha256,
      character(1)
    ))
  )
  expect_identical(output$resources$required, c(TRUE, TRUE))
  expect_identical(
    output$resources$roles,
    list("catalog", "request-ledger-export")
  )
  expect_identical(output$resources$present, c(TRUE, TRUE))
  expect_equal(output$resources$actual_bytes, c(42, 65))
  expect_identical(
    output$resources$actual_sha256,
    output$resources$expected_sha256
  )
  expect_identical(output$resources$status, c("verified", "verified"))
})

test_that("resource rows are bytewise sorted independently of manifest order", {
  dir <- snapshot_test_copy()
  manifest <- snapshot_test_read_manifest(dir)
  manifest$resources <- rev(manifest$resources)
  snapshot_test_write_manifest(dir, manifest)

  output <- gx_snapshot_verify_impl(dir)

  expect_identical(
    output$resources$path,
    sort(
      vapply(manifest$resources, `[[`, character(1), "path"),
      method = "radix"
    )
  )
  expect_identical(
    output$resources$actual_sha256,
    unname(vapply(
      file.path(dir, output$resources$path),
      snapshot_test_sha256,
      character(1)
    ))
  )
})

test_that("spatial AOI snapshots rebind canonical geometry and digest identity", {
  dir <- snapshot_test_copy()
  manifest <- snapshot_test_read_manifest(dir)
  expected <- snapshot_test_polygon_aoi()
  manifest$recipe <- expected$recipe
  snapshot_test_write_manifest(dir, manifest)

  output <- gx_snapshot_verify_impl(dir)

  expect_identical(output$status, "verified")
  expect_identical(output$aoi, expected)
  expect_identical(output$aoi$id, expected$recipe$aoi$wkb_sha256)
  expect_identical(
    output$manifest$recipe$aoi$canonical_geojson,
    expected$recipe$aoi$canonical_geojson
  )
})

test_that("valid planned recipe fields remain inert normalized metadata", {
  dir <- snapshot_test_copy()
  manifest <- snapshot_test_read_manifest(dir)
  manifest$recipe$time <- list(
    start = "2026-01-01T00:00:00Z",
    end = "2026-01-02T00:00:00Z"
  )
  manifest$recipe$catalog <- list(
    include = list("sites", "datasets"),
    providers = list("provider-a"),
    variables = list("variable-a")
  )
  manifest$recipe$fetch <- list(
    enabled = FALSE,
    max_datasets = 0L,
    max_requests = 0L,
    max_encoded_bytes = 0L,
    max_decoded_bytes = 0L,
    handler_order = list("handler-a")
  )
  manifest$recipe$harmonize <- list(
    enabled = FALSE,
    target_units = list(discharge = "https://example.org/unit/m3-s")
  )
  manifest$recipe$output <- list(
    timeseries = "csv",
    keep_raw = TRUE,
    report = FALSE
  )
  snapshot_test_write_manifest(dir, manifest)

  network <- snapshot_test_block_external_work()
  output <- gx_snapshot_verify_impl(dir)

  expect_identical(output$status, "verified")
  expect_identical(output$manifest$recipe$time, manifest$recipe$time)
  expect_identical(output$manifest$recipe$catalog, manifest$recipe$catalog)
  expect_identical(output$manifest$recipe$fetch, manifest$recipe$fetch)
  expect_identical(output$manifest$recipe$harmonize, manifest$recipe$harmonize)
  expect_identical(output$manifest$recipe$output, manifest$recipe$output)
  expect_identical(network$calls, 0L)
})

test_that("normalized manifests ignore JSON object member order", {
  first_dir <- snapshot_test_copy()
  second_dir <- snapshot_test_copy()
  manifest <- snapshot_test_read_manifest(first_dir)
  reordered <- manifest[rev(names(manifest))]
  reordered$package <- reordered$package[rev(names(reordered$package))]
  reordered$recipe <- reordered$recipe[rev(names(reordered$recipe))]
  reordered$resources <- lapply(
    reordered$resources,
    function(resource) resource[rev(names(resource))]
  )
  snapshot_test_write_manifest(second_dir, reordered)

  first <- gx_snapshot_verify_impl(first_dir)
  second <- gx_snapshot_verify_impl(second_dir)

  expect_false(identical(first$manifest_sha256, second$manifest_sha256))
  expect_identical(first$manifest, second$manifest)
  expect_identical(first$aoi, second$aoi)
  expect_identical(first$resources, second$resources)
})

test_that("missing optional resources are visible while required absence fails", {
  optional_dir <- snapshot_test_copy()
  optional_manifest <- snapshot_test_read_manifest(optional_dir)
  index <- snapshot_test_resource_index(optional_manifest, "requests.csv")
  optional_manifest$resources[[index]]$required <- FALSE
  snapshot_test_write_manifest(optional_dir, optional_manifest)
  unlink(file.path(optional_dir, "requests.csv"))

  output <- gx_snapshot_verify_impl(optional_dir)

  expect_identical(output$status, "verified_with_optional_absences")
  expect_identical(
    output$resources$status,
    c("verified", "missing_optional")
  )
  expect_identical(output$resources$present, c(TRUE, FALSE))
  expect_true(is.na(output$resources$actual_bytes[[2L]]))
  expect_true(is.na(output$resources$actual_sha256[[2L]]))

  required_dir <- snapshot_test_copy()
  unlink(file.path(required_dir, "requests.csv"))
  snapshot_test_expect_error(
    function() gx_snapshot_verify_impl(required_dir)
  )
})

test_that("size and digest mismatches fail for required and optional files", {
  same_size <- snapshot_test_copy()
  path <- file.path(same_size, "catalog", "datasets.csv")
  bytes <- snapshot_test_read_raw(path)
  bytes[[1L]] <- as.raw(bitwXor(as.integer(bytes[[1L]]), 1L))
  writeBin(bytes, path)
  snapshot_test_expect_error(function() gx_snapshot_verify_impl(same_size))

  wrong_size <- snapshot_test_copy()
  path <- file.path(wrong_size, "catalog", "datasets.csv")
  writeBin(c(snapshot_test_read_raw(path), as.raw(0x0a)), path)
  snapshot_test_expect_error(function() gx_snapshot_verify_impl(wrong_size))

  wrong_hash <- snapshot_test_copy()
  manifest <- snapshot_test_read_manifest(wrong_hash)
  index <- snapshot_test_resource_index(manifest, "catalog/datasets.csv")
  manifest$resources[[index]]$sha256 <- strrep("0", 64L)
  snapshot_test_write_manifest(wrong_hash, manifest)
  snapshot_test_expect_error(function() gx_snapshot_verify_impl(wrong_hash))

  optional_corrupt <- snapshot_test_copy()
  manifest <- snapshot_test_read_manifest(optional_corrupt)
  index <- snapshot_test_resource_index(manifest, "requests.csv")
  manifest$resources[[index]]$required <- FALSE
  snapshot_test_write_manifest(optional_corrupt, manifest)
  path <- file.path(optional_corrupt, "requests.csv")
  bytes <- snapshot_test_read_raw(path)
  bytes[[1L]] <- as.raw(bitwXor(as.integer(bytes[[1L]]), 1L))
  writeBin(bytes, path)
  snapshot_test_expect_error(
    function() gx_snapshot_verify_impl(optional_corrupt)
  )
})

test_that("snapshot roots and fixed manifests must be scalar regular local paths", {
  fixture_file <- file.path(snapshot_test_fixture_dir(), "manifest.json")
  invalid_roots <- list(
    NULL,
    character(),
    c(snapshot_test_fixture_dir(), snapshot_test_fixture_dir()),
    NA_character_,
    "",
    paste0(snapshot_test_fixture_dir(), "\n"),
    fixture_file,
    tempfile("gx-missing-snapshot-")
  )
  for (root in invalid_roots) {
    snapshot_test_expect_error(
      function() gx_snapshot_verify_impl(root)
    )
  }

  missing_manifest <- snapshot_test_copy()
  unlink(snapshot_test_manifest_path(missing_manifest))
  snapshot_test_expect_error(
    function() gx_snapshot_verify_impl(missing_manifest)
  )

  directory_manifest <- snapshot_test_copy()
  unlink(snapshot_test_manifest_path(directory_manifest))
  dir.create(snapshot_test_manifest_path(directory_manifest))
  snapshot_test_expect_error(
    function() gx_snapshot_verify_impl(directory_manifest)
  )
})

test_that("manifest JSON rejects ambiguous encodings and syntax", {
  baseline_dir <- snapshot_test_copy()
  original <- snapshot_test_read_raw(snapshot_test_manifest_path(baseline_dir))
  original_text <- rawToChar(original)
  invalid_utf8 <- append(original, as.raw(0xff), after = 2L)
  literal_nul <- append(original, as.raw(0x00), after = 2L)
  utf16le_bom <- c(as.raw(c(0xff, 0xfe)), original)
  utf16be_bom <- c(as.raw(c(0xfe, 0xff)), original)
  utf32le_bom <- c(as.raw(c(0xff, 0xfe, 0x00, 0x00)), original)
  utf32be_bom <- c(as.raw(c(0x00, 0x00, 0xfe, 0xff)), original)
  duplicate_root <- snapshot_test_replace_text(
    original,
    '"contract_version": "1.0.0",',
    paste0(
      '"contract_version": "1.0.0",',
      '"\\u0063ontract_version": "1.0.0",'
    )
  )
  duplicate_nested <- snapshot_test_replace_text(
    original,
    '"name": "geoconnexr",',
    paste0(
      '"name": "geoconnexr",',
      '"\\u006eame": "geoconnexr",'
    )
  )
  escaped_nul <- snapshot_test_replace_text(
    original,
    '"version": "0.0.0.9000"',
    '"version": "0.0.0.9000\\u0000"'
  )
  escaped_controls <- lapply(
    c("\\b", "\\f", "\\n", "\\r", "\\t"),
    function(escape) {
      snapshot_test_replace_text(
        original,
        '"version": "0.0.0.9000"',
        paste0('"version": "0.0.0.9000', escape, '"')
      )
    }
  )
  unpaired_surrogate <- snapshot_test_replace_text(
    original,
    '"version": "0.0.0.9000"',
    '"version": "\\uD800"'
  )
  malformed_escape <- snapshot_test_replace_text(
    original,
    '"version": "0.0.0.9000"',
    '"version": "\\x"'
  )
  cases <- list(
    malformed = charToRaw("{"),
    utf8_bom = c(as.raw(c(0xef, 0xbb, 0xbf)), original),
    utf16le_bom = utf16le_bom,
    utf16be_bom = utf16be_bom,
    utf32le_bom = utf32le_bom,
    utf32be_bom = utf32be_bom,
    invalid_utf8 = invalid_utf8,
    literal_nul = literal_nul,
    duplicate_root = duplicate_root,
    duplicate_nested = duplicate_nested,
    escaped_nul = escaped_nul,
    escaped_backspace = escaped_controls[[1L]],
    escaped_form_feed = escaped_controls[[2L]],
    escaped_newline = escaped_controls[[3L]],
    escaped_return = escaped_controls[[4L]],
    escaped_tab = escaped_controls[[5L]],
    unpaired_surrogate = unpaired_surrogate,
    malformed_escape = malformed_escape,
    trailing_garbage = c(original, charToRaw(" true")),
    literal_control = charToRaw(sub(
      "0.0.0.9000",
      paste0("0.0.0.9000", intToUtf8(1L)),
      original_text,
      fixed = TRUE
    ))
  )

  for (name in names(cases)) {
    dir <- snapshot_test_copy()
    snapshot_test_write_raw(dir, cases[[name]])
    snapshot_test_expect_error(
      function() gx_snapshot_verify_impl(dir)
    )
  }
})

test_that("runtime URI and date-time checks follow the shipped contract", {
  lowercase <- snapshot_test_copy()
  manifest <- snapshot_test_read_manifest(lowercase)
  manifest$created_at <- "2026-07-14t12:00:00z"
  manifest$requests[[1L]]$retrieved_at <- "2026-07-14t12:00:01z"
  snapshot_test_write_manifest(lowercase, manifest)

  output <- gx_snapshot_verify_impl(lowercase)

  expect_identical(output$manifest$created_at, manifest$created_at)
  expect_identical(
    output$manifest$requests[[1L]]$retrieved_at,
    manifest$requests[[1L]]$retrieved_at
  )
  expect_identical(
    gx_snapshot_uri("http://[v1.future]/resource"),
    "http://[v1.future]/resource"
  )

  snapshot_test_expect_manifest_mutation(function(x) {
    x$created_at <- "2026-12-31T23:59:60Z"
    x
  })
  for (uri in c(
    "https://[",
    "http:[",
    "urn:[",
    "http:/[",
    "http://example.com/[",
    "https://example.com/path?value=[",
    "http://user[name@example.com/",
    "http://user]name@example.com/",
    "http://[::1%25eth0]/",
    "urn:?value"
  )) {
    snapshot_test_expect_manifest_mutation(function(x) {
      x$endpoints$graph <- uri
      x
    })
  }
  empty_endpoint <- snapshot_test_copy()
  bytes <- snapshot_test_read_raw(snapshot_test_manifest_path(empty_endpoint))
  snapshot_test_write_raw(
    empty_endpoint,
    snapshot_test_replace_text(bytes, '"reference":', '"":')
  )
  snapshot_test_expect_error(
    function() gx_snapshot_verify_impl(empty_endpoint)
  )
})

test_that("manifest root structure and scalar identity are exact", {
  mutations <- list(
    extra = function(x) {
      x$unexpected <- TRUE
      x
    },
    missing_contract = function(x) {
      x$contract_version <- NULL
      x
    },
    bad_contract = function(x) {
      x$contract_version <- "1.0.1"
      x
    },
    bad_manifest_version = function(x) {
      x$manifest_version <- "2.0.0"
      x
    },
    missing_package = function(x) {
      x$package <- NULL
      x
    },
    package_name = function(x) {
      x$package$name <- "other"
      x
    },
    package_extra = function(x) {
      x$package$unexpected <- TRUE
      x
    },
    bad_created_at = function(x) {
      x$created_at <- "2026-07-14"
      x
    },
    missing_recipe = function(x) {
      x$recipe <- NULL
      x
    },
    missing_replay = function(x) {
      x$replay <- NULL
      x
    },
    missing_endpoints = function(x) {
      x$endpoints <- NULL
      x
    },
    missing_assets = function(x) {
      x$asset_hashes <- NULL
      x
    },
    missing_requests = function(x) {
      x$requests <- NULL
      x
    },
    missing_resources = function(x) {
      x$resources <- NULL
      x
    },
    missing_completeness = function(x) {
      x$completeness <- NULL
      x
    },
    missing_session = function(x) {
      x$session <- NULL
      x
    }
  )

  for (name in names(mutations)) {
    snapshot_test_expect_manifest_mutation(mutations[[name]])
  }
})

test_that("recipe structure is validated before AOI hydration", {
  mutations <- list(
    recipe_extra = function(x) {
      x$recipe$unexpected <- TRUE
      x
    },
    recipe_contract = function(x) {
      x$recipe$contract_version <- "1.0.1"
      x
    },
    missing_aoi = function(x) {
      x$recipe$aoi <- NULL
      x
    },
    aoi_extra = function(x) {
      x$recipe$aoi$unexpected <- TRUE
      x
    },
    invalid_identifier = function(x) {
      x$recipe$aoi$identifier <- "2070010"
      x
    },
    missing_pipeline = function(x) {
      x$recipe$pipeline <- NULL
      x
    },
    invalid_pipeline = function(x) {
      x$recipe$pipeline$end_stage <- "aoi"
      x
    },
    invalid_time = function(x) {
      x$recipe$time <- list(start = "yesterday", end = NULL)
      x
    },
    catalog_extra = function(x) {
      x$recipe$catalog <- list(unexpected = TRUE)
      x
    },
    fetch_negative = function(x) {
      x$recipe$fetch <- list(max_requests = -1L)
      x
    },
    harmonize_type = function(x) {
      x$recipe$harmonize <- list(enabled = "false")
      x
    },
    output_enum = function(x) {
      x$recipe$output <- list(timeseries = "xml")
      x
    }
  )

  for (name in names(mutations)) {
    snapshot_test_expect_manifest_mutation(mutations[[name]])
  }
})

test_that("embedded request ledger shape is bounded and typed", {
  mutations <- list(
    request_extra = function(x) {
      x$requests[[1L]]$unexpected <- TRUE
      x
    },
    missing_request_id = function(x) {
      x$requests[[1L]]$request_id <- NULL
      x
    },
    empty_request_id = function(x) {
      x$requests[[1L]]$request_id <- ""
      x
    },
    invalid_method = function(x) {
      x$requests[[1L]]$method <- "DELETE"
      x
    },
    empty_canonical_url = function(x) {
      x$requests[[1L]]$canonical_url_redacted <- ""
      x
    },
    invalid_request_hash = function(x) {
      x$requests[[1L]]$request_hash <- strrep("g", 64L)
      x
    },
    invalid_body_hash = function(x) {
      x$requests[[1L]]$body_hash <- "abc"
      x
    },
    invalid_final_url = function(x) {
      x$requests[[1L]]$final_url <- "relative"
      x
    },
    invalid_status = function(x) {
      x$requests[[1L]]$response_status <- 99L
      x
    },
    fractional_status = function(x) {
      x$requests[[1L]]$response_status <- 200.5
      x
    },
    invalid_encoded_bytes = function(x) {
      x$requests[[1L]]$encoded_bytes <- -1L
      x
    },
    invalid_decoded_bytes = function(x) {
      x$requests[[1L]]$decoded_bytes <- 1073741825
      x
    },
    invalid_content_hash = function(x) {
      x$requests[[1L]]$content_hash <- strrep("A", 64L)
      x
    },
    invalid_retrieved_at = function(x) {
      x$requests[[1L]]$retrieved_at <- "2026-07-14"
      x
    },
    invalid_elapsed = function(x) {
      x$requests[[1L]]$elapsed_ms <- -0.1
      x
    },
    invalid_cache_origin = function(x) {
      x$requests[[1L]]$cache_origin <- "memory"
      x
    },
    non_object_request = function(x) {
      x$requests[[1L]] <- "request"
      x
    }
  )

  for (name in names(mutations)) {
    snapshot_test_expect_manifest_mutation(mutations[[name]])
  }
})

test_that("resource declarations require exact typed fields", {
  mutations <- list(
    resource_extra = function(x) {
      x$resources[[1L]]$unexpected <- TRUE
      x
    },
    missing_path = function(x) {
      x$resources[[1L]]$path <- NULL
      x
    },
    empty_media_type = function(x) {
      x$resources[[1L]]$media_type <- ""
      x
    },
    negative_bytes = function(x) {
      x$resources[[1L]]$bytes <- -1L
      x
    },
    fractional_bytes = function(x) {
      x$resources[[1L]]$bytes <- 43.5
      x
    },
    oversized_bytes = function(x) {
      x$resources[[1L]]$bytes <- 1073741825
      x
    },
    invalid_sha = function(x) {
      x$resources[[1L]]$sha256 <- strrep("A", 64L)
      x
    },
    required_string = function(x) {
      x$resources[[1L]]$required <- "true"
      x
    },
    empty_roles = function(x) {
      x$resources[[1L]]$roles <- list()
      x
    },
    duplicate_roles = function(x) {
      x$resources[[1L]]$roles <- list("catalog", "catalog")
      x
    },
    empty_role = function(x) {
      x$resources[[1L]]$roles <- list("")
      x
    },
    invalid_source_uri = function(x) {
      x$resources[[1L]]$source_uri <- "relative"
      x
    },
    invalid_license_uri = function(x) {
      x$resources[[1L]]$license_uri <- "relative"
      x
    },
    non_object_resource = function(x) {
      x$resources[[1L]] <- "resource"
      x
    },
    no_resources = function(x) {
      x$resources <- list()
      x
    }
  )

  for (name in names(mutations)) {
    snapshot_test_expect_manifest_mutation(mutations[[name]])
  }
})

test_that("portable resource paths reject aliases and hostile spellings", {
  expect_identical(
    gx_snapshot_ascii_fold(c("I", "i", "MANIFEST.JSON")),
    c("i", "i", "manifest.json")
  )

  overlong_component <- paste0(strrep("a", 256L), ".csv")
  too_many_components <- paste(rep("a", 17L), collapse = "/")
  invalid_paths <- c(
    "",
    "/tmp/data.csv",
    "../data.csv",
    "./data.csv",
    "a/../data.csv",
    "a//data.csv",
    "a\\data.csv",
    "C:data.csv",
    "C:/data.csv",
    "//server/share.csv",
    "https://example.org/data.csv",
    ".hidden.csv",
    "a/.hidden.csv",
    "a/.",
    "a/..",
    "a/trailing.",
    "manifest.json",
    "MANIFEST.JSON",
    "CON",
    "con.txt",
    "a/NUL.csv",
    "a\ndata.csv",
    "caf\u00e9.csv",
    overlong_component,
    too_many_components,
    strrep("a", 1025L)
  )

  for (path in invalid_paths) {
    snapshot_test_expect_manifest_mutation(function(x) {
      x$resources[[1L]]$path <- path
      x
    })
  }
})

test_that("resource paths reject duplicates, case aliases, and prefix collisions", {
  mutations <- list(
    exact_duplicate = function(x) {
      x$resources[[2L]] <- snapshot_test_clone(x$resources[[1L]])
      x
    },
    case_alias = function(x) {
      alias <- snapshot_test_clone(x$resources[[1L]])
      alias$path <- "CATALOG/DATASETS.CSV"
      x$resources[[length(x$resources) + 1L]] <- alias
      x
    },
    file_directory_prefix = function(x) {
      prefix <- snapshot_test_clone(x$resources[[1L]])
      prefix$path <- "catalog"
      x$resources[[length(x$resources) + 1L]] <- prefix
      x
    },
    resource_prefix = function(x) {
      child <- snapshot_test_clone(x$resources[[1L]])
      child$path <- "catalog/datasets.csv/child"
      x$resources[[length(x$resources) + 1L]] <- child
      x
    }
  )

  for (name in names(mutations)) {
    snapshot_test_expect_manifest_mutation(mutations[[name]])
  }
})

test_that("the snapshot tree is closed to undeclared files and directories", {
  extra_file <- snapshot_test_copy()
  writeBin(charToRaw("undeclared"), file.path(extra_file, "extra.bin"))
  snapshot_test_expect_error(function() gx_snapshot_verify_impl(extra_file))

  extra_nested_file <- snapshot_test_copy()
  dir.create(file.path(extra_nested_file, "extra"))
  writeBin(
    charToRaw("undeclared"),
    file.path(extra_nested_file, "extra", "data.bin")
  )
  snapshot_test_expect_error(
    function() gx_snapshot_verify_impl(extra_nested_file)
  )

  extra_empty_dir <- snapshot_test_copy()
  dir.create(file.path(extra_empty_dir, "extra"))
  snapshot_test_expect_error(
    function() gx_snapshot_verify_impl(extra_empty_dir)
  )
})

test_that("root, manifest, resource, and intermediate symlinks fail closed", {
  skip_on_os("windows")

  root_target <- snapshot_test_copy()
  root_parent <- withr::local_tempdir()
  root_link <- file.path(root_parent, "snapshot-link")
  if (!file.symlink(root_target, root_link)) {
    skip("This platform does not permit symlink creation")
  }
  snapshot_test_expect_error(function() gx_snapshot_verify_impl(root_link))

  manifest_link_dir <- snapshot_test_copy()
  outside_manifest <- tempfile("gx-manifest-outside-", fileext = ".json")
  withr::defer(unlink(outside_manifest))
  file.copy(snapshot_test_manifest_path(manifest_link_dir), outside_manifest)
  unlink(snapshot_test_manifest_path(manifest_link_dir))
  expect_true(file.symlink(
    outside_manifest,
    snapshot_test_manifest_path(manifest_link_dir)
  ))
  snapshot_test_expect_error(
    function() gx_snapshot_verify_impl(manifest_link_dir)
  )

  resource_link_dir <- snapshot_test_copy()
  outside_resource <- tempfile("gx-resource-outside-")
  withr::defer(unlink(outside_resource))
  writeBin(charToRaw("outside"), outside_resource)
  resource_path <- file.path(resource_link_dir, "requests.csv")
  unlink(resource_path)
  expect_true(file.symlink(outside_resource, resource_path))
  snapshot_test_expect_error(
    function() gx_snapshot_verify_impl(resource_link_dir)
  )

  intermediate_link_dir <- snapshot_test_copy()
  outside_dir <- withr::local_tempdir()
  file.copy(
    file.path(intermediate_link_dir, "catalog", "datasets.csv"),
    file.path(outside_dir, "datasets.csv")
  )
  unlink(file.path(intermediate_link_dir, "catalog"), recursive = TRUE)
  expect_true(file.symlink(
    outside_dir,
    file.path(intermediate_link_dir, "catalog")
  ))
  snapshot_test_expect_error(
    function() gx_snapshot_verify_impl(intermediate_link_dir)
  )

  dangling_dir <- snapshot_test_copy()
  dangling_path <- file.path(dangling_dir, "requests.csv")
  unlink(dangling_path)
  expect_true(file.symlink(
    file.path(dangling_dir, "does-not-exist"),
    dangling_path
  ))
  snapshot_test_expect_error(function() gx_snapshot_verify_impl(dangling_dir))
})

test_that("unreadable directories and hard-link aliases fail closed", {
  skip_on_os("windows")

  unreadable <- snapshot_test_copy()
  secret_dir <- file.path(unreadable, "secret")
  dir.create(secret_dir)
  secret_path <- file.path(secret_dir, "data.bin")
  writeBin(charToRaw("secret"), secret_path)
  manifest <- snapshot_test_read_manifest(unreadable)
  manifest$resources[[length(manifest$resources) + 1L]] <- list(
    path = "secret/data.bin",
    media_type = "application/octet-stream",
    bytes = as.integer(file.info(secret_path)$size[[1L]]),
    sha256 = snapshot_test_sha256(secret_path),
    required = FALSE,
    roles = list("opaque")
  )
  snapshot_test_write_manifest(unreadable, manifest)
  Sys.chmod(secret_dir, mode = "0000")
  withr::defer(Sys.chmod(secret_dir, mode = "0700"))
  access_denied <- tryCatch(
    {
      fs::dir_ls(secret_dir, fail = TRUE)
      FALSE
    },
    error = function(cnd) TRUE
  )
  if (access_denied) {
    snapshot_test_expect_error(
      function() gx_snapshot_verify_impl(unreadable),
      subclass = "gx_error_snapshot_io"
    )
  }

  linked <- snapshot_test_copy()
  original <- file.path(linked, "requests.csv")
  alias <- file.path(linked, "0.csv")
  skip_if(!file.link(original, alias), "Hard links are unavailable")
  link_count <- as.numeric(fs::file_info(alias)$hard_links[[1L]])
  skip_if(
    !is.finite(link_count) || link_count < 2,
    "The filesystem does not report hard-link counts"
  )
  manifest <- snapshot_test_read_manifest(linked)
  resource <- snapshot_test_clone(
    manifest$resources[[snapshot_test_resource_index(
      manifest,
      "requests.csv"
    )]]
  )
  resource$path <- "0.csv"
  manifest$resources[[length(manifest$resources) + 1L]] <- resource
  snapshot_test_write_manifest(linked, manifest)
  hash_state <- new.env(parent = emptyenv())
  hash_state$calls <- 0L
  snapshot_test_expect_error(
    function() testthat::with_mocked_bindings(
      gx_snapshot_verify_impl(linked),
      gx_snapshot_hash_file = function(...) {
        hash_state$calls <- hash_state$calls + 1L
        stop("hard link reached hashing", call. = FALSE)
      },
      .package = "geoconnexr"
    ),
    subclass = "gx_error_snapshot_tree"
  )
  expect_identical(hash_state$calls, 0L)
})

test_that("FIFO resources are rejected by type before the hashing seam", {
  skip_on_os("windows")
  mkfifo <- Sys.which("mkfifo")
  skip_if(!nzchar(mkfifo), "mkfifo is unavailable")

  dir <- snapshot_test_copy()
  fifo <- file.path(dir, "requests.csv")
  unlink(fifo)
  status <- suppressWarnings(system2(mkfifo, shQuote(fifo)))
  skip_if(status != 0L, "This platform does not permit FIFO creation")
  expect_identical(
    tolower(as.character(fs::file_info(fifo)$type[[1L]])),
    "fifo"
  )

  hash_state <- new.env(parent = emptyenv())
  hash_state$calls <- 0L
  blocked_hash <- function(...) {
    hash_state$calls <- hash_state$calls + 1L
    stop("FIFO reached the hashing seam", call. = FALSE)
  }
  testthat::local_mocked_bindings(
    gx_snapshot_hash_file = blocked_hash,
    .package = "geoconnexr"
  )

  snapshot_test_expect_error(function() gx_snapshot_verify_impl(dir))
  expect_identical(hash_state$calls, 0L)
})

test_that("runtime budgets match schema ceilings and enforce compact probes", {
  schema <- jsonlite::fromJSON(
    system.file("schema", "manifest-v1.json", package = "geoconnexr"),
    simplifyVector = FALSE
  )

  expect_equal(gx_snapshot_max_manifest_bytes, 16 * 1024^2)
  expect_identical(gx_snapshot_max_depth, 8L)
  expect_equal(gx_snapshot_max_members, 650000)
  expect_equal(gx_snapshot_max_structural_units, 1950002)
  expect_identical(gx_snapshot_max_requests, 10000L)
  expect_identical(gx_snapshot_max_resources, 10000L)
  expect_equal(gx_snapshot_max_resource_bytes, 1024^3)
  expect_identical(gx_snapshot_max_tree_entries, 50000L)
  expect_identical(gx_snapshot_max_path_bytes, 1024L)
  expect_identical(gx_snapshot_max_component_bytes, 255L)
  expect_identical(gx_snapshot_max_path_depth, 16L)
  expect_identical(gx_snapshot_max_roles, 16L)
  expect_equal(
    schema$properties$requests$maxItems,
    gx_snapshot_max_requests
  )
  expect_equal(
    schema$properties$resources$maxItems,
    gx_snapshot_max_resources
  )
  expect_equal(
    schema$`$defs`$resource$properties$path$maxLength,
    gx_snapshot_max_path_bytes
  )
  expect_equal(
    schema$`$defs`$resource$properties$bytes$maximum,
    gx_snapshot_max_resource_bytes
  )
  expect_equal(
    schema$`$defs`$resource$properties$roles$maxItems,
    gx_snapshot_max_roles
  )

  dir <- snapshot_test_copy()
  manifest_size <- file.info(snapshot_test_manifest_path(dir))$size[[1L]]
  snapshot_test_expect_error(
    function() testthat::with_mocked_bindings(
      gx_snapshot_read_manifest(dir),
      gx_snapshot_max_manifest_bytes = manifest_size - 1,
      .package = "geoconnexr"
    ),
    subclass = "gx_error_snapshot_budget"
  )
  decoded <- gx_snapshot_parse_json(
    snapshot_test_read_raw(snapshot_test_manifest_path(dir))
  )
  snapshot_test_expect_error(
    function() testthat::with_mocked_bindings(
      gx_snapshot_validate_requests(decoded$requests),
      gx_snapshot_max_requests = 0L,
      .package = "geoconnexr"
    )
  )
  snapshot_test_expect_error(
    function() testthat::with_mocked_bindings(
      gx_snapshot_tree_inventory(
        dir,
        c("catalog/datasets.csv", "requests.csv")
      ),
      gx_snapshot_max_tree_entries = 1L,
      .package = "geoconnexr"
    ),
    subclass = "gx_error_snapshot_budget"
  )
})

test_that("AOI and aggregate budgets fail before resource hashing", {
  hash_state <- new.env(parent = emptyenv())
  hash_state$calls <- 0L
  blocked_hash <- function(...) {
    hash_state$calls <- hash_state$calls + 1L
    stop("pre-hash validation reached hashing", call. = FALSE)
  }

  invalid_aoi <- snapshot_test_copy()
  manifest <- snapshot_test_read_manifest(invalid_aoi)
  manifest$recipe$aoi$identifier <- "2070010"
  snapshot_test_write_manifest(invalid_aoi, manifest)
  snapshot_test_expect_error(function() testthat::with_mocked_bindings(
    gx_snapshot_verify_impl(invalid_aoi),
    gx_snapshot_hash_file = blocked_hash,
    .package = "geoconnexr"
  ))
  expect_identical(hash_state$calls, 0L)

  aggregate <- snapshot_test_copy()
  manifest <- snapshot_test_read_manifest(aggregate)
  manifest$resources[[1L]]$bytes <- 600 * 1024^2
  manifest$resources[[2L]]$bytes <- 600 * 1024^2
  snapshot_test_write_manifest(aggregate, manifest)
  snapshot_test_expect_error(
    function() testthat::with_mocked_bindings(
      gx_snapshot_verify_impl(aggregate),
      gx_snapshot_hash_file = blocked_hash,
      .package = "geoconnexr"
    ),
    subclass = "gx_error_snapshot_budget"
  )
  expect_identical(hash_state$calls, 0L)
})

test_that("resource mutation during hashing fails the post-hash metadata check", {
  dir <- snapshot_test_copy()
  hash_state <- new.env(parent = emptyenv())
  hash_state$calls <- 0L
  mutating_hash <- function(path) {
    hash_state$calls <- hash_state$calls + 1L
    hash <- snapshot_test_sha256(path)
    writeBin(c(snapshot_test_read_raw(path), as.raw(0x00)), path)
    hash
  }

  snapshot_test_expect_error(
    function() testthat::with_mocked_bindings(
      gx_snapshot_verify_impl(dir),
      gx_snapshot_hash_file = mutating_hash,
      .package = "geoconnexr"
    ),
    subclass = "gx_error_snapshot_mutation"
  )
  expect_identical(hash_state$calls, 1L)
})

test_that("resources remain opaque and verification performs no external work", {
  dir <- snapshot_test_copy()
  opaque_path <- file.path(dir, "catalog", "datasets.csv")
  opaque <- as.raw(c(0x00, 0xff, 0x80, 0x01, 0x02, 0x03))
  writeBin(opaque, opaque_path)
  manifest <- snapshot_test_read_manifest(dir)
  manifest <- snapshot_test_rebind_resource(
    manifest,
    dir,
    "catalog/datasets.csv"
  )
  snapshot_test_write_manifest(dir, manifest)
  before <- snapshot_test_tree_state(dir)
  network <- snapshot_test_block_external_work()

  output <- gx_snapshot_verify_impl(dir)

  after <- snapshot_test_tree_state(dir)
  expect_identical(output$status, "verified")
  expect_identical(network$calls, 0L)
  expect_identical(after, before)
})

test_that("snapshot verification internals are not exported", {
  exports <- getNamespaceExports("geoconnexr")

  expect_false("gx_snapshot_verify_impl" %in% exports)
  expect_false("gx_snapshot_verify" %in% exports)
  expect_false("gx_replay" %in% exports)
})
