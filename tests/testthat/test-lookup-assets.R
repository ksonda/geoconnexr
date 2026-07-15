test_that("runtime mainstem registry matches the pinned evidence", {
  spec <- gx_mainstem_lookup_spec("v3.2")
  expect_identical(spec$lookup_id, "ref-rivers-nhdpv2-v3.2")
  expect_identical(spec$release, "v3.2")
  expect_identical(spec$tag_commit, "fd42072b1e9bf9de33cd047052b99508753c7fea")
  expect_identical(spec$asset_id, "378822640")
  expect_identical(spec$columns, c("uri", "comid"))
  expect_identical(spec$rows, 2357730L)
  expect_identical(spec$bytes, 120422425L)
  expect_identical(
    spec$sha256,
    "d73898e3eabbdc4aef493e2cb47d4bddcf4762872ca3971c74e1dbe0a4b73acb"
  )
  expect_identical(spec$forward_cardinality, "zero_or_one")
  expect_identical(spec$active_state, "non_superseded_at_release")
  expect_identical(spec$license, "CC0-1.0")
  expect_identical(
    spec$known_answers,
    tibble::tibble(
      comid = c("17789327", "13637491"),
      mainstem_uri = c(
        "https://geoconnex.us/ref/mainstems/1622734",
        "https://geoconnex.us/ref/mainstems/323742"
      )
    )
  )
  expect_identical(spec$known_absent, "999999999")
  expect_false(grepl("levelpath", spec$source_url, ignore.case = TRUE))
})

test_that("lookup fixture manifest pins observed and synthetic CSV bytes", {
  manifest <- jsonlite::fromJSON(
    gx_lookup_fixture("manifest-v1.json"),
    simplifyVector = FALSE
  )
  entries <- manifest$fixtures
  paths <- vapply(entries, `[[`, character(1), "path")
  hashes <- vapply(entries, `[[`, character(1), "stored_sha256")
  observed <- vapply(paths, function(path) {
    digest::digest(
      file = gx_lookup_fixture(path),
      algo = "sha256",
      serialize = FALSE
    )
  }, character(1))
  expect_identical(unname(observed), unname(hashes))
  kinds <- vapply(entries, `[[`, character(1), "evidence_kind")
  expect_true("observed_minimized_mapping_rows" %in% kinds)
  expect_true("synthetic_adversarial_mapping" %in% kinds)
})

test_that("lookup info reports a miss without creating data directories", {
  data_dir <- tempfile("missing-data-")
  info <- gx_mainstem_lookup_info(data_dir = data_dir)
  expect_identical(
    names(info),
    c(
      "lookup_id", "release", "tag_commit", "asset_id", "available",
      "verified", "path", "expected_bytes", "expected_sha256", "bytes",
      "sha256", "installed_at", "verified_at", "source_url", "license",
      "active_state", "currentness_policy", "cache_origin"
    )
  )
  expect_false(dir.exists(data_dir))
  expect_false(info$available)
  expect_false(info$verified)
  expect_identical(info$cache_origin, "missing")
  expect_identical(info$expected_bytes, 120422425)
  expect_identical(
    info$expected_sha256,
    "d73898e3eabbdc4aef493e2cb47d4bddcf4762872ca3971c74e1dbe0a4b73acb"
  )
  expect_identical(info$active_state, "non_superseded_at_release")
  expect_identical(info$currentness_policy, "not_checked")
  expect_true(is.na(info$bytes))
  expect_true(is.na(info$sha256))
})

test_that("zero-length COMID mapping is typed and requires no lookup", {
  data_dir <- tempfile("missing-data-")
  out <- gx_comid_to_mainstem_impl(character(), data_dir = data_dir)
  expect_s3_class(out, "gx_comid_crosswalk")
  expect_identical(names(out), names(gx_empty_comid_crosswalk()))
  expect_identical(nrow(out), 0L)
  expect_false(dir.exists(data_dir))
  metadata <- attr(out, "gx_crosswalk")
  expect_identical(metadata$operation, "comid_to_mainstem")
  expect_identical(metadata$mapping$currentness_policy, "not_checked")
  expect_identical(metadata$mapping$cache_origin, "not_loaded")
})

test_that("COMID validation fails before lookup or network access", {
  called <- 0L
  withr::local_options(geoconnexr.file_performer = function(...) {
    called <<- called + 1L
    stop("network should not be called")
  })
  invalid <- list(
    17789327,
    NA_character_,
    "",
    " ",
    "017789327",
    "1e3",
    "12\n34",
    paste(rep("9", 11L), collapse = "")
  )
  for (value in invalid) {
    expect_error(
      gx_comid_to_mainstem_impl(value, data_dir = tempfile()),
      class = "gx_error_crosswalk_input"
    )
  }
  expect_identical(called, 0L)
  expect_error(
    gx_comid_to_mainstem_impl("17789327", data_dir = tempfile()),
    class = "gx_error_crosswalk_lookup_missing"
  )
  expect_identical(called, 0L)
})

test_that("local installation verifies provenance and supports offline mapping", {
  data_dir <- withr::local_tempdir()
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  withr::local_options(geoconnexr.file_performer = function(...) {
    stop("offline local mapping attempted network access")
  })
  info <- gx_mainstem_lookup_install(
    source = "file",
    file = fixture,
    version = spec$release,
    confirm = FALSE,
    offline = TRUE,
    data_dir = data_dir
  )
  expect_true(info$available)
  expect_true(info$verified)
  expect_identical(info$bytes, as.double(spec$bytes))
  expect_identical(info$sha256, spec$sha256)
  expect_identical(info$expected_bytes, as.double(spec$bytes))
  expect_identical(info$expected_sha256, spec$sha256)
  expect_identical(info$tag_commit, spec$tag_commit)
  expect_identical(info$asset_id, spec$asset_id)
  expect_identical(info$license, spec$license)
  expect_identical(info$active_state, spec$active_state)
  expect_identical(info$currentness_policy, "not_checked")
  expect_identical(info$cache_origin, "local_import")
  expect_true(file.exists(gx_lookup_data_marker(data_dir)))

  receipt_path <- file.path(dirname(info$path), "receipt-v1.json")
  receipt_text <- paste(readLines(receipt_path, warn = FALSE), collapse = "\n")
  expect_false(grepl(normalizePath(fixture), receipt_text, fixed = TRUE))
  expect_false(grepl("sig=", receipt_text, fixed = TRUE))

  out <- gx_comid_to_mainstem_impl(
    c("17789327", "999999999", "17789327", "13637491"),
    version = spec$release,
    data_dir = data_dir
  )
  expect_identical(out$input_index, 1:4)
  expect_identical(
    out$status,
    c("matched", "not_found", "matched", "matched")
  )
  expect_identical(
    out$mainstem_uri,
    c(
      "https://geoconnex.us/ref/mainstems/1622734",
      NA_character_,
      "https://geoconnex.us/ref/mainstems/1622734",
      "https://geoconnex.us/ref/mainstems/323742"
    )
  )
  expect_true(all(out$mapping_release == spec$release))
  expect_identical(
    attr(out, "gx_crosswalk")$mapping$active_state,
    "non_superseded_at_release"
  )
  expect_identical(
    attr(out, "gx_crosswalk")$mapping$currentness_policy,
    "not_checked"
  )
  expect_true(any(
    attr(out, "gx_crosswalk")$diagnostics$code ==
      "mainstem_currentness_not_checked"
  ))
})

test_that("future zero-to-many lookup adapters retain deterministic ambiguity", {
  data_dir <- withr::local_tempdir()
  fixture <- gx_lookup_fixture("nhdpv2-lookup-ambiguous.synthetic.csv")
  spec <- gx_lookup_test_spec(
    fixture,
    forward_cardinality = "zero_or_many",
    known_answers = tibble::tibble(
      comid = "600",
      mainstem_uri = "https://geoconnex.us/ref/mainstems/30"
    ),
    known_absent = "999"
  )
  gx_lookup_mock_spec(spec)
  gx_mainstem_lookup_install(
    source = "file", file = fixture, version = spec$release,
    confirm = FALSE, offline = TRUE, data_dir = data_dir
  )
  out <- gx_comid_to_mainstem_impl(
    c("500", "600"), version = spec$release, data_dir = data_dir
  )
  expect_identical(out$input_index, c(1L, 1L, 2L))
  expect_identical(out$status, c("ambiguous", "ambiguous", "matched"))
  expect_identical(out$match_index, c(1L, 2L, 1L))
  expect_identical(
    out$mainstem_uri,
    c(
      "https://geoconnex.us/ref/mainstems/10",
      "https://geoconnex.us/ref/mainstems/20",
      "https://geoconnex.us/ref/mainstems/30"
    )
  )
  expect_identical(attr(out, "gx_crosswalk")$ambiguous_input_count, 1L)
})

test_that("tampering is detected and a failed replacement preserves valid data", {
  data_dir <- withr::local_tempdir()
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  installed <- gx_mainstem_lookup_install(
    source = "file", file = fixture, version = spec$release,
    confirm = FALSE, offline = TRUE, data_dir = data_dir
  )
  bad <- tempfile(fileext = ".csv")
  expect_true(file.copy(fixture, bad))
  connection <- file(bad, open = "r+b")
  writeBin(charToRaw("X"), connection)
  close(connection)
  expect_error(
    gx_mainstem_lookup_install(
      source = "file", file = bad, version = spec$release,
      force = TRUE, confirm = FALSE, offline = TRUE, data_dir = data_dir
    ),
    class = "gx_error_crosswalk_lookup_integrity"
  )
  expect_true(gx_mainstem_lookup_info(spec$release, data_dir)$verified)

  installed_path <- installed$path[[1]]
  connection <- file(installed_path, open = "r+b")
  writeBin(charToRaw("X"), connection)
  close(connection)
  invalid <- gx_mainstem_lookup_info(spec$release, data_dir)
  expect_true(invalid$available)
  expect_false(invalid$verified)
  expect_identical(invalid$cache_origin, "invalid")
  expect_error(
    gx_comid_to_mainstem_impl(
      "17789327", version = spec$release, data_dir = data_dir
    ),
    class = "gx_error_crosswalk_lookup_integrity"
  )
})

test_that("schema-invalid pinned bytes fail closed before installation", {
  bad <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "comid,uri",
      "17789327,https://geoconnex.us/ref/mainstems/1622734"
    ),
    bad,
    useBytes = TRUE
  )
  spec <- gx_lookup_test_spec(
    bad,
    known_answers = tibble::tibble(
      comid = character(), mainstem_uri = character()
    ),
    known_absent = "999"
  )
  gx_lookup_mock_spec(spec)
  data_dir <- withr::local_tempdir()
  expect_error(
    gx_mainstem_lookup_install(
      source = "file", file = bad, version = spec$release,
      confirm = FALSE, offline = TRUE, data_dir = data_dir
    ),
    class = "gx_error_crosswalk_lookup_schema"
  )
  expect_false(gx_mainstem_lookup_info(spec$release, data_dir)$available)
})

test_that("offline release install fails before writes or performer access", {
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  called <- 0L
  withr::local_options(geoconnexr.file_performer = function(...) {
    called <<- called + 1L
    stop("network should not run")
  })
  data_dir <- tempfile("offline-data-")
  expect_error(
    gx_mainstem_lookup_install(
      source = "release", version = spec$release,
      confirm = FALSE, offline = TRUE, data_dir = data_dir
    ),
    class = "gx_error_crosswalk_lookup_offline"
  )
  expect_identical(called, 0L)
  expect_false(dir.exists(data_dir))
})

test_that("unmarked non-empty data directories are never adopted", {
  data_dir <- withr::local_tempdir()
  writeLines("unrelated", file.path(data_dir, "foreign.txt"))
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  expect_error(
    gx_mainstem_lookup_install(
      source = "file", file = fixture, version = spec$release,
      confirm = FALSE, offline = TRUE, data_dir = data_dir
    ),
    class = "gx_error_crosswalk_lookup_ownership"
  )
  expect_true(file.exists(file.path(data_dir, "foreign.txt")))
})

test_that("disk performer enforces bytes, identity encoding, and cleanup", {
  withr::local_options(
    geoconnexr.dns_resolver = function(host) "93.184.216.34",
    geoconnexr.file_performer = gx_lookup_test_file_performer(function(request) {
      list(
        status = 200L,
        headers = list(
          `Content-Type` = "text/plain",
          `Content-Length` = "3",
          `Content-Encoding` = "identity"
        ),
        body = charToRaw("abc")
      )
    })
  )
  client <- gx_client("pid", retries = 0L, max_bytes = 10L, cache = FALSE)
  path <- tempfile()
  response <- gx_http_download_file(client, "https://example.org/a", path)
  expect_identical(response$status, 200L)
  expect_identical(response$bytes, 3)
  expect_identical(readBin(path, raw(), n = 3L), charToRaw("abc"))

  withr::local_options(
    geoconnexr.file_performer = gx_lookup_test_file_performer(function(request) {
      list(
        status = 200L,
        headers = list(`Content-Length` = "4"),
        body = charToRaw("abc")
      )
    })
  )
  bad_length <- tempfile()
  expect_error(
    gx_http_download_file(client, "https://example.org/b", bad_length),
    class = "gx_error_download"
  )
  expect_false(file.exists(bad_length))

  withr::local_options(
    geoconnexr.file_performer = gx_lookup_test_file_performer(function(request) {
      list(
        status = 200L,
        headers = list(
          `Content-Length` = "3",
          `Content-Encoding` = "gzip"
        ),
        body = charToRaw("abc")
      )
    })
  )
  encoded <- tempfile()
  expect_error(
    gx_http_download_file(client, "https://example.org/c", encoded),
    class = "gx_error_download"
  )
  expect_false(file.exists(encoded))
})

test_that("file downloads share host throttle slots and offline bypasses them", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  now <- 0
  clock_calls <- 0L
  sleeps <- numeric()
  starts <- numeric()
  hosts <- character()
  delays <- numeric()
  performer_calls <- 0L
  withr::local_options(list(
    geoconnexr.dns_resolver = function(host) {
      rep("93.184.216.34", length(host))
    },
    geoconnexr.clock = function() {
      as.POSIXct("2026-07-12 16:00:00", tz = "UTC")
    },
    geoconnexr.throttle_clock = function() {
      clock_calls <<- clock_calls + 1L
      now
    },
    geoconnexr.throttle_sleep = function(seconds) {
      sleeps <<- c(sleeps, seconds)
      now <<- now + seconds
    },
    geoconnexr.file_performer = gx_lookup_test_file_performer(function(request) {
      performer_calls <<- performer_calls + 1L
      starts <<- c(starts, now)
      hosts <<- c(hosts, request$resolved_host)
      delays <<- c(delays, request$throttle_delay)
      list(
        status = 200L,
        headers = list(`Content-Length` = "1"),
        body = as.raw(1L)
      )
    })
  ))
  cache_dir <- withr::local_tempdir()
  client <- gx_client(
    "pid", retries = 0L, min_interval = 2,
    cache = FALSE, cache_dir = cache_dir
  )

  first <- gx_http_download_file(
    client, "https://one.example/a", tempfile()
  )
  other_host <- gx_http_download_file(
    client, "https://two.example/a", tempfile()
  )
  same_host <- gx_http_download_file(
    client, "https://one.example/b", tempfile()
  )

  expect_identical(starts, c(0, 0, 2))
  expect_identical(hosts, c("one.example", "two.example", "one.example"))
  expect_identical(delays, c(0, 0, 2))
  expect_identical(sleeps, 2)
  expect_identical(first$request$throttle_delay, 0)
  expect_identical(other_host$request$throttle_delay, 0)
  expect_identical(same_host$request$throttle_delay, 2)

  offline <- gx_client(
    "pid", retries = 0L, min_interval = 2, offline = TRUE,
    cache = TRUE, cache_dir = cache_dir
  )
  calls_before_offline <- c(
    clock = clock_calls,
    sleeper = length(sleeps),
    performer = performer_calls
  )
  expect_error(
    gx_http_download_file(
      offline, "https://one.example/offline", tempfile()
    ),
    class = "gx_error_offline_miss"
  )
  expect_identical(
    c(
      clock = clock_calls,
      sleeper = length(sleeps),
      performer = performer_calls
    ),
    calls_before_offline
  )
})

test_that("default disk performer stops before writing a chunk past its cap", {
  request <- list(
    url = "https://example.org/asset.csv",
    headers = list(),
    max_bytes = 5,
    timeout = 1,
    user_agent = "geoconnexr-test",
    resolved_ip = character(),
    resolved_host = "example.org",
    resolved_port = 443L
  )
  testthat::local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) {
      fun(as.raw(c(1L, 2L, 3L)))
      fun(as.raw(c(4L, 5L, 6L)))
      stop("the second chunk should abort")
    },
    .package = "curl"
  )
  path <- tempfile()
  on.exit(unlink(path, force = TRUE), add = TRUE)
  expect_error(
    gx_default_file_performer(request, path),
    class = "gx_error_download"
  )
  expect_identical(as.double(file.info(path)$size[[1]]), 3)
})

test_that("default disk performer withholds transport URL warnings and errors", {
  sentinel <- "signed-secret-must-not-escape"
  request <- list(
    url = paste0("https://example.org/asset.csv?sig=", sentinel),
    headers = list(),
    max_bytes = 5,
    timeout = 1,
    user_agent = "geoconnexr-test",
    resolved_ip = character(),
    resolved_host = "example.org",
    resolved_port = 443L
  )
  testthat::local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) {
      warning(paste("transport warning", url), call. = FALSE)
      stop(paste("transport error", url), call. = FALSE)
    },
    .package = "curl"
  )
  path <- tempfile()
  on.exit(unlink(path, force = TRUE), add = TRUE)
  observed_warnings <- character()
  condition <- withCallingHandlers(
    tryCatch(
      gx_default_file_performer(request, path),
      error = identity
    ),
    warning = function(cnd) {
      observed_warnings <<- c(observed_warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(condition, "gx_error_download")
  expect_length(observed_warnings, 0L)
  expect_false(grepl(sentinel, conditionMessage(condition), fixed = TRUE))
})

test_that("lookup scanning rejects exact duplicate target rows", {
  duplicate <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "uri,comid",
      "https://geoconnex.us/ref/mainstems/10,500",
      "https://geoconnex.us/ref/mainstems/10,500",
      "https://geoconnex.us/ref/mainstems/30,600"
    ),
    duplicate,
    useBytes = TRUE
  )
  spec <- gx_lookup_test_spec(
    duplicate,
    known_answers = tibble::tibble(
      comid = character(), mainstem_uri = character()
    ),
    known_absent = "999"
  )
  expect_error(
    gx_mainstem_lookup_scan(duplicate, spec, targets = "500"),
    class = "gx_error_crosswalk_lookup_integrity"
  )
})

test_that("mainstem release download validates redirects without retaining secrets", {
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  body <- readBin(fixture, raw(), n = file.info(fixture)$size[[1]])
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  calls <- character()
  withr::local_options(
    geoconnexr.dns_resolver = function(host) "93.184.216.34",
    geoconnexr.file_performer = gx_lookup_test_file_performer(function(request) {
      calls <<- c(calls, request$url)
      host <- httr2::url_parse(request$url)$hostname
      if (identical(host, "example.org")) {
        return(list(
          status = 302L,
          headers = list(
            Location = "https://cdn.example.org/download/blob?sig=top-secret",
            `Content-Length` = "0"
          ),
          body = raw()
        ))
      }
      list(
        status = 200L,
        headers = list(
          `Content-Type` = "text/csv",
          `Content-Length` = as.character(length(body))
        ),
        body = body
      )
    })
  )
  data_dir <- withr::local_tempdir()
  info <- gx_mainstem_lookup_install(
    source = "release", version = spec$release,
    confirm = FALSE, offline = FALSE, data_dir = data_dir
  )
  expect_true(info$verified)
  expect_identical(length(calls), 2L)
  expect_true(grepl("sig=top-secret", calls[[2]], fixed = TRUE))
  receipt <- paste(
    readLines(file.path(dirname(info$path), "receipt-v1.json"), warn = FALSE),
    collapse = "\n"
  )
  expect_false(grepl("top-secret", receipt, fixed = TRUE))
  expect_match(receipt, "example.org", fixed = TRUE)
  expect_match(receipt, "cdn.example.org", fixed = TRUE)
})

test_that("redirect loops and partial file failures leave no usable asset", {
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  withr::local_options(
    geoconnexr.dns_resolver = function(host) "93.184.216.34",
    geoconnexr.file_performer = gx_lookup_test_file_performer(function(request) {
      list(
        status = 302L,
        headers = list(Location = spec$source_url, `Content-Length` = "0"),
        body = raw()
      )
    })
  )
  stage_dir <- withr::local_tempdir()
  target <- file.path(stage_dir, "lookup.csv")
  expect_error(
    gx_mainstem_lookup_download(target, spec),
    class = "gx_error_crosswalk_download"
  )
  expect_false(file.exists(target))

  withr::local_options(geoconnexr.file_performer = function(request, path) {
    writeBin(charToRaw("partial"), path)
    stop("socket reset")
  })
  partial <- file.path(stage_dir, "partial.csv")
  client <- gx_client("pid", retries = 0L, max_bytes = 20L, cache = FALSE)
  expect_error(
    gx_http_download_file(client, "https://example.org/partial", partial),
    class = "gx_error_download"
  )
  expect_false(file.exists(partial))
})

test_that("COMID crosswalk row and metadata validators fail on corruption", {
  data_dir <- withr::local_tempdir()
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  gx_mainstem_lookup_install(
    source = "file", file = fixture, version = spec$release,
    confirm = FALSE, offline = TRUE, data_dir = data_dir
  )
  out <- gx_comid_to_mainstem_impl(
    c("17789327", "13637491"), version = spec$release, data_dir = data_dir
  )
  reordered <- out[2:1, , drop = FALSE]
  attr(reordered, "gx_crosswalk") <- attr(out, "gx_crosswalk")
  expect_error(
    gx_validate_comid_crosswalk(reordered),
    class = "gx_error_crosswalk_contract"
  )
  metadata <- attr(out, "gx_crosswalk")
  metadata$mapping$asset_sha256 <- "wrong"
  expect_error(
    gx_validate_comid_crosswalk(out, metadata),
    class = "gx_error_crosswalk_contract"
  )

  bad_comid <- out
  bad_comid$requested_comid[[1]] <- "evil"
  attr(bad_comid, "gx_crosswalk") <- attr(out, "gx_crosswalk")
  expect_error(
    gx_validate_comid_crosswalk(bad_comid),
    class = "gx_error_crosswalk_contract"
  )

  metadata <- attr(out, "gx_crosswalk")
  metadata$mapping$asset_sha256 <- paste(rep("0", 64L), collapse = "")
  expect_error(
    gx_validate_comid_crosswalk(out, metadata),
    class = "gx_error_crosswalk_contract"
  )

  metadata <- attr(out, "gx_crosswalk")
  metadata$mapping$release <- "forged-v1"
  expect_error(
    gx_validate_comid_crosswalk(out, metadata),
    class = "gx_error_crosswalk_contract"
  )

  metadata <- attr(out, "gx_crosswalk")
  metadata$mapping$asset_rows <- NA_integer_
  expect_error(
    gx_validate_comid_crosswalk(out, metadata),
    class = "gx_error_crosswalk_contract"
  )

  metadata <- attr(out, "gx_crosswalk")
  metadata$mapping$cache_origin <- "not_loaded"
  metadata$mapping$installed_at <- as.POSIXct(NA, tz = "UTC")
  metadata$mapping$verified_at <- as.POSIXct(NA, tz = "UTC")
  metadata$retrieved_at <- as.POSIXct(NA, tz = "UTC")
  expect_error(
    gx_validate_comid_crosswalk(out, metadata),
    class = "gx_error_crosswalk_contract"
  )
})

test_that("receipt parser rejects coercible JSON shapes and false source chains", {
  data_dir <- withr::local_tempdir()
  fixture <- gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv")
  spec <- gx_lookup_test_spec(fixture)
  gx_lookup_mock_spec(spec)
  info <- gx_mainstem_lookup_install(
    source = "file", file = fixture, version = spec$release,
    confirm = FALSE, offline = TRUE, data_dir = data_dir
  )
  receipt <- jsonlite::fromJSON(
    file.path(dirname(info$path), "receipt-v1.json"),
    simplifyVector = FALSE,
    bigint_as_char = TRUE
  )

  object_version <- receipt
  object_version$receipt_version <- list(value = 1L)
  fractional_bytes <- receipt
  fractional_bytes$asset_bytes <- spec$bytes + 0.5
  scalar_hosts <- receipt
  scalar_hosts$redirect_hosts <- "example.org"
  false_local_chain <- receipt
  false_local_chain$redirect_hosts <- list("example.org")
  false_release_chain <- receipt
  false_release_chain$source <- "release_download"
  false_release_chain$redirect_hosts <- list()

  corruptions <- list(
    object_version,
    fractional_bytes,
    scalar_hosts,
    false_local_chain,
    false_release_chain
  )
  for (index in seq_along(corruptions)) {
    path <- file.path(data_dir, paste0("bad-receipt-", index, ".json"))
    jsonlite::write_json(
      corruptions[[index]],
      path,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    )
    expect_error(
      gx_mainstem_lookup_read_receipt(path, spec),
      class = "gx_error_crosswalk_lookup_integrity"
    )
  }
})

test_that("swap failure restores or retains the prior installation", {
  restore_case <- function() {
    parent <- withr::local_tempdir()
    stage <- file.path(parent, "stage")
    final <- file.path(parent, "final")
    dir.create(stage)
    dir.create(final)
    writeLines("new", file.path(stage, "identity"))
    writeLines("old", file.path(final, "identity"))
    calls <- 0L
    real_rename <- base::file.rename
    testthat::local_mocked_bindings(
      gx_lookup_rename = function(from, to) {
        calls <<- calls + 1L
        if (calls == 2L) return(FALSE)
        real_rename(from, to)
      },
      .package = "geoconnexr"
    )
    condition <- expect_error(
      gx_mainstem_lookup_swap(stage, final),
      class = "gx_error_crosswalk_lookup_io"
    )
    expect_true(condition$rollback_restored)
    expect_identical(readLines(file.path(final, "identity")), "old")
    expect_identical(readLines(file.path(stage, "identity")), "new")
  }

  retain_case <- function() {
    parent <- withr::local_tempdir()
    stage <- file.path(parent, "stage")
    final <- file.path(parent, "final")
    dir.create(stage)
    dir.create(final)
    writeLines("new", file.path(stage, "identity"))
    writeLines("old", file.path(final, "identity"))
    calls <- 0L
    real_rename <- base::file.rename
    testthat::local_mocked_bindings(
      gx_lookup_rename = function(from, to) {
        calls <<- calls + 1L
        if (calls == 1L) return(real_rename(from, to))
        FALSE
      },
      .package = "geoconnexr"
    )
    condition <- expect_error(
      gx_mainstem_lookup_swap(stage, final),
      class = "gx_error_crosswalk_lookup_io"
    )
    backups <- list.files(
      parent,
      pattern = "^[.]backup-",
      all.files = TRUE,
      full.names = TRUE
    )
    expect_false(condition$rollback_restored)
    expect_length(backups, 1L)
    expect_identical(
      condition$recovery_path,
      normalizePath(backups, winslash = "/")
    )
    expect_identical(readLines(file.path(backups, "identity")), "old")
    expect_identical(readLines(file.path(stage, "identity")), "new")
  }

  restore_case()
  retain_case()
})
