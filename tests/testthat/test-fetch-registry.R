fetch_registry_test_asset_dir <- function(.local_envir = parent.frame()) {
  source <- system.file("handlers", package = "geoconnexr")
  target <- withr::local_tempdir(.local_envir = .local_envir)
  copied <- file.copy(
    file.path(source, c("registry.yml", "implementations-r.json")),
    target,
    overwrite = TRUE
  )
  stopifnot(all(copied))
  target
}

fetch_registry_test_yaml <- function(asset_dir) {
  yaml::read_yaml(file.path(asset_dir, "registry.yml"))
}

fetch_registry_test_json <- function(asset_dir) {
  jsonlite::fromJSON(
    file.path(asset_dir, "implementations-r.json"), simplifyVector = FALSE
  )
}

fetch_registry_test_write_yaml <- function(x, asset_dir) {
  writeLines(
    yaml::as.yaml(x),
    file.path(asset_dir, "registry.yml"),
    sep = "\n",
    useBytes = TRUE
  )
}

fetch_registry_test_write_json <- function(x, asset_dir) {
  writeLines(
    jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      null = "null",
      pretty = TRUE,
      digits = NA
    ),
    file.path(asset_dir, "implementations-r.json"),
    sep = "\n",
    useBytes = TRUE
  )
}

fetch_registry_test_raw <- function(path) {
  readBin(path, what = "raw", n = as.integer(file.info(path)$size))
}

fetch_registry_test_error <- function(expr) {
  tryCatch(expr, error = identity)
}

test_that("the internal registry loads an exact, validated metadata contract", {
  asset_dir <- fetch_registry_test_asset_dir()
  registry <- gx_handler_registry_load_impl(asset_dir)

  expect_identical(class(registry), "gx_handler_registry")
  expect_identical(
    names(registry),
    c(
      "contract_version", "registry_version", "evaluation", "protocol",
      "allowed_fact_names", "portable_sha256", "implementations_sha256",
      "handlers"
    )
  )
  expect_identical(registry$contract_version, "0.1.0")
  expect_identical(registry$registry_version, 1L)
  expect_identical(registry$evaluation, "first_match_wins")
  expect_identical(
    registry$protocol, c("probe", "plan", "fetch", "normalize")
  )
  expect_identical(
    registry$allowed_fact_names,
    c("access_url", "media_type", "conforms_to")
  )
  expect_identical(
    names(registry$handlers),
    c(
      "id", "precedence", "lifecycle", "outcome", "classifier",
      "implementation_id", "availability", "package", "minimum_version",
      "missing_package_status", "planning_metadata", "payload_class"
    )
  )
  expect_identical(class(registry$handlers), c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(registry$handlers), 9L)
  expect_identical(registry$handlers$id[[9L]], "unknown")
  expect_identical(registry$handlers$outcome[[9L]], "reference_only")
  expect_identical(registry$handlers$availability[[9L]], "classifier_only")
  expect_identical(registry$handlers$classifier[[9L]], list(always = TRUE))
  expect_true(all(registry$handlers$availability[-9L] == "planned"))
  expect_true(all(diff(registry$handlers$precedence) > 0L))
  expect_true(all(vapply(
    registry$handlers$planning_metadata[-9L],
    function(x) xor("function" %in% names(x), "functions" %in% names(x)),
    logical(1)
  )))
  expect_identical(registry$handlers$planning_metadata[[9L]], list())
  expect_identical(gx_handler_registry_validate_impl(registry), invisible(registry))
})

test_that("registry fingerprints are the hashes of the exact raw assets", {
  asset_dir <- fetch_registry_test_asset_dir()
  portable_path <- file.path(asset_dir, "registry.yml")
  implementation_path <- file.path(asset_dir, "implementations-r.json")
  registry <- gx_handler_registry_load_impl(asset_dir)

  expect_identical(
    registry$portable_sha256,
    digest::digest(
      fetch_registry_test_raw(portable_path),
      algo = "sha256",
      serialize = FALSE
    )
  )
  expect_identical(
    registry$implementations_sha256,
    digest::digest(
      fetch_registry_test_raw(implementation_path),
      algo = "sha256",
      serialize = FALSE
    )
  )

  original <- registry
  raw <- fetch_registry_test_raw(portable_path)
  writeBin(c(raw, charToRaw("\n")), portable_path, useBytes = TRUE)
  changed <- gx_handler_registry_load_impl(asset_dir)
  expect_false(identical(changed$portable_sha256, original$portable_sha256))
  expect_identical(
    changed$portable_sha256,
    digest::digest(
      fetch_registry_test_raw(portable_path),
      algo = "sha256",
      serialize = FALSE
    )
  )
  expect_identical(changed$handlers, original$handlers)
  expect_identical(
    changed$implementations_sha256, original$implementations_sha256
  )
})

test_that("asset reads reject nonregular, symlinked, non-UTF-8, and non-LF input", {
  mutate_raw <- function(transform) {
    asset_dir <- fetch_registry_test_asset_dir(.local_envir = environment())
    path <- file.path(asset_dir, "registry.yml")
    writeBin(transform(fetch_registry_test_raw(path)), path, useBytes = TRUE)
    fetch_registry_test_error(gx_handler_registry_load_impl(asset_dir))
  }

  no_final_lf <- mutate_raw(function(x) x[-length(x)])
  bom <- mutate_raw(function(x) c(as.raw(c(0xef, 0xbb, 0xbf)), x))
  crlf <- mutate_raw(function(x) {
    unlist(lapply(x, function(byte) {
      if (identical(byte, as.raw(0x0a))) as.raw(c(0x0d, 0x0a)) else byte
    }), use.names = FALSE)
  })
  invalid_utf8 <- mutate_raw(function(x) c(x[-length(x)], as.raw(0xff), x[length(x)]))

  for (error in list(no_final_lf, bom, crlf, invalid_utf8)) {
    expect_s3_class(error, "gx_error_handler_registry")
    expect_s3_class(error, "gx_error_fetch_plan")
    expect_s3_class(error, "gx_error_fetch_plan_registry_asset")
  }

  directory_asset <- fetch_registry_test_asset_dir()
  path <- file.path(directory_asset, "registry.yml")
  unlink(path)
  dir.create(path)
  expect_error(
    gx_handler_registry_load_impl(directory_asset),
    class = "gx_error_fetch_plan_registry_asset"
  )

  oversized <- fetch_registry_test_asset_dir()
  writeBin(
    rep(as.raw(0x20), .gx_handler_registry_max_asset_bytes + 1L),
    file.path(oversized, "registry.yml"),
    useBytes = TRUE
  )
  expect_error(
    gx_handler_registry_load_impl(oversized),
    class = "gx_error_fetch_plan_registry_asset"
  )

  symlinked <- fetch_registry_test_asset_dir()
  original <- file.path(symlinked, "portable-real.yml")
  file.rename(file.path(symlinked, "registry.yml"), original)
  linked <- suppressWarnings(file.symlink(
    original, file.path(symlinked, "registry.yml")
  ))
  if (isTRUE(linked)) {
    expect_error(
      gx_handler_registry_load_impl(symlinked),
      class = "gx_error_fetch_plan_registry_asset"
    )
  }
})

test_that("portable YAML roots, entries, predicates, and fallback fail closed", {
  malformed <- list(
    extra_root = function(x) {
      x$unexpected <- TRUE
      x
    },
    reordered_facts = function(x) {
      x$allowed_fact_names <- rev(x$allowed_fact_names)
      x
    },
    duplicate_precedence = function(x) {
      x$handlers[[2L]]$precedence <- x$handlers[[1L]]$precedence
      x
    },
    unknown_not_final = function(x) {
      x$handlers <- x$handlers[c(1:7, 9, 8)]
      x
    },
    nonfinal_always = function(x) {
      x$handlers[[1L]]$classifier <- list(always = TRUE)
      x
    },
    invalid_regex = function(x) {
      x$handlers[[4L]]$classifier$all[[1L]]$value <- "(["
      x
    },
    duplicate_values = function(x) {
      values <- x$handlers[[1L]]$classifier$all[[1L]]$values
      x$handlers[[1L]]$classifier$all[[1L]]$values <- c(values, values[[1L]])
      x
    },
    incompatible_fact = function(x) {
      x$handlers[[1L]]$classifier$all[[1L]]$fact <- "media_type"
      x
    },
    explicit_fetch_outcome = function(x) {
      handler <- x$handlers[[1L]]
      handler <- append(handler, list(outcome = "fetch"), after = 2L)
      x$handlers[[1L]] <- handler
      x
    },
    extra_predicate_key = function(x) {
      x$handlers[[1L]]$classifier$all[[1L]]$extra <- "forbidden"
      x
    }
  )

  for (name in names(malformed)) {
    asset_dir <- fetch_registry_test_asset_dir()
    fetch_registry_test_write_yaml(
      malformed[[name]](fetch_registry_test_yaml(asset_dir)), asset_dir
    )
    expect_error(
      gx_handler_registry_load_impl(asset_dir),
      class = "gx_error_handler_registry",
      info = name
    )
  }

  duplicate_key <- fetch_registry_test_asset_dir()
  path <- file.path(duplicate_key, "registry.yml")
  original <- fetch_registry_test_raw(path)
  writeBin(c(charToRaw("version: 1\n"), original), path, useBytes = TRUE)
  expect_error(
    gx_handler_registry_load_impl(duplicate_key),
    class = "gx_error_fetch_plan_registry_parse"
  )

  alias <- fetch_registry_test_asset_dir()
  path <- file.path(alias, "registry.yml")
  original <- fetch_registry_test_raw(path)
  writeBin(c(charToRaw("forbidden: &anchor value\n"), original), path, useBytes = TRUE)
  expect_error(
    gx_handler_registry_load_impl(alias),
    class = "gx_error_fetch_plan_registry_parse"
  )
})

test_that("R implementation roots and entries reconcile exactly with classifiers", {
  malformed <- list(
    extra_root = function(x) {
      x$unexpected <- TRUE
      x
    },
    missing_id = function(x) {
      names(x$implementations)[[1L]] <- "renamed"
      x
    },
    duplicate_implementation_id = function(x) {
      x$implementations$csv$implementation_id <-
        x$implementations$ogc_api_features$implementation_id
      x
    },
    executable_classifier_only = function(x) {
      x$implementations$csv$availability <- "classifier_only"
      x
    },
    unplanned_executable = function(x) {
      x$implementations$csv[["function"]] <- NULL
      x
    },
    null_package_status = function(x) {
      x$implementations$ogc_api_features$missing_package_status <-
        "skipped_missing_pkg"
      x
    },
    unlisted_package = function(x) {
      x$implementations$csv$package <- "curl"
      x
    },
    extra_entry_key = function(x) {
      x$implementations$csv$command <- "read_csv"
      x
    },
    lifecycle_without_warning = function(x) {
      x$implementations$nwis_legacy_iv$warning <- NULL
      x
    },
    deprecated_without_metadata = function(x) {
      x$implementations$nwis_legacy_iv$lifecycle <- NULL
      x$implementations$nwis_legacy_iv$warning <- NULL
      x
    },
    active_with_deprecation_metadata = function(x) {
      entry <- x$implementations$csv
      entry$lifecycle <- "deprecated_compatibility"
      entry$warning <- "Unexpected warning metadata."
      x$implementations$csv <- entry[c(
        "implementation_id", "availability", "package", "function",
        "lifecycle", "warning", "plan_must_record",
        "missing_package_status"
      )]
      x
    },
    review_mismatch = function(x) {
      x$checked$edr4r$minimum_version <- "9.9.9"
      x
    }
  )

  for (name in names(malformed)) {
    asset_dir <- fetch_registry_test_asset_dir()
    fetch_registry_test_write_json(
      malformed[[name]](fetch_registry_test_json(asset_dir)), asset_dir
    )
    expect_error(
      gx_handler_registry_load_impl(asset_dir),
      class = "gx_error_handler_registry",
      info = name
    )
  }

  duplicate_key <- fetch_registry_test_asset_dir()
  path <- file.path(duplicate_key, "implementations-r.json")
  original <- rawToChar(fetch_registry_test_raw(path))
  duplicate <- sub(
    '"version": 1,',
    '"version": 1, "version": 1,',
    original,
    fixed = TRUE
  )
  writeBin(charToRaw(duplicate), path, useBytes = TRUE)
  expect_error(
    gx_handler_registry_load_impl(duplicate_key),
    class = "gx_error_handler_registry"
  )
})

test_that("forged registry objects and hashes fail typed validation", {
  registry <- gx_handler_registry_load_impl()
  mutations <- list(
    extra_class = function(x) {
      class(x) <- c("gx_handler_registry", "list")
      x
    },
    uppercase_hash = function(x) {
      x$portable_sha256 <- toupper(x$portable_sha256)
      x
    },
    extra_column = function(x) {
      x$handlers$unexpected <- NA_character_
      x
    },
    duplicate_implementation_id = function(x) {
      x$handlers$implementation_id[[2L]] <- x$handlers$implementation_id[[1L]]
      x
    },
    executable_unknown = function(x) {
      x$handlers$availability[[9L]] <- "planned"
      x
    },
    empty_planning_metadata = function(x) {
      x$handlers$planning_metadata[[1L]] <- list()
      x
    },
    lifecycle_without_warning = function(x) {
      x$handlers$planning_metadata[[4L]]$warning <- NULL
      x
    },
    deprecated_without_metadata = function(x) {
      x$handlers$planning_metadata[[4L]]$lifecycle <- NULL
      x$handlers$planning_metadata[[4L]]$warning <- NULL
      x
    },
    active_with_deprecation_metadata = function(x) {
      planning <- x$handlers$planning_metadata[[8L]]
      x$handlers$planning_metadata[[8L]] <- list(
        "function" = planning[["function"]],
        lifecycle = "deprecated_compatibility",
        warning = "Unexpected warning metadata.",
        plan_must_record = planning$plan_must_record
      )
      x
    },
    unlisted_package = function(x) {
      x$handlers$package[[8L]] <- "curl"
      x
    }
  )
  for (name in names(mutations)) {
    expect_error(
      gx_handler_registry_validate_impl(mutations[[name]](registry)),
      class = "gx_error_fetch_plan",
      info = name
    )
  }
})

test_that("registry loading is offline, read-only, and does not inspect packages", {
  asset_dir <- fetch_registry_test_asset_dir()
  files <- file.path(asset_dir, c("registry.yml", "implementations-r.json"))
  before_bytes <- lapply(files, fetch_registry_test_raw)
  before_namespaces <- intersect(
    c("edr4r", "dataRetrieval", "readr"), loadedNamespaces()
  )
  testthat::local_mocked_bindings(
    gx_default_dns_resolver = function(...) stop("network forbidden"),
    gx_default_performer = function(...) stop("network forbidden"),
    gx_default_file_performer = function(...) stop("network forbidden"),
    gx_mainstem_lookup_download = function(...) stop("download forbidden")
  )

  registry <- gx_handler_registry_load_impl(asset_dir)

  expect_s3_class(registry, "gx_handler_registry")
  expect_identical(lapply(files, fetch_registry_test_raw), before_bytes)
  expect_identical(
    intersect(c("edr4r", "dataRetrieval", "readr"), loadedNamespaces()),
    before_namespaces
  )
  expect_setequal(list.files(asset_dir), basename(files))
})

test_that("registry failures redact traces, paths, and parser details", {
  asset_dir <- fetch_registry_test_asset_dir()
  secret <- "registry-secret-token"
  path <- file.path(asset_dir, "registry.yml")
  writeBin(
    charToRaw(paste0("version: [", secret, "\n")),
    path,
    useBytes = TRUE
  )
  error <- fetch_registry_test_error(gx_handler_registry_load_impl(asset_dir))

  expect_s3_class(error, "gx_error_handler_registry")
  expect_s3_class(error, "gx_error_fetch_plan")
  expect_null(error$call)
  expect_identical(nrow(error$trace), 0L)
  expect_false(grepl(secret, conditionMessage(error), fixed = TRUE))
  expect_false(grepl(asset_dir, conditionMessage(error), fixed = TRUE))
})
