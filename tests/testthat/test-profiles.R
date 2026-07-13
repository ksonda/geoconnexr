gx_profile_fixture_text <- function(...) {
  paste(
    readLines(testthat::test_path("..", "fixtures", "jsonld", ...), warn = FALSE),
    collapse = "\n"
  )
}

test_that("location parser tolerates the checked reference-gage profile", {
  text <- gx_profile_fixture_text("observed", "reference-gage-1000001.min.json")
  out <- gx_parse_location(text)

  expect_s3_class(out, "gx_location")
  expect_equal(nrow(out), 1L)
  expect_equal(out$site_uri, "https://geoconnex.us/ref/gages/1000001")
  expect_equal(out$provider_uri, "https://waterdata.usgs.gov")
  expect_equal(out$mainstem_uri, "https://geoconnex.us/ref/mainstems/1622734")
  expect_equal(out$geometry_wkt, "POINT (-107.2826306 35.9456833)")
  expect_equal(out$site_type, "hydrometricStation")
  expect_contains(attr(out, "diagnostics")$code, "known_prefix_repaired")
  expect_contains(attr(out, "diagnostics")$code, "literal_location_type")
  expect_error(gx_parse_location(text, strict = TRUE), class = "gx_error_parser_strict")
})

test_that("incomplete observed datasets remain visible", {
  text <- gx_profile_fixture_text("observed", "reference-gage-1000001.min.json")
  out <- gx_parse_datasets(text)

  expect_s3_class(out, "gx_datasets")
  expect_equal(nrow(out), 1L)
  expect_match(out$dataset_id, "^[0-9a-f]{64}$")
  expect_true(is.na(out$distribution_id))
  expect_true(is.na(out$variable_id))
  expect_false(out$fetchable)
  expect_contains(attr(out, "diagnostics")$code, "missing_distribution")
  expect_contains(attr(out, "diagnostics")$code, "missing_variable")
})

test_that("compact, expanded graph, and aliased profiles converge", {
  cases <- c("standard-compact.json", "expanded-at-graph.json", "aliased-singletons.json")
  for (case in cases) {
    text <- gx_profile_fixture_text("synthetic", case)
    location <- gx_parse_location(text)
    datasets <- gx_parse_datasets(text)
    expect_equal(nrow(location), 1L, info = case)
    expect_equal(nrow(datasets), 1L, info = case)
    expect_match(datasets$dataset_id, "^[0-9a-f]{64}$", info = case)
    expect_true(datasets$fetchable, info = case)
  }
})

test_that("dataset parser creates a deterministic distribution-variable product", {
  text <- gx_profile_fixture_text("synthetic", "missing-geometry-open-temporal-multi.json")
  locations <- gx_parse_location(text)
  out <- gx_parse_datasets(text)

  expect_equal(nrow(locations), 1L)
  expect_contains(attr(locations, "diagnostics")$code, "missing_geometry")
  expect_equal(nrow(out), 5L)
  expect_identical(
    paste(out$dataset_name, basename(out$distribution_url), out$variable_name, sep = "|"),
    c(
      "Dataset A|a.csv|Variable A", "Dataset A|a.csv|Variable B",
      "Dataset A|a.json|Variable A", "Dataset A|a.json|Variable B",
      "Dataset B|b.csv|Variable C"
    )
  )
  expect_true(all(is.na(out$temporal_end[out$dataset_name == "Dataset A"])))
  expect_true(all(is.na(out$temporal_start[out$dataset_name == "Dataset B"])))
  expect_equal(length(unique(out$dataset_id[out$dataset_name == "Dataset A"])), 1L)
  expect_equal(length(unique(out$distribution_id[out$dataset_name == "Dataset A"])), 2L)
  expect_equal(length(unique(out$variable_id[out$dataset_name == "Dataset A"])), 2L)
})

test_that("profile parsers have stable typed zero-row outputs", {
  text <- gx_profile_fixture_text("observed", "reference-mainstem-29559.min.json")
  location <- gx_parse_location(text)
  datasets <- gx_parse_datasets(text)

  expect_s3_class(location, "gx_location")
  expect_equal(nrow(location), 0L)
  expect_type(location$rdf_types, "list")
  expect_type(location$diagnostics, "list")
  expect_contains(attr(location, "diagnostics")$code, "no_location_node")
  expect_s3_class(datasets, "gx_datasets")
  expect_equal(nrow(datasets), 0L)
  expect_s3_class(datasets$temporal_start, "POSIXct")
  expect_type(datasets$fetchable, "logical")
})

test_that("profile table contracts have exact columns, types, and safe printing", {
  text <- gx_profile_fixture_text("synthetic", "standard-compact.json")
  location <- gx_parse_location(text)
  datasets <- gx_parse_datasets(text)

  expect_identical(names(location), c(
    "contract_version", "site_uri", "name", "description", "site_type",
    "rdf_types", "provider_uri", "provider_name", "provider_url",
    "mainstem_uri", "geometry_wkt", "geometry_crs", "landing_url",
    "source_url", "diagnostics"
  ))
  expect_identical(names(datasets), c(
    "contract_version", "site_uri", "dataset_id", "distribution_id",
    "variable_id", "dataset_uri", "dataset_name", "dataset_description",
    "temporal_coverage", "temporal_start", "temporal_end", "variable_uri",
    "variable_name", "unit_uri", "unit_label", "measurement_technique",
    "distribution_url", "media_type", "conforms_to", "provider_uri",
    "provider_name", "provider_url", "license", "access_rights", "handler_id",
    "fetchable", "source_url", "diagnostics"
  ))
  expect_true(all(vapply(
    location[setdiff(names(location), c("rdf_types", "diagnostics"))],
    is.character,
    logical(1)
  )))
  expect_s3_class(datasets$temporal_start, "POSIXct")
  expect_s3_class(datasets$temporal_end, "POSIXct")
  expect_type(datasets$conforms_to, "list")
  expect_type(datasets$diagnostics, "list")
  expect_type(datasets$fetchable, "logical")

  secret <- "https://user:secret@example.net/data?token=value#fragment"
  location$source_url <- secret
  datasets$source_url <- secret
  location_print <- capture.output(print(location, width = Inf))
  dataset_print <- capture.output(print(datasets, width = Inf))
  expect_false(any(grepl("user|secret|value|fragment", location_print)))
  expect_false(any(grepl("user|secret|value|fragment", dataset_print)))
  expect_identical(location$source_url, secret)
  expect_identical(datasets$source_url, secret)
})

test_that("dataset identifiers preserve semantic IRI fragments", {
  base <- gx_profile_fixture_text("synthetic", "standard-compact.json")
  with_fragment <- sub(
    "https://example.net/dataset/discharge",
    "https://example.net/dataset/catalog#discharge",
    base,
    fixed = TRUE
  )
  without_fragment <- sub(
    "https://example.net/dataset/discharge",
    "https://example.net/dataset/catalog",
    base,
    fixed = TRUE
  )
  expect_false(identical(
    gx_parse_datasets(with_fragment)$dataset_id,
    gx_parse_datasets(without_fragment)$dataset_id
  ))
})

test_that("dataset products are rejected before exceeding the row budget", {
  text <- gx_profile_fixture_text("synthetic", "missing-geometry-open-temporal-multi.json")
  withr::local_options(geoconnexr.max_dataset_rows = 4L)

  expect_error(gx_parse_datasets(text), class = "gx_error_parser_budget")
})

test_that("temporal intervals preserve ISO offsets and fractional seconds", {
  parsed <- geoconnexr:::gx_parse_temporal(
    "2020-01-01T00:00:00-05:00/2020-01-02T05:06:07.125Z",
    "/temporal"
  )

  expect_equal(format(parsed$start, tz = "UTC", usetz = TRUE), "2020-01-01 05:00:00 UTC")
  expect_equal(as.numeric(parsed$end), 1577941567.125, tolerance = 0.001)
  expect_equal(nrow(parsed$diagnostics), 0L)

  invalid <- geoconnexr:::gx_parse_temporal("not-a-date/..", "/temporal")
  expect_true(is.na(invalid$start))
  expect_contains(invalid$diagnostics$code, "invalid_temporal_endpoint")
})

test_that("fixture manifest pins every profile, negative, and retrieval fixture", {
  root <- testthat::test_path("..", "fixtures", "jsonld")
  manifest <- jsonlite::fromJSON(file.path(root, "manifest-v1.json"), simplifyVector = FALSE)
  expected <- jsonlite::fromJSON(file.path(root, "expected-v1.json"), simplifyVector = FALSE)$cases
  entries <- manifest$fixtures
  paths <- vapply(entries, `[[`, character(1), "path")
  hashes <- vapply(entries, `[[`, character(1), "stored_sha256")
  expected_cases <- vapply(entries, `[[`, character(1), "expected_case")
  fixture_files <- sort(c(
    file.path("observed", list.files(file.path(root, "observed"), pattern = "[.]json$")),
    file.path("synthetic", list.files(file.path(root, "synthetic"), pattern = "[.]json$")),
    file.path("negative", list.files(file.path(root, "negative"), pattern = "[.]json$")),
    file.path("retrieval", list.files(file.path(root, "retrieval"), pattern = "[.]html$"))
  ))

  expect_setequal(paths, fixture_files)
  expect_true(all(file.exists(file.path(root, paths))))
  actual <- vapply(file.path(root, paths), digest::digest, character(1), algo = "sha256", serialize = FALSE, file = TRUE)
  expect_identical(unname(actual), unname(hashes))
  expect_true(all(expected_cases %in% names(expected)))

  profiles <- entries[vapply(entries, function(entry) {
    entry$evidence_kind %in% c("observed_minimized", "synthetic_conformance")
  }, logical(1))]
  for (entry in profiles) {
    text <- paste(readLines(file.path(root, entry$path), warn = FALSE), collapse = "\n")
    case <- expected[[entry$expected_case]]
    expect_equal(nrow(gx_parse_location(text)), case$location_rows, info = entry$fixture_id)
    expect_equal(nrow(gx_parse_datasets(text)), case$dataset_rows, info = entry$fixture_id)
  }
})

test_that("identity contract has portable golden hashes and Unicode normalization", {
  contract <- jsonlite::fromJSON(
    system.file("contracts", "identity-contract-v0.1.0.json", package = "geoconnexr"),
    simplifyVector = FALSE
  )
  for (case in contract$known_answers) {
    values <- lapply(case$values, function(value) {
      if (is.null(value)) NA_character_ else as.character(value)
    })
    expect_equal(
      gx_contract_hash(values, case$namespace, contract$contract_version),
      case$expected_sha256,
      info = case$id
    )
  }
  for (case in contract$canonicalization_known_answers$identity_iri) {
    expect_identical(
      geoconnexr:::gx_identity_iri(case$input),
      case$expected,
      info = case$input
    )
  }
  composed <- "café flow"
  decomposed <- paste0("cafe", "\u0301", " flow")
  expect_identical(
    geoconnexr:::gx_normalize_label(composed),
    geoconnexr:::gx_normalize_label(decomposed)
  )
})

test_that("opaque absolute IRIs remain stable semantic identifiers", {
  expect_identical(geoconnexr:::gx_identity_iri("URN:uuid:1234"), "urn:uuid:1234")
  expect_identical(geoconnexr:::gx_identity_iri("DOI:10.1/abc"), "doi:10.1/abc")
  expect_true(is.na(geoconnexr:::gx_identity_iri("urn:uuid:bad path")))
  expect_true(is.na(geoconnexr:::gx_identity_iri(paste0("urn:uuid:bad", "\u200b", "value"))))
})

test_that("contradictory and unsupported profile values remain diagnostic", {
  source <- jsonlite::fromJSON(
    testthat::test_path("..", "fixtures", "jsonld", "synthetic", "standard-compact.json"),
    simplifyVector = FALSE
  )
  source[["schema:name"]] <- list("Alpha", "Beta")
  source[["schema:provider"]] <- list(
    list(`@id` = "https://example.net/org/one"),
    list(`@id` = "https://example.net/org/two")
  )
  dataset <- source[["schema:subjectOf"]]
  dataset[["schema:temporalCoverage"]] <- list("2020-01-01/..", "../2024-01-01")
  dataset[["@type"]] <- list("schema:Dataset", "schema:Article")
  source[["schema:subjectOf"]] <- dataset

  location <- gx_parse_location(source)
  datasets <- gx_parse_datasets(source)
  expect_contains(attr(location, "diagnostics")$code, "contradictory_location_name")
  expect_contains(attr(location, "diagnostics")$code, "contradictory_provider")
  expect_contains(attr(datasets, "diagnostics")$code, "contradictory_temporal_coverage")
  expect_contains(attr(datasets, "diagnostics")$code, "contradictory_dataset_type")

  source[["schema:subjectOf"]][["@type"]] <- "schema:Article"
  expect_contains(
    attr(gx_parse_datasets(source), "diagnostics")$code,
    "unexpected_subject_type"
  )
})

test_that("missing datasets and non-mainstem positions are explicit", {
  source <- list(
    `@context` = list(hyf = "https://www.opengis.net/def/schema/hy_features/hyf/"),
    `@id` = "https://example.net/site/no-dataset",
    `@type` = "hyf:HY_HydroLocation",
    `hyf:referencedPosition` = list(
      `hyf:linearElement` = "https://geoconnex.us/ref/nhdplusv2/reachcode/123"
    )
  )
  location <- gx_parse_location(source)
  datasets <- gx_parse_datasets(source)

  expect_true(is.na(location$mainstem_uri))
  expect_contains(attr(location, "diagnostics")$code, "unsupported_linear_element")
  expect_contains(attr(datasets, "diagnostics")$code, "no_dataset_node")
  expect_error(gx_parse_datasets(source, strict = TRUE), class = "gx_error_parser_strict")

  source[["hyf:referencedPosition"]][["hyf:linearElement"]] <-
    "https://evil.example/ref/mainstems/123"
  evil <- gx_parse_location(source)
  expect_true(is.na(evil$mainstem_uri))
  expect_contains(attr(evil, "diagnostics")$code, "unsupported_linear_element")
})

test_that("split graph nodes merge once and repeated identities are bounded", {
  dataset_id <- "https://example.net/dataset/split"
  graph <- list(
    list(
      `@id` = "https://example.net/site/split",
      `@type` = list("https://www.opengis.net/def/schema/hy_features/hyf/HY_HydroLocation"),
      `https://schema.org/subjectOf` = list(list(`@id` = dataset_id))
    ),
    list(`@id` = dataset_id, `@type` = list("https://schema.org/Dataset"), `https://schema.org/name` = list(list(`@value` = "Split"))),
    list(`@id` = dataset_id, `https://schema.org/variableMeasured` = list(list(`@id` = "https://example.net/variable/split"))),
    list(`@id` = dataset_id, `https://schema.org/distribution` = list(list(`https://schema.org/contentUrl` = list(list(`@id` = "https://example.net/data/split.csv")))))
  )
  out <- gx_parse_datasets(list(`@graph` = graph))
  expect_equal(out$dataset_name, "Split")
  expect_equal(out$distribution_url, "https://example.net/data/split.csv")

  withr::local_options(geoconnexr.jsonld_max_id_fragments = 2L)
  attacked <- list(`@graph` = c(graph[1], rep(graph[2], 3L)))
  expect_error(gx_parse_datasets(attacked), class = "gx_error_parser_budget")

  references <- rep(list(list(`@id` = dataset_id)), 70L)
  references[[71L]] <- graph[[2]]
  expect_no_error(geoconnexr:::gx_node_index(references))
})

test_that("malformed semantic IRIs use diagnostics rather than unstable IDs", {
  source <- jsonlite::fromJSON(
    testthat::test_path("..", "fixtures", "jsonld", "synthetic", "standard-compact.json"),
    simplifyVector = FALSE
  )
  source[["schema:subjectOf"]][["@id"]] <- "https://example.net/bad path"
  source[["schema:subjectOf"]][["schema:variableMeasured"]][["@id"]] <- "http:"
  out <- gx_parse_datasets(source)

  expect_contains(attr(out, "diagnostics")$code, "invalid_dataset_uri")
  expect_contains(attr(out, "diagnostics")$code, "invalid_source_id")
  expect_match(out$dataset_id, "^[0-9a-f]{64}$")
})
