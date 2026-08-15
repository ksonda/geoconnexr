publisher_test_hash <- function(x, domain) {
  gx_contract_hash(x, domain, "0.1.0")
}

publisher_test_internal <- function(name) {
  get(name, envir = asNamespace("geoconnexr"), inherits = FALSE)
}

publisher_test_provider <- function() {
  list(
    uri = "https://example.org/provider/fixture",
    name = "Fixture Provider",
    url = "https://example.org/provider"
  )
}

publisher_conformance_root <- function() {
  system.file("conformance", "publisher-v1", package = "geoconnexr")
}

publisher_conformance_json <- function(name, simplify = FALSE) {
  jsonlite::fromJSON(
    file.path(publisher_conformance_root(), name),
    simplifyVector = simplify
  )
}

publisher_conformance_tables <- function(input) {
  site_rows <- input$sites
  sites <- sf::st_sf(
    tibble::as_tibble(do.call(rbind, lapply(site_rows, function(site) {
      as.data.frame(
        site[setdiff(names(site), "geometry")],
        stringsAsFactors = FALSE,
        optional = TRUE
      )
    }))),
    geometry = sf::st_sfc(lapply(site_rows, function(site) {
      sf::st_point(as.double(unlist(site$geometry$coordinates, use.names = FALSE)))
    }), crs = "OGC:CRS84")
  )
  dataset_rows <- list()
  for (dataset in input$datasets) {
    for (distribution in dataset$distributions) {
      for (variable in dataset$variables) {
        dataset_rows[[length(dataset_rows) + 1L]] <- c(
          dataset[setdiff(names(dataset), c("distributions", "variables"))],
          distribution,
          variable
        )
      }
    }
  }
  character_field <- function(name) {
    vapply(dataset_rows, function(row) {
      value <- row[[name]]
      if (is.null(value)) NA_character_ else as.character(value)
    }, character(1))
  }
  datasets <- tibble::tibble(
    contract_version = character_field("contract_version"),
    site_uri = character_field("site_uri"),
    dataset_id = character_field("dataset_id"),
    distribution_id = character_field("distribution_id"),
    variable_id = character_field("variable_id"),
    dataset_uri = character_field("dataset_uri"),
    dataset_name = character_field("dataset_name"),
    dataset_description = character_field("dataset_description"),
    temporal_coverage = character_field("temporal_coverage"),
    temporal_start = as.POSIXct(character_field("temporal_start"), tz = "UTC"),
    temporal_end = as.POSIXct(character_field("temporal_end"), tz = "UTC"),
    variable_uri = character_field("variable_uri"),
    variable_name = character_field("variable_name"),
    unit_uri = character_field("unit_uri"),
    unit_label = character_field("unit_label"),
    measurement_technique = character_field("measurement_technique"),
    distribution_url = character_field("distribution_url"),
    media_type = character_field("media_type"),
    conforms_to = unname(lapply(dataset_rows, function(row) {
      unlist(row$conforms_to, use.names = FALSE)
    })),
    provider_uri = character_field("provider_uri"),
    provider_name = character_field("provider_name"),
    provider_url = character_field("provider_url"),
    license = character_field("license"),
    access_rights = character_field("access_rights"),
    handler_id = character_field("handler_id"),
    fetchable = vapply(dataset_rows, function(row) isTRUE(row$fetchable), logical(1)),
    source_url = character_field("source_url")
  )
  list(sites = sites, datasets = datasets)
}

publisher_test_sites <- function() {
  sf::st_sf(
    tibble::tibble(
      contract_version = "0.1.0",
      site_uri = "https://example.org/site/00001",
      name = "Example & Test Site",
      description = "Publisher round-trip fixture",
      site_type = "hydrometricStation",
      provider_id = "fixture-provider",
      provider_uri = "https://example.org/provider/fixture",
      provider_name = "Fixture Provider",
      provider_url = "https://example.org/provider",
      mainstem_uri = "https://geoconnex.us/ref/mainstems/29559",
      landing_url = "https://example.org/site/00001",
      source_url = "https://example.org/site/00001.jsonld"
    ),
    geometry = sf::st_sfc(sf::st_point(c(-78.5, 37.25)), crs = "OGC:CRS84")
  )
}

publisher_test_datasets <- function(sites = publisher_test_sites()) {
  dataset_uri <- "https://example.org/dataset/flow"
  dataset_id <- publisher_test_hash(
    list("uri", sites$site_uri[[1L]], dataset_uri),
    "geoconnexr.dataset-id.v1"
  )
  distributions <- tibble::tibble(
    distribution_url = c(
      "https://example.org/data/flow.csv",
      "https://example.org/data/flow.json"
    ),
    media_type = c("text/csv", "application/json")
  )
  distributions$distribution_id <- vapply(seq_len(nrow(distributions)), function(i) {
    publisher_test_hash(
      list(
        dataset_id,
        distributions$distribution_url[[i]],
        distributions$media_type[[i]]
      ),
      "geoconnexr.distribution-id.v1"
    )
  }, character(1))
  variables <- tibble::tibble(
    variable_id = c(
      "https://example.org/variable/flow",
      "https://example.org/variable/temperature"
    ),
    variable_uri = c(
      "https://example.org/variable/flow",
      "https://example.org/variable/temperature"
    ),
    variable_name = c("Flow", "Temperature"),
    unit_uri = c(
      "https://qudt.org/vocab/unit/M3-PER-SEC",
      "https://qudt.org/vocab/unit/DEG_C"
    ),
    unit_label = c("m3/s", "deg C")
  )
  grid <- expand.grid(
    distribution = seq_len(nrow(distributions)),
    variable = seq_len(nrow(variables)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  d <- grid$distribution
  v <- grid$variable
  n <- nrow(grid)
  tibble::tibble(
    contract_version = rep("0.1.0", n),
    site_uri = rep(sites$site_uri[[1L]], n),
    dataset_id = rep(dataset_id, n),
    distribution_id = distributions$distribution_id[d],
    variable_id = variables$variable_id[v],
    dataset_uri = rep(dataset_uri, n),
    dataset_name = rep("Flow observations", n),
    dataset_description = rep("Two distributions and two variables", n),
    temporal_coverage = rep("2024-01-01T00:00:00Z/2024-12-31T00:00:00Z", n),
    temporal_start = as.POSIXct(rep("2024-01-01 00:00:00", n), tz = "UTC"),
    temporal_end = as.POSIXct(rep("2024-12-31 00:00:00", n), tz = "UTC"),
    variable_uri = variables$variable_uri[v],
    variable_name = variables$variable_name[v],
    unit_uri = variables$unit_uri[v],
    unit_label = variables$unit_label[v],
    measurement_technique = rep(NA_character_, n),
    distribution_url = distributions$distribution_url[d],
    media_type = distributions$media_type[d],
    conforms_to = rep(list(c(
      "https://example.org/spec/a", "https://example.org/spec/b"
    )), n),
    provider_uri = rep("https://example.org/provider/fixture", n),
    provider_name = rep("Fixture Provider", n),
    provider_url = rep("https://example.org/provider", n),
    license = rep("https://creativecommons.org/publicdomain/zero/1.0/", n),
    access_rights = rep("public", n),
    handler_id = c("csv", "unknown", "csv", "unknown")[d + 2L * (v - 1L)],
    fetchable = distributions$media_type[d] == "text/csv",
    source_url = rep("https://example.org/site/00001.jsonld", n)
  )
}

test_that("gx_context returns a fresh fixed local context", {
  first <- gx_context()
  second <- gx_context()

  expect_identical(names(first), c("schema", "hyf", "geo", "gx"))
  expect_identical(first, second)
  first$schema <- "changed"
  expect_identical(gx_context()$schema, "https://schema.org/")
  profile <- publisher_test_internal("gx_publisher_profile_impl")()
  expect_identical(profile$profile_version, "1.0.0")
  expect_identical(
    profile$validation_finding_columns,
    c(
      "severity", "json_pointer", "rule_id", "profile_version", "message",
      "suggested_fix"
    )
  )
})

test_that("installed publisher conformance resources are closed and pinned", {
  root <- publisher_conformance_root()
  manifest <- publisher_conformance_json("manifest.json")
  expected_paths <- vapply(manifest$resources, `[[`, character(1), "path")
  actual_paths <- setdiff(
    list.files(root, all.files = FALSE, no.. = TRUE),
    c("README.md", "manifest.json")
  )

  expect_true(nzchar(root))
  expect_identical(sort(expected_paths), sort(actual_paths))
  expect_identical(manifest$profile_version, "1.0.0")
  expect_identical(
    manifest$contract_version,
    "geoconnexr.publisher-conformance/1.0.0"
  )
  for (resource in manifest$resources) {
    path <- file.path(root, resource$path)
    expect_identical(as.integer(file.info(path)$size), resource$bytes,
                     info = resource$path)
    expect_identical(
      digest::digest(file = path, algo = "sha256", serialize = FALSE),
      resource$sha256,
      info = resource$path
    )
  }
})

test_that("R reproduces shared publisher profile and finding known answers", {
  input <- publisher_conformance_json("input.json")
  tables <- publisher_conformance_tables(input)
  expected <- publisher_conformance_json("expected-profile.json")
  manifest <- publisher_conformance_json("manifest.json")

  built <- gx_jsonld_build(tables$sites, tables$datasets, input$provider)
  expect_identical(built, expected)
  expect_identical(
    digest::digest(
      charToRaw(publisher_test_internal("gx_json_serialize")(built)),
      algo = "sha256",
      serialize = FALSE
    ),
    manifest$known_answers$profile_canonical_sha256
  )

  invalid <- publisher_conformance_json("invalid-profile.json")
  expected_findings <- publisher_conformance_json(
    "expected-findings.json", simplify = TRUE
  )
  findings <- gx_jsonld_validate(invalid)
  expect_false(attr(findings, "valid"))
  expect_identical(nrow(findings), manifest$known_answers$invalid_finding_count)
  attr(findings, "valid") <- NULL
  class(findings) <- "data.frame"
  expect_identical(findings, expected_findings)
})

test_that("R reproduces shared sitemap bytes and digest", {
  root <- publisher_conformance_root()
  uris <- unlist(publisher_conformance_json("sitemap-uris.json"), use.names = FALSE)
  expected <- readBin(
    file.path(root, "expected-sitemap.xml"),
    what = "raw",
    n = file.info(file.path(root, "expected-sitemap.xml"))$size
  )
  manifest <- publisher_conformance_json("manifest.json")
  destination <- file.path(withr::local_tempdir(), "sitemap")

  out <- gx_sitemap(uris, destination)

  actual <- readBin(out$file, what = "raw", n = out$bytes)
  expect_identical(actual, expected)
  expect_identical(out$sha256, manifest$known_answers$sitemap_sha256)
})

test_that("publisher profiles build deterministically and round trip", {
  sites <- publisher_test_sites()
  datasets <- publisher_test_datasets(sites)
  provider <- publisher_test_provider()

  built <- gx_jsonld_build(sites, datasets, provider)
  shuffled <- gx_jsonld_build(sites, datasets[c(4L, 2L, 3L, 1L), ], provider)

  expect_identical(
    publisher_test_internal("gx_json_serialize")(built),
    publisher_test_internal("gx_json_serialize")(shuffled)
  )
  expect_identical(built[["gx:profileVersion"]], "1.0.0")
  expect_identical(
    built[["gx:profile"]],
    publisher_test_internal(".gx_publisher_profile_iri")
  )
  expect_length(built[["@graph"]], 1L)

  locations <- gx_parse_location(built)
  parsed <- gx_parse_datasets(built)
  expect_identical(locations$site_uri, sites$site_uri)
  expect_identical(locations$name, sites$name)
  expect_identical(locations$site_type, sites$site_type)
  expect_identical(locations$provider_uri, sites$provider_uri)
  expect_identical(locations$mainstem_uri, sites$mainstem_uri)
  expect_match(locations$geometry_wkt, "^POINT")
  expect_equal(nrow(parsed), 4L)
  expect_setequal(parsed$dataset_id, datasets$dataset_id)
  expect_setequal(parsed$distribution_id, datasets$distribution_id)
  expect_setequal(parsed$variable_id, datasets$variable_id)
  expect_setequal(parsed$distribution_url, datasets$distribution_url)
  expect_setequal(parsed$unit_uri, datasets$unit_uri)

  findings <- gx_jsonld_validate(built)
  expect_s3_class(findings, "gx_jsonld_validation")
  expect_true(attr(findings, "valid"))
  expect_equal(nrow(findings), 0L)

  expanded <- publisher_test_internal("gx_prepare_jsonld")(built)$expanded
  expect_true(attr(gx_jsonld_validate(expanded), "valid"))
})

test_that("publisher round-trip properties hold across supported type variants", {
  location_types <- c(
    "Unknown",
    "hydrometricStation",
    paste0(publisher_test_internal(".gx_loctype"), "hydrometricStation")
  )
  orders <- list(1:4, 4:1, c(2L, 4L, 1L, 3L))
  for (location_type in location_types) {
    for (order in orders) {
      sites <- publisher_test_sites()
      sites$site_type <- location_type
      datasets <- publisher_test_datasets(sites)[order, ]
      built <- gx_jsonld_build(sites, datasets, publisher_test_provider())
      location <- gx_parse_location(built)
      parsed <- gx_parse_datasets(built)

      expect_identical(location$site_type, location_type)
      expect_setequal(parsed$distribution_id, datasets$distribution_id)
      expect_setequal(parsed$variable_id, datasets$variable_id)
      expect_true(attr(gx_jsonld_validate(built), "valid"))
    }
  }
})

test_that("publisher builder admits no-dataset sites and location-type IRIs", {
  sites <- publisher_test_sites()
  sites$site_type <- paste0(
    publisher_test_internal(".gx_loctype"),
    "hydrometricStation"
  )

  built <- gx_jsonld_build(sites, provider = publisher_test_provider())
  parsed <- gx_parse_location(built)

  expect_identical(parsed$site_type, sites$site_type)
  expect_false("schema:subjectOf" %in% names(built[["@graph"]][[1L]]))
  expect_true(attr(gx_jsonld_validate(built), "valid"))
})

test_that("publisher builder rejects mismatched and lossy inputs", {
  sites <- publisher_test_sites()
  datasets <- publisher_test_datasets(sites)
  provider <- publisher_test_provider()

  wrong_provider <- provider
  wrong_provider$name <- "Another Provider"
  expect_error(
    gx_jsonld_build(sites, datasets, wrong_provider),
    class = "gx_error_publisher_provider"
  )

  bad_type <- sites
  bad_type$site_type <- "stream gauge"
  expect_error(
    gx_jsonld_build(bad_type, datasets, provider),
    class = "gx_error_publisher_location_type"
  )

  sparse <- datasets[-1L, ]
  expect_error(
    gx_jsonld_build(sites, sparse, provider),
    class = "gx_error_publisher_cardinality"
  )

  context <- gx_context()
  context$schema <- "https://example.org/schema/"
  expect_error(
    gx_jsonld_build(sites, datasets, provider, context),
    class = "gx_error_publisher_input"
  )
})

test_that("publisher validation returns stable actionable findings", {
  built <- gx_jsonld_build(
    publisher_test_sites(),
    publisher_test_datasets(),
    publisher_test_provider()
  )
  built[["gx:profileVersion"]] <- "2.0.0"
  built[["@graph"]][[1L]][["hyf:HY_HydroLocationType"]] <- "stream gauge"

  findings <- gx_jsonld_validate(built)

  expect_false(attr(findings, "valid"))
  expect_identical(names(findings), c(
    "severity", "json_pointer", "rule_id", "profile_version", "message",
    "suggested_fix"
  ))
  expect_contains(findings$rule_id, "profile.version")
  expect_contains(findings$rule_id, "site.location_type")
  expect_true(all(nzchar(findings$suggested_fix)))

  malformed <- gx_jsonld_validate(charToRaw("{"))
  expect_false(attr(malformed, "valid"))
  expect_identical(malformed$rule_id, "document.parse")
})

test_that("gx_sitemap publishes deterministic escaped XML once", {
  parent <- withr::local_tempdir()
  first_dir <- file.path(parent, "first")
  second_dir <- file.path(parent, "second")
  uris <- c(
    "https://example.org/z?x=1&y=2",
    "https://example.org/a"
  )

  first <- gx_sitemap(uris, first_dir)
  second <- gx_sitemap(rev(uris), second_dir)

  expect_s3_class(first, "gx_sitemap")
  expect_identical(first$contract_version, "1.0.0")
  expect_identical(first$url_count, 2L)
  expect_identical(first$sha256, second$sha256)
  expect_identical(readBin(first$file, "raw", n = first$bytes),
                   readBin(second$file, "raw", n = second$bytes))
  document <- xml2::read_xml(first$file)
  locations <- xml2::xml_text(xml2::xml_find_all(
    document, ".//*[local-name()='loc']"
  ))
  expect_identical(locations, sort(uris))
  expect_match(
    paste(readLines(first$file, warn = FALSE), collapse = "\n"),
    "&amp;",
    fixed = TRUE
  )

  expect_error(gx_sitemap(uris, first_dir), class = "gx_error_publisher_path")
  expect_error(
    gx_sitemap(c(uris, uris[[1L]]), file.path(parent, "duplicate")),
    class = "gx_error_publisher_input"
  )
  expect_error(
    gx_sitemap("ftp://example.org/a", file.path(parent, "scheme")),
    class = "gx_error_publisher_input"
  )
  expect_error(
    gx_sitemap(
      paste0("https://example.org/", strrep("a", 2048L)),
      file.path(parent, "long")
    ),
    class = "gx_error_publisher_input"
  )
})
