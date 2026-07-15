test_that("query-template assets match the independent pinned contract", {
  manifest <- gx_query_test_manifest()
  contract <- gx_query_test_contract()
  query_dir <- system.file("queries", package = "geoconnexr")

  expect_identical(contract$fixture_version, 2L)
  expect_identical(manifest$version, 2L)
  expect_identical(manifest$contract_version, contract$contract_version)
  expect_identical(manifest$runtime, contract$runtime)
  expect_identical(manifest$integrity, contract$integrity)
  expect_identical(
    names(manifest),
    c(
      "version", "contract_version", "runtime", "integrity",
      "endpoint_policy", "defaults", "templates"
    )
  )
  expect_identical(
    names(manifest$endpoint_policy),
    c(
      "method", "content_type", "accept", "follow_redirects",
      "reject_content_types"
    )
  )
  expect_identical(
    names(manifest$defaults),
    c("page_size", "max_page_size", "http_iri_list_max_items")
  )
  expect_identical(names(manifest$templates), names(contract$templates))

  expected_template_fields <- c(
    "file", "stored_bytes", "stored_sha256", "query_type", "parameters",
    "result_variables", "required_result_variables", "order", "result_key",
    "pagination", "row_budget"
  )
  expected_input_types <- c(
    sites_on_mainstem = "http_iri",
    sites_in_aoi = "crs84_wkt_literal",
    datasets_for_sites = "http_iri_list",
    datasets_by_variable = "http_iri",
    sites_by_provider = "http_iri",
    provider_coverage = "http_iri"
  )
  for (name in names(contract$templates)) {
    spec <- manifest$templates[[name]]
    pinned <- contract$templates[[name]]
    actual_sha256 <- digest::digest(
      file = file.path(query_dir, spec$file),
      algo = "sha256",
      serialize = FALSE
    )

    expect_identical(names(spec), expected_template_fields, info = name)
    expect_identical(spec$file, pinned$path, info = name)
    expect_identical(spec$stored_bytes, pinned$stored_bytes, info = name)
    expect_identical(spec$stored_sha256, pinned$stored_sha256, info = name)
    expect_identical(
      as.integer(file.info(file.path(query_dir, spec$file))$size),
      pinned$stored_bytes,
      info = name
    )
    expect_identical(actual_sha256, pinned$stored_sha256, info = name)
    expect_identical(
      names(spec$parameters),
      unlist(pinned$slots, use.names = FALSE),
      info = name
    )
    input <- spec$parameters[[1L]]
    expect_identical(input$type, unname(expected_input_types[[name]]), info = name)
    expect_identical(input$required, TRUE, info = name)
    if (identical(input$type, "http_iri")) {
      expect_identical(
        input,
        list(type = "http_iri", required = TRUE, maximum_bytes = 8192L),
        info = name
      )
    } else if (identical(input$type, "http_iri_list")) {
      expect_identical(
        input,
        list(
          type = "http_iri_list", required = TRUE,
          minimum_items = 1L, maximum_items = 200L,
          item_maximum_bytes = 8192L, encoded_maximum_bytes = 65536L,
          unique_items = TRUE, sort = "bytewise"
        ),
        info = name
      )
    } else {
      expect_identical(
        input,
        list(
          type = "crs84_wkt_literal", required = TRUE,
          maximum_bytes = 131072L,
          geometry_types = c("POLYGON", "MULTIPOLYGON"),
          allow_empty = FALSE
        ),
        info = name
      )
    }
    expected_limit_maximum <- if (identical(name, "provider_coverage")) 1L else 1000L
    expected_offset_maximum <- if (identical(name, "provider_coverage")) 0L else 9999L
    expect_identical(
      spec$parameters$limit,
      list(
        type = "integer", required = TRUE, minimum = 1L,
        maximum = expected_limit_maximum
      ),
      info = name
    )
    expect_identical(
      spec$parameters$offset,
      list(
        type = "integer", required = TRUE, minimum = 0L,
        maximum = expected_offset_maximum
      ),
      info = name
    )
    expect_identical(
      spec$result_variables,
      unlist(pinned$result_variables, use.names = FALSE),
      info = name
    )
    expect_identical(
      spec$required_result_variables,
      unlist(pinned$required_result_variables, use.names = FALSE),
      info = name
    )
    expect_identical(
      spec$order$variables,
      unlist(pinned$order$variables, use.names = FALSE),
      info = name
    )
    expect_identical(spec$order$direction, pinned$order$direction, info = name)
    expect_identical(
      spec$order$covers_result_variables,
      pinned$order$covers_result_variables,
      info = name
    )
    expect_identical(spec$order$total, pinned$order$total, info = name)
    expect_identical(
      spec$order$stable_across_requests,
      pinned$order$stable_across_requests,
      info = name
    )
    expect_identical(
      spec$result_key$variables,
      unlist(pinned$result_key$variables, use.names = FALSE),
      info = name
    )
    expect_identical(
      spec$result_key[names(spec$result_key) != "variables"],
      pinned$result_key[names(pinned$result_key) != "variables"],
      info = name
    )
    expect_identical(
      spec$pagination$blockers,
      unlist(pinned$pagination$blockers, use.names = FALSE),
      info = name
    )
    expect_identical(
      spec$pagination[names(spec$pagination) != "blockers"],
      pinned$pagination[names(pinned$pagination) != "blockers"],
      info = name
    )
    expect_identical(spec$row_budget, pinned$row_budget, info = name)
  }
})

test_that("manifest reader rejects ambiguous, unsafe, or unbounded YAML bytes", {
  bundle_dir <- gx_query_test_bundle_dir()
  manifest_path <- file.path(bundle_dir, "manifest.yml")
  original <- readBin(
    manifest_path,
    what = "raw",
    n = file.info(manifest_path)$size
  )
  original_text <- rawToChar(original)
  testthat::local_mocked_bindings(
    gx_query_asset_dir = function() bundle_dir,
    .package = "geoconnexr"
  )

  valid <- geoconnexr:::gx_read_query_manifest()
  expect_identical(valid$version, 2L)
  expect_identical(valid$runtime$render_enabled, TRUE)

  mutations <- list(
    missing_final_lf = original[-length(original)],
    utf8_bom = c(as.raw(c(0xef, 0xbb, 0xbf)), original),
    nul_byte = c(original, as.raw(c(0x00, 0x0a))),
    yaml_anchor = charToRaw(sub(
      "version: 2",
      "version: &version 2",
      original_text,
      fixed = TRUE
    )),
    duplicate_root_key = c(original, charToRaw("version: 2\n")),
    oversized_manifest = c(
      original,
      charToRaw("#"),
      rep(as.raw(0x61), 1048576L),
      as.raw(0x0a)
    )
  )
  for (name in names(mutations)) {
    writeBin(mutations[[name]], manifest_path)
    expect_error(
      geoconnexr:::gx_read_query_manifest(),
      class = "gx_error_query_manifest",
      info = name
    )
  }
  writeBin(original, manifest_path)
})

test_that("query-manifest JSON Schema rejects every structural mutation", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("jsonvalidate")

  manifest <- gx_query_test_manifest()
  validator <- jsonvalidate::json_validator(
    gx_query_test_schema_path(),
    engine = "ajv"
  )
  expect_true(validator(gx_query_test_manifest_json(manifest)))

  mutations <- gx_query_test_structural_mutations()
  for (name in names(mutations)) {
    mutated <- mutations[[name]](gx_query_test_clone(manifest))
    expect_false(
      validator(gx_query_test_manifest_json(mutated)),
      info = name
    )
  }
})

test_that("runtime validation mirrors the strict manifest structure", {
  manifest <- gx_query_test_manifest()
  asset_dir <- gx_query_test_asset_dir()

  expect_identical(
    geoconnexr:::gx_validate_query_manifest(manifest, asset_dir),
    manifest
  )

  mutations <- gx_query_test_structural_mutations()
  for (name in names(mutations)) {
    mutated <- mutations[[name]](gx_query_test_clone(manifest))
    expect_error(
      geoconnexr:::gx_validate_query_manifest(mutated, asset_dir),
      class = "gx_error_query_manifest",
      info = name
    )
  }
})

test_that("future literal and datetime parameter shapes remain typed and safe", {
  manifest <- gx_query_test_manifest()
  asset_dir <- gx_query_test_asset_dir()
  parameter_shapes <- list(
    literal = list(
      type = "literal", required = TRUE, maximum_bytes = 8192L
    ),
    datetime = list(type = "datetime", required = TRUE)
  )
  validator <- if (requireNamespace("jsonvalidate", quietly = TRUE)) {
    jsonvalidate::json_validator(gx_query_test_schema_path(), engine = "ajv")
  } else {
    NULL
  }

  for (type in names(parameter_shapes)) {
    mutated <- gx_query_test_clone(manifest)
    mutated$templates$sites_on_mainstem$parameters$mainstem_uri <-
      parameter_shapes[[type]]
    expect_no_error(
      geoconnexr:::gx_validate_query_manifest(mutated, asset_dir)
    )
    if (!is.null(validator)) {
      expect_true(
        validator(gx_query_test_manifest_json(mutated)),
        info = type
      )
    }
  }

  literal_spec <- parameter_shapes$literal
  literal <- "line 1\n\"quoted\" \\ } SERVICE {"
  expect_identical(
    geoconnexr:::gx_encode_parameter(literal, literal_spec),
    as.character(jsonlite::toJSON(
      literal,
      auto_unbox = TRUE,
      pretty = FALSE
    ))
  )
  expect_error(
    geoconnexr:::gx_encode_parameter(strrep("a", 8193L), literal_spec),
    class = "gx_error_query_parameter"
  )
  invalid_utf8 <- rawToChar(as.raw(0xff))
  Encoding(invalid_utf8) <- "UTF-8"
  expect_error(
    geoconnexr:::gx_encode_parameter(invalid_utf8, literal_spec),
    class = "gx_error_query_parameter"
  )

  datetime_spec <- parameter_shapes$datetime
  datetime <- "2026-07-14T12:34:56Z"
  expect_identical(
    geoconnexr:::gx_encode_parameter(datetime, datetime_spec),
    paste0(
      "\"", datetime,
      "\"^^<http://www.w3.org/2001/XMLSchema#dateTime>"
    )
  )
  expect_identical(
    geoconnexr:::gx_encode_parameter(
      as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      datetime_spec
    ),
    paste0(
      "\"", datetime,
      "\"^^<http://www.w3.org/2001/XMLSchema#dateTime>"
    )
  )
  bad_datetimes <- list(
    "2026-02-30T12:34:56Z",
    "2026-07-14T12:34:56-04:00",
    "2026-07-14T12:34:56Z\" SERVICE",
    as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") + 0.5
  )
  for (value in bad_datetimes) {
    expect_error(
      geoconnexr:::gx_encode_parameter(value, datetime_spec),
      class = "gx_error_query_parameter"
    )
  }
})

test_that("runtime validation enforces semantic paging and metadata contracts", {
  manifest <- gx_query_test_manifest()
  asset_dir <- gx_query_test_asset_dir()

  mutations <- list(
    defaults_page_exceeds_maximum = function(x) {
      x$defaults$page_size <- x$defaults$max_page_size + 1L
      x
    },
    duplicate_template_name = function(x) {
      x$templates <- c(x$templates, x$templates[1L])
      names(x$templates)[length(x$templates)] <- names(x$templates)[1L]
      x
    },
    declared_hash_mismatch = function(x) {
      x$templates$sites_on_mainstem$stored_sha256 <- paste(
        rep("0", 64L), collapse = ""
      )
      x
    },
    declared_byte_count_mismatch = function(x) {
      x$templates$sites_on_mainstem$stored_bytes <-
        x$templates$sites_on_mainstem$stored_bytes + 1L
      x
    },
    result_variable_not_projected = function(x) {
      x$templates$sites_on_mainstem$result_variables[[2L]] <- "rogue"
      x
    },
    declared_order_not_query_order = function(x) {
      order <- x$templates$sites_on_mainstem$order$variables
      x$templates$sites_on_mainstem$order$variables[1:2] <- order[2:1]
      x
    },
    result_key_not_projected = function(x) {
      x$templates$sites_on_mainstem$result_key$variables[[1L]] <- "rogue"
      x
    },
    result_key_not_ordered = function(x) {
      x$templates$provider_coverage$result_key$variables <- "site_count"
      x
    },
    required_result_not_projected = function(x) {
      x$templates$sites_on_mainstem$required_result_variables <- "rogue"
      x
    },
    covers_flag_mismatch = function(x) {
      x$templates$sites_on_mainstem$order$covers_result_variables <- FALSE
      x
    },
    unsafe_total_order_claim = function(x) {
      x$templates$sites_on_mainstem$order$total <- TRUE
      x
    },
    unsafe_order_stability_claim = function(x) {
      x$templates$sites_on_mainstem$order$stable_across_requests <- TRUE
      x
    },
    unsafe_key_stability_claim = function(x) {
      x$templates$sites_on_mainstem$result_key$stable_across_requests <- TRUE
      x
    },
    key_uniqueness_not_supported_by_query = function(x) {
      x$templates$sites_on_mainstem$result_key$uniqueness <- "group_by"
      x
    },
    paging_blocker_removed = function(x) {
      x$templates$sites_on_mainstem$pagination$blockers <-
        x$templates$sites_on_mainstem$pagination$blockers[-1L]
      x
    },
    aggregate_candidate_strategy_wrong = function(x) {
      x$templates$provider_coverage$pagination$candidate_strategy <- "offset"
      x
    },
    limit_missing = function(x) {
      x$templates$sites_on_mainstem$parameters$limit <- NULL
      x
    },
    limit_wrong_type = function(x) {
      x$templates$sites_on_mainstem$parameters$limit <- list(
        type = "uri", required = TRUE
      )
      x
    },
    limit_minimum_not_positive = function(x) {
      x$templates$sites_on_mainstem$parameters$limit$minimum <- 0L
      x
    },
    limit_exceeds_default_maximum = function(x) {
      x$templates$sites_on_mainstem$parameters$limit$maximum <- 1001L
      x
    },
    limit_minimum_exceeds_maximum = function(x) {
      x$templates$sites_on_mainstem$parameters$limit$minimum <- 1001L
      x
    },
    offset_missing = function(x) {
      x$templates$sites_on_mainstem$parameters$offset <- NULL
      x
    },
    offset_wrong_type = function(x) {
      x$templates$sites_on_mainstem$parameters$offset <- list(
        type = "uri", required = TRUE
      )
      x
    },
    offset_minimum_negative = function(x) {
      x$templates$sites_on_mainstem$parameters$offset$minimum <- -1L
      x
    },
    page_maximum_exceeds_row_budget = function(x) {
      x$templates$sites_on_mainstem$row_budget <- 999L
      x
    },
    uri_list_minimum_exceeds_maximum = function(x) {
      x$templates$datasets_for_sites$parameters$site_uris$minimum_items <- 201L
      x
    },
    uri_list_exceeds_chunk_default = function(x) {
      x$templates$datasets_for_sites$parameters$site_uris$maximum_items <- 201L
      x
    },
    duplicate_template_file = function(x) {
      x$templates$sites_in_aoi$file <- x$templates$sites_on_mainstem$file
      x$templates$sites_in_aoi$stored_bytes <-
        x$templates$sites_on_mainstem$stored_bytes
      x$templates$sites_in_aoi$stored_sha256 <-
        x$templates$sites_on_mainstem$stored_sha256
      x
    }
  )

  for (name in names(mutations)) {
    mutated <- mutations[[name]](gx_query_test_clone(manifest))
    expect_error(
      geoconnexr:::gx_validate_query_manifest(mutated, asset_dir),
      class = "gx_error_query_manifest",
      info = name
    )
  }
})

test_that("runtime validation binds hashes, slots, projection, and order to bytes", {
  source_manifest <- gx_query_test_manifest()
  template <- "sites_on_mainstem"

  mutations <- list(
    bytes_without_hash_update = list(
      rehash = FALSE,
      mutate = function(query) paste0(query, "\n# changed bytes")
    ),
    malformed_slot = list(
      rehash = TRUE,
      mutate = function(query) sub(
        "{{mainstem_uri}}", "{{ mainstem_uri }}", query, fixed = TRUE
      )
    ),
    unmatched_slot_delimiter = list(
      rehash = TRUE,
      mutate = function(query) sub(
        "{{mainstem_uri}}", "{{mainstem_uri}", query, fixed = TRUE
      )
    ),
    undeclared_slot = list(
      rehash = TRUE,
      mutate = function(query) sub(
        "WHERE {",
        "WHERE {\n  BIND({{rogue}} AS ?rogue)",
        query,
        fixed = TRUE
      )
    ),
    missing_declared_slot = list(
      rehash = TRUE,
      mutate = function(query) gsub(
        "{{mainstem_uri}}",
        "<https://example.org/mainstem>",
        query,
        fixed = TRUE
      )
    ),
    projection_changed = list(
      rehash = TRUE,
      mutate = function(query) sub(
        "SELECT DISTINCT ?site ?name ?provider ?site_wkt",
        "SELECT DISTINCT ?site ?provider ?site_wkt",
        query,
        fixed = TRUE
      )
    ),
    order_changed = list(
      rehash = TRUE,
      mutate = function(query) sub(
        "ORDER BY ?site ?name ?provider ?site_wkt",
        "ORDER BY ?name ?site ?provider ?site_wkt",
        query,
        fixed = TRUE
      )
    ),
    limit_slot_not_terminal = list(
      rehash = TRUE,
      mutate = function(query) {
        query <- sub(
          "WHERE {",
          "WHERE {\n  BIND({{limit}} AS ?limit_copy)",
          query,
          fixed = TRUE
        )
        sub("LIMIT {{limit}}", "LIMIT 1", query, fixed = TRUE)
      }
    ),
    query_form_changed = list(
      rehash = TRUE,
      mutate = function(query) sub(
        "SELECT DISTINCT", "CONSTRUCT", query, fixed = TRUE
      )
    )
  )

  for (name in names(mutations)) {
    asset_dir <- gx_query_test_asset_dir()
    manifest <- gx_query_test_clone(source_manifest)
    path <- file.path(asset_dir, manifest$templates[[template]]$file)
    query <- paste(
      readLines(path, warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
    mutated_query <- mutations[[name]]$mutate(query)
    expect_false(identical(mutated_query, query), info = name)
    writeLines(mutated_query, path, useBytes = TRUE)
    if (isTRUE(mutations[[name]]$rehash)) {
      manifest <- gx_query_test_rehash(manifest, asset_dir, template)
    }

    expect_error(
      geoconnexr:::gx_validate_query_manifest(manifest, asset_dir),
      class = "gx_error_query_manifest",
      info = name
    )
  }
})

test_that("runtime validation rejects unsafe stored bytes and unlisted queries", {
  source_manifest <- gx_query_test_manifest()
  template <- "sites_on_mainstem"

  binary_mutations <- list(
    utf8_bom = function(raw) c(as.raw(c(0xef, 0xbb, 0xbf)), raw),
    nul_byte = function(raw) c(raw, as.raw(0x00)),
    invalid_utf8 = function(raw) c(raw, as.raw(0xff))
  )
  for (name in names(binary_mutations)) {
    asset_dir <- gx_query_test_asset_dir()
    manifest <- gx_query_test_clone(source_manifest)
    path <- file.path(asset_dir, manifest$templates[[template]]$file)
    raw <- readBin(path, what = "raw", n = file.info(path)$size)
    writeBin(binary_mutations[[name]](raw), path)
    manifest <- gx_query_test_rehash(manifest, asset_dir, template)

    expect_error(
      geoconnexr:::gx_validate_query_manifest(manifest, asset_dir),
      class = "gx_error_query_manifest",
      info = name
    )
  }

  asset_dir <- gx_query_test_asset_dir()
  file.copy(
    file.path(asset_dir, source_manifest$templates[[template]]$file),
    file.path(asset_dir, "unlisted.rq")
  )
  expect_error(
    geoconnexr:::gx_validate_query_manifest(source_manifest, asset_dir),
    class = "gx_error_query_manifest"
  )
})

test_that("render-only discovery exposes v2 metadata without an execution API", {
  templates <- gx_templates()
  contract <- gx_query_test_contract()
  expected_hashes <- vapply(
    contract$templates,
    `[[`,
    character(1),
    "stored_sha256"
  )

  expect_identical(
    names(templates),
    c(
      "contract_version", "render_enabled", "execution_enabled",
      "pagination_enabled", "chunking_enabled", "gate", "name", "file",
      "stored_bytes", "stored_sha256", "query_type", "parameters",
      "result_variables", "required_result_variables", "order", "result_key",
      "pagination", "row_budget"
    )
  )
  expect_true(all(templates$contract_version == "0.2.0"))
  expect_true(all(templates$render_enabled))
  expect_false(any(templates$execution_enabled))
  expect_false(any(templates$pagination_enabled))
  expect_false(any(templates$chunking_enabled))
  expect_true(all(templates$gate == "ADR-0004"))
  expect_identical(
    stats::setNames(templates$stored_sha256, templates$name),
    expected_hashes
  )
  expect_true(all(vapply(
    templates$pagination,
    function(x) identical(x$enabled, FALSE),
    logical(1)
  )))

  planned_execution <- c("gx_sparql", "gx_query")
  exports <- getNamespaceExports("geoconnexr")
  namespace <- asNamespace("geoconnexr")
  expect_false(any(planned_execution %in% exports))
  expect_false(any(vapply(
    planned_execution,
    exists,
    logical(1),
    envir = namespace,
    inherits = FALSE
  )))

  graph_calls <- 0L
  network_calls <- 0L
  testthat::local_mocked_bindings(
    gx_graph_execute_once = function(...) {
      graph_calls <<- graph_calls + 1L
      stop("Unexpected graph execution.", call. = FALSE)
    },
    .package = "geoconnexr"
  )
  withr::local_options(list(
    geoconnexr.performer = function(...) {
      network_calls <<- network_calls + 1L
      stop("Unexpected network execution.", call. = FALSE)
    },
    geoconnexr.dns_resolver = function(...) {
      network_calls <<- network_calls + 1L
      stop("Unexpected DNS execution.", call. = FALSE)
    }
  ))

  params <- gx_query_test_valid_params()
  rendered <- Map(gx_render_query, names(params), params)
  expect_true(all(vapply(rendered, is.character, logical(1))))
  expect_true(all(lengths(rendered) == 1L))
  expect_true(all(endsWith(unlist(rendered), "\n")))
  expect_false(any(grepl("{{", unlist(rendered), fixed = TRUE)))
  expect_identical(graph_calls, 0L)
  expect_identical(network_calls, 0L)

  unsorted <- params$datasets_for_sites
  unsorted$site_uris <- rev(unsorted$site_uris)
  sorted_query <- gx_render_query("datasets_for_sites", unsorted)
  expect_match(
    sorted_query,
    paste0(
      "VALUES ?site { ",
      "<https://example.org/site/1> <https://example.org/site/2>",
      " }"
    ),
    fixed = TRUE
  )
  expect_identical(graph_calls, 0L)
  expect_identical(network_calls, 0L)
})

test_that("renderer rejects exact-parameter and typed injection mutations", {
  valid <- gx_query_test_valid_params()
  mainstem <- valid$sites_on_mainstem
  duplicate <- c(mainstem, list(mainstem_uri = mainstem$mainstem_uri))
  encoded_budget_uris <- vapply(1:9, function(index) {
    paste0(
      "https://example.org/",
      strrep(letters[[index]], 7900L),
      index
    )
  }, character(1))

  cases <- list(
    missing_parameter = list(
      template = "sites_on_mainstem",
      params = mainstem[names(mainstem) != "mainstem_uri"]
    ),
    extra_parameter = list(
      template = "sites_on_mainstem",
      params = c(mainstem, list(rogue = "injected"))
    ),
    duplicate_parameter = list(
      template = "sites_on_mainstem",
      params = duplicate
    ),
    unnamed_parameter = list(
      template = "sites_on_mainstem",
      params = unname(mainstem)
    ),
    uri_sparql_injection = list(
      template = "sites_on_mainstem",
      params = utils::modifyList(mainstem, list(
        mainstem_uri = "https://example.org/> } SERVICE <https://evil.invalid/"
      ))
    ),
    uri_control_character = list(
      template = "sites_on_mainstem",
      params = utils::modifyList(mainstem, list(
        mainstem_uri = "https://example.org/id\nSERVICE"
      ))
    ),
    uri_userinfo = list(
      template = "sites_on_mainstem",
      params = utils::modifyList(mainstem, list(
        mainstem_uri = "https://user:password@example.org/mainstem"
      ))
    ),
    uri_invalid_percent_escape = list(
      template = "sites_on_mainstem",
      params = utils::modifyList(mainstem, list(
        mainstem_uri = "https://example.org/%zz"
      ))
    ),
    uri_missing_host = list(
      template = "sites_on_mainstem",
      params = utils::modifyList(mainstem, list(
        mainstem_uri = "https:///mainstem"
      ))
    ),
    uri_exceeds_byte_budget = list(
      template = "sites_on_mainstem",
      params = utils::modifyList(mainstem, list(
        mainstem_uri = paste0("https://example.org/", strrep("a", 8200L))
      ))
    ),
    integer_logical = list(
      template = "sites_on_mainstem",
      params = utils::modifyList(mainstem, list(limit = TRUE))
    ),
    integer_fractional = list(
      template = "sites_on_mainstem",
      params = utils::modifyList(mainstem, list(limit = 1.5))
    ),
    integer_infinite = list(
      template = "sites_on_mainstem",
      params = utils::modifyList(mainstem, list(offset = Inf))
    ),
    integer_below_minimum = list(
      template = "sites_on_mainstem",
      params = utils::modifyList(mainstem, list(offset = -1L))
    ),
    page_exceeds_row_budget = list(
      template = "sites_on_mainstem",
      params = utils::modifyList(mainstem, list(limit = 1000L, offset = 9001L))
    ),
    uri_list_empty = list(
      template = "datasets_for_sites",
      params = utils::modifyList(
        valid$datasets_for_sites,
        list(site_uris = character())
      )
    ),
    uri_list_too_long = list(
      template = "datasets_for_sites",
      params = utils::modifyList(
        valid$datasets_for_sites,
        list(site_uris = sprintf("https://example.org/site/%03d", 1:201))
      )
    ),
    uri_list_member_injection = list(
      template = "datasets_for_sites",
      params = utils::modifyList(
        valid$datasets_for_sites,
        list(site_uris = c(
          "https://example.org/site/1",
          "https://example.org/> } UNION { ?s ?p ?o } #"
        ))
      )
    ),
    uri_list_duplicate = list(
      template = "datasets_for_sites",
      params = utils::modifyList(
        valid$datasets_for_sites,
        list(site_uris = rep("https://example.org/site/1", 2L))
      )
    ),
    uri_list_encoded_bytes_exceeded = list(
      template = "datasets_for_sites",
      params = utils::modifyList(
        valid$datasets_for_sites,
        list(site_uris = encoded_budget_uris)
      )
    ),
    wkt_literal_injection = list(
      template = "sites_in_aoi",
      params = utils::modifyList(
        valid$sites_in_aoi,
        list(aoi_wkt = "POINT(0 0)\"^^<urn:evil> . SERVICE")
      )
    ),
    wkt_unsupported_type = list(
      template = "sites_in_aoi",
      params = utils::modifyList(
        valid$sites_in_aoi,
        list(aoi_wkt = "CIRCULARSTRING(0 0,1 1,2 0)")
      )
    ),
    wkt_disallowed_point = list(
      template = "sites_in_aoi",
      params = utils::modifyList(
        valid$sites_in_aoi,
        list(aoi_wkt = "POINT(0 0)")
      )
    ),
    wkt_empty = list(
      template = "sites_in_aoi",
      params = utils::modifyList(
        valid$sites_in_aoi,
        list(aoi_wkt = "POLYGON EMPTY")
      )
    ),
    wkt_unclosed_ring = list(
      template = "sites_in_aoi",
      params = utils::modifyList(
        valid$sites_in_aoi,
        list(aoi_wkt = "POLYGON((0 0,1 0,1 1,0 1))")
      )
    ),
    wkt_non_finite = list(
      template = "sites_in_aoi",
      params = utils::modifyList(
        valid$sites_in_aoi,
        list(aoi_wkt = "POLYGON((0 0,1 0,1 Inf,0 0))")
      )
    )
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    expect_error(
      gx_render_query(case$template, case$params),
      class = "gx_error_query_parameter",
      info = name
    )
  }
})
