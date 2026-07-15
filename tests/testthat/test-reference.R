gx_ref_test_clock <- function() {
  as.POSIXct("2026-07-13 20:00:00", tz = "UTC")
}

gx_ref_test_dns <- function(host) {
  rep("93.184.216.34", length(host))
}

gx_ref_test_fixture <- function(name) {
  path <- testthat::test_path("..", "fixtures", "reference", name)
  readBin(path, what = "raw", n = file.info(path)$size)
}

gx_ref_test_json <- function(value) {
  charToRaw(as.character(jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", na = "null", digits = NA
  )))
}

gx_ref_test_response <- function(request, status = 200L, body = raw(),
                                 content_type = "application/json", headers = list()) {
  headers[["Content-Type"]] <- content_type
  list(
    status = as.integer(status), headers = headers,
    body = if (is.raw(body)) body else charToRaw(body), url = request$url
  )
}

gx_ref_test_router <- function(handler) {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  performer <- function(request) {
    state$calls[[length(state$calls) + 1L]] <- request
    handler(request, state)
  }
  list(performer = performer, state = state)
}

gx_ref_test_client <- function(handler, retries = 0L, max_bytes = 1024L * 1024L) {
  routed <- gx_ref_test_router(handler)
  withr::local_options(
    list(
      geoconnexr.performer = routed$performer,
      geoconnexr.dns_resolver = gx_ref_test_dns,
      geoconnexr.clock = gx_ref_test_clock,
      geoconnexr.cache_dir = withr::local_tempdir(),
      geoconnexr.offline = FALSE
    ),
    .local_envir = parent.frame()
  )
  list(
    client = gx_client(
      "reference", retries = retries, max_bytes = max_bytes,
      cache = FALSE
    ),
    state = routed$state
  )
}

gx_ref_test_queryables <- function(collection) {
  name <- switch(
    collection,
    gages = "queryables-gages.min.json",
    mainstems = "queryables-mainstems.min.json",
    mainstems_v3 = "queryables-mainstems-v3.min.json",
    hu12 = "queryables-hu12.min.json",
    counties = "queryables-counties.min.json"
  )
  gx_ref_test_fixture(name)
}

gx_ref_test_feature <- function(id, properties = list(), geometry = NULL) {
  if (is.null(geometry)) {
    geometry <- list(type = "Point", coordinates = c(-107, 35))
  }
  list(
    type = "Feature", id = id, properties = properties,
    geometry = geometry
  )
}

gx_ref_test_collection <- function(features, matched = length(features),
                                   returned = length(features), links = list()) {
  list(
    type = "FeatureCollection", features = features, links = links,
    numberMatched = matched, numberReturned = returned,
    timeStamp = "2026-07-13T20:00:00Z"
  )
}

test_that("reference fixture manifest pins every minimized schema", {
  root <- testthat::test_path("..", "fixtures", "reference")
  manifest <- jsonlite::fromJSON(
    file.path(root, "manifest-v1.json"), simplifyVector = FALSE
  )
  entries <- manifest$fixtures
  paths <- vapply(entries, `[[`, character(1), "path")
  hashes <- vapply(entries, `[[`, character(1), "stored_sha256")
  fixture_files <- setdiff(
    list.files(root, pattern = "[.]json$"),
    "manifest-v1.json"
  )

  expect_identical(manifest$contract_version, "0.1.0")
  expect_match(manifest$checked_at, "^2026-07-13T")
  expect_setequal(paths, fixture_files)
  expect_true(all(file.exists(file.path(root, paths))))
  actual <- vapply(
    file.path(root, paths), digest::digest, character(1),
    algo = "sha256", serialize = FALSE, file = TRUE
  )
  expect_identical(unname(actual), unname(hashes))
  expect_true(all(startsWith(
    vapply(entries, `[[`, character(1), "source_url"),
    "https://reference.geoconnex.us/"
  )))
  expect_setequal(
    sub("^queryables-", "", sub("[.]min[.]json$", "", paths[grepl("^queryables-", paths)])),
    c("gages", "mainstems", "mainstems-v3", "hu12", "counties")
  )
})

test_that("reference collections and queryables preserve their contracts", {
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections")) {
      return(gx_ref_test_response(request, body = gx_ref_test_fixture("collections.min.json")))
    }
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_queryables("gages"),
        content_type = "application/schema+json"
      ))
    }
    stop("Unexpected reference request: ", request$url, call. = FALSE)
  })

  collections <- gx_ref_collections(client = setup$client)
  queryables <- gx_ref_queryables("gages", client = setup$client)

  expect_s3_class(collections, "gx_ref_collections")
  expect_identical(names(collections), c(
    "contract_version", "collection_id", "title", "description",
    "item_type", "crs", "extent", "links", "raw"
  ))
  expect_contains(collections$collection_id, c(
    "gages", "mainstems", "mainstems_v3", "hu12", "counties"
  ))
  expect_true(all(vapply(collections[c("crs", "extent", "links", "raw")], is.list, logical(1))))
  expect_s3_class(queryables, "gx_ref_queryables")
  expect_identical(names(queryables), c(
    "contract_version", "collection_id", "name", "json_types", "format",
    "title", "description", "enum", "schema"
  ))
  expect_equal(queryables$json_types[[match("id", queryables$name)]], "integer")
  expect_equal(
    queryables$schema[[match("id", queryables$name)]][["x-ogc-role"]],
    "id"
  )
  expect_identical(names(attr(queryables, "gx_reference")$requests), c(
    "request_id", "method", "url", "status", "media_type", "bytes",
    "body_sha256", "retrieved_at", "cache_origin"
  ))
  returned <- NULL
  invisible(capture.output(returned <- print(collections)))
  expect_s3_class(returned, "gx_ref_collections")
  expect_error(
    geoconnexr:::gx_ref_validate_collections(
      collections[setdiff(names(collections), "title")],
      attr(collections, "gx_reference")
    ),
    class = "gx_error_reference_contract"
  )
  expect_error(
    geoconnexr:::gx_ref_validate_queryables(
      queryables[setdiff(names(queryables), "schema")],
      attr(queryables, "gx_reference")
    ),
    class = "gx_error_reference_contract"
  )
  object_identity <- gx_ref_test_feature(
    "1000001",
    list(
      id = list(evil = "1000001"), provider_id = "P",
      nhdpv2_comid = 17789327L
    )
  )
  expect_false(geoconnexr:::gx_ref_feature_matches_id(
    object_identity, "1000001", "id"
  ))
  expect_error(
    geoconnexr:::gx_ref_features_sf(list(object_identity), queryables),
    class = "gx_error_reference_payload"
  )
})

test_that("every M3 acceptance collection exercises its advertised identity role", {
  expected <- c(
    gages = "id", mainstems = "id", mainstems_v3 = "id",
    hu12 = "huc12", counties = "geoid"
  )
  setup <- gx_ref_test_client(function(request, state) {
    collection <- sub(
      "^.*/collections/([^/]+)/queryables$", "\\1", request$url
    )
    if (!collection %in% names(expected)) {
      stop("Unexpected request: ", request$url, call. = FALSE)
    }
    gx_ref_test_response(
      request,
      body = gx_ref_test_queryables(collection),
      content_type = "application/schema+json"
    )
  })

  schemas <- lapply(names(expected), function(collection) {
    gx_ref_queryables(collection, client = setup$client)
  })
  names(schemas) <- names(expected)

  for (collection in names(expected)) {
    roles <- vapply(schemas[[collection]]$schema, function(schema) {
      as.character(schema[["x-ogc-role"]] %||% NA_character_)
    }, character(1))
    expect_identical(
      schemas[[collection]]$name[!is.na(roles) & roles == "id"],
      unname(expected[[collection]]),
      info = collection
    )
  }
  v3_id <- match("id", schemas$mainstems_v3$name)
  expect_identical(schemas$mainstems_v3$json_types[[v3_id]], "string")
})

test_that("empty collection and queryable inventories retain zero-row contracts", {
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections")) {
      return(gx_ref_test_response(
        request,
        body = charToRaw('{"collections":[],"links":[]}')
      ))
    }
    if (endsWith(request$url, "/collections/empty/queryables")) {
      return(gx_ref_test_response(
        request,
        body = charToRaw('{"type":"object","properties":{}}')
      ))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  collections <- gx_ref_collections(client = setup$client)
  queryables <- gx_ref_queryables("empty", client = setup$client)

  expect_equal(nrow(collections), 0L)
  expect_equal(nrow(queryables), 0L)
  expect_type(collections$collection_id, "character")
  expect_type(queryables$name, "character")
  expect_true(all(vapply(collections[c("crs", "extent", "links", "raw")], is.list, logical(1))))
  expect_true(all(vapply(queryables[c("json_types", "enum", "schema")], is.list, logical(1))))
  returned <- NULL
  invisible(capture.output(returned <- print(queryables)))
  expect_s3_class(returned, "gx_ref_queryables")
})

test_that("malformed collection inventories fail with completed-response provenance", {
  setup <- gx_ref_test_client(function(request, state) {
    gx_ref_test_response(request, body = charToRaw('{"error":"temporary"}'))
  })

  error <- tryCatch(
    gx_ref_collections(client = setup$client),
    error = identity
  )

  expect_s3_class(error, "gx_error_reference_payload")
  expect_equal(nrow(error$requests), 1L)
  expect_identical(error$requests$status, 200L)
})

test_that("queryables cannot collide with package-owned output columns", {
  schema <- list(
    type = "object",
    properties = list(feature_id = list(type = "string"))
  )
  setup <- gx_ref_test_client(function(request, state) {
    gx_ref_test_response(request, body = gx_ref_test_json(schema))
  })

  error <- tryCatch(
    gx_ref_queryables("unsafe", client = setup$client),
    error = identity
  )

  expect_s3_class(error, "gx_error_reference_payload")
  expect_equal(nrow(error$requests), 1L)
})

test_that("filtered features force classic JSON and normalize identifiers", {
  feature <- gx_ref_test_feature(
    1000001L,
    list(
      id = 1000001L, provider_id = "USGS-08332622",
      nhdpv2_comid = 17789327, name = "RIO PUERCO"
    )
  )
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    if (grepl("/collections/gages/items[?]", request$url)) {
      expect_match(request$url, "[?]f=json&provider_id=USGS-08332622&limit=10$")
      body <- gx_ref_test_collection(
        list(feature), matched = 1L, returned = 1L,
        links = list(list(rel = "next", href = paste0(request$url, "&offset=1")))
      )
      return(gx_ref_test_response(request, body = gx_ref_test_json(body)))
    }
    stop("Unexpected reference request: ", request$url, call. = FALSE)
  })

  out <- gx_ref_features(
    "gages", query = list(provider_id = "USGS-08332622"),
    limit = 10L, client = setup$client
  )
  metadata <- attr(out, "gx_reference")

  expect_s3_class(out, "gx_ref_features")
  expect_s3_class(out, "sf")
  expect_identical(out$feature_id, "1000001")
  expect_identical(out$id, "1000001")
  expect_identical(out$nhdpv2_comid, "17789327")
  expect_type(out$provider_id, "character")
  expect_false(metadata$truncated)
  expect_equal(metadata$stop_reason, "number_matched")
  expect_equal(metadata$pages, 1L)
  expect_true(all(vapply(setup$state$calls, function(x) {
    identical(x$headers[["accept-encoding"]], "identity") && length(x$resolved_ip) == 1L
  }, logical(1))))
  expect_error(
    geoconnexr:::gx_ref_validate_features(
      sf::st_drop_geometry(out), metadata
    ),
    class = "gx_error_reference_contract"
  )
})

test_that("GeoJSON conversion preserves identity-property alignment when GDAL reorders FIDs", {
  queryables <- NULL
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    features <- list(
      gx_ref_test_feature(
        12L,
        list(id = 12L, provider_id = "P", nhdpv2_comid = 12L)
      ),
      gx_ref_test_feature(
        11L,
        list(id = 11L, provider_id = "P", nhdpv2_comid = 11L)
      )
    )
    gx_ref_test_response(
      request,
      body = gx_ref_test_json(gx_ref_test_collection(features, 2L, 2L))
    )
  })

  out <- gx_ref_features(
    "gages", query = list(provider_id = "P"), limit = 2L,
    client = setup$client
  )

  expect_identical(out$feature_id, c("12", "11"))
  expect_identical(out$id, c("12", "11"))
  expect_identical(out$nhdpv2_comid, c("12", "11"))
})

test_that("large numeric identifiers preserve their exact lexical value", {
  id <- "9007199254740993"
  body <- charToRaw(paste0(
    '{"type":"FeatureCollection","features":[',
    '{"type":"Feature","id":', id, ',"properties":{"id":', id,
    ',"provider_id":"P","nhdpv2_comid":17789327},',
    '"geometry":{"type":"Point","coordinates":[-107,35]}}],',
    '"numberMatched":1,"numberReturned":1,"links":[]}'
  ))
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    if (grepl("/collections/gages/items[?]", request$url)) {
      return(gx_ref_test_response(request, body = body))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  out <- gx_ref_features(
    "gages", query = list(id = id), limit = 1L, client = setup$client
  )

  expect_identical(out$feature_id, id)
  expect_identical(out$id, id)
})

test_that("numeric equality filters serialize round-trip-safe doubles", {
  value <- 0.12345678901234566
  encoded <- geoconnexr:::gx_ref_query_value(
    value, list(type = "number"), "measure"
  )

  expect_identical(as.numeric(encoded), value)
})

test_that("controlled OGC parameters cannot masquerade as property filters", {
  setup <- gx_ref_test_client(function(request, state) {
    stop("Controlled parameters must fail before transport.", call. = FALSE)
  })

  expect_error(
    gx_ref_features(
      "gages", query = list(filter = "id=1"), client = setup$client
    ),
    class = "gx_error_reference_query"
  )
  expect_error(
    gx_ref_features(
      "gages", query = list(skipGeometry = TRUE), client = setup$client
    ),
    class = "gx_error_reference_query"
  )
  expect_length(setup$state$calls, 0L)
})

test_that("query validation blocks ignored or unsafe filter shapes", {
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    stop("Items request should not be reached.", call. = FALSE)
  })

  error <- expect_error(
    gx_ref_features("gages", query = list(not_advertised = "x"), client = setup$client),
    class = "gx_error_reference_query"
  )
  expect_identical(nrow(error$requests), 1L)
  expect_identical(error$requests$status, 200L)
  expect_error(
    gx_ref_features("gages", query = list(geometry = "POINT (0 0)"), client = setup$client),
    class = "gx_error_reference_query"
  )
  expect_error(
    gx_ref_features("gages", query = list(id = "not-integer"), client = setup$client),
    class = "gx_error_reference_query"
  )
  expect_error(
    gx_ref_features("gages", query = list(limit = 2L), client = setup$client),
    class = "gx_error_reference_query"
  )
  expect_error(
    gx_ref_features("gages", query = list(id = c(1L, 2L)), client = setup$client),
    class = "gx_error_reference_query"
  )
  expect_error(
    gx_ref_features(
      "gages", query = stats::setNames(list("x"), NA_character_),
      client = setup$client
    ),
    class = "gx_error_reference_query"
  )
  expect_error(
    gx_ref_features("gages", allow_unbounded = FALSE, client = setup$client),
    class = "gx_error_reference_unbounded"
  )
  expect_equal(length(setup$state$calls), 3L)
})

test_that("oversized filter values fail before reference transport", {
  setup <- gx_ref_test_client(function(request, state) {
    stop("Oversized filters must fail before transport.", call. = FALSE)
  })
  withr::local_options(geoconnexr.ref_max_query_value_bytes = 16L)

  expect_error(
    gx_ref_features(
      "gages", query = list(provider_id = strrep("x", 17L)),
      client = setup$client
    ),
    class = "gx_error_reference_budget"
  )
  expect_length(setup$state$calls, 0L)
})

test_that("outbound query ceilings do not truncate inbound string properties", {
  long_name <- strrep("x", 5000L)
  feature <- gx_ref_test_feature(
    1L,
    list(
      id = 1L, provider_id = "P", nhdpv2_comid = 17789327L,
      name = long_name
    )
  )
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    gx_ref_test_response(
      request,
      body = gx_ref_test_json(gx_ref_test_collection(list(feature), 1L, 1L))
    )
  })
  withr::local_options(geoconnexr.ref_max_query_value_bytes = 16L)

  out <- gx_ref_features(
    "gages", query = list(provider_id = "P"), client = setup$client
  )

  expect_identical(out$name, long_name)
})

test_that("advertised object properties remain list-columns", {
  schema <- list(
    type = "object",
    properties = list(
      geometry = list(format = "geometry-any", `x-ogc-role` = "primary-geometry"),
      id = list(type = "string", `x-ogc-role` = "id"),
      details = list(type = "object")
    )
  )
  feature <- gx_ref_test_feature(
    "x", list(id = "x", details = list(foo = "bar"))
  )
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/custom/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_json(schema)))
    }
    gx_ref_test_response(
      request,
      body = gx_ref_test_json(gx_ref_test_collection(list(feature), 1L, 1L))
    )
  })

  out <- gx_ref_features(
    "custom", query = list(id = "x"), client = setup$client
  )

  expect_true(is.list(out$details))
  expect_identical(out$details[[1]], list(foo = "bar"))
})

test_that("unexpected null properties remain typed and diagnostic", {
  schema <- list(
    type = "object",
    properties = list(
      geometry = list(format = "geometry-any", `x-ogc-role` = "primary-geometry"),
      id = list(type = "string", `x-ogc-role` = "id"),
      optional = list(type = "string")
    )
  )
  feature <- gx_ref_test_feature(
    "x", list(id = "x", optional = NULL)
  )
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/custom/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_json(schema)))
    }
    gx_ref_test_response(
      request,
      body = gx_ref_test_json(gx_ref_test_collection(list(feature), 1L, 1L))
    )
  })

  out <- gx_ref_features(
    "custom", query = list(id = "x"), client = setup$client
  )

  expect_type(out$optional, "character")
  expect_true(is.na(out$optional))
  expect_contains(
    attr(out, "gx_reference")$diagnostics$code,
    "unexpected_null_property"
  )
})

test_that("pagination follows next links and reconciles stop conditions", {
  feature <- function(id) gx_ref_test_feature(
    id,
    list(id = id, provider_id = paste0("P-", id), nhdpv2_comid = id)
  )
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    expect_identical(httr2::url_parse(request$url)$query$f, "json")
    if (grepl("offset=2", request$url, fixed = TRUE)) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(
          list(feature(3L)), matched = 3L, returned = 1L
        ))
      ))
    }
    if (grepl("/collections/gages/items[?]", request$url)) {
      next_url <- paste0(
        sub("[?].*$", "", request$url),
        "?provider_id=P&limit=2&offset=2"
      )
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(
          list(feature(1L), feature(2L)), matched = 3L, returned = 2L,
          links = list(list(rel = "next", href = next_url))
        ))
      ))
    }
    stop("Unexpected reference request: ", request$url, call. = FALSE)
  })
  withr::local_options(geoconnexr.ref_page_size = 2L)

  out <- gx_ref_features(
    "gages", query = list(provider_id = "P"), limit = 3L,
    client = setup$client
  )
  metadata <- attr(out, "gx_reference")

  expect_identical(out$feature_id, c("1", "2", "3"))
  expect_equal(metadata$pages, 2L)
  expect_equal(metadata$stop_reason, "number_matched")
  expect_false(metadata$truncated)
  expect_match(setup$state$calls[[3]]$url, "[?]provider_id=P&limit=2&offset=2&f=json$")
})

test_that("repeated and missing next links return visible truncation", {
  mode <- "repeat"
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    feature <- gx_ref_test_feature(1L, list(id = 1L, provider_id = "P", nhdpv2_comid = 1L))
    payload <- if (mode == "repeat") {
      value <- gx_ref_test_collection(list(feature), links = list(list(
        rel = "next",
        href = "https://reference.geoconnex.us/collections/gages/items?limit=10&provider_id=P&f=json"
      )))
      value$numberMatched <- NULL
      value$numberReturned <- NULL
      value
    } else {
      gx_ref_test_collection(list(feature), matched = 3L, returned = 1L)
    }
    gx_ref_test_response(request, body = gx_ref_test_json(payload))
  })

  repeated <- gx_ref_features(
    "gages", query = list(provider_id = "P"), limit = 10L,
    client = setup$client
  )
  expect_true(attr(repeated, "gx_reference")$truncated)
  expect_equal(attr(repeated, "gx_reference")$stop_reason, "repeated_next")

  mode <- "missing"
  missing <- gx_ref_features(
    "gages", query = list(provider_id = "P"), limit = 10L,
    client = setup$client
  )
  metadata <- attr(missing, "gx_reference")
  expect_true(metadata$truncated)
  expect_equal(metadata$stop_reason, "no_next")
  expect_contains(metadata$diagnostics$code, "missing_next_link")
})

test_that("page and cumulative-byte budgets stop deterministically", {
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    feature <- gx_ref_test_feature(1L, list(id = 1L, provider_id = "P", nhdpv2_comid = 1L))
    payload <- gx_ref_test_collection(list(feature))
    payload$numberMatched <- NULL
    payload$numberReturned <- NULL
    payload$links <- list(list(rel = "next", href = paste0(request$url, "&offset=1")))
    gx_ref_test_response(request, body = gx_ref_test_json(payload))
  })
  withr::local_options(geoconnexr.ref_max_pages = 1L)

  paged <- gx_ref_features(
    "gages", query = list(provider_id = "P"), limit = 10L,
    client = setup$client
  )
  expect_true(attr(paged, "gx_reference")$truncated)
  expect_equal(attr(paged, "gx_reference")$stop_reason, "page_budget")
  expect_equal(length(setup$state$calls), 2L)

  setup_bytes <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    stop("Items request should not occur after the queryables exceed the budget.", call. = FALSE)
  })
  withr::local_options(geoconnexr.ref_total_bytes = 10L)
  expect_error(
    gx_ref_features(
      "gages", query = list(provider_id = "P"),
      client = setup_bytes$client
    ),
    class = "gx_error_reference_budget"
  )
  expect_equal(length(setup_bytes$state$calls), 1L)

  setup_feature <- gx_ref_test_client(function(request, state) {
    gx_ref_test_response(
      request,
      status = 500L,
      body = strrep("x", 20L),
      content_type = "text/plain"
    )
  })
  error <- tryCatch(
    gx_ref_feature("gages", "1", client = setup_feature$client),
    error = identity
  )
  expect_s3_class(error, "gx_error_reference_budget")
  expect_identical(error$budget_scope, "reference")
  expect_equal(nrow(error$requests), 1L)
  expect_equal(length(setup_feature$state$calls), 1L)
})

test_that("reference budget options are validated before network access", {
  setup <- gx_ref_test_client(function(request, state) {
    stop("Invalid options must fail before requesting queryables.", call. = FALSE)
  })
  withr::local_options(geoconnexr.ref_max_pages = 0L)

  expect_error(
    gx_ref_features(
      "gages", query = list(provider_id = "P"),
      client = setup$client
    ),
    class = "gx_error_client"
  )
  expect_length(setup$state$calls, 0L)
})

test_that("off-origin next links hard-stop before a second page request", {
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    feature <- gx_ref_test_feature(1L, list(id = 1L, provider_id = "P", nhdpv2_comid = 1L))
    gx_ref_test_response(
      request,
      body = gx_ref_test_json({
        value <- gx_ref_test_collection(list(feature))
        value$numberMatched <- NULL
        value$numberReturned <- NULL
        value$links <- list(list(rel = "next", href = "https://evil.example/items?offset=1"))
        value
      })
    )
  })

  expect_error(
    gx_ref_features(
      "gages", query = list(provider_id = "P"), limit = 10L,
      client = setup$client
    ),
    class = "gx_error_reference_endpoint"
  )
  expect_equal(length(setup$state$calls), 2L)
})

test_that("antimeridian bboxes are accepted and invalid latitude order is not", {
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/counties/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("counties")))
    }
    expect_match(request$url, "bbox=170%2C-10%2C-170%2C10")
    gx_ref_test_response(
      request,
      body = gx_ref_test_json(gx_ref_test_collection(list(), matched = 0L, returned = 0L))
    )
  })

  expect_no_error(gx_ref_features(
    "counties", bbox = c(170, -10, -170, 10), client = setup$client
  ))
  expect_error(
    gx_ref_features(
      "counties", bbox = c(-10, 20, 10, -20), client = setup$client
    ),
    class = "gx_error_reference_query"
  )
})

test_that("empty and leading-zero feature results retain advertised types", {
  empty <- FALSE
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/hu12/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("hu12")))
    }
    features <- if (empty) list() else list(gx_ref_test_feature(
      "010100020101",
      list(huc12 = "010100020101", name = "Fixture HUC")
    ))
    gx_ref_test_response(
      request,
      body = gx_ref_test_json(gx_ref_test_collection(features))
    )
  })

  populated <- gx_ref_features(
    "hu12", query = list(huc12 = "010100020101"),
    client = setup$client
  )
  expect_identical(populated$feature_id, "010100020101")
  expect_identical(populated$huc12, "010100020101")

  empty <- TRUE
  zero <- gx_ref_features(
    "hu12", query = list(huc12 = "999999999999"),
    client = setup$client
  )
  expect_s3_class(zero, "sf")
  expect_equal(nrow(zero), 0L)
  expect_type(zero$feature_id, "character")
  expect_type(zero$huc12, "character")
  expect_type(zero$name, "character")
})

test_that("path identifiers reject separators before endpoint construction", {
  expect_error(
    geoconnexr:::gx_ref_path_segment("a/b", "id"),
    class = "gx_error_reference_input"
  )
  expect_error(
    geoconnexr:::gx_ref_path_segment("a\\b", "id"),
    class = "gx_error_reference_input"
  )
  expect_error(
    geoconnexr:::gx_ref_path_segment("a%2Fb", "id"),
    class = "gx_error_reference_input"
  )
  expect_error(
    geoconnexr:::gx_ref_path_segment("%2e%2e", "id"),
    class = "gx_error_reference_input"
  )
  expect_error(
    geoconnexr:::gx_ref_path_segment("..", "id"),
    class = "gx_error_reference_input"
  )

  setup <- gx_ref_test_client(function(request, state) {
    stop("Traversal input must fail before transport.", call. = FALSE)
  })
  expect_error(
    gx_ref_feature(
      "gages", "x/../../../../admin", client = setup$client
    ),
    class = "gx_error_reference_input"
  )
  expect_length(setup$state$calls, 0L)
})

test_that("single-feature item retrieval verifies identity", {
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/items/1000001?f=json")) {
      feature <- gx_ref_test_feature(
        1000001L,
        list(id = 1000001L, provider_id = "USGS-08332622", nhdpv2_comid = 17789327)
      )
      return(gx_ref_test_response(request, body = gx_ref_test_json(feature)))
    }
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  }, retries = 3L)

  out <- gx_ref_feature("gages", "1000001", client = setup$client)
  metadata <- attr(out, "gx_reference")

  expect_s3_class(out, "gx_ref_feature")
  expect_equal(nrow(out), 1L)
  expect_identical(out$feature_id, "1000001")
  expect_equal(metadata$retrieval_mode, "item")
  expect_true(metadata$complete)
  expect_identical(metadata$attempts$stage, "item")
  expect_equal(setup$state$calls[[1]]$retries, 0L)
  expect_equal(setup$state$calls[[2]]$retries, 0L)
  returned <- NULL
  invisible(capture.output(returned <- print(out)))
  expect_s3_class(returned, "gx_ref_feature")
})

test_that("reference ledgers include each package-owned retry attempt", {
  queryable_calls <- 0L
  setup <- gx_ref_test_client(function(request, state) {
    queryable_calls <<- queryable_calls + 1L
    if (queryable_calls == 1L) {
      return(gx_ref_test_response(
        request, status = 503L, body = "wait", content_type = "text/plain"
      ))
    }
    gx_ref_test_response(request, body = gx_ref_test_queryables("gages"))
  }, retries = 1L)
  withr::local_options(geoconnexr.retry_jitter = function(max_seconds) 0)

  out <- gx_ref_queryables("gages", client = setup$client)
  requests <- attr(out, "gx_reference")$requests

  expect_identical(requests$status, c(503L, 200L))
  expect_identical(requests$bytes[[1]], 4L)
  expect_identical(requests$request_id[[1]], requests$request_id[[2]])
  expect_identical(queryable_calls, 2L)
  expect_true(all(vapply(setup$state$calls, `[[`, integer(1), "retries") == 0L))
})

test_that("single-feature fallback uses the advertised identity queryable", {
  collection <- "mainstems"
  schema <- gx_ref_test_queryables(collection)
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/mainstems/items/29559?f=json")) {
      return(gx_ref_test_response(request, status = 500L, body = "", content_type = "text/html"))
    }
    if (endsWith(request$url, "/collections/mainstems/queryables")) {
      return(gx_ref_test_response(request, body = schema))
    }
    if (grepl("/collections/mainstems/items[?]f=json&id=29559&limit=2$", request$url)) {
      feature <- gx_ref_test_feature(
        "29559",
        list(id = "29559", uri = "https://geoconnex.us/ref/mainstems/29559", name = "Fixture", nhdpv2_comid = 1),
        list(type = "LineString", coordinates = list(c(-111, 44), c(-110, 43)))
      )
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(list(feature), 1L, 1L))
      ))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  out <- gx_ref_feature(collection, "29559", client = setup$client)
  metadata <- attr(out, "gx_reference")

  expect_equal(metadata$retrieval_mode, "filter")
  expect_true(metadata$complete)
  expect_identical(metadata$attempts$stage, c("item", "filter"))
  expect_equal(metadata$attempts$status[[1]], 500L)
  expect_s3_class(sf::st_geometry(out), "sfc_LINESTRING")
  expect_equal(length(setup$state$calls), 3L)
})

test_that("post-response item errors retain their stage status", {
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/items/1?f=json")) {
      return(gx_ref_test_response(request, body = charToRaw("{")))
    }
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    if (grepl("/collections/gages/items[?]f=json&id=1&limit=2$", request$url)) {
      feature <- gx_ref_test_feature(
        1L, list(id = 1L, provider_id = "P", nhdpv2_comid = 1L)
      )
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(list(feature), 1L, 1L))
      ))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  out <- gx_ref_feature("gages", "1", client = setup$client)
  metadata <- attr(out, "gx_reference")

  expect_identical(metadata$retrieval_mode, "filter")
  expect_identical(metadata$attempts$stage, c("item", "filter"))
  expect_identical(metadata$attempts$status, c(200L, 200L))
  expect_identical(metadata$attempts$code[[1]], "reference_payload")
})

test_that("direct JSON-LD fallback probes remain single-attempt", {
  item_calls <- 0L
  direct_calls <- 0L
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/items/1?f=json")) {
      item_calls <<- item_calls + 1L
      return(gx_ref_test_response(request, status = 500L, body = ""))
    }
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    if (grepl("/collections/gages/items[?]f=json&id=1&limit=2$", request$url)) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(list(), 0L, 0L))
      ))
    }
    if (endsWith(request$url, "/collections/gages/items/1")) {
      direct_calls <<- direct_calls + 1L
      return(gx_ref_test_response(request, status = 503L, body = "wait"))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  }, retries = 3L)
  withr::local_options(geoconnexr.retry_jitter = function(max_seconds) 0)

  error <- NULL
  invisible(capture.output(
    error <- tryCatch(
      gx_ref_feature("gages", "1", client = setup$client),
      error = identity
    ),
    type = "message"
  ))

  expect_s3_class(error, "gx_error_reference_feature")
  expect_identical(item_calls, 1L)
  expect_identical(direct_calls, 1L)
  expect_identical(error$attempts$status, c(500L, 200L, 503L))
  expect_identical(error$attempts$stage, c("item", "filter", "jsonld"))
})

test_that("post-response JSON-LD payload errors retain their stage status", {
  item_url <- "https://reference.geoconnex.us/collections/gages/items/1"
  empty_graph <- list(
    `@context` = list(schema = "https://schema.org/"),
    `@graph` = list()
  )
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/items/1?f=json")) {
      return(gx_ref_test_response(request, status = 500L, body = ""))
    }
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    if (grepl("/collections/gages/items[?]f=json&id=1&limit=2$", request$url)) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(list(), 0L, 0L))
      ))
    }
    if (identical(request$url, item_url)) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(empty_graph),
        content_type = "application/ld+json"
      ))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  error <- tryCatch(
    gx_ref_feature("gages", "1", client = setup$client),
    error = identity
  )

  expect_s3_class(error, "gx_error_reference_feature")
  expect_identical(error$attempts$status, c(500L, 200L, 200L))
  expect_identical(error$attempts$code[[3]], "reference_payload")
  expect_identical(error$requests$status, c(500L, 200L, 200L, 200L))
})

test_that("non-id identity roles are used for filtered feature fallback", {
  id <- "010100020101"
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, paste0("/collections/hu12/items/", id, "?f=json"))) {
      return(gx_ref_test_response(request, status = 404L, body = "{}"))
    }
    if (endsWith(request$url, "/collections/hu12/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("hu12")))
    }
    if (grepl(paste0("[?]f=json&huc12=", id, "&limit=2$"), request$url)) {
      feature <- gx_ref_test_feature(id, list(huc12 = id, name = "Fixture HUC"))
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(list(feature), 1L, 1L))
      ))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  out <- gx_ref_feature("hu12", id, client = setup$client)

  expect_equal(attr(out, "gx_reference")$retrieval_mode, "filter")
  expect_identical(out$feature_id, id)
  expect_identical(out$huc12, id)
  expect_false(any(grepl("[?&]id=", vapply(setup$state$calls, `[[`, character(1), "url"))))
})

test_that("filtered fallback accepts a role identity when top-level id is absent", {
  id <- "010100020101"
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, paste0("/collections/hu12/items/", id, "?f=json"))) {
      return(gx_ref_test_response(request, status = 404L, body = "{}"))
    }
    if (endsWith(request$url, "/collections/hu12/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("hu12")))
    }
    if (grepl(paste0("[?]f=json&huc12=", id, "&limit=2$"), request$url)) {
      feature <- gx_ref_test_feature(id, list(huc12 = id, name = "Fixture HUC"))
      feature$id <- NULL
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(list(feature), 1L, 1L))
      ))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  out <- gx_ref_feature("hu12", id, client = setup$client)

  expect_equal(attr(out, "gx_reference")$retrieval_mode, "filter")
  expect_identical(out$feature_id, id)
  expect_identical(out$huc12, id)
})

test_that("contradictory top-level and advertised identities fail closed", {
  id <- "1000001"
  conflict <- gx_ref_test_feature(
    1000001L,
    list(id = 999L, provider_id = "P", nhdpv2_comid = 17789327L)
  )
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, paste0("/collections/gages/items/", id, "?f=json"))) {
      return(gx_ref_test_response(request, body = gx_ref_test_json(conflict)))
    }
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    if (grepl("/collections/gages/items[?]f=json&id=1000001&limit=2$", request$url)) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(list(conflict), 1L, 1L))
      ))
    }
    if (endsWith(request$url, paste0("/collections/gages/items/", id))) {
      return(gx_ref_test_response(request, status = 404L, body = "{}"))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  error <- tryCatch(
    gx_ref_feature("gages", id, client = setup$client),
    error = identity
  )

  expect_s3_class(error, "gx_error_reference_feature")
  expect_identical(
    error$attempts$code,
    c("reference_identity", "reference_identity", "reference_http")
  )
  expect_equal(nrow(error$requests), 4L)
})

test_that("an unmarked id property is not treated as the identity role", {
  id <- "abc"
  schema <- list(
    type = "object",
    properties = list(
      geometry = list(format = "geometry-any", `x-ogc-role` = "primary-geometry"),
      id = list(type = "string")
    )
  )
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/items/abc?f=json")) {
      return(gx_ref_test_response(request, status = 404L, body = "{}"))
    }
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_json(schema)))
    }
    if (endsWith(request$url, "/collections/gages/items/abc")) {
      return(gx_ref_test_response(request, status = 404L, body = "{}"))
    }
    stop("Filtered items must not be requested without an identity role.", call. = FALSE)
  })

  error <- tryCatch(
    gx_ref_feature("gages", id, client = setup$client),
    error = identity
  )

  expect_s3_class(error, "gx_error_reference_feature")
  expect_identical(error$attempts$stage, c("item", "filter", "jsonld"))
  expect_identical(error$attempts$code[[2]], "reference_identity")
  expect_equal(length(setup$state$calls), 3L)
})

test_that("single-item compatibility shapes reject malformed or incomplete identity", {
  duplicate <- structure(
    list("Feature", "1", "evil", list(), NULL),
    names = c("type", "id", "id", "properties", "geometry")
  )
  vector_id <- gx_ref_test_feature(list("1", "evil"), list(id = "1"))
  infinite_id <- gx_ref_test_feature(Inf, list(id = "1"))
  paged <- gx_ref_test_collection(
    list(gx_ref_test_feature("1", list(id = "1"))),
    matched = 2L,
    returned = 1L,
    links = list(list(rel = "next", href = "?offset=1"))
  )

  expect_false(geoconnexr:::gx_ref_valid_feature(duplicate))
  expect_error(
    geoconnexr:::gx_ref_item_features(vector_id),
    class = "gx_error_reference_payload"
  )
  expect_error(
    geoconnexr:::gx_ref_item_features(infinite_id),
    class = "gx_error_reference_payload"
  )
  expect_error(
    geoconnexr:::gx_ref_item_features(paged),
    class = "gx_error_reference_ambiguous"
  )
})

test_that("JSON-LD is the bounded, visibly incomplete final fallback", {
  schema <- gx_ref_test_queryables("mainstems")
  setup <- gx_ref_test_client(function(request, state) {
    accept <- request$headers[["accept"]]
    if (endsWith(request$url, "/collections/mainstems/items/29559?f=json")) {
      return(gx_ref_test_response(request, status = 500L, body = "", content_type = "text/html"))
    }
    if (endsWith(request$url, "/collections/mainstems/queryables")) {
      return(gx_ref_test_response(request, body = schema))
    }
    if (grepl("/collections/mainstems/items[?]f=json&id=29559&limit=2$", request$url)) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(list(), 0L, 0L))
      ))
    }
    if (endsWith(request$url, "/collections/mainstems/items/29559") &&
        grepl("application/ld[+]json", accept)) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_fixture("../jsonld/observed/reference-mainstem-29559.min.json"),
        content_type = "application/ld+json"
      ))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  out <- gx_ref_feature("mainstems", "29559", client = setup$client)
  metadata <- attr(out, "gx_reference")

  expect_equal(metadata$retrieval_mode, "jsonld")
  expect_false(metadata$complete)
  expect_contains(metadata$diagnostics$code, "jsonld_fallback_incomplete")
  expect_identical(out$feature_id, "29559")
  expect_identical(out$id, "29559")
  expect_s3_class(sf::st_geometry(out), "sfc_LINESTRING")
  expect_identical(metadata$attempts$stage, c("item", "filter", "jsonld"))
  expect_equal(length(setup$state$calls), 4L)
})

test_that("top-level JSON-LD arrays remain compatible with the final fallback", {
  id <- "1"
  item_url <- "https://reference.geoconnex.us/collections/gages/items/1"
  expanded <- list(list(
    `@id` = item_url,
    `@type` = list("https://schema.org/Place"),
    `https://schema.org/name` = list(list(`@value` = "Array feature")),
    `http://www.opengis.net/ont/geosparql#hasGeometry` = list(list(
      `http://www.opengis.net/ont/geosparql#asWKT` = list(list(
        `@value` = "POINT (-107 35)"
      ))
    ))
  ))
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/items/1?f=json")) {
      return(gx_ref_test_response(request, status = 500L, body = ""))
    }
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    if (grepl("/collections/gages/items[?]f=json&id=1&limit=2$", request$url)) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(list(), 0L, 0L))
      ))
    }
    if (identical(request$url, item_url)) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(expanded),
        content_type = "application/ld+json"
      ))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  out <- gx_ref_feature("gages", id, client = setup$client)

  expect_equal(attr(out, "gx_reference")$retrieval_mode, "jsonld")
  expect_identical(out$feature_id, id)
  expect_identical(out$id, id)
  expect_s3_class(sf::st_geometry(out), "sfc_POINT")
})

test_that("mainstems v3 does not inherit a legacy JSON-LD identity", {
  id <- "29559"
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/mainstems_v3/items/29559?f=json")) {
      return(gx_ref_test_response(request, status = 500L, body = ""))
    }
    if (endsWith(request$url, "/collections/mainstems_v3/queryables")) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_queryables("mainstems_v3")
      ))
    }
    if (grepl("/collections/mainstems_v3/items[?]f=json&id=29559&limit=2$", request$url)) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_json(gx_ref_test_collection(list(), 0L, 0L))
      ))
    }
    if (endsWith(request$url, "/collections/mainstems_v3/items/29559")) {
      return(gx_ref_test_response(
        request,
        body = gx_ref_test_fixture("../jsonld/observed/reference-mainstem-29559.min.json"),
        content_type = "application/ld+json"
      ))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  error <- tryCatch(
    gx_ref_feature("mainstems_v3", id, client = setup$client),
    error = identity
  )

  expect_s3_class(error, "gx_error_reference_feature")
  expect_identical(error$attempts$stage, c("item", "filter", "jsonld"))
  expect_identical(error$attempts$code[[3]], "reference_identity")
  expect_equal(nrow(error$requests), 4L)
})

test_that("reference JSON depth budgets hard-stop before fallback", {
  setup <- gx_ref_test_client(function(request, state) {
    body <- charToRaw(paste0(
      '{"type":"Feature","id":"1","properties":{"id":"1",',
      '"nested":{"too":"deep"}},"geometry":null}'
    ))
    gx_ref_test_response(request, body = body)
  })
  withr::local_options(geoconnexr.jsonld_max_depth = 2L)

  error <- tryCatch(
    gx_ref_feature("gages", "1", client = setup$client),
    error = identity
  )
  expect_s3_class(error, "gx_error_jsonld_too_deep")
  expect_equal(nrow(error$requests), 1L)
  expect_equal(length(setup$state$calls), 1L)
})

test_that("malformed fallback responses remain in the final response ledger", {
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/items/1?f=json")) {
      return(gx_ref_test_response(request, status = 500L, body = ""))
    }
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    if (grepl("/collections/gages/items[?]f=json&id=1&limit=2$", request$url)) {
      return(gx_ref_test_response(request, body = charToRaw("{")))
    }
    if (endsWith(request$url, "/collections/gages/items/1")) {
      return(gx_ref_test_response(
        request,
        body = charToRaw("{"),
        content_type = "application/ld+json"
      ))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  error <- tryCatch(
    gx_ref_feature("gages", "1", client = setup$client),
    error = identity
  )

  expect_s3_class(error, "gx_error_reference_feature")
  expect_equal(length(setup$state$calls), 4L)
  expect_equal(nrow(error$requests), 4L)
  expect_identical(error$requests$status, c(500L, 200L, 200L, 200L))
  expect_identical(
    error$attempts$code,
    c("http_500", "reference_payload", "reference_payload")
  )
  expect_identical(error$attempts$status, c(500L, 200L, 200L))
})

test_that("unsafe item redirects hard-stop instead of falling through", {
  setup <- gx_ref_test_client(function(request, state) {
    gx_ref_test_response(
      request,
      status = 302L,
      headers = list(Location = "https://evil.example/private")
    )
  })

  expect_error(
    gx_ref_feature("gages", "1000001", client = setup$client),
    class = "gx_error_reference_endpoint"
  )
  expect_equal(length(setup$state$calls), 1L)
})

test_that("all compatible feature routes fail with ordered attempts", {
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/items/missing?f=json")) {
      return(gx_ref_test_response(request, status = 404L, body = "{}"))
    }
    if (endsWith(request$url, "/collections/gages/queryables")) {
      body <- list(
        type = "object",
        properties = list(geometry = list(format = "geometry-any", `x-ogc-role` = "primary-geometry"))
      )
      return(gx_ref_test_response(request, body = gx_ref_test_json(body)))
    }
    if (endsWith(request$url, "/collections/gages/items/missing")) {
      return(gx_ref_test_response(request, status = 404L, body = "{}"))
    }
    stop("Unexpected request: ", request$url, call. = FALSE)
  })

  error <- tryCatch(
    gx_ref_feature("gages", "missing", client = setup$client),
    error = identity
  )

  expect_s3_class(error, "gx_error_reference_feature")
  expect_identical(error$attempts$stage, c("item", "filter", "jsonld"))
  expect_identical(error$attempts$code, c("http_404", "reference_identity", "reference_http"))
  expect_identical(error$attempts$status, c(404L, NA_integer_, 404L))
  expect_equal(nrow(error$requests), 3L)
  expect_identical(error$requests$status, c(404L, 200L, 404L))
})

test_that("invalid response counts and changing counts remain diagnostic", {
  page <- 0L
  setup <- gx_ref_test_client(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    page <<- page + 1L
    feature <- gx_ref_test_feature(page, list(id = page, provider_id = "P", nhdpv2_comid = page))
    payload <- gx_ref_test_collection(
      list(feature), matched = if (page == 1L) 2L else 3L, returned = list(1L, 2L),
      links = if (page < 3L) list(list(rel = "next", href = paste0(request$url, "&p=", page + 1L))) else list()
    )
    gx_ref_test_response(request, body = gx_ref_test_json(payload))
  })

  out <- gx_ref_features(
    "gages", query = list(provider_id = "P"), limit = 10L,
    client = setup$client
  )
  diagnostics <- attr(out, "gx_reference")$diagnostics

  expect_equal(nrow(out), 3L)
  expect_contains(diagnostics$code, "number_matched_changed")
  expect_contains(diagnostics$code, "invalid_number_returned")
  expect_true(is.na(attr(out, "gx_reference")$number_matched))
})

test_that("query-bearing feature responses are not promised offline", {
  cache_dir <- withr::local_tempdir()
  routes <- gx_ref_test_router(function(request, state) {
    if (endsWith(request$url, "/collections/gages/queryables")) {
      return(gx_ref_test_response(request, body = gx_ref_test_queryables("gages")))
    }
    feature <- gx_ref_test_feature(1L, list(id = 1L, provider_id = "P", nhdpv2_comid = 1L))
    gx_ref_test_response(
      request,
      body = gx_ref_test_json(gx_ref_test_collection(list(feature), 1L, 1L))
    )
  })
  withr::local_options(list(
    geoconnexr.performer = routes$performer,
    geoconnexr.dns_resolver = gx_ref_test_dns,
    geoconnexr.clock = gx_ref_test_clock,
    geoconnexr.cache_dir = cache_dir,
    geoconnexr.offline = FALSE
  ))
  online <- gx_client("reference", retries = 0L, cache = TRUE, cache_dir = cache_dir)
  expect_no_error(gx_ref_features(
    "gages", query = list(provider_id = "P"), client = online
  ))

  withr::local_options(geoconnexr.performer = function(request) {
    stop("Offline reference access attempted the network.", call. = FALSE)
  })
  offline <- gx_client(
    "reference", retries = 0L, cache = TRUE, offline = TRUE,
    cache_dir = cache_dir
  )
  expect_error(
    gx_ref_features(
      "gages", query = list(provider_id = "P"), client = offline
    ),
    class = "gx_error_offline_miss"
  )
})
