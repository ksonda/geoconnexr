test_that("SPARQL fixture manifest pins exact stored bytes", {
  manifest <- jsonlite::fromJSON(
    gx_graph_fixture_path("manifest-v1.json"),
    simplifyVector = FALSE
  )
  expect_identical(manifest$fixture_version, 1L)
  expect_identical(manifest$standard, "SPARQL 1.1 Query Results JSON")

  paths <- vapply(manifest$fixtures, `[[`, character(1), "path")
  expected <- vapply(
    manifest$fixtures, `[[`, character(1), "stored_sha256"
  )
  observed <- vapply(paths, function(path) {
    digest::digest(
      file = gx_graph_fixture_path(path),
      algo = "sha256",
      serialize = FALSE
    )
  }, character(1))

  expect_identical(unname(observed), unname(expected))
  expect_true(all(startsWith(
    vapply(manifest$fixtures, `[[`, character(1), "kind"),
    "synthetic_"
  )))
})

test_that("experimental graph execution and parsing remain unexported", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_graph_execute_once",
    "gx_graph_parse_results",
    "gx_sparql"
  ) %in% exports))
})

test_that("SELECT parsing preserves ordered sparse RDF term metadata", {
  body <- gx_graph_fixture_raw("select-terms.min.json")
  result <- gx_graph_test_parse(body)

  expect_s3_class(result, "gx_sparql_select")
  expect_s3_class(result, "gx_sparql_result")
  expect_identical(result$contract_version, "0.1.0")
  expect_identical(result$contract_status, "experimental")
  expect_identical(result$result_type, "select")
  expect_identical(
    result$variables,
    c("iri", "node", "plain", "label", "count", "never")
  )
  expect_identical(result$row_count, 2L)
  expect_identical(result$links, "urn:example:metadata")
  expect_identical(
    names(result$bindings),
    c(
      "row", "variable_index", "variable", "term_type", "value",
      "datatype", "language", "bnode_scope"
    )
  )
  expect_identical(result$bindings$row, c(rep(1L, 5L), rep(2L, 4L)))
  expect_identical(
    result$bindings$variable_index,
    c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 5L)
  )
  expect_identical(
    result$bindings$variable,
    c(
      "iri", "node", "plain", "label", "count",
      "iri", "node", "plain", "count"
    )
  )
  expect_identical(
    result$bindings$term_type,
    c(
      "uri", "bnode", "literal", "literal", "literal",
      "uri", "bnode", "literal", "literal"
    )
  )
  expect_identical(
    result$bindings$value,
    c(
      "https://example.org/id/1", "b0", "", "Café ☃", "007",
      "urn:example:id:2", "b0", "NA", "1.2300"
    )
  )
  expect_identical(
    result$bindings$datatype,
    c(
      NA_character_, NA_character_, NA_character_, NA_character_,
      "http://www.w3.org/2001/XMLSchema#integer",
      NA_character_, NA_character_, NA_character_,
      "http://www.w3.org/2001/XMLSchema#decimal"
    )
  )
  expect_identical(
    result$bindings$language,
    c(
      NA_character_, NA_character_, NA_character_, "en-US", NA_character_,
      NA_character_, NA_character_, NA_character_, NA_character_
    )
  )
  expect_false("never" %in% result$bindings$variable)

  bnodes <- result$bindings[result$bindings$term_type == "bnode", ]
  expect_identical(bnodes$value, c("b0", "b0"))
  expect_identical(length(unique(bnodes$bnode_scope)), 1L)
  expect_match(bnodes$bnode_scope[[1]], "^[0-9a-f]{64}$")
  expect_true(all(is.na(
    result$bindings$bnode_scope[result$bindings$term_type != "bnode"]
  )))

  reparsed <- gx_graph_test_parse(body)
  expect_false(identical(
    unique(bnodes$bnode_scope),
    unique(reparsed$bindings$bnode_scope[
      reparsed$bindings$term_type == "bnode"
    ])
  ))
  expect_identical(
    result$document_sha256,
    digest::digest(body, algo = "sha256", serialize = FALSE)
  )
  expect_true(result$complexity$depth > 0L)
  expect_true(result$complexity$members > 0)
  expect_true(result$complexity$atomic_bytes > 0)
})

test_that("SELECT row_count distinguishes empty, zero-width, and unbound rows", {
  empty <- gx_graph_test_parse(
    gx_graph_fixture_raw("select-empty.min.json"),
    max_rows = 0L
  )
  zero_width <- gx_graph_test_parse(
    gx_graph_fixture_raw("select-zero-width.min.json")
  )
  unbound <- gx_graph_test_parse(
    gx_graph_fixture_raw("select-unbound-row.min.json")
  )

  expect_identical(empty$variables, c("x", "y"))
  expect_identical(empty$row_count, 0L)
  expect_identical(nrow(empty$bindings), 0L)

  expect_identical(zero_width$variables, character())
  expect_identical(zero_width$row_count, 1L)
  expect_identical(nrow(zero_width$bindings), 0L)

  expect_identical(unbound$variables, c("x", "y"))
  expect_identical(unbound$row_count, 1L)
  expect_identical(nrow(unbound$bindings), 0L)
  expect_identical(names(empty$bindings), names(zero_width$bindings))
  expect_identical(names(empty$bindings), names(unbound$bindings))
})

test_that("ASK parsing retains exact JSON booleans and finite links", {
  true_result <- gx_graph_test_parse(
    gx_graph_fixture_raw("ask-true.min.json"),
    expected = "ask"
  )
  false_result <- gx_graph_test_parse(
    gx_graph_fixture_raw("ask-false.min.json"),
    expected = "ask"
  )

  expect_s3_class(true_result, "gx_sparql_ask")
  expect_s3_class(false_result, "gx_sparql_ask")
  expect_identical(true_result$result_type, "ask")
  expect_true(true_result$value)
  expect_false(false_result$value)
  expect_identical(true_result$links, "urn:example:ask-metadata")
  expect_identical(false_result$links, character())
  expect_false("bindings" %in% names(true_result))
  expect_false("row_count" %in% names(true_result))
})

test_that("SPARQL parser accepts BOM and extensions but rejects malformed shapes", {
  extension <- charToRaw(paste0(
    '{"head":{"vars":[]},"results":{"bindings":[]},',
    '"vendor":{"trace":"ignored"}}'
  ))
  extended <- gx_graph_test_parse(extension)
  expect_identical(extended$row_count, 0L)
  expect_false("vendor" %in% names(extended))

  with_bom <- c(
    as.raw(c(239L, 187L, 191L)),
    gx_graph_fixture_raw("ask-false.min.json")
  )
  expect_false(gx_graph_test_parse(with_bom, expected = "ask")$value)

  cases <- list(
    list(
      name = "incomplete JSON", expected = "select",
      class = "gx_error_graph_payload", body = '{"head":'
    ),
    list(
      name = "top-level array", expected = "select",
      class = "gx_error_graph_payload", body = "[]"
    ),
    list(
      name = "missing head", expected = "select",
      class = "gx_error_graph_payload",
      body = '{"results":{"bindings":[]}}'
    ),
    list(
      name = "both result forms", expected = "select",
      class = "gx_error_graph_result_type",
      body = paste0(
        '{"head":{"vars":[]},"results":{"bindings":[]},',
        '"boolean":true}'
      )
    ),
    list(
      name = "no result form", expected = "select",
      class = "gx_error_graph_result_type",
      body = '{"head":{"vars":[]}}'
    ),
    list(
      name = "wrong expected form", expected = "ask",
      class = "gx_error_graph_result_type",
      body = '{"head":{"vars":[]},"results":{"bindings":[]}}'
    ),
    list(
      name = "duplicate object member", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":[]},"head":{"vars":[]},',
        '"results":{"bindings":[]}}'
      )
    ),
    list(
      name = "missing variable array", expected = "select",
      class = "gx_error_graph_payload",
      body = '{"head":{},"results":{"bindings":[]}}'
    ),
    list(
      name = "duplicate variables", expected = "select",
      class = "gx_error_graph_payload",
      body = '{"head":{"vars":["x","x"]},"results":{"bindings":[]}}'
    ),
    list(
      name = "extra results member", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":[]},"results":{"bindings":[],',
        '"more":true}}'
      )
    ),
    list(
      name = "bindings object", expected = "select",
      class = "gx_error_graph_payload",
      body = '{"head":{"vars":[]},"results":{"bindings":{}}}'
    ),
    list(
      name = "binding row array", expected = "select",
      class = "gx_error_graph_payload",
      body = '{"head":{"vars":[]},"results":{"bindings":[[]]}}'
    ),
    list(
      name = "undeclared variable", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"y":{"type":"literal","value":"y"}}]}}'
      )
    ),
    list(
      name = "null term", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"x":null}]}}'
      )
    ),
    list(
      name = "term missing value", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"x":{"type":"literal"}}]}}'
      )
    ),
    list(
      name = "numeric lexical value", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"x":{"type":"literal","value":7}}]}}'
      )
    ),
    list(
      name = "unsupported term type", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"x":{"type":"triple","value":"x"}}]}}'
      )
    ),
    list(
      name = "literal with language and datatype", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"x":{"type":"literal","value":"x","xml:lang":"en",',
        '"datatype":"urn:datatype"}}]}}'
      )
    ),
    list(
      name = "URI with literal metadata", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"x":{"type":"uri","value":"urn:x",',
        '"datatype":"urn:datatype"}}]}}'
      )
    ),
    list(
      name = "relative URI value", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"x":{"type":"uri","value":"relative/path"}}]}}'
      )
    ),
    list(
      name = "relative datatype", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"x":{"type":"literal","value":"x",',
        '"datatype":"relative/type"}}]}}'
      )
    ),
    list(
      name = "underscore language tag", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"x":{"type":"literal","value":"x",',
        '"xml:lang":"en_US"}}]}}'
      )
    ),
    list(
      name = "term extension", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"x":{"type":"literal","value":"x","extra":true}}]}}'
      )
    ),
    list(
      name = "duplicate nested term member", expected = "select",
      class = "gx_error_graph_payload",
      body = paste0(
        '{"head":{"vars":["x"]},"results":{"bindings":[',
        '{"x":{"type":"literal","value":"x","value":"y"}}]}}'
      )
    ),
    list(
      name = "ASK string", expected = "ask",
      class = "gx_error_graph_payload",
      body = '{"head":{},"boolean":"true"}'
    ),
    list(
      name = "ASK variable head", expected = "ask",
      class = "gx_error_graph_payload",
      body = '{"head":{"vars":[]},"boolean":true}'
    ),
    list(
      name = "SPARQL 1.2 root version", expected = "ask",
      class = "gx_error_graph_version",
      body = '{"version":"1.2","head":{},"boolean":true}'
    ),
    list(
      name = "SPARQL 1.2 head version", expected = "ask",
      class = "gx_error_graph_version",
      body = '{"head":{"version":"1.2"},"boolean":true}'
    )
  )

  for (case in cases) {
    expect_error(
      gx_graph_test_parse(
        charToRaw(case$body),
        expected = case$expected
      ),
      class = case$class,
      info = case$name
    )
  }

  expect_error(
    gx_graph_test_parse(as.raw(255L)),
    class = "gx_error_graph_payload"
  )
  expect_error(
    gx_graph_test_parse(c(charToRaw('{"head":'), as.raw(0L))),
    class = "gx_error_graph_payload"
  )
})

test_that("SPARQL parser enforces exact finite complexity boundaries", {
  body <- gx_graph_fixture_raw("select-terms.min.json")
  baseline <- gx_graph_test_parse(body)

  exact <- gx_graph_test_parse(
    body,
    max_rows = 2L,
    max_variables = 6L,
    max_bound_terms = 9L,
    max_links = 1L,
    max_members = baseline$complexity$members,
    max_atomic_bytes = baseline$complexity$atomic_bytes,
    max_depth = baseline$complexity$depth
  )
  expect_identical(exact$row_count, 2L)
  expect_identical(nrow(exact$bindings), 9L)

  nested_extension_body <- charToRaw(paste0(
    '{"head":{"vars":[]},"results":{"bindings":[]},',
    '"vendor":{"nested":{}}}'
  ))
  nested_extension <- gx_graph_test_parse(nested_extension_body)
  expect_identical(nested_extension$complexity$members, 6)
  nested_exact <- gx_graph_test_parse(
    nested_extension_body,
    max_members = nested_extension$complexity$members
  )
  expect_identical(nested_exact$row_count, 0L)
  expect_identical(
    nested_exact$complexity$members,
    nested_extension$complexity$members
  )

  row_error <- expect_error(
    gx_graph_test_parse(body, max_rows = 1L),
    class = "gx_error_graph_budget"
  )
  expect_identical(row_error$budget_kind, "rows")
  variable_error <- expect_error(
    gx_graph_test_parse(body, max_variables = 5L),
    class = "gx_error_graph_budget"
  )
  expect_identical(variable_error$budget_kind, "variables")
  term_error <- expect_error(
    gx_graph_test_parse(body, max_bound_terms = 8L),
    class = "gx_error_graph_budget"
  )
  expect_identical(term_error$budget_kind, "bound_terms")
  link_error <- expect_error(
    gx_graph_test_parse(body, max_links = 0L),
    class = "gx_error_graph_budget"
  )
  expect_identical(link_error$budget_kind, "links")

  for (limit in c("max_members", "max_atomic_bytes", "max_depth")) {
    value <- baseline$complexity[[sub("^max_", "", limit)]]
    args <- setNames(list(value - 1L), limit)
    error <- expect_error(
      do.call(gx_graph_test_parse, c(list(body = body), args)),
      class = "gx_error_graph_budget",
      info = limit
    )
    expected_kind <- if (identical(limit, "max_atomic_bytes")) {
      "bytes"
    } else {
      sub("^max_", "", limit)
    }
    expect_identical(error$budget_kind, expected_kind, info = limit)
  }

  invalid_limits <- list(
    list(max_rows = -1L),
    list(max_variables = NA_integer_),
    list(max_bound_terms = Inf),
    list(max_links = 1.5),
    list(max_members = 0L),
    list(max_atomic_bytes = 0L),
    list(max_depth = 0L)
  )
  for (args in invalid_limits) {
    expect_error(
      do.call(gx_graph_test_parse, c(list(body = body), args)),
      class = "gx_error_graph_input"
    )
  }

  query_limit <- geoconnexr:::.gx_graph_query_byte_limit
  exact_query <- strrep("x", query_limit)
  expect_identical(
    length(geoconnexr:::gx_graph_query_text(exact_query)$raw),
    query_limit
  )
  expect_error(
    geoconnexr:::gx_graph_query_text(paste0(exact_query, "x")),
    class = "gx_error_graph_input"
  )
  utf8_query <- 'ASK { BIND("Café ☃" AS ?label) }'
  expect_identical(
    geoconnexr:::gx_graph_query_text(utf8_query)$raw,
    charToRaw(enc2utf8(utf8_query))
  )
})

test_that("graph execution performs one exact logical POST with provenance", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  body <- gx_graph_fixture_raw("select-terms.min.json")
  script <- gx_graph_test_script(list(gx_graph_test_response(
    body,
    content_type = "Application/SPARQL-Results+JSON; charset=UTF-8"
  )))
  withr::local_options(list(
    geoconnexr.endpoint_graph = "https://graph.example/sparql",
    geoconnexr.performer = script$performer,
    geoconnexr.dns_resolver = gx_graph_test_public_dns,
    geoconnexr.clock = gx_graph_test_clock,
    geoconnexr.throttle_clock = function() 0
  ))
  client <- gx_client(
    "graph", retries = 0L, min_interval = 0, max_bytes = length(body),
    cache = FALSE, cache_dir = withr::local_tempdir()
  )
  query <- paste(
    "SELECT ?iri ?node ?plain ?label ?count ?never WHERE { ?iri ?p ?o }",
    "LIMIT 2",
    "OFFSET 0 # preserve-exactly-no-pagination",
    sep = "\n"
  )

  result <- gx_graph_test_execute(
    query,
    client,
    max_rows = 2L,
    max_variables = 6L,
    max_bound_terms = 9L,
    max_links = 1L,
    max_requests = 1L,
    max_total_bytes = length(body)
  )

  expect_identical(script$state$index, 1L)
  request <- script$state$calls[[1]]
  expect_identical(request$method, "POST")
  expect_identical(request$url, "https://graph.example/sparql")
  expect_identical(request$body, charToRaw(query))
  expect_identical(
    request$headers[["accept"]],
    "application/sparql-results+json"
  )
  expect_identical(
    request$headers[["content-type"]],
    "application/sparql-query"
  )
  expect_identical(request$headers[["accept-encoding"]], "identity")
  expect_identical(request$retries, 0L)

  expect_true(result$complete)
  expect_identical(result$row_count, 2L)
  expect_identical(
    result$query_sha256,
    digest::digest(charToRaw(query), algo = "sha256", serialize = FALSE)
  )
  expect_identical(result$endpoint, "https://graph.example/sparql")
  expect_identical(result$response$status, 200L)
  expect_identical(
    result$response$media_type,
    "application/sparql-results+json"
  )
  expect_identical(result$response$bytes, as.integer(length(body)))
  expect_false(result$response$from_cache)
  expect_identical(result$response$cache_origin, "network")
  expect_identical(nrow(result$requests), 1L)
  expect_identical(result$requests$method, "POST")
  expect_identical(result$requests$status, 200L)
  expect_identical(result$requests$bytes, as.integer(length(body)))
  expect_identical(result$requests$body_sha256, result$document_sha256)
  expect_identical(nrow(result$attempts), 1L)
  expect_true(result$attempts$physical)
  expect_identical(result$attempts$resolved_host, "graph.example")
  expect_identical(result$attempts$resolved_ip, "93.184.216.34")
})

test_that("graph executor independently caps each response at 16 MiB", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  body <- gx_graph_fixture_raw("ask-false.min.json")
  script <- gx_graph_test_script(list(gx_graph_test_response(body)))
  withr::local_options(list(
    geoconnexr.endpoint_graph = "https://graph.example/sparql",
    geoconnexr.performer = script$performer,
    geoconnexr.dns_resolver = gx_graph_test_public_dns,
    geoconnexr.clock = gx_graph_test_clock,
    geoconnexr.throttle_clock = function() 0
  ))
  response_cap <- geoconnexr:::.gx_graph_response_byte_limit
  client <- gx_client(
    "graph",
    retries = 0L,
    min_interval = 0,
    max_bytes = 2L * response_cap,
    cache = FALSE,
    cache_dir = withr::local_tempdir()
  )

  result <- gx_graph_test_execute(
    "ASK { FILTER(false) }",
    client,
    expected = "ask",
    max_requests = 1L,
    max_total_bytes = 2L * response_cap
  )

  expect_identical(script$state$index, 1L)
  expect_identical(script$state$calls[[1]]$max_bytes, as.integer(response_cap))
  expect_identical(result$limits$max_result_raw_bytes, response_cap)
  expect_false(result$value)
})

test_that("graph retries preserve one query and expose every physical attempt", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  body <- gx_graph_fixture_raw("ask-true.min.json")
  script <- gx_graph_test_script(list(
    gx_graph_test_response(
      charToRaw("wait"), status = 503L, content_type = "text/plain"
    ),
    gx_graph_test_response(body)
  ))
  sleeps <- numeric()
  withr::local_options(list(
    geoconnexr.endpoint_graph = "https://graph.example/sparql",
    geoconnexr.performer = script$performer,
    geoconnexr.dns_resolver = gx_graph_test_public_dns,
    geoconnexr.clock = gx_graph_test_clock,
    geoconnexr.throttle_clock = function() 0,
    geoconnexr.retry_jitter = function(max_seconds) 0.5,
    geoconnexr.retry_sleep = function(seconds) {
      sleeps <<- c(sleeps, seconds)
    }
  ))
  client <- gx_client(
    "graph", retries = 1L, min_interval = 0, cache = FALSE,
    cache_dir = withr::local_tempdir()
  )
  query <- "ASK { ?s ?p ?o }"

  result <- gx_graph_test_execute(
    query,
    client,
    expected = "ask",
    max_requests = 2L
  )

  expect_true(result$value)
  expect_identical(script$state$index, 2L)
  expect_identical(sleeps, 0.5)
  expect_identical(result$requests$status, c(503L, 200L))
  expect_identical(result$attempts$attempt, c(1L, 2L))
  expect_identical(result$attempts$delay, c(0.5, NA_real_))
  expect_identical(result$attempts$retryable, c(TRUE, FALSE))
  expect_length(unique(result$attempts$request_id), 1L)
  expect_identical(
    lapply(script$state$calls, `[[`, "body"),
    list(charToRaw(query), charToRaw(query))
  )

  capped <- gx_graph_test_script(list(
    gx_graph_test_response(
      charToRaw("wait"), status = 503L, content_type = "text/plain"
    ),
    gx_graph_test_response(body)
  ))
  withr::local_options(geoconnexr.performer = capped$performer)
  budget_error <- expect_error(
    gx_graph_test_execute(
      query,
      client,
      expected = "ask",
      max_requests = 1L
    ),
    class = "gx_error_graph_budget"
  )
  expect_identical(capped$state$index, 1L)
  expect_identical(nrow(budget_error$requests), 1L)
  expect_identical(nrow(budget_error$attempts), 1L)

  byte_capped <- gx_graph_test_script(list(gx_graph_test_response(body)))
  withr::local_options(geoconnexr.performer = byte_capped$performer)
  byte_error <- expect_error(
    gx_graph_test_execute(
      query,
      client,
      expected = "ask",
      max_requests = 1L,
      max_total_bytes = length(body) - 1L
    ),
    class = "gx_error_graph_budget"
  )
  expect_identical(byte_error$budget_kind, "bytes")
  expect_identical(byte_capped$state$index, 1L)
  expect_identical(nrow(byte_error$requests), 1L)

  for (invalid in list(
    list(max_requests = 0L),
    list(max_total_bytes = 0L)
  )) {
    expect_error(
      do.call(
        gx_graph_test_execute,
        c(list(query = query, client = client, expected = "ask"), invalid)
      ),
      class = "gx_error_graph_input"
    )
  }
  expect_identical(byte_capped$state$index, 1L)
})

test_that("graph cache and offline paths avoid DNS, throttle, and transport", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  body <- gx_graph_fixture_raw("ask-false.min.json")
  script <- gx_graph_test_script(list(gx_graph_test_response(body)))
  dns_calls <- 0L
  throttle_calls <- 0L
  withr::local_options(list(
    geoconnexr.endpoint_graph = "https://graph.example/sparql",
    geoconnexr.performer = script$performer,
    geoconnexr.dns_resolver = function(host) {
      dns_calls <<- dns_calls + 1L
      gx_graph_test_public_dns(host)
    },
    geoconnexr.clock = gx_graph_test_clock,
    geoconnexr.throttle_clock = function() {
      throttle_calls <<- throttle_calls + 1L
      0
    }
  ))
  cache_dir <- withr::local_tempdir()
  online <- gx_client(
    "graph", retries = 0L, min_interval = 0, cache = TRUE,
    cache_dir = cache_dir
  )
  query <- "ASK { FILTER(false) }"

  network <- gx_graph_test_execute(
    query, online, expected = "ask", max_requests = 1L
  )
  counters_after_network <- c(
    performer = script$state$index,
    dns = dns_calls,
    throttle = throttle_calls
  )
  cached <- gx_graph_test_execute(
    query, online, expected = "ask", max_requests = 1L
  )

  expect_false(network$response$from_cache)
  expect_identical(network$requests$cache_origin, "network")
  expect_true(cached$response$from_cache)
  expect_identical(cached$response$cache_origin, "fresh_cache")
  expect_identical(cached$requests$cache_origin, "fresh_cache")
  expect_identical(nrow(cached$requests), 1L)
  expect_identical(nrow(cached$attempts), 0L)
  expect_identical(
    c(
      performer = script$state$index,
      dns = dns_calls,
      throttle = throttle_calls
    ),
    counters_after_network
  )

  offline <- gx_client(
    "graph", retries = 0L, min_interval = 0, cache = TRUE, offline = TRUE,
    cache_dir = cache_dir
  )
  offline_hit <- gx_graph_test_execute(
    query, offline, expected = "ask", max_requests = 1L
  )
  expect_false(offline_hit$value)
  expect_true(offline_hit$response$from_cache)
  expect_identical(offline_hit$response$cache_origin, "offline_cache")
  expect_identical(offline_hit$requests$cache_origin, "offline_cache")
  expect_identical(nrow(offline_hit$attempts), 0L)

  miss <- expect_error(
    gx_graph_test_execute(
      paste0(query, " # different body"),
      offline,
      expected = "ask",
      max_requests = 1L
    ),
    class = "gx_error_graph_offline"
  )
  expect_identical(nrow(miss$requests), 0L)
  expect_identical(
    c(
      performer = script$state$index,
      dns = dns_calls,
      throttle = throttle_calls
    ),
    counters_after_network
  )
})

test_that("semantically malformed cache entries refetch online and fail offline", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  malformed <- charToRaw(paste0(
    '{"head":{"vars":["x"]},"results":{"bindings":[',
    '{"x":{"type":"literal"}}]}}'
  ))
  valid <- gx_graph_fixture_raw("select-empty.min.json")
  query <- "SELECT ?x WHERE { VALUES ?x { 1 } } LIMIT 1"
  script <- gx_graph_test_script(list(gx_graph_test_response(valid)))
  dns_calls <- 0L
  throttle_calls <- 0L
  withr::local_options(list(
    geoconnexr.endpoint_graph = "https://graph.example/sparql",
    geoconnexr.performer = script$performer,
    geoconnexr.dns_resolver = function(host) {
      dns_calls <<- dns_calls + 1L
      gx_graph_test_public_dns(host)
    },
    geoconnexr.clock = gx_graph_test_clock,
    geoconnexr.throttle_clock = function() {
      throttle_calls <<- throttle_calls + 1L
      0
    }
  ))

  online_dir <- withr::local_tempdir()
  online <- gx_client(
    "graph", retries = 0L, min_interval = 0, cache = TRUE,
    cache_dir = online_dir
  )
  seeded_online <- gx_graph_test_seed_cache(online, query, malformed)
  expect_true(seeded_online$backend$exists(seeded_online$key))

  recovered <- gx_graph_test_execute(
    query,
    online,
    max_requests = 2L,
    max_total_bytes = length(malformed) + length(valid)
  )

  expect_identical(script$state$index, 1L)
  expect_false(recovered$response$from_cache)
  expect_identical(
    recovered$requests$cache_origin,
    c("fresh_cache", "network")
  )
  expect_identical(
    recovered$requests$bytes,
    as.integer(c(length(malformed), length(valid)))
  )
  expect_identical(nrow(recovered$requests), 2L)
  expect_identical(nrow(recovered$attempts), 1L)
  expect_true(recovered$attempts$physical)
  expect_true(seeded_online$backend$exists(seeded_online$key))
  expect_identical(
    seeded_online$backend$get(seeded_online$key)$response$body,
    valid
  )

  offline_dir <- withr::local_tempdir()
  offline <- gx_client(
    "graph", retries = 0L, min_interval = 0, cache = TRUE, offline = TRUE,
    cache_dir = offline_dir
  )
  seeded_offline <- gx_graph_test_seed_cache(offline, query, malformed)
  counters_before_offline <- c(
    performer = script$state$index,
    dns = dns_calls,
    throttle = throttle_calls
  )
  offline_error <- expect_error(
    gx_graph_test_execute(
      query,
      offline,
      max_requests = 1L,
      max_total_bytes = length(malformed)
    ),
    class = "gx_error_graph_payload"
  )

  expect_identical(nrow(offline_error$requests), 1L)
  expect_identical(offline_error$requests$cache_origin, "offline_cache")
  expect_identical(
    offline_error$requests$bytes,
    as.integer(length(malformed))
  )
  expect_identical(nrow(offline_error$attempts), 0L)
  expect_false(seeded_offline$backend$exists(seeded_offline$key))
  expect_identical(gx_cache_info(offline_dir)$entries, 0L)
  expect_identical(
    c(
      performer = script$state$index,
      dns = dns_calls,
      throttle = throttle_calls
    ),
    counters_before_offline
  )
})

test_that("expected result-type mismatches retain structurally valid cache", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  valid_select <- gx_graph_fixture_raw("select-empty.min.json")
  query <- "SELECT ?x WHERE { VALUES ?x { 1 } } LIMIT 1"
  script <- gx_graph_test_script(list())
  withr::local_options(list(
    geoconnexr.endpoint_graph = "https://graph.example/sparql",
    geoconnexr.performer = script$performer,
    geoconnexr.dns_resolver = gx_graph_test_public_dns,
    geoconnexr.clock = gx_graph_test_clock,
    geoconnexr.throttle_clock = function() 0
  ))
  cache_dir <- withr::local_tempdir()
  client <- gx_client(
    "graph", retries = 0L, min_interval = 0, cache = TRUE,
    cache_dir = cache_dir
  )
  seeded <- gx_graph_test_seed_cache(client, query, valid_select)

  mismatch <- expect_error(
    gx_graph_test_execute(
      query,
      client,
      expected = "ask",
      max_requests = 1L,
      max_total_bytes = length(valid_select)
    ),
    class = "gx_error_graph_expected_type"
  )

  expect_false(isTRUE(mismatch$cache_invalid))
  expect_identical(nrow(mismatch$requests), 1L)
  expect_identical(mismatch$requests$cache_origin, "fresh_cache")
  expect_identical(nrow(mismatch$attempts), 0L)
  expect_identical(mismatch$response$cache_origin, "fresh_cache")
  expect_true(seeded$backend$exists(seeded$key))
  expect_identical(gx_cache_info(cache_dir)$entries, 1L)
  expect_identical(script$state$index, 0L)

  retained <- gx_graph_test_execute(
    query,
    client,
    expected = "select",
    max_requests = 1L,
    max_total_bytes = length(valid_select)
  )
  expect_identical(retained$row_count, 0L)
  expect_true(retained$response$from_cache)
  expect_identical(retained$requests$cache_origin, "fresh_cache")
  expect_true(seeded$backend$exists(seeded$key))
  expect_identical(script$state$index, 0L)
})

test_that("malformed and wrong-media graph responses are evicted before reuse", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  malformed <- charToRaw(paste0(
    '{"head":{"vars":["x"]},"results":{"bindings":[',
    '{"x":{"type":"literal"}}]}}'
  ))
  valid <- gx_graph_fixture_raw("select-empty.min.json")
  script <- gx_graph_test_script(list(
    gx_graph_test_response(malformed),
    gx_graph_test_response(valid)
  ))
  withr::local_options(list(
    geoconnexr.endpoint_graph = "https://graph.example/sparql",
    geoconnexr.performer = script$performer,
    geoconnexr.dns_resolver = gx_graph_test_public_dns,
    geoconnexr.clock = gx_graph_test_clock,
    geoconnexr.throttle_clock = function() 0
  ))
  cache_dir <- withr::local_tempdir()
  client <- gx_client(
    "graph", retries = 0L, min_interval = 0, cache = TRUE,
    cache_dir = cache_dir
  )
  query <- "SELECT ?x WHERE { VALUES ?x { 1 } } LIMIT 1"

  malformed_error <- expect_error(
    gx_graph_test_execute(query, client, max_requests = 1L),
    class = "gx_error_graph_payload"
  )
  expect_identical(script$state$index, 1L)
  expect_identical(gx_cache_info(cache_dir)$entries, 0L)
  expect_identical(nrow(malformed_error$requests), 1L)
  expect_identical(nrow(malformed_error$attempts), 1L)
  expect_identical(
    malformed_error$response$body_sha256,
    digest::digest(malformed, algo = "sha256", serialize = FALSE)
  )

  recovered <- gx_graph_test_execute(query, client, max_requests = 1L)
  reused <- gx_graph_test_execute(query, client, max_requests = 1L)
  expect_identical(script$state$index, 2L)
  expect_identical(gx_cache_info(cache_dir)$entries, 1L)
  expect_false(recovered$response$from_cache)
  expect_true(reused$response$from_cache)
  expect_identical(reused$requests$cache_origin, "fresh_cache")

  wrong_media <- gx_graph_test_script(list(
    gx_graph_test_response(valid, content_type = "application/json"),
    gx_graph_test_response(valid)
  ))
  withr::local_options(geoconnexr.performer = wrong_media$performer)
  other_query <- paste0(query, " # wrong media")
  media_error <- expect_error(
    gx_graph_test_execute(other_query, client, max_requests = 1L),
    class = "gx_error_graph_content_type"
  )
  expect_identical(media_error$source_error, "content_type")
  expect_identical(media_error$requests$media_type, "application/json")
  expect_identical(gx_cache_info(cache_dir)$entries, 1L)
  media_recovered <- gx_graph_test_execute(
    other_query, client, max_requests = 1L
  )
  expect_identical(wrong_media$state$index, 2L)
  expect_false(media_recovered$response$from_cache)
  expect_identical(gx_cache_info(cache_dir)$entries, 2L)
})

test_that("graph errors redact endpoint, query, response, and transport secrets", {
  geoconnexr:::gx_http_throttle_reset()
  on.exit(geoconnexr:::gx_http_throttle_reset(), add = TRUE)
  endpoint_token <- "TOPSECRET-ENDPOINT"
  query_token <- "TOPSECRET-QUERY"
  body_token <- "TOPSECRET-BODY"
  transport_token <- "TOPSECRET-TRANSPORT"
  query <- paste0('ASK { BIND("', query_token, '" AS ?x) }')
  malformed <- charToRaw(paste0(
    '{"head":{},"boolean":"', body_token, '"}'
  ))
  script <- gx_graph_test_script(list(gx_graph_test_response(malformed)))
  withr::local_options(list(
    geoconnexr.endpoint_graph = paste0(
      "https://graph.example/sparql?token=", endpoint_token
    ),
    geoconnexr.performer = script$performer,
    geoconnexr.dns_resolver = gx_graph_test_public_dns,
    geoconnexr.clock = gx_graph_test_clock,
    geoconnexr.throttle_clock = function() 0
  ))
  client <- gx_client(
    "graph", retries = 0L, min_interval = 0, cache = FALSE,
    cache_dir = withr::local_tempdir()
  )

  payload_error <- expect_error(
    gx_graph_test_execute(
      query, client, expected = "ask", max_requests = 1L
    ),
    class = "gx_error_graph_payload"
  )
  expect_identical(
    payload_error$endpoint,
    "https://graph.example/sparql?[redacted]"
  )
  expect_identical(
    payload_error$requests$url,
    "https://graph.example/sparql?[redacted]"
  )
  expect_identical(
    payload_error$attempts$url,
    "https://graph.example/sparql?[redacted]"
  )
  rendered <- paste(
    c(
      conditionMessage(payload_error),
      format(payload_error),
      capture.output(str(payload_error, max.level = 3L))
    ),
    collapse = "\n"
  )
  for (token in c(endpoint_token, query_token, body_token)) {
    expect_false(grepl(token, rendered, fixed = TRUE))
  }

  withr::local_options(
    geoconnexr.performer = function(request) {
      stop(transport_token, call. = FALSE)
    }
  )
  transport_error <- expect_error(
    gx_graph_test_execute(
      query, client, expected = "ask", max_requests = 1L
    ),
    class = "gx_error_graph_transport"
  )
  transport_rendered <- paste(
    c(
      conditionMessage(transport_error),
      format(transport_error),
      capture.output(str(transport_error, max.level = 3L))
    ),
    collapse = "\n"
  )
  for (token in c(endpoint_token, query_token, transport_token)) {
    expect_false(grepl(token, transport_rendered, fixed = TRUE))
  }
})
