test_that("M7f produces an exact character-only parsed response contract", {
  response <- csv_parse_test_response()
  result <- gx_csv_parsed_response_impl(response, max_fields = 1000L)

  expect_s3_class(result, "gx_csv_parsed_response", exact = TRUE)
  expect_identical(
    names(result),
    c(
      "contract_version", "validated_response", "policy", "schema", "data",
      "parse", "metadata"
    )
  )
  expect_identical(result$contract_version, "0.1.0")
  expect_identical(result$validated_response, response)
  expect_identical(
    names(result$policy),
    .gx_csv_parsed_response_policy_fields
  )
  expect_identical(
    names(result$schema),
    c("contract_version", "column_index", "column_name", "storage_type")
  )
  expect_identical(
    result$schema$column_name, c("station", "value")
  )
  expect_identical(result$schema$storage_type, rep("character", 2L))
  expect_identical(result$data$station, "00123")
  expect_identical(result$data$value, "4.5")
  expect_identical(
    names(result$parse), .gx_csv_parsed_response_parse_fields
  )
  expect_identical(result$parse$row_count, 1L)
  expect_identical(result$parse$column_count, 2L)
  expect_identical(result$parse$field_count, 4L)
  expect_match(result$parse$parse_id, "^[a-f0-9]{64}$")
  expect_match(result$parse$result_sha256, "^[a-f0-9]{64}$")
  expect_identical(
    result$parse$validation_id, response$validation$validation_id
  )
  expect_identical(result$parse$body_sha256, response$validation$body_sha256)
  expect_identical(
    names(result$metadata), .gx_csv_parsed_response_metadata_fields
  )
  expect_false(result$metadata$host_specific)
  expect_false(result$metadata$replayable)
  expect_false(result$metadata$execution_ready)
  expect_false(result$metadata$transport_authorized)
  expect_true(result$metadata$response_candidate_validated)
  expect_false(result$metadata$provider_response_observed)
  expect_false(result$metadata$budgets_consumed)
  expect_true(result$metadata$parser_executed)
  expect_true(result$metadata$csv_semantics_validated)
  expect_true(result$metadata$result_contract_bound)
  expect_identical(result$metadata$observation_origin, "caller_supplied")
  expect_false(any(c(
    "csv_parser_enforcement_unimplemented",
    "csv_parser_semantics_unbound",
    "result_schema_unbound"
  ) %in% result$metadata$non_replayable_reasons))
  expect_true(all(c(
    "attempt_identity_unbound", "attempt_ledger_unbound",
    "response_origin_unbound", "runtime_package_preflight_required",
    "provider_transport_unauthorized", "serialization_unbound"
  ) %in% result$metadata$non_replayable_reasons))
  expect_invisible(gx_csv_parsed_response_validate_impl(result))
})

test_that("M7f known-answer fixtures and M7e dependencies stay pinned", {
  fixture_dir <- csv_parse_test_fixture_dir()
  pinned <- list(
    "manifest-v1.json" = list(
      bytes = 1694L,
      sha256 = "1abd74a3ef22b773a737b8137a0f4680929b95d2bfa61f3a35cbc063ad4292ef"
    ),
    "cases-v1.json" = list(
      bytes = 1042L,
      sha256 = "6d4c89dae63b5174e0d4bc0163ee446588967d19890353ae5533ca43b6910bdd"
    ),
    "expected-v1.json" = list(
      bytes = 2605L,
      sha256 = "9616bc38ea73be8eb65d7a36c1719c9156bddbfd3b6c0ad825ac31c31986667d"
    )
  )
  for (name in names(pinned)) {
    path <- file.path(fixture_dir, name)
    raw <- readBin(path, "raw", n = pinned[[name]]$bytes + 1L)
    expect_identical(length(raw), pinned[[name]]$bytes)
    expect_identical(
      digest::digest(raw, algo = "sha256", serialize = FALSE),
      pinned[[name]]$sha256
    )
    expect_identical(tail(raw, 1L), as.raw(0x0a))
  }

  manifest <- csv_parse_test_read_json("manifest-v1.json")
  cases <- csv_parse_test_read_json("cases-v1.json")
  expected <- csv_parse_test_read_json("expected-v1.json")
  expect_identical(manifest$manifest_version, "1.0.0")
  expect_identical(manifest$contract, "gx_csv_parsed_response")
  expect_identical(manifest$contract_version, "0.1.0")
  expect_identical(length(cases$cases), 1L)
  expect_identical(
    cases$cases[[1L]]$response_case,
    "../csv-response-validation/cases-v1.json#accepted_text_csv_identity"
  )
  expect_identical(
    cases$cases[[1L]]$body_path,
    "../csv-response-validation/body-accepted-v1.csv"
  )
  for (record in manifest$files) {
    pin <- pinned[[record$path]]
    expect_identical(as.integer(record$bytes), pin$bytes)
    expect_identical(record$sha256, pin$sha256)
  }

  dependency_dir <- file.path(fixture_dir, "..", "csv-response-validation")
  for (record in manifest$source_dependencies) {
    path <- file.path(dependency_dir, basename(record$path))
    raw <- readBin(path, "raw", n = as.integer(record$bytes) + 1L)
    expect_identical(length(raw), as.integer(record$bytes))
    expect_identical(
      digest::digest(raw, algo = "sha256", serialize = FALSE),
      record$sha256
    )
  }

  response_cases <- jsonlite::fromJSON(
    file.path(dependency_dir, "cases-v1.json"),
    simplifyVector = FALSE
  )
  response_case <- response_cases$cases[[1L]]
  header_names <- vapply(
    response_case$headers, `[[`, character(1), "name"
  )
  header_values <- vapply(
    response_case$headers, `[[`, character(1), "value"
  )
  headers <- as.list(header_values)
  names(headers) <- header_names
  body <- readBin(
    file.path(dependency_dir, response_case$body_path),
    "raw", n = 1024L
  )
  plan <- csv_response_validation_test_plan()
  response <- csv_response_validation_test_build(
    plan,
    request_order = as.integer(response_case$logical_request_order),
    candidate = list(
      status = as.integer(response_case$status),
      headers = headers,
      body = body,
      url = response_case$url
    )
  )
  result <- gx_csv_parsed_response_impl(
    response,
    max_fields = as.integer(cases$cases[[1L]]$max_fields)
  )

  expected_policy <- expected$policy
  expected_policy$record_terminators <- as.character(unlist(
    expected_policy$record_terminators, use.names = FALSE
  ))
  integer_policy <- c(
    "max_input_bytes", "max_field_bytes", "max_header_name_bytes",
    "max_header_bytes", "max_fields", "request_max_rows",
    "request_max_columns", "implementation_max_rows",
    "implementation_max_columns", "hash_chunk_fields"
  )
  expected_policy[integer_policy] <- lapply(
    expected_policy[integer_policy], as.integer
  )
  expected_policy$trim_whitespace <- as.logical(
    expected_policy$trim_whitespace
  )
  expect_identical(result$policy, expected_policy)

  for (position in seq_along(expected$schema)) {
    row <- expected$schema[[position]]
    row$column_index <- as.integer(row$column_index)
    expect_identical(
      as.list(result$schema[position, , drop = FALSE]), row
    )
  }
  expect_identical(result$data$station, expected$data$station)
  expect_identical(result$data$value, expected$data$value)
  integer_parse <- c("row_count", "column_count", "field_count")
  expected$parse[integer_parse] <- lapply(
    expected$parse[integer_parse], as.integer
  )
  expected$parse$bom_present <- as.logical(expected$parse$bom_present)
  expect_identical(result$parse, expected$parse)
  expected$metadata$non_replayable_reasons <- as.character(unlist(
    expected$metadata$non_replayable_reasons, use.names = FALSE
  ))
  expect_identical(result$metadata, expected$metadata)
})

test_that("M7f freezes strict lexical and literal-value semantics", {
  body <- charToRaw(paste0(
    "\"station id\",value,note,missing,token\r\n",
    "\"00123\",\"4,500\",\"a\"\"b\",,NA\n",
    "\" 00456 \" , 7 ,#literal,\"\",001\n"
  ))
  expect_error(
    csv_parse_test_build(body),
    class = "gx_error_csv_parse_syntax"
  )

  body <- charToRaw(paste0(
    "\"station id\",value,note,missing,token\r\n",
    "\"00123\",\"4,500\",\"a\"\"b\",,NA\n",
    " 00456 , 7 ,#literal,\"\",001\n"
  ))
  result <- csv_parse_test_build(body)
  expect_identical(
    result$data[["station id"]], c("00123", " 00456 ")
  )
  expect_identical(result$data$value, c("4,500", " 7 "))
  expect_identical(result$data$note, c("a\"b", "#literal"))
  expect_identical(result$data$missing, c("", ""))
  expect_identical(result$data$token, c("NA", "001"))
  expect_false(anyNA(result$data))
  expect_true(all(vapply(result$data, is.character, logical(1))))
})

test_that("M7f admits only the fixed BOM and record profile", {
  bom <- as.raw(c(239L, 187L, 191L))
  result <- csv_parse_test_build(c(bom, charToRaw("a,b\r\n1,2\n")))
  expect_true(result$parse$bom_present)
  expect_identical(result$data$a, "1")

  no_bom <- csv_parse_test_build(charToRaw("a,b"))
  expect_false(no_bom$parse$bom_present)
  expect_identical(nrow(no_bom$data), 0L)

  for (body in list(
    raw(),
    bom,
    c(charToRaw("a,"), bom, charToRaw("b")),
    c(bom, bom, charToRaw("a")),
    charToRaw("a\r1"),
    charToRaw("a\r"),
    charToRaw("a\n\nb"),
    charToRaw("a\n\n"),
    charToRaw("a\n\"x\ny\"")
  )) {
    expect_error(
      csv_parse_test_build(body),
      class = "gx_error_csv_parse"
    )
  }
})

test_that("M7f preserves trailing and quoted empty cells but rejects empty headers", {
  trailing <- csv_parse_test_build(charToRaw("a,b\n1,"))
  expect_identical(trailing$data$a, "1")
  expect_identical(trailing$data$b, "")

  quoted <- csv_parse_test_build(charToRaw("h\n\"\""))
  expect_identical(quoted$data$h, "")
  expect_false(anyNA(quoted$data$h))

  for (body in list(
    charToRaw(",b\n1,2"),
    charToRaw("\"\",b\n1,2"),
    charToRaw("a,\"a\"\n1,2")
  )) {
    expect_error(
      csv_parse_test_build(body),
      class = "gx_error_csv_parse_header"
    )
  }
})

test_that("M7f rejects malformed quotes and nonrectangular records", {
  bodies <- c(
    "a,b\n1,2,3",
    "a,b\n1",
    "a,b\na\"b,2",
    "a,b\n\"a\"x,2",
    "a,b\n\"a\" ,2",
    "a,b\n\"a,2",
    "h\n\"\"\""
  )
  for (body in bodies) {
    expect_error(
      csv_parse_test_build(charToRaw(body)),
      class = "gx_error_csv_parse"
    )
  }
})

test_that("M7f rejects invalid UTF-8 and control values without warnings", {
  invalid <- list(
    csv_parse_test_raw(97L, 10L, 192L, 175L),
    csv_parse_test_raw(97L, 10L, 237L, 160L, 128L),
    csv_parse_test_raw(97L, 10L, 244L, 144L, 128L, 128L),
    csv_parse_test_raw(97L, 10L, 226L, 130L),
    csv_parse_test_raw(97L, 10L, 0L),
    csv_parse_test_raw(97L, 10L, 9L),
    c(charToRaw("a\nx"), as.raw(c(226L, 128L, 139L)))
  )
  for (body in invalid) {
    expect_warning(
      expect_error(
        csv_parse_test_build(body),
        class = "gx_error_csv_parse_encoding"
      ),
      NA
    )
  }

  utf8 <- csv_parse_test_build(charToRaw(enc2utf8("café,δ\nnaïve,水")))
  expect_identical(utf8$data[["café"]], enc2utf8("naïve"))
  expect_identical(utf8$data[["δ"]], enc2utf8("水"))

  portable <- csv_parse_test_build(
    charToRaw(enc2utf8("café,débit\nnaïve,001"))
  )
  expect_identical(
    portable$parse$parse_id,
    "772d1041d68cfec1d26a288d76bd3834343bbf802972f0e1960bc1a764a62dc9"
  )
  expect_identical(
    portable$parse$result_sha256,
    "6734923185aa4314a664077fe87045a6a67a1006c0d807f4140a38328a6c68cc"
  )
})

test_that("M7f enforces rows, columns, total fields, and scalar bytes", {
  expect_error(
    csv_parse_test_build(
      charToRaw("a\n1\n2"), max_rows = 1L
    ),
    class = "gx_error_csv_parse_budget"
  )
  expect_error(
    csv_parse_test_build(
      charToRaw("a,b\n1,2"), max_columns = 1L
    ),
    class = "gx_error_csv_parse_budget"
  )
  expect_error(
    csv_parse_test_build(
      charToRaw("a,b\n1,2"), max_fields = 3L
    ),
    class = "gx_error_csv_parse_budget"
  )
  exact <- csv_parse_test_build(
    charToRaw("a,b\n1,2"), max_rows = 1L, max_columns = 2L,
    max_fields = 4L
  )
  expect_identical(exact$parse$field_count, 4L)

  exact_scalar <- testthat::with_mocked_bindings(
    csv_parse_test_build(charToRaw("h\n12345")),
    .gx_csv_parsed_response_max_field_bytes = 5L,
    .package = "geoconnexr"
  )
  expect_identical(exact_scalar$data$h, "12345")
  expect_error(
    testthat::with_mocked_bindings(
      csv_parse_test_build(charToRaw("h\n123456")),
      .gx_csv_parsed_response_max_field_bytes = 5L,
      .package = "geoconnexr"
    ),
    class = "gx_error_csv_parse_budget"
  )
  for (value in list(NULL, 0L, 1.5, Inf, NA_real_, "4", 1000001L)) {
    expect_error(
      gx_csv_parsed_response_impl(csv_parse_test_response(), value),
      class = "gx_error_csv_parse_budget"
    )
  }
})

test_that("M7f enforces exact input and header byte boundaries", {
  exact_input <- testthat::with_mocked_bindings(
    csv_parse_test_build(charToRaw("h\n123456")),
    .gx_csv_parsed_response_max_input_bytes = 8L,
    .package = "geoconnexr"
  )
  expect_identical(exact_input$data$h, "123456")
  expect_error(
    testthat::with_mocked_bindings(
      csv_parse_test_build(charToRaw("h\n1234567")),
      .gx_csv_parsed_response_max_input_bytes = 8L,
      .package = "geoconnexr"
    ),
    class = "gx_error_csv_parse_budget"
  )

  exact_header <- testthat::with_mocked_bindings(
    csv_parse_test_build(charToRaw("abc,de\n1,2")),
    .gx_csv_parsed_response_max_header_name_bytes = 3L,
    .gx_csv_parsed_response_max_header_bytes = 5L,
    .package = "geoconnexr"
  )
  expect_identical(names(exact_header$data), c("abc", "de"))
  expect_error(
    testthat::with_mocked_bindings(
      csv_parse_test_build(charToRaw("abcd,e\n1,2")),
      .gx_csv_parsed_response_max_header_name_bytes = 3L,
      .gx_csv_parsed_response_max_header_bytes = 5L,
      .package = "geoconnexr"
    ),
    class = "gx_error_csv_parse_header"
  )
  expect_error(
    testthat::with_mocked_bindings(
      csv_parse_test_build(charToRaw("abc,def\n1,2")),
      .gx_csv_parsed_response_max_header_name_bytes = 3L,
      .gx_csv_parsed_response_max_header_bytes = 5L,
      .package = "geoconnexr"
    ),
    class = "gx_error_csv_parse_header"
  )
})

test_that("M7f identities bind exact data, policy, BOM, and dimensions", {
  plain <- csv_parse_test_build(charToRaw("a\n1"))
  quoted <- csv_parse_test_build(charToRaw("a\n\"1\""))
  bom <- csv_parse_test_build(c(
    as.raw(c(239L, 187L, 191L)), charToRaw("a\n1")
  ))
  more_fields <- csv_parse_test_build(
    charToRaw("a\n1"), max_fields = 20L
  )

  expect_identical(plain$data, quoted$data)
  expect_identical(plain$parse$result_sha256, quoted$parse$result_sha256)
  expect_false(identical(plain$parse$parse_id, quoted$parse$parse_id))
  expect_false(identical(plain$parse$parse_id, bom$parse$parse_id))
  expect_false(identical(plain$parse$parse_id, more_fields$parse$parse_id))

  withr::local_options(list(scipen = 999))
  repeated <- csv_parse_test_build(charToRaw("a\n1"))
  expect_identical(repeated$parse, plain$parse)
  negative <- withr::with_options(
    list(scipen = -9),
    csv_parse_test_build(charToRaw("a\n1"))
  )
  expect_identical(negative$parse, plain$parse)
})

test_that("M7f whole-object validation rejects forged nested and owned facts", {
  result <- csv_parse_test_build()
  mutations <- list(
    function(x) { x$contract_version <- "9.9.9"; x },
    function(x) { x$validated_response$body[[1L]] <- as.raw(88L); x },
    function(x) { x$policy$delimiter <- ";"; x },
    function(x) { x$schema$column_name[[1L]] <- "forged"; x },
    function(x) { x$data[[1L]][[1L]] <- "forged"; x },
    function(x) { x$parse$result_sha256 <- strrep("0", 64L); x },
    function(x) { x$parse$row_count <- 2L; x },
    function(x) { x$metadata$parser_executed <- FALSE; x },
    function(x) { x$metadata$provider_response_observed <- TRUE; x },
    function(x) {
      x$metadata$non_replayable_reasons <- character()
      x
    },
    function(x) { attr(x$data[[1L]], "forged") <- TRUE; x },
    function(x) { class(x) <- c("gx_csv_parsed_response", "forged"); x }
  )
  for (mutate in mutations) {
    forged <- mutate(csv_parse_test_clone(result))
    expect_error(
      gx_csv_parsed_response_validate_impl(forged),
      class = "gx_error_csv_parse"
    )
  }
})

test_that("M7f prints only bounded redacted facts", {
  result <- csv_parse_test_build()
  output <- capture.output(print(result))
  expect_match(output[[1L]], "<gx_csv_parsed_response>", fixed = TRUE)
  expect_true(any(grepl("rows: 1", output, fixed = TRUE)))
  expect_true(any(grepl("columns: 2", output, fixed = TRUE)))
  expect_false(any(grepl("00123", output, fixed = TRUE)))
  expect_false(any(grepl("https://", output, fixed = TRUE)))
})

test_that("M7f remains offline and independent of optional parser packages", {
  before <- vapply(
    c("readr", "vroom"),
    function(package) package %in% loadedNamespaces(),
    logical(1)
  )
  old <- options(
    geoconnexr.performer = function(...) stop("performer called"),
    geoconnexr.dns_resolver = function(...) stop("DNS called")
  )
  on.exit(options(old), add = TRUE)

  result <- csv_parse_test_build()
  after <- vapply(
    c("readr", "vroom"),
    function(package) package %in% loadedNamespaces(),
    logical(1)
  )
  expect_identical(after, before)
  expect_false(result$metadata$host_specific)
  expect_false(result$metadata$budgets_consumed)
  expect_false(result$metadata$provider_response_observed)
})

test_that("M7f constructor and parser contract remain internal", {
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(c(
    "gx_csv_parsed_response_impl",
    "gx_csv_parsed_response_validate_impl",
    "gx_csv_parsed_response"
  ) %in% exports))
})
