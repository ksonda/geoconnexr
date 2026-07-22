test_that("M7e validates one caller-supplied response with an exact contract", {
  request_plan <- csv_response_validation_test_plan()
  plan_bytes <- serialize(request_plan, NULL)
  body <- csv_response_validation_test_body()
  result <- csv_response_validation_test_build(request_plan)

  expect_identical(class(result), "gx_csv_validated_response")
  expect_identical(names(result), c(
    "contract_version", "request_plan", "body", "validation", "metadata"
  ))
  expect_identical(result$contract_version, "0.1.0")
  expect_identical(serialize(request_plan, NULL), plan_bytes)
  expect_identical(serialize(result$request_plan, NULL), plan_bytes)
  expect_identical(result$body, body)
  expect_null(attributes(result$body))

  expect_identical(names(result$validation), c(
    "validation_id", "logical_request_id", "intent_id", "reservation_id",
    "distribution_id", "status", "media_type", "content_encoding",
    "content_length_present", "content_length", "encoded_bytes",
    "decoded_bytes", "body_sha256", "validation_status"
  ))
  request <- request_plan$request_plans[1L, , drop = FALSE]
  expect_identical(result$validation$logical_request_id,
                   request$logical_request_id[[1L]])
  expect_identical(result$validation$intent_id, request$intent_id[[1L]])
  expect_identical(
    result$validation$reservation_id, request$reservation_id[[1L]]
  )
  expect_identical(
    result$validation$distribution_id, request$distribution_id[[1L]]
  )
  expect_identical(result$validation$status, 200L)
  expect_identical(result$validation$media_type, "text/csv")
  expect_identical(result$validation$content_encoding, "identity")
  expect_true(result$validation$content_length_present)
  expect_identical(result$validation$content_length, as.integer(length(body)))
  expect_identical(result$validation$encoded_bytes, as.integer(length(body)))
  expect_identical(result$validation$decoded_bytes, as.integer(length(body)))
  expect_identical(
    result$validation$body_sha256,
    digest::digest(body, algo = "sha256", serialize = FALSE)
  )
  expect_match(result$validation$validation_id, "^[a-f0-9]{64}$")
  expect_identical(
    result$validation$validation_status, "validated_caller_supplied"
  )

  expect_identical(names(result$metadata), c(
    "host_specific", "replayable", "execution_ready", "transport_authorized",
    "response_candidate_validated", "provider_response_observed",
    "budgets_consumed", "parser_executed", "observation_origin",
    "non_replayable_reasons"
  ))
  expect_identical(result$metadata$host_specific, FALSE)
  expect_identical(result$metadata$replayable, FALSE)
  expect_identical(result$metadata$execution_ready, FALSE)
  expect_identical(result$metadata$transport_authorized, FALSE)
  expect_identical(result$metadata$response_candidate_validated, TRUE)
  expect_identical(result$metadata$provider_response_observed, FALSE)
  expect_identical(result$metadata$budgets_consumed, FALSE)
  expect_identical(result$metadata$parser_executed, FALSE)
  expect_identical(result$metadata$observation_origin, "caller_supplied")
  expect_identical(result$metadata$non_replayable_reasons, c(
    "arbitrary_provider_client_unimplemented",
    "attempt_identity_unbound",
    "attempt_ledger_unbound",
    "csv_parser_enforcement_unimplemented",
    "csv_parser_semantics_unbound",
    "handler_implementations_planned",
    "non_csv_request_plans_absent",
    "provider_transport_unauthorized",
    "response_origin_unbound",
    "result_schema_unbound",
    "runtime_package_preflight_required",
    "serialization_unbound",
    "timeout_policy_unbound",
    "transport_adapter_unimplemented"
  ))
  expect_false(
    "response_validator_unimplemented" %in%
      result$metadata$non_replayable_reasons
  )
  expect_true(
    "response_validator_unimplemented" %in%
      result$request_plan$metadata$non_replayable_reasons
  )
  expect_identical(
    gx_csv_validated_response_validate_impl(result), invisible(result)
  )
})

test_that("M7e known-answer fixtures and recursive dependencies stay pinned", {
  fixture_dir <- csv_response_validation_test_fixture_dir()
  pinned <- list(
    "manifest-v1.json" = list(
      bytes = 1368L,
      sha256 = "2d329bb780cae838f3c5db70f759903bfeb6e6a84f4ace30a797ac07780a0771"
    ),
    "body-accepted-v1.csv" = list(
      bytes = 24L,
      sha256 = "e1e4bae6de933a5219206b4c76b19eda0d00c59930d334a2df5d7b3402af651f"
    ),
    "cases-v1.json" = list(
      bytes = 1396L,
      sha256 = "4301fefa69850844f94ac4f3d29a28eb55b9173fa5879375c4abb46bd96a39e7"
    ),
    "expected-v1.json" = list(
      bytes = 1649L,
      sha256 = "8ddd1fdaa5a72d3866d2ae0e6ffff14c294d5bd54692d415f1fee7597436944c"
    )
  )
  for (name in names(pinned)) {
    path <- file.path(fixture_dir, name)
    raw <- readBin(path, what = "raw", n = pinned[[name]]$bytes + 1L)
    expect_identical(length(raw), pinned[[name]]$bytes)
    expect_identical(
      digest::digest(raw, algo = "sha256", serialize = FALSE),
      pinned[[name]]$sha256
    )
    expect_identical(tail(raw, 1L), as.raw(0x0a))
  }

  manifest <- csv_response_validation_test_read_json("manifest-v1.json")
  cases <- csv_response_validation_test_read_json("cases-v1.json")
  expected <- csv_response_validation_test_read_json("expected-v1.json")
  expect_identical(manifest$manifest_version, "1.0.0")
  expect_identical(manifest$contract, "gx_csv_validated_response")
  expect_identical(manifest$contract_version, "0.1.0")
  expect_identical(length(cases$cases), 1L)

  for (position in seq_along(manifest$files)) {
    record <- manifest$files[[position]]
    pin <- pinned[[record$path]]
    expect_identical(as.integer(record$bytes), pin$bytes)
    expect_identical(record$sha256, pin$sha256)
  }
  dependency_dir <- file.path(fixture_dir, "..", "csv-request-plan")
  dependency_names <- c("manifest-v1.json", "expected-v1.json")
  dependency_bytes <- c(939L, 6457L)
  dependency_hashes <- c(
    "dd1a7b5bad781a428e259aba7606de28c849936ae3c192ea647b565dd05985a8",
    "082f6b409d8287c679b315a32b59f2fe075b64d29a07e2ee5e008d096c12eb4f"
  )
  for (position in seq_along(dependency_names)) {
    raw <- readBin(
      file.path(dependency_dir, dependency_names[[position]]),
      what = "raw", n = dependency_bytes[[position]] + 1L
    )
    expect_identical(length(raw), dependency_bytes[[position]])
    expect_identical(
      digest::digest(raw, algo = "sha256", serialize = FALSE),
      dependency_hashes[[position]]
    )
    expect_identical(
      manifest$source_dependencies[[position]]$sha256,
      dependency_hashes[[position]]
    )
  }

  case <- cases$cases[[1L]]
  header_names <- vapply(case$headers, `[[`, character(1), "name")
  header_values <- vapply(case$headers, `[[`, character(1), "value")
  headers <- as.list(header_values)
  names(headers) <- header_names
  body_path <- file.path(fixture_dir, case$body_path)
  body <- readBin(body_path, what = "raw", n = 1024L)
  plan <- csv_response_validation_test_plan()
  candidate <- list(
    status = as.integer(case$status),
    headers = headers,
    body = body,
    url = case$url
  )
  result <- csv_response_validation_test_build(
    plan,
    request_order = as.integer(case$logical_request_order),
    candidate = candidate
  )
  expect_identical(result$contract_version, expected$contract_version)
  integer_fields <- c(
    "status", "content_length", "encoded_bytes", "decoded_bytes"
  )
  logical_fields <- "content_length_present"
  for (name in names(expected$validation)) {
    value <- expected$validation[[name]]
    if (name %in% integer_fields) value <- as.integer(value)
    if (name %in% logical_fields) value <- as.logical(value)
    expect_identical(result$validation[[name]], value)
  }
  expected_reasons <- as.character(unlist(
    expected$metadata$non_replayable_reasons, use.names = FALSE
  ))
  expect_identical(
    result$metadata$non_replayable_reasons, expected_reasons
  )
  expect_identical(result$metadata$observation_origin,
                   expected$metadata$observation_origin)
  expect_identical(
    result$validation$body_sha256,
    digest::digest(body, algo = "sha256", serialize = FALSE)
  )
  expect_false(any(grepl(
    "fixture-token",
    csv_response_validation_test_owned_text(result),
    fixed = TRUE
  )))
})

test_that("M7e accepts exact response-media and identity-encoding variants", {
  plan <- csv_response_validation_test_plan()
  baseline <- csv_response_validation_test_build(plan)
  uppercase <- csv_response_validation_test_build(
    plan,
    candidate = csv_response_validation_test_candidate(
      plan,
      headers = c(
        "x-irrelevant" = "retained nowhere",
        "CONTENT-LENGTH" = as.character(
          length(csv_response_validation_test_body())
        ),
        "content-type" = " TEXT/CSV ; charset=UTF-8 ",
        "Content-Encoding" = " IDENTITY "
      )
    )
  )
  absent_encoding <- csv_response_validation_test_build(
    plan,
    candidate = csv_response_validation_test_candidate(
      plan, content_encoding = NULL
    )
  )
  application <- csv_response_validation_test_build(
    plan,
    candidate = csv_response_validation_test_candidate(
      plan, content_type = "Application/CSV; profile=fixture"
    )
  )

  expect_identical(uppercase$validation, baseline$validation)
  expect_identical(absent_encoding$validation, baseline$validation)
  expect_identical(application$validation$media_type, "application/csv")
  expect_false(identical(
    application$validation$validation_id,
    baseline$validation$validation_id
  ))
  expect_false(any(c("headers", "url") %in% names(uppercase)))
  expect_false(any(c(
    "content_type", "canonical_url", "canonical_url_redacted"
  ) %in% names(uppercase$validation)))
})

test_that("M7e treats Content-Length presence as a validated identity fact", {
  plan <- csv_response_validation_test_plan()
  body <- csv_response_validation_test_body()
  present <- csv_response_validation_test_build(plan)
  leading_zero <- csv_response_validation_test_build(
    plan,
    candidate = csv_response_validation_test_candidate(
      plan, content_length = paste0("  000", length(body), "  ")
    )
  )
  absent <- csv_response_validation_test_build(
    plan,
    candidate = csv_response_validation_test_candidate(
      plan, content_length = NULL
    )
  )

  expect_identical(leading_zero$validation, present$validation)
  expect_false(absent$validation$content_length_present)
  expect_identical(absent$validation$content_length, NA_integer_)
  expect_false(identical(
    absent$validation$validation_id, present$validation$validation_id
  ))
})

test_that("M7e accepts empty and exact-limit opaque raw bodies", {
  empty_plan <- csv_response_validation_test_plan(max_response_bytes = 1L)
  empty <- csv_response_validation_test_build(
    empty_plan,
    candidate = csv_response_validation_test_candidate(
      empty_plan, body = raw(), content_length = "0"
    )
  )
  expect_identical(empty$body, raw())
  expect_identical(empty$validation$encoded_bytes, 0L)
  expect_identical(empty$validation$decoded_bytes, 0L)

  limit_plan <- csv_response_validation_test_plan(max_response_bytes = 3L)
  at_limit <- csv_response_validation_test_build(
    limit_plan,
    candidate = csv_response_validation_test_candidate(
      limit_plan, body = as.raw(c(0xff, 0x00, 0x80)), content_length = "3"
    )
  )
  expect_identical(at_limit$validation$encoded_bytes, 3L)
  expect_identical(at_limit$body, as.raw(c(0xff, 0x00, 0x80)))

  condition <- csv_response_validation_test_error(
    csv_response_validation_test_build(
      limit_plan,
      candidate = csv_response_validation_test_candidate(
        limit_plan, body = as.raw(1:4), content_length = "4"
      )
    )
  )
  expect_s3_class(
    condition, "gx_error_csv_response_validation_payload_too_large"
  )
  expect_s3_class(condition, "gx_error_payload_too_large")
})

test_that("M7e enforces independently allocated encoded and decoded ceilings", {
  encoded_tight_intents <- csv_request_plan_test_intent_set(
    max_requests = 1L,
    max_encoded_bytes = 2,
    max_decoded_bytes = 9
  )
  encoded_tight <- csv_response_validation_test_plan(
    encoded_tight_intents, max_response_bytes = 9L
  )
  expect_identical(encoded_tight$request_plans$max_encoded_bytes, 2)
  expect_identical(encoded_tight$request_plans$max_decoded_bytes, 9)
  expect_silent(csv_response_validation_test_build(
    encoded_tight,
    candidate = csv_response_validation_test_candidate(
      encoded_tight, body = as.raw(1:2), content_length = "2"
    )
  ))
  expect_s3_class(
    csv_response_validation_test_error(
      csv_response_validation_test_build(
        encoded_tight,
        candidate = csv_response_validation_test_candidate(
          encoded_tight, body = as.raw(1:3), content_length = "3"
        )
      )
    ),
    "gx_error_csv_response_validation_payload_too_large"
  )

  decoded_tight_intents <- csv_request_plan_test_intent_set(
    max_requests = 1L,
    max_encoded_bytes = 9,
    max_decoded_bytes = 2
  )
  decoded_tight <- csv_response_validation_test_plan(
    decoded_tight_intents, max_response_bytes = 9L
  )
  expect_identical(decoded_tight$request_plans$max_encoded_bytes, 9)
  expect_identical(decoded_tight$request_plans$max_decoded_bytes, 2)
  expect_s3_class(
    csv_response_validation_test_error(
      csv_response_validation_test_build(
        decoded_tight,
        candidate = csv_response_validation_test_candidate(
          decoded_tight, body = as.raw(1:3), content_length = "3"
        )
      )
    ),
    "gx_error_csv_response_validation_payload_too_large"
  )
})

test_that("M7e requires status 200 and the exact candidate input shape", {
  plan <- csv_response_validation_test_plan()
  for (status in c(199, 201, 204, 206, 301, 304, 404, 500)) {
    condition <- csv_response_validation_test_error(
      csv_response_validation_test_build(
        plan,
        candidate = csv_response_validation_test_candidate(
          plan, status = status
        )
      )
    )
    expect_s3_class(
      condition, "gx_error_csv_response_validation_status"
    )
  }
  for (status in list(NULL, NA_integer_, Inf, 200.5, "200", c(200L, 200L))) {
    candidate <- csv_response_validation_test_candidate(plan)
    candidate["status"] <- list(status)
    condition <- csv_response_validation_test_error(
      csv_response_validation_test_build(plan, candidate = candidate)
    )
    expect_s3_class(
      condition, "gx_error_csv_response_validation_status"
    )
  }

  candidate <- csv_response_validation_test_candidate(plan)
  expect_s3_class(
    csv_response_validation_test_error(
      gx_csv_validated_response_impl(
        plan, plan$request_plans$logical_request_id[[1L]],
        candidate[c("headers", "status", "body", "url")]
      )
    ),
    "gx_error_csv_response_validation_input"
  )
  candidate$extra <- TRUE
  expect_s3_class(
    csv_response_validation_test_error(
      gx_csv_validated_response_impl(
        plan, plan$request_plans$logical_request_id[[1L]], candidate
      )
    ),
    "gx_error_csv_response_validation_input"
  )
})

test_that("M7e rejects missing, unsupported, and ambiguous media headers", {
  plan <- csv_response_validation_test_plan()
  cases <- list(
    list(`Content-Length` = "25"),
    list(`Content-Type` = ""),
    list(`Content-Type` = "application/octet-stream"),
    list(`Content-Type` = "text/plain; charset=utf-8"),
    list(`Content-Type` = "text/csv, application/csv"),
    list(`Content-Type` = "text/csv; charset=utf-8, application/json"),
    structure(
      list("text/csv", "text/csv"),
      names = c("Content-Type", "content-type")
    )
  )
  for (headers in cases) {
    candidate <- csv_response_validation_test_candidate(plan, headers = headers)
    condition <- csv_response_validation_test_error(
      csv_response_validation_test_build(plan, candidate = candidate)
    )
    expect_s3_class(
      condition, "gx_error_csv_response_validation_content_type"
    )
    expect_s3_class(condition, "gx_error_content_type")
  }
})

test_that("M7e rejects nonidentity, empty, and ambiguous encodings", {
  plan <- csv_response_validation_test_plan()
  for (encoding in c("", "gzip", "br", "deflate", "identity, identity")) {
    condition <- csv_response_validation_test_error(
      csv_response_validation_test_build(
        plan,
        candidate = csv_response_validation_test_candidate(
          plan, content_encoding = encoding
        )
      )
    )
    expect_s3_class(
      condition, "gx_error_csv_response_validation_content_encoding"
    )
  }
  headers <- structure(
    list("text/csv", "identity", "identity"),
    names = c("Content-Type", "Content-Encoding", "content-encoding")
  )
  expect_s3_class(
    csv_response_validation_test_error(
      csv_response_validation_test_build(
        plan,
        candidate = csv_response_validation_test_candidate(
          plan, headers = headers
        )
      )
    ),
    "gx_error_csv_response_validation_content_encoding"
  )
})

test_that("M7e rejects malformed, ambiguous, and mismatched Content-Length", {
  plan <- csv_response_validation_test_plan()
  invalid <- c(
    "", "-1", "+25", "25.0", "2.5e1", "25, 25", "0x19",
    "999999999999999999999999999999999999999999999999999999999999"
  )
  for (value in invalid) {
    condition <- csv_response_validation_test_error(
      csv_response_validation_test_build(
        plan,
        candidate = csv_response_validation_test_candidate(
          plan, content_length = value
        )
      )
    )
    expect_s3_class(
      condition, "gx_error_csv_response_validation_content_length"
    )
  }
  headers <- structure(
    list("text/csv", "25", "25"),
    names = c("Content-Type", "Content-Length", "content-length")
  )
  expect_s3_class(
    csv_response_validation_test_error(
      csv_response_validation_test_build(
        plan,
        candidate = csv_response_validation_test_candidate(
          plan, headers = headers
        )
      )
    ),
    "gx_error_csv_response_validation_content_length"
  )
})

test_that("M7e enforces bounded scalar response headers", {
  plan <- csv_response_validation_test_plan()
  too_many <- as.list(rep("value", 65L))
  names(too_many) <- paste0("x-fixture-", seq_along(too_many))
  too_many[[1L]] <- "text/csv"
  names(too_many)[[1L]] <- "Content-Type"
  condition <- csv_response_validation_test_error(
    csv_response_validation_test_build(
      plan,
      candidate = csv_response_validation_test_candidate(
        plan, headers = too_many
      )
    )
  )
  expect_s3_class(condition, "gx_error_csv_response_validation_headers")

  invalid <- list(
    list(`Content-Type` = c("text/csv", "application/csv")),
    list(`Content Type` = "text/csv"),
    list(`Content-Type` = NA_character_),
    list(`Content-Type` = paste(rep("x", 8193L), collapse = "")),
    structure(list("text/csv"), names = NA_character_)
  )
  for (headers in invalid) {
    condition <- csv_response_validation_test_error(
      csv_response_validation_test_build(
        plan,
        candidate = csv_response_validation_test_candidate(
          plan, headers = headers
        )
      )
    )
    expect_s3_class(
      condition, "gx_error_csv_response_validation_headers"
    )
  }

  aggregate <- as.list(rep(
    paste(rep("x", 8190L), collapse = ""), 9L
  ))
  names(aggregate) <- paste0("X-Fixture-", seq_along(aggregate))
  aggregate[[1L]] <- "text/csv"
  names(aggregate)[[1L]] <- "Content-Type"
  expect_s3_class(
    csv_response_validation_test_error(
      csv_response_validation_test_build(
        plan,
        candidate = csv_response_validation_test_candidate(
          plan, headers = aggregate
        )
      )
    ),
    "gx_error_csv_response_validation_headers"
  )

  invalid_name <- rawToChar(as.raw(0xff))
  Encoding(invalid_name) <- "UTF-8"
  headers <- list("text/csv", "fixture")
  names(headers) <- c("Content-Type", invalid_name)
  warnings <- character()
  condition <- withCallingHandlers(
    csv_response_validation_test_error(
      csv_response_validation_test_build(
        plan,
        candidate = csv_response_validation_test_candidate(
          plan, headers = headers
        )
      )
    ),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 0L)
  expect_s3_class(condition, "gx_error_csv_response_validation_headers")

  invalid_value <- rawToChar(as.raw(0xff))
  Encoding(invalid_value) <- "UTF-8"
  warnings <- character()
  condition <- withCallingHandlers(
    csv_response_validation_test_error(
      csv_response_validation_test_build(
        plan,
        candidate = csv_response_validation_test_candidate(
          plan,
          headers = list(
            `Content-Type` = "text/csv", `X-Fixture` = invalid_value
          )
        )
      )
    ),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 0L)
  expect_s3_class(condition, "gx_error_csv_response_validation_headers")
})

test_that("M7e requires the exact safe canonical no-redirect target", {
  plan <- csv_response_validation_test_plan()
  target <- csv_response_validation_test_url(plan)
  canonical <- gx_csv_get_intents_target_impl(target)$url
  fragment <- csv_response_validation_test_build(
    plan,
    candidate = csv_response_validation_test_candidate(
      plan, url = paste0(canonical, "#different-fragment")
    )
  )
  default_port <- csv_response_validation_test_build(
    plan,
    candidate = csv_response_validation_test_candidate(
      plan,
      url = sub(
        "https://data.example.org/", "https://data.example.org:443/",
        canonical, fixed = TRUE
      )
    )
  )
  expect_identical(
    fragment$validation$validation_id,
    csv_response_validation_test_build(plan)$validation$validation_id
  )
  expect_identical(
    default_port$validation$validation_id,
    csv_response_validation_test_build(plan)$validation$validation_id
  )

  changed <- c(
    sub("station=00123", "station=99999", target, fixed = TRUE),
    sub("token=fixture-token", "token=other", target, fixed = TRUE),
    sub("observations[.]csv", "redirected.csv", target),
    sub("data[.]example[.]org", "other.example.org", target),
    "https://user:password@data.example.org/observations.csv",
    "http://127.0.0.1/observations.csv",
    "http://[::1]/observations.csv"
  )
  for (url in changed) {
    condition <- csv_response_validation_test_error(
      csv_response_validation_test_build(
        plan,
        candidate = csv_response_validation_test_candidate(plan, url = url)
      )
    )
    expect_s3_class(
      condition, "gx_error_csv_response_validation_url"
    )
    expect_s3_class(condition, "gx_error_redirect")
  }
})

test_that("M7e identities are deterministic and bind semantic response facts", {
  plan <- csv_response_validation_test_plan()
  baseline <- csv_response_validation_test_build(plan)
  repeated <- csv_response_validation_test_build(plan)
  expect_identical(repeated, baseline)

  compact <- withr::with_options(
    list(scipen = 0), csv_response_validation_test_build(plan)
  )
  expanded <- withr::with_options(
    list(scipen = 999), csv_response_validation_test_build(plan)
  )
  expect_identical(compact, expanded)

  changed_body <- csv_response_validation_test_build(
    plan,
    candidate = csv_response_validation_test_candidate(
      plan, body = charToRaw("station,value\n00123,4.6\n")
    )
  )
  expect_false(identical(
    changed_body$validation$body_sha256,
    baseline$validation$body_sha256
  ))
  expect_false(identical(
    changed_body$validation$validation_id,
    baseline$validation$validation_id
  ))

  application <- csv_response_validation_test_build(
    plan,
    candidate = csv_response_validation_test_candidate(
      plan, content_type = "application/csv"
    )
  )
  expect_false(identical(
    application$validation$validation_id,
    baseline$validation$validation_id
  ))
})

test_that("M7e rejects foreign logical IDs and malformed nested M7d objects", {
  plan <- csv_response_validation_test_plan()
  candidate <- csv_response_validation_test_candidate(plan)
  for (logical_id in list(
      NULL, NA_character_, "not-a-hash", strrep("0", 64L),
      plan$request_plans$logical_request_id
  )) {
    condition <- csv_response_validation_test_error(
      gx_csv_validated_response_impl(plan, logical_id, candidate)
    )
    expect_s3_class(
      condition, "gx_error_csv_response_validation_input"
    )
  }

  forged <- csv_response_validation_test_clone(plan)
  forged$request_plans$response_byte_limit[[1L]] <-
    forged$request_plans$response_byte_limit[[1L]] + 1
  condition <- csv_response_validation_test_error(
    gx_csv_validated_response_impl(
      forged, plan$request_plans$logical_request_id[[1L]], candidate
    )
  )
  expect_s3_class(condition, "gx_error_csv_response_validation_input")
  expect_false(inherits(condition, "gx_error_csv_request_plan"))
})

test_that("M7e whole-object validation rejects forged fields and attributes", {
  baseline <- csv_response_validation_test_build()
  mutations <- list(
    function(x) { class(x) <- c("gx_csv_validated_response", "list"); x },
    function(x) { names(x)[[1L]] <- "version"; x },
    function(x) { x$contract_version <- "0.2.0"; x },
    function(x) { attr(x$body, "forged") <- TRUE; x },
    function(x) { x$body[[1L]] <- as.raw(0x00); x },
    function(x) { names(x$validation)[[1L]] <- "id"; x },
    function(x) { x$validation$validation_id <- strrep("0", 64L); x },
    function(x) { x$validation$logical_request_id <- strrep("0", 64L); x },
    function(x) { x$validation$intent_id <- strrep("0", 64L); x },
    function(x) { x$validation$reservation_id <- strrep("0", 64L); x },
    function(x) { x$validation$distribution_id <- strrep("0", 64L); x },
    function(x) { x$validation$status <- 201L; x },
    function(x) { x$validation$media_type <- "text/plain"; x },
    function(x) { x$validation$content_encoding <- "gzip"; x },
    function(x) { x$validation$content_length_present <- FALSE; x },
    function(x) { x$validation$content_length <- 0L; x },
    function(x) { x$validation$encoded_bytes <- 0L; x },
    function(x) { x$validation$decoded_bytes <- 0L; x },
    function(x) { x$validation$body_sha256 <- strrep("f", 64L); x },
    function(x) { x$validation$validation_status <- "transport_observed"; x },
    function(x) { names(x$metadata)[[1L]] <- "host_dependent"; x },
    function(x) { x$metadata$host_specific <- TRUE; x },
    function(x) { x$metadata$replayable <- TRUE; x },
    function(x) { x$metadata$execution_ready <- TRUE; x },
    function(x) { x$metadata$transport_authorized <- TRUE; x },
    function(x) { x$metadata$response_candidate_validated <- FALSE; x },
    function(x) { x$metadata$provider_response_observed <- TRUE; x },
    function(x) { x$metadata$budgets_consumed <- TRUE; x },
    function(x) { x$metadata$parser_executed <- TRUE; x },
    function(x) { x$metadata$observation_origin <- "provider"; x },
    function(x) { x$metadata$non_replayable_reasons <- "none"; x }
  )
  for (position in seq_along(mutations)) {
    forged <- mutations[[position]](
      csv_response_validation_test_clone(baseline)
    )
    condition <- csv_response_validation_test_error(
      gx_csv_validated_response_validate_impl(forged)
    )
    expect_s3_class(
      condition, "gx_error_csv_response_validation"
    )
  }

  oversized <- csv_response_validation_test_clone(baseline)
  oversized$body <- raw(
    as.integer(baseline$request_plan$request_plans$response_byte_limit[[1L]]) +
      1L
  )
  expect_s3_class(
    csv_response_validation_test_error(
      gx_csv_validated_response_validate_impl(oversized)
    ),
    "gx_error_csv_response_validation_payload_too_large"
  )
})

test_that("M7e owns only redacted response facts outside its intentional body", {
  plan <- csv_response_validation_test_plan()
  secret_header <- "cookie-secret-must-not-survive"
  candidate <- csv_response_validation_test_candidate(
    plan,
    headers = list(
      `Set-Cookie` = secret_header,
      `Content-Type` = "text/csv",
      `Content-Length` = as.character(
        length(csv_response_validation_test_body())
      )
    )
  )
  result <- csv_response_validation_test_build(plan, candidate = candidate)
  expect_false(any(grepl(
    secret_header,
    csv_response_validation_test_owned_text(result),
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "fixture-token",
    csv_response_validation_test_owned_text(result),
    fixed = TRUE
  )))

  output <- capture.output(print(result), type = "message")
  expect_false(any(grepl(secret_header, output, fixed = TRUE)))
  expect_false(any(grepl("fixture-token", output, fixed = TRUE)))

  bad <- csv_response_validation_test_candidate(
    plan,
    headers = list(`Content-Type` = "text/plain", `Set-Cookie` = secret_header),
    url = paste0(csv_response_validation_test_url(plan), "&leak=fixture-token")
  )
  condition <- csv_response_validation_test_error(
    csv_response_validation_test_build(plan, candidate = bad)
  )
  expect_false(grepl(secret_header, conditionMessage(condition), fixed = TRUE))
  expect_false(grepl("fixture-token", conditionMessage(condition), fixed = TRUE))
  expect_null(condition$call)
  expect_identical(nrow(condition$trace), 0L)
})

test_that("M7e construction remains offline and parser-free", {
  plan <- csv_response_validation_test_plan()
  dns_calls <- 0L
  old_resolver <- getOption("geoconnexr.dns_resolver")
  on.exit(options(geoconnexr.dns_resolver = old_resolver), add = TRUE)
  options(geoconnexr.dns_resolver = function(host) {
    dns_calls <<- dns_calls + 1L
    stop("DNS must remain disabled.", call. = FALSE)
  })
  before_loaded <- "readr" %in% loadedNamespaces()
  result <- csv_response_validation_test_build(plan)
  expect_identical(dns_calls, 0L)
  expect_identical("readr" %in% loadedNamespaces(), before_loaded)
  expect_false(result$metadata$provider_response_observed)
  expect_false(result$metadata$budgets_consumed)
  expect_false(result$metadata$parser_executed)

  namespace <- getNamespace("geoconnexr")
  internal <- ls(namespace, pattern = "^gx_csv_validated_response_")
  source_text <- paste(unlist(lapply(internal, function(name) {
    value <- get(name, envir = namespace, inherits = FALSE)
    if (is.function(value)) deparse(body(value)) else character()
  }), use.names = FALSE), collapse = "\n")
  prohibited <- paste0(
    "\\b(gx_http_request|gx_http_perform|req_perform|nslookup|",
    "requireNamespace|loadNamespace|read_csv|read[.]csv|writeLines|",
    "writeBin|saveRDS|readRDS|Sys[.]time|gx_now)\\s*\\("
  )
  expect_false(grepl(prohibited, source_text, perl = TRUE))
})

test_that("M7e preserves incomplete-source blockers", {
  intent_set <- csv_request_plan_test_intent_set(
    catalog = fetch_plan_test_catalog(status = "partial", truncated = TRUE),
    max_datasets = 2L,
    max_requests = 2L,
    max_encoded_bytes = 200,
    max_decoded_bytes = 200
  )
  result <- csv_response_validation_test_build(
    csv_response_validation_test_plan(intent_set)
  )
  expect_true(
    "source_catalog_incomplete" %in%
      result$metadata$non_replayable_reasons
  )
  expect_false(
    "response_validator_unimplemented" %in%
      result$metadata$non_replayable_reasons
  )
  expect_true(
    "response_origin_unbound" %in%
      result$metadata$non_replayable_reasons
  )
})

test_that("M7e owns a separate aggregate text budget", {
  result <- csv_response_validation_test_build()
  owned <- list(
    contract_version = result$contract_version,
    validation = result$validation,
    metadata = result$metadata
  )
  owned_total <- gx_fetch_plan_text_total(owned)
  wrapped_total <- gx_fetch_plan_text_total(result)
  temporary_limit <- as.integer(
    owned_total + max(1, floor((wrapped_total - owned_total) / 2))
  )
  expect_lt(owned_total, temporary_limit)
  expect_gt(wrapped_total, temporary_limit)

  validated <- testthat::with_mocked_bindings(
    gx_csv_validated_response_validate_impl(result),
    .gx_csv_validated_response_max_text_bytes = temporary_limit,
    .package = "geoconnexr"
  )
  expect_identical(validated, invisible(result))

  condition <- testthat::with_mocked_bindings(
    csv_response_validation_test_error(
      gx_csv_validated_response_validate_impl(result)
    ),
    .gx_csv_validated_response_max_text_bytes = 1L,
    .package = "geoconnexr"
  )
  expect_s3_class(condition, "gx_error_csv_response_validation_budget")
})

test_that("the M7e direct-CSV response contract remains internal", {
  internal <- c(
    "gx_csv_validated_response_impl",
    "gx_csv_validated_response_validate_impl"
  )
  exports <- getNamespaceExports("geoconnexr")
  expect_false(any(internal %in% exports))
  expect_false("gx_csv_validated_response" %in% exports)
  expect_true("gx_fetch" %in% exports)
  expect_false("gx_fetch_response" %in% exports)
})
