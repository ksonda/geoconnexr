.gx_csv_parsed_response_contract_version <- "0.1.0"
.gx_csv_parsed_response_max_input_bytes <- 16777216L
.gx_csv_parsed_response_max_field_bytes <- 1048576L
.gx_csv_parsed_response_max_header_name_bytes <- 16384L
.gx_csv_parsed_response_max_header_bytes <- 1048576L
.gx_csv_parsed_response_max_fields <- 1000000L
.gx_csv_parsed_response_max_rows <- 1000000L
.gx_csv_parsed_response_max_columns <- 10000L
.gx_csv_parsed_response_hash_chunk_fields <- 1024L
.gx_csv_parsed_response_max_text_bytes <- 33554432L

.gx_csv_parsed_response_fields <- c(
  "contract_version", "validated_response", "policy", "schema", "data",
  "parse", "metadata"
)

.gx_csv_parsed_response_policy_fields <- c(
  "slice_id", "encoding", "bom_policy", "delimiter", "quote",
  "escape_policy", "header_policy", "record_terminators",
  "embedded_record_terminators", "comment_policy", "blank_record_policy",
  "trim_whitespace", "missing_value_policy", "type_inference",
  "storage_type", "max_input_bytes", "max_field_bytes",
  "max_header_name_bytes", "max_header_bytes", "max_fields",
  "request_max_rows", "request_max_columns", "implementation_max_rows",
  "implementation_max_columns", "hash_chunk_fields"
)

.gx_csv_parsed_response_schema_columns <- c(
  "contract_version", "column_index", "column_name", "storage_type"
)

.gx_csv_parsed_response_parse_fields <- c(
  "parse_id", "validation_id", "body_sha256", "result_sha256",
  "bom_present", "row_count", "column_count", "field_count",
  "parse_status"
)

.gx_csv_parsed_response_metadata_fields <- c(
  "host_specific", "replayable", "execution_ready", "transport_authorized",
  "response_candidate_validated", "provider_response_observed",
  "budgets_consumed", "parser_executed", "csv_semantics_validated",
  "result_contract_bound", "observation_origin", "non_replayable_reasons"
)

gx_csv_parsed_response_abort <- function(
    message,
    class = "gx_error_csv_parse_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_csv_parse", "gx_error_fetch_plan"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_csv_parsed_response_exact_attributes <- function(x, expected) {
  observed <- names(attributes(x))
  is.character(observed) && !anyNA(observed) &&
    length(observed) == length(expected) && all(expected %in% observed)
}

gx_csv_parsed_response_valid_scalar_text <- function(x, nonempty = TRUE) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    is.null(attributes(x)) && gx_fetch_plan_text_valid(
      x, allow_na = FALSE, nonempty = nonempty
    )
}

gx_csv_parsed_response_selected_request_impl <- function(validated_response) {
  request_plan <- validated_response$request_plan
  logical_request_id <- validated_response$validation$logical_request_id
  position <- which(
    request_plan$request_plans$logical_request_id == logical_request_id
  )
  if (length(position) != 1L) {
    gx_csv_parsed_response_abort(
      "The validated response does not select one direct-CSV request.",
      "gx_error_csv_parse_input"
    )
  }
  request_plan$request_plans[unname(as.integer(position)), , drop = FALSE]
}

gx_csv_parsed_response_field_limit_impl <- function(max_fields) {
  if (!is.numeric(max_fields) || length(max_fields) != 1L ||
      is.na(max_fields) || !is.finite(max_fields) || max_fields < 1 ||
      max_fields != floor(max_fields) ||
      max_fields > .gx_csv_parsed_response_max_fields) {
    gx_csv_parsed_response_abort(
      "The CSV parser field limit must be an explicit bounded whole number.",
      "gx_error_csv_parse_budget"
    )
  }
  unname(as.integer(max_fields))
}

gx_csv_parsed_response_policy_impl <- function(request, max_fields) {
  max_fields <- gx_csv_parsed_response_field_limit_impl(max_fields)
  list(
    slice_id = "direct_csv_parse_v1",
    encoding = "UTF-8",
    bom_policy = "optional_at_byte_one_strip",
    delimiter = ",",
    quote = "\"",
    escape_policy = "doubled_quote_only",
    header_policy = "required_nonempty_unique_exact",
    record_terminators = c("LF", "CRLF"),
    embedded_record_terminators = "reject",
    comment_policy = "disabled",
    blank_record_policy = "reject",
    trim_whitespace = FALSE,
    missing_value_policy = "none_empty_is_empty_string",
    type_inference = "disabled",
    storage_type = "character",
    max_input_bytes = .gx_csv_parsed_response_max_input_bytes,
    max_field_bytes = .gx_csv_parsed_response_max_field_bytes,
    max_header_name_bytes = .gx_csv_parsed_response_max_header_name_bytes,
    max_header_bytes = .gx_csv_parsed_response_max_header_bytes,
    max_fields = max_fields,
    request_max_rows = request$max_rows[[1L]],
    request_max_columns = request$max_columns[[1L]],
    implementation_max_rows = .gx_csv_parsed_response_max_rows,
    implementation_max_columns = .gx_csv_parsed_response_max_columns,
    hash_chunk_fields = .gx_csv_parsed_response_hash_chunk_fields
  )
}

gx_csv_parsed_response_byte_impl <- function(body, index) {
  as.integer(body[[index]])
}

gx_csv_parsed_response_utf8_length_impl <- function(body, index, size) {
  first <- gx_csv_parsed_response_byte_impl(body, index)
  tail_byte <- function(offset, lower = 128L, upper = 191L) {
    position <- index + offset
    position <= size && {
      value <- gx_csv_parsed_response_byte_impl(body, position)
      value >= lower && value <= upper
    }
  }
  if (first >= 194L && first <= 223L && tail_byte(1)) return(2L)
  if (first == 224L && tail_byte(1, 160L, 191L) && tail_byte(2)) return(3L)
  if (((first >= 225L && first <= 236L) ||
      (first >= 238L && first <= 239L)) &&
      tail_byte(1) && tail_byte(2)) return(3L)
  if (first == 237L && tail_byte(1, 128L, 159L) && tail_byte(2)) return(3L)
  if (first == 240L && tail_byte(1, 144L, 191L) &&
      tail_byte(2) && tail_byte(3)) return(4L)
  if (first >= 241L && first <= 243L && tail_byte(1) &&
      tail_byte(2) && tail_byte(3)) return(4L)
  if (first == 244L && tail_byte(1, 128L, 143L) &&
      tail_byte(2) && tail_byte(3)) return(4L)
  0L
}

gx_csv_parsed_response_add_field_bytes_impl <- function(
    current, addition, limit) {
  if (addition > limit - current) {
    gx_csv_parsed_response_abort(
      "A CSV field exceeds the parser scalar-byte budget.",
      "gx_error_csv_parse_budget"
    )
  }
  current + addition
}

gx_csv_parsed_response_scan_impl <- function(body, policy) {
  if (!is.raw(body) || !is.null(attributes(body))) {
    gx_csv_parsed_response_abort(
      "The CSV parser requires the exact validated raw response body.",
      "gx_error_csv_parse_input"
    )
  }
  size <- as.double(length(body))
  if (size > as.double(policy$max_input_bytes)) {
    gx_csv_parsed_response_abort(
      "The validated CSV body exceeds the parser input-byte budget.",
      "gx_error_csv_parse_budget"
    )
  }
  if (size == 0) {
    gx_csv_parsed_response_abort(
      "The CSV parser requires one nonempty header record.",
      "gx_error_csv_parse_syntax"
    )
  }

  bom_present <- size >= 3 && identical(
    body[seq_len(3L)], as.raw(c(239L, 187L, 191L))
  )
  index <- if (bom_present) 4 else 1
  if (index > size) {
    gx_csv_parsed_response_abort(
      "The CSV parser requires one nonempty header record.",
      "gx_error_csv_parse_syntax"
    )
  }

  field_start <- 0L
  unquoted <- 1L
  quoted <- 2L
  after_quote <- 3L
  state <- field_start
  active_record <- FALSE
  records <- 0
  record_fields <- 0
  columns <- 0
  fields <- 0
  field_bytes <- 0
  effective_rows <- min(
    as.double(policy$request_max_rows),
    as.double(policy$implementation_max_rows)
  )
  effective_columns <- min(
    as.double(policy$request_max_columns),
    as.double(policy$implementation_max_columns)
  )

  add_field <- function() {
    if (fields >= as.double(policy$max_fields)) {
      gx_csv_parsed_response_abort(
        "CSV fields exceed the explicit parser field budget.",
        "gx_error_csv_parse_budget"
      )
    }
    if (records == 0 && field_bytes == 0) {
      gx_csv_parsed_response_abort(
        "CSV header names must be nonempty after exact decoding.",
        "gx_error_csv_parse_header"
      )
    }
    fields <<- fields + 1
    record_fields <<- record_fields + 1
    if (records == 0 && record_fields > effective_columns) {
      gx_csv_parsed_response_abort(
        "CSV columns exceed the selected request or implementation limit.",
        "gx_error_csv_parse_budget"
      )
    }
    field_bytes <<- 0
    invisible(NULL)
  }

  end_record <- function() {
    if (!active_record) {
      gx_csv_parsed_response_abort(
        "Blank CSV records are not admitted by the parser profile.",
        "gx_error_csv_parse_syntax"
      )
    }
    add_field()
    if (records == 0) {
      columns <<- record_fields
      if (columns < 1 || columns > effective_columns) {
        gx_csv_parsed_response_abort(
          "CSV columns exceed the selected request or implementation limit.",
          "gx_error_csv_parse_budget"
        )
      }
    } else if (record_fields != columns) {
      gx_csv_parsed_response_abort(
        "CSV data records must have the exact header width.",
        "gx_error_csv_parse_shape"
      )
    }
    records <<- records + 1
    if (records - 1 > effective_rows) {
      gx_csv_parsed_response_abort(
        "CSV rows exceed the selected request or implementation limit.",
        "gx_error_csv_parse_budget"
      )
    }
    record_fields <<- 0
    active_record <<- FALSE
    state <<- field_start
    invisible(NULL)
  }

  while (index <= size) {
    byte <- gx_csv_parsed_response_byte_impl(body, index)

    if (byte == 239L && index + 2 <= size &&
        gx_csv_parsed_response_byte_impl(body, index + 1) == 187L &&
        gx_csv_parsed_response_byte_impl(body, index + 2) == 191L) {
      gx_csv_parsed_response_abort(
        "A UTF-8 BOM is allowed only once at the first body byte.",
        "gx_error_csv_parse_encoding"
      )
    }
    if (byte == 0L || (byte >= 1L && byte <= 9L) ||
        byte %in% c(11L, 12L) || (byte >= 14L && byte <= 31L) ||
        byte == 127L) {
      gx_csv_parsed_response_abort(
        "The CSV body contains a disallowed control byte.",
        "gx_error_csv_parse_encoding"
      )
    }

    if (byte >= 128L) {
      width <- gx_csv_parsed_response_utf8_length_impl(body, index, size)
      if (width == 0L ||
          (byte == 194L && index + 1 <= size &&
           gx_csv_parsed_response_byte_impl(body, index + 1) <= 159L)) {
        gx_csv_parsed_response_abort(
          "The CSV body is not strict control-safe UTF-8.",
          "gx_error_csv_parse_encoding"
        )
      }
      if (state == after_quote) {
        gx_csv_parsed_response_abort(
          "Only a delimiter or record terminator may follow a closing quote.",
          "gx_error_csv_parse_syntax"
        )
      }
      if (state == field_start) {
        state <- unquoted
        active_record <- TRUE
      }
      field_bytes <- gx_csv_parsed_response_add_field_bytes_impl(
        field_bytes, width, as.double(policy$max_field_bytes)
      )
      index <- index + width
      next
    }

    if (state == quoted) {
      if (byte %in% c(10L, 13L)) {
        gx_csv_parsed_response_abort(
          "Embedded record terminators are not admitted in quoted fields.",
          "gx_error_csv_parse_syntax"
        )
      }
      if (byte == 34L) {
        if (index < size &&
            gx_csv_parsed_response_byte_impl(body, index + 1) == 34L) {
          field_bytes <- gx_csv_parsed_response_add_field_bytes_impl(
            field_bytes, 1, as.double(policy$max_field_bytes)
          )
          index <- index + 2
          next
        }
        state <- after_quote
        index <- index + 1
        next
      }
      field_bytes <- gx_csv_parsed_response_add_field_bytes_impl(
        field_bytes, 1, as.double(policy$max_field_bytes)
      )
      index <- index + 1
      next
    }

    if (byte == 34L) {
      if (state != field_start) {
        gx_csv_parsed_response_abort(
          "A CSV quote may appear only at the start of a field.",
          "gx_error_csv_parse_syntax"
        )
      }
      state <- quoted
      active_record <- TRUE
      index <- index + 1
      next
    }

    if (byte == 44L) {
      if (state == after_quote || state == unquoted || state == field_start) {
        active_record <- TRUE
        add_field()
        state <- field_start
        index <- index + 1
        next
      }
    }

    if (byte == 13L || byte == 10L) {
      if (state == unquoted || state == after_quote || state == field_start) {
        if (byte == 13L) {
          if (index >= size ||
              gx_csv_parsed_response_byte_impl(body, index + 1) != 10L) {
            gx_csv_parsed_response_abort(
              "A carriage return must be paired with a following line feed.",
              "gx_error_csv_parse_syntax"
            )
          }
          index <- index + 2
        } else {
          index <- index + 1
        }
        end_record()
        next
      }
    }

    if (state == after_quote) {
      gx_csv_parsed_response_abort(
        "Only a delimiter or record terminator may follow a closing quote.",
        "gx_error_csv_parse_syntax"
      )
    }
    if (state == field_start) {
      state <- unquoted
      active_record <- TRUE
    }
    field_bytes <- gx_csv_parsed_response_add_field_bytes_impl(
      field_bytes, 1, as.double(policy$max_field_bytes)
    )
    index <- index + 1
  }

  if (state == quoted) {
    gx_csv_parsed_response_abort(
      "The CSV body ends inside a quoted field.",
      "gx_error_csv_parse_syntax"
    )
  }
  if (active_record) end_record()
  if (records == 0) {
    gx_csv_parsed_response_abort(
      "The CSV parser requires one nonempty header record.",
      "gx_error_csv_parse_syntax"
    )
  }

  body_without_bom <- if (bom_present) body[-seq_len(3L)] else body
  text <- rawToChar(body_without_bom)
  Encoding(text) <- "UTF-8"
  valid_utf8 <- tryCatch(
    stringi::stri_enc_isutf8(text),
    error = function(cnd) FALSE,
    warning = function(cnd) FALSE
  )
  forbidden_unicode <- tryCatch(
    stringi::stri_detect_regex(text, "[\\p{Cf}\\p{Cs}]"),
    error = function(cnd) TRUE,
    warning = function(cnd) TRUE
  )
  if (!identical(valid_utf8, TRUE) || !identical(forbidden_unicode, FALSE)) {
    gx_csv_parsed_response_abort(
      "The CSV body is not strict control-safe UTF-8.",
      "gx_error_csv_parse_encoding"
    )
  }

  list(
    bom_present = bom_present,
    row_count = unname(as.integer(records - 1)),
    column_count = unname(as.integer(columns)),
    field_count = unname(as.integer(fields))
  )
}

gx_csv_parsed_response_raw_slice_impl <- function(body, first, last) {
  if (last < first) return(raw())
  unname(body[seq.int(first, last)])
}

gx_csv_parsed_response_decode_field_impl <- function(
    body, first, last, quoted) {
  bytes <- gx_csv_parsed_response_raw_slice_impl(body, first, last)
  value <- rawToChar(bytes)
  Encoding(value) <- "UTF-8"
  if (quoted && length(bytes)) {
    value <- gsub("\"\"", "\"", value, fixed = TRUE, useBytes = TRUE)
    Encoding(value) <- "UTF-8"
  }
  unname(value)
}

gx_csv_parsed_response_fields_impl <- function(
    body, scan, stop_after = scan$field_count) {
  stop_after <- as.double(stop_after)
  values <- rep.int("", unname(as.integer(stop_after)))
  size <- as.double(length(body))
  index <- if (scan$bom_present) 4 else 1
  field_start <- index
  field_start_state <- 0L
  unquoted <- 1L
  quoted <- 2L
  after_quote <- 3L
  state <- field_start_state
  quoted_field <- FALSE
  closing_quote <- 0
  output <- 0

  emit <- function(last) {
    output <<- output + 1
    if (output > stop_after) return(TRUE)
    if (quoted_field) {
      values[[output]] <<- gx_csv_parsed_response_decode_field_impl(
        body, field_start + 1, closing_quote - 1, TRUE
      )
    } else {
      values[[output]] <<- gx_csv_parsed_response_decode_field_impl(
        body, field_start, last, FALSE
      )
    }
    output >= stop_after
  }

  while (index <= size) {
    byte <- gx_csv_parsed_response_byte_impl(body, index)
    if (byte >= 128L) {
      width <- gx_csv_parsed_response_utf8_length_impl(body, index, size)
      if (state == field_start_state) state <- unquoted
      index <- index + width
      next
    }
    if (state == quoted) {
      if (byte == 34L) {
        if (index < size &&
            gx_csv_parsed_response_byte_impl(body, index + 1) == 34L) {
          index <- index + 2
          next
        }
        closing_quote <- index
        state <- after_quote
      }
      index <- index + 1
      next
    }
    if (byte == 34L && state == field_start_state) {
      quoted_field <- TRUE
      state <- quoted
      index <- index + 1
      next
    }
    if (byte == 44L) {
      if (emit(index - 1)) break
      state <- field_start_state
      quoted_field <- FALSE
      closing_quote <- 0
      index <- index + 1
      field_start <- index
      next
    }
    if (byte == 10L || byte == 13L) {
      if (emit(index - 1)) break
      if (byte == 13L) index <- index + 2 else index <- index + 1
      state <- field_start_state
      quoted_field <- FALSE
      closing_quote <- 0
      field_start <- index
      next
    }
    if (state == field_start_state) state <- unquoted
    index <- index + 1
  }
  if (output < stop_after) emit(size)
  if (output != stop_after) {
    gx_csv_parsed_response_abort(
      "CSV materialization did not match its bounded lexical scan.",
      "gx_error_csv_parse_contract"
    )
  }
  unname(values)
}

gx_csv_parsed_response_header_impl <- function(body, scan, policy) {
  header <- gx_csv_parsed_response_fields_impl(
    body, scan, stop_after = scan$column_count
  )
  bytes <- nchar(header, type = "bytes", allowNA = TRUE)
  if (anyNA(header) || any(!nzchar(header)) || anyNA(bytes) ||
      any(bytes > policy$max_header_name_bytes) ||
      sum(as.double(bytes)) > as.double(policy$max_header_bytes)) {
    gx_csv_parsed_response_abort(
      "CSV header names violate their exact text or byte budget.",
      "gx_error_csv_parse_header"
    )
  }
  if (anyDuplicated(header)) {
    gx_csv_parsed_response_abort(
      "CSV header names must be unique without name repair.",
      "gx_error_csv_parse_header"
    )
  }
  unname(header)
}

gx_csv_parsed_response_data_impl <- function(body, scan, policy) {
  header <- gx_csv_parsed_response_header_impl(body, scan, policy)
  fields <- gx_csv_parsed_response_fields_impl(body, scan)
  if (!identical(fields[seq_len(scan$column_count)], header)) {
    gx_csv_parsed_response_abort(
      "CSV header decoding was not stable across bounded passes.",
      "gx_error_csv_parse_contract"
    )
  }
  rows <- scan$row_count
  columns <- scan$column_count
  data_columns <- lapply(seq_len(columns), function(column) {
    if (rows == 0L) return(character())
    positions <- columns + column +
      as.double(seq.int(0L, rows - 1L)) * columns
    unname(fields[positions])
  })
  names(data_columns) <- header
  tibble::as_tibble(data_columns, .name_repair = "minimal")
}

gx_csv_parsed_response_schema_impl <- function(data) {
  tibble::tibble(
    contract_version = rep.int(
      .gx_csv_parsed_response_contract_version, ncol(data)
    ),
    column_index = seq_len(ncol(data)),
    column_name = unname(names(data)),
    storage_type = rep.int("character", ncol(data))
  )
}

gx_csv_parsed_response_column_hash_impl <- function(name, values) {
  chunks <- if (!length(values)) 0L else as.integer(ceiling(
    length(values) / .gx_csv_parsed_response_hash_chunk_fields
  ))
  chunk_hashes <- if (chunks == 0L) character() else vapply(
    seq_len(chunks),
    function(chunk) {
      first <- (chunk - 1L) * .gx_csv_parsed_response_hash_chunk_fields + 1L
      last <- min(
        length(values), chunk * .gx_csv_parsed_response_hash_chunk_fields
      )
      part <- unname(values[seq.int(first, last)])
      gx_contract_hash(
        c(
          list("chunk", chunk, "field_count", length(part)),
          as.list(part)
        ),
        namespace = "geoconnexr.csv-result-chunk.v1",
        contract_version = .gx_csv_parsed_response_contract_version
      )
    },
    character(1), USE.NAMES = FALSE
  )
  gx_contract_hash(
    c(
      list(
        "column_name", name,
        "row_count", length(values),
        "chunk_fields", .gx_csv_parsed_response_hash_chunk_fields,
        "chunk_count", chunks
      ),
      as.list(chunk_hashes)
    ),
    namespace = "geoconnexr.csv-result-column.v1",
    contract_version = .gx_csv_parsed_response_contract_version
  )
}

gx_csv_parsed_response_result_hash_impl <- function(data) {
  column_hashes <- vapply(seq_along(data), function(position) {
    gx_csv_parsed_response_column_hash_impl(
      names(data)[[position]], data[[position]]
    )
  }, character(1), USE.NAMES = FALSE)
  gx_contract_hash(
    c(
      list(
        "row_count", nrow(data),
        "column_count", ncol(data),
        "column_hash_count", length(column_hashes)
      ),
      as.list(column_hashes)
    ),
    namespace = "geoconnexr.csv-result.v1",
    contract_version = .gx_csv_parsed_response_contract_version
  )
}

gx_csv_parsed_response_parse_id_impl <- function(
    validated_response, policy, scan, result_sha256) {
  validation <- validated_response$validation
  gx_contract_hash(
    list(
      "validation_id", validation$validation_id,
      "body_sha256", validation$body_sha256,
      "slice_id", policy$slice_id,
      "encoding", policy$encoding,
      "bom_policy", policy$bom_policy,
      "delimiter", policy$delimiter,
      "quote", policy$quote,
      "escape_policy", policy$escape_policy,
      "header_policy", policy$header_policy,
      "record_terminator_count", length(policy$record_terminators),
      "record_terminator_1", policy$record_terminators[[1L]],
      "record_terminator_2", policy$record_terminators[[2L]],
      "embedded_record_terminators", policy$embedded_record_terminators,
      "comment_policy", policy$comment_policy,
      "blank_record_policy", policy$blank_record_policy,
      "trim_whitespace", policy$trim_whitespace,
      "missing_value_policy", policy$missing_value_policy,
      "type_inference", policy$type_inference,
      "storage_type", policy$storage_type,
      "max_input_bytes", policy$max_input_bytes,
      "max_field_bytes", policy$max_field_bytes,
      "max_header_name_bytes", policy$max_header_name_bytes,
      "max_header_bytes", policy$max_header_bytes,
      "max_fields", policy$max_fields,
      "request_max_rows", policy$request_max_rows,
      "request_max_columns", policy$request_max_columns,
      "implementation_max_rows", policy$implementation_max_rows,
      "implementation_max_columns", policy$implementation_max_columns,
      "hash_chunk_fields", policy$hash_chunk_fields,
      "bom_present", scan$bom_present,
      "row_count", scan$row_count,
      "column_count", scan$column_count,
      "field_count", scan$field_count,
      "result_sha256", result_sha256
    ),
    namespace = "geoconnexr.csv-parse.v1",
    contract_version = .gx_csv_parsed_response_contract_version
  )
}

gx_csv_parsed_response_parse_impl <- function(
    validated_response, policy, scan, data) {
  result_sha256 <- gx_csv_parsed_response_result_hash_impl(data)
  list(
    parse_id = gx_csv_parsed_response_parse_id_impl(
      validated_response, policy, scan, result_sha256
    ),
    validation_id = validated_response$validation$validation_id,
    body_sha256 = validated_response$validation$body_sha256,
    result_sha256 = result_sha256,
    bom_present = scan$bom_present,
    row_count = scan$row_count,
    column_count = scan$column_count,
    field_count = scan$field_count,
    parse_status = "parsed_caller_supplied_validated_response"
  )
}

gx_csv_parsed_response_reasons_impl <- function(validated_response) {
  remove <- c(
    "csv_parser_enforcement_unimplemented",
    "csv_parser_semantics_unbound",
    "result_schema_unbound"
  )
  reasons <- setdiff(
    validated_response$metadata$non_replayable_reasons, remove
  )
  reasons[gx_catalog_byte_order(reasons)]
}

gx_csv_parsed_response_metadata_impl <- function(validated_response) {
  list(
    host_specific = FALSE,
    replayable = FALSE,
    execution_ready = FALSE,
    transport_authorized = FALSE,
    response_candidate_validated = TRUE,
    provider_response_observed = FALSE,
    budgets_consumed = FALSE,
    parser_executed = TRUE,
    csv_semantics_validated = TRUE,
    result_contract_bound = TRUE,
    observation_origin = "caller_supplied",
    non_replayable_reasons = gx_csv_parsed_response_reasons_impl(
      validated_response
    )
  )
}

gx_csv_parsed_response_new_impl <- function(
    validated_response, policy, schema, data, parse, metadata) {
  object <- structure(
    list(
      contract_version = .gx_csv_parsed_response_contract_version,
      validated_response = validated_response,
      policy = policy,
      schema = schema,
      data = data,
      parse = parse,
      metadata = metadata
    ),
    class = "gx_csv_parsed_response"
  )
  gx_csv_parsed_response_validate_impl(object)
  object
}

gx_csv_parsed_response_impl <- function(
    validated_response, max_fields = NULL) {
  valid_response <- tryCatch({
    gx_csv_validated_response_validate_impl(validated_response)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid_response) {
    gx_csv_parsed_response_abort(
      "M7f construction requires one valid M7e response object.",
      "gx_error_csv_parse_input"
    )
  }
  request <- gx_csv_parsed_response_selected_request_impl(validated_response)
  policy <- gx_csv_parsed_response_policy_impl(request, max_fields)
  scan <- gx_csv_parsed_response_scan_impl(
    validated_response$body, policy
  )
  data <- gx_csv_parsed_response_data_impl(
    validated_response$body, scan, policy
  )
  schema <- gx_csv_parsed_response_schema_impl(data)
  parse <- gx_csv_parsed_response_parse_impl(
    validated_response, policy, scan, data
  )
  gx_csv_parsed_response_new_impl(
    validated_response = validated_response,
    policy = policy,
    schema = schema,
    data = data,
    parse = parse,
    metadata = gx_csv_parsed_response_metadata_impl(validated_response)
  )
}

gx_csv_parsed_response_table_attributes <- function(x, rows) {
  expected_rows <- if (rows == 0L) integer() else {
    c(NA_integer_, -as.integer(rows))
  }
  gx_csv_parsed_response_exact_attributes(
    x, c("class", "row.names", "names")
  ) && identical(.row_names_info(x, type = 0L), expected_rows)
}

gx_csv_parsed_response_assert_owned_shape_impl <- function(x) {
  if (!inherits(x$schema, "tbl_df") ||
      !identical(names(x$schema), .gx_csv_parsed_response_schema_columns) ||
      !inherits(x$data, "tbl_df")) {
    gx_csv_parsed_response_abort(
      "Parsed CSV schema or data violates its exact table shape."
    )
  }
  schema_rows <- gx_catalog_table_rows(x$schema)
  data_rows <- gx_catalog_table_rows(x$data)
  if (is.null(schema_rows) || is.null(data_rows) ||
      schema_rows < 1 || schema_rows > .gx_csv_parsed_response_max_columns ||
      length(x$data) != schema_rows ||
      data_rows > .gx_csv_parsed_response_max_rows ||
      schema_rows * (data_rows + 1) > .gx_csv_parsed_response_max_fields ||
      !gx_csv_parsed_response_table_attributes(
        x$schema, unname(as.integer(schema_rows))
      ) || !gx_csv_parsed_response_table_attributes(
        x$data, unname(as.integer(data_rows))
      )) {
    gx_csv_parsed_response_abort(
      "Parsed CSV schema or data violates its bounded table contract.",
      "gx_error_csv_parse_budget"
    )
  }
  if (!all(vapply(x$schema, function(column) {
    is.null(attributes(column))
  }, logical(1))) || !all(vapply(x$data, function(column) {
    is.character(column) && !anyNA(column) && is.null(attributes(column))
  }, logical(1)))) {
    gx_csv_parsed_response_abort(
      "Parsed CSV schema or data has invalid column types or attributes."
    )
  }
  invisible(NULL)
}

gx_csv_parsed_response_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(
    names(x), .gx_csv_parsed_response_fields
  ) && identical(class(x), "gx_csv_parsed_response") &&
    gx_csv_parsed_response_exact_attributes(x, c("names", "class")) &&
    identical(
      x$contract_version, .gx_csv_parsed_response_contract_version
    ) && is.null(attributes(x$contract_version))
  if (!valid_top) {
    gx_csv_parsed_response_abort(
      "Parsed CSV response violates its exact top-level contract."
    )
  }
  valid_response <- tryCatch({
    gx_csv_validated_response_validate_impl(x$validated_response)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!valid_response) {
    gx_csv_parsed_response_abort(
      "Parsed CSV response embeds an invalid M7e response object."
    )
  }
  request <- gx_csv_parsed_response_selected_request_impl(
    x$validated_response
  )
  valid_policy_shape <- is.list(x$policy) && identical(
    names(x$policy), .gx_csv_parsed_response_policy_fields
  ) && gx_csv_parsed_response_exact_attributes(x$policy, "names")
  expected_policy <- if (valid_policy_shape) tryCatch(
    gx_csv_parsed_response_policy_impl(request, x$policy$max_fields),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  ) else NULL
  if (is.null(expected_policy) || !identical(x$policy, expected_policy)) {
    gx_csv_parsed_response_abort(
      "Parsed CSV policy violates its exact fixed semantics."
    )
  }
  gx_csv_parsed_response_assert_owned_shape_impl(x)
  valid_schema <- is.character(x$schema$contract_version) &&
    is.integer(x$schema$column_index) &&
    is.character(x$schema$column_name) &&
    is.character(x$schema$storage_type) &&
    !anyNA(x$schema$contract_version) && !anyNA(x$schema$column_index) &&
    !anyNA(x$schema$column_name) && !anyNA(x$schema$storage_type)
  valid_parse_shape <- is.list(x$parse) && identical(
    names(x$parse), .gx_csv_parsed_response_parse_fields
  ) && gx_csv_parsed_response_exact_attributes(x$parse, "names")
  valid_metadata_shape <- is.list(x$metadata) && identical(
    names(x$metadata), .gx_csv_parsed_response_metadata_fields
  ) && gx_csv_parsed_response_exact_attributes(x$metadata, "names")
  if (!valid_schema || !valid_parse_shape || !valid_metadata_shape) {
    gx_csv_parsed_response_abort(
      "Parsed CSV schema, parse facts, or metadata has an invalid shape."
    )
  }
  parse_character_fields <- c(
    "parse_id", "validation_id", "body_sha256", "result_sha256",
    "parse_status"
  )
  parse_integer_fields <- c(
    "row_count", "column_count", "field_count"
  )
  valid_parse_types <- all(vapply(
    x$parse[parse_character_fields],
    gx_csv_parsed_response_valid_scalar_text,
    logical(1)
  )) && all(vapply(x$parse[parse_integer_fields], function(value) {
    is.integer(value) && length(value) == 1L && !is.na(value) &&
      is.null(attributes(value))
  }, logical(1))) && is.logical(x$parse$bom_present) &&
    length(x$parse$bom_present) == 1L && !is.na(x$parse$bom_present) &&
    is.null(attributes(x$parse$bom_present))
  if (!valid_parse_types ||
      !gx_catalog_is_sha256(x$parse$parse_id) ||
      !gx_catalog_is_sha256(x$parse$validation_id) ||
      !gx_catalog_is_sha256(x$parse$body_sha256) ||
      !gx_catalog_is_sha256(x$parse$result_sha256)) {
    gx_csv_parsed_response_abort(
      "Parsed CSV parse facts have invalid types or identities."
    )
  }
  owned_text <- gx_fetch_plan_text_total(
    list(x$policy, x$schema, x$data, x$parse, x$metadata),
    limit = .gx_csv_parsed_response_max_text_bytes
  )
  if (!is.finite(owned_text) ||
      owned_text > .gx_csv_parsed_response_max_text_bytes) {
    gx_csv_parsed_response_abort(
      "Parsed CSV owned text exceeds its aggregate byte budget.",
      "gx_error_csv_parse_budget"
    )
  }

  scan <- gx_csv_parsed_response_scan_impl(
    x$validated_response$body, x$policy
  )
  expected_data <- gx_csv_parsed_response_data_impl(
    x$validated_response$body, scan, x$policy
  )
  expected_schema <- gx_csv_parsed_response_schema_impl(expected_data)
  expected_parse <- gx_csv_parsed_response_parse_impl(
    x$validated_response, x$policy, scan, expected_data
  )
  if (!identical(x$schema, expected_schema) ||
      !identical(x$data, expected_data) ||
      !identical(x$parse, expected_parse)) {
    gx_csv_parsed_response_abort(
      "Parsed CSV schema, data, or identity does not match the exact body."
    )
  }
  expected_metadata <- gx_csv_parsed_response_metadata_impl(
    x$validated_response
  )
  if (!identical(x$metadata, expected_metadata)) {
    gx_csv_parsed_response_abort(
      "Parsed CSV metadata overstates authority or parser provenance."
    )
  }
  invisible(x)
}

print.gx_csv_parsed_response <- function(x, ...) {
  gx_csv_parsed_response_validate_impl(x)
  cat("<gx_csv_parsed_response>\n")
  cat("  logical request: ", substr(
    x$validated_response$validation$logical_request_id, 1L, 12L
  ), "...\n", sep = "")
  cat("  rows: ", x$parse$row_count, "\n", sep = "")
  cat("  columns: ", x$parse$column_count, "\n", sep = "")
  cat("  storage: character\n")
  cat("  observation origin: caller_supplied\n")
  cat("  execution ready: no\n")
  invisible(x)
}
