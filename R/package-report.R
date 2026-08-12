.gx_package_report_contract_version <- "0.1.0"
.gx_package_report_render_timeout <- 30
.gx_package_report_max_source_bytes <- 64 * 1024
.gx_package_report_max_output_bytes <- 8 * 1024 * 1024

.gx_package_report_fields <- c(
  "contract_version", "mode", "status", "stage", "hydrated", "runtime",
  "summary", "source", "output", "metadata", "report_id"
)

.gx_package_report_summary_fields <- c(
  "sites", "datasets", "problems", "requests", "fetch_statuses",
  "native_resources", "observations", "harmonized_resources"
)

.gx_package_report_source_fields <- c(
  "path", "media_type", "bytes", "byte_count", "sha256"
)

.gx_package_report_output_fields <- c(
  "path", "media_type", "bytes", "byte_count", "sha256", "structure"
)

.gx_package_report_structure_fields <- c(
  "title", "landmark", "contract_version", "hydration_id", "stage",
  .gx_package_report_summary_fields, "scripts", "external_references"
)

.gx_package_report_metadata_fields <- c(
  "scope", "host_specific", "fixed_source", "execution_enabled", "cache",
  "format", "minimal", "embed_resources", "render_command",
  "render_arguments", "render_timeout", "closed_output_tree",
  "source_unchanged", "html_verified", "temporary_stage_removed",
  "package_integrated", "public", "replayable", "limitations"
)

gx_package_report_abort <- function(
    message,
    class = "gx_error_package_report_contract",
    ...,
    call = rlang::caller_env()) {
  gx_abort(
    message,
    class = unique(c(
      class, "gx_error_package_report", "gx_error_package_quarto_cli",
      "gx_error_package_quarto", "gx_error_package"
    )),
    ...,
    call = call,
    .redact_trace = TRUE
  )
}

gx_package_report_call_safely <- function(code, message, class) {
  tryCatch(
    withCallingHandlers(
      code,
      warning = function(cnd) gx_package_report_abort(message, class)
    ),
    gx_error_package_report = function(cnd) stop(cnd),
    error = function(cnd) gx_package_report_abort(message, class)
  )
}

gx_package_report_summary_impl <- function(hydrated) {
  gx_package_hydrated_validate_impl(hydrated)
  count <- function(x) {
    if (is.null(x)) 0L else unname(as.integer(nrow(x)))
  }
  list(
    sites = count(hydrated$catalog$sites),
    datasets = count(hydrated$catalog$datasets),
    problems = count(hydrated$catalog$problems),
    requests = count(hydrated$catalog$requests),
    fetch_statuses = if (is.null(hydrated$fetch)) {
      0L
    } else {
      count(hydrated$fetch$status)
    },
    native_resources = if (is.null(hydrated$fetch)) {
      0L
    } else {
      count(hydrated$fetch$resources)
    },
    observations = if (is.null(hydrated$harmonized)) {
      0L
    } else {
      count(hydrated$harmonized$observations)
    },
    harmonized_resources = if (is.null(hydrated$harmonized)) {
      0L
    } else {
      count(hydrated$harmonized$resources)
    }
  )
}

gx_package_report_source_text_impl <- function(hydrated, summary) {
  gx_package_hydrated_validate_impl(hydrated)
  valid_summary <- is.list(summary) && identical(
    names(summary), .gx_package_report_summary_fields
  ) && all(vapply(
    summary,
    function(value) {
      is.integer(value) && length(value) == 1L && !is.na(value) &&
        value >= 0L && is.null(attributes(value))
    },
    logical(1)
  ))
  if (!valid_summary || !identical(
    summary, gx_package_report_summary_impl(hydrated)
  )) {
    gx_package_report_abort(
      "The report summary does not bind the verified package view.",
      "gx_error_package_report_source"
    )
  }

  labels <- c(
    sites = "Sites",
    datasets = "Datasets",
    problems = "Problems",
    requests = "Catalog requests",
    fetch_statuses = "Fetch statuses",
    native_resources = "Native resources",
    observations = "Harmonized observations",
    harmonized_resources = "Harmonized resources"
  )
  rows <- vapply(names(summary), function(name) {
    paste0(
      "<tr data-summary=\"", name, "\"><th>", labels[[name]],
      "</th><td>", summary[[name]], "</td></tr>"
    )
  }, character(1))
  lines <- c(
    "---",
    "title: \"Geoconnex data package report\"",
    "lang: en",
    "format:",
    "  html:",
    "    minimal: true",
    "    embed-resources: true",
    "    toc: false",
    "    section-divs: false",
    "execute:",
    "  enabled: false",
    "  cache: false",
    "---",
    "",
    paste0(
      "<main id=\"geoconnexr-report\" data-contract-version=\"",
      .gx_package_report_contract_version, "\" data-hydration-id=\"",
      hydrated$hydration_id, "\" data-stage=\"", hydrated$stage, "\">"
    ),
    "<p>This is a redacted, read-only package summary. It does not authenticate or replay the package.</p>",
    "<table><thead><tr><th>Measure</th><th>Count</th></tr></thead><tbody>",
    rows,
    "</tbody></table>",
    "</main>",
    ""
  )
  text <- paste(lines, collapse = "\n")
  valid <- isTRUE(stringi::stri_enc_isutf8(text)) &&
    !grepl("\r", text, fixed = TRUE) && endsWith(text, "\n") &&
    !grepl("```|`r[[:space:]]|\\{r\\}|\\{python\\}", text, perl = TRUE)
  if (!valid) {
    gx_package_report_abort(
      "The fixed report source could not be constructed safely.",
      "gx_error_package_report_source"
    )
  }
  text
}

gx_package_report_source_impl <- function(hydrated, summary) {
  bytes <- charToRaw(gx_package_report_source_text_impl(hydrated, summary))
  if (length(bytes) < 1L ||
      length(bytes) > .gx_package_report_max_source_bytes) {
    gx_package_report_abort(
      "The fixed report source exceeds its byte contract.",
      "gx_error_package_report_budget"
    )
  }
  list(
    path = "report.qmd",
    media_type = "text/markdown; charset=utf-8",
    bytes = bytes,
    byte_count = unname(as.double(length(bytes))),
    sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE)
  )
}

gx_package_report_render_arguments_impl <- function(input) {
  c(
    "render", input, "--to", "html", "--output", "report.html",
    "--no-execute", "--no-cache", "--quiet"
  )
}

gx_package_report_render_command_impl <- function(
    path,
    input,
    timeout = .gx_package_report_render_timeout) {
  arguments <- gx_package_report_render_arguments_impl(input)
  output <- system2(
    command = path,
    args = vapply(arguments, shQuote, character(1)),
    stdout = TRUE,
    stderr = TRUE,
    timeout = timeout
  )
  status <- attr(output, "status", exact = TRUE)
  if (!is.null(status) && !identical(unname(as.integer(status)), 0L)) {
    stop("The fixed Quarto report command failed.", call. = FALSE)
  }
  unname(as.character(output))
}

gx_package_report_write_raw_impl <- function(path, bytes) {
  gx_snapshot_writer_write_raw(path, bytes)
}

gx_package_report_read_raw_impl <- function(path, max_bytes) {
  before <- gx_snapshot_assert_fs_type(path, "file")
  size <- unname(as.double(before$size[[1L]]))
  if (!is.finite(size) || size < 1 || size != trunc(size) ||
      size > max_bytes) {
    gx_package_report_abort(
      "A report artifact exceeds its fixed byte ceiling.",
      "gx_error_package_report_budget"
    )
  }
  bytes <- gx_package_report_call_safely(
    readBin(path, what = "raw", n = as.integer(size + 1)),
    "A report artifact could not be read as bounded bytes.",
    "gx_error_package_report_io"
  )
  after <- gx_snapshot_assert_fs_type(path, "file")
  same <- tryCatch({
    gx_snapshot_assert_same_info(before, after)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!same || length(bytes) != size) {
    gx_package_report_abort(
      "A report artifact changed during its bounded read.",
      "gx_error_package_report_race"
    )
  }
  bytes
}

gx_package_report_inventory_impl <- function(stage) {
  gx_snapshot_assert_fs_type(stage, "directory")
  entries <- list.files(
    stage,
    all.files = TRUE,
    full.names = FALSE,
    recursive = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  entries <- entries[gx_catalog_byte_order(entries)]
  if (!identical(entries, c("report.html", "report.qmd"))) {
    gx_package_report_abort(
      "The fixed report render produced an unexpected output tree.",
      "gx_error_package_report_inventory"
    )
  }
  for (entry in entries) {
    gx_snapshot_assert_fs_type(file.path(stage, entry), "file")
  }
  invisible(entries)
}

gx_package_report_html_text_impl <- function(bytes) {
  valid <- is.raw(bytes) && !is.object(bytes) && is.null(attributes(bytes)) &&
    length(bytes) >= 1L && length(bytes) <= .gx_package_report_max_output_bytes
  text <- if (valid) tryCatch(rawToChar(bytes), error = function(cnd) NULL) else
    NULL
  bom <- valid && length(bytes) >= 3L && identical(
    as.integer(bytes[seq_len(3L)]), c(239L, 187L, 191L)
  )
  if (is.null(text) || bom || !isTRUE(stringi::stri_enc_isutf8(text)) ||
      grepl("\r", text, fixed = TRUE)) {
    gx_package_report_abort(
      "The report output must be bounded unmarked UTF-8 HTML.",
      "gx_error_package_report_html"
    )
  }
  text
}

gx_package_report_html_structure_impl <- function(bytes, hydrated, summary) {
  gx_package_hydrated_validate_impl(hydrated)
  expected_summary <- gx_package_report_summary_impl(hydrated)
  if (!identical(summary, expected_summary)) {
    gx_package_report_abort(
      "The report output summary no longer binds its package view.",
      "gx_error_package_report_html"
    )
  }
  text <- gx_package_report_html_text_impl(bytes)
  document <- tryCatch(
    xml2::read_html(text, options = c("RECOVER", "NOERROR", "NOWARNING", "NONET")),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(document)) {
    gx_package_report_abort(
      "The report output is not parseable HTML.",
      "gx_error_package_report_html"
    )
  }
  one_text <- function(xpath) {
    nodes <- xml2::xml_find_all(document, xpath)
    if (length(nodes) != 1L) return(NULL)
    trimws(xml2::xml_text(nodes[[1L]]))
  }
  landmark <- xml2::xml_find_all(document, "//*[@id='geoconnexr-report']")
  forbidden <- xml2::xml_find_all(
    document,
    "//script|//iframe|//frame|//object|//embed|//form|//base|//link|//img|//audio|//video|//source|//style[contains(translate(.,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'@import') or contains(translate(.,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'url(')]|//meta[translate(@http-equiv,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')='refresh']"
  )
  reference_nodes <- xml2::xml_find_all(
    document,
    "//*[@href or @src or @action or @poster or @data]"
  )
  references <- character()
  for (attribute in c("href", "src", "action", "poster", "data")) {
    values <- xml2::xml_attr(reference_nodes, attribute)
    references <- c(references, values[!is.na(values) & nzchar(values)])
  }
  values <- vapply(names(summary), function(name) {
    one_text(paste0("//tr[@data-summary='", name, "']/td")) %||% NA_character_
  }, character(1))
  valid <- length(xml2::xml_find_all(document, "/html/head")) == 1L &&
    length(xml2::xml_find_all(document, "/html/body")) == 1L &&
    identical(one_text("/html/head/title"), "Geoconnex data package report") &&
    length(landmark) == 1L && length(forbidden) == 0L &&
    length(references) == 0L &&
    identical(xml2::xml_attr(landmark, "data-contract-version"),
              .gx_package_report_contract_version) &&
    identical(xml2::xml_attr(landmark, "data-hydration-id"),
              hydrated$hydration_id) &&
    identical(xml2::xml_attr(landmark, "data-stage"), hydrated$stage) &&
    identical(unname(values), unname(vapply(summary, as.character, character(1))))
  if (!valid) {
    gx_package_report_abort(
      "The report HTML violates its fixed structure or isolation contract.",
      "gx_error_package_report_html"
    )
  }
  structure <- c(
    list(
      title = "Geoconnex data package report",
      landmark = "geoconnexr-report",
      contract_version = .gx_package_report_contract_version,
      hydration_id = hydrated$hydration_id,
      stage = hydrated$stage
    ),
    summary,
    list(scripts = 0L, external_references = 0L)
  )
  structure
}

gx_package_report_output_impl <- function(bytes, hydrated, summary) {
  structure <- gx_package_report_html_structure_impl(bytes, hydrated, summary)
  list(
    path = "report.html",
    media_type = "text/html; charset=utf-8",
    bytes = bytes,
    byte_count = unname(as.double(length(bytes))),
    sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE),
    structure = structure
  )
}

gx_package_report_metadata_impl <- function(runtime) {
  limitations <- c(
    "distribution_authenticity_unchecked", "package_integration_deferred",
    "public_exposure_deferred", "render_not_replayable"
  )
  limitations <- limitations[gx_catalog_byte_order(limitations)]
  list(
    scope = "fixed_quarto_html_report_v1",
    host_specific = TRUE,
    fixed_source = TRUE,
    execution_enabled = FALSE,
    cache = FALSE,
    format = "html",
    minimal = TRUE,
    embed_resources = TRUE,
    render_command = runtime$path,
    render_arguments = c(
      "render", "report.qmd", "--to", "html", "--output", "report.html",
      "--no-execute", "--no-cache", "--quiet"
    ),
    render_timeout = .gx_package_report_render_timeout,
    closed_output_tree = TRUE,
    source_unchanged = TRUE,
    html_verified = TRUE,
    temporary_stage_removed = TRUE,
    package_integrated = FALSE,
    public = FALSE,
    replayable = FALSE,
    limitations = limitations
  )
}

gx_package_report_id_impl <- function(
    hydrated,
    runtime,
    summary,
    source,
    output,
    metadata) {
  summary_values <- unname(as.list(vapply(
    summary, as.character, character(1)
  )))
  summary_id <- gx_contract_hash(
    summary_values,
    namespace = "geoconnexr.package-report-summary.v1",
    contract_version = .gx_package_report_contract_version
  )
  arguments_id <- gx_contract_hash(
    unname(as.list(metadata$render_arguments)),
    namespace = "geoconnexr.package-report-arguments.v1",
    contract_version = .gx_package_report_contract_version
  )
  gx_contract_hash(
    list(
      "mode", "fixed_quarto_html_report",
      "status", "rendered_and_verified",
      "stage", hydrated$stage,
      "hydration_id", hydrated$hydration_id,
      "cli_id", runtime$cli_id,
      "summary_id", summary_id,
      "source_sha256", source$sha256,
      "output_sha256", output$sha256,
      "render_arguments_id", arguments_id,
      "render_timeout", metadata$render_timeout
    ),
    namespace = "geoconnexr.package-report.v1",
    contract_version = .gx_package_report_contract_version
  )
}

gx_package_report_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_package_report") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_report_fields) &&
    identical(x$contract_version, .gx_package_report_contract_version) &&
    identical(x$mode, "fixed_quarto_html_report") &&
    identical(x$status, "rendered_and_verified") &&
    is.character(x$stage) && length(x$stage) == 1L && !is.na(x$stage) &&
    is.character(x$report_id) && length(x$report_id) == 1L &&
    !is.na(x$report_id) && gx_catalog_is_sha256(x$report_id)
  if (!valid_top) {
    gx_package_report_abort(
      "Quarto report evidence violates its exact top-level contract."
    )
  }
  hydrated_valid <- tryCatch({
    gx_package_hydrated_validate_impl(x$hydrated)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  runtime_valid <- tryCatch({
    gx_package_quarto_cli_validate_impl(x$runtime)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  expected <- if (hydrated_valid && runtime_valid) tryCatch({
    summary <- gx_package_report_summary_impl(x$hydrated)
    source <- gx_package_report_source_impl(x$hydrated, summary)
    output <- gx_package_report_output_impl(
      x$output$bytes, x$hydrated, summary
    )
    metadata <- gx_package_report_metadata_impl(x$runtime)
    list(
      summary = summary,
      source = source,
      output = output,
      metadata = metadata,
      report_id = gx_package_report_id_impl(
        x$hydrated, x$runtime, summary, source, output, metadata
      )
    )
  }, error = function(cnd) NULL, warning = function(cnd) NULL) else NULL
  valid <- !is.null(expected) && identical(x$stage, x$hydrated$stage) &&
    identical(x$summary, expected$summary) &&
    is.list(x$source) && identical(
      names(x$source), .gx_package_report_source_fields
    ) && identical(x$source, expected$source) &&
    is.list(x$output) && identical(
      names(x$output), .gx_package_report_output_fields
    ) && is.list(x$output$structure) && identical(
      names(x$output$structure), .gx_package_report_structure_fields
    ) && identical(x$output, expected$output) &&
    is.list(x$metadata) && identical(
      names(x$metadata), .gx_package_report_metadata_fields
    ) && identical(x$metadata, expected$metadata) &&
    identical(x$report_id, expected$report_id)
  if (!valid) {
    gx_package_report_abort(
      "Quarto report evidence no longer binds its source, runtime, and HTML."
    )
  }
  invisible(x)
}

# Internal M9y boundary. It derives one fixed execution-disabled report source
# from an exact typed package view, renders through the already admitted Quarto
# CLI under fixed controls, verifies a closed minimal HTML output, retains only
# bounded in-memory bytes, and removes its private temporary stage.
gx_package_report_impl <- function(
    hydrated,
    runtime_resolver = gx_package_quarto_cli_impl,
    render_resolver = gx_package_report_render_command_impl,
    temp_parent = tempdir()) {
  if (!is.function(runtime_resolver) || !is.function(render_resolver)) {
    gx_package_report_abort(
      "Report runtime and render resolvers must be functions.",
      "gx_error_package_report_input"
    )
  }
  gx_package_hydrated_validate_impl(hydrated)
  runtime <- gx_package_report_call_safely(
    runtime_resolver(),
    "The reviewed Quarto runtime could not be admitted.",
    "gx_error_package_report_runtime"
  )
  gx_package_quarto_cli_validate_impl(runtime)
  before_cli <- gx_package_quarto_cli_path_admit_impl(runtime$path)
  summary <- gx_package_report_summary_impl(hydrated)
  source <- gx_package_report_source_impl(hydrated, summary)

  parent <- tryCatch(
    normalizePath(temp_parent, winslash = "/", mustWork = TRUE),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(parent)) {
    gx_package_report_abort(
      "The private report staging parent is unavailable.",
      "gx_error_package_report_io"
    )
  }
  gx_snapshot_assert_fs_type(parent, "directory")
  stage <- tempfile(pattern = ".gx-report-stage-", tmpdir = parent)
  owned <- FALSE
  on.exit({
    if (owned) try(gx_snapshot_writer_cleanup_stage(stage), silent = TRUE)
  }, add = TRUE)
  if (!dir.create(stage, mode = "0700", showWarnings = FALSE)) {
    gx_package_report_abort(
      "The private report staging directory could not be created.",
      "gx_error_package_report_io"
    )
  }
  owned <- TRUE
  source_path <- file.path(stage, source$path)
  gx_package_report_call_safely(
    gx_package_report_write_raw_impl(source_path, source$bytes),
    "The fixed report source could not be staged.",
    "gx_error_package_report_io"
  )
  source_info <- gx_snapshot_assert_fs_type(source_path, "file")
  gx_package_report_call_safely(
    render_resolver(
      runtime$path, source_path, .gx_package_report_render_timeout
    ),
    "The fixed Quarto report render failed.",
    "gx_error_package_report_render"
  )
  after_cli <- gx_package_quarto_cli_path_admit_impl(runtime$path)
  gx_package_quarto_cli_assert_same_file_impl(
    before_cli$info, after_cli$info
  )
  source_after <- gx_snapshot_assert_fs_type(source_path, "file")
  source_same <- tryCatch({
    gx_snapshot_assert_same_info(source_info, source_after)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  if (!source_same) {
    gx_package_report_abort(
      "The fixed report source changed during rendering.",
      "gx_error_package_report_race"
    )
  }
  gx_package_report_inventory_impl(stage)
  rebound_source <- gx_package_report_read_raw_impl(
    source_path, .gx_package_report_max_source_bytes
  )
  if (!identical(rebound_source, source$bytes)) {
    gx_package_report_abort(
      "The staged report source no longer matches its fixed bytes.",
      "gx_error_package_report_race"
    )
  }
  output_bytes <- gx_package_report_read_raw_impl(
    file.path(stage, "report.html"), .gx_package_report_max_output_bytes
  )
  output <- gx_package_report_output_impl(output_bytes, hydrated, summary)
  if (!gx_snapshot_writer_cleanup_stage(stage)) {
    gx_package_report_abort(
      "The private report staging directory could not be removed.",
      "gx_error_package_report_cleanup"
    )
  }
  owned <- FALSE
  metadata <- gx_package_report_metadata_impl(runtime)
  object <- structure(
    list(
      contract_version = .gx_package_report_contract_version,
      mode = "fixed_quarto_html_report",
      status = "rendered_and_verified",
      stage = hydrated$stage,
      hydrated = hydrated,
      runtime = runtime,
      summary = summary,
      source = source,
      output = output,
      metadata = metadata,
      report_id = gx_package_report_id_impl(
        hydrated, runtime, summary, source, output, metadata
      )
    ),
    class = "gx_package_report"
  )
  gx_package_report_validate_impl(object)
  object
}
