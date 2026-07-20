gx_lookup_fixture <- function(name) {
  testthat::test_path("..", "fixtures", "crosswalk", name)
}

gx_lookup_test_spec <- function(
    path = gx_lookup_fixture("nhdpv2-lookup-v3.2.sample.csv"),
    forward_cardinality = "zero_or_one",
    known_answers = tibble::tibble(
      comid = c("17789327", "13637491"),
      mainstem_uri = c(
        "https://geoconnex.us/ref/mainstems/1622734",
        "https://geoconnex.us/ref/mainstems/323742"
      )
    ),
    known_absent = "999999999") {
  lines <- readLines(path, warn = FALSE)
  list(
    lookup_id = "fixture-nhdpv2-v1",
    release = "fixture-v1",
    tag_commit = paste(rep("a", 40L), collapse = ""),
    asset_id = "1",
    asset_name = "nhdpv2_lookup.csv",
    source_url = "https://example.org/releases/fixture/nhdpv2_lookup.csv",
    release_url = "https://example.org/releases/fixture",
    repository_url = "https://example.org/repository",
    allowed_hosts = c("example.org", "cdn.example.org"),
    media_types = c("text/csv", "application/octet-stream"),
    encoding = "UTF-8",
    line_ending = "LF",
    final_newline = TRUE,
    columns = c("uri", "comid"),
    rows = as.integer(length(lines) - 1L),
    bytes = as.integer(file.info(path)$size[[1]]),
    sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE),
    forward_cardinality = forward_cardinality,
    active_state = "non_superseded_at_release",
    license = "CC0-1.0",
    known_answers = known_answers,
    known_absent = known_absent,
    registry_version = 1L
  )
}

gx_lookup_mock_spec <- function(spec, .env = rlang::caller_env()) {
  testthat::local_mocked_bindings(
    gx_mainstem_lookup_spec = function(version = spec$release) spec,
    .package = "geoconnexr",
    .env = .env
  )
}

gx_lookup_install_fixture <- function(spec = gx_lookup_test_spec(),
                                      path = gx_lookup_fixture(
                                        "nhdpv2-lookup-v3.2.sample.csv"
                                      ),
                                      data_dir = withr::local_tempdir(.local_envir = rlang::caller_env()),
                                      .env = rlang::caller_env()) {
  gx_lookup_mock_spec(spec, .env = .env)
  gx_mainstem_lookup_install(
    source = "file",
    file = path,
    version = spec$release,
    confirm = FALSE,
    offline = TRUE,
    data_dir = data_dir
  )
}

gx_lookup_test_file_performer <- function(handler) {
  force(handler)
  function(request, path) {
    response <- handler(request)
    body <- response$body %||% raw()
    connection <- file(path, open = "wb")
    on.exit(close(connection), add = TRUE)
    writeBin(body, connection, useBytes = TRUE)
    list(
      status = response$status,
      headers = response$headers %||% list(),
      url = request$url,
      path = path
    )
  }
}
