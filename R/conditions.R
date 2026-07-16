gx_abort <- function(message, class = "gx_error", ..., call = rlang::caller_env(),
                     .redact_trace = FALSE) {
  format_env <- if (is.environment(call)) call else rlang::caller_env()
  .redact_trace <- .redact_trace ||
    any(startsWith(class, "gx_error_jsonld")) ||
    any(startsWith(class, "gx_error_parser")) ||
    any(startsWith(class, "gx_error_reference")) ||
    any(startsWith(class, "gx_error_crosswalk")) ||
    any(startsWith(class, "gx_error_graph")) ||
    any(startsWith(class, "gx_error_fetch_plan")) ||
    any(startsWith(class, "gx_error_fetch_preflight")) ||
    any(startsWith(class, "gx_error_aoi_recipe")) ||
    any(startsWith(class, "gx_error_catalog")) ||
    any(startsWith(class, "gx_error_snapshot")) ||
    any(startsWith(class, "gx_error_asset")) ||
    any(startsWith(class, "gx_error_download"))
  if (any(startsWith(class, "gx_error_jsonld")) &&
      !"gx_error_jsonld" %in% class) {
    class <- c(class, "gx_error_jsonld")
  }
  if (.redact_trace) {
    trace <- rlang::trace_back()
    trace <- trace[0, , drop = FALSE]
    cli::cli_abort(
      message,
      class = c(class, "gx_error"),
      ...,
      call = NULL,
      trace = trace,
      .envir = format_env
    )
  } else {
    cli::cli_abort(
      message,
      class = c(class, "gx_error"),
      ...,
      call = call,
      .envir = format_env
    )
  }
}
