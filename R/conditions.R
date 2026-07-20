gx_abort <- function(message, class = "gx_error", ..., call = rlang::caller_env(),
                     .redact_trace = FALSE) {
  format_env <- if (is.environment(call)) call else rlang::caller_env()
  .redact_trace <- .redact_trace ||
    any(startsWith(class, "gx_error_jsonld")) ||
    any(startsWith(class, "gx_error_parser"))
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
