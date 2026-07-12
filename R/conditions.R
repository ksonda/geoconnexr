gx_abort <- function(message, class = "gx_error", ..., call = rlang::caller_env()) {
  cli::cli_abort(
    message,
    class = c(class, "gx_error"),
    ...,
    call = call,
    .envir = call
  )
}
