gx_empty_diagnostics <- function() {
  tibble::tibble(
    severity = character(),
    code = character(),
    path = character(),
    message = character(),
    recoverable = logical()
  )
}

gx_diagnostic <- function(severity, code, path, message, recoverable = TRUE) {
  tibble::tibble(
    severity = as.character(severity),
    code = as.character(code),
    path = as.character(path),
    message = as.character(message),
    recoverable = as.logical(recoverable)
  )
}

gx_bind_diagnostics <- function(...) {
  inputs <- list(...)
  inputs <- inputs[!vapply(inputs, is.null, logical(1))]
  if (!length(inputs)) {
    return(gx_empty_diagnostics())
  }
  do.call(rbind, inputs)
}

gx_strict_diagnostics <- function(diagnostics, strict, stage) {
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    gx_abort("{.arg strict} must be one non-missing logical value.", "gx_error_parser")
  }
  blocking <- diagnostics$severity %in% c("warning", "error")
  if (strict && any(blocking)) {
    first <- diagnostics[which(blocking)[[1]], , drop = FALSE]
    gx_abort(
      "Strict {stage} parsing stopped at {first$code}: {first$message}",
      "gx_error_parser_strict"
    )
  }
  invisible(diagnostics)
}
