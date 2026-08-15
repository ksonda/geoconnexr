gx_utc_microsecond_text_impl <- function(x) {
  seconds <- unname(as.numeric(x))
  whole <- floor(seconds)
  microseconds <- round((seconds - whole) * 1e6)
  if (microseconds >= 1e6) {
    whole <- whole + 1
    microseconds <- microseconds - 1e6
  }
  base <- as.POSIXct(whole, origin = "1970-01-01", tz = "UTC")
  paste0(
    format(base, "%Y-%m-%dT%H:%M:%S", tz = "UTC", usetz = FALSE),
    sprintf(".%06dZ", as.integer(microseconds))
  )
}
