.gx_endpoint_defaults <- c(
  graph = "https://graph.geoconnex.us/",
  reference = "https://reference.geoconnex.us",
  pid = "https://geoconnex.us",
  nldi = "https://api.water.usgs.gov/nldi/linked-data"
)

#' Effective Geoconnex service endpoints
#'
#' Returns the configured service endpoints, including the USGS NLDI base used
#' by hydrologic crosswalks. These values are defaults, not a guarantee that an
#' upstream service is available or stable. Override them
#' with `options(geoconnexr.endpoint_<name> = "...")`.
#'
#' @return A named character vector with `graph`, `reference`, `pid`, and
#'   `nldi`.
#' @export
gx_endpoints <- function() {
  out <- vapply(
    names(.gx_endpoint_defaults),
    function(name) {
      getOption(
        paste0("geoconnexr.endpoint_", name),
        .gx_endpoint_defaults[[name]]
      )
    },
    character(1)
  )

  invalid <- !vapply(out, gx_is_http_uri, logical(1))
  if (any(invalid)) {
    gx_abort(
      "Configured endpoint{?s} {paste(names(out)[invalid], collapse = ', ')} must be absolute HTTP(S) URIs.",
      "gx_error_option"
    )
  }

  out
}
