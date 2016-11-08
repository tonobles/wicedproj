edprojOPTS <- settings::options_manager(
  testing = FALSE
)

#' Set or get options for oostypo package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported:
#' \itemize{
#'   \item{\code{a}}{(\code{numeric};1) The value of a}
#' }
wicedproj_options <- function(...) {
  # check against use of reserved words
  settings::stop_if_reserved(...)
  edprojOPTS(...)
}
