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
#'   \item{\code{testing}}{(\code{boolean};1) Whether to run only a single chain for a few iterations
#'   for testing purposes.}
#' }
#' @export
wicedproj_options <- function(...) {
  # check against use of reserved words
  settings::stop_if_reserved(...)
  edprojOPTS(...)
}
