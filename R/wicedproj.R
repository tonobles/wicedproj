#' \code{wicedproj}: A package implementing the
#' Education Projection Model of the Wittgenstein Centre for Demography and Global Human Capital.
#'
#' @import dplyr
#' @import dtplyr
#' @docType package
#' @name wicedproj
NULL

#' Title
#'
#' @param df
#'
#' @export
edproj <- function(x) {
  x %>%
  estimate %>%
  process %>%
  ceiling(x) %>%
  export4popproj %>%
  identity
}

popN <- 1e4
