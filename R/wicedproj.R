#' \code{wicedproj}: A package for [TO COMPLETE]
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
  x           %>%
  estimate     %>%
  process      %>%
  ceiling(x)  %>%
  export4popproj %>%
  identity
}

popN <- 1e4
