#=============================================
#=== bound target from below by trend
#=============================================

#' Title
#'
#'
# floor <- function(df) {
#   df                                   %>%
#     group_by(
#       gender, level, country, year) %>%
#     mutate(
#       value = pmax(
#         lo[which(type == 'target')],
#         md[which(type == 'trend')]))
# }
