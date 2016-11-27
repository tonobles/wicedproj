#' Title
#'
#'
#' When dealing with real historic data, the number of \code{attainers} at a given level
#' out of a \code{population} of a given size can be provided as observed,
#' and the estimation will take sample variation duly into account.
#' However, in the application to *reconstructed* attainment data, the error in a given
#' observation is not necessarily related to population size.
#'
#' @param df Input data of (reconstructed) attainment. Assumed to contain
#'   variables:
#'   \code{country} (factor),
#'   \code{region} (factor),
#'   \code{gender} (factor),
#'   \code{age_group} (factor),
#'   \code{year} (numeric),
#'   \code{level} (factor, increasing order, including residual category),
#'   \code{attainers} (numeric) see details below,
#'   \code{population} (numeric) see details below.
#'
#' @export
wic_prepare <- function(x) {
  x %>%
  filter(!Education %in% c('Total', 'Under 15')) 	%>%
  transmute(
    baseline,
    gender 	  = factor(ifelse(Sex == 'Female', 1, 2), labels = c('female', 'male')),
    country   = Area,
    year 	    = Year,
    age_group = factor(Age),
    #
    # levels in REVERSE ORDER!
    level 	  = ordered(Education,
                       levels = rev(c(
                         'No Education',
                         'Incomplete Primary',
                         'Primary',
                         'Lower Secondary',
                         'Upper Secondary',
                         'Post Secondary'))),
    attainers = Population) 					%>%
  mutate(level = forcats::fct_recode(level,
    'none'  = 'No Education',
    'someP' = 'Incomplete Primary',
    'P'     = 'Primary',
    'loS'   = 'Lower Secondary',
    'upS'   = 'Upper Secondary',
    'postS' = 'Post Secondary'
  )) %>%
  inner_join(countries, by = 'country')		%>%
  group_by(country, year, gender, age_group) 		%>%
  #
  # levels already in decreasing order!
  arrange(level)	 								%>%
  mutate(
    attainers  = popN *
      cumsum(attainers)/sum(attainers),
    population = popN) 							%>%
  #
  # no need to relevel,
  # since 'e1' has the highest index
  filter(level != 'none') 		%>%
  mutate(level = forcats::fct_drop(level)) %>%
  na.omit 										%>%
  ungroup                     %>%
  mutate(
    country = factor(country),
    region = factor(region))
}

#' @export
age_thresholding <- function(x) {
  bind_rows(
    x %>% filter(age_group == '30--34'),
    x %>%
      filter(
        year == baseline &
        age_group == '25--29') %>%
      mutate(year = year + 5),
    x %>%
      filter(
        year == baseline &
        age_group == '20--24' &
        level %in% c('someP', 'P', 'loS')) %>%
      mutate(year = year + 10),
    x %>%
      filter(
        year == baseline &
          age_group == '15--19' &
          level %in% c('someP', 'P')) %>%
      mutate(year = year + 15)
  ) %>%
  select(-age_group)
}

#' @export
get_data_countries <- function(x) {
  x %>%
  select(country, region) %>%
  distinct %>%
  arrange(country, region)
}

add_target <- function(., lvl, tgt, by_yr) {
  bind_rows(
    obsv = .,
    trgt = select(., gender, level,
                  region, country)		%>%
           distinct 									%>%
           filter(level %in% c(lvl))  %>%
           mutate(
             year       = by_yr + 15,
             population = popN,
             attainers  = tgt * popN
      ),
    .id = 'status') 	%>%
    arrange(desc(status))								%>%
    distinct(gender, level, region, country, year, .keep_all = TRUE)
}
