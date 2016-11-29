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

#' Pre-process Wittgenstein DataExplorer extract
#'
#' Function \code{wic_prepare} puts data from the Wittgenstein DataExplorer extract
#' into the correct input format for the education projection. For the most part,
#' this involves straightforward operations, with two exceptions noted in the details below.
#'
#' When dealing with real historic data, the number of \code{attainers} at a given level
#' out of a \code{population} of a given size can be provided as observed,
#' and the estimation will take sample variation duly into account.
#' However, in the application to \emph{reconstructed} attainment data, the error in a given
#' observation is not necessarily related to population size. All observations are therefore
#' treated as referring to a population of size 10,000.
#'
#' The raw data extract contains entries for all age groups. The education projection
#' model uses the age group 30-34 as a reference age group. However, presently it is
#' assumed that post-secondary and upper secondary attainment is largely complete by age 25-29,
#' lower secondary by 20--24, and primary by 15--19.
#' @export
wic_prepare <- function(x) {
  x %>%
  ungroup %>%
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
    region = factor(region)) %>%
  age_thresholding %>%
  select(-baseline)
}


get_data_countries <- function(x) {
  x %>%
  select(country, region) %>%
  distinct %>%
  arrange(country, region)
}

#' Augment observed attainment data with "future observations" as targets
#'
#' Function \code{add_target} is a convenience wrapper to simplify the
#' specification of (e.g. SDG) targets for educational expansions.
#'
#' @param . The dataframe of observed attainment to which the target should be added
#' @param lvl The target attainment level
#' @param tgt The target share as a fraction
#' @param by_yr The year by which the target is reached
#' @export
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
