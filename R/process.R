#=============================================
#=== combined pipeline
#=============================================

#' Title
#'
#'
process <- .  %>%
  consistency %>%
  condense

#=============================================
#=== enforce consistency across levels
#=============================================

#' Title
#'
#' @export
consistency <- function(x) {
  x %>%
  group_by(scenario, country, gender, year, iteration) 	%>%
  # decreasing order in terms of educational hierarchy
  arrange(level) 						                            %>%
  mutate(value = pmax(value, cummax(value)))            %>%
  ungroup
}

#=============================================
#=== summarise
#=============================================

#' Title
#'
#'
condense <- function(df){
  df %>%
  group_by(
	  scenario, gender, level,
	  country, year)            %>%
	summarise(
	  hi = quantile(value, .9),
		lo = quantile(value, .1),
		md = median(value))       %>%
  tidyr::gather(
    variable, value,
    hi, lo, md)               %>%
  ungroup
}

#=============================================
#=== rescale to cap post-secondary at 90%
#=============================================

scale_factors <- function(x) {
  x                               %>%
  group_by(gender, country, level) %>%
  filter(
    status == 'obsv' &
    level  == 'postS')             %>%
  summarise(max_obs = max(attainers/population))  %>%
  mutate(scale_factor =
    (.9 - max_obs)/(1 - max_obs))  %>%
  ungroup
}


#' Title
#'
#'
ceiling <- function(x, obs) {
  x                                       %>%
  mutate(
    rescale_postS =
      level == 'postS' &
      year  >= 2010)                       %>%
  left_join(scale_factors(obs)) %>%
  mutate(value = ifelse(
    rescale_postS & value > max_obs,
    max_obs + pmax(0, (value-max_obs) * scale_factor),
    value
  ))	                                     %>%
  select(-rescale_postS, -max_obs,
         -scale_factor)
}
