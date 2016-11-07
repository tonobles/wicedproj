#=============================================
#=== combined pipeline
#=============================================

#' Title
#'
#'
export4popproj <- .     %>%
  extract_1future       %>%
  uncumulate            %>%
  backproject           %>%
  calc_noed             %>%
  data.frame

#=============================================
#=== median future projections only
#=============================================

#' Title
#'
#'
extract_1future <- function(x) {
  x %>%
  filter(
    variable == ifelse(scenario == 'TGT', 'lo', 'md') &
    year     >= 2010)  %>%
  select(-variable)
}

#=============================================
#=== 'at least ed x' -> 'exactly ed x'
#=============================================

#' Title
#'
#'
uncumulate <- function(x) {
  x							                   %>%
  group_by(
    scenario, year, country, gender) %>%
  arrange(level)								     %>%
  mutate(value =
    value - lag(value, default = 0)) %>%
  ungroup
}


#=============================================
#=== back-project to lower ages
#=============================================

schedules <- array(dim = c(5, 4, 5), dimnames = list(
  currentEd = c("postS", "upS", "loS", "P", "someP"),
  age       = c('15-19', '20-24', '25-29', '30-34'),
  finalEd   = c("postS", "upS", "loS", "P", "someP")))

sched_matrix <- function(m) matrix(data = m, nrow = 5, byrow = TRUE)

schedules[ , , 'postS'] <- sched_matrix(c(
  0,  70, 100, 100,
  40,  30,   0,   0,
  60,   0,   0,   0,
  0,   0,   0,   0,
  0,   0,   0,   0
)/100)
schedules[ , , 'upS'] <- sched_matrix(c(
  0,   0,   0,   0,
  40, 100, 100, 100,
  60,   0,   0,   0,
  0,   0,   0,   0,
  0,   0,   0,   0
)/100)
schedules[ , , 'loS'] <- sched_matrix(c(
  0,   0,   0,   0,
  0,   0,   0,   0,
  100, 100, 100, 100,
  0,   0,   0,   0,
  0,   0,   0,   0
)/100)
schedules[ , , 'P'] <- sched_matrix(c(
  0,   0,   0,   0,
  0,   0,   0,   0,
  0,   0,   0,   0,
  100, 100, 100, 100,
  0,   0,   0,   0
)/100)
schedules[ , , 'someP'] <- sched_matrix(c(
  0,   0,   0,   0,
  0,   0,   0,   0,
  0,   0,   0,   0,
  0,   0,   0,   0,
  100,100,100, 100
)/100)

schedules <-
  schedules                                 %>%
  reshape2::melt(value.name = 'fctr')   		%>%
  mutate(
    level      = factor(finalEd),
    year_shift = 5 * (as.numeric(age) - 4)) %>%
  select(-finalEd)	                        %>%
  data.frame

#' Title
#'
#'
backproject <- function(x) {
	x					                       %>%
  left_join(schedules, by = 'level') %>%
  mutate(
    value = fctr * value,
    level = currentEd,
    year  = year + year_shift)			 %>%
  group_by(scenario, year, country,
    gender, age, level)						   %>%
  summarise(value = sum(value))      %>%
  ungroup
}

#=============================================
#=== residual 'no education' category
#=============================================

#' Title
#'
#'
calc_noed <- function(x) {
  x %>%
  tidyr::spread(level, value)				 %>%
  mutate(none = 1 -
    (postS + upS + loS + P + someP)) %>%
  mutate_each(
    funs(round(., 3)),
    none, someP, P, loS, upS, postS)
}
