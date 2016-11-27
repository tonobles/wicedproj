# This script demonstrates the use of the \code{wicedproj} package using data from the
# Wittgenstein Centre's database of reconstructed educational attainment time series.
# A copy of the extract is included with the package as there is currently no R API to
# access the Wittgenstein DataExplorer automatically.

library(wicedproj)

# In the Wittgenstein DataExplorer, reconstructed data for all countries is provided with 2010 as a baseline year.
# For some countries where the most recent available raw data is from before 2010, this already involves
# a certain amount of imputation. These imputed values are to be ignored for the purpose of estimating past trends.
# At the same time, some normalisation of country names is performed.
baseline_years <-
  readr::read_csv('backproj_baseline_year.csv',
                  col_types = 'cii')	%>%
  select(Area = country, baseline = Max_year) %>%
  mutate(
    Area = ifelse(Area == 'United Kingdom', 'United Kingdom of Great Britain and Northern Ireland', Area),
    Area = ifelse(Area == 'TFYR Macedonia', 'The former Yugoslav Republic of Macedonia', Area),
    Area = ifelse(Area == 'China, Macao SAR', 'Macao Special Administrative Region of China', Area),
    Area = ifelse(Area == 'China, Hong Kong SAR', 'Hong Kong Special Administrative Region of China', Area),
    Area = ifelse(Area == 'Bolivia', 'Bolivia (Plurinational State of)', Area)
  )

# pre-process Data from the Wittgenstein DataExplorer
wicdf <-
  readr::read_csv('wicdf.csv',
                  skip = 8,
                  col_types = 'cicccd') %>%
  left_join(baseline_years,
            by = 'Area')            %>%
  group_by(Area)                    %>%
  filter(
    Year <= baseline &
    min(Year) == 1970)              %>%
  ungroup                           %>%
  wic_prepare                       %>%
  age_thresholding                  %>%
  select(-baseline)

# list of countries that actually entered the estimation
data_countries <- get_data_countries(wicdf)

# SDG scenario of
# universal UPPER secondary by 2030
sdg_proj <-
  wicdf %>%
  add_target('upS', .97, 2030) %>%
  edproj
save(sdg_proj, file = 'sdg_edu_proj.rda')

# alternative scenario of
# universal LOWER secondary by 2030
low_proj <-
  wicdf %>%
  add_target('loS', .97, 2030) %>%
  edproj
save(low_proj, file = 'low_edu_proj.rda')
