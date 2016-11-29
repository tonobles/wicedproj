# This script demonstrates the use of the \code{wicedproj} package using data from the
# Wittgenstein Centre's database of reconstructed educational attainment time series.
# A copy of the extract is included with the package as there is currently no R API to
# access the Wittgenstein DataExplorer automatically. The data files are in the same
# directory as this script. Either adapt the paths below, or set the working directory
# accordingly.

library(wicedproj)

# Set the 'testing' option to FALSE (or comment out the following line) in order to
# run the full estimation, which can take several hours.
wicedproj_options(testing = TRUE)

# In the Wittgenstein DataExplorer, reconstructed data for all countries is provided with 2010 as a baseline year.
# For some countries where the most recent available raw data is from before 2010, this already involves
# a certain amount of imputation. These imputed values are to be ignored for the purpose of estimating past trends.
# At the same time, some normalisation of country names is performed.
baseline_years <-
  readr::read_csv('backproj_baseline_year.csv',
                  col_types = 'cii') %>%
  select(Area = country, baseline = Max_year) %>%
  mutate(
    Area = ifelse(Area == 'United Kingdom', 'United Kingdom of Great Britain and Northern Ireland', Area),
    Area = ifelse(Area == 'TFYR Macedonia', 'The former Yugoslav Republic of Macedonia', Area),
    Area = ifelse(Area == 'China, Macao SAR', 'Macao Special Administrative Region of China', Area),
    Area = ifelse(Area == 'China, Hong Kong SAR', 'Hong Kong Special Administrative Region of China', Area),
    Area = ifelse(Area == 'Bolivia', 'Bolivia (Plurinational State of)', Area)
  )

# pre-process Data from the Wittgenstein DataExplorer
# to bring into correct input format for education projection model
wicdf <-
  readr::read_csv('wicdf.csv',
                  skip = 8,
                  col_types = 'cicccd') %>%
  # truncate at true baseline data year
  # on a per-country basis
  left_join(baseline_years,
            by = 'Area')            %>%
  group_by(Area)                    %>%
  filter(
    Year <= baseline &
    min(Year) == 1970)              %>%
  # pre-processing
  wic_prepare

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
