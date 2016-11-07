baseline_years <-
  readr::read_csv('data/backproj_baseline_year.csv',
                  col_types = 'cii')	%>%
  select(Area = country, baseline = Max_year) %>%
  mutate(
    Area = ifelse(Area == 'United Kingdom', 'United Kingdom of Great Britain and Northern Ireland', Area),
    Area = ifelse(Area == 'TFYR Macedonia', 'The former Yugoslav Republic of Macedonia', Area),
    Area = ifelse(Area == 'China, Macao SAR', 'Macao Special Administrative Region of China', Area),
    Area = ifelse(Area == 'China, Hong Kong SAR', 'Hong Kong Special Administrative Region of China', Area),
    Area = ifelse(Area == 'Bolivia', 'Bolivia (Plurinational State of)', Area)
  )

wicdf <-
  readr::read_csv('data/wicdf.csv',
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

data_countries <- get_data_countries(wicdf)

sdg_trgt <-
  wicdf										    %>%
  select(gender, level,
         region, country)			%>%
  distinct 										%>%
  filter(level %in% c('upS')) %>%
  mutate(
    year       = 2045,
    population = popN,
    attainers  = .97 * popN
  )

sdg_proj <-
  wicdf %>%
  add_target('upS', .97, 2030) %>%
  edproj
save(sdg_proj, file = 'sdg_proj.rda')

low_proj <-
  wicdf %>%
  add_target('loS', .97, 2030) %>%
  edproj
save(low_proj, file = 'low_proj.rda')

efa_proj <-
  wicdf %>%
  filter(year <= 2000) %>%
  add_target('P', .97, 2015) %>%
  edproj
save(efa_proj, file = 'sdg_proj.rda')

ssa_proj <-
  wicdf %>%
  filter(
    region != 'Sub-Saharan Africa' |
    year <= 1980) %>%
  add_target('P', .97, 2015) %>%
  edproj
save(ssa_proj, file = 'ssa_proj.rda')
