# Lines before the first chunk are invisible to Rmd/Rnw callers
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below. 
base::source("./scripts/common-functions.R")

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
library(magrittr) # pipes
library(dplyr)    # disable when temp lines are removed
library(ggplot2)  # graphs
library(ggpubr)   # documents

# ---- declare-globals ---------------------------------------------------------
path_file_input <- "./data-unshared/derived/9-combined.rds"
html_flip <- FALSE
baseSize <- 10
# ---- load-data ---------------------------------------------------------------
dto      <- readRDS(path_file_input)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()

# ----- custom-functions --------------------------------------

# ---- tweak-data ---------------------------------------------------------------
# to collapse into a single data frame
ds <- dto[["granularity_population"]] %>% 
  Reduce(function(a , b) dplyr::left_join( a, b ), . )

ds %>% explore::describe_all()

d <- ds %>% 
  dplyr::filter(county == "Lake") %>% 
  dplyr::filter(year == 2015)



# to help us filter out those counties that had programming
counties_gls <- ds %>% 
  na.omit(region) %>% 
  dplyr::distinct(county) %>% 
  as.list() %>% unlist() %>% as.character()
# to view the total programming delivered (between 2015 and 2017)
ds %>% 
  dplyr::filter(county %in% counties_gls) %>% 
  dplyr::distinct(county, year, community, professionals ) %>% 
  na.omit() %>% 
  dplyr::group_by(county) %>% 
  dplyr::summarize(
    community      = sum(community)
    ,professionals = sum(professionals)
  ) %>% 
  dplyr::arrange(desc(professionals))
# to aid interpretation and graphing
ds <- ds %>% 
  dplyr::rename(
    "deaths_by_suicide" = "resident_deaths" # to remind what we count
  ) %>% 
  dplyr::mutate(
    # to have a standardized measure / put counties on the same scale
    suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
    # to have a single variable describing racial background
    ,racethnicity = paste0(race," + ", ethnicity)
    # to aid in graph production ( note the spaces at the end of "NE  ")
    ,rgn = car::recode(
      region,
      "
      'central'  ='CN'
      ;'southeast'='SE'
      ;'northeast'='NE  '
      "
    )
  ) %>% 
  dplyr::select(county, year, sex, age_group, race, ethnicity, racethnicity, # context
                region, rgn, # support for graphing and grouping
                population_count, deaths_by_suicide, suicide_rate_per100k, #measures
                community, professionals # treatment
  )

ds %>% explore::describe_all()

ds <- ds %>% 
  dplyr::filter(county == "Lake") %>% 
  dplyr::filter(year == 2015)
# to remind out how to aggregate 
# the most granular level includes (6):
# county, year, sex, age_group, race, ethnicity
d1 <- ds %>% 
  # apply filters to better understand the structure of the data
  # dplyr::filter(county     %in% c("Orange") ) %>%
  # dplyr::filter(year       %in% c("2015")   ) %>%
  # dplyr::filter(sex        %in% c("Male")   ) %>%
  # dplyr::filter(age_group  %in% c("15_19")  ) %>%
  # dplyr::filter(race       %in% c("White")  ) %>%
  # dplyr::filter(ethnicity %in% c("Non-Hispanic") )%>%
  # dplyr::filter(ethnicity %in% c("Hispanic","Non-Hispanic") )%>%
  # dplyr::group_by(county, year, sex, age_group, race, ethnicity) %>% # no aggregation
  # to exemplify useful aggregates:
  dplyr::group_by(county, year, professionals, community                            ) %>%
  # dplyr::group_by(county, year, sex                          ) %>%
  # dplyr::group_by(county, year, sex, age_group               ) %>%
  # dplyr::group_by(county, year, sex, racethnicity                  ) %>%
  dplyr::summarize(
    population_count   = sum(population_count,   na.rm = T)
    ,deaths_by_suicide = sum(deaths_by_suicide,  na.rm = T)
    # ,professionals     = sum(professionals,      na.rm = T)
    # ,community         = sum(community,          na.rm = T)
  ) 
# use the code for preparing data for custom graphs
# d1 %>% glimpse(60)
# d1 %>% explore::describe()


# ---- x1 -------------------------------
# find the match for selected counties
# to help us filter out those counties that had programming
counties_gls <- ds %>% 
  distinct(county,region) %>% # those who have region had programming
  na.omit() %>%
  dplyr::distinct(county) %>% 
  as.list() %>% unlist() %>% as.character()

d1 <- ds %>% 
  dplyr::filter(county == "Lake") %>% 
  # dplyr::filter(year %in% 2015:2017) %>% 
  dplyr::filter(year %in% 2015) %>%
  dplyr::mutate(
    county_gls = ifelse(county %in% counties_gls, TRUE, FALSE)
  ) %>% 
  group_by(county, year, county_gls) %>% 
  dplyr::summarize(
    population_count   = sum(population_count,   na.rm = T)
    ,deaths_by_suicide = sum(deaths_by_suicide,  na.rm = T)
    ,professionals     = sum(professionals,      na.rm = T)
    ,community         = sum(community,          na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    suicide_rate_per100k         = (deaths_by_suicide / population_count) *100000
    ,community_reach_per100k     = (community/ population_count) * 100000
    ,professionals_reach_per100k = (professionals/ population_count) * 100000
  )

# following https://rpsychologist.com/r-guide-longitudinal-lme-lmer

dv <- "suicide_rate_per100k"
# dv <- "deaths_by_suicide"
# model_equation <-  paste0( dv, " ~ 1 + community + professionals + ( year | county)")
# model_equation <-  paste0( dv, " ~ 1 +  county_gls + ( 1 | county)")
model_equation <-  paste0( dv, " ~ 1 + year + ( year  | county)")

eq_formula <- as.formula(model_equation)
# model_object <- lme4::lmer(eq_formula, data = ds)
model_object <- lme4::lmer(eq_formula, data = d1)

summary(model_object)

# adjust the following custom models to work with lmer objects
basic_model_info(model_object)
make_result_table(model_object)


# ---- publish ---------------------------------
rmarkdown::render(
  # input = "./analysis/gls-activity/gls-activity-1.Rmd"
  input = "./analysis/trend-summary/trend-summary.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)