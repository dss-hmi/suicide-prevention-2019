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
  dplyr::group_by(county, year, sex, age_group, race, ethnicity) %>% # no aggregation
  # to exemplify useful aggregates:
  # dplyr::group_by(county, year                               ) %>%
  # dplyr::group_by(county, year, sex                          ) %>%
  # dplyr::group_by(county, year, sex, age_group               ) %>%
  # dplyr::group_by(county, year, sex, racethnicity                  ) %>%
  dplyr::summarize(
    population_count   = sum(population_count,   na.rm = T)
    ,deaths_by_suicide = sum(deaths_by_suicide,  na.rm = T)
    ,professionals     = sum(professionals,      na.rm = T)
    ,community         = sum(community,          na.rm = T)
  ) 
# use the code for preparing data for custom graphs
# d1 %>% glimpse(60)
# d1 %>% explore::describe()

# ---- g0 -------------------------------
#
d1 <- ds %>% 
  # dplyr::filter(county %in% c("Orange")) %>% 
  dplyr::filter(county %in% c("Lake")) %>%
  dplyr::filter(age_group %in% c("10_14","15_19","20_24")) %>%
  dplyr::filter(year %in% c(2011:2017)) %>% 
  # dplyr::filter(age_group %in% c("20_24")) %>%
  # dplyr::filter(age_group %in% c("15_19")) %>%
  # dplyr::filter(age_group %in% c("10_14")) %>% 
  dplyr::group_by(county, year, sex, race, ethnicity, racethnicity) %>%
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    # produces the same results as within group_by() summarization
    # suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
    years_since_2000 = as.integer(as.integer(year) - 2000) # for short label
    # ,sex = factor(sex, levels = c("Male","Female") )
    ,sex = factor(sex, levels = c("Female","Male") )
  ) 
d1 %>% explore::describe()

g1 <- d1 %>% 
  # dplyr::filter(year == 2016) %>% 
  dplyr::mutate(
    sexracethnicity = paste0(sex,"+",racethnicity)
  ) %>%
  ggplot(aes(
    x     = year
    ,y    = suicide_rate_per100k
    ,fill = racethnicity
    ,color = racethnicity
  ))+
  geom_point(aes(shape = sex), size = 4, color = "black")+
  geom_line(aes(group = sexracethnicity,color = racethnicity), alpha = 1, size =1.4)+
  # geom_point(aes(shape = sex,color = racethnicity), size = 4)+
  # geom_point(aes(shape = sex), size = 4, color = "black")+
  # geom_line(aes(group = sex*racethnicity))+
  scale_shape_manual(values = c("Male" = 25, "Female" = 24 ))+
  # scale_fill_viridis_d()+
  # scale_color_viridis_d()+
  theme_minimal()+
  guides(fill = "legend", color = "legend")
g1

# ---- g1 -------------------------------
trend_summary <- function(
  d
  ,county_i
){
  
  d1 <- d %>% 
    # dplyr::filter(county %in% c("Orange")) %>% 
    dplyr::filter(county %in% c(county_i)) %>%
    dplyr::filter(age_group %in% c("10_14","15_19","20_24")) %>%
    dplyr::filter(year %in% c(2006:2017)) %>% 
    # dplyr::filter(age_group %in% c("20_24")) %>%
    # dplyr::filter(age_group %in% c("15_19")) %>%
    # dplyr::filter(age_group %in% c("10_14")) %>% 
    dplyr::group_by(county, year, sex, race, ethnicity, racethnicity) %>%
    dplyr::summarize(
      population_count      = sum(population_count,   na.rm = T)
      ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
      ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(
      # produces the same results as within group_by() summarization
      # suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
      years_since_2000 = as.integer(as.integer(year) - 2000) # for short label
      # ,sex = factor(sex, levels = c("Male","Female") )
      ,sex = factor(sex, levels = c("Female","Male") )
    ) 
  d1 %>% explore::describe()
  
  g_out <- d1 %>% 
    # dplyr::filter(year == 2016) %>% 
    dplyr::mutate(
      sexracethnicity = paste0(sex,"+",racethnicity)
    ) %>%
    ggplot(aes(
      x     = year
      # ,y    = suicide_rate_per100k
      ,y    = deaths_by_suicide
      ,fill = racethnicity
      ,color = racethnicity
    ))+
    # geom_point(aes(shape = sex), size = 4, color = "black")+
    geom_line(aes(group = sexracethnicity,color = racethnicity), alpha = 1, size =1.4)+
    # geom_point(aes(shape = sex,color = racethnicity), size = 4)+
    # geom_point(aes(shape = sex), size = 4, color = "black")+
    # geom_line(aes(group = sex*racethnicity))+
    # scale_shape_manual(values = c("Male" = 25, "Female" = 24 ))+
    # scale_fill_viridis_d()+
    # scale_color_viridis_d(end = .8, option = "plasma")+
    # scale_color_viridis_d(end = .8, option = "inferno")+
    # scale_color_viridis_d(end = .8, option = "magma")+
    geom_point(shape = 21, color = "black", size = 2)+
    theme_minimal()+
    facet_grid(sex ~ .)+
    scale_y_continuous(breaks = seq(0,20,2),limits = c(0,15))+
    guides(fill = "legend", color = "legend")+
    labs(title = paste0(toupper(county_i),": Suicide rates per 100k among 10-24 yo"))
  return(g_out)
}
ds %>% trend_summary("Broward")
# how to use
ds %>% trend_summary("Orange")
ds %>% trend_summary("Saint Lucie")
ds %>% trend_summary("Volusia")
ds %>% trend_summary("Seminole")
ds %>% trend_summary("Lake")
ds %>% trend_summary("Palm Beach")
ds %>% trend_summary("Brevard")
ds %>% trend_summary("Martin")
ds %>% trend_summary("Flagler")

ds %>% trend_summary("Hillsborough")
ds %>% trend_summary("Broward")
ds %>% trend_summary("Duval")
ds %>% trend_summary("Miami-Dade")


# ---- g2 -------------------------------

# ---- g3 -------------------------------


# ---- x1 -------------------------------


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