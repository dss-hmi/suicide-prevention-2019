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

d1 <- ds %>% 
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
    ,gls_programming = ifelse(county %in% counties_gls, TRUE, FALSE)
  ) %>% 
  dplyr::select(county, year, sex, age_group, race, ethnicity, racethnicity, # context
                gls_programming, # was any programming administered?
                region, rgn, # support for graphing and grouping
                population_count, deaths_by_suicide, suicide_rate_per100k, #measures
                community, professionals # treatment
  )

ds %>% explore::describe_all()

# to automate aggregation:
compute_aggregate <- function(
  d
  ,age_group_i = c("10_14","15_19","20_24") # willcombine these, exclude others
  ,group_by_variables  =  c("county","year") # will aggregate over these, exclude others
){
  # d <- ds
  # group_by_variables <- c("county","year")
  group_by_variables <- c(group_by_variables, "professionals","community") # because cannot summarize
  d1 <- d %>% 
    dplyr::group_by(.dots = group_by_variables) %>%
    dplyr::summarize(
      population_count   = sum(population_count,   na.rm = T)
      ,deaths_by_suicide = sum(deaths_by_suicide,  na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      suicide_rate_per100k         = (deaths_by_suicide / population_count) *100000
      ,community_reach_per100k     = (community/ population_count) * 100000
      ,professionals_reach_per100k = (professionals/ population_count) * 100000
    )
  return(d1)
  
}
d1 <- ds %>% compute_aggregate(
   age_group_i         = c("10_14","15_19","20_24") # willcombine these, exclude others
  # ,group_by_variables  =  c("county","year") # will aggregate over these, exclude others
  # ,group_by_variables  =  c("county","year","sex")
  ,group_by_variables  =  c("county","year","sex","racethnicity")
  # ,group_by_variables  =  c("county","year","gls_programming") 
)

# ----- new -----------------------
d1 <- ds %>% 
  dplyr::mutate(
    tx = ifelse(county %in% counties_gls, TRUE, FALSE)
  ) %>% 
  dplyr::group_by(year, tx) %>% 
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>% 
  dplyr::ungroup()

g1 <- d1 %>% 
  dplyr::filter(year %in% 2006:2017) %>% 
  ggplot(aes(x = year, y = suicide_rate_per100k))+
  geom_line(aes(color = tx, group = tx))+
  geom_point(shape=21)+
  theme_minimal()+
  labs(color = "Counties with \n GLS programming")
g1


d2 <- ds %>% 
  dplyr::mutate(
    # tx_level = tx_level
    tx_level = car::recode(
      county,
      "
     'Orange'  ='high P high C'
     ;'Saint Lucie'='high P mid C'
     ;'Palm Beach'='mid P mid C  '
     ;'Brevard'='mid P mid C  '
     ;'Seminole'='mid P mid C  '
     ;'Volusia'='mid P high C  '
     ;'Lake'='mid P low C  '
     "
    )
    ,tx_level = ifelse(tx_level %in% counties_gls, "low P low C", tx_level)
    ,tx = ifelse(county %in% counties_gls, TRUE, FALSE)
    ,tx_level = ifelse(
      (!tx_level %in% counties_gls) & (tx == FALSE),"control", tx_level
    )
  ) %>% 
  dplyr::group_by(year,tx, tx_level) %>% 
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>% 
  dplyr::ungroup()
g2 <- d2 %>% 
  dplyr::filter(year %in% 2006:2017) %>% 
  ggplot(aes(x = year, y = suicide_rate_per100k))+
  geom_line(aes(color = tx_level, group = tx_level))+
  geom_point(shape=21)+
  theme_minimal()+
  labs(color = "Counties with \n GLS programming")
g2


focal_counties <- c(
   "Orange"  
  ,"Saint Lucie" 
  ,"Palm Beach"  
  ,"Brevard" 
  ,"Seminole"
  ,"Volusia"
  ,"Lake"        
)
d3 <- ds %>% 
  dplyr::mutate(
    tx_level = ifelse(county %in% focal_counties, county,
                      ifelse((!county %in% focal_counties) & (county %in% counties_gls), "low GLS",
                             "control"))
                      
    ) %>% 
  dplyr::group_by(year,tx_level) %>% 
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>% 
  dplyr::ungroup()
g3 <- d3 %>% 
  dplyr::filter(year %in% 2013:2017) %>% 
  ggplot(aes(x = year, y = suicide_rate_per100k))+
  # ggplot(aes(x = year, y = deaths_by_suicide))+
  geom_line(aes(color = tx_level, group = tx_level), size =1.5)+
  geom_point(shape=21, size = 2)+
  theme_minimal()+
  labs(color = "Counties with \n GLS programming")
g3

focal_counties <- c(
  "Broward"
  ,"Brevard"
  ,"Hillsborough" 
  ,"Palm Beach"  
  ,"Miami-Dade"
  ,"Orange"
  ,"Seminole"        
  # ,"Brevard"
  ,"Volusia"
)
d4 <- ds %>% 
  dplyr::filter(county %in% focal_counties) %>% 
   dplyr::group_by(county, year) %>% 
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>% 
  dplyr::ungroup()
g4 <- d4 %>% 
  dplyr::filter(year %in% 2006:2017) %>% 
  ggplot(aes(x = year, y = suicide_rate_per100k))+
  # ggplot(aes(x = year, y = deaths_by_suicide))+
  geom_line(aes(color = county, group = county), size =1.5)+
  geom_point(shape=21, size = 2)+
  theme_minimal()+
  labs(color = "Focal Counties")
g4


d5 <- ds %>% 
  dplyr::filter(county %in% c("Palm Beach")) %>% 
  dplyr::group_by(county, year, racethnicity) %>% 
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>% 
  dplyr::ungroup()
g5 <- d5 %>% 
  dplyr::filter(year %in% 2006:2017) %>% 
  ggplot(aes(x = year, y = suicide_rate_per100k))+
  # ggplot(aes(x = year, y = deaths_by_suicide))+
  geom_line(aes(color = racethnicity, group = racethnicity), size =1.5)+
  geom_point(shape=21, size = 2)+
  theme_minimal()+
  labs(color = "Race Groups")
g5


# create a ration of population size to n_trained


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