# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md", figure)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./manipulation/stitched-output/0-greeter.md", )
# this command is typically executed by the ./manipulation/governor.R

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
path_file_input <- "./data-unshared/derived/4-combined.rds"
html_flip <- FALSE
baseSize <- 10
# ---- load-data ---------------------------------------------------------------
dto      <- readRDS(path_file_input)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()

# ----- custom-functions --------------------------------------

# ---- tweak-data ---------------------------------------------------------------

ds <- dto[["lowest_granularity"]] %>% 
  Reduce(function(a , b) dplyr::left_join( a, b ), . )

ds %>% explore::describe_all()

# to help us filter out those counties that had programming
counties_gls <- ds %>% 
  na.omit(region) %>% 
  dplyr::distinct(county) %>% 
  as.list() %>% unlist() %>% as.character()

ds <- ds %>% 
  dplyr::mutate(
    ethnic = paste0(race," + ", ethnicity)
  ) #%>% 
  # dplyr::select(region, dplyr::everything()) %>% 
  # dplyr::mutate(
  #   region      = factor(region) # region of counties with programming
  #   ,sex        = factor(sex)
  #   ,race       = factor(race)
  #   ,ethnicity  = factor(ethnicity)
  #   ,age_group  = factor(age_group)
  # )

ds %>% explore::describe_all()

# aggregate 
g1 <- ds %>% 
  # dplyr::group_by(county, year,sex) %>% 
  dplyr::group_by(county, year,sex,race,ethnicity,age_group) %>%
  # dplyr::group_by(county, year,sex,race,ethnicity,age_group) %>%
  # dplyr::group_by(county, year,sex,race,ethnicity,age_group) %>%
  # dplyr::group_by(county, year,sex,race,ethnicity,age_group) %>%
  # dplyr::group_by(county, year,sex,race,ethnicity,age_group) %>%
  dplyr::summarize(
    population_count = sum(population_count, na.rm = T)
    ,resident_deaths = sum(resident_deaths, na.rm = T)
    ,professionals   = sum(professionals, na.rm =T)
    ,community       = sum(community, na.rm =T)
    ,suicide_rate    =  (resident_deaths / population_count)*1000
  ) 


# create auxilary variables
ds <- ds %>% 
  dplyr::mutate(
    rgn = car::recode(
      region,
      "
      'central'  ='CN'
     ;'southeast'='SE'
     ;'northeast'='NE  '
      "
    )
  )
ds %>% glimpse(60)

ds %>% explore::describe()

# ---- explore-1 -------------------------------
# g1 <- ds %>% 
#   dplyr::group_by(county, year,sex) %>% 
#   dplyr::summarize(
#     population_count = sum(population_count, na.rm = T)
#     ,resident_deaths = sum(resident_deaths, na.rm = T)
#     ,professionals   = sum(professionals, na.rm =T)
#     ,community       = sum(community, na.rm =T)
#   ) %>% 
#   # dplyr::filter(county == "Orange") %>% 
#   # dplyr::filter(year == "2015")
#   # ggplot(aes(x = year, y = population_count))+
#   ggplot(aes(x = year, y = resident_deaths))+
#   geom_bar(stat = "identity")+
#   # geom_point()+
#   # geom_line()+
#   # facet_wrap("county")+
#   theme_minimal()
# g1
# ---- explore-2 -------------------------------
ds %>% dplyr::glimpse(70)

g1 <- ds %>% 
  dplyr::group_by(county, year, sex) %>% 
  dplyr::summarize(
      population_count = sum(population_count, na.rm = T)
      ,resident_deaths = sum(resident_deaths, na.rm = T)
      ,professionals   = sum(professionals, na.rm =T)
      ,community       = sum(community, na.rm =T)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    year = as.numeric(year)
    # ,sex = factor(sex, levels = c("Male","Female"))
    ,sex = factor(sex, levels = c("Male","Female"))
    ,suicide_rate_per10k = (resident_deaths / population_count) *10000
  ) %>%
  # dplyr::filter(county     == "Orange") %>%
  # dplyr::filter(sex        == "Male") %>% 
  # dplyr::filter(age_group  == "25_34") %>%
  # dplyr::filter(age_group  == "20_24") %>% 
  # dplyr::filter(race       == "White") %>%
  # dplyr::filter(ethnicity  == "Hispanic") %>% 
  # dplyr::filter(ethnicity  == "Non-Hispanic") %>%
  # ggplot(aes(x=year, y = resident_deaths, fill = sex))+
  ggplot(aes(x=year, y = suicide_rate_per10k, fill = sex))+
  geom_area(alpha = 1, position = "identity")+ 
  # geom_line(aes(group= sex))+
  # geom_point(shape = 21, size = 3, fill = "white")+
  scale_y_continuous(limits = c(0,NA))+
  scale_fill_viridis_d(end = .9, option = "plasma")+
  # facet_grid("community")+
  facet_wrap("county")+
  theme_minimal()
g1

# ---- explore-3 -------------------------------

g2 <- ds %>% 
  dplyr::group_by(county, year, sex, age_group) %>% 
  dplyr::summarize(
    population_count = sum(population_count, na.rm = T)
    ,resident_deaths = sum(resident_deaths, na.rm = T)
    ,professionals   = sum(professionals, na.rm =T)
    ,community       = sum(community, na.rm =T)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    year = as.numeric(year)
    # ,sex = factor(sex, levels = c("Male","Female"))
    ,sex = factor(sex, levels = c("Male","Female"))
    ,suicide_rate_per10k = (resident_deaths / population_count) *10000
  ) %>%
  # dplyr::filter(county     == "Orange") %>%
  # dplyr::filter(sex        == "Male") %>% 
  # dplyr::filter(age_group  == "25_34") %>%
  # dplyr::filter(age_group  == "20_24") %>% 
  # dplyr::filter(race       == "White") %>%
  # dplyr::filter(ethnicity  == "Hispanic") %>% 
  # dplyr::filter(ethnicity  == "Non-Hispanic") %>%
  # ggplot(aes(x=year, y = resident_deaths, fill = sex))+
  ggplot(aes(x=year, y = suicide_rate_per10k, fill = sex))+
  # geom_area(alpha = 1, position = "identity")+ 
  # geom_line(aes(group= sex))+
  # geom_line(aes(group = age_group))+
  # geom_point(shape = 21, size = 3, fill = "white")+
  scale_y_continuous(limits = c(0,NA))+
  scale_fill_viridis_d(end = .9, option = "plasma")+
  # facet_grid(age_group)+
  # facet_wrap("county")+
  # facet_wrap("age_group", scales = "free_y")+
  theme_minimal()
g2




# ---- explore-4 -------------------------------


# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------
rmarkdown::render(
  # input = "./analysis/gls-activity/gls-activity-1.Rmd"
  input = "./analysis/gls-activity/gls-activity-2-coverage.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



