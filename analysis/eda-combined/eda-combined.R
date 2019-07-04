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


ds <- ds %>% 
  dplyr::rename(
    "deaths_by_suicide" = "resident_deaths" # to remind what we count
  ) %>% 
  dplyr::mutate(
    # to have a standardized measure / put counties on the same scale
    suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
    # to have a single variable describing racial background
    ,ethnic = paste0(race," + ", ethnicity)
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
  
  dplyr::select(county, year, sex, age_group, race, ethnicity, ethnic, # context
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
  # dplyr::group_by(county, year, sex, ethnic                  ) %>%
  dplyr::summarize(
    population_count   = sum(population_count,   na.rm = T)
    ,deaths_by_suicide = sum(deaths_by_suicide,  na.rm = T)
    ,professionals     = sum(professionals,      na.rm = T)
    ,community         = sum(community,          na.rm = T)
  ) 
# use the code for preparing data for custom graphs
# d1 %>% glimpse(60)
# d1 %>% explore::describe()

# ---- explore-4 -------------------------------
# to understand how suicide rate varies across time and counties for youth regardless of race
d4 <- ds %>% 
  dplyr::filter(year %in% c(2006:2017)) %>% 
  # dplyr::filter(age_group %in% c("10_14","15_19","20_24")) %>%
  dplyr::filter(age_group %in% c("20_24")) %>%
  # dplyr::filter(age_group %in% c("15_19")) %>%
  # dplyr::filter(age_group %in% c("10_14")) %>% 
  dplyr::group_by(county, year, sex) %>%
  dplyr::summarize(
    population_count   = sum(population_count,   na.rm = T)
    ,deaths_by_suicide = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
    ,professionals     = sum(professionals,      na.rm = T)
    ,community         = sum(community,          na.rm = T)
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    # produces the same results as within group_by() summarization
    # suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
    years_since_2000 = as.integer(as.integer(year) - 2000)
    # ,sex = factor(sex, levels = c("Male","Female") )
    ,sex = factor(sex, levels = c("Female","Male") )
  ) 
d4 %>% explore::describe()
g4 <- d4 %>% 
  ggplot(aes(
    x     = years_since_2000
    ,y    = suicide_rate_per100k
    # ,y    = deaths_by_suicide
    # ,y    = population_count
    ,fill = sex
  ))+
  geom_bar(stat = "identity", position = "stack", color = "black")+
  # geom_point()+
  # geom_line(aes(group = county))+
  facet_wrap("county")+
  scale_x_continuous(breaks = seq(6,17,2))+
  scale_fill_viridis_d(end = .9, option = "plasma")+
  # scale_fill_viridis_d(end = .5, option = "magma")+
  # scale_fill_viridis_d(end = .5, option = "inferno")+
  # scale_fill_viridis_d(end = .5, option = "viridis")+
  # scale_fill_manual(values = c("Female"="darksalmon","Male" = "deepskyblue1"))+
  # theme_minimal()
  theme_bw()
g4

# ---- explore-1 -------------------------------
# g1 <- ds %>%
#   dplyr::group_by(county, year,sex) %>%
#   dplyr::summarize(
#     population_count = sum(population_count, na.rm = T)
#     ,deaths_by_suicide = sum(deaths_by_suicide, na.rm = T)
#     ,professionals   = sum(professionals, na.rm =T)
#     ,community       = sum(community, na.rm =T)
#   ) %>%
#   # dplyr::filter(county == "Orange") %>%
#   # dplyr::filter(year == "2015")
#   # ggplot(aes(x = year, y = population_count))+
#   ggplot(aes(x = year, y = deaths_by_suicide))+
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
      ,deaths_by_suicide = sum(deaths_by_suicide, na.rm = T)
      ,professionals   = sum(professionals, na.rm =T)
      ,community       = sum(community, na.rm =T)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    year = as.numeric(year)
    # ,sex = factor(sex, levels = c("Male","Female"))
    ,sex = factor(sex, levels = c("Male","Female"))
    ,suicide_rate_per10k = (deaths_by_suicide / population_count) *10000
  ) %>%
  # dplyr::filter(county     == "Orange") %>%
  # dplyr::filter(sex        == "Male") %>% 
  # dplyr::filter(age_group  == "25_34") %>%
  # dplyr::filter(age_group  == "20_24") %>% 
  # dplyr::filter(race       == "White") %>%
  # dplyr::filter(ethnicity  == "Hispanic") %>% 
  # dplyr::filter(ethnicity  == "Non-Hispanic") %>%
  # ggplot(aes(x=year, y = deaths_by_suicide, fill = sex))+
  ggplot(aes(x=year, y = suicide_rate_per10k, fill = sex))+
  geom_area(alpha = 1, position = "identity")+ 
  # geom_line(aes(group= sex))+
  # geom_point(shape = 21, size = 3, fill = "white")+
  scale_y_continuous(limits = c(0,NA))+
  # scale_fill_manual(values = c("Female"="red","Male"="white"))+
  scale_fill_viridis_d(end = .9, option = "plasma")+
  # facet_grid(county ~ age_group)+
  # facet_wrap("county", scales = "free_y")+
  facet_wrap("county")+
  theme_minimal()
g1

# ---- explore-3 -------------------------------
target_counties <- c("Lake", "Brevard", "Saint Lucie","Volusia",
                     "Palm Beach","Orange", "Seminole")
g3 <- ds %>% 
  # dplyr::filter(!is.na(region)) %>%
  # dplyr::filter(county == "Orange") %>% 
  dplyr::filter(sex        == "Male") %>%
  dplyr::filter(age_group  %in% c("10_14","15_19","20_24")) %>%
  dplyr::group_by(region, county, year, sex) %>% 
  dplyr::summarize(
    population_count = sum(population_count, na.rm = T)
    ,deaths_by_suicide = sum(deaths_by_suicide, na.rm = T)
    ,professionals   = sum(professionals, na.rm =T)
    ,community       = sum(community, na.rm =T)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    year = as.integer(year)
    ,sex = factor(sex, levels = c("Male","Female"))
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
    ,suicide_rate_per100k = ifelse(suicide_rate_per100k==0,NA,suicide_rate_per100k)
    ,target_county = ifelse(county %in% target_counties, TRUE, FALSE)
  ) %>% 
  ggplot(aes(x=year, y =suicide_rate_per100k))+
  # geom_point(aes(fill = region), shape = 21)+
  geom_line(aes(group = county), color = "black", alpha = .1)+
  geom_line(aes(group = county, color = target_county))+
  # geom_smooth(aes(group=county), method = "lm", se =F)+
  geom_smooth(aes(group = target_county, color = target_county), method = "lm", se =T, alpha = .3)+
  scale_y_continuous(limits = c(0,NA))+
  scale_x_continuous(limits = c(2014,NA))+
  scale_color_manual(values = c("FALSE"="blue", "TRUE"="red"))+
  # facet_grid(target_county ~ .)+
  # scale_fill_manual(values = c("central"="red","northeast"="blue","southeast"="green","NA"="NA"))+
  # scale_fill_manual(values = c("central"="red","northeast"="blue","southeast"="green","NA"="NA"))+
  theme_minimal()
g3



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



