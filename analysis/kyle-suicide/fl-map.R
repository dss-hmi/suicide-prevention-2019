# knitr::stitch_rmd(script="./___/___.R", output="./___/stitched-output/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graphing/graph-presets.R") # fonts, colors, themes 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(maps)     # maps
library(mapdata)  # addational map data
library(ggplot2) # graphing
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).


# ---- declare-globals ---------------------------------------------------------
path_file_input <- "data-public/derived/9-population-suicide.rds"

# to help with sorting the levels of the `age_group` factor
lvl_age_groups <-c(
  "less_than_1"
  ,"1_4"
  ,"5_9"
  ,"10_14"
  ,"15_19"
  ,"20_24"
  ,"25_34"
  ,"35_44"
  ,"45_54"
  ,"55_64"
  ,"65_74"
  ,"75_84"
  ,"85_plus"
)
youth_age_group <-   lvl_age_groups[4:6]

# ---- load-data ---------------------------------------------------------------
ds_population_suicide_raw<-   readr::read_rds(path_file_input)

# ---- load-map-data -----------------------------------------------------------
florida_counties_map <- map_data("county") %>% 
  dplyr::filter(region == "florida") %>% 
  dplyr::mutate_at(
    "subregion"
    , ~stringr::str_replace_all(
        .
        ,c(
          "de soto" = "desoto"
          ,"st johns" ="saint johns"
          ,"st lucie" = "saint lucie"
          )
        )
    )

# ---- tweak-data --------------------------------------------------------------
ds0 <- ds_population_suicide_raw %>% 
  dplyr::mutate(
    year          = as.integer(year)
    ,sex           = factor(sex)
    ,race_ethnicity = factor(paste0(race, " + ", ethnicity))
    ,race          = factor(race)
    ,ethnicity     = factor(ethnicity)
    ,age_group     = factor(age_group, levels = lvl_age_groups)
    ,n_population  = as.integer(n_population)
    ,n_suicides    = as.integer(n_suicides)
  ) %>% 
  dplyr::mutate_at("county",tolower)



# filter-data -------------------------------------------------------------

# filters out just youth age group, keeps all data
ds_youth0 <- ds0 %>% 
  dplyr::filter(age_group %in% youth_age_group)

# all race + ethnicities merged by county, year
ds_youth_county_totals <- ds_youth0 %>% 
  dplyr::group_by(county,year) %>% 
  dplyr::summarise(
    n_population = sum(n_population, na.rm = TRUE)
    ,n_suicides   = sum(n_suicides, na.rm = TRUE)
  ) %>% dplyr::ungroup()

# same as above but keeps race + ethnicity seperated
ds_youth_county_race_totals <- ds_youth0 %>% 
  dplyr::group_by(county,year, race_ethnicity) %>% 
  dplyr::summarise(
    n_population = sum(n_population, na.rm = TRUE)
    ,n_suicides   = sum(n_suicides, na.rm = TRUE)
  ) %>% dplyr::ungroup()


# testing-merge-data ------------------------------------------------------


  
# ---- base-map ---------------------------------------------------------------

#base map - no other data added

county_map <- florida_counties_map %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_map()

county_map


# total-youth-suicides ----------------------------------------------------

youth_total_map_data <- florida_counties_map %>% 
  dplyr::left_join(ds_youth_county_totals, by = c("subregion" = "county"))

g_youth_total_map <-  youth_total_map_data %>% 
  dplyr::filter(year == 2017) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = n_suicides)) +
  geom_polygon(color = "black") +
  coord_map() +
  theme_void() +
  scale_fill_gradient2(
    low =  "#edf8e9"
    ,mid = "#74c476"
    ,high = "#006d2c"
    ,midpoint = mean(youth_total_map_data$n_suicides)
  ) 
g_youth_total_map  

