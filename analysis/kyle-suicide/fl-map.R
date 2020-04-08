# knitr::stitch_rmd(script="./___/___.R", output="./___/stitched-output/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
# source("./scripts/common-functions.R") # used in multiple reports
# source("./scripts/graphing/graph-presets.R") # fonts, colors, themes 

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
# must be in the environement for function to operate
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
# ne
county_map <- florida_counties_map %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_map()

county_map


# ---- total-youth-suicides ----------------------------------------------------
# function to compute the dataset
prep_data_map <- function(d, group_by_var, d_map = florida_counties_map ){
  # d <- ds_youth0
  # group_by_var <- c("county","year")
  # d_map <- florida_counties_map

  d <- ds_youth0 %>% 
    dplyr::group_by(.dots = group_by_var) %>% 
    dplyr::summarise(
      n_population = sum(n_population, na.rm = TRUE)
      ,n_suicides   = sum(n_suicides, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(
      rate_per100k_suicide  = (( n_suicides/n_population)*100000)
    ) %>% 
    dplyr::right_join(d_map, by = c( "county" = "subregion"))
  d
  return(d)
}
# How to use
# d <- ds_youth0 %>% prep_data_map(c("county","year","sex"))
d <- ds_youth0 %>% prep_data_map(c("county","year"))

# function to plot the map
plot_florida_suicides <- function(d, year_i, metric){
    # d <- youth_total_map_data
  # year_i <- 2017
  # metric <- "n_suicides"
  # 
  g1 <-  d %>% 
    dplyr::filter(year == year_i) %>%
    ggplot(aes_string(x = "long", y = "lat", group = "group", fill = metric)) +
    geom_polygon(color = "black") +
    coord_map() +
    theme_void() +
    # scale_fill_continuous(label = scales::label_comma())+
    scale_fill_gradient2(
      low   = "#fcfbfd" 
      ,mid  = "#9e9ac8"
      ,high = "#3f007d"
      ,midpoint = median(youth_total_map_data$n_suicides)
      ,label = scales::label_comma()
    ) +
    theme(
      legend.position = c(.4, .5)
    )+
    # facet_wrap(~year)+
    labs(
      title = paste0("Total Youth (10-24) Suicides in Florida for ",year_i)
      ,fill = "Number of Suicides"
    )
  # g1
  return(g1)
}
# How to use
# youth_total_map_data %>% plot_florida_suicides(2016,"n_suicides")
youth_total_map_data %>% plot_florida_suicides(2016,"n_population")

ds_youth0 %>% 
  prep_data_map(c("county","year")) %>% 
  plot_florida_suicides(2008,"rate_per100k_suicide")

# ---- youth suicides by Race + Ethnicity --------------------------------------

youth_race_ethnicity_map_data <- florida_counties_map %>% 
  dplyr::left_join(ds_youth_county_race_totals, by = c("subregion" = "county"))

g_youth_race_map <-  youth_race_ethnicity_map_data %>% 
  dplyr::filter(year == 2017) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = n_suicides)) +
  geom_polygon(color = "black") +
  coord_map() +
  theme_void() +
  scale_fill_gradient2(
    low   = "#fcfbfd" 
    ,mid  = "#9e9ac8"
    ,high = "#3f007d"
    ,midpoint = median(youth_race_ethnicity_map_data$n_suicides)
  ) +
  labs(
    title = "Total Youth (10-24) Suicides in Florida for 2017"
    ,fill = "Number of Suicides"
  ) +
  facet_wrap(~race_ethnicity)

g_youth_race_map



