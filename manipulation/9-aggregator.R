rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr) # disable when temp lines are removed
library(ggplot2)

# ---- declare-globals ---------------------------------------------------------
path_file_input_0 <- "./data-unshared/derived/0-greeted-gls.rds"
path_file_input_1 <- "./data-unshared/derived/1-greeted-population.rds"
path_file_input_2 <- "./data-unshared/derived/2-greeted-suicide.rds"
# path_file_input_9 <- "./data-public/raw/9-combined.rds"


# ---- load-data ---------------------------------------------------------------
ds_gls        <- readRDS(path_file_input_0) %>% dplyr::glimpse(100)
ds_population <- readRDS(path_file_input_1) %>% dplyr::glimpse(100)
ds_suicide    <- readRDS(path_file_input_2) %>% dplyr::glimpse(100)
# ds_combined   <- readRDS(path_file_input_9) %>% dplyr::glimpse(100)

# ---- inspect-data ------------------------------
ds_gls %>% dplyr::glimpse() 
ds_population %>% dplyr::glimpse() 
ds_suicide %>% dplyr::glimpse() 

# ---- tweek-data --------------------------------------------------------------
ds_gls <- ds_gls %>% 
  dplyr::mutate(
    year = lubridate::year(date) %>% as.character() # for joining to other tables
  ) 

ds_population <- ds_population %>% 
  dplyr::rename(
    n_population = count
  )

ds_suicide    <- ds_suicide %>% 
  dplyr::mutate(
    mortality_cause = gsub("^Suicide: ","",mortality_cause),
    mortality_cause = gsub(" \\(.+\\)","",mortality_cause)
  ) %>% 
  dplyr::rename(
    n_suicides = resident_deaths
  ) %>% 
  dplyr::select(-mortality_locus)
ds_suicide %>% dplyr::distinct(mortality_cause)

# ---- aggregate-0 --------------------------------------------------------------

# there is too much temporal granularity in the GLS source.
# to reduce it, making it compatible with the FL health records:
ds_gls_by_year <- ds_gls %>% 
  dplyr::group_by(region, county, year, audience, type_training) %>%  # one line for this combination
  dplyr::summarize(
    n_trained = sum(n_trained, na.rm = T)
  )

# join population and suicide tables
d_suicide = ds_suicide  %>% 
  dplyr::group_by(county, year, sex, race, ethnicity, age_group) %>% 
  # aggregating over `mortality_casue`, otherwise multiple rows per county-year-sex-race-ethnicity-age_group
  dplyr::summarize(
    n_suicides = sum(n_suicides, na.rm = T)
  )
d_suicide_wide = ds_suicide %>% 
  tidyr::spread(mortality_cause, n_suicides)

ds_suicide_wide <- dplyr::left_join(
  d_suicide
  ,d_suicide_wide
)
ds_suicide_wide %>% dplyr::glimpse()

ds_population_suicide <- dplyr::left_join(
  ds_population
  ,ds_suicide_wide
  ,by = c("county", "year", "sex", "race", "ethnicity", "age_group")
  ) %>% 
  dplyr::mutate(
    # to have a single variable describing racial background
    race_ethnicity = paste0(race," + ", ethnicity)
  ) 
ds_population_suicide %>% dplyr::glimpse()












ds_gls_by_year %>% dplyr::glimpse()
# note that `audience` and `type_training` are more granular than county-by-year
ds_population_by_year <- ds_population %>% 
  dplyr::rename(
    "population_count" = "count" # to be more explicit and because a better label
  )
ds_suicide_by_year <- ds_suicide %>% 
  dplyr::group_by(county, year, sex, race, ethnicity, age_group, mortality_cause) %>% 
  dplyr::summarize(
    n_suicides = sum(n_suicides, na.rm = T)
  )
ds_suicide_by_year %>% dplyr::glimpse()
# note that `mortality_cause` is more granular than county-by-year
# to help with elementwise list operations:
ls_ds <- list(
  "population" = ds_population_by_year # first, because most complete
  ,"suicide"   = ds_suicide_by_year
  ,"gls"       = ds_gls_by_year # last, because left join
)
ls_ds %>% lapply(dplyr::glimpse,100) %>% invisible() 

# ---- aggregate-1-suicide ------------------------------
#let us aggregate at the level of `population`
# this means that we will provide a single measure at the resolution:
# county - year - sex - age_group - race - enthicity 
# for every unique combination of values on these six we want:
# `population_count` - number of people alive 
# `resident_deaths`  - number of registered deaths among residents
# `community`        - number of people reached through community event
# `professionals`    - number of professionals who recieved training


ls_ds_1 <- list(
  "population" = ls_ds[["population"]] 
  ,"suicide" = ls_ds[["suicide"]]  %>% 
    dplyr::group_by(county, year, sex, race, ethnicity, age_group) %>% 
    # aggregating over `mortality_casue`
    dplyr::summarize(
      resident_deaths = sum(resident_deaths, na.rm = T)
    )
  ,"suicide_wide" = ls_ds[["suicide"]] %>% 
    tidyr::spread(mortality_cause, resident_deaths)
)
ls_ds_1 %>% lapply(dplyr::glimpse,100) %>% invisible() 
ds_aggregate_1 <- ls_ds_1 %>% Reduce(function(a , b) dplyr::left_join( a, b ), . ) 
ds_aggregate_1 %>% dplyr::glimpse(100)

# ----- tweak-aggregate-1 ------------------------------------------

ds_aggregate_1 <- ds_aggregate_1 %>% 
  dplyr::rename(
    "deaths_by_suicide" = "resident_deaths" # to remind what we count
  ) %>% 
  dplyr::mutate(
    # to have a single variable describing racial background
    race_ethnicity = paste0(race," + ", ethnicity),
    deaths_per_100k  = (deaths_by_suicide/population_count)*100000
    # to aid in graph production ( note the spaces at the end of "NE  ")
  ) %>% 
  
  dplyr::select(county, year, sex, age_group, race, ethnicity, race_ethnicity, # context
                population_count, deaths_by_suicide, #measures
                dplyr::everything()
  ) 
ds_aggregate_1 %>% explore::describe_all()

# ---- test-snippets -------------------------------
# ds_aggregate_1 %>% 
#   dplyr::filter(county == "Brevard") %>% 
#   dplyr::summarize(
#     pop = sum(community,na.rm = T)
#   )


# mc <- ds_suicide %>% 
#   dplyr::distinct(county, year, sex, age_group, race, ethnicity, resident_deaths, mortality_cause) %>% 
#   dplyr::rename(
#     "deaths_by_suicide" = "resident_deaths" # to remind what we count
#   )
# mc %>% explore::describe_all()
# d <- dplyr::left_join(
#   ds_aggregate_1
#   ,mc
# )

# ----- save-to-disk ---------------------------------------------------------------

ds_gls                   %>% readr::write_csv("./data-public/derived/gls.csv")
ds_population            %>% readr::write_csv("./data-public/derived/population.csv")
ds_suicide               %>% readr::write_csv("./data-public/derived/suicide.csv")
ds_aggregate_1          %>% readr::write_csv("./data-public/derived/aggregate-1-suicide.csv")


# ---- aggregate-2-gls ------------------------------
#let us aggregate at the level of `population`
# this means that we will provide a single measure at the resolution:
# county - year - sex - age_group - race - enthicity 
# for every unique combination of values on these six we want:
# `population_count` - number of people alive 
# `resident_deaths`  - number of registered deaths among residents
# `community`        - number of people reached through community event
# `professionals`    - number of professionals who recieved training


ls_ds_2 <- list(
  "population" = ls_ds[["population"]] 
  ,"suicide" = ls_ds[["suicide"]]  %>% 
    dplyr::group_by(county, year, sex, race, ethnicity, age_group ) %>% 
    # aggregating over `mortality_casue`
    dplyr::summarize(
      resident_deaths = sum(resident_deaths, na.rm = T)
    )
  ,"suicide_wide" = ls_ds[["suicide"]] %>% 
    # dplyr::mutate(
    #   lethal_means = mortality_cause
    # ) %>%
    # tidyr::spread(lethal_means, resident_deaths)
    tidyr::spread(mortality_cause, resident_deaths)
  
  ,"gls" = ls_ds[["gls"]]  %>%
    dplyr::group_by(region, county, year, audience) %>%
    # aggregating over `type_training`
    dplyr::summarize(
      n_trained = sum(n_trained, na.rm = T)
    ) %>%
    tidyr::spread(audience,n_trained)
)
ls_ds_2 %>% lapply(dplyr::glimpse,100) %>% invisible() 
ds_aggregate_2 <- ls_ds_2 %>% Reduce(function(a , b) dplyr::left_join( a, b ), . ) 
ds_aggregate_2 %>% dplyr::glimpse(50)

# ----- tweak-aggregate-2 ------------------------------------------

ds_aggregate_2 <- ds_aggregate_2 %>% 
  dplyr::rename(
    "deaths_by_suicide" = "resident_deaths" # to remind what we count
  ) %>% 
  dplyr::mutate(
    # to have a single variable describing racial background
    race_ethnicity = paste0(race," + ", ethnicity),
    deaths_per_100k  = (deaths_by_suicide/population_count)*100000
    # to aid in graph production ( note the spaces at the end of "NE  ")
  ) %>% 
  
  dplyr::select(county, year, sex, age_group, race, ethnicity, race_ethnicity, # context
                population_count, deaths_by_suicide, #measures
                # mortality_cause,
                dplyr::everything()
  ) 
ds_aggregate_2 %>% explore::describe_all()


# ---- test-snippet-2 --------------------------
# d1 <- ds_aggregate_2 %>% 
#   dplyr::filter(county == "Orange") %>% 
#   dplyr::filter(sex == "Male") %>% 
#   dplyr::filter(year == "2016") %>% 
#   dplyr::filter() 

# ----- save-to-disk ---------------------------------------------------------------

# ds_gls                   %>% readr::write_csv("./data-public/derived/gls.csv")
# ds_population            %>% readr::write_csv("./data-public/derived/population.csv")
# ds_suicide               %>% readr::write_csv("./data-public/derived/suicide.csv")
ds_aggregate_2          %>% readr::write_csv("./data-public/derived/aggregate-2-gls.csv")
