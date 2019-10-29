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

# function-to-aggregate ---------------------------------------------------

compute_rate <- function(
  d,
  grouping_frame
){
  d <- ds_population_suicide
  grouping_frame <- c("county","year")
  
  d1 <- d %>%
    dplyr::group_by_(.dots = grouping_frame) %>%
    dplyr::summarize(
      n_population = sum(n_population, na.rm = T)
      ,n_suicides = sum(n_suicides, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      suicide_rate_per_100k  = (n_suicides/n_population)*100000
    )
  return(d1)
}
# how to use
d_computed <- ds_population_suicide %>% compute_rate(grouping_frame = c("county","year"))
d_computed %>% arrange(desc(suicide_rate_per_100k))


# apply rate computation --------------------------------------------------

grouping_frame <- c("county","year")
path_to_folder <- "./data-unshared/derived/rate/"


d_computed <- ds_population_suicide %>% compute_rate(grouping_frame = grouping_frame)

file_name <- paste0(path_to_folder, paste0(grouping_frame,collapse = "-"),".csv")
readr::write_csv(d_computed, file_name)


# create loop to apply the function

ls_grouping_frame <- list(
  c("county")
  ,c("county","year")
  ,c("county","year","sex")
)
for (frame_i in ls_grouping_frame){
  # frame_i %>% print()
  file_name_i  <- paste0(path_to_folder, paste0(frame_i,collapse = "-"),".csv")
  d_computed <- ds_population_suicide %>% compute_rate(grouping_frame = frame_i)
  d_computed %>% readr::write_csv(file_name_i)
  
}


