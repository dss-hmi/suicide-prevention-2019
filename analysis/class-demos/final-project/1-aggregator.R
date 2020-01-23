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
path_file_input_1 <- "data-unshared/raw/1-greeted-population.csv"
path_file_input_2 <- "data-unshared/raw/2-greeted-suicide.csv"

# ---- load-data ---------------------------------------------------------------
ds_population <- readr::read_csv(file = path_file_input_1)
ds_suicide <- readr::read_csv(file = path_file_input_2)

# ---- inspect-data ------------------------------
ds_population %>% dplyr::glimpse() 
ds_suicide %>% dplyr::glimpse() 

# ---- tweek-data --------------------------------------------------------------
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


# minimal -----------------------------------------------------------------
# select_county <- c("Orange")
# select_year   <- c(2016)
# select_age_group    <- c("55_64")
# select_race   <- c("White")
# select_ethnicity <- c("Non-Hispanic")
# select_mortality_cause <- c("Firearms Discharge")
# 
# select_county <- c("Orange", "Brevard")
# select_year   <- c( 2015, 2016)
# select_age_group    <- c("55_64", "65_74")
# select_race   <- c("White")
# select_ethnicity <- c("Non-Hispanic")
# select_mortality_cause <- c("Firearms Discharge", 
#                             "Hanging, Strangulation, Suffocation")
# 
# ds_population <- ds_population %>% 
#   dplyr::filter(county %in% select_county) %>% 
#   dplyr::filter(year %in% select_year) %>% 
#   dplyr::filter(age_group %in% select_age_group) %>% 
#   dplyr::filter(race %in% select_race) %>% 
#   dplyr::filter(ethnicity %in% select_ethnicity)
# ds_population %>% print(n=nrow(.))
# 
# ds_suicide <- ds_suicide %>% 
#   dplyr::filter(county %in% select_county) %>% 
#   dplyr::filter(year %in% select_year) %>% 
#   dplyr::filter(age_group %in% select_age_group) %>% 
#   dplyr::filter(race %in% select_race) %>% 
#   dplyr::filter(ethnicity %in% select_ethnicity) %>% 
#   dplyr::filter(mortality_cause %in% select_mortality_cause) 
# ds_suicide %>% print(n=nrow(.))


# ---- join-tables --------------------------------------------------------------

# join population and suicide tables

# create a temp table with total suicide counts
d_suicide = ds_suicide  %>% # `d` instead of `ds` in the name of object typically indicate transitory nature
  dplyr::group_by(county, year, sex, race, ethnicity, age_group) %>% 
  # aggregating over `mortality_casue`, otherwise multiple rows per county-year-sex-race-ethnicity-age_group
  dplyr::summarize(
    n_suicides = sum(n_suicides, na.rm = T)
  )
# create a temp table with suicide counts by mortality cause
d_suicide_wide = ds_suicide %>% 
  tidyr::spread(mortality_cause, n_suicides)
# now combine these tables to have both total and itemized counts in the same table
ds_suicide_wide <- dplyr::left_join(
  d_suicide        # contains total suicide count
  ,d_suicide_wide  # contains itemized suicide count by mortality cause
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
# Note : this function relies on the unfiltered data (will break if apply on minimal dataset)

compute_rate <- function(
  d,
  grouping_frame
){
  # d <- ds_population_suicide
  # grouping_frame <- c("sex","year")
  # 
  d_wide <- d %>%
    dplyr::group_by_(.dots = grouping_frame) %>%
    dplyr::summarize(
      n_population = sum(n_population, na.rm = T)
      ,n_suicide   = sum(n_suicides, na.rm = T)
      ,n_drug      = sum(`Drugs & Biological Substances`, na.rm=T)
      ,n_gun       = sum(`Firearms Discharge`, na.rm=T)
      ,n_hanging   = sum(`Hanging, Strangulation, Suffocation`, na.rm=T)
      ,n_jump      = sum(`Jump From High Place`, na.rm=T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      n_other      = n_suicide - n_drug - n_gun -n_hanging - n_jump
      
      ,rate_suicide = (n_suicide/n_population)*100000
      ,rate_drug    = (n_drug/n_population)*100000
      ,rate_gun     = (n_gun/n_population)*100000
      ,rate_hanging = (n_hanging/n_population)*100000
      ,rate_jump    = (n_jump/n_population)*100000
      ,rate_other   = (n_other/n_population)*100000
      
    )
  # d_wide %>% glimpse()
  d_n <- d_wide %>% dplyr::select_(.dots = c(grouping_frame
                                             ,"n_suicide"
                                             ,"n_drug"
                                             ,"n_gun"
                                             ,"n_hanging"
                                             ,"n_jump"
                                             ,"n_other")) %>% 
    tidyr::gather("suicide_cause", "n_suicides", n_suicide, n_drug,n_gun, n_hanging, n_jump, n_other) %>% 
    dplyr::mutate(
      suicide_cause = gsub("^n_","",suicide_cause)
    )
  d_rate <- d_wide %>% dplyr::select_(.dots = c(grouping_frame
                                                ,"rate_suicide"
                                                ,"rate_drug"
                                                ,"rate_gun"
                                                ,"rate_hanging"
                                                ,"rate_jump"
                                                ,"rate_other")) %>% 
    tidyr::gather("suicide_cause", "rate_per_100k", rate_suicide, rate_drug,rate_gun, rate_hanging, rate_jump, rate_other) %>% 
    dplyr::mutate(
      suicide_cause = gsub("^rate_","",suicide_cause)
    )
  
  d_long <- d_wide %>% dplyr::select_(.dots = c(grouping_frame,"n_population")) %>% 
    dplyr::left_join(d_n) %>% 
    dplyr::left_join(d_rate)
  
  d_long <- d_long %>% 
    dplyr::group_by_(.dots = c(grouping_frame,"n_population")) %>% 
    dplyr::mutate(
      n_suicides_total = sum(n_suicides, na.rm = T)
      ,rate_per_100k_total = (n_suicides_total/n_population)*100000
    ) %>% 
    dplyr::ungroup()
  
  ls_out <- list("wide" = d_wide, "long" = d_long )
  return(ls_out)
}
# how to use
ls_computed <-ds_population_suicide %>% compute_rate(grouping_frame = c("county","year"))
# OR
ls_computed <- compute_rate(d = ds_population_suicide, grouping_frame = c("county","year"))


# apply rate computation --------------------------------------------------

# you can use the function to compute the rates for the following grouping frames:
ls_grouping_frame <- list(
  c(         "year"                                   )
  ,c(         "year","sex"                             )
  ,c(         "year",      "race_ethnicity"            )
  ,c(         "year"                       ,"age_group")
  ,c(         "year","sex","race_ethnicity"            )
  ,c(         "year",      "race_ethnicity","age_group")
  ,c(         "year","sex"                 ,"age_group")
  ,c(         "year","sex","race_ethnicity","age_group")
)

# common filter
# to apply a filter that would cascade down the workflow: 
ds_filtered <- ds_population_suicide %>% 
  dplyr::filter(year %in% 2006:2017) %>% 
  dplyr::filter(!age_group == "less_than_1") # remove those younger than 1 year old

ds_filtered %>% distinct(age_group) %>% arrange(age_group)

# how to compute rate for a single grouping frame:
ls_year <- compute_rate(d = ds_filtered, grouping_frame = c("year"))
ls_year$wide %>% readr::write_csv("./data-unshared/derived/year-wide.csv")
ls_year$long %>% readr::write_csv("./data-unshared/derived/year-long.csv")


# how to compute rate for set of grouping frames
for (i in seq_along(ls_grouping_frame)){
  # frame_i %>% print()
  # i <- 1
  path_to_folder <- "./data-unshared/derived/loop/"
  frame_i <- ls_grouping_frame[[i]]
  file_name_wide_i  <- paste0(path_to_folder, paste0(frame_i,collapse = "-"),"-wide",".csv")
  file_name_long_i  <- paste0(path_to_folder, paste0(frame_i,collapse = "-"),"-long",".csv")
  l_computed <- ds_filtered %>% 
    
    compute_rate(grouping_frame = frame_i)
  l_computed[["wide"]] %>% readr::write_csv(file_name_wide_i)
  l_computed[["long"]] %>% readr::write_csv(file_name_long_i)
  
}

