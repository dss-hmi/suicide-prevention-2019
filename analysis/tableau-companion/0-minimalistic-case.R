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
path_file_input_1 <- "./data-unshared/derived/1-greeted-population.rds"
path_file_input_2 <- "./data-unshared/derived/2-greeted-suicide.rds"


# ---- load-data ---------------------------------------------------------------
ds_population <- readRDS(path_file_input_1) %>% dplyr::glimpse(70)
ds_suicide    <- readRDS(path_file_input_2) %>% dplyr::glimpse(70)



# ---- inspect-data ------------------------------
ds_gls %>% dplyr::glimpse() 
ds_population %>% dplyr::glimpse() 
ds_suicide %>% dplyr::glimpse() 

# TASK 1 ------------------------------------------------------------------

# Let us create minimalistic data sets that we can use to study their structure and develop code
# what as few records as possible that still allow us to understand the range of values
ds_population %>% distinct(race)
ds_population %>% distinct(ethnicity)
ds_population %>% distinct(age_group)

ds_suicide %>% distinct(mortality_cause)

# define filter -------------------------

# let us specify the filtering conditions that we can apply to both data sets
select_county <- c("Orange", "Brevard")
select_year   <- c( 2015, 2016)
select_age_group    <- c("55_64", "65_74")
select_race   <- c("White")
# select_race   <- c("")
select_ethnicity <- c("Non-Hispanic")
# select_ethnicity <- c("Hispanic")

select_mortality_cause <- c("Suicide: Firearms Discharge (X72-X74)", 
                            "Suicide: Hanging, Strangulation, Suffocation (X70)")

# apply filter -------------------------------------------------------
ds_population_min <- ds_population %>% 
  dplyr::filter(county %in% select_county) %>% 
  dplyr::filter(year %in% select_year) %>% 
  dplyr::filter(age_group %in% select_age_group) %>% 
  dplyr::filter(race %in% select_race) %>% 
  dplyr::filter(ethnicity %in% select_ethnicity)
ds_population_min %>% print(n=nrow(.))

ds_suicide_min <- ds_suicide %>% 
  dplyr::filter(county %in% select_county) %>% 
  dplyr::filter(year %in% select_year) %>% 
  dplyr::filter(age_group %in% select_age_group) %>% 
  dplyr::filter(race %in% select_race) %>% 
  dplyr::filter(ethnicity %in% select_ethnicity) %>% 
  dplyr::filter(mortality_cause %in% select_mortality_cause) %>% 
  dplyr::select(-mortality_locus) # because ds_suicide %>% distinct(mortality_locus)

ds_suicide_min %>% print()


# organize columns --------------------------------------------------------
# to help us with understanding the data let us organize the columns of both data sets
# to highlight their similarity/difference
ds_population_min <- ds_population_min %>% 
  dplyr::select(county, year, sex, age_group, race, ethnicity, dplyr::everything())

ds_suicide_min <- ds_suicide_min %>% 
  dplyr::select(county, year, sex, age_group, race, ethnicity, dplyr::everything())

ds_population_min
ds_suicide_min


# TASK 2 ------------------------------------------------------------------


# ---- tweek-data --------------------------------------------------------------
ds_population <- ds_population %>% 
  dplyr::rename(
    n_population = count # to remind us what we are counting
  )

ds_suicide    <- ds_suicide %>% 
  dplyr::mutate(
    mortality_cause = gsub("^Suicide: ","",mortality_cause),
    mortality_cause = gsub(" \\(.+\\)","",mortality_cause)
  ) %>% 
  dplyr::rename(
    n_suicides = resident_deaths # to remind us what we are counting
  ) %>% 
  dplyr::select(-mortality_locus)
ds_suicide %>% dplyr::distinct(mortality_cause)


# save to disk ----------------------------