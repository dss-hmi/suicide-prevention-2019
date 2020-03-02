# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md")
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
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr) # disable when temp lines are removed
library(ggplot2)
library(ggpubr)
library(readxl)
# ---- declare-globals ---------------------------------------------------------
path_file_input_0 <- "./data-unshared/derived/0-greeted-gls.rds"
path_file_input_1 <- "./data-unshared/derived/1-greeted-population.rds"
path_file_input_2 <- "./data-unshared/derived/2-greeted-suicide.rds"
# path_file_input       <- "./data-unshared/raw/FloridaDeathsReport/FloridaDeathsReport-full.xlsx"
output_format = "pandoc"
# ---- load-data ---------------------------------------------------------------
ds_gls        <- readRDS(path_file_input_0) %>% dplyr::glimpse(100)
ds_population <- readRDS(path_file_input_1) %>% dplyr::glimpse(100)
ds_suicide    <- readRDS(path_file_input_2) %>% dplyr::glimpse(100)

# ---- tweak-data -----------------------------------------------------
ds_gls <- ds_gls %>% 
  dplyr::mutate(
    year = lubridate::year(date) %>% as.character() # for joining to other tables
  ) 

# there is too much temporal granularity in the GLS source.
# to reduce it, making it compatible with the FL health records:
ds_gls_by_year <- ds_gls %>% 
  dplyr::group_by(region, county, year, audience, type_training) %>% 
  dplyr::summarize(
    n_trained = sum(n_trained, na.rm = T)
  )
# note that `audience` and `type_training` are more granular than county-by-year
ds_population_by_year <- ds_population %>% 
  dplyr::rename(
    "population_count" = "count" # to be more explicit and because a better label
  )
ds_suicide_by_year <- ds_suicide %>% 
  dplyr::group_by(county, year, sex, race, ethnicity, age_group, mortality_cause) %>% 
  dplyr::summarize(
    resident_deaths = sum(resident_deaths, na.rm = T)
  )
# note that `mortality_cause` is more granular than county-by-year
# to help with elementwise list operations:
ls_ds <- list(
  "population" = ds_population_by_year # first, because most complete
  ,"suicide"   = ds_suicide_by_year
  ,"gls"       = ds_gls_by_year # last, because left join
)
ls_ds %>% lapply(dplyr::glimpse,100) %>% invisible() 

# ---- aggregate-1 ----------------------------------

# use to limit the view during testing of the merger
# for(i in c("suicide","population")){
#   ls_ds[[i]] <- ls_ds[[i]] %>%
#     dplyr::filter(county == "Orange") %>%
#     dplyr::filter(year == 2016) %>%
#     dplyr::filter(age_group == "25_34") %>%
#     dplyr::filter(sex == "Female") %>%
#     # dplyr::filter(sex == "Male") %>%
#     dplyr::filter(race == "White") %>%
#     dplyr::filter(ethnicity == "Hispanic") %>%
#     # dplyr::filter(ethnicity == "Non-Hispanic") %>%
#     dplyr::arrange(sex, race, ethnicity)
# }
ls_ds %>% lapply(names)
# to join all elements of the list, one after another
ds <- ls_ds %>% Reduce(function(a , b) dplyr::left_join( a, b ), . ) 
ds %>% dplyr::glimpse(100)
# this join preserves the granularity in the GLS file (except binning dates into years)
# note, however, that  you cannot aggregate population and suicide measures
# sums of `population_count` and `resident_deaths` are not meaningful
# because these values repeat in row to accomodate 
# a higher granularity of GLS file (audience, type_training)

# let us create a list object to capture this useful data set
ls_out <- list(
  "granularity_gls" = ls_ds
)
# ---- aggregate-2 ------------------------------------
# let us aggregate at the level of `population`
# this means that we will provide a single measure at the resolution:
# county - year - sex - age_group - race - enthicity 
# for every unique combination of values on these six we want:
# `population_count` - number of people alive 
# `resident_deaths`  - number of registered deaths among residents
# `community`        - number of people reached through community event
# `professionals`    - number of professionals who recieved training

ls_ds <- list(
  "population" = ls_ds[["population"]] 
  ,"suicide" = ls_ds[["suicide"]]  %>% 
    dplyr::group_by(county, year, sex, race, ethnicity, age_group) %>% 
    # aggregating over `mortality_casue`
    dplyr::summarize(
      resident_deaths = sum(resident_deaths, na.rm = T)
    )
  ,"gls" = ls_ds[["gls"]]  %>% 
    dplyr::group_by(region, county, year, audience) %>% 
    # aggregating over `type_training`
    dplyr::summarize(
      n_trained = sum(n_trained, na.rm = T)
    ) %>% 
    tidyr::spread(audience,n_trained)
)
ls_ds %>% lapply(dplyr::glimpse,100) %>% invisible() 
ds <- ls_ds %>% Reduce(function(a , b) dplyr::left_join( a, b ), . ) 
ds %>% dplyr::glimpse(100)

# add a new level of granularity to the output `dto`
ls_out[["granularity_population"]] <- ls_ds
# ---- save-to-disk ----------------------------


ls_out %>% pryr::object_size()
ls_out %>%          saveRDS("./data-unshared/derived/9-combined.rds")

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/9-combiner/9-combiner.Rmd"
  ,output_format = c(
    "html_document" 
    # ,"pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



