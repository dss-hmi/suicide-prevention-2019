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
# -2--- declare-globals ---------------------------------------------------------
path_file_input_0 <- "./data-unshared/derived/0-greeted-gls.rds"
path_file_input_1 <- "./data-unshared/derived/1-greeted-population.rds"
path_file_input_2 <- "./data-unshared/derived/2-greeted-suicide.rds"
# path_file_input       <- "./data-unshared/raw/FloridaDeathsReport/FloridaDeathsReport-full.xlsx"

# ---- load-data ---------------------------------------------------------------
#
ds0 <- readRDS(path_file_input_0) 
ds1 <- readRDS(path_file_input_1)
ds2 <- readRDS(path_file_input_2)


ds0 %>% glimpse()
ds1 %>% glimpse()
ds2 %>% glimpse()

ds0 <- ds0 %>% 
  dplyr::mutate(
    year = lubridate::year(date) %>% as.character()
  )


# there is too much temporal granularity in the GLS source
# to reduce it, making it compatible with the FL health records:
ds_gls_by_year <- ds0 %>% 
  dplyr::group_by(region, county, audience, type_training, year) %>% 
  dplyr::summarize(
    n_trained = sum(n_trained, na.rm = T)
  )
ds_population_by_year <- ds1 %>% 
  dplyr::rename(
    "population_count" = "count"
  )
ds_suicide_by_year <- ds2 %>% 
  dplyr::group_by(county, year, sex, race, ethnicity, age_group) %>% 
  dplyr::summarize(
    resident_deaths = sum(resident_deaths, na.rm = T)
  )

ls_ds <- list(
  "population" = ds_population_by_year
  ,"suicide"   = ds_suicide_by_year
  ,"gls"       = ds_gls_by_year
)

# use to limit the view during testing of the merger
for(i in c("suicide","population")){
  ls_ds[[i]] <- ls_ds[[i]] %>%
    dplyr::filter(county == "Orange") %>%
    dplyr::filter(year == 2016) %>%
    dplyr::filter(age_group == "25_34") %>%
    dplyr::filter(sex == "Female") %>%
    dplyr::filter(race == "White") %>%
    dplyr::filter(ethnicity == "Hispanic") %>%
    dplyr::arrange(sex, race, ethnicity)
}
ls_ds %>% lapply(names)

ds <- ls_ds %>% 
  Reduce(function(a,b) dplyr::left_join(a,b) , .) 


# ---- tweak-data -----------------------------------------------------

ls_ds <- list(
  "suicide"     =  ls_ds[["population"]]
  ,"gls"        = ls_ds[["gls"]]
  ,"population"  = ls_ds[["population"]]

)

ds <- ls_ds %>% 
  Reduce(function(a,b) dplyr::full_join(a,b) , .) 





print_distinct <- function(
  d
  ,group_by_variables
){
  # define values needed for testing and development inside the function:
  # d <- ds0
  # group_by_variables <- "area"
  # group_by_variables <- c("area","age_group")
  
  d_out <- d %>%
    dplyr::group_by(.dots = c(group_by_variables) ) %>%
    dplyr::count() 
  
  return(d_out)
}




# d0 <- ds0 %>% print_distinct("county") %>% print(n = nrow(.))
# d1 <- ds1 %>% print_distinct("county") %>% print(n = nrow(.))
# d2 <- ds2 %>% print_distinct("county") %>% print(n = nrow(.))
# 
# ls_ds <- list(
#   "gls"         = ds0 %>% print_distinct("county"), #dplyr::distinct(county) %>% tibble::as_tibble(),
#   "population"  = ds1 %>% print_distinct("county"), #dplyr::distinct(county) %>% tibble::as_tibble(),
#   "suicide"     = ds2 %>% print_distinct("county") #dplyr::distinct(county) %>% tibble::as_tibble()
# )
# 
# ds <- ls_ds %>% 
#   Reduce(function(a,b) dplyr::full_join(a,b, by = "county") , .) 
# 

# ----- --------
# d1 <- ds1 %>% print_distinct("age_group") %>% print(n = nrow(.))
# d2 <- ds2 %>% print_distinct("age_group") %>% print(n = nrow(.))
# 
# ls_ds <- list(
#   "population"  = ds1 %>% print_distinct("age_group") %>% print(n = nrow(.))
#   ,"suicide"     = ds2 %>% print_distinct("age_group") %>% print(n = nrow(.))
# )
# 
# ds <- ls_ds %>% 
#   Reduce(function(a,b) dplyr::full_join(a,b, by = "age_group") , .) 
# 


d0 <- ds0 %>% print_distinct("age_group") %>% print(n = nrow(.))
d1 <- ds1 %>% print_distinct("age_group") %>% print(n = nrow(.))
d2 <- ds2 %>% print_distinct("age_group") %>% print(n = nrow(.))

common_variables <- c("county","age_group")
ls_ds <- list(
  "gls       "  = ds1 %>% print_distinct(common_variables) %>% print(n = nrow(.))
  ,"population"  = ds1 %>% print_distinct(common_variables) %>% print(n = nrow(.))
  ,"suicide"     = ds2 %>% print_distinct(common_variables) %>% print(n = nrow(.))
)

ds <- ls_ds %>% 
  Reduce(function(a,b) dplyr::full_join(a,b, by = c("county","age_group") ) , .) 


# ----- -------

ds12 <- dplyr::left_join(
  ds1,
  ds2,
  by = c("county", "year", "sex")
)

# ---- save-to-disk ----------------------------


ds1 %>% pryr::object_size()
ds1 %>%          saveRDS("./data-unshared/derived/2-greeted-suicide.rds")
ds1 %>% readr::write_csv("./data-unshared/derived/2-greeted-suicide.csv") # for read-only inspection

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/2-greeter/2-greeter-suicide.Rmd"
  ,output_format = c(
    "html_document" 
    # ,"pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



