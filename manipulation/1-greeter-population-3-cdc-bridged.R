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
# path_input       <- "data-unshared/raw/cdc-bridged/Bridged-Race Population Estimates 2018.txt"

path_folder <- "./data-unshared/raw/cdc-bridged"

# ---- load-data ---------------------------------------------------------------
# ds <- readr::read_delim(path_input, 
#                         "\t", escape_double = FALSE, trim_ws = TRUE)

input_files <- list.files(path_folder, full.names = TRUE)



ls_input <- list()

for(i in seq_along(input_files)){
  element_name <- gsub(pattern = ".txt", replacement = "",input_files[i])
  year_i <- stringr::str_sub(basename(element_name),-4)
  if(stringr::str_detect(element_name,"01")){
    element_title <- paste0(year_i, "_01to42")
  }
  if(stringr::str_detect(element_name,"43")){
    element_title <- paste0(year_i, "_43to84")
  }
  if(stringr::str_detect(element_name,"less")){
    element_title <- paste0(year_i, "lessthan1and85plus")
  }
  
  ls_input[[element_title]] <- readr::read_tsv(input_files[i]
                                              , trim_ws = TRUE)
}


# ----- tweak-data ---------------
# https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697
# find county codes here



for(i in names(ls_input)){
  # i <- names(ls_input)[1]
  # (year_i <- stringr::str_sub(basename(i),-4))
  # names(ls_input[[i]]) <- year_i
  ls_input[[i]] <- ls_input[[i]] %>% 
    # mutate(
    #   year =  as.integer(year_i)
    # ) %>% 
    rename_all(snakecase::to_snake_case) %>% 
    select(-notes, -race_code,-ethnicity_code,-gender_code,-age_code ) %>%
    mutate_at(
      "county", ~stringr::str_remove_all(.," County, FL")
    ) %>% 
    mutate_at(
      "age", ~stringr::str_remove_all(.,c(" years| year"))) %>% 
    mutate_at(
      "age", ~stringr::str_replace_all(., c("< 1"  = "0"
                                            ,"85\\+" = "86"))) %>%
    mutate_at(
      "age", as.integer)
}

ds0 <- ls_input %>% bind_rows(.id = "year") %>% 
  mutate_at(
    "year", ~stringr::str_sub(.,0,4)
  ) %>% 
  mutate_at(
    "year", as.integer
  )

# ---- tweak-data-1 -----


ds0 %>% group_by(ethnicity) %>% 
  count()

ds1 <- ds0 %>% 
  mutate(
    race_f = forcats::fct_recode(race,
                                "Black & Other"  =  "American Indian or Alaska Native" 
                                ,"Black & Other" =  "Asian or Pacific Islander"        
                                ,"Black & Other" =  "Black or African American")
    ,ethnicity_f = forcats::fct_recode(ethnicity,
                                    "Hispanic"      = "Hispanic or Latino"
                                    ,"Non-Hispanic" = "Not Hispanic or Latino")
  )




# save-to-disk ------------------------------------------------------------


ds1 %>% readr::write_rds("./data-unshared/derived/1-greeted-population-3-cdc.rds"
                         ,compress = 'gz')
ds1 %>% readr::write_csv("./data-unshared/derived/1-greeted-population-3-cdc.csv")

