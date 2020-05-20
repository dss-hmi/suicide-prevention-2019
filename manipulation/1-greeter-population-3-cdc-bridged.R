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
  (year_i <- stringr::str_sub(basename(element_name),-4))
  ls_input[[year_i]] <- readr::read_tsv(input_files[i]
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
      "age", ~stringr::str_remove_all(.," years")) %>% 
    mutate_at(
      "age", as.integer)
}

ds0 <- ls_input %>% bind_rows(.id = "year")

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









# Study the following example for the batch processing
# 
# ls_input <- list()
# for(i in seq_along(input_files)){
#   # i <- 1
#   element_name <- input_files[i] %>% names()
#   if(element_name %in% c("count_Total", "rate_Total")){
#     ls_input[[element_name]] <- readxl::read_excel(input_files[i], col_names = FALSE, skip = 3)
#   }else{
#     ls_input[[element_name]] <- readxl::read_excel(input_files[i], col_names = FALSE, skip = 4)
#   }
#   
#   # View(ls_input[[element_name]])
# }
# 
# 
# for(i in names(ls_input)){
#   # i <- ls_input[1] %>% names()
#   # element_name <- ls_input[[i]] %>% names()
#   names(ls_input[[i]]) <-  c("external","injury","mortality_cause", "sex","age_group","age", c(2006:2018),'total')
#   ls_input[[i]]<- ls_input[[i]] %>% 
#     dplyr::mutate_all(fill_last_seen) %>% 
#     dplyr::mutate(
#       mortality_cause = ifelse(mortality_cause == "Suicide By Firearms Discharge (X72-X74)","Firearms", mortality_cause)
#       ,mortality_cause = ifelse(mortality_cause == "Suicide By Other & Unspecified Means & Sequelae (X60-X71, X75-X84, Y87.0)","Other", mortality_cause)
#     ) %>% 
#     dplyr::select(-external, -injury) %>% 
#     dplyr::distinct() %>%
#     dplyr::mutate(
#       measure = gsub("(\\w+)_(\\w+)$", "\\1",i)
#       ,race    = gsub("(\\w+)_(\\w+)$", "\\2",i)
#     ) %>% 
#     dplyr::select(measure, race, dplyr::everything()) %>% 
#     dplyr::select(-total) %>% 
#     dplyr::mutate(
#       `2006`  = as.numeric(`2006`)
#       ,`2007` = as.numeric(`2007`)
#       ,`2008` = as.numeric(`2008`)
#       ,`2009` = as.numeric(`2009`)
#       ,`2010` = as.numeric(`2010`)
#       ,`2011` = as.numeric(`2011`)
#       ,`2012` = as.numeric(`2012`)
#       ,`2013` = as.numeric(`2013`)
#       ,`2014` = as.numeric(`2014`)
#       ,`2015` = as.numeric(`2015`)
#       ,`2016` = as.numeric(`2016`)
#       ,`2017` = as.numeric(`2017`)
#       ,`2018` = as.numeric(`2018`)
#       
#     )
# }