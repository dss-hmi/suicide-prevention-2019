# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md", figure)
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
library(magrittr) # pipes
library(dplyr)    # disable when temp lines are removed
library(ggplot2)  # graphs
library(ggpubr)   # documents
  
# ---- declare-globals ---------------------------------------------------------
path_file_input <- "./data-unshared/derived/0-greeted-gls.rds"
html_flip <- FALSE
# ---- load-data ---------------------------------------------------------------
dto      <- readRDS(path_file_input)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()

# assign aliases for this report
ds <- dto

# ----- custom-functions --------------------------------------
get_a_sample <- function(
  d,
  varname            # unique of these
  ,sample_size
  ,show_all = FALSE
){
  # varname = "offense_arrest_cd"
  sample_pool <- ds %>% 
    dplyr::distinct_(.dots = varname) %>% na.omit() %>% 
    as.list() %>% unlist() %>% as.vector() 
  if(show_all){ sample_size = length(sample_pool)}
  selected_sample <- sample_pool %>% sample(size = sample_size, replace = FALSE )
  
  return(selected_sample)
}  
# How to use
# ds %>% get_a_sample("person_id",  5)
# ds %>% get_a_sample("offense_arrest_cd",  5, show_all = T) 
# set.seed(42)
# target_sample <- ds %>% 
#   dplyr::filter(n_offenses > 1L) %>% 
#   get_a_sample("person_id", 500)

# ---- tweak-data ---------------------------------------------------------------

# create auxilary variables
ds <- ds %>% 
  dplyr::mutate(
    year = lubridate::year(date)
    ,month = lubridate::month(date)
    ,weekday = lubridate::wday(date)
  )
ds %>% glimpse()

# ----- basic-questions -------------------------------------------------

#How many counties and zipcodes were engaged by the program?
#How many distincts training types?
#How many individuals received training?
ds %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarize(
    n_counties = length(unique(county))
    ,n_zipcodes = length(unique(zipcode))
    ,n_training_types = length(unique(type_training))
    ,total_persons_trained = sum(na.omit(n_trained))
    )

ds %>% 
  dplyr::group_by(region, county,zipcode) %>% 
  dplyr::summarize(
     n_zipcodes = length(unique(zipcode))
    ,n_program_types = length(unique(type_training))
    ,total_persons_trained = sum(n_trained)
    ) %>% 
  neat()

# ---- basic-graph -------------------------------------------------------

g1 <- ds %>% 
  ggplot2::ggplot(aes(x = date, y = ))

# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/gls-activity/gls-activity-1.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



