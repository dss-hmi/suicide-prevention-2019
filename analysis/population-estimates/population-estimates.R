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
path_input_population <- "./data-unshared/derived/1-greeted-population.rds"


html_flip <- FALSE

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

# ---- load-data ---------------------------------------------------------------
ds_population  <- readRDS(path_input_population)
ds_population %>% pryr::object_size(); ds_population %>% class(); ds_population %>% names()


# ---- tweak-data ---------------------------------------------
ds_gls %>% dplyr::glimpse()

target_areas <- ds_gls %>% 
  dplyr::group_by(region, county) %>% 
  dplyr::summarize()

ds <- target_areas %>% 
  dplyr::left_join(ds_population)

target_areas <- ds_gls %>% 
  dplyr::distinct(region, county) 
  
ds_long <- ds %>% 
  tidyr::gather("age_group", "count", age_groups) %>% 
  dplyr::filter(age_group %in% c ("10_14","15_19","20_24")) %>% 
  dplyr::mutate(
    county_region = paste0(county,"-",toupper(region))
    ,region_county = paste0(toupper(region),"-",county)
  ) 


cr_levels <- ds_long %>% 
  dplyr::arrange(region, county) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(county_region ) %>% 
  as.list() %>% unlist() %>% as.character()

g1 <- ds_long %>% 
  
# ds_long %>% 
#   dplyr::group_by_(.dots = stem) %>% 
#   dplyr::summarize(
#     n_people = sum(na.omit(count))
#   )

d1 <- ds_long %>% 
  dplyr::group_by(race, ethnicity) %>% 
  dplyr::summarize(
    n_people = sum(na.omit(count))
  )
# ----- basic-questions -------------------------------------------------

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



