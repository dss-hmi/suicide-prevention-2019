# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
# These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console 

# This script reads two files: patient event table + location map. 
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html

# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R")        # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions

# ---- load-globals ------------------------------------------------------------

path_input <- "./data-unshared/derived/talahassee/7_20/7_20-full.rds"
# ---- load-data -------------------------------------------------------------
ds <- readRDS(path_input)
# ---- inspect-data -------------------------------------------------------------
ds %>% select(race, sex, mortality_cause) %>% lapply(unique)
# ---- tweak-data --------------------------------------------------------------

# ---- basic-table --------------------------------------------------------------
ds %>% filter(race == "Total", sex == "Total", mortality_cause == "Total") %>% 
  neat()
# ---- basic-graph --------------------------------------------------------------


# compute change function -------------------------------------------------

compute_change <- function(
  d
){
  # (d <- ds %>%  filter(race == "Total", sex == "Total", mortality_cause == "Total"))
  # reference_year = 2018
  # browser()
  # reference_year * 1
  d <- d %>% dplyr::mutate(year = as.integer(year))
  reference_year <- d %>% select(year) %>% max()
  single_reference_year <- d %>%
    filter(year == reference_year) %>% select(year) %>% 
    as.list() %>% unlist() %>% as.vector() %>% 
    length == 1L 
  
  if(single_reference_year){
    d1 <- d %>% 
      dplyr::mutate(
        count_ref = filter(., year == reference_year) %>% select(count) %>% as.list() %>% unlist() %>% as.vector()
        ,rate_ref = filter(., year == reference_year) %>% select(rate) %>% as.list() %>% unlist() %>% as.vector()
      ) %>% 
      dplyr::mutate(
        change_count = (count - count_ref)/count
        ,change_rate  = (rate - rate_ref)/rate
      ) %>% 
      dplyr::select(-count_ref, -rate_ref)
    # d1 %>% select(-race, -mortality_cause, -sex)
  }else{
    cat("ERROR: more than one occurence of the reference year\n")
  }
  return(d1)
}
# how to use
# ds %>% filter(
#   race             == "Total"
#   ,sex             == "Total"
#   ,mortality_cause == "Total") %>% 
#   # compute_change(reference_year == 2018)
#   compute_change()


# Sonata form report structure
# ---- dev-a-0 ---------------------------------
ds %>% filter(
  race             == "Total"
  ,sex             == "Total"
  ,mortality_cause == "Total") %>% 
  # compute_change(reference_year == 2018)
  compute_change() %>% print()
# ---- dev-a-1 ---------------------------------
ds %>% filter(
  race             == "Total"
  # ,sex             == "Total"
  # ,mortality_cause == "Total"
  ) %>% 
  # compute_change(reference_year == 2018)
  compute_change() %>% print()
# ---- dev-a-2 ---------------------------------
# ---- dev-a-3 ---------------------------------
# ---- dev-a-4 ---------------------------------
# ---- dev-a-5 ---------------------------------

# ---- dev-b-0 ---------------------------------
# ---- dev-b-1 ---------------------------------
# ---- dev-b-2 ---------------------------------
# ---- dev-b-3 ---------------------------------
# ---- dev-b-4 ---------------------------------
# ---- dev-b-5 ---------------------------------

# ---- recap-0 ---------------------------------
# ---- recap-1 ---------------------------------
# ---- recap-2 ---------------------------------
# ---- recap-3 ---------------------------------


# ---- publish ---------------------------------------
path_report_1 <- "./reports/*/report_1.Rmd"
path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1,path_report_2)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      # "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

