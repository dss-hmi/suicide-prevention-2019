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
# ---- tweak-data -----------------------------------------------------
names(ds0) <- c("county","year","mortality_locus","mortality_cause", "age","sex","race","ethnicity","value")
ds0 %>% dplyr::glimpse()

fill_last_seen <- function(
  column
){
  # first value is non-empty
  last_seen = column[[1]]
  count = 1L
  #fill rest of the cells with first non empty value
    for(cell in column){
    if(is.na(cell)){
      cell            = last_seen
      column[[count]] = cell
    }
    else{
      last_seen = cell
    }
    count = count +1
  }
  return(column)
}

ds1 <- ds0 %>%
  dplyr::mutate_all(fill_last_seen)
names(ds1) <- names(ds0)

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



