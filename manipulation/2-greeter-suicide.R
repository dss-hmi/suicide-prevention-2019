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
path_input       <- "./data-unshared/raw/FloridaDeathsReport/"

# ---- load-data ---------------------------------------------------------------
#
input_files <- list.files(path_input,pattern = ".xlsx$", full.names = T)
# when downloading the tables the interface couldn't handle all years at once
ls_ds <- list(
  "1" =  readxl::read_excel(input_files[1], col_names = FALSE, skip = 2)
  ,"2" = readxl::read_excel(input_files[2], col_names = FALSE, skip = 2)
  ,"3" = readxl::read_excel(input_files[3], col_names = FALSE, skip = 2)
)
# to bring all batches into the same dataframe:
ds0 <- ls_ds %>% dplyr::bind_rows()
ds0 %>% head(30) %>% neat()
# ---- tweak-data -----------------------------------------------------
# hardcoding, because cheaper and easy to check
names(ds0) <- c(
  "county","year","mortality_locus","mortality_cause","age_group","sex","race","ethnicity","resident_deaths"
)

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
ds1 %>% dplyr::glimpse(100)

ds1 <- ds1 %>% 
  dplyr::mutate(
    age_group = tolower(age_group)
    ,age_group = gsub("-","_",age_group)
    ,age_group = gsub("\\+","_plus",age_group)
    ,age_group = gsub("'","",age_group)
  )
ds1 %>% 
  dplyr::distinct(age_group)
# because it will be easier to compute on site + demonstration of summary logic
ds2 <- ds1 %>% 
  dplyr::filter(! county    == "Total") %>% 
  dplyr::filter(! year      == "Total") %>% 
  dplyr::filter(! mortality_locus      == "Total") %>% 
  dplyr::filter(! mortality_cause      == "Total") %>% 
  dplyr::filter(! sex       == "Total") %>% 
  dplyr::filter(! race      == "Total") %>% 
  dplyr::filter(! ethnicity == "Total") %>% 
  dplyr::filter(! age_group == "total")  

# ---- save-to-disk ----------------------------
ds2 %>% pryr::object_size()
ds2 %>%          saveRDS("./data-unshared/derived/2-greeted-suicide.rds")
ds2 %>% readr::write_csv("./data-unshared/derived/2-greeted-suicide.csv") # for read-only inspection

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



