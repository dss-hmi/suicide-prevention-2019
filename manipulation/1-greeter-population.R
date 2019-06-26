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
# path_file_input       <- "./data-unshared/raw/FloridaPopulation/FloridaPopulation-small.xlsx"
path_file_input       <- "./data-unshared/raw/FloridaPopulation/FloridaPopulation-full.xlsx"

# ---- load-data ---------------------------------------------------------------
#
ds0 <-  readxl::read_excel(path_file_input, col_names = FALSE, skip = 3) #%>% dplyr::slice(1:1000)

# ---- tweak-data -----------------------------------------------------
# names(ds0) <- c("county","year","sex","race","ethnicity","10_14","15_19", "20_24","total") # small
ds1 <- ds0
names(ds1) <- c(
  "county","year","sex","race","ethnicity",
  "less_than_1", "1_4","5_9","10_14","15_19", "20_24","25_34",
  "35_44","45_54","55_64","65_74","75_84","85_plus","total"
)

# function to fill last seen for a given column
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

ds1 %>% head(20)
ds2 <- ds1 %>%
  dplyr::mutate_all(fill_last_seen)
names(ds2) <- names(ds1) # because mutate_all messes up column names
ds2 %>% dplyr::glimpse(100)

var_stem <- c("county","year","sex","race","ethnicity")
ds2 <- ds2 %>% 
  tidyr::gather(
    "age_group","count", setdiff(names(ds1), var_stem)
  )

ds2 %>% dplyr::glimpse(100)
# because it will be easier to compute on site + demonstration of summary logic
ds3 <- ds2 %>% 
  dplyr::filter(! county    == "Total") %>% 
  dplyr::filter(! year      == "Total") %>% 
  dplyr::filter(! sex       == "Total") %>% 
  dplyr::filter(! race      == "Total") %>% 
  dplyr::filter(! ethnicity == "Total") %>% 
  dplyr::filter(! age_group == "total") %>% 
  dplyr::arrange(county, year, sex, race, ethnicity, age_group) 


# ---- save-to-disk ----------------------------
ds3 %>% pryr::object_size()
ds3 %>%          saveRDS("./data-unshared/derived/1-greeted-population.rds")
ds3 %>% readr::write_csv("./data-unshared/derived/1-greeted-population.csv") # for read-only inspection

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/1-greeter/1-greeter-population.Rmd"
  ,output_format = c(
    "html_document" 
    # ,"pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



