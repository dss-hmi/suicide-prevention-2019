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
ds0 <-  readxl::read_excel(path_file_input, col_names = FALSE, skip = 3) %>% dplyr::slice(1:1000)

# ---- tweak-data -----------------------------------------------------
# names(ds0) <- c("county","year","sex","race","ethnicity","10_14","15_19", "20_24","total") # small

names(ds0) <- c("county","year","sex","race","ethnicity",
                "less_than_1", "1_4","5_9","10_14","15_19", "20_24",
                "25_34","35_44","45_54","55_64","65_74","75_84","85_plus"
                ,"total")

d <- ds0 %>% 
  dplyr::slice(1:76) %>% 
  dplyr::select("county", "year","sex","race","ethnicity", "total")

# function to fill last seen for given column

fill_last_seen <- function(column){
  
  # first value is non-empty
  last_seen = column[[1]]
  count = 1L
  
  #fill rest of the cells with first non empty value
  
  for(cell in column){
    if(is.na(cell)){
      cell = last_seen
      column[[count]] = cell
    }
    else{
      last_seen = cell
    }
    
    count = count +1
  }
  return(column)
}

# replace with a apply function to do in one go
ds0$county <- fill_last_seen(column = ds0$county)
ds0$race <- fill_last_seen(column = ds0$race)
ds0$year <- fill_last_seen(column = ds0$year)
ds0$sex <- fill_last_seen(column = ds0$sex)
ds0$ethnicity <- fill_last_seen(column = ds0$ethnicity)

readr::write_csv(d,"./data-public/derived/fill_NA_subject.csv")
dput(d)


# ---- save-to-disk ----------------------------


ds_out %>% pryr::object_size()
ds_out %>%          saveRDS("./data-unshared/derived/0-greeted-gls.rds")
ds_out %>% readr::write_csv("./data-unshared/derived/0-greeted-gls.csv") # for read-only inspection

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/0-greeter/0-greeter-gls.Rmd"
  ,output_format = c(
    "html_document" 
    # ,"pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



