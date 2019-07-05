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
path_input       <- "./data-unshared/raw/FloridaRankings/"

# ---- load-data ---------------------------------------------------------------
#
input_files <- list.files(path_input,pattern = ".xls", full.names = T)
# create list of relevent sheets per input file
# sheets chosen: Outcomes & Factors Rankings
ls_ds = list(
  "ds_2010" = readxl::read_excel(input_files[1],sheet = "Outcomes & Factors Rankings"),
  "ds_2011" = readxl::read_excel(input_files[2],sheet = "Outcomes & Factors Rankings"),
  "ds_2012" = readxl::read_excel(input_files[3],sheet = "Outcomes & Factors Rankings"),
  "ds_2013" = readxl::read_excel(input_files[4],sheet = "Outcomes & Factors Rankings"),
  "ds_2014" = readxl::read_excel(input_files[5],sheet = "Outcomes & Factors Rankings"),
  "ds_2015" = readxl::read_excel(input_files[6],sheet = "Outcomes & Factors Rankings"),
  "ds_2016" = readxl::read_excel(input_files[7],sheet = "Outcomes & Factors Rankings"),
  "ds_2017" = readxl::read_excel(input_files[8],sheet = "Outcomes & Factors Rankings"),
  "ds_2018" = readxl::read_excel(input_files[9],sheet = "Outcomes & Factors Rankings"),
  "ds_2019" = readxl::read_excel(input_files[10],sheet = "Outcomes & Factors Rankings")
)
#---- tweak data ----------------------------------------------------------------
head(ls_ds[1])
class(ls_ds[[1]])
#workflow : 
# 1. add year column
# 2. put all sheets together
# 3. clean

#---- tweak-1 -------------------------------------------------------------------
# add year column for each file in list
library(tibble)
for(i in c(1:10)){
  ls_ds[[i]] <- ls_ds[[i]] %>% tibble::add_column(year = 2009+i)
}
library(data.table)
ds <- rbindlist(ls_ds)
#hard coding names for columns
names(ds) <- c("FIPS","State","County","Health-Outcomes-Z-score",
               "Health-Outcomes-Rank","Health-Factors-Z-score",
               "Health-Factors-Rank","year")
# remove na rows
ds <- na.omit(ds)
#remove non-sense header rows
ds <- ds %>% dplyr::filter(!County == "County")

head(ds)
#---- future-tweaks-&-tables -----------------------------------------------------

# ---- save-to-disk ----------------------------
ds %>% pryr::object_size()
ds %>%          saveRDS("./data-unshared/derived/3-greeter-rankings.rds")
ds %>% readr::write_csv("./data-unshared/derived/3-greeted-rankings.csv") # for read-only inspection

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/3-greeter/3-greeter-rankings.Rmd"
  ,output_format = c(
    "html_document" 
    # ,"pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



