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
# path_file_input       <- "./data-unshared/raw/GLS targeted areas _Final.xlsx"
# path_file_input       <- "./data-unshared/raw/GLS-roster-2019-05-10.xlsx"
path_file_input       <- "./data-unshared/raw/GLS-roster-2019-05-10.xlsx"

# ---- load-data ---------------------------------------------------------------
# source 1 : Data from Nebraska dept of Corrections
# create a list of data sets
ls_ds <- list(
  "central"    = readxl::read_excel(path_file_input,sheet = "central")
  ,"southeast" = readxl::read_excel(path_file_input,sheet = "southeast" )
  ,"northeast" = readxl::read_excel(path_file_input,sheet = "northeast" )
)

lapply(ls_ds, dplyr::glimpse)

# ---- tweak-1 -----------------------------------------------------
# capture basic grooming sequence in a function to be applied to each sheet
basic_grooming <- function(d){
  # remove the right shoulder
  first_five_colnames <- names(d)[1:5]
  d <- d %>% dplyr::select(first_five_colnames)
  # because names contained non-text characters:  
  colnames(d) <- gsub(" " ,"_",colnames(d)) %>% tolower()
  colnames(d) <- gsub("/" ,"_",colnames(d)) 
  colnames(d) <- gsub("#" ,"n",colnames(d)) 
  # because sections were separated by a group of empty cells  
    d <- d %>% 
      dplyr::filter(
        !( is.na(dates) & is.na(county_zipcode) & is.na(type_training) )
      ) %>% 
      dplyr::filter(
        ! grepl("(^TOTAL)",dates)
      )
    return(d)
}
# apply grooming to each sheet
for(i in names(ls_ds) ){
  ls_ds[[i]] <- ls_ds[[i]] %>% basic_grooming()
}
ls_ds[["central"]] %>% dplyr::glimpse()
# to handle as a single flat dataframe:
ds <- ls_ds %>% dplyr::bind_rows( .id = "region")

ds <- ds %>% 
  dplyr::mutate(
    dates = ifelse(dates %in% c("missing", "date missing"), NA, dates)
  )

# ---- tweak-2 ------------------------------------------------------
# Dates are stored in a variety of formats. Make them conform.

# separate those in proper date format, numeric relative date
d0 <- ds %>% 
  dplyr::filter(grepl("^(\\d{5})$" , dates) | is.na(dates) ) %>%
  dplyr::mutate(date = lubridate::as_date(as.integer(dates), origin = "1900-01-01"))
d0 %>% dplyr::glimpse(80)

# all those needing special attention
d1 <- ds %>% 
  dplyr::filter( ! grepl("^(\\d{5})$" , dates)| is.na(dates) ) 

# dates stored as M/D/YY
d2 <- ds %>% 
  dplyr::filter(grepl("^(\\d{1,2})\\/(\\d{1,2})\\/(\\d{2,4})$", dates)) %>% 
  dplyr::mutate(
    date_char = gsub("(\\d{1,2})\\/(\\d{1,2})\\/(\\d{2,4})$", "\\1-\\2-\\3", dates)
  )

# dates stored as  MMDDYY with a "C" prefix
d3 <- ds %>% 
  dplyr::filter(grepl("^C(\\d+)\\/(\\d{6})", dates) ) %>% 
  dplyr::mutate(
    date_char  =  gsub("^C(\\d+)\\/(\\d{2})(\\d{2})(\\d{2})$", "\\2-\\3-\\4", dates)
  )

# dates stored as M/D/YY or M/D/YYYY with a "C" prefix
d4 <- ds %>% 
  dplyr::filter(grepl("^C(\\d+): ?.+", dates)) %>% 
  dplyr::mutate(
      date_char  =  gsub("^C(\\d+)(: ?)(\\d{1,2})\\/(\\d{1,2})\\/(\\d{2,4})$", "\\3-\\4-\\5", dates)
  )

# dates stored as M.D.YY 
d5 <- ds %>% 
  dplyr::filter(grepl("^(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2,4})$", dates)) %>% 
  dplyr::mutate(
    date_char  =  gsub("^(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2,4})$", "\\1-\\2-\\3", dates)
  )

# combine all special cases into a singl df
dd <- list(d2,d3,d4,d5) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(
    date_char      = gsub("  "," ", date_char)
    ,day           = gsub("^(\\d+)-(\\d+)-(\\d+)$","\\2", date_char)
    ,month         = gsub("^(\\d+)-(\\d+)-(\\d+)$","\\1", date_char)
    ,year          = gsub("^(\\d+)-(\\d+)-(\\d+)$","\\3", date_char)
    ,year          = ifelse(nchar(year)==2L, paste0("20",year), year)
    ,date_standard = paste0(year,"-",month,"-",day)
    ,date          = lubridate::as_date(date_standard)
  ) %>% 
  dplyr::select_(.dots = c(
    "region","audience","dates","county_zipcode","type_training","n_trained", "date"
    )
  )
# inspect what was not addressed 
d <- d1 %>% dplyr::left_join(dd)# see what is yet to be adjusted, quick inspection

# combine the unproblematic dates with corrected dates
ds_combined <- list(d0, dd) %>% 
  dplyr::bind_rows() 

# ---- tweak-3 ---------------------------------
# separate and standardize county names and zipcode
counties_without_zipcode <- c(
  "West Palm Beach",
  "Indian River County",
  "Palm Beach",
  "Fort Lauderdale"
)

ds_combined <- ds_combined %>% 
  dplyr::filter(!county_zipcode == "/3276") %>% # correct typo
  dplyr::mutate(
    county_zipcode = ifelse(county_zipcode=="missing",NA,county_zipcode)
  ) %>% 
  dplyr::mutate(
    county   = gsub("^(.+)\\/(.+)$", "\\1", county_zipcode)
    ,zipcode = gsub("^(.+)\\/(.+)$", "\\2", county_zipcode)
    ,zipcode = ifelse(zipcode %in% c(counties_without_zipcode,"missing"),NA, zipcode)
  ) %>% 
  # manual correction of county name spelling
  dplyr::mutate(
    county = ifelse(county %in% c("Indian River County"), "Indian River", county)
    ,county = ifelse(county %in% c("St John","St. John's","St. Johns"), "St Johns", county)
    ,county = ifelse(county %in% c("St Luice","St. Lucie"), "St Lucie", county)
  )
# check unique spelling of counties
ds_combined %>% dplyr::arrange(county) %>% dplyr::distinct(county) %>% print(n = nrow(.))
ds_combined %>% dplyr::arrange(zipcode) %>% dplyr::distinct(zipcode) %>% print(n = nrow(.))
ds_combined %>% dplyr::arrange(county,zipcode) %>% dplyr::distinct(county,zipcode) %>% print(n = nrow(.))

# dplyr::select(c("date","region","county_zipcode", "county","zipcode", "type_training","n_trained","audience"))

# ---- tweak-4 --------------------------------
ds_combined %>% 
  dplyr::group_by(audience, type_training) %>% 
  dplyr::summarize(
    n_trained = sum(n_trained)
  ) %>% 
  # dplyr::distinct(type_training) %>% 
  dplyr::arrange() %>%
  print(n = nrow(.))

# ---- save-to-disk ----------------------------


ds_combined %>% pryr::object_size()
ds_combined %>%          saveRDS("./data-unshared/derived/0-greeted-gls.rds")
ds_combined %>% readr::write_csv("./data-unshared/derived/0-greeted-gls.csv") # for read-only inspection

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



