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
path_input       <- "./data-unshared/raw/talahassee/12_18/"

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
# ---- load-data ---------------------------------------------------------------
#
input_files <- list.files(path_input,pattern = ".xlsx$", full.names = T, recursive = T)

# input_files <- c(
#  "count_black"   = "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-black-non-hispanic.xlsx"      
# ,"count_blother" = "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-black-other-non-hispanic.xlsx"
# ,"count_latino"  = "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-white-hispanic.xlsx"          
# ,"count_white"   = "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-white-non-hispanic.xlsx"      
# ,"count_Total"   = "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18.xlsx"                         
# 
# ,"rate_black"    =   "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-black-non-hispanic.xlsx"        
# ,"rate_blother"  =   "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-black-other-non-hispanic.xlsx"  
# ,"rate_latino"   =   "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-white-hispanic.xlsx"            
# ,"rate_white"    =   "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-white-non-hispanic.xlsx"        
# ,"rate_Total"    =   "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18.xlsx"                         
# )  

input_files <- list(
   "counts_cause_sex_allrace"         =  "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18.xlsx"
  ,"rates_cause_sex_allrace"         =  "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18.xlsx"

  ,"counts_cause_sex_black"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-black-non-hispanic.xlsx"
  ,"counts_cause_sex_blother" =  "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-black-other-non-hispanic.xlsx"
  ,"counts_cause_sex_latino"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-white-hispanic.xlsx"
  ,"counts_cause_sex_white"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-white-non-hispanic.xlsx"
  ,"rates_cause_sex_black"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-black-non-hispanic.xlsx"
  ,"rates_cause_sex_blother" =  "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-black-other-non-hispanic.xlsx"
  ,"rates_cause_sex_latino"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-white-hispanic.xlsx"
  ,"rates_cause_sex_white"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-white-non-hispanic.xlsx"

  ,"counts_sex_cause_allrace"         =  "./data-unshared/raw/talahassee/12_18/counts/counts-sex-cause(113)-year(2004-2018)-12_18.xlsx"
  ,"rates_sex_cause_allrace"         =  "./data-unshared/raw/talahassee/12_18/rates/rates-sex-cause(113)-year(2004-2018)-12_18.xlsx"

  ,"counts_sex_cause_black"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-sex-cause(113)-year(2004-2018)-12_18-black-non-hispanic.xlsx"
  ,"counts_sex_cause_blother" =  "./data-unshared/raw/talahassee/12_18/counts/counts-sex-cause(113)-year(2004-2018)-12_18-black-other-non-hispanic.xlsx"
  ,"counts_sex_cause_latino"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-sex-cause(113)-year(2004-2018)-12_18-white-hispanic.xlsx"
  ,"counts_sex_cause_white"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-sex-cause(113)-year(2004-2018)-12_18-white-non-hispanic.xlsx"
  ,"rates_sex_cause_black"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-sex-cause(113)-year(2004-2018)-12_18-black-non-hispanic.xlsx"
  ,"rates_sex_cause_blother" =  "./data-unshared/raw/talahassee/12_18/rates/rates-sex-cause(113)-year(2004-2018)-12_18-black-other-non-hispanic.xlsx"
  ,"rates_sex_cause_latino"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-sex-cause(113)-year(2004-2018)-12_18-white-hispanic.xlsx"
  ,"rates_sex_cause_white"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-sex-cause(113)-year(2004-2018)-12_18-white-non-hispanic.xlsx"

)
# 
# ls_input <- list(
#   "cause_sex"       = list(
#     "counts_cause_sex"         =  "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18.xlsx"      
#     ,"rates_cause_sex"         =  "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18.xlsx"                         
#     
#   )
#   ,"cause_sex_race" = list(
#     "counts_cause_sex_black"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-black-non-hispanic.xlsx"      
#     ,"counts_cause_sex_blother" =  "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-black-other-non-hispanic.xlsx"
#     ,"counts_cause_sex_latino"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-white-hispanic.xlsx"          
#     ,"counts_cause_sex_white"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-cause(113)-sex-year(2004-2018)-12_18-white-non-hispanic.xlsx"      
#     ,"rates_cause_sex_black"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-black-non-hispanic.xlsx"        
#     ,"rates_cause_sex_blother" =  "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-black-other-non-hispanic.xlsx"  
#     ,"rates_cause_sex_latino"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-white-hispanic.xlsx"            
#     ,"rates_cause_sex_white"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-cause(113)-sex-year(2004-2018)-12_18-white-non-hispanic.xlsx"        
#     
#   )
#   ,"sex_cause"      = list(
#     "counts_sex_cause"         =  "./data-unshared/raw/talahassee/12_18/counts/counts-sex-cause(113)-year(2004-2018)-12_18.xlsx"      
#     ,"rates_sex_cause"         =  "./data-unshared/raw/talahassee/12_18/rates/rates-sex-cause(113)-year(2004-2018)-12_18.xlsx"                         
#     
#   )
#   ,"sex_cause_race" = list(
#     "counts_sex_cause_black"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-sex-cause(113)-year(2004-2018)-12_18-black-non-hispanic.xlsx"      
#     ,"counts_sex_cause_blother" =  "./data-unshared/raw/talahassee/12_18/counts/counts-sex-cause(113)-year(2004-2018)-12_18-black-other-non-hispanic.xlsx"
#     ,"counts_sex_cause_latino"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-sex-cause(113)-year(2004-2018)-12_18-white-hispanic.xlsx"          
#     ,"counts_sex_cause_white"   =  "./data-unshared/raw/talahassee/12_18/counts/counts-sex-cause(113)-year(2004-2018)-12_18-white-non-hispanic.xlsx"      
#     ,"rates_sex_cause_black"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-sex-cause(113)-year(2004-2018)-12_18-black-non-hispanic.xlsx"        
#     ,"rates_sex_cause_blother" =  "./data-unshared/raw/talahassee/12_18/rates/rates-sex-cause(113)-year(2004-2018)-12_18-black-other-non-hispanic.xlsx"  
#     ,"rates_sex_cause_latino"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-sex-cause(113)-year(2004-2018)-12_18-white-hispanic.xlsx"            
#     ,"rates_sex_cause_white"   =  "./data-unshared/raw/talahassee/12_18/rates/rates-sex-cause(113)-year(2004-2018)-12_18-white-non-hispanic.xlsx"          
#     
#   )
# )

ls_input <- list()
for(i in seq_along(input_files)){
  # i <- 1
  element_name <- input_files[i] %>% names()
  ls_input[[element_name]] <- readxl::read_excel(input_files[[i]], col_names = FALSE, skip = 4)
}

# ---- tweak-data -----------------------------------------------------
for(i in names(ls_input)){
  # i <- ls_input[1] %>% names()
  # element_name <- ls_input[[i]] %>% names()
  if(i %in% grep("^counts_sex.+|^rates_sex",names(ls_input),value = T ) ){
    names(ls_input[[i]]) <-  c("sex","external","injury","mortality_cause", c(2004:2018),'total')
  }else{
    names(ls_input[[i]]) <-  c("external","injury","mortality_cause", "sex",c(2004:2018),'total')
  }
  ls_input[[i]]<- ls_input[[i]] %>% 
    dplyr::mutate_all(fill_last_seen) %>% 
    dplyr::mutate(
      mortality_cause = ifelse(mortality_cause == "Suicide By Firearms Discharge (X72-X74)","Firearms", mortality_cause)
      ,mortality_cause = ifelse(mortality_cause == "Suicide By Other & Unspecified Means & Sequelae (X60-X71, X75-X84, Y87.0)","Other", mortality_cause)
    ) %>% 
    dplyr::select(-external, -injury) %>% 
    dplyr::distinct() %>%
    dplyr::mutate(
      measure = gsub("^(\\w+)_(\\w+)_(\\w+)_(\\w+)$", "\\1",i)
      ,order    = gsub("^(\\w+)_(\\w+)_(\\w+)_(\\w+)$", paste0("\\2","_","\\3"), i)
      ,race    = gsub("^(\\w+)_(\\w+)_(\\w+)_(\\w+)$", "\\4",i)
    ) %>% 
    dplyr::select(order, measure, race, dplyr::everything()) %>% 
    dplyr::select(-total)
}

# View(ls_input[[1]])

ds0 <- ls_input %>% dplyr::bind_rows() 
ds0 %>% distinct(measure)
ds0 %>% glimpse()
ds1 <- ds0 %>% tidyr::gather("year","value", 5:19 )
ds2 <- ds1 %>% tidyr::spread(measure,value)


ds2 %>% glimpse(60)

# separate into 

# ds <- ds2 %>% 
#   filter(race == "Total") %>%
#   # filter(sex == "Total") %>%
#   filter(mortality_cause == "Total")


# g0 ----------------------------------------------------------------------
# g0 <- ds2 %>% 
#   filter(sex = "all") %>% 
#   filter(race = "all")
# 
#   

# save-to-disk ------------------------------------------------------------
race_combined <- ds2 %>% 
  dplyr::filter(race == "Total")

race_separate <- ds2 %>% 
  dplyr::filter(! race == "Total")
    
ds2 %>%          saveRDS("./data-unshared/derived/talahassee/12_18/12_18-full.rds")
ds0 %>% readr::write_csv("./data-unshared/derived/talahassee/12_18/12_18-full.csv") # for read-only inspection

race_combined %>%          saveRDS("./data-unshared/derived/talahassee/12_18/race_combined.rds")
race_combined %>% readr::write_csv("./data-unshared/derived/talahassee/12_18/race_combined.csv") # for read-only inspection

race_separate %>%          saveRDS("./data-unshared/derived/talahassee/12_18/race_separate.rds")
race_separate %>% readr::write_csv("./data-unshared/derived/talahassee/12_18/race_separate.csv") # for read-only inspection






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



