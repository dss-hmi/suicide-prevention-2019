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
path_input       <- "./data-unshared/raw/cause113-age7-age99"

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
# input_files <- list.files(path_input,pattern = ".xlsx$", full.names = T, recursive = T)

input_files <- c(
 "count_black"    = "./data-unshared/raw/cause113-age10-age99/counts/counts-cause113-age10-age99-black-non-hispanic.xlsx"
,"count_blother"  = "./data-unshared/raw/cause113-age10-age99/counts/counts-cause113-age10-age99-black-other-non-hispanic.xlsx"
,"count_latino"   = "./data-unshared/raw/cause113-age10-age99/counts/counts-cause113-age10-age99-white-hispanic.xlsx"
,"count_white"    = "./data-unshared/raw/cause113-age10-age99/counts/counts-cause113-age10-age99-white-non-hispanic.xlsx"
,"count_Total"    = "./data-unshared/raw/cause113-age10-age99/counts/counts-cause113-age10-age99.xlsx"
,"rate_black"     = "./data-unshared/raw/cause113-age10-age99/rates/rates-cause113-age10-age99-black-non-hispanic.xlsx"
,"rate_blother"   = "./data-unshared/raw/cause113-age10-age99/rates/rates-cause113-age10-age99-black-other-non-hispanic.xlsx"
,"rate_latino"    = "./data-unshared/raw/cause113-age10-age99/rates/rates-cause113-age10-age99-white-hispanic.xlsx"
,"rate_white"     = "./data-unshared/raw/cause113-age10-age99/rates/rates-cause113-age10-age99-white-non-hispanic.xlsx"
,"rate_Total"     = "./data-unshared/raw/cause113-age10-age99/rates/rates-cause113-age10-age99.xlsx"
)

# d <- readxl::read_xlsx("./data-unshared/raw/cause113-age7-age99/counts/counts-cause113-age7-age99.xlsx")

ls_input <- list()
for(i in seq_along(input_files)){
  # i <- 1
  element_name <- input_files[i] %>% names()
  if(element_name %in% c("count_Total", "rate_Total")){
    ls_input[[element_name]] <- readxl::read_excel(input_files[i], col_names = FALSE, skip = 3)
  }else{
    ls_input[[element_name]] <- readxl::read_excel(input_files[i], col_names = FALSE, skip = 4)
  }
  
  # View(ls_input[[element_name]])
}

# ---- tweak-data -----------------------------------------------------
for(i in names(ls_input)){
  # i <- ls_input[1] %>% names()
  # element_name <- ls_input[[i]] %>% names()
  names(ls_input[[i]]) <-  c("external","injury","mortality_cause", "sex","age_group","age", c(2006:2018),'total')
  ls_input[[i]]<- ls_input[[i]] %>% 
    dplyr::mutate_all(fill_last_seen) %>% 
    dplyr::mutate(
      mortality_cause = ifelse(mortality_cause == "Suicide By Firearms Discharge (X72-X74)","Firearms", mortality_cause)
      ,mortality_cause = ifelse(mortality_cause == "Suicide By Other & Unspecified Means & Sequelae (X60-X71, X75-X84, Y87.0)","Other", mortality_cause)
    ) %>% 
    dplyr::select(-external, -injury) %>% 
    dplyr::distinct() %>%
    dplyr::mutate(
      measure = gsub("(\\w+)_(\\w+)$", "\\1",i)
      ,race    = gsub("(\\w+)_(\\w+)$", "\\2",i)
    ) %>% 
    dplyr::select(measure, race, dplyr::everything()) %>% 
    dplyr::select(-total) %>% 
    dplyr::mutate(
      `2006`  = as.numeric(`2006`)
      ,`2007` = as.numeric(`2007`)
      ,`2008` = as.numeric(`2008`)
      ,`2009` = as.numeric(`2009`)
      ,`2010` = as.numeric(`2010`)
      ,`2011` = as.numeric(`2011`)
      ,`2012` = as.numeric(`2012`)
      ,`2013` = as.numeric(`2013`)
      ,`2014` = as.numeric(`2014`)
      ,`2015` = as.numeric(`2015`)
      ,`2016` = as.numeric(`2016`)
      ,`2017` = as.numeric(`2017`)
      ,`2018` = as.numeric(`2018`)
      
    )
}

# ls_input$count_Total %>% dplyr::distinct(mortality_cause)
# ls_input$count_blother %>% dplyr::distinct(mortality_cause)
# ls_input$rate_Total %>% dplyr::distinct(mortality_cause)
# 
# ls_input$count_black %>% names()
# ls_input$count_Total %>% names()
# ls_input$rate_black %>% names()
# ls_input$rate_Total %>% names()
# 
# ls_input$count_black %>% glimpse()
# ls_input$count_Total %>% glimpse()
# ls_input$rate_black  %>% glimpse()
# ls_input$rate_Total  %>% glimpse()

# lapply(ls_input,dplyr::glimpse)

# View(ls_input[[1]])

ds0 <- ls_input %>% dplyr::bind_rows() 
ds0 %>% glimpse()
ds1 <- ds0 %>% tidyr::gather("year","value", 7:19 )
ds2 <- ds1 %>% tidyr::spread(measure,value)
ds2 %>% glimpse(60)


# save-to-disk ------------------------------------------------------------


ds2 %>%          saveRDS("./data-unshared/derived/cause113-age10-age99/cause113-age10-age99.rds")
ds2 %>% readr::write_csv("./data-unshared/derived/cause113-age10-age99/cause113-age10-age99.csv") # for read-only inspection




ds <- ds2 %>% 
  filter(race == "Total") %>%
  # filter(sex == "Total") %>%
  filter(mortality_cause == "Total")


# g0 ----------------------------------------------------------------------
g0 <- ds2 %>% 
  filter(sex = "all") %>% 
  filter(race = "all")

  

# save-to-disk ------------------------------------------------------------

    
ds2 %>%          saveRDS("./data-unshared/derived/talahassee/7_20/7_20-full.rds")
ds2 %>% readr::write_csv("./data-unshared/derived/talahassee/7_20/7_20-full.csv") # for read-only inspection






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



