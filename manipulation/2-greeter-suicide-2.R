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
path_input       <- "./data-unshared/raw/cause113-county/"

# ---- load-data ---------------------------------------------------------------
#
input_files <- list.files(path_input,pattern = ".xlsx$", full.names = T)
# when downloading the tables the interface couldn't handle all years at once

ls_input <- list()

for(i in seq_along(input_files)){
  element_name <- gsub(pattern = ".xlsx", replacement = "",input_files[i])
  year_i <- stringr::str_sub(basename(element_name),-4)
  ls_input[[year_i]] <- readxl::read_excel(input_files[i], col_names = TRUE, skip = 4)
}


# ---- tweak-data ----

for(i in names(ls_input)){
  names_all           <- names(ls_input[[i]])
  names_part2         <- names_all[8:length(names_all)]
  custom_names_part1  <- c("county"
                           ,"locus1"
                           ,"locus2"
                           ,"cause"
                           ,"sex"
                           ,"race"
                           ,"ethnicity" 
                          )
  names(ls_input[[i]]) <- c(custom_names_part1,names_part2)
  
  d <- ls_input[[i]] %>% tidyr::fill(custom_names_part1) %>% 
    filter_at(vars(custom_names_part1),all_vars(!stringr::str_detect(.,"Total"))) %>% 
    select(-locus1, -locus2, -starts_with("Total"), -starts_with("..."))
  
  
  names_stem <- setdiff(custom_names_part1,c("locus1","locus2"))
  names_body <- setdiff(names(d), names_stem)
  
  d_out <- d %>% 
    tidyr::pivot_longer(names_body
                        ,names_to  = "age"
                        ,values_to = "n_suicides"
                        ) %>% 
    mutate_at("age", as.integer)
  
  ls_input[[i]] <- d_out
  
}



# ---- combine-data ----

ds0 <- ls_input %>% bind_rows(.id = "year")

# ---- save-to-disk ----------------------------

ds0 %>% readr::write_rds("./data-unshared/derived/2-greeted-suicide-2.rds"
                         ,compress = 'gz')
# for read-only inspection
ds0 %>% readr::write_csv("./data-unshared/derived/2-greeted-suicide-2.csv") 


# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/2-greeter/2-greeter-suicide-2.Rmd"
  ,output_format = c(
    "html_document" 
    # ,"pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)




# ---- single-file-testing ----

# used for testing script, unneeded for final production script

# input_file <- "./data-unshared/raw/cause113-county/cause113-county-2018.xlsx"
# 
# ds0 <- readxl::read_excel(input_file, col_names = TRUE, skip = 4)
# 
# names_all <- names(ds0)
# names_part2 <- names_all[8:length(names_all)]
# custom_names_part1 <- c("county"
#                         ,"locus1"
#                         ,"locus2"
#                         ,"cause"
#                         ,"sex"
#                         ,"race"
#                         ,"ethnicity"
# )
# names(ds0) <- c(custom_names_part1,names_part2)
# 
# #GOT IT!!!!
# 
# ds1 <-  ds0 %>% tidyr::fill(custom_names_part1) %>%
#   filter_at(vars(custom_names_part1),all_vars(!stringr::str_detect(.,"Total"))) %>% 
# # filter(locus1 != "Total") %>%
# # filter(locus2 != "Total") %>%
# # filter(cause != "Total") %>%
# # filter(sex != "Total") %>%
# # filter(race != "Total") %>%
# # filter(ethnicity != "Total") %>%
#  select(-locus1, -locus2) %>%
#   select(-starts_with("Total"), -starts_with("..."))
# 
# names_stem <- setdiff(custom_names_part1,c("locus1", "locus2"))
# names_body <- setdiff(names(ds1), names_stem)
# 
# 
# ds2 <- ds1 %>% tidyr::pivot_longer(names_body
#                                    ,names_to  = "age"
#                                    ,values_to = "n_suicides") %>%
#   mutate(
#     year = "2018"
#   )
# 

