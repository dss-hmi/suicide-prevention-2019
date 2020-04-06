rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # pipes %>% 
library(ggplot2)  # graphs
library(dplyr)    # data wrangling
requireNamespace("tidyr")  # data tidying
requireNamespace("readxl") # data import

# ---- declare-globals ---------------------------------------------------------
# you will need to replace this path to the location where you stored your data file
path_file_input <- "./data-unshared/raw/FloridaPopulation/race-ethnicity-sex-age_group-age/FloridaPopulation-2006-2020.xlsx"
# a copy was saved along with the blogpost report for greater reproducibility
# path_file_input <- "./analysis/blogposts/florida-demographic-growth/data/FloridaPopulation.xlsx"

# ---- load-data ---------------------------------------------------------------
ds0 <-  readxl::read_excel(path_file_input, col_names = FALSE, skip = 3)
# ---- tweak-data-1 -----------------------------------------------------
ds1 <- ds0 # duplicate to preserve the original
# because we removed the row with columns names during import:
names(ds1) <- c("race","ethnicity","sex","age_group","age",as.character(c(2006:2020)),"total")
ds1 %>% dplyr::glimpse(90)

# Note, we will create a separate name (e.g. `ds1`, `ds2`, `ds3`, etc) only for the dataframes 
#we intend to keep, otherwise we will overwrite the existing to augment with trivial changes

# ---- tweak-data-2 -------------------------------------
ds1 <- ds1 %>%
  # carry observations forward to fill cells missing due to Excel structure ()
  tidyr::fill(race, ethnicity,sex,age_group, age) %>% 
  dplyr::filter(age != "Total")  # because makes data untidy, we'll compute totals later
ds1 %>% dplyr::glimpse(90)

# ---- tweak-data-4 -------------------------------------
ds1 %>% dplyr::distinct(age_group) # to find out what strings got misinterpreted
ds1 %>% 
  dplyr::filter(age_group %in% c("43834","43960","44118")) %>% 
  dplyr::distinct(age_group, age) # to identify how to recode
# imlement the recoding
ds1 <- ds1 %>%   
  dplyr::mutate(
    age_group = stringr::str_replace(age_group, "43834", "1-4")
    ,age_group = stringr::str_replace(age_group, "43960", "5-9")
    ,age_group = stringr::str_replace(age_group, "44118", "10-14")
  ) %>%
  dplyr::select(-total) # because it makes no sense in this context (adds up across the rows)
ds1 %>% glimpse(90)

# ---- tweak-data-5 ---------------
ds1 <- ds1 %>% 
  dplyr::mutate(
    age_group5 = dplyr::case_when(
      age %in% c(0:4)    ~ "00-04"
      ,age %in% c(5:9)   ~ "05-09"
      ,age %in% c(10:14) ~ "10-14"
      ,age %in% c(15:19) ~ "15-19"
      ,age %in% c(20:24) ~ "20-24"
      ,age %in% c(25:29) ~ "25-29"
      ,age %in% c(30:34) ~ "30-34"
      ,age %in% c(35:39) ~ "35-39"
      ,age %in% c(40:44) ~ "40-44"
      ,age %in% c(45:49) ~ "45-49"
      ,age %in% c(50:54) ~ "50-54"
      ,age %in% c(55:59) ~ "55-59"
      ,age %in% c(60:64) ~ "60-64"
      ,age %in% c(65:69) ~ "65-69"
      ,age %in% c(70:74) ~ "70-74"
      ,age %in% c(75:79) ~ "75-79"
      ,age %in% c(80:84) ~ "80-84"
      ,age > 85          ~ "85+"
    )
  ) %>% 
  dplyr::select(race,ethnicity,sex,age_group,age,age_group5, dplyr::everything())

ds1 %>% dplyr::distinct(age_group, age_group5) # to inspect the result

# ---- tweak-data-6 -------------------
# to translate into a longer form with respect to year
ds2 <- ds1 %>%
  tidyr::pivot_longer(cols = as.character(2006:2020),names_to = "year", values_to = "count") 
ds2 %>% dplyr::glimpse(90)
# `ds2` is the source set for computing totals because it has both `age_group`` categories

# ---- tweak-data-7 ------------------
lvl_age_groups <-c(
  "<1"
  ,"1-4"
  ,"5-9"
  ,"10-14"
  ,"15-19"
  ,"20-24"
  ,"25-34"
  ,"35-44"
  ,"45-54"
  ,"55-64"
  ,"65-74"
  ,"75-84"
  ,"85+"
)

lvl_age_groups5 <- c(
  "00-04"
  ,"05-09"
  ,"10-14"
  ,"15-19"
  ,"20-24"
  ,"25-29"
  ,"30-34"
  ,"35-39"
  ,"40-44"
  ,"45-49"
  ,"50-54"
  ,"55-59"
  ,"60-64"
  ,"65-69"
  ,"70-74"
  ,"75-79"
  ,"80-84"
  ,"85+"
)
ds2 <- ds2 %>%
  dplyr::mutate(
    race_ethnicity = factor(paste0(race, " + ", ethnicity))
    ,race          = factor(race)
    ,ethnicity     = factor(ethnicity)
    ,age_group     = factor(age_group, levels = lvl_age_groups)
    ,age_group5    = factor(age_group5, levels = lvl_age_groups5)
    ,age           = as.integer(age)
    ,sex           = factor(sex)
    ,year          = as.integer(year)
  ) %>% 
  dplyr::select(race, ethnicity, sex, age_group,age, age_group5, dplyr::everything())
ds2 %>% dplyr::glimpse(90)


# ---- tweak-data-8 -----------------------
ds_age_group <- ds2 %>% 
  dplyr::group_by(race, ethnicity, sex, age_group, year) %>% 
  dplyr::summarize(
    count = sum(count, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    race_ethnicity = paste0(race, " + ", ethnicity),
    race_ethnicity = factor(race_ethnicity)
  )

ds_age_group5 <- ds2 %>% 
  dplyr::group_by(race, ethnicity, sex, age_group5, year) %>% 
  dplyr::summarize(
    count = sum(count, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    race_ethnicity = paste0(race, " + ", ethnicity),
    race_ethnicity = factor(race_ethnicity)
  )

# ---- save-to-disk -------------------
list(
  "ds_wide"        = ds1
  ,"ds_long"       = ds2
  ,"ds_age_group"  = ds_age_group
  ,"ds_age_group5" = ds_age_group5
) %>%
  # readr::write_rds("./analysis/blogposts/florida-demographic-growth/data/clean_data.rds")
# a copy is saved to the native github/dss-hmi/suicide-prevention-2019/ repository  
# readr::write_rds("./data-unshared/derived/FloridaPopulation/race-ethnicity-sex-age_group-age/FlordiaPopulation-2006-2020.rds")
readr::write_rds("./data-unshared/derived/1-greeter-population-2.rds")


# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/case-study-florida-population/case-study-florida-population.Rmd"
  ,output_format = c(
    "html_document"
    # ,"pdf_document"
    # ,"md_document"
    # "word_document"
  )
  ,clean=TRUE
)



