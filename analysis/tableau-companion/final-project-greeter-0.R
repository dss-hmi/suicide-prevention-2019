# this script imports the raw data described in this shared document 
# https://drive.google.com/file/d/10idMxy8eX8nTHr6wr2Q40x4XOP3Y5ck7/view
# and prepares a state of data used as a standard point of departure for any subsequent reproducible analytics

# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./manipulation/0-greeter.R",
#   output = "./manipulation/stitched-output/0-greeter.md"
# )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr)
library(ggplot2)
library(readr)

# ---1- declare-globals ---------------------------------------------------------
# Objective: Create character vectors storing the paths to the respective datasets

# ---- load-data ---------------------------------------------------------------
ds_population <- readr::read_csv(file = path_file_input_1)
ds_suicide <- readr::read_csv(file = path_file_input_2)

ds_population %>% glimpse()
ds_suicide %>% glimpse()

# Control + Shift + R to creted a new chunk

# tweek-data --------------------------------------------------------------
# 
# Objective A: sort the columns in `ds_population` in the the following order: county, year, sex, age_group, race, ethnicity, then everything else
# Objective B: rename colunm `count` into `n_population

ds_population <- ds_population %>% 
  
  # Objective A: sort the columns in `ds_population` in the the following order: county, year, sex, age_group, race, ethnicity, then everything else
  # Objective B: rename colunm `resident_deaths` into `n_suicide
  # Objective C: Print all unique values of `mortality_locus` (Hint: use dplyr::distinct())
  # Objective D: remove the column `mortality_ocus`  
  ds_suicide <- ds_suicide %>% 
  
  
  
  
  # minimal -----------------------------------------------------------------
# Objective A: create a minimal subset of each dataset , with just enough row to represent values
# Name the subset datasets `ds_population_min` and `ds_suicide_min` respectivley

# For both `ds_population` and `ds_suicide`:
# Keep only Orange and Brevard counties
# Keep only years 2015 and 2016
# Keep only age groups between 55 and 74 years of age
# Keep only White Non-Hispanic demography

# For `ds_suicide`
# Keep only deaths by `Firearm Discharge` and  by `Hanging, Strangulation, Suffocation`

ds_population_min <- ds_population %>% 
  
  
  ds_suicide_min <- ds_suicide %>% 
  
  
  
  # clean suicide values ---------------------------
# Objective: Remove the prefix "Suicide: " from the values in the column `mortality_cause`
ds_suicide_min <- ds_suicide_min %>% 
  dplyr::mutate(
    # add script here
  )

# merge datasets --------------------------
# Objective: Combine `ds_population` and `ds_suicide` into a single dataframe

# study the following example, then replicate for the full data frames
ds_population_suicide_min <- dplyr::left_join(
  ds_population_min
  ,ds_suicide_min
  ,by = c("county", "year", "sex", "age_group", "race", "ethnicity")
)


# compute total suicide -----------------------
# Objective: Compute the total counts of suicide, aggregating over `mortality_cause`

# Develop the study example using minimal dataframes
# Use the table views to manually verify the accuracy of the summary
# Hint: use dplyr::summarize
ds_suicide_total_min <- ds_suicide_min %>% 
  
  
  # save-to-disk ------------------------------------------------------------

ds_population %>% readr::write_csv(path = "./data-unshared/derived/florida_population_counts.csv")
ds_suicide %>% readr::write_csv(path = "./data-unshared/derived/florida_suicide_count.csv")

ds_population_suicide %>% readr::write_csv(path = "./data-unshared/derived/florida_population_suicide.csv")
ds_population_suicide_total %>% readr::write_csv(path = "./data-unshared/derived/florida_population_suicide_total.csv")

# connect to tableau --------------------
# Objective: Create 4 workbooks, each input a separate product of this script

