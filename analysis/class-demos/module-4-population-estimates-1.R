rm(list=ls(all=TRUE)) # because we need a clear memory, free of variables from previous runs
cat("\f") # because old output in the console is distracting

# ---- load-packages -----------------------------------------------------------
library("magrittr") # Piping, see https://r4ds.had.co.nz/pipes.html
library("ggplot2") # Graphing, see https://r4ds.had.co.nz/data-visualisation.html
requireNamespace("dplyr") # Data wrangling, see https://r4ds.had.co.nz/transform.html
# ---- load-sources ------------------------------------------------------------
# no exteral scripts are referenced, if there were, they would be typed here

# ---- load-data -------------------------------------------------------------
path_to_data <- "data-unshared/derived/1-greeted-population.csv" # because we might use it later
ds <- readr::read_csv(file = path_to_data)

# ---- inspect-data -------------------------------------------------------------
print(x = ds, n = 20 ) # print the first 20 rows of object `ds` 
# alternative way of inspecting an object:
dplyr::glimpse(ds, width = 80)      # these two lines are equivalent, they demonstate "piping" 
ds %>% dplyr::glimpse(width = 80)  # these two lines are equivalent, they demonstate "piping"

# ---- tweak-data --------------------------------------------------------------
# this data set was pulled from the Population Estimates report by Florida Dept of Health
# let us rename one of the columns to help us remember what we are counting
ds <- ds %>% 
  # one way to achieve this is to create a dublicate column with a new name and drop the original
  dplyr::mutate(
    n_population = count # create a duplicate with a new name
  ) %>% 
  dplyr::select(-count) # we drop the original column

# we would also like to create a new variable that combines the categories of `race` and `ethnicity`
ds <- ds %>% 
  dplyr::mutate(
    race_ethnicity  = paste0(race," + ", ethnicity)
  )

# ---- inspect-data-2 --------------------------------------------------------------
# the unit of analysis of this dataframe is county-year-sex-race-ethnicity-age_group
# this means that there is a distinct value of `n_population` (renamed `count`)
# for each unique combination of values in colums `county`, `year`, `sex`,`race`,`ethnicity`, and `age_group`
nrow(ds) # number of rows in the dataframe
ds %>% dplyr::distinct(county, year, sex, race, ethnicity, age_group) # number of unique combinations of values on listed variables
ds %>% dplyr::distinct(county, year, sex, race_ethnicity, age_group) # notice it produces the same count as the line above

# population estimates for which years does this dataset provide? 
# in other words:
# whare are the distinct values in the column `year`
ds %>% dplyr::distinct(year)
ds %>% dplyr::distinct(county) %>% print(n=70)

# ---- example-1 ------------------------------------
# This example demonstrates the basics of `dplyr` operations, please read section 5.3.1 of https://r4ds.had.co.nz/transform.html
# Question: what is the total number residents of Orange county in 2017?
# First, let us print all the rows that match this description
ds %>% 
  dplyr::filter(county == "Orange", year == 2017) %>% 
  print(n = nrow(.))

# Now, we need to add up all values in the column `n_population`
ds %>% 
  dplyr::filter(county == "Orange", year == 2017) %>%
  dplyr::summarize(
    n_total = sum(n_population)
  )

# a better way of getting this sum would be with the use of `group_by` statement 
ds %>% 
  dplyr::filter(county == "Orange", year == 2017) %>%
  dplyr::group_by(county, year) %>% # includes these columns in the output
  dplyr::summarize(
    n_total = sum(n_population)
  )

# this way, we can add additional columns and break down the sum by them
ds %>% 
  dplyr::filter(county == "Orange", year == 2017) %>%
  # dplyr::group_by(county, year, sex) %>%
  # dplyr::group_by(county, year, sex, race) %>%
  # dplyr::group_by(county, year, sex, race, ethnicity) %>%
  dplyr::group_by(county, year, sex, race, ethnicity, age_group) %>%
  dplyr::summarize(
    n_total = sum(n_population)
  )
# Note that we do note save the answer, but only print it into the console

# ---- task-1 ---------------------------------------
# Please adjust the script from example 1 to create a table that answers the following 
# Question: How many men aged 15 to 19 lived in Dade county in 2015?
# ds %>% 

# ---- example-2 ------------------------------------
# Let us create a table that provides the answer to the following 
# Question: How does the population in Orange county change over the years?
# in other words
# What is the total population of Orange county for each year?
# we will use a combination of `filter` and `group_by` functions from `dplyr` package 
e2 <- ds %>% 
  dplyr::filter(county == "Orange") %>% # only "Orange" county remains
  dplyr::group_by(county, year) %>%  # for each year we will compute a value
  dplyr::summarize(
    n_total = sum(n_population) # this is the value we compute for each year
  )
e2 %>% print()

# ---- task-2 ---------------------------------------
# Please adjust the script from example 2 to create a table that answers the following 
# Question: How does the population of women in Dade county change over the years?
# in other words
# What is the number of female residents in Dade county for each year?
# t2 <- ds %>% 

# ---- example-3 ------------------------------------
# Let us create a table that provides the answer to the following 
# Question: What what was the age composition of female residents of Orange county in 2016?
# in other words
# How many women of each age group resided in Orange county in 2016?
e3 <- ds %>% 
  dplyr::filter(county == "Orange", year == 2016, sex == "Female") %>%
  dplyr::group_by(county, year, sex, age_group) %>% 
  dplyr::summarize(
    n_total = sum(n_population)
  )
e3 

# ---- task-3 --------------------------------------
# Please adjust the script from example 3 to create a table that answers the following 
# Question: what was the age composition of non-white men in Orange county in 2015?
# in other words
# How many non-white men of each age group resided in Orange county in 2015?
# t3 <- ds %>% 

# ---- example-4 --------------------------------------------------------------
# let us crate a basic dot plot using the data from example 2 
# Remember that data from example 2 (e2) answers the following question:
# What is the total population of Orange county for each year?
ge2 <- e2 %>% 
  ggplot(aes(x = year, y = n_total))+
  geom_point(stat = "identity")+
  geom_line()+
  labs(title = "Total population in Orange county over the years")
ge2

# let us crate a basic bar plot using the data from example 3 
# Remember that data from example 3 (e3) answers the following question:
# How many women of each age group resided in Orange county in 2016?
ge3 <- e3 %>% 
  ggplot(aes(x = age_group, y = n_total ))+
  geom_bar(stat = "identity")+
  labs(title = "Population of women in Dade county in 2016 by age group")
ge3

# ---- task-4 ---------------------------------------
# Task: Study the script in example 4 and create the graphs for tasks 2 and 3

# Remember that data from task 2 (t2) answers the following question:
# What is the number of female residents in Dade county for each year?
# gt2 <- t2 %>% 

  
# Remember that data from task 3 (t3) answers the following question:
# How many non-white men of each age group resided in Orange county in 2015?
# gt3 <- t3 %>% 

