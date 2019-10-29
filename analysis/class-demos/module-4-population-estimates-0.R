rm(list=ls(all=TRUE)) # because we need a clear memory, free of variables from previous runs
cat("\f") # because old output in the console is distracting

# ---- load-packages -----------------------------------------------------------
library("magrittr") # Piping, see https://r4ds.had.co.nz/pipes.html
library("ggplot2") # Graphing, see https://r4ds.had.co.nz/data-visualisation.html
requireNamespace("dplyr") # Data wrangling, see https://r4ds.had.co.nz/transform.html
# ---- load-sources ------------------------------------------------------------
# no exteral scripts are referenced, if there were, they would be typed here

# ---- load-data -------------------------------------------------------------

# ---- inspect-data -------------------------------------------------------------

# ---- tweak-data --------------------------------------------------------------
# this data set was pulled from the Population Estimates report by Florida Dept of Health
# let us rename one of the columns to help us remember what we are counting

# we would also like to create a new variable that combines the categories of `race` and `ethnicity`

# ---- inspect-data-2 --------------------------------------------------------------
# the unit of analysis of this dataframe is county-year-sex-race-ethnicity-age_group
# this means that there is a distinct value of `n_population` (renamed `count`)
# for each unique combination of values in colums `county`, `year`, `sex`,`race`,`ethnicity`, and `age_group`

# population estimates for which years does this dataset provide? 
# in other words:
# whare are the distinct values in the column `year`

# ---- example-1 ------------------------------------
# This example demonstrates the basics of `dplyr` operations, please read section 5.3.1 of https://r4ds.had.co.nz/transform.html
# Question: what is the total number residents of Orange county in 2017?
# First, let us print all the rows that match this description

# Now, we need to add up all values in the column `n_population`

# a better way of getting this sum would be with the use of `group_by` statement 

# this way, we can add additional columns and break down the sum by them

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

# ---- task-4 ---------------------------------------
# Task: Study the script in example 4 and create the graphs for tasks 2 and 3

# Remember that data from task 2 (t2) answers the following question:
# What is the number of female residents in Dade county for each year?
# gt2 <- t2 %>% 

# Remember that data from task 3 (t3) answers the following question:
# How many non-white men of each age group resided in Orange county in 2015?
# gt3 <- t3 %>% 
