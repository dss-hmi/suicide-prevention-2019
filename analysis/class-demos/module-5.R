rm(list=ls(all=TRUE)) # because we need a clear memory, free of variables from previous runs
cat("\f") # because old output in the console is distracting

# ---- load-packages -----------------------------------------------------------
library("magrittr") # Piping, see https://r4ds.had.co.nz/pipes.html
library("ggplot2") # Graphing, see https://r4ds.had.co.nz/data-visualisation.html
requireNamespace("dplyr") # Data wrangling, see https://r4ds.had.co.nz/transform.html
# ---- load-sources ------------------------------------------------------------
# no exteral scripts are referenced, if there were, they would be typed here

# ---- load-data -------------------------------------------------------------
path_to_data <- "data-unshared/derived/0-greeted-gls.csv" # because we might use it later
ds <- readr::read_csv(file = path_to_data)

# ---- inspect-data -------------------------------------------------------------
ds %>% print() # because we want to view the first few rows
ds %>% dplyr::glimpse(width = 50)   # because we want to inspect properties of the tibble
# please study the data and notice that we have 4 groups of variables

# 1) geography            : `region`, `county`,`zipcode` 
# 2) date                 : `date`
# 3) features of training : `audience`, `training_type`
# 4) amount of training   : `n_trained`

# note that there may be multiple zipcodes in a county
# note that a county can belong to a single region
# these threee measures (`region`, `county`,`zipcode`) are nested within in each other

# ---- example-1 --------------------------------------------------------------
# let us inspect the features of training

# Garrett Lee Smith (GLS) suicide prevention program offers training to
# counselors and caretakers (`professionals`)  as well conducts awareness events (`community`):
ds %>% dplyr::distinct(audience)
# Depending on the audience GLS, deliver a variety of different training events:
ds %>% dplyr::distinct(audience, type_training)
# note that there are only a handful of training for professionals:
ds %>% dplyr::filter(audience=="professionals") %>% dplyr::distinct(type_training)
# while almost every event for community would be unique
ds %>% dplyr::filter(audience=="community") %>% dplyr::distinct(type_training) %>% print(n=nrow(.))
# at least it would appear so from the long list of events, so let's verify this
ds %>% 
  dplyr::filter(audience=="community") %>% 
  dplyr::group_by(type_training) %>% 
  dplyr::summarize(n_occurences = dplyr::n()) %>% # number of rows
  dplyr::arrange(desc(n_occurences)) %>% 
  print(n=nrow(.))

# ---- task-1 ------------------------------------------------------


# ---- example-2 --------------------------------------------------------------
# let us explore the geography of the observational unit
ds %>% dplyr::distinct(region)
ds %>% dplyr::distinct(region, county) %>% print(n=nrow(.))
ds %>% dplyr::distinct(region, county, zipcode) %>% 
  dplyr::arrange(region, county, zipcode) %>% 
  print(n=nrow(.))

# ---- task-2 -----------------------------------------------------------------



# ---- example-3 -------------------------------------------------------------
# let us explore the amount of training


# ---- tweak-data --------------------------------------------------------------

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
  dplyr::group_by(county, year, sex) %>%
  # dplyr::group_by(county, year, sex, race) %>%
  # dplyr::group_by(county, year, sex, race, ethnicity) %>% 
  # dplyr::group_by(county, year, sex, race, ethnicity, age_group) %>% 
  dplyr::summarize(
    n_total = sum(n_population)
  )

# ---- task-1 ---------------------------------------
# adjust the script from example 1 to create a table that answers the following question:
# Question:  How many men aged 15 to 19 lived in Dade county in 2015?

# ---- example-2 ------------------------------------
# Question: How does the population in Orange county changes over the years?

# we will use a combination of `filter` and `group_by` statements 
d2 <- ds %>% 
  dplyr::filter(county == "Orange") %>% # only "Orange" county remains
  dplyr::group_by(year) %>%  # for each year we will compute a value
  dplyr::summarize(
    n_total = sum(n_population) # this is the value we compute for each year
  )
d2 %>% print()
# ---- task-2 ---------------------------------------
# How does the population of women in Dade county changes over the years?


# ---- example-3 ------------------------------------
# What what was the age composition of female residents of Orange county in 2016?
d3 <- ds %>% 
  dplyr::filter(county == "Orange", year == 2016) %>%
  dplyr::group_by(county, year, age_group) %>%
  dplyr::summarize(
    n_total = sum(n_population)
  )
d3 %>% print()

# ---- task-3 --------------------------------------
# what was the age composition black men in Orange county in 2015?

# ---- example-4 --------------------------------------------------------------
# let us crate a basic dot plot using the data from example 2 
g2 <- d2 %>% 
  ggplot2::ggplot(aes(x = year, y = n_total))+
  geom_point(stat = "identity")+
  geom_line()
g2
  

# let us crate a basic bar plot using the data from example 3 
g3 <- d3 %>% 
  ggplot(aes(x = age_group, y = n_total ))+
  geom_bar(stat = "identity")
g3

# ---- task-4 ---------------------------------------
# using the script in example 4, create the graphs for tasks 2 and 3


# ---- publish ---------------------------------------
