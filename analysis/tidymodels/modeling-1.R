#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)
library(tidymodels)

# ---- declare-globals ---------------------------------------------------------

path_to_file <- "./data-unshared/derived/9-population-suicide-2.rds"

# ---- load-data ---------------------------------------------------------------

ds0 <- read_rds(path_to_file)

county_info <- read_csv("./data-public/metadata/county_names.csv")

# ---- tweak-data ---------
# 
# ds1 <- ds0 %>% 
#   pivot_wider(names_from = cause, values_from = n_suicides)


# ---- merge-county-info ----

ds2 <- ds0 %>% 
  left_join(county_info, by = c("county" = "ds_population_suicide")) %>% 
  select(-corrections, -ggplot_map_data)



# ---- prep-data ----

ds3 <- ds2 %>% 
  group_by(year, gender, age, race_ethnicity, region) %>%
  summarise(.groups = "keep"
    ,population = sum(population, na.rm = TRUE)
    ,n_suicides = sum(n_suicides, na.rm = TRUE)
  ) %>% 
  mutate_at(.vars = c("age", "year", "region"), .funs = as.factor)


# ---- split-data -----

set.seed(444)

data_split <- initial_split(ds3, prop = 3/4)

train_data <- training(data_split)
test_data  <- testing(data_split)


suicide_rec <- recipe(race_ethnicity ~ age + n_suicides + region, data = train_data)

suicide_model <- logistic_reg() %>% set_engine("glm")

suicide_wrkfl <- workflow() %>% add_model(suicide_model) %>%  add_recipe(suicide_rec)

suicide_fits <-  suicide_wrkfl %>% fit(data = train_data)



