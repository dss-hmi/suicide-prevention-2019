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
# you will need to replace this path to the location where you stored your data file
path_file_input       <- "./data-unshared/raw/FloridaPopulation/race-ethnicity-sex-age_group-age/FloridaPopulation.xlsx"

# ---- load-data ---------------------------------------------------------------
ds0 <-  readxl::read_excel(path_file_input, col_names = FALSE, skip = 3)

# ---- tweak-data -----------------------------------------------------
ds1 <- ds0 # duplicate to preserve the original
# because we removed the row with columns names during import
names(ds1) <- c("race","ethnicity","sex","age_group","age",as.character(c(2006:2020)),"total")
# ds1 %>% glimpse()

ds2 <- ds1 %>%
  # carry observations forward to fill cells missing due to Excel structure
  tidyr::fill(race, ethnicity,sex,age_group, age)%>%
  # adjust for automatic encoding of certain character strings by Excel
  dplyr::mutate(
    age_group = stringr::str_replace(age_group, "43834", "1-4")
    ,age_group = stringr::str_replace(age_group, "43960", "5-9")
    ,age_group = stringr::str_replace(age_group, "44118", "10-14")
  ) %>%
  dplyr::select(-total) # because it makes no sense in this context (adds up across the rows)
ds2 %>% glimpse()

# becaus we can use this vector later to structure the factor levels
# lvl_age_groups <-c(
#   "<1"
#   ,"1-4"
#   ,"5-9"
#   ,"10-14"
#   ,"15-19"
#   ,"20-24"
#   ,"25-34"
#   ,"35-44"
#   ,"45-54"
#   ,"55-64"
#   ,"65-74"
#   ,"75-84"
#   ,"85+"
#   ,"Total"
# )

lvl_age_groups <- c(
  "<1"
  ,"1-4"
  ,"5-9"
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
  ,"Total"
)

ds2$age_group[ds2$age %in% c(25:29)] <- "25-29"
ds2$age_group[ds2$age %in% c(30:34)] <- "30-34"
ds2$age_group[ds2$age %in% c(35:39)] <- "35-39"
ds2$age_group[ds2$age %in% c(40:44)] <- "40-44"
ds2$age_group[ds2$age %in% c(45:49)] <- "45-49"
ds2$age_group[ds2$age %in% c(50:54)] <- "50-54"
ds2$age_group[ds2$age %in% c(55:59)] <- "55-59"
ds2$age_group[ds2$age %in% c(60:64)] <- "60-64"
ds2$age_group[ds2$age %in% c(65:69)] <- "65-69"
ds2$age_group[ds2$age %in% c(70:74)] <- "70-74"
ds2$age_group[ds2$age %in% c(75:79)] <- "75-79"
ds2$age_group[ds2$age %in% c(80:84)] <- "80-84"

ds2 <- ds2 %>%
  dplyr::filter(!age == "Total") %>% # because we will compute new total, for new age_groups

  dplyr::mutate(
    age_group       = factor(age_group, levels = lvl_age_groups)
    ,race_ethnicity = paste0(race, " + ", ethnicity)
  ) %>%
  dplyr::select(race, ethnicity, race_ethnicity, dplyr::everything())

# ds2 %>% dplyr::glimpse(40)

# to create a cleaner data set
ds3 <- ds2 %>%
  dplyr::filter(age_group != "Total")

# to separate totals into a dedicated dataframe
ds_totals <- ds2 %>%
  dplyr::filter(age_group == "Total")

# to translate into a longer form with respect to year
ds4 <- ds3 %>%
  tidyr::pivot_longer(cols = as.character(2006:2020),names_to = "year", values_to = "count")

# ---- g1 -----------------------------
# graph of the total population of Florida by broken down by 4 ethnic groups (race_ethnicity)
# Hint 1: use `ds_totals`
# Hint 2: not all rows will be used, figure out which one you need
# Hint 3: you will need to pivot_longer
# Hint 4: https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
g1 <- ds_totals %>%
  dplyr::filter(sex == "Total")  %>%
  dplyr::filter(ethnicity != "Total")  %>%
  tidyr::pivot_longer(cols = as.character(2006:2020),names_to = "year", values_to = "count") %>%
  dplyr::select(-age_group, -sex) %>%
  ggplot(aes(x = year,  y = count, color = race_ethnicity))+
  geom_line(aes(group = race_ethnicity))+
  geom_point(shape = 21, fill = NA, size =2)+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = - 90,vjust =.5, hjust = -0)
    #https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
  )+
  labs(
    title = "Population growth in Florida over last 15 years \n  broken down by ethnic groups"
    ,color = "Ethnic Group"
    ,x = "Calendar Year"
    ,y = "Population Count"
  )
g1

# ----- q1 ------------------------------------
# Q: what Ethnic group is most dissimilar from the other three in their dynamics?
# Hint 1: You will neet to tweak g1 a bit to see this
# Hist 2: Dynamics means HOW they change over time
# Hint 3: https://r-graphics.org/recipe-facet-free
# Answer: "White + Non-Hispanic" because of a "dip" in lat 2000's
g1 + facet_wrap(~race_ethnicity, scale = "free_y")

# ---- g2 -------------------------------------
# Build a graph showing age composition of ethnic groups in 2019
# Hint 1: use `ds4`
# Hint 2: https://stackoverflow.com/questions/14563989/force-r-to-stop-plotting-abbreviated-axis-labels-e-g-1e00-in-ggplot2 and https://r-graphics.org/recipe-axes-tick-label
# Hint 3: https://r-graphics.org/recipe-axes-tick-label
g2 <- ds4 %>%
  dplyr::filter(year == 2019) %>%
  ggplot(aes(x = age_group, y = count)) +
  geom_col()+
  facet_grid(sex ~ race_ethnicity)+
  scale_y_continuous(labels = scales::comma)+
  # https://stackoverflow.com/questions/14563989/force-r-to-stop-plotting-abbreviated-axis-labels-e-g-1e00-in-ggplot2 also https://r-graphics.org/recipe-axes-tick-label
  theme_bw()+
  theme(
     axis.text.x = element_text(angle = - 90,vjust =.5, hjust = -0)
     #https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
  )+
  labs(
    title = "Population in Florida in 2019 broken down by age groups and gender"
    ,color = "Ethnic Group"
    ,x = "Calendar Year"
    ,y = "Population Count"
  )
g2
# ----- q2 -------------------------
# Q2 Why is there a sudden jump in numbers  after age 24 in every ethnic group?
# Answer: because the age category includes twice as many values (e.g. 20-24 vs 25-34)



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



