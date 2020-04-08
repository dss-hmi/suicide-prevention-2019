# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
# These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console 

# This script reads two files: patient event table + location map. 
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(dplyr) # manipulation
library(ggplot2) #graphing
library(gghighlight)
library(lubridate) # work with dates
library(gganimate)
library(tidyr)

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
source("./manipulation/function-support.R")  # assisting functions for data wrangling and testing
source("./manipulation/object-glossary.R")   # object definitions
source("./scripts/common-functions.R")        # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions

# ---- declare-globals ---------------------------------
path_to_input <- "./data-unshared/derived/cause113-age10-age99/cause113-age10-age99.rds"

# ---- load-data -------------------------------------------------------------
ds0 <- readRDS(path_to_input) 
# ---- inspect-data -------------------------------------------------------------

# ---- tweak-data --------------------------------------------------------------
ds_no_total <- ds0 %>% filter(!age == "Total", !race == "Total")

ds_only_total <- ds0 %>% filter(age == "Total")

purrr::map(ds_no_total %>% select(-age,-rate), unique)

# ---- basic-table --------------------------------------------------------------
ds_no_total %>% dplyr::group_by(age,sex) %>% summarize(n = n()) %>% print(n = nrow(.))

sex_i <- "Male"
age_group_i <- "20-24"
race_i <- "white"
# a single case 
d <- ds_only_total %>% 
  filter(sex == sex_i, age_group == age_group_i, race == race_i) %>% 
  group_by(age_group, sex) 
d

# Kyle's solution
age_model <- function(df) {
  lm(count ~ year, data = df)
}


ds_lm <- ds_only_total %>%
  dplyr::filter(
    race != "Total"
    ,sex != "Total"
    ,mortality_cause != "Total"
    ,age_group != "Total"
  ) %>% 
  ## filter a single condition for easier testing
  # dplyr::filter(
  #   race == "white"
  #   ,sex == "Male"
  #   ,mortality_cause == "Firearms"
  #   ,age_group == "20-24"
  # ) %>%
  dplyr::select(-age) %>% 
  drop_na() %>%
  mutate(
    year = as.numeric(year)
    ) %>%
  group_by(race,sex,age_group,mortality_cause) %>%
  nest() %>%
  mutate(
    model    = purrr::map(data, age_model)
    ,results = purrr::map(model, broom::augment)
  ) %>% 
  mutate(
     slope = purrr::map(model,`[`, "coefficients")
    ,slope = purrr::map(slope, unlist)
    ,slope = as.numeric(purrr::map(slope,2))
    ,int = purrr::map(model,`[`, "coefficients")
    ,int = purrr::map(int, unlist)
    ,int = as.numeric(purrr::map(int,1))
  ) %>% 
  unnest(results) %>% ungroup() %>% 
  dplyr::select(
    race, mortality_cause, sex, age_group, year, count, .fitted, int, slope
  ) %>% 
  dplyr::mutate(
    # ntile = Hmisc::cut2(slope, g = 10)
    ntile = dplyr::ntile(slope, n = 10)
  ) %>% 
  dplyr::rename(
    predicted = `.fitted`
  )



g <- ds_lm %>% 
  ggplot(aes(x = year, y = predicted))+
  geom_line(
    aes(
      group = interaction(age_group, sex, race, mortality_cause)
      ,color = (ntile ==10)
      )
    )+
  # facet_wrap(~ntile)+ # this might be better to compare
  theme_bw()
g

g2 <- ds_lm %>% 
  ggplot(aes(x = year, y = count))+
  geom_smooth(
    aes(
      group = interaction(age_group, sex, race, mortality_cause)
    ), se = F
  )+
  theme_bw()
g2
g3 <- plotly::ggplotly(g2)
g3

# ---- basic-graph --------------------------------------------------------------
g1b <-  ds_lm %>% 
  # mutate(age = as.factor(age_group)) %>% 
  filter(race == "white", mortality_cause == "Firearms")%>%
  ggplot(aes(x = year, y = count, color = age, group = interaction(age,sex))) +
  geom_smooth(method = "lm", se = F, na.rm = TRUE )+
  # geom_line(show.legend = FALSE, na.rm = TRUE) +
  facet_grid(sex ~ .) +
  gghighlight(slope > 0.8 , use_direct_label = FALSE, calculate_per_facet = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_bw()

g1b

# Sonata form report structure
# ---- dev-a-0 ---------------------------------
# ---- dev-a-1 ---------------------------------
# ---- dev-a-2 ---------------------------------
# ---- dev-a-3 ---------------------------------
# ---- dev-a-4 ---------------------------------
# ---- dev-a-5 ---------------------------------

# ---- dev-b-0 ---------------------------------
# ---- dev-b-1 ---------------------------------
# ---- dev-b-2 ---------------------------------
# ---- dev-b-3 ---------------------------------
# ---- dev-b-4 ---------------------------------
# ---- dev-b-5 ---------------------------------

# ---- recap-0 ---------------------------------
# ---- recap-1 ---------------------------------
# ---- recap-2 ---------------------------------
# ---- recap-3 ---------------------------------


# ---- publish ---------------------------------------
path_report_1 <- "./reports/*/report_1.Rmd"
path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1,path_report_2)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      # "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

