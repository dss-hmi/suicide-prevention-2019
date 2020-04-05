#this script contains the offline population graphs used in the shiny app
#3-31-20 Update:  updating graphs to use new data set


rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below. 
base::source("./scripts/common-functions.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
library(magrittr) # pipes
library(dplyr)    # disable when temp lines are removed
library(ggplot2)  # graphs
library(ggpubr)   # documents
library(shiny)
library(plotly)
library(rlang)

# ---- declare-globals ---------------------------------------------------------
path_input_population <- "./data-unshared/derived/clean_data.rds"



# ---- load-data ---------------------------------------------------------------
ds_population <- readRDS(path_input_population)

ds_population5 <- ds_population[["ds_age_group3"]]



# color vars --------------------------------------------------------------

race_ethnicity_colors = c(
  "Black & Other + Hispanic"       = "#e66101"
  ,"Black & Other + Non-Hispanic"  = "#fdb863"
  ,"White + Hispanic"              = "#b2abd2"
  ,"White + Non-Hispanic"          = "#5e3c99"
  )
                          
#--- graphing functions ------------------------------------------------------


make_line_graph <- function(ds,x,y,..., group = NULL, color = NULL){
  # browser()
  x <- enquo(x)
  y <- enquo(y)
  group <- enquo(group)
  color <- enquo(color)
  g <- ds %>%
  ggplot(
    aes(
      x = !!x
      ,y = !!y
      ,group = !!group
      ,color = !!color
      )
    ) +
    geom_line(aes(...)) 
  return(g)
}



add_facets <- function(..., ncol = 1, scale = NULL  ){
  facet_wrap(vars(...),ncol = ncol, scales = scale)
}




ds_test <- ds_population5 %>% 
  filter(age_group5 == "20-24")

make_line_graph(ds_test,year,count, group = race_ethnicity, color = race_ethnicity) +
  add_facets(sex) +
  scale_color_manual(values = race_ethnicity_colors)

make_line_graph(ds_population5,year,count
                ,group = interaction(race_ethnicity,sex)
                ,color = race_ethnicity
                ,linetype = sex) +
  add_facets(age_group5, ncol = 4) +
  scale_color_manual(values = race_ethnicity_colors)



# ---- total population --------------------------------------------------------
g <- ds_population5 %>% 
  filter(age_group5 == "20-24") %>%   #filter would be user input  
  ggplot(
    aes(
      x      = year
      ,y     = count
      ,color = race_ethnicity
      ,group = race_ethnicity
    )
  ) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  geom_vline(
    xintercept = 2018
    ,linetype  = "dashed"
    ,alpha     = 0.2
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(min(ds_population5$year),max(ds_population5$year),1)) +
  # facet_grid(sex ~ .) +
  facet_wrap(sex ~ ., nrow = 2) +
  labs(
    x      = NULL
    ,y     = "Total Persons"
    ,color = NULL
  )
g


