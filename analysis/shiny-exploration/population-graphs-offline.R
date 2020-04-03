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

# ---- declare-globals ---------------------------------------------------------
path_input_population <- "./data-unshared/derived/clean_data.rds"



# ---- load-data ---------------------------------------------------------------
ds_population <- readRDS(path_input_population)

ds_population5 <- ds_population[["ds_age_group3"]]



# ---- total population --------------------------------------------------------

sex <- c("Male", "Female")


plots <- ds_population5 %>% 
  filter(age_group5 == "20-24") %>% 
  {lapply(sex, function(x){
    filter(.,sex == x) %>% 
    plot_ly(.,x =~year, y = ~count, color = ~race_ethnicity, showlegend = FALSE) %>% 
      add_lines() 
  })}

fig <- subplot(plots, nrows = 2, shareX = TRUE)

fig


# ---- ggplot total population -------------------------------------------------

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
  facet_grid(sex ~ .) +
  labs(
    x      = NULL
    ,y     = "Total Persons"
    ,color = NULL
  )
fig <- ggplotly(g) %>%
  layout(
    legend = list(
      orientation = "h"
      ,y = -0.2
    )
  )
  
fig

