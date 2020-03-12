# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md", figure)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./manipulation/stitched-output/0-greeter.md", )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below. 
base::source("../../scripts/common-functions.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
library(magrittr) # pipes
library(dplyr)    # disable when temp lines are removed
library(ggplot2)  # graphs
library(ggpubr)   # documents
library(shiny)
  
# ---- declare-globals ---------------------------------------------------------
path_input_population <- "../../data-unshared/derived/1-greeted-population.rds"


html_flip <- FALSE

# ----- custom-functions --------------------------------------
get_a_sample <- function(
  d,
  varname            # unique of these
  ,sample_size
  ,show_all = FALSE
){
  # varname = "offense_arrest_cd"
  sample_pool <- ds %>% 
    dplyr::distinct_(.dots = varname) %>% na.omit() %>% 
    as.list() %>% unlist() %>% as.vector() 
  if(show_all){ sample_size = length(sample_pool)}
  selected_sample <- sample_pool %>% sample(size = sample_size, replace = FALSE )
  
  return(selected_sample)
}  
# How to use
# ds %>% get_a_sample("person_id",  5)
# ds %>% get_a_sample("offense_arrest_cd",  5, show_all = T) 
# set.seed(42)
# target_sample <- ds %>% 
#   dplyr::filter(n_offenses > 1L) %>% 
#   get_a_sample("person_id", 500)

# ---- load-data ---------------------------------------------------------------
ds_population  <- readRDS(path_input_population)
ds_population %>% pryr::object_size(); ds_population %>% class(); ds_population %>% names()


# ---- tweak-data ---------------------------------------------

lvl_age_group <- c(
  "less_than_1"         =   "<1"          
  ,"1_4"                =   "1-4"  
  ,"5_9"                =   "5-9"  
  ,"10_14"              =   "10-14"    
  ,"15_19"              =   "15-19"    
  ,"20_24"              =   "20-24"    
  ,"25_34"              =   "25-34"    
  ,"35_44"              =   "35-44"    
  ,"45_54"              =   "45-54"    
  ,"55_64"              =   "55-64"    
  ,"65_74"              =   "65-74"    
  ,"75_84"              =   "75-84"    
  ,"85_plus"            =   "85+"      
)

ds1 <-  ds_population %>% 
  filter(year >= 2006 ) %>% 
  mutate(
    racethnicity = paste0(race, " + ", ethnicity)
    ,age_group   = factor(
      age_group
      ,levels = names(lvl_age_group)
      ,labels = lvl_age_group
      )
    ,year        = factor(
      year
      ,levels = year
      ,labels = year
    )
  )
    

ds1 %>% count(racethnicity)

ds_grouped_totals <- ds1 %>% 
  group_by(year,sex,racethnicity,age_group) %>% 
  summarise(
    n_people = sum(count, na.rm = TRUE)
  ) %>% 
  ungroup()



# ----- basic-questions -------------------------------------------------

# ---- g1  --------------------------------------------------------------
# 
# ds1 %>% 
#   group_by(year,racethnicity,age_group) %>% 
#   summarise(
#     n_people = sum(count, na.rm = TRUE)
#   ) %>% 
#   ggplot(aes(x = year, y = n_people, group = racethnicity, color = racethnicity)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~age_group, scales = "free_y") 


g1 <- ds1 %>% 
  group_by(year,sex,racethnicity,age_group) %>% 
  summarise(
    n_people = sum(count, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = n_people, color = sex, group = interaction(racethnicity, sex))) +
  geom_line() +
  facet_grid(racethnicity ~ age_group, scales = "free_y")
g1



# ---- g2 ----------------------------------------------------------------------

#User would choose age group to examine


selectInput("age_group", label = "Select Age Group"
            ,choices = ds_grouped_totals$age_group
            ,selected = "less_than_1" )


renderPlot({ 

g2 <-  ds_grouped_totals %>% 
  filter(age_group == input$age_group) %>%   #filter will be user determined
#could also have more then one group
  ggplot(aes(x = year, y = n_people, color = racethnicity, group = racethnicity)) +
  geom_line() +
  facet_grid(sex ~ .) 
  
g2
  
})




# ---- g3 ----------------------------------------------------------------------

# bar graph by year, user can select year to disply

# ds_totals <- ds_grouped_totals %>% 
#   group_by(year, sex) %>% 
#   mutate(
#     t_people = sum(n_people)
#   ) %>% 
#   ungroup()

selectInput("year", label = "Select Year"
            ,choices  = levels(ds_grouped_totals$year)
            ,selected = levels(ds_grouped_totals$year[1]) )

renderPlot({

g3 <- ds_grouped_totals %>% 
  filter(year == input$year) %>% 
  ggplot(aes(x = age_group, y = n_people)) +
  geom_col(color = "red") +
  facet_grid(sex ~ racethnicity)
g3
})
  


# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/population-estimates/population-estimates.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



