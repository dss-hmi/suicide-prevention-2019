# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md", figure)
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
library(magrittr) # pipes
library(dplyr)    # disable when temp lines are removed
library(ggplot2)  # graphs
library(ggpubr)   # documents
  
# ---- declare-globals ---------------------------------------------------------
path_file_input <- "./data-unshared/derived/0-greeted-gls.rds"
html_flip <- FALSE
# ---- load-data ---------------------------------------------------------------
dto      <- readRDS(path_file_input)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()

# assign aliases for this report
ds <- dto

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

# ---- tweak-data ---------------------------------------------------------------

# create auxilary variables
ds <- ds %>% 
  dplyr::mutate(
    year = lubridate::year(date)
    ,month = lubridate::month(date)
    ,weekday = lubridate::wday(date)
  )
ds %>% glimpse(60)

ds %>% explore::explore( )
ds %>% explore::explore_all( )
ds %>% explore::describe()
# ----- basic-questions -------------------------------------------------

#How many counties and zipcodes were engaged by the program?
#How many distincts training types?
#How many individuals received training?
ds %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarize(
    n_counties             =  dplyr::n_distinct(county)
    ,n_zipcodes            =  dplyr::n_distinct(zipcode)
    ,n_training_types      =  dplyr::n_distinct(type_training)
    ,total_persons_trained = sum(na.omit(n_trained))
  )

ds %>% 
  dplyr::group_by(audience, region) %>% 
  dplyr::summarize(
    n_counties             =  dplyr::n_distinct(county)
    ,n_zipcodes            =  dplyr::n_distinct(zipcode)
    ,n_training_types      =  dplyr::n_distinct(type_training)
    ,total_persons_trained = sum(na.omit(n_trained))
    )

dt1 <- ds %>% 
  dplyr::group_by(region, county) %>% 
  dplyr::summarize(
    n_zipcodes             =  dplyr::n_distinct(zipcode)
    ,n_training_types      =  dplyr::n_distinct(type_training)
    ,total_persons_trained = sum(na.omit(n_trained))
    ) 

dt1 %>%  neat()

g1 <- dt1 %>% 
  tidyr::gather(
    "measure"
    ,"count"
    , c("n_zipcodes","n_training_types","total_persons_trained")
  ) %>% 
  ggplot(aes(x=county, y = count, fill = measure ))+
  geom_bar(stat = "identity")+
  coord_flip()+
  # facet_grid(region ~ measure )+
  facet_grid(region ~ measure,scales = "free")+
  theme_minimal()+
  theme( legend.position = "none")



#########################


dt2 <- ds %>% 
  dplyr::group_by(region, county, audience) %>% 
  dplyr::summarize(
    n_zipcodes             =  dplyr::n_distinct(zipcode)
    ,n_training_types      =  dplyr::n_distinct(type_training)
    ,total_persons_trained = sum(na.omit(n_trained))
  ) 

dt2 %>%  neat()

# BOTH audiences
g2 <- dt2 %>% 
  tidyr::gather(
    "measure"
    ,"count"
    , c("n_zipcodes","n_training_types","total_persons_trained")
  ) %>% 
  dplyr::mutate(
    measure = factor(measure, levels = c(
      "n_zipcodes"
      ,"n_training_types"
      ,"total_persons_trained"))
    ,audience = factor(audience, levels = c(
      "community"
      ,"professionals"
      ,NA
    ))
  ) %>%    
  ggplot(aes(x=county, y = count, color = audience, fill = audience ))+
  scale_fill_manual(values = c("professionals" = "red", "community" = NA))+
  scale_color_manual(values = c("professionals" = NA, "community" = "black"))+
  geom_bar(stat = "identity", position = "identity", alpha = .5)+
  coord_flip()+
  # facet_grid(region ~ measure )+
  facet_grid(region ~ measure, scales = "free")+
  theme_minimal()

# SINGLE audience
g3 <- dt2 %>% 
  # dplyr::filter(audience == "professionals") %>%
  dplyr::filter(audience == "community") %>%
  tidyr::gather(
    "measure"
    ,"count"
    , c("n_zipcodes","n_training_types","total_persons_trained")
  ) %>% 
  dplyr::mutate(
    measure = factor(measure, levels = c(
      "n_zipcodes"
      ,"n_training_types"
      ,"total_persons_trained")) 
  ) %>%  
  # dplyr::arrange(county) %>% 
  ggplot(aes(x=county, y = count, fill = measure, pattern = region ))+
  # ggplot(aes(x=county, y = count ))+
  geom_bar(stat = "identity", alpha = .5)+
  geom_text( aes(label = count), vjust = 0.2)+
  coord_flip()+
  facet_grid(region ~ measure, scales = "free")+
  theme_minimal()+
  theme(
    legend.position = "none"
  )
g3


# Experimental: want to make bars the same width
dt4 <- dt2 %>% 
  # dplyr::filter(audience == "professionals") %>%
  dplyr::filter(audience == "community") %>%
  tidyr::gather(
    "measure"
    ,"count"
    , c("n_zipcodes","n_training_types","total_persons_trained")
  ) %>% 
  dplyr::mutate(
    measure = factor(measure, levels = c(
      "n_zipcodes"
      ,"n_training_types"
      ,"total_persons_trained"))
    ,county_region = paste0(county,"-",toupper(region))
    ,region_county = paste0(toupper(region),"-",county)
  ) 

cr_levels <- dt4 %>% 
  # dplyr::arrange(county, region) %>% 
  dplyr::arrange(region, county) %>% 
  dplyr::ungroup() %>% 
  dplyr::distinct(county_region ) %>% 
  as.list() %>% unlist() %>% as.character()

g4 <- dt4 %>% 
  dplyr::mutate(
    county_region = factor(county_region,levels = cr_levels)
    ,county_region = factor(county_region, levels = rev(levels(county_region)))
  ) %>% 
  ggplot(aes(x=county_region, y = count, fill = measure ))+
  # ggplot(aes(x=region_county, y = count, fill = measure ))+
  geom_bar(stat = "identity", alpha = .5)+
  geom_text( aes(label = count), vjust = 0.2)+
  coord_flip()+
  facet_grid(. ~ measure, scales = "free")+
  theme_minimal()+
  theme(
    legend.position = "none"
    ,axis.text.y = element_text(hjust = 1)
    ,axis.title.y = element_blank()
  )
g4
# ---- basic-graph -------------------------------------------------------

g1 <- ds %>% 
  ggplot2::ggplot(aes(x = date, y = ))

# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/gls-activity/gls-activity-1.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



