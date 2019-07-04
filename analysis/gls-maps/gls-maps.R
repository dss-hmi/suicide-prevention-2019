# Lines before the first chunk are invisible to Rmd/Rnw callers
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below. 
base::source("./scripts/common-functions.R")

# ---- load-packages -----------------------------------------------------------
install.packages(c("cowplot", "googleway", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
# Attach these packages so their functions don't need to be qualified
library(magrittr) # pipes
library(dplyr)    # disable when temp lines are removed
library(ggplot2)  # graphs
library(ggpubr)   # documents
library(ggmap)
library(maps)
library(sf)
library(mapdata)
# ---- declare-globals ---------------------------------------------------------
path_file_input <- "./data-unshared/derived/9-combined.rds"
html_flip <- FALSE
baseSize <- 10
# ---- load-data ---------------------------------------------------------------
dto      <- readRDS(path_file_input)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()

# ----- custom-functions --------------------------------------

# ---- tweak-data ---------------------------------------------------------------
# to collapse into a single data frame
ds <- dto[["granularity_population"]] %>% 
  Reduce(function(a , b) dplyr::left_join( a, b ), . )

ds %>% explore::describe_all()

# to help us filter out those counties that had programming
counties_gls <- ds %>% 
  na.omit(region) %>% 
  dplyr::distinct(county) %>% 
  as.list() %>% unlist() %>% as.character()
# to view the total programming delivered (between 2015 and 2017)
ds %>% 
  dplyr::filter(county %in% counties_gls) %>% 
  dplyr::distinct(county, year, community, professionals ) %>% 
  na.omit() %>% 
  dplyr::group_by(county) %>% 
  dplyr::summarize(
    community      = sum(community)
    ,professionals = sum(professionals)
  ) %>% 
  dplyr::arrange(desc(professionals))
# to aid interpretation and graphing
ds <- ds %>% 
  dplyr::rename(
    "deaths_by_suicide" = "resident_deaths" # to remind what we count
  ) %>% 
  dplyr::mutate(
    # to have a standardized measure / put counties on the same scale
    suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
    # to have a single variable describing racial background
    ,racethnicity = paste0(race," + ", ethnicity)
    # to aid in graph production ( note the spaces at the end of "NE  ")
    ,rgn = car::recode(
      region,
      "
      'central'  ='CN'
      ;'southeast'='SE'
      ;'northeast'='NE  '
      "
    )
    ) %>% 
  dplyr::select(county, year, sex, age_group, race, ethnicity, racethnicity, # context
                region, rgn, # support for graphing and grouping
                population_count, deaths_by_suicide, suicide_rate_per100k, #measures
                community, professionals # treatment
  )

ds %>% explore::describe_all()

# to remind out how to aggregate 
d1 <- ds %>% 
  dplyr::distinct(county, year, region, rgn, community, professionals) %>% 
  na.omit()
# ---- Map-1 ----------------------------------
# get florida county coordinates
counties <- map_data("county")
fl_counties <- subset(counties, region == "florida")
head(fl_counties)
#plot florida
fl_base <- ggplot(data = fl_counties, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(data = fl_counties, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_nothing()
fl_base
# create table for year=x and training_type = y
year_var = 2015L
pro = T

create_map <- function(year_var=2015L, pro=T){
  #function body
  d1_2015 <- d1 %>% 
    dplyr::filter(year == year_var) %>% 
    dplyr::mutate(subregion = paste0(tolower(county)))
  fl_counties_d1 <- dplyr::inner_join(fl_counties, d1_2015, by ="subregion")
  if(pro){
    m1 <- fl_base + 
      geom_polygon(data = fl_counties_d1,aes(fill=professionals),color = "white")+
      geom_polygon(color = "black", fill = NA) +
      theme_bw()
  }
  if(pro==F){
    m1 <- fl_base + 
      geom_polygon(data = fl_counties_d1,aes(fill=community),color = "white")+
      geom_polygon(color = "black", fill = NA) +
      scale_fill_gradient(trans = "log10") +
      theme_bw()  
  }
  m1
}

create_map(2016,T)
create_map(2017,F)
# ---- define-utility-functions ---------------


# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------
rmarkdown::render(
  # input = "./analysis/gls-activity/gls-activity-1.Rmd"
  input = "./analysis/gls-maps/gls-maps-1.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



