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
output_format = "pandoc"
baseSize <- 10
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
ds %>% distinct(region)
# ---- tweak-data ---------------------------------------------------------------

# create auxilary variables
ds <- ds %>% 
  # dplyr::select(-county_zipcode) %>% 
  dplyr::mutate(
    year = lubridate::year(date)
    ,month = lubridate::month(date)
    ,yearmonth = zoo::as.yearmon(date)
    ,weekday = lubridate::wday(date)
    ,rgn = car::recode(
      region,
      "
      'central'='CN'
     ;'southeast'='SE'
     ;'northeast'='NE  '
      "
    )
  )

ds %>% glimpse(60)

# ds %>% explore::explore( )
ds %>% explore::describe()

# ---- accumulation-0 -------------------------

gls_accumulation <- function(
  d
  ,audience_i
){
  d1 <- ds %>%  
    dplyr::filter(audience %in% audience_i) %>% 
    dplyr::arrange(date) %>% 
    dplyr::group_by(audience,county) %>% 
    dplyr::mutate(
      n_trained_cum = cumsum(x = n_trained)
    )
  g1 <- d1 %>% 
    ggplot(aes(x=date,y=n_trained_cum))+
    geom_point(aes(group = audience, color = audience))+
    geom_area(aes(group=audience, fill = audience), alpha = .4)+
    # facet_wrap("county", scales="free")+
    facet_wrap("county")+
    scale_fill_manual(values = c("professionals" = "#d95f02", "community" = "#1b9e77"))+
    scale_color_manual(values = c("professionals" = "#d95f02", "community" = "#1b9e77"))+
    theme_minimal()+
    labs(
      title = "Accumulation of GLS programming by counties"
      ,y = "Amount of delivered programming (cumulative)"
      ,x = "Date"
    )
  return(g1)
}
# how to use
# g <- ds %>% gls_accumulation(c("community","professionals"))

# ---- accumulation-1 -------------------------
ds %>% 
  gls_accumulation(c("community","professionals"))
# ---- accumulation-2 -------------------------
# g
ds %>% 
  gls_accumulation(c("community","professionals")) %+% 
  facet_wrap("county", scales = "free")

# ---- accumulation-3 -------------------------
ds %>% 
  gls_accumulation(c("community"))

# ---- accumulation-4 -------------------------
ds %>% 
  gls_accumulation(c("community")) %+% 
  facet_wrap("county", scales = "free")

# ---- accumulation-5 -------------------------
ds %>%
  gls_accumulation(c("professionals"))

# ---- accumulation-6 -------------------------
ds %>% 
  gls_accumulation(c("professionals")) %+%
  facet_wrap("county", scales = "free")





# ---- schedule-0 ---------------------
gls_county_by_month <- function(
  d
  ,audience_i
  ,ntile_groups = 5
){
  # d <- ds; audience_i = "community"; ntile_groups = 5
  d1 <- d %>% 
    dplyr::filter(audience==audience_i) %>% 
    na.omit() %>%
    dplyr::mutate(
      month       = ifelse( month<=9, paste0("0",month), month)
      ,year_month = paste0(substr(year,3,4),"\n",month)
      # ,year_month = paste0(month,"\n",substr(year,3,4))
      ,county     = paste0(county,"-",rgn)
      # ,yearmonth = as.character(yearmonth)
      # ,yearmonth = gsub(
      #   "^(\\w+) (\\d+)", paste0("\\1","\n",substr(year,3,4), yearmonth)
      # )
      ,month = gsub("^(\\w+) (\\d+)$","\\1", yearmonth)
      ,yearmonth = paste0(month,"\n",substr(year,3,4))
    ) %>%  
    dplyr::group_by(county, year, month) %>% 
    dplyr::mutate(
      n_trained_sum = sum(n_trained)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(county, year, month) %>% 
    dplyr::mutate(
      value_ntile_interval = Hmisc::cut2(n_trained_sum,g = ntile_groups )
      ,value_ntile_interval = factor(value_ntile_interval, levels = rev(levels(value_ntile_interval)))
    )
  # d1 %>% glimpse()
  county_levels <- d1 %>% 
    dplyr::arrange(region, county) %>% 
    dplyr::distinct(county ) %>% 
    as.list() %>% unlist() %>% as.character()
  
  d2 <- d1 %>% 
    dplyr::mutate(
      county  = factor(county, levels = county_levels)
      ,county = factor(county, levels = rev(levels(county)))
    )
  g1 <- d2 %>% 
    # ggplot2::ggplot(aes(x = date, y = county))+
    # ggplot2::ggplot(aes(x = yearmonth, y = county))+
    ggplot2::ggplot(aes(x = year_month, y = county))+
    # geom_tile(aes(fill = n_trained))+
    geom_raster(aes(fill = value_ntile_interval))+
    # geom_tile(aes(fill = value_ntile_interval))+
    # geom_text(aes(label = n_trained_sum))+
    # facet_grid(region ~ .)+
    # scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks("2 month") )+
    theme_minimal()+
    # scale_fill_viridis_d(end=.7)+
    scale_fill_viridis_d(end=.85, option = "plasma")+
    # scale_fill_viridis_d(end=.8, option = "magma")+
    # scale_fill_viridis_d(end=.9, option = "inferno")+
    # scale_fill_viridis_c()+
    theme(axis.text.x = element_text(angle = 0, hjust = 1))+
    labs(
      title = paste0("Number of trained: ", toupper(audience_i))
      ,x = "Month of administration (year / month)"
      ,y = "County-REGION"
      ,fill = paste0("percentile\ncategory\nwith ",ntile_groups, " groups")
    )
  return(g1)
}

# how to use
# ds %>% gls_county_by_month("professionals")

# ---- schedule-1 ---------------------
ds %>% gls_county_by_month("community")
# ---- schedule-2 ---------------------
ds %>% gls_county_by_month("professionals")



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
  dplyr::group_by(audience, region, county) %>% 
  dplyr::summarize(
    n_zipcodes             =  dplyr::n_distinct(zipcode)
    ,n_training_types      =  dplyr::n_distinct(type_training)
    ,total_persons_trained = sum(na.omit(n_trained))
    ) 

dt1 %>%  neat()

# ----- county-reports ---------------------------------

# to produce tables with (row = county) 

region_table <- function(
  d
  ,pick_region
){
  # d <- ds 
  # pick_region = "central"

cat("\n Trained PROFESSIONALS by training type (for all years)","\n")
  d <- d %>% 
    dplyr::filter(region %in% pick_region) 
  d1 <- d %>% 
    dplyr::filter(audience == "professionals") %>% 
    dplyr::group_by(county, type_training) %>% 
    dplyr::summarize(
      n_trained = sum(n_trained)
    ) %>% 
    dplyr::group_by(county) %>% 
    dplyr::mutate(
      total_trained = sum(n_trained)
    ) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(key = "type_training", value = "n_trained") %>% 
    dplyr::arrange(desc(total_trained))
  d1[is.na(d1)] <- "."
  d1  %>% neat() %>% print()

  cat("\n Trained PROFESSIONALS by year (for all training types)","\n") 
  d2 <- d %>% 
    dplyr::filter(audience == "professionals") %>% 
    dplyr::group_by(county, year) %>% 
    dplyr::summarize(
      n_trained = sum(n_trained)
    ) %>% 
    dplyr::group_by(county) %>% 
    dplyr::mutate(
      total_trained = sum(n_trained)
    ) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(key = "year", value = "n_trained") %>% 
    dplyr::arrange(desc(total_trained))
  d2[is.na(d2)] <- "."
  d2 %>% neat() %>% print()
  
  cat("\n Trained IN COMMUNITY by year (for all training types)","\n") 
  d3 <- d %>% 
    dplyr::filter(audience == "community") %>% 
    dplyr::group_by(county, year) %>% 
    dplyr::summarize(
      n_trained = sum(na.omit(n_trained))
    ) %>% 
    dplyr::group_by(county) %>% 
    dplyr::mutate(
      total_trained = sum(na.omit(n_trained))
    ) %>% 
    tidyr::spread("year","n_trained") %>% 
    dplyr::arrange(desc(total_trained)) %>% 
    tidyr::replace_na(list("."))
 d3[is.na(d3)] <- "."
 d3 %>% neat()  %>% print()
 cat("\n") 
  
  
}
regions_available <- ds %>% dplyr::distinct(region) %>% as.list() %>% unlist() %>% as.character()

cat("\n## all regions\n")
ds %>% 
  dplyr::mutate(
    county = paste0(county,"     (",region,")" )
  ) %>% 
  region_table(pick_region = regions_available)
cat("\n")
# ds %>% region_table("central")

# ----- county-reports-1 ---------------------------------
for(region_i in regions_available){
  cat("\n## ", region_i, "\n")
  ds %>% region_table(pick_region = region_i)
  cat("\n")
}

# ---- explore-1 ------------------
d1 <- ds %>% 
  dplyr::group_by(county, audience, region) %>% 
  # dplyr::group_by(county, year, audience) %>% 
  dplyr::summarize(
    n_trained = sum(na.omit(n_trained))
  ) %>% 
  tidyr::spread(key = "audience", value = "n_trained")

g1 <- d1 %>% 
  ggplot2::ggplot(
    aes(
      x  = community
      ,y = professionals
    )
  )+
  geom_text(aes(label= county, color = region), size = baseSize-4 )+
  scale_color_manual(values = c(
    "central"    ="#1b9e77"
    ,"northeast" ="#d95f02"
    , "southeast"="#7570b3"
  ))+
  theme_minimal()+
  labs(
    x = "Number of people in community exposed to GLS program"
    ,y = "Number of professionals receiving GLP training"
    )+
  theme(
    axis.text = element_text(size = baseSize + 2)
  )
# to zoom in 
g1 +
  geom_hline(aes(yintercept = 1400), linetype = "dashed")+
  geom_vline(aes(xintercept = 5000), linetype = "dashed")
# to zoom in 
g1 +
  scale_x_continuous(limits = c(0, 5000))+
  scale_y_continuous(limits = c(0, 1400))+
  geom_hline(aes(yintercept = 400), linetype = "dashed")+
  geom_vline(aes(xintercept = 600), linetype = "dashed")
# to zoom in 
g1 +
  scale_x_continuous(limits = c(0, 600))+
  scale_y_continuous(limits = c(0, 400))

# ---- explore-2-define-function -------------------------------

show_coverage <- function(
  d
  ,pick_audience = "both"
)
{
  # d <- ds
  d1 <- d %>% 
    dplyr::group_by(region,rgn, county, audience) %>% 
    dplyr::summarize(
      n_zipcodes             =  dplyr::n_distinct(zipcode)
      ,n_training_types      =  dplyr::n_distinct(type_training)
      ,total_persons_trained = sum(na.omit(n_trained))
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      county  = paste0(county,"-",rgn)
    ) 
  # dt2 %>%  neat()
 
   cr_levels <- d1 %>% 
    # dplyr::arrange(county, region) %>% 
    dplyr::arrange(region, county) %>% 
    # dplyr::ungroup() %>% 
    dplyr::distinct(county ) %>% 
    as.list() %>% unlist() %>% as.character()
  
   d2 <- d1 %>% 
     dplyr::mutate(
       # to control the order of counties on the graph
       county  = factor(county,levels = cr_levels)
       ,county = factor(county, levels = rev(levels(county)))
     ) %>% 
     tidyr::gather(
       "measure"
       ,"count"
       , c("n_zipcodes","n_training_types","total_persons_trained")
     ) %>% 
     dplyr::mutate(
       measure = factor(measure, levels = c(
         "n_zipcodes"
         ,"n_training_types"
         ,"total_persons_trained"),
         labels = c("Zipcodes", "Training Types", "Persons")
         )
       ,audience = factor(audience, levels = c(
         "community"
         ,"professionals"
         ,NA
       ))
     )
  # to make plot of 
  # BOTH audiences
  if(pick_audience == "both"){
    g1 <- d2 %>% 
      ggplot(aes(x=county, y = count, color = audience, fill = audience ))+
      scale_fill_manual( values = c("professionals" = "red", "community" = NA))+
      scale_color_manual(values = c("professionals" = NA,    "community" = "black"))+
      geom_bar(stat = "identity", position = "identity", alpha = .5)+
      coord_flip()+
      facet_grid(. ~ measure, scales = "free")+
      # facet_grid(region ~ measure, scales = "free")+
      theme_minimal()+
      labs(
        title = paste0("How many ... were engaged by GLS program?")
        ,x = "County in Florida" , y = "Units engaged"
        ,fill = "Audience", color = "Audience"
      )+
      theme(
        axis.text = element_text(size = baseSize + 2)
      )
      
    g_out <- g1
  }
  # SINGLE audience
  if(!pick_audience == "both"){
    g2 <- d2 %>% 
      dplyr::filter(audience == pick_audience) %>% 
      #
      ggplot(aes(x=county, y = count, fill = measure, pattern = region ))+
      # ggplot(aes(x=county, y = count ))+
      geom_bar(stat = "identity", alpha = .5)+
      geom_text( aes(label = count), vjust = 0.2)+
      coord_flip()+
      facet_grid(. ~ measure, scales = "free")+
      scale_fill_manual(values = c(
        "Zipcodes"        = "#a6cee3"
        ,"Training Types"  = "#1f78b4"
        ,"Persons"         = "#b2df8a"

      ))+
      theme_minimal()+
      theme(
        legend.position = "none"
      )+
      labs(
        title = paste0("How many ... were engaged by GLS program? Audience = (",pick_audience,") ?")
        ,x = "County in Florida" , y = "Units engaged", fill = "Audience"
      )+
      theme(
        axis.text = element_text(size = baseSize + 2)
      )
    
    g_out <- g2
  }
   return(g_out)
}
# how to use:
# ds %>% show_coverage()

# ---- explore-2 -------------------------------

ds %>% show_coverage()

# to remove the outlier
ds %>% 
  dplyr::filter( !county %in% c("Orange") ) %>% 
  show_coverage()

# ---- explore-3 -------------------------------
ds %>% show_coverage("professionals")
# ---- explore-4 -------------------------------
ds %>% show_coverage("community")

# ---- basic-graph -------------------------------------------------------


# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------
rmarkdown::render(
  # input = "./analysis/gls-activity/gls-activity-1.Rmd"
  input = "./analysis/gls-activity/gls-activity-2-coverage.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



