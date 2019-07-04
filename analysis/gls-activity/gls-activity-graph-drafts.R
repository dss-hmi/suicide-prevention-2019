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
baseSize <- 10
# ---- load-data ---------------------------------------------------------------
dto      <- readRDS(path_file_input)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()

# assign aliases for this report
ds <- dto
# ds %>% head()
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


# ---- graph-2 ---------------------------------


ds_plot_2 <- ds %>% 
  # dplyr::filter(audience=="professionals") %>% 
  dplyr::filter(audience=="community") %>% 
  dplyr::mutate(
    year     = lubridate::year(date)
    ,month   = lubridate::month(date)
    ,weekday = lubridate::wday(date)
    ,rgn     = car::recode(
      region,
      "
      'central'='CN'
      ;'southeast'='SE'
      ;'northeast'='NE  '
      "
    )
    ,month = ifelse( month<=9, paste0("0",month), month)
    ,year_month = paste0(substr(year,3,4),"-",month)
    ,county  = paste0(county,"-",rgn)
    ) %>% 
  dplyr::group_by(county, year, month) %>% 
  dplyr::mutate(
    n_trained_sum = sum(n_trained)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(county, year, month) %>% 
  dplyr::mutate(
    value_ntile_interval = Hmisc::cut2(n_trained_sum,g = 5 )
    ,value_ntile_interval = factor(value_ntile_interval, levels = rev(levels(value_ntile_interval)))
  )
ds_plot_2 %>% glimpse()

cr_levels <- ds_plot_2 %>% 
  dplyr::arrange(region, county) %>% 
  dplyr::distinct(county ) %>% 
  as.list() %>% unlist() %>% as.character()

ds_plot_2 <- ds_plot_2 %>% 
  dplyr::mutate(
    county  = factor(county, levels = cr_levels)
    ,county = factor(county, levels = rev(levels(county)))
  )

g2 <- ds_plot_2 %>% 
  # ggplot2::ggplot(aes(x = date, y = county))+
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
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g2


# ---- graph-3 ---------------------------------
# grid of regions, X= date, Y=n_trained, professional audience
library(scales)

# not sure what to do with the graph, come back later

ds_plot_3 <- ds %>% 
  dplyr::filter(audience=="professionals") %>% 
  # dplyr::filter(county %in% c("Volusia","Lake","Palm Beach") ) %>%
  # dplyr::filter(!county == "Orange") %>%
  # dplyr::filter(county == "St Lucie") %>%
  dplyr::arrange(date) %>% 
  dplyr::group_by(county) %>% 
  dplyr::mutate(
    cum_n_trained = cumsum(n_trained)
  ) %>% 
  dplyr::ungroup()

g3 <- ds_plot_3 %>% 
  # ggplot2::ggplot(aes(x = date, y = n_trained, fill = county))+
  # ggplot2::ggplot(aes(x = date, y = cum_n_trained, fill = county))+
  ggplot2::ggplot(aes(x = date, y = cum_n_trained, color = region))+
  # geom_bar(stat="identity") +
  # geom_area(alpha = .5, position = "stack")+
    # geom_area(aes(group = county), alpha = .5)+
  geom_line(aes(group = county), size = 2.5, alpha = .5)+
  geom_line(aes(group = county), color = "black", size = .2, alpha = .5)+
  facet_grid(region ~ . , scales = "free") +
  # facet_wrap("county") +
  scale_x_date(
    labels  = scales::date_format("%m-%Y")
    ,breaks = scales::date_breaks("6 month")
  )+
  theme_minimal()
g3

# ---- graph-4 -----------------------------------------------------------
# graph showing professional cumulative traning provided over time in all counties
ds_plot_4 <- ds %>% dplyr::filter(audience=="professionals") %>% 
  dplyr::arrange(date) %>% 
  dplyr::group_by(county) %>% 
  dplyr::mutate(
    n_trained_pro = cumsum(x = n_trained)
  )
g4 <- ggplot(ds_plot_4, aes(x=date,y=n_trained_pro))+
  geom_bar(stat="identity")+
  facet_wrap("county", scales="free")+
  theme_minimal()
g4

# ---- graph-5 -----------------------------------------------------------
# graph showing all traning provided over time in all counties
ds_plot_5 <- ds %>%  
  dplyr::arrange(date) %>% 
  dplyr::group_by(audience,county) %>% 
  dplyr::mutate(
    n_trained_cum = cumsum(x = n_trained)
  )
g5 <- ggplot(ds_plot_5, aes(x=date,y=n_trained_cum))+
  geom_point(aes(group = audience, color = audience))+
  geom_area(aes(group=audience, fill = audience), alpha = .4)+
  # facet_wrap("county", scales="free")+
  facet_wrap("county")+
  theme_minimal()
g5

# ---- graph-6 -----------------------------------------------------------
# animation graph showing all traning provided over time in all counties
ds_plot_6 <- ds %>%  
  dplyr::mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>% 
  dplyr::group_by(audience,year,county) %>% 
  dplyr::mutate(
    n_trained_year = cumsum(n_trained)
  ) %>% 
  dplyr::summarise(
    n_trained_overall = sum(n_trained_year)
  )

ds_plot_6_pro <- ds_plot_6 %>% 
  dplyr::filter(audience=="professionals")

ds_plot_6_com <- ds_plot_6 %>% 
  dplyr::filter(audience=="community")  

ds_plot_7 <- merge(ds_plot_6_pro, ds_plot_6_com, by=c("year","county"))
library(gganimate)
g7 <- ggplot(ds_plot_7, aes(x=n_trained_overall.x,y=n_trained_overall.y))+
  geom_point(aes(color=county, group=county, size=6))+
  scale_x_log10()+
  scale_y_log10()+
  labs(title= "Year: {closest_state}", x= "Professionals Trained", y="Community Trained")+
  transition_states(year,state_length = 2)+ 
  exit_disappear()+
  theme_minimal()
g7

# ----- g8 ---------------------------
d8_1 <- ds %>% 
  dplyr::mutate(
    year = lubridate::year(date)
    ,month = lubridate::month(date)
  ) %>% 
  dplyr::group_by(audience, county, year, month) %>%
  dplyr::summarize(
    n_trained     = sum(n_trained, na.rm = T)
  ) %>% 
  dplyr::ungroup() 
d8_1 %>% head()

# to create all possible values for county - year - month 
names_counties <- ds %>% 
  dplyr::distinct(county) %>% 
  dplyr::arrange(county) %>% 
  na.omit() %>% 
  as.list() %>% unlist() %>% as.character()
names_years <- 2014:2019
names_months <- 1:12
# to unite into a single data frame
d_stem <- tidyr::crossing(
  county = names_counties
  ,year = names_years
  ,month = names_months
)
# attach to the stem
d8_2 <- dplyr::left_join(
  d_stem # first
  ,d8_1 # second
) %>% 
  tidyr::spread(key = audience, value = n_trained) %>% 
  dplyr::select(-`<NA>`) %>% 
  dplyr::mutate(
     community     = ifelse(is.na(community),0,community)
    ,professionals = ifelse(is.na(professionals),0,professionals)
  ) %>% 
  # dplyr::arrange(county, year, month) %>% 
  # dplyr::ungroup() %>% 
  dplyr::group_by(county, year, month) %>% 
  dplyr::summarize(
    community = sum(community, na.rm = T)
    ,professionals = sum(professionals, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(county) %>% 
  dplyr::mutate(
    yearmonth = paste0(year,"-", ifelse(month<10, paste0("0",month),month))
    ,cumsum_community     = cumsum(community)
    ,cumsum_professionals = cumsum(professionals)
  )
  
d8_2 %>% explore::describe_all()
d8_2 %>% head()

g8 <- d8_2 %>% 
  dplyr::filter(!county %in% c("Orange")) %>% 
  ggplot(aes(
    x=cumsum_professionals
    , y=cumsum_community
  ))+
  # geom_point(aes(group=county), shape = 21, fill = NA, size = 6)+
  geom_text(aes(label = county))+
  # scale_x_log10()+
  # scale_y_log10()+
  # labs(title= "Year: {closest_state}", x= "Professionals Trained", y="Community Trained")+
  # scale_x_continuous(breaks = seq(0, 200, 200))+
  # scale_y_continuous(breaks = seq(0, 3000, 1000))+
  labs(
    title= "Year: {closest_state}"
    , x  = "Professionals Trained"
    , y  = "Community Trained"
  )
g8 

g8 <- g8 +
  gganimate::transition_states(yearmonth, state_length = 200)+ 
  gganimate::shadow_mark(alpha = 0.3, size = 0.5)+
  # gganimate::exit_disappear()+
  gganimate::view_follow()+
  theme_minimal()
g8





# ---- 
# ds_plot_6 <- ds %>% 
#   dplyr::mutate(
#     year = lubridate::year(date),
#     month = lubridate::month(date)
#   ) %>% 
#   dplyr::filter(county == "Brevard") %>%
#   dplyr::filter(year %in% c(2015,2016) ) %>%
#   tidyr::spread(key = audience, value = n_trained ) %>% 
#   dplyr::group_by(county, year, month) %>%
#   dplyr::summarize(
#     professionals = sum(professionals, na.rm = T)
#     ,community    = sum(community, na.rm = T)
#   ) %>% 
#   dplyr::ungroup() %>% 
#   dplyr::group_by(county) %>% 
#   dplyr::mutate(
#     cumsum_professionals = cumsum(professionals)
#     ,cumsum_community    = cumsum(community)
#   ) %>% 
#   dplyr::arrange(
#     county, year, month
#   ) 


  
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



