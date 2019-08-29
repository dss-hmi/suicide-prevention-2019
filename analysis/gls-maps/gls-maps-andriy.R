# Lines before the first chunk are invisible to Rmd/Rnw callers
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below. 
base::source("./scripts/common-functions.R")

# ---- load-packages -----------------------------------------------------------
#install.packages(c("cowplot", "googleway", "ggrepel", 
#                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
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
quick_save <- function(g,name,...){
  ggplot2::ggsave(
    filename = paste0(name,".png"), 
    plot     = g,
    device   = png,
    path     = "./analysis/gls-maps/prints/1/", # female marital educ poor_healt
    # width    = width,
    # height   = height,
    # units = "cm",
    # dpi      = 50,
    limitsize = FALSE,
    ...
  )
}
# ---- tweak-data ---------------------------------------------------------------
# to collapse into a single data frame
ds <- dto[["granularity_population"]] %>% 
  Reduce(function(a , b) dplyr::left_join( a, b ), . )

# ds %>% explore::describe_all()

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

# ds %>% explore::describe_all()

# to remind out how to aggregate 
ds1 <- ds %>% 
  # dplyr::filter(county == "Lake") %>% 
  # dplyr::filter(year == "2015") %>% 
  dplyr::distinct(county,region, rgn, year, community, professionals) %>% 
  dplyr::group_by(county, region, rgn) %>% 
  dplyr::summarize(
     professionals     = sum(professionals,      na.rm = T)
    ,community         = sum(community,          na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::distinct(county,region, rgn, community, professionals) %>% 
  na.omit()

ds1 %>% explore::describe_all()

# ---- define-map-1 ----------------------------------
# get florida county coordinates
usa_counties <- ggplot2::map_data("county")
fl_counties  <- subset(usa_counties, region == "florida")
head(fl_counties)
#fix saint to st to match with our database
fl_counties <- fl_counties %>% 
  dplyr::mutate(
    subregion = ifelse(subregion == "st johns", "saint johns", subregion),
    subregion = ifelse(subregion == "st lucie", "saint lucie", subregion)
  )
#plot florida
fl_base <- ggplot(data = fl_counties, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(data = fl_counties, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_nothing()
fl_base
# create table for year=x and training_type = y
# year_var = 2015L
pro = T

create_map_1 <- function(
  d
  ,measure_name 
  ,ntile_groups = 5
){
  #function body
  # ntile_groups <- 5
  # measure_name <- "professionals"
  # d <- ds1 
  d1 <- d %>% 
    # dplyr::filter(year == year_var) %>% 
    dplyr::mutate(subregion = paste0(tolower(county))) %>% 
    dplyr::inner_join(fl_counties, d1, by ="subregion") %>% 
    dplyr::rename(
      "measure" = measure_name
    ) %>% 
    dplyr::mutate(
      ntile_bin = Hmisc::cut2(measure, g = ntile_groups)
      ,ntile_bin = factor(ntile_bin, levels = rev(levels(ntile_bin)))
    )

  # d1 %>% dplyr::glimpse()
  g1 <- fl_base + 
    geom_polygon(data = d1, aes_string(fill="ntile_bin"),color = "black")+
    # geom_text(data = d1 %>% distinct(county), aes(label = county))+
    # geom_polygon(color = "black", fill = NA) +
    # RColorBrewer::brewer.pal.info # to view options
    # my_palette <- RColorBrewer::brewer.pal(5, "YlOrRd")
    scale_fill_manual(values = rev(RColorBrewer::brewer.pal(ntile_groups, "YlOrRd")))+
    # scale_fill_manual(values = RColorBrewer::brewer.pal(ntile_groups, "Greens"))+
    # scale_fill_manual(values = RColorBrewer::brewer.pal(ntile_groups, "BuGn"))+
    # scale_fill_manual(values = RColorBrewer::brewer.pal(ntile_groups, "YlGn"))+
    # scale_fill_viridis_d(end = .9, option = "plasma")+
    # scale_fill_viridis_d(end = 1, option = "viridis")+
    theme_minimal()+
    
    # theme_bw()+
    labs(
      title = paste0("Amount of training delivered to ",toupper(measure_name)," during 2015-2018")
      ,fill = paste0("Number of persons trained \n by ntile groups")
      # ,y = "", x = ""
    )+
    theme(
      axis.title = element_blank()
      ,axis.text = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      )
  g1
  return(g1)
}
# how to use
# ds1 <- ds %>% 
#   dplyr::distinct(county,region, rgn, year, community, professionals) %>% 
#   dplyr::group_by(county, region, rgn) %>% 
#   dplyr::summarize(
#     professionals     = sum(professionals,      na.rm = T)
#     ,community         = sum(community,          na.rm = T)
#   ) %>% 
#   dplyr::ungroup() %>% 
#   na.omit() 
# 
# ds1 %>%   create_map_1("professionals")
# ds1 %>%   create_map_1("community")
# 
# ds2 <- ds %>% 
#   dplyr::distinct(county,region, rgn, year, community, professionals) %>% 
#   dplyr::group_by(county, region, rgn, year) %>% 
#   dplyr::summarize(
#     professionals     = sum(professionals,      na.rm = T)
#     ,community         = sum(community,          na.rm = T)
#   ) %>% 
#   dplyr::ungroup() %>%  
#   na.omit() 
# 
# ds2 %>% dplyr::filter(year == 2015) %>% create_map_1("professionals")

# ---- map-1-professionals ---------------------------------
ds1 %>% create_map_1("professionals") %>% 
  quick_save(name = "GLS-professionals-2015-2018",width = 1200, height = 900) 
  
# ---- map-1-community ---------------------------------
ds1 %>%   create_map_1("community") %>% 
  quick_save(name = "GLS-community-2015-2018",width = 1200, height = 900) 

# ---- define-map-2 ----------------------------------
# get florida county coordinates
usa_counties <- ggplot2::map_data("county")
fl_counties  <- subset(usa_counties, region == "florida")
head(fl_counties)
#fix saint to st to match with our database
fl_counties <- fl_counties %>% 
  dplyr::mutate(
    subregion = ifelse(subregion == "st johns", "saint johns", subregion),
    subregion = ifelse(subregion == "st lucie", "saint lucie", subregion)
  )
#plot florida
fl_base <- ggplot(data = fl_counties, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(data = fl_counties, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_nothing()
fl_base
# create table for year=x and training_type = y
# year_var = 2015L
pro = T

create_map_2 <- function(
  d
  ,measure_name 
  ,ntile_groups = 5
){
  #function body
  # ntile_groups <- 5
  # measure_name <- "professionals"
  # d <- ds1 
  d1 <- d %>% 
    # dplyr::filter(year == year_var) %>% 
    dplyr::mutate(subregion = paste0(tolower(county))) %>% 
    dplyr::inner_join(fl_counties, d1, by ="subregion") %>% 
    dplyr::rename(
      "measure" = measure_name
    ) %>% 
    dplyr::mutate(
      ntile_bin = Hmisc::cut2(measure, g = ntile_groups)
      ,ntile_bin = factor(ntile_bin, levels = rev(levels(ntile_bin)))
    )
  
  # d1 %>% dplyr::glimpse()
  g1 <- fl_base + 
    geom_polygon(data = d1, aes_string(fill="ntile_bin"),color = "black")+
    # geom_text(data = d1 %>% distinct(county), aes(label = county))+
    # geom_polygon(color = "black", fill = NA) +
    # RColorBrewer::brewer.pal.info # to view options
    # my_palette <- RColorBrewer::brewer.pal(5, "YlOrRd")
    scale_fill_manual(values = rev(RColorBrewer::brewer.pal(ntile_groups, "YlOrRd")))+
    # scale_fill_manual(values = RColorBrewer::brewer.pal(ntile_groups, "Greens"))+
    # scale_fill_manual(values = RColorBrewer::brewer.pal(ntile_groups, "BuGn"))+
    # scale_fill_manual(values = RColorBrewer::brewer.pal(ntile_groups, "YlGn"))+
    # scale_fill_viridis_d(end = .9, option = "plasma")+
    # scale_fill_viridis_d(end = 1, option = "viridis")+
    theme_minimal()+
    
    # theme_bw()+
    labs(
      title = paste0("Amount of training delivered to ",toupper(measure_name)," during 2015-2018")
      ,fill = paste0("Number of persons trained \n by ntile groups")
      # ,y = "", x = ""
    )+
    theme(
      axis.title = element_blank()
      ,axis.text = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
    )
  g1
  return(g1)
}
# how to use
# ds1 <- ds %>% 
#   dplyr::distinct(county,region, rgn, year, community, professionals) %>% 
#   dplyr::group_by(county, region, rgn) %>% 
#   dplyr::summarize(
#     professionals     = sum(professionals,      na.rm = T)
#     ,community         = sum(community,          na.rm = T)
#   ) %>% 
#   dplyr::ungroup() %>% 
#   na.omit() 
# 
# ds1 %>%   create_map_1("professionals")
# ds1 %>%   create_map_1("community")
# 
# ds2 <- ds %>% 
#   dplyr::distinct(county,region, rgn, year, community, professionals) %>% 
#   dplyr::group_by(county, region, rgn, year) %>% 
#   dplyr::summarize(
#     professionals     = sum(professionals,      na.rm = T)
#     ,community         = sum(community,          na.rm = T)
#   ) %>% 
#   dplyr::ungroup() %>%  
#   na.omit() 
# 
# ds2 %>% dplyr::filter(year == 2015) %>% create_map_1("professionals")


# ----- get-peer-counties ----------------------------------
ds_counties_peer <-  readxl::read_excel(
  "data-unshared/raw/peer-counties-tool/CHSIpeers.xlsx"
  ,sheet = "chsi_peer_2015"
)
names(ds_counties_peer)
names(ds_counties_peer) <- c("county_code", "county_name","peer_group")
ds_counties_peer <- ds_counties_peer %>%
  dplyr::mutate(
    state = gsub("^(.+),(.+)$","\\2",county_name)
    ,state = substring(state, 2) # remove leading space
    ,county = gsub("^(.+),(.+)$","\\1",county_name)
    ,county = gsub("County","",county)
    ,county = gsub(" $","",county)
  ) %>%
  # dplyr::filter(county == "Lake")
  dplyr::filter(state == "Florida") %>%
  dplyr::arrange(peer_group) %>%
  dplyr::mutate(
    county = ifelse(county %in% c("St. Johns"), "Saint Johns", county)
    ,county = ifelse(county %in% c("St. Lucie"), "Saint Lucie", county)
  )

ds3 <- ds_counties_peer %>% 
  # dplyr::filter(year == year_var) %>% 
  dplyr::mutate(subregion = paste0(tolower(county))) %>% 
  dplyr::inner_join(fl_counties, ds_counties_peer, by ="subregion")# %>% 
 # dplyr::mutate(
  #  ntile_bin = Hmisc::cut2(peer_group, g = 24)
  #) 
# ---- peer-counties-map -------------------------------------
ds3_distinct <- ds3 %>% 
  dplyr::distinct(subregion, .keep_all = TRUE) %>% 
  dplyr::filter(county %in% ds1$county)

library(ggrepel)

g2 <- fl_base + 
  geom_polygon(data = ds3, aes(fill=factor(peer_group),group=subregion),color = "black")+
  #geom_text(data = ds3_distinct, aes(label = subregion),angle = 60)+
  geom_label_repel(data = ds3_distinct, aes(x=long, y=lat, label = subregion), 
             size = 3, fontface = "bold")+
  # geom_polygon(color = "black", fill = NA) +
  # RColorBrewer::brewer.pal.info # to view options
  # my_palette <- RColorBrewer::brewer.pal(5, "YlOrRd")
  #scale_fill_manual(values = RColorBrewer::brewer.pal(24, "YlOrRd"))+
  # scale_fill_manual(values = RColorBrewer::brewer.pal(ntile_groups, "Greens"))+
  # scale_fill_manual(values = RColorBrewer::brewer.pal(ntile_groups, "BuGn"))+
  # scale_fill_manual(values = RColorBrewer::brewer.pal(ntile_groups, "YlGn"))+
  
  theme_minimal()+
  # theme_bw()+
  theme(
    axis.title = element_blank()
    ,axis.text = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )
g2


# ---- Match test -----------------------------
d1_2015 <- d1 %>% 
  dplyr::distinct(county) %>% 
  #dplyr::filter(year == year_var) %>% 
  dplyr::mutate(subregion = paste0(tolower(county)))
  
subset <- c(d1_2015$subregion)
fl_counties_filtered <- dplyr::filter(fl_counties,subregion == subset) %>% 
  dplyr::distinct(subregion)
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



