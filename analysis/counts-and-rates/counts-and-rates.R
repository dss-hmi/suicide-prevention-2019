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
path_file_input <- "./data-unshared/derived/9-combined.rds"
html_flip <- FALSE
baseSize <- 10


age_group_order <- c(
  "less_than_1"
  ,"1_4"        
  ,"5_9"        
  ,"10_14"      
  ,"15_19"      
  ,"20_24"      
  ,"25_34"      
  ,"35_44"      
  ,"45_54"      
  ,"55_64"      
  ,"65_74"      
  ,"75_84"      
  ,"85_plus"    
)

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
# the most granular level includes (6):
# county, year, sex, age_group, race, ethnicity
d1 <- ds %>% 
  # apply filters to better understand the structure of the data
  # dplyr::filter(county     %in% c("Orange") ) %>%
  # dplyr::filter(year       %in% c("2015")   ) %>%
  # dplyr::filter(sex        %in% c("Male")   ) %>%
  # dplyr::filter(age_group  %in% c("15_19")  ) %>%
  # dplyr::filter(race       %in% c("White")  ) %>%
  # dplyr::filter(ethnicity %in% c("Non-Hispanic") )%>%
  # dplyr::filter(ethnicity %in% c("Hispanic","Non-Hispanic") )%>%
  dplyr::group_by(county, year, sex, age_group, race, ethnicity) %>% # no aggregation
  # to exemplify useful aggregates:
  # dplyr::group_by(county, year                               ) %>%
  # dplyr::group_by(county, year, sex                          ) %>%
  # dplyr::group_by(county, year, sex, age_group               ) %>%
  # dplyr::group_by(county, year, sex, racethnicity                  ) %>%
  dplyr::summarize(
    population_count   = sum(population_count,   na.rm = T)
    ,deaths_by_suicide = sum(deaths_by_suicide,  na.rm = T)
    ,professionals     = sum(professionals,      na.rm = T)
    ,community         = sum(community,          na.rm = T)
  ) 
# use the code for preparing data for custom graphs
# d1 %>% glimpse(60)
# d1 %>% explore::describe()

# ---- population-00 -------------------------------
# let us see how populous each Florida county is
ntile_groups = 10
d1 <- ds %>% 
  dplyr::filter(year == 2015) %>%
  dplyr::filter(age_group %in% c("10_14","15_19","20_24")) %>% 
  # dplyr::filter(county == "Lake") %>%
  dplyr::group_by(county,age_group, racethnicity) %>% 
  dplyr::summarize(
    n_residents = sum(population_count, na.rm= T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    age_group = factor(age_group, levels = age_group_order)
   # ,value_ntile_interval = Hmisc::cut2(n_residents,g = ntile_groups )
   # ,value_ntile_interval = cut(n_residents, c(100, 1000, 10000, 50000, 10000) )
   ,value_ntile_interval = cut(
     n_residents 
     ,breaks = c(-Inf, 100, 1000, 2000, 5000, 10000, 50000, 100000 , Inf)
     ,labels = c(" < 100","1,000", "2,000","5,000", "10,000", "50,000","100,000", "100k +" ))
     # ,breaks = c(-Inf, 100, 3000, 6000, 12000, 24000, 48000, 96000 , Inf)
     # ,labels = c(" < 100","3,000", "6,000","12,000", "24,000", "48,000","96,000", "96k +" ))
     # ,breaks = c(-Inf, 100, 1000,2000,3000, 5000, 10000, 50000, 75000, 100000, Inf)
     # ,labels = c(" < 100","1,000", "2,000","3,000", "5,000", "10,000","50,000","75,000", "100,000", "> 100,000" ))
     # ,breaks = c(-Inf,1000,5000,10000, Inf) 
     # ,labels = c("< 1000", "1k - 5k", "5k - 10k", "10k +")
  )
# length(unique(d1$value_ntile_interval))

county_levels <- d1 %>% 
  # dplyr::arrange(desc(n_residents)) %>% 
  dplyr::arrange(n_residents) %>% 
  dplyr::distinct(county ) %>% 
  as.list() %>% unlist() %>% as.character()


g1 <- d1 %>% 
  dplyr::mutate(
    county = factor(county, levels = county_levels)
    ,value_ntile_interval = factor(value_ntile_interval,levels = rev(levels(value_ntile_interval)))
  ) %>% 
  ggplot(aes(x = age_group, y = county))+
  geom_raster(aes(fill = value_ntile_interval))+
  theme_minimal()+
  scale_fill_viridis_d(end=.85, option = "plasma")+
  facet_grid(.~ racethnicity)+
  theme(axis.text.x = element_text(angle = 0, hjust = .5))#+
  # labs(
  #   title = paste0("Number of trained: ", toupper(audience_i))
  #   ,x = "Month of administration (year / month)"
  #   ,y = "County-REGION"
  #   ,fill = paste0("percentile\ncategory\nwith ",ntile_groups, " groups")
  # )
g1
   
# ---- population-0 -------------------------------
display_pop_estimates <- function(
  d
  ,year_i      = 2006:2017
  ,age_group_i = c("10_14","15_19","20_24")
  ,measure     = "population_count" # population_count, deaths_by_suicide, suicide_rate_per100k
  ,grouping    = "sex" # race, ethnicity, racethnicity
){
  group_by_variables <- c("county","year",grouping)
  title_derived <- paste0(
    toupper(measure)," for persons aged (",paste0(age_group_i, collapse = ", "),") grouped by ",toupper(grouping))
  
  # to understand how suicide rate varies across time and counties for youth regardless of race
  d1 <- d %>% 
    dplyr::filter(year %in% year_i) %>% 
    dplyr::filter(age_group %in% age_group_i) %>%
    dplyr::mutate(
       sex             = factor(sex, levels = c("Female","Male") )
      ,race            = factor(race)
      ,ethnicity       = factor(ethnicity)
      ,racethnicity    = factor(racethnicity)
    ) %>% 
    dplyr::group_by(.dots = group_by_variables) %>%
    dplyr::summarize(
      population_count      = sum(population_count,   na.rm = T)
      ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
      ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # produces the same results as within group_by() summarization
      # suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
      years_since_2000 = as.integer(as.integer(year) - 2000) # for short label
    )
  # d1 %>% explore::describe()
  g1 <- d1 %>% 
    ggplot(aes_string(
      x     = "years_since_2000"
      ,y    = measure
      ,fill = grouping
    ))+
    geom_bar(stat = "identity", position = "stack", color = "black")+
    facet_wrap("county")+
    # scale_x_continuous(breaks = seq(from=min(year_i),to=max(year_i),by=2))+
    scale_x_continuous(breaks = seq(6,17,2))+
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks())+
    scale_fill_viridis_d(end = .9, option = "plasma")+
    theme_bw()+
    labs(
      title = title_derived
      ,x = "Years since 2000"
      ,y = toupper(measure)
    )
  return(g1)

}
# how to use
# ds %>% display_pop_estimates(
#    year_i      = 2006:2017
#   ,age_group_i = c("10_14","15_19","20_24")
#   ,measure     = "population_count" # population_count, deaths_by_suicide, suicide_rate_per100k
#   ,grouping    = "sex" # race, ethnicity, racethnicity
# )

age_bin <- list(
  "youth"   = c("10_14","15_19","20_24")
  ,"adults"  = c("25_34","35_44","45_54","55_64")
  ,"elderly" = c("65_74","75_84","85_plus")
)


# ---- population-1 -------------------------------
large <- c("Broward","Hillsborough","Miami-Dade","Orange", "Palm Beach")

g <- ds %>% 
  dplyr::filter(!county %in% large ) %>%
  display_pop_estimates(
    year_i      = 2006:2017
    ,age_group_i = c("10_14","15_19","20_24")
    # ,age_group_i = c("25_34","35_44","45_54","55_64")
    # ,age_group_i = c("65_74","75_84","85_plus")
    ,measure     = "population_count" # population_count, deaths_by_suicide, suicide_rate_per100k
    ,grouping    = "sex" # sex, race, ethnicity, racethnicity
  )
g
g <- ds %>% 
  dplyr::filter(county %in% large ) %>% 
  display_pop_estimates(
    year_i      = 2006:2017
    ,age_group_i = c("10_14","15_19","20_24")
    # ,age_group_i = c("25_34","35_44","45_54","55_64")
    # ,age_group_i = c("65_74","75_84","85_plus")
    ,measure     = "population_count" # population_count, deaths_by_suicide, suicide_rate_per100k
    ,grouping    = "racethnicity" # sex, race, ethnicity, racethnicity
  )
g %+% facet_wrap("county", scales = "free",ncol = 5)

county_size <- list(
  "large" = c("Broward","Hillsborough","Miami-Dade","Orange", "Palm Beach")
  ""
)

# ---- g1 -------------------------------
g1 %+% aes(y = population_count)+
  labs(
    title = "Population count for youth aged 10 - 24"
  )
# ---- g2 -------------------------------
g1 %+% aes(y = deaths_by_suicide)+
  labs(
    title = "Death by suicide among youth aged 10 - 24"
  )
# ---- g3 -------------------------------
g1 + 
  labs(
    title = "Suicide rates for youth aged 10 - 24"
  )

# ---- x1 -------------------------------

# it appears that some counties may not have sufficient population to use rates reliably
d2 <- ds %>% 
  dplyr::filter(year %in% c(2006:2017)) %>% 
  dplyr::filter(age_group %in% c("10_14","15_19","20_24")) %>%
  dplyr::group_by(county, year) %>% # excluded sex to get the total population counts
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    pop_less_3000 = ifelse(population_count < 3000, TRUE, FALSE)
  ) %>% 
  dplyr::arrange(desc(population_count))

d2 %>% neat_DT()

# g2 <- d2 %>% 
  

# ---- publish ---------------------------------
rmarkdown::render(
  # input = "./analysis/gls-activity/gls-activity-1.Rmd"
  input = "./analysis/counts-and-rates/counts-and-rates.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)