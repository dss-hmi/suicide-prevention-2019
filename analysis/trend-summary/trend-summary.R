# Lines before the first chunk are invisible to Rmd/Rnw callers
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
# ---- load-data ---------------------------------------------------------------
dto      <- readRDS(path_file_input)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()
ls <- dto$granularity_population

# ---- tweak-data ---------------------------------------------------------------
# to collapse into a single data frame
ds <- dto[["granularity_population"]] %>%
  Reduce(function(a , b) dplyr::left_join( a, b ), . )
# note that this data frame SHOULD NOT be used for any further aggregation
# GLS records are reported on county-year frame level, whereas
# Population estimates and death counts on county-year-DEMOGRAPHIC level
# This is a good general purpose ds, expecting no futher aggregation

ds %>% explore::describe_all()

d1 <- ds %>%
  dplyr::filter(county == "Lake") %>%
  dplyr::filter(year == 2015)

# to help us filter out those counties that had programming
counties_gls <- ds %>%
  na.omit(region) %>%
  dplyr::distinct(county) %>%
  as.list() %>% unlist() %>% as.character()
# to view the total programming delivered (between 2015 and 2017)
ds %>%
  dplyr::filter(county %in% counties_gls) %>%
  dplyr::distinct(county, year, community, professionals ) %>%
  # na.omit() %>%
  dplyr::group_by(county) %>%
  dplyr::summarize(
    community      = sum(community,na.rm =T)
    ,professionals = sum(professionals,na.rm =T)
  ) %>%
  dplyr::arrange(desc(professionals))
# to aid interpretation and graphing
ds <- ds %>%
  dplyr::rename(
    "deaths_by_suicide" = "resident_deaths" # to remind what we count
  ) %>%
  dplyr::mutate(
    # dummy indicator for treatment condition
    # gls_programming = ifelse(county %in% counties_gls, TRUE, FALSE)
    # to have a single variable describing racial background
    racethnicity = paste0(race," + ", ethnicity)
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
                # gls_programming, # was any programming administered?
                region, rgn, # support for graphing and grouping
                population_count, deaths_by_suicide,#measures
                community, professionals # treatment
  )

ds %>% explore::describe_all()

counties_gls <- ls[["gls"]] %>%
  dplyr::ungroup() %>%
  na.omit(region) %>%
  dplyr::distinct(county) %>%
  # na.omit() %>% 
  as.list() %>% unlist() %>% as.character() 

counties_gls %>% print()


# ----- custom-functions --------------------------------------

# to automate aggregation:
compute_aggregate <- function(
  l
  ,age_group_i = c("10_14","15_19","20_24") # will combine these, exclude others
  ,group_by_variables  =  c("county","year") # will group by these, sum over others
){
  # l <- ls #dto$granularity_population
  # l <- ls #dto$granularity_population
  # group_by_variables <- c("county","year")
  # group_by_variables <- c("year")
  d1 <- list(
    "population" = l[["population"]]
    ,"suicide"   = l[["suicide"]]
  ) %>% 
    Reduce(function(a , b) dplyr::left_join( a, b ), . ) 
  # compute the aggregation at the chosen level
  # must separate the treatment measure because cannot summarize
  d2 <- d1 %>% 
    dplyr::group_by(.dots = c(group_by_variables,"peer_group") ) %>%
    dplyr::summarize(
      population_count    = sum(population_count,   na.rm = T)
      ,deaths_by_suicide  = sum(resident_deaths,  na.rm = T)
    ) %>%
    dplyr::ungroup()
  # now compute the metrics
  counties_gls <- l[["gls"]] %>%
    dplyr::ungroup() %>%
    na.omit(region) %>%
    dplyr::distinct(county) %>%
    # na.omit() %>% 
    as.list() %>% unlist() %>% as.character() 
  
  d3 <- d2 %>% 
    # dplyr::mutate(
    # ) %>% 
    dplyr::left_join(l[["gls"]]) %>% 
    dplyr::mutate(
      suicide_rate_per100k         = (deaths_by_suicide / population_count) * 100000
      ,community_reach_per100k     = (community / population_count) * 100000
      ,professionals_reach_per100k = (professionals/ population_count) * 100000
      # ,gls_programming             = ifelse( county %in% counties_gls,TRUE,FALSE)
      ,county_gls             = ifelse( county %in% counties_gls,TRUE,FALSE)
    ) #%>% 
  # dplyr::group_by(county) %>% 
  # dplyr::mutate(
  #   county_gls = ifelse(is.na(county_gls),FALSE,TRUE)
  # )
  # d <- d3 %>% 
  #   dplyr::distinct(county_gls, professionals)
  return(d3)
  
}
# d1 <- ds %>%
#   dplyr::filter(county %in% c("Lake")) %>%
#   dplyr::filter(year %in% 2015) %>%
#   compute_aggregate(
#     age_group_i         = c("10_14","15_19","20_24") # willcombine these, exclude others
#     ,group_by_variables  =  c("county","year") # will aggregate over these, exclude others
#     #   ,group_by_variables  =  c("county","year","sex")
#     #   ,group_by_variables  =  c("county","year","sex","racethnicity")
#   )



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

peer_groups <- ls$population %>% dplyr::distinct(peer_group) %>% 
  dplyr::filter(peer_group %in% c(1,6,7,9,11,24,25)) %>% # only these have treatment
  dplyr::arrange(peer_group) %>% 
  as.list() %>% unlist() %>% as.vector()
peer_group_i <- 1

ds_counties_peer <- dplyr::left_join(
  ds_counties_peer,
  dto$granularity_population$gls %>% 
    dplyr::distinct(county, county_gls) %>% 
    na.omit()
)


for(peer_group_i in peer_groups){
  cat("\n")
  cat("\n## Peer Group ", peer_group_i,"\n")
  
  ls$population %>% 
    left_join(ls$gls %>% dplyr::distinct(county,county_gls)) %>% 
    dplyr::filter(peer_group == peer_group_i) %>% 
    distinct(county,county_gls,peer_group) %>%
    dplyr::arrange(peer_group) %>%
    # na.omit(peer_group) %>% 
    neat() %>%
    print(n = nrow(.)) 
  cat("\n")
}


# ds_counties_gls <- dto$granularity_population$gls %>% dplyr::ungroup()
# ds_counties_gls %>% dplyr::distinct(county) %>% print(n = nrow(.))
# # augment the gls with peer groups
# ds_counties_gls <- ds_counties_gls %>% 
#   dplyr::left_join(ds_counties_peer %>% dplyr::distinct(county,peer_group))
# 
# ds_counties_gls %>% 
#   dplyr::distinct(county, peer_group) %>% 
#   dplyr::arrange(peer_group) %>% 
#   print(n = nrow(.))

# ---- a1 -------------------------------
d1 <- ls %>% 
  compute_aggregate(
    age_group_i         = c("10_14","15_19","20_24") 
    ,group_by_variables  =  c("county","year")
  ) 

measure_levels <- c(
  "deaths_by_suicide"
  ,"suicide_rate_per100k"
  ,"community_reach_per100k"
  ,"professionals_reach_per100k"
)
measure_labels <- c(
  "Death by Suicide"
  ,"Suicide rate (per 100k)"
  ,"Community reach (per 100k)"
  ,"Professional reach (per 100k)"
)
g1 <- d1 %>% 
  # dplyr::filter(peer_group %in% c(1,6,7,9,11,24,25)) %>% # only these have treatment
  dplyr::filter(peer_group %in% c(1)) %>% # only these have treatment
  dplyr::filter(year %in% 2010:2019) %>% # treatment starts in2015
  dplyr::mutate(
    years_since_2000 = as.integer(as.integer(year) - 2000)
  ) %>% 
  tidyr::gather(measure,value,measure_levels) %>% 
  dplyr::mutate(
    measure = factor(measure, levels = measure_levels, labels = measure_labels)
  ) %>% 
  ggplot(aes(
    y = value
    ,x = years_since_2000
    ,group = county
    ,color = county_gls
  )) + 
  geom_line(
    aes(color = county_gls, group = county)
    ,alpha = .4
  )+
  geom_point(shape=21)+
  geom_smooth(aes(group=peer_group), se=F, data =. %>% dplyr::filter(county_gls == TRUE))+
  geom_smooth(aes(group=peer_group), se=F, data =. %>% dplyr::filter(county_gls == FALSE))+
  
  facet_grid(measure ~ peer_group, scale = "free")+
  scale_color_manual(values = c("TRUE" = "salmon", "FALSE" = "black"))+
  theme_minimal()+
  labs(color = "Counties with \n GLS programming")
g1



# ----- new -----------------------
d1 <- ds %>% 
  dplyr::mutate(
    tx = ifelse(county %in% counties_gls, TRUE, FALSE)
  ) %>% 
  dplyr::group_by(year, tx) %>% 
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>% 
  dplyr::ungroup()

g1 <- d1 %>% 
  dplyr::filter(year %in% 2006:2017) %>% 
  ggplot(aes(x = year, y = suicide_rate_per100k))+
  geom_line(aes(color = tx, group = tx))+
  geom_point(shape=21)+
  theme_minimal()+
  labs(color = "Counties with \n GLS programming")
g1


d2 <- ds %>% 
  dplyr::mutate(
    # tx_level = tx_level
    tx_level = car::recode(
      county,
      "
      'Orange'  ='high P high C'
      ;'Saint Lucie'='high P mid C'
      ;'Palm Beach'='mid P mid C  '
      ;'Brevard'='mid P mid C  '
      ;'Seminole'='mid P mid C  '
      ;'Volusia'='mid P high C  '
      ;'Lake'='mid P low C  '
      "
    )
    ,tx_level = ifelse(tx_level %in% counties_gls, "low P low C", tx_level)
    ,tx = ifelse(county %in% counties_gls, TRUE, FALSE)
    ,tx_level = ifelse(
      (!tx_level %in% counties_gls) & (tx == FALSE),"control", tx_level
    )
    ) %>% 
  dplyr::group_by(year,tx, tx_level) %>% 
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>% 
  dplyr::ungroup()
g2 <- d2 %>% 
  dplyr::filter(year %in% 2006:2017) %>% 
  ggplot(aes(x = year, y = suicide_rate_per100k))+
  geom_line(aes(color = tx_level, group = tx_level))+
  geom_point(shape=21)+
  theme_minimal()+
  labs(color = "Counties with \n GLS programming")
g2


focal_counties <- c(
  "Orange"  
  ,"Saint Lucie" 
  ,"Palm Beach"  
  ,"Brevard" 
  ,"Seminole"
  ,"Volusia"
  ,"Lake"        
)
d3 <- ds %>% 
  dplyr::mutate(
    tx_level = ifelse(county %in% focal_counties, county,
                      ifelse((!county %in% focal_counties) & (county %in% counties_gls), "low GLS",
                             "control"))
    
  ) %>% 
  dplyr::group_by(year,tx_level) %>% 
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>% 
  dplyr::ungroup()
g3 <- d3 %>% 
  dplyr::filter(year %in% 2013:2017) %>% 
  ggplot(aes(x = year, y = suicide_rate_per100k))+
  # ggplot(aes(x = year, y = deaths_by_suicide))+
  geom_line(aes(color = tx_level, group = tx_level), size =1.5)+
  geom_point(shape=21, size = 2)+
  theme_minimal()+
  labs(color = "Counties with \n GLS programming")
g3

focal_counties <- c(
  "Broward"
  ,"Brevard"
  ,"Hillsborough" 
  ,"Palm Beach"  
  ,"Miami-Dade"
  ,"Orange"
  ,"Seminole"        
  # ,"Brevard"
  ,"Volusia"
)
d4 <- ds %>% 
  dplyr::filter(county %in% focal_counties) %>% 
  dplyr::group_by(county, year) %>% 
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>% 
  dplyr::ungroup()
g4 <- d4 %>% 
  dplyr::filter(year %in% 2006:2017) %>% 
  ggplot(aes(x = year, y = suicide_rate_per100k))+
  # ggplot(aes(x = year, y = deaths_by_suicide))+
  geom_line(aes(color = county, group = county), size =1.5)+
  geom_point(shape=21, size = 2)+
  theme_minimal()+
  labs(color = "Focal Counties")
g4


d5 <- ds %>% 
  dplyr::filter(county %in% c("Palm Beach")) %>% 
  dplyr::group_by(county, year, racethnicity) %>% 
  dplyr::summarize(
    population_count      = sum(population_count,   na.rm = T)
    ,deaths_by_suicide    = sum(deaths_by_suicide,  na.rm = T)
    ,suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
  ) %>% 
  dplyr::ungroup()
g5 <- d5 %>% 
  dplyr::filter(year %in% 2006:2017) %>% 
  ggplot(aes(x = year, y = suicide_rate_per100k))+
  # ggplot(aes(x = year, y = deaths_by_suicide))+
  geom_line(aes(color = racethnicity, group = racethnicity), size =1.5)+
  geom_point(shape=21, size = 2)+
  theme_minimal()+
  labs(color = "Race Groups")
g5


# create a ration of population size to n_trained


# ---- g0 -------------------------------
#
d1 <- ds %>% 
  # dplyr::filter(county %in% c("Orange")) %>% 
  dplyr::filter(county %in% c("Lake")) %>%
  dplyr::filter(age_group %in% c("10_14","15_19","20_24")) %>%
  dplyr::filter(year %in% c(2011:2017)) %>% 
  # dplyr::filter(age_group %in% c("20_24")) %>%
  # dplyr::filter(age_group %in% c("15_19")) %>%
  # dplyr::filter(age_group %in% c("10_14")) %>% 
  dplyr::group_by(county, year, sex, race, ethnicity, racethnicity) %>%
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
    # ,sex = factor(sex, levels = c("Male","Female") )
    ,sex = factor(sex, levels = c("Female","Male") )
  ) 
d1 %>% explore::describe()

g1 <- d1 %>% 
  # dplyr::filter(year == 2016) %>% 
  dplyr::mutate(
    sexracethnicity = paste0(sex,"+",racethnicity)
  ) %>%
  ggplot(aes(
    x     = year
    ,y    = suicide_rate_per100k
    ,fill = racethnicity
    ,color = racethnicity
  ))+
  geom_point(aes(shape = sex), size = 4, color = "black")+
  geom_line(aes(group = sexracethnicity,color = racethnicity), alpha = 1, size =1.4)+
  # geom_point(aes(shape = sex,color = racethnicity), size = 4)+
  # geom_point(aes(shape = sex), size = 4, color = "black")+
  # geom_line(aes(group = sex*racethnicity))+
  scale_shape_manual(values = c("Male" = 25, "Female" = 24 ))+
  # scale_fill_viridis_d()+
  # scale_color_viridis_d()+
  theme_minimal()+
  guides(fill = "legend", color = "legend")
g1

# ---- g1-define -------------------------------
trend_summary <- function(
  d
  ,county_i
){
  
  d1 <- d %>% 
    # dplyr::filter(county %in% c("Orange")) %>% 
    dplyr::filter(county %in% c(county_i)) %>%
    dplyr::filter(age_group %in% c("10_14","15_19","20_24")) %>%
    dplyr::filter(year %in% c(2006:2017)) %>% 
    # dplyr::filter(age_group %in% c("20_24")) %>%
    # dplyr::filter(age_group %in% c("15_19")) %>%
    # dplyr::filter(age_group %in% c("10_14")) %>% 
    dplyr::group_by(county, year, sex, race, ethnicity, racethnicity) %>%
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
      # ,sex = factor(sex, levels = c("Male","Female") )
      ,sex = factor(sex, levels = c("Female","Male") )
    ) 
  d1 %>% explore::describe()
  
  g_out <- d1 %>% 
    # dplyr::filter(year == 2016) %>% 
    dplyr::mutate(
      sexracethnicity = paste0(sex,"+",racethnicity)
    ) %>%
    ggplot(aes(
      x     = year
      # ,y    = suicide_rate_per100k
      ,y    = deaths_by_suicide
      ,fill = racethnicity
      ,color = racethnicity
    ))+
    # geom_point(aes(shape = sex), size = 4, color = "black")+
    geom_line(aes(group = sexracethnicity,color = racethnicity), alpha = 1, size =1.4)+
    # geom_point(aes(shape = sex,color = racethnicity), size = 4)+
    # geom_point(aes(shape = sex), size = 4, color = "black")+
    # geom_line(aes(group = sex*racethnicity))+
    # scale_shape_manual(values = c("Male" = 25, "Female" = 24 ))+
    # scale_fill_viridis_d()+
    # scale_color_viridis_d(end = .8, option = "plasma")+
    # scale_color_viridis_d(end = .8, option = "inferno")+
    # scale_color_viridis_d(end = .8, option = "magma")+
    geom_point(shape = 21, color = "black", size = 2)+
    theme_minimal()+
    facet_grid(sex ~ .)+
    scale_y_continuous(breaks = seq(0,20,2),limits = c(0,15))+
    guides(fill = "legend", color = "legend")+
    labs(title = paste0(toupper(county_i),": Suicide rates per 100k among 10-24 yo"))
  return(g_out)
}
# how to use
# ds %>% trend_summary("Broward")
# ds %>% trend_summary("Orange")
# ---- g1 -------------------------------
# ds %>% trend_summary("Saint Lucie")
# ds %>% trend_summary("Volusia")
# ds %>% trend_summary("Seminole")
# ds %>% trend_summary("Lake")
# ds %>% trend_summary("Palm Beach")
# ds %>% trend_summary("Brevard")
# ds %>% trend_summary("Martin")
# ds %>% trend_summary("Flagler")
# 
# ds %>% trend_summary("Hillsborough")
# ds %>% trend_summary("Broward")
# ds %>% trend_summary("Duval")
# ds %>% trend_summary("Miami-Dade")
for(county_i in counties_gls){
  cat("\n")
  cat("\n## ",county_i,"\n")
  ds %>% trend_summary(county_i) %>% print()
  cat("\n")
}

# ---- g2 -------------------------------

# ---- g3 -------------------------------


# ---- x1 -------------------------------


# ---- publish ---------------------------------
rmarkdown::render(
  # input = "./analysis/gls-activity/gls-activity-1.Rmd"
  input = "./analysis/trend-summary/trend-summary.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)