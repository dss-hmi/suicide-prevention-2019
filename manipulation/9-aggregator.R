rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr) # disable when temp lines are removed
library(ggplot2)

# ---- declare-globals ---------------------------------------------------------
path_file_input_0 <- "./data-unshared/derived/0-greeted-gls.rds"
path_file_input_1 <- "./data-unshared/derived/1-greeted-population.rds"
path_file_input_2 <- "./data-unshared/derived/2-greeted-suicide.rds"
# path_file_input_9 <- "./data-public/raw/9-combined.rds"

# ---- load-data ---------------------------------------------------------------
ds_gls        <- readRDS(path_file_input_0) %>% dplyr::glimpse(100)
ds_population <- readRDS(path_file_input_1) %>% dplyr::glimpse(100)
ds_suicide    <- readRDS(path_file_input_2) %>% dplyr::glimpse(100)
# ds_combined   <- readRDS(path_file_input_9) %>% dplyr::glimpse(100)

# ---- inspect-data ------------------------------
ds_gls %>% dplyr::glimpse() 
ds_population %>% dplyr::glimpse() 
ds_suicide %>% dplyr::glimpse() 

# ---- tweek-data --------------------------------------------------------------
ds_gls <- ds_gls %>% 
  dplyr::mutate(
    year = lubridate::year(date) %>% as.character() # for joining to other tables
  ) 

counties_gls <- ds_gls %>% 
  na.omit(region) %>% 
  dplyr::distinct(county) %>% 
  as.list() %>% unlist() %>% as.character()

ds_population <- ds_population %>% 
  dplyr::rename(
    n_population = count
  ) %>% 
  dplyr::select(
    "county", "year", "sex", "race", "ethnicity", "age_group", dplyr::everything()
  ) # because we want the same order of columns in both datasets

ds_suicide    <- ds_suicide %>% 
  dplyr::mutate(
    mortality_cause = gsub("^Suicide: ","",mortality_cause),
    mortality_cause = gsub(" \\(.+\\)","",mortality_cause)
  ) %>% 
  dplyr::rename(
    n_suicides = resident_deaths
  ) %>% 
  dplyr::select(
    "county", "year", "sex", "race", "ethnicity", "age_group",dplyr::everything()
  ) # because we want the same order of columns in both datasets

# ---- inspect-data  ---------------------------
ds_suicide %>% dplyr::distinct(mortality_cause) 

# ---- aggregate-0 --------------------------------------------------------------

# there is too much temporal granularity in the GLS source.
# to reduce it, making it compatible with the FL health records:
ds_gls_by_year <- ds_gls %>% 
  dplyr::group_by(region, county, year, audience, type_training) %>%  # one line for this combination
  dplyr::summarize(
    n_trained = sum(n_trained, na.rm = T)
  )

# join population and suicide tables
# to compute the total numbe of suicides across mortality cause
d_suicide = ds_suicide  %>% 
  dplyr::group_by(county, year, sex, race, ethnicity, age_group) %>% 
  # aggregating over `mortality_casue`, otherwise multiple rows per county-year-sex-race-ethnicity-age_group
  dplyr::summarize(
    n_suicides = sum(n_suicides, na.rm = T)
  )
d_suicide %>% glimpse()
# to separate mortality causes into individual columns
d_suicide_wide = ds_suicide %>% 
  tidyr::pivot_wider(names_from = "mortality_cause", values_from = "n_suicides")
# to have both the total of suicides and broken by mortality cause
ds_suicide_wide <- dplyr::left_join(
  d_suicide
  ,d_suicide_wide
  ,by = c("county", "year", "sex", "race", "ethnicity", "age_group")
)
ds_suicide_wide %>% dplyr::glimpse()
rm(d_suicide, d_suicide_wide)


ds_population_suicide <- dplyr::left_join(
  ds_population %>% dplyr::filter(year %in% unique(ds_suicide_wide$year)) # only for available counts
  ,ds_suicide_wide
  ,by = c("county", "year", "sex", "race", "ethnicity", "age_group")
)
# NOTE: the order of dfs in join is correct. Otherwise, not complete population count
ds_population_suicide %>% dplyr::glimpse()

# Save to disk
# This data product (`ds_population_suicide`) is most generic that combines
# suicide counts and population estimates. Rates can be computes from its data.
ds_population_suicide %>% 
  # readr::write_rds("./data-unshared/derived/9-population-suicide.rds")
  readr::write_csv("./data-unshared/derived/9-population-suicide.csv")

ds <- readr::read_csv("./data-unshared/derived/9-population-suicide.csv")

# function-to-aggregate ---------------------------------------------------

# this is why we need to combine some suicide categories:
# g <- ds_suicide %>% 
#   # filter(year == 2015) %>% 
#   filter(!age_group %in% c("1_4","5_9","85_plus","unknown")) %>%
#   # filter(!mortality_cause %in% "Firearms Discharge") %>% 
#   # filter(!mortality_cause %in% c("Firearms Discharge","Hanging, Strangulation, Suffocation") ) %>% 
#   # filter(age_group %in% c("10_14","15_19","20_24")) %>% 
#   group_by(mortality_cause, year, age_group) %>% 
#   summarize(
#     n =  sum(n_suicides, na.rm=T)
#   ) %>% 
#   ungroup()%>% 
#   ggplot(aes(x = year, y = n, group = mortality_cause, color = mortality_cause))+
#   facet_wrap(~age_group, scales = "free")+
#   geom_point(shape=21) + geom_line()+ theme_minimal() +
#   theme(legend.position = "bottom")
# g <- plotly::ggplotly(g)     
# g  
  


compute_rate <- function(
  d,
  grouping_frame
){
  d <- ds_population_suicide
  grouping_frame <- c("county","year")
  # 
  d_wide <- d %>%
    dplyr::group_by_(.dots = grouping_frame) %>%
    dplyr::summarize(
      n_population = sum(n_population, na.rm = T)
      ,n_suicide  = sum(n_suicides, na.rm = T)
      ,n_drug    = sum(`Drugs & Biological Substances`, na.rm=T)
      ,n_gun     = sum(`Firearms Discharge`, na.rm=T)
      ,n_hanging  = sum(`Hanging, Strangulation, Suffocation`, na.rm=T)
      ,n_jump     = sum(`Jump From High Place`, na.rm=T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      n_other = n_suicide - n_drug - n_gun -n_hanging - n_jump
      
      ,rate_suicide = (n_suicide/n_population)*100000
      ,rate_gun   = (n_gun/n_population)*100000
      ,rate_hanging = (n_hanging/n_population)*100000
      ,rate_drug   = (n_drug/n_population)*100000
      ,rate_jump    = (n_jump/n_population)*100000
      ,rate_other   = (n_other/n_population)*100000
      
    )
  d_wide %>% glimpse()
  d_n <- d_wide %>% dplyr::select_(.dots = c(grouping_frame
                                             ,"n_suicide"
                                             ,"n_drug"
                                             ,"n_gun"
                                             ,"n_hanging"
                                             ,"n_jump"
                                             ,"n_other")) %>% 
    tidyr::gather("suicide_cause", "n_suicides", n_suicide, n_drug,n_gun, n_hanging, n_jump, n_other) %>% 
    dplyr::mutate(
      suicide_cause = gsub("^n_","",suicide_cause)
    )
  d_rate <- d_wide %>% dplyr::select_(.dots = c(grouping_frame
                                                ,"rate_suicide"
                                                ,"rate_drug"
                                                ,"rate_gun"
                                                ,"rate_hanging"
                                                ,"rate_jump"
                                                ,"rate_other")) %>% 
    tidyr::gather("suicide_cause", "rate_per_100k", rate_suicide, rate_drug,rate_gun, rate_hanging, rate_jump, rate_other) %>% 
    dplyr::mutate(
      suicide_cause = gsub("^rate_","",suicide_cause)
    )
  
  d_long <- d_wide %>% dplyr::select_(.dots = c(grouping_frame,"n_population")) %>% 
    dplyr::left_join(d_n) %>% 
    dplyr::left_join(d_rate)
  
  ls_out <- list("wide" = d_wide, "long" = d_long )
  return(ls_out)
}
# how to use
ls_computed <-ds_population_suicide %>% compute_rate(grouping_frame = c("county","year"))

# apply rate computation --------------------------------------------------

# grouping_frame <- c("county","year")
# path_to_folder <- "./data-public/derived/rate/"
# 
# 
# d_computed <- ds_population_suicide %>% compute_rate(grouping_frame = grouping_frame)
# 
# file_name <- paste0(path_to_folder, paste0(grouping_frame,collapse = "-"),".csv")
# readr::write_csv(d_computed, file_name)


# create loop to apply the function

ls_grouping_frame <- list(
  c("county","year"                                   )
  ,c("county","year","sex"                             )
  ,c("county","year"      ,"race_ethnicity"            )
  ,c("county","year"                       ,"age_group")
  ,c("county","year","sex","race_ethnicity"            )
  ,c("county","year"      ,"race_ethnicity","age_group")
  ,c("county","year","sex"                 ,"age_group")
  ,c("county","year","sex","race_ethnicity","age_group")
  ,c(         "year"                                   )
  ,c(         "year","sex"                             )
  ,c(         "year",      "race_ethnicity"            )
  ,c(         "year"                       ,"age_group")
  ,c(         "year","sex","race_ethnicity"            )
  ,c(         "year",      "race_ethnicity","age_group")
  ,c(         "year","sex"                 ,"age_group")
  ,c(         "year","sex","race_ethnicity","age_group")
  
)
# common filter
ds_filtered <- ds_population_suicide %>% 
  dplyr::filter(year %in% 2006:2017) %>% 
  dplyr::filter(!age_group == "less_than_1")
# common mutate
ds_filtered <- ds_filtered %>% 
  dplyr::mutate(
    # age_group = ifelse(age_group == "85_plus","85+", age_group)
    # ,age_group = gsub("_", " - ",age_group)
    county = ifelse(county %in% counties_gls, toupper(county),county)
  )

ds_filtered %>% distinct(age_group) %>% arrange(age_group)

for (i in seq_along(ls_grouping_frame)){
  # frame_i %>% print()
  # i <- 1
  path_to_folder <- "./data-unshared/derived/rate/"
  frame_i <- ls_grouping_frame[[i]]
  file_name_wide_i  <- paste0(path_to_folder, paste0(frame_i,collapse = "-"),"-wide",".csv")
  file_name_long_i  <- paste0(path_to_folder, paste0(frame_i,collapse = "-"),"-long",".csv")
  l_computed <- ds_filtered %>% 
    
    compute_rate(grouping_frame = frame_i)
  l_computed[["wide"]] %>% readr::write_csv(file_name_wide_i)
  l_computed[["long"]] %>% readr::write_csv(file_name_long_i)
  
}

for (i in seq_along(ls_grouping_frame)){
  # frame_i %>% print()
  # i <- 1
  path_to_folder <- "./data-unshared/derived/rate_10_24/"
  frame_i <- ls_grouping_frame[[i]]
  file_name_wide_i  <- paste0(path_to_folder, paste0(frame_i,collapse = "-"),"-wide-10-24",".csv")
  file_name_long_i  <- paste0(path_to_folder, paste0(frame_i,collapse = "-"),"-long-10-24",".csv")
  l_computed <- ds_filtered %>% 
    dplyr::filter(age_group %in% c("10_14","15_19","20_24")) %>% 
    compute_rate(grouping_frame = frame_i)
  l_computed[["wide"]] %>% readr::write_csv(file_name_wide_i)
  l_computed[["long"]] %>% readr::write_csv(file_name_long_i)
  
}

for (i in seq_along(ls_grouping_frame)){
  # frame_i %>% print()
  # i <- 1
  path_to_folder <- "./data-unshared/derived/rate_10_19/"
  frame_i <- ls_grouping_frame[[i]]
  file_name_wide_i  <- paste0(path_to_folder, paste0(frame_i,collapse = "-"),"-wide-10-24",".csv")
  file_name_long_i  <- paste0(path_to_folder, paste0(frame_i,collapse = "-"),"-long-10-24",".csv")
  l_computed <- ds_filtered %>% 
    dplyr::filter(age_group %in% c("10_14","15_19")) %>% 
    compute_rate(grouping_frame = frame_i)
  l_computed[["wide"]] %>% readr::write_csv(file_name_wide_i)
  l_computed[["long"]] %>% readr::write_csv(file_name_long_i)
  
}

