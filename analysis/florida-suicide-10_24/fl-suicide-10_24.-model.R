rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # pipes %>% 
library(ggplot2)  # graphs
library(dplyr)    # data wrangling
requireNamespace("tidyr")  # data tidying

#----- load-sources -------------------------------
source("./scripts/modeling/model-basic.R")  
source("./scripts/common-functions.R")
# ---- declare-globals ---------------------------------------------------------
# you will need to replace this path to the location where you stored your data file
path_file_input <- "data-unshared/derived/9-population-suicide.csv"

# to help with sorting the levels of the `age_group` factor
lvl_age_groups <- c(
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
age_groups_in_focus <-   lvl_age_groups[4:12]
age_groups_10_24    <-   lvl_age_groups[4:6]

#set default ggplot theme
ggplot2::theme_set(ggplot2::theme_bw())

# ---- load-data ---------------------------------------------------------------
# data prepared by "./manipulation/9-aggregator.R" combining population estimates and suicide counts
ds_population_suicide <-   readr::read_csv(path_file_input)

# map of florida counties
florida_counties_map <- ggplot2::map_data("county") %>% 
  dplyr::filter(region == "florida") %>% 
  dplyr::mutate_at(
    "subregion"
    , ~stringr::str_replace_all(
      .
      ,c(
        "de soto" = "desoto"
        ,"st johns" ="saint johns"
        ,"st lucie" = "saint lucie"
      )
    )
  ) %>% tibble::as_tibble()

# ---- tweak-data-1 -----------------------------------------------------

#mutate and filter data to include only ages 10-24

ds0 <- ds_population_suicide %>%
  dplyr::mutate(
    year          = as.integer(year)
    ,sex           = factor(sex,levels = c("Male", "Female"))
    ,race_ethnicity = factor(paste0(race, " + ", ethnicity))
    ,race          = factor(race)
    ,ethnicity     = factor(ethnicity)
    ,age_group     = factor(age_group
                            ,levels = names(lvl_age_groups)
                            ,labels = lvl_age_groups
                            )
    ,n_population  = as.integer(n_population)
    ,n_suicides    = as.integer(n_suicides)
  ) %>% filter(age_group %in% age_groups_10_24)


ds0 %>% dplyr::glimpse(70)


# ---- declare-functions----------------------------------------

#updated to new compute rate function, that includes option for wide data

compute_rate <- function( d  ,grouping_frame  ,wide = FALSE ){
  # d <- ds_population_suicide
  # grouping_frame <- c("year")
  # 
  d_wide <- d %>%
    dplyr::group_by(.dots = grouping_frame) %>%
    dplyr::summarize(
      n_population      = sum(n_population, na.rm = T)
      ,n_suicide        = sum(n_suicides, na.rm = T)
      ,n_gun            = sum(`Firearms Discharge`, na.rm=T)
      ,n_drug           = sum(`Drugs & Biological Substances`, na.rm=T)
      ,n_hanging        = sum(`Hanging, Strangulation, Suffocation`, na.rm=T)
      ,n_jump           = sum(`Jump From High Place`, na.rm=T)
      ,n_other_seq      = sum(`Other & Unspec & Sequelae`, na.rm = T)
      ,n_other_liq      = sum(`Other & Unspec Sol/Liq & Vapor`, na.rm = T)
      ,n_other_gas      = sum(`Other Gases & Vapors`, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # n_other = n_suicide - n_drug - n_gun -n_hanging - n_jump
      n_non_gun = n_suicide - n_gun
      ,n_non_gun_hang_drug   = n_suicide - n_gun - n_drug - n_hanging 
      
      ,rate_suicide                 = (n_suicide/n_population)*100000
      ,rate_gun                     = (n_gun/n_population)*100000
      ,rate_drug                    = (n_drug/n_population)*100000
      ,rate_hanging                 = (n_hanging/n_population)*100000
      ,rate_jump                    = (n_jump/n_population)*100000
      # ,rate_other                 = (n_other/n_population)*100000
      ,rate_other_seq               = (n_other_seq/n_population)*100000
      ,rate_other_liq               = (n_other_liq/n_population)*100000
      ,rate_other_gas               = (n_other_gas/n_population)*100000
      ,rate_non_gun                 = (n_non_gun/n_population)*100000
      ,rate_non_gun_hang_drug       = (n_non_gun_hang_drug/n_population)*100000
      
    )
  # d_wide %>% glimpse()
  col_select <- c("n_suicide"
                  ,"n_drug"
                  ,"n_gun"
                  ,"n_hanging"
                  ,"n_jump"
                  ,"n_other_seq" 
                  ,"n_other_liq" 
                  ,"n_other_gas"
                  ,"n_non_gun"
                  ,"n_non_gun_hang_drug")
  d_n <- d_wide %>% dplyr::select(c(grouping_frame , col_select)) %>% 
    tidyr::pivot_longer(
      cols       = col_select
      ,names_to  = "suicide_cause"
      ,values_to = "n_suicides"
    ) %>% 
    # tidyr::gather("suicide_cause", "n_suicides", n_suicide, n_drug,n_gun, n_hanging, n_jump, n_other) %>% 
    dplyr::mutate(
      suicide_cause = gsub("^n_","",suicide_cause)
    )
  d_rate <- d_wide %>% dplyr::select(
    c(grouping_frame, gsub("^n_","rate_",col_select))
  ) %>%
    tidyr::pivot_longer(
      cols = gsub("^n_","rate_",col_select)
      ,names_to  = "suicide_cause"
      ,values_to = "rate_suicides"
    ) %>% 
    # tidyr::gather("suicide_cause", "rate_per_100k", rate_suicide, rate_drug,rate_gun, rate_hanging, rate_jump, rate_other) %>% 
    dplyr::mutate(
      suicide_cause = gsub("^rate_","",suicide_cause)
    )
  
  d_long <- d_wide %>% dplyr::select( c(grouping_frame,"n_population") ) %>% 
    dplyr::left_join(d_n) %>% 
    dplyr::left_join(d_rate)
  
  d_out <- d_long
  if(wide){
    d_out <- d_wide
  }
  
  return(d_out)
}


#how to use
# ls_compute_rate <- ds0 %>% compute_rate("year")




# ---- publish ---------------------------------



