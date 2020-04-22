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
lvl_age_groups <-c(
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
ds0 <- ds_population_suicide %>%
  dplyr::mutate(
    year          = as.integer(year)
    ,sex           = factor(sex)
    ,race_ethnicity = factor(paste0(race, " + ", ethnicity))
    ,race          = factor(race)
    ,ethnicity     = factor(ethnicity)
    ,age_group     = factor(age_group, levels = lvl_age_groups)
    ,n_population  = as.integer(n_population)
    ,n_suicides    = as.integer(n_suicides)
  ) 
ds0 %>% dplyr::glimpse(70)




# ---- compute-rate-function --------------------------------------------------------------------
compute_rate <- function(
  d,
  grouping_frame
){
  # d <- ds_population_suicide
  # grouping_frame <- c("year")
  # 
  d_wide <- d %>%
    dplyr::group_by_(.dots = grouping_frame) %>%
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
  d_wide %>% glimpse()
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
  d_n <- d_wide %>% dplyr::select_(.dots = c(grouping_frame , col_select)) %>% 
    tidyr::pivot_longer(
      cols = col_select
      ,names_to  = "suicide_cause"
      ,values_to = "n_suicides"
    ) %>% 
    # tidyr::gather("suicide_cause", "n_suicides", n_suicide, n_drug,n_gun, n_hanging, n_jump, n_other) %>% 
    dplyr::mutate(
      suicide_cause = gsub("^n_","",suicide_cause)
    )
  d_rate <- d_wide %>% dplyr::select_(.dots = c(grouping_frame
                                                ,gsub("^n_","rate_",col_select))) %>%
    tidyr::pivot_longer(
      cols = gsub("^n_","rate_",col_select)
      ,names_to  = "suicide_cause"
      ,values_to = "rate_suicides"
    ) %>% 
    # tidyr::gather("suicide_cause", "rate_per_100k", rate_suicide, rate_drug,rate_gun, rate_hanging, rate_jump, rate_other) %>% 
    dplyr::mutate(
      suicide_cause = gsub("^rate_","",suicide_cause)
    )
  
  d_long <- d_wide %>% dplyr::select_(.dots = c(grouping_frame,"n_population")) %>% 
    dplyr::left_join(d_n) %>% 
    dplyr::left_join(d_rate)
  
  ls_out <- list("wide" = d_wide, "long" = d_long )
  return(ls_out)
}

#how to use
ls_compute_rate <- ds0 %>% compute_rate("year")


# ---- suicide cause -----------------------------------------------------------
major_causes <- c("gun","hanging","drug","non_gun","non_gun_hang_drug")
d <- ds0 %>% 
  filter(age_group %in% age_groups_10_24) %>%
  # filter(year == 2017) %>% 
  compute_rate("year")
d <- d$long


g <- d %>%  
  filter(suicide_cause %in% major_causes) %>% 
  ggplot(aes(x = reorder(suicide_cause,-n_suicides), y = n_suicides)) +
  geom_col(alpha = 0.4) +
  geom_text(aes(label = n_suicides)) +
  coord_flip() +
  facet_wrap(~year)
g

g <- d %>%  
  filter(suicide_cause %in% major_causes) %>% 
  ggplot(aes(x = reorder(suicide_cause, rate_suicides), y = rate_suicides)) +
  geom_col(alpha = 0.4) +
  geom_text(aes(label = round(rate_suicides,1))) +
  coord_flip() +
  facet_wrap(~year)
g

g <- d %>% 
  filter(suicide_cause %in% major_causes) %>% 
  ggplot(aes(x = year, y = rate_suicides, color = suicide_cause)) +
  geom_line() +
  geom_point(shape = 21)
g


g <- d %>% 
  filter(suicide_cause %in% major_causes) %>% 
  ggplot(aes(x = year, y = rate_suicides, color = suicide_cause)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(2007,2017,5)) +
  facet_wrap(~suicide_cause
             # , scales = "free"
             ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    # , vjust = 7
  ) 
g

#when ignoring race and ethnicity for age group 10-24 average increase of suicide
#mortality from gun (+0.1) per year is simaliar to average 
#increase from non-gun means (+0.105)

d <- ds0 %>% 
  filter(age_group %in% age_groups_10_24) %>% 
  compute_rate(c("year","sex","race_ethnicity"))
  
d <- d$long  
  

g <- d %>% 
  filter(suicide_cause %in% c("gun","non_gun")) %>% 
  ggplot(aes(x = year, y = rate_suicides, color = sex)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(2007,2017,5)) +
  facet_grid(suicide_cause ~race_ethnicity
             # , scales = "free"
  ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    # , vjust = 7
  ) 
g

g <- d %>% 
  filter(suicide_cause %in% c("gun","non_gun")) %>% 
  ggplot(aes(x = year, y = rate_suicides, color = suicide_cause)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(2007,2017,5)) +
  facet_grid(sex ~race_ethnicity
             # , scales = "free"
  ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    # , vjust = 7
  ) 
g

g <- d %>% 
  filter(suicide_cause %in% c("gun","non_gun","suicide")) %>% 
  ggplot(aes(x = year, y = rate_suicides, color = suicide_cause)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(2007,2017,5)) +
  facet_grid(sex ~race_ethnicity
             # , scales = "free"
  ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    # , vjust = 7
  ) 
g



# ---- graphing function --------------------------------------------------
library(rlang)

ds_test <- d %>% 
  filter(suicide_cause %in% c("gun","non_gun","suicide"))

make_facet_graph <- function(
  ds
  ,x
  ,y
  ,color
  ,facet_expr = NULL
  ,smooth = FALSE
  ){
  
  # use of ensym, allows user to either provided quoted strings or unqouted strings
  g_out <- ds %>% 
    ggplot(
      aes(
        x      = !!ensym(x)
        ,y     = !!ensym(y)
        ,color = !!ensym(color)
        )
      ) +
    geom_line() +
    geom_point(shape = 21) 
    # scale_x_continuous(breaks = seq(2007,2017,5))
  
  if(smooth){
    g_out <- g_out +
      geom_smooth(method = "lm", se = FALSE)
  }
  
  if(!is.null(facet_expr)){
    
    facet_formula <- enexpr(facet_expr)
    
    g <- g +
      facet_grid(facet_formula)
    }
  
return(g)
}      



#Use of Enysm within graph allows both quoted or unqouted variables

test_graph <- make_facet_graph(ds_test,"year","rate_suicides","suicide_cause"
                               ,facet_expr =  "sex ~ race_ethnicity"
                               ,smooth    = TRUE)

test_graph4 <-  make_facet_graph(ds_test,year,rate_suicides,suicide_cause
                                 ,facet_expr = "sex ~ year")

test_graph5 <-  make_facet_graph(ds_test,year,rate_suicides,suicide_cause
                                 ,facet_expr = sex ~ .)




# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/blogposts/florida-demographic-growth/fl-demo-growth.Rmd"
  ,output_format = c(
    "html_document"
    # ,"pdf_document"
    # ,"md_document"
    # "word_document"
  )
  ,clean=TRUE
)



