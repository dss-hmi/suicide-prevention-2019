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

# For quick verification of the annual cohort structure
ds_suicide_by_age <- readRDS("./data-unshared/derived/cause113-age10-age99/cause113-age10-age99.rds") 

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
      # n_non_gun = n_suicide - n_gun
      n_non_gun_hang   = n_suicide - n_gun - n_hanging 
      
      ,rate_suicide                 = (n_suicide/n_population)*100000
      ,rate_gun                     = (n_gun/n_population)*100000
      ,rate_drug                    = (n_drug/n_population)*100000
      ,rate_hanging                 = (n_hanging/n_population)*100000
      ,rate_jump                    = (n_jump/n_population)*100000
      # ,rate_other                 = (n_other/n_population)*100000
      ,rate_other_seq               = (n_other_seq/n_population)*100000
      ,rate_other_liq               = (n_other_liq/n_population)*100000
      ,rate_other_gas               = (n_other_gas/n_population)*100000
      # ,rate_non_gun                 = (n_non_gun/n_population)*100000
      ,rate_non_gun_hang            = (n_non_gun_hang/n_population)*100000
      
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
                  # ,"n_non_gun"
                  ,"n_non_gun_hang")
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


# ---- overall-trends ---------------------------------------------------------

d <- ds0 %>% 
  compute_rate("year") %>% 
  filter(suicide_cause == "suicide") %>% 
  select(-suicide_cause) %>% 
  mutate(
    n_out_of      = round(n_population/n_suicides,0)
    ,n_population = n_population/1000000
  ) %>% 
  tidyr::pivot_longer(
    cols       = c("n_suicides","n_population", "rate_suicides","n_out_of")
    ,names_to  = "metric"
    ,values_to = "value"
  ) 

labels <- c(
  "n_suicides"     = "Suicides"
  ,"n_population"  = "Population (in Millions)"
  ,"rate_suicides" = "Rate per 100k"
  ,"n_out_of"      = "1 Out of"
)



d %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(alpha = 0.5) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "#1B9E77") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2007,2017,3)) +
  facet_wrap(~metric, scales = "free_y",nrow = 2 , labeller = as_labeller(labels)) +
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse = TRUE
                        ,label.x = 0.9
                        ,label.y = 0.05) +
  geom_text(
    data = d %>% dplyr::filter(year %in% c(2006, 2017))
    , aes(label = round(value,2)), vjust =-0
  )+
  labs(
    x  = NULL
    ,y = NULL
  )


# ---- age-breakdown -----------------------------------------------------------

d <- ds0 %>% 
  compute_rate(c("year","age_group")) %>% 
  filter(suicide_cause == "suicide") %>% 
  select(-suicide_cause) %>% 
  mutate(
    n_out_of     = n_population/n_suicides
    ,n_population = n_population/1000000
  ) %>% 
  tidyr::pivot_longer(
    cols       = c("n_suicides","n_population", "rate_suicides" ,"n_out_of")
    ,names_to  = "metric"
    ,values_to = "value"
  ) 

labels <- c(
  "n_suicides"     = "Suicides"
  ,"n_population"  = "Population \n (in Millions)"
  ,"rate_suicides" = "Rate per 100k"
  ,"n_out_of"      = "1 Out of"
  ,"10-14"         =   "10-14"    
  ,"15-19"         =   "15-19"    
  ,"20-24"         =   "20-24" 
  
)

d %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(alpha = 0.5) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "#1B9E77") +
  facet_grid(metric ~ age_group, scales = "free_y", labeller = as_labeller(labels)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2007,2017,3)) +
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse   = TRUE
                        ,label.x = 0.05
                        ,label.y = 1
                        ,color   = "#D95F02") +
  labs(
    x  = NULL
    ,y = NULL
  )

# ---- age-breakdown-2 -----------------------------------------------------------

d %>% 
  dplyr::filter(age_group %in% c("15-19","20-24")) %>% 
  dplyr::filter(metric %in% c("n_out_of")) %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(alpha = 0.5) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "#1B9E77") +
  facet_wrap(~ age_group, scales = "free_y", labeller = as_labeller(labels)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2007,2017,3)) +
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse   = TRUE
                        ,label.x = 0.05
                        ,label.y = 1
                        ,color   = "#D95F02") +
  labs(
    x  = NULL
    ,y = NULL
  )


# ---- age-breakdown-3 -------------------------------------
# Hypothesis: does entering high-school associated with increased suicide events?
# Can we see the spike in mortality at 13-14 years of age?
# For that we need to view by year mortality event? 

d <- ds_suicide_by_age %>% 
  mutate(age = as.integer(age)) %>% 
  filter(age %in% c(10:40)) %>% 
  group_by(year, age) %>% 
  summarize(
    n_suicide        = sum(count, na.rm = T)
  ) %>% 
  ungroup()

g <- d %>% 
  ggplot(aes(x = age, y = n_suicide))+
  geom_smooth(method = "lm", se= F, size = 1,color = "salmon")+
  geom_smooth(method = "loess", se= F, size = 1,color = "cyan3")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, color = "salmon"
    # , vjust = 7
  )+
  geom_boxplot(aes( group = age), fill = NA)+
  scale_x_continuous(breaks = seq(10,40,1))+
  scale_y_continuous(breaks = seq(0,100,10))+
  geom_vline(xintercept = 24.5, size = 4, alpha = .1)+
  geom_vline(xintercept = 17.5, size = 1, linetype = "dashed", color = "grey80")+
  theme(
    panel.grid.minor = element_blank()
  )+
  labs(
    title = "Suicide events among person of the same age (2006-2018)"
    ,x = "Age in years", y = "Count of suicides (all causes)"
  )
g

# ---- age-breakdown-4 -------------------------------------
# among 10-24 the increase across age is very linear
g <- d %>% 
  filter(age %in% c(10:24)) %>% 
  ggplot(aes(x = age, y = n_suicide))+
  geom_point(shape = 21, alpha = .4, size = 2, position = position_jitter(width = .1))+
  geom_smooth(method = "lm", se= F, size = 1,color = "salmon")+
  geom_smooth(method = "loess", se= F, size = 1,color = "cyan3")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, color = "salmon"
    # , vjust = 7
  )+
  geom_boxplot(aes( group = age), fill = NA)+
  scale_x_continuous(breaks = seq(10,40,1))+
  scale_y_continuous(breaks = seq(0,100,10))+
  geom_vline(xintercept = 24.5, size = 4, alpha = .1)+
  geom_vline(xintercept = 17.5, size = 1, linetype = "dashed", color = "grey80")+
  theme(
    panel.grid.minor = element_blank()
  )+
  labs(
    title = "Suicide events among person of the same age (2006-2018)"
    ,x = "Age in years", y = "Count of suicides (all causes)"
  )
g


# ---- year-breakdown-count -------------------------------------------

suicide_cause_order <- c(
  "drug"        = "Drug"
  ,"gun"          = "Gun"
  ,"hanging"      = "Hanging"
  ,"jump"         = "Jump"
  ,"other_seq"    = "Other Sequelae"
  ,"other_liq"    = "Other Liqud"
  ,"other_gas"    = "Other Gas & Vapor"
)




d <- ds0 %>% 
  compute_rate("year")
# d <- d$long

d %>%  
  filter(!suicide_cause %in% c("suicide","non_gun_hang")) %>% 
  mutate(
    suicide_cause = factor(suicide_cause
                           ,levels = names(suicide_cause_order)
                           ,labels = suicide_cause_order)
  ) %>% 
  ggplot(aes(x = reorder(suicide_cause,desc(suicide_cause)), y = n_suicides)) +
  geom_col(alpha = 0.4, fill = "#1B9E77", color = "#666666") +
  geom_text(aes(label = n_suicides), hjust = 1) +
  coord_flip() +
  facet_wrap(~year) +
  labs(
    x        = NULL
    ,y       = NULL
    ,title   = "Breakdown of Yearly Suicide Counts"
  )


# ---- year-breakdown-rate -------------------------------------------

d %>%  
  filter(!suicide_cause %in% c("suicide","non_gun_hang")) %>% 
  mutate(
    suicide_cause = factor(suicide_cause
                           ,levels = names(suicide_cause_order)
                           ,labels = suicide_cause_order)
  ) %>% 
  ggplot(aes(x = reorder(suicide_cause,desc(suicide_cause)), y = rate_suicides)) +
  geom_col(alpha = 0.4, fill = "#1B9E77", color = "#666666") +
  geom_text(aes(label = round(rate_suicides,1)), hjust = 1) +
  coord_flip() +
  facet_wrap(~year) +
  labs(
    x        = NULL
    ,y       = NULL
    ,title   = "Breakdown of Yearly Suicide Rates"
  )


# ---- g5 -----------------------------------------------------------------

major_causes <- c(
  "gun"           = "Gun"
  ,"hanging"      = "Hanging"
  ,"non_gun_hang" = "Non Gun/Hang")

d %>% 
  filter(suicide_cause %in% names(major_causes)) %>%
  mutate(
    suicide_cause = factor(suicide_cause
                           ,levels = names(major_causes)
                           ,labels = major_causes)
  ) %>% 
  ggplot(aes(x = year,y = n_suicides, color = suicide_cause)) +
  geom_line() +
  geom_point(shape = 21, size = 3) +
  scale_x_continuous(breaks = seq(2006,2017,2)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = NULL
    ,y = " Count"
  )


# ---- g6 -----------------------------------------------------------------

d %>% 
  filter(suicide_cause %in% names(major_causes)) %>%
  mutate(
    suicide_cause = factor(suicide_cause
                           ,levels = names(major_causes)
                           ,labels = major_causes)
  ) %>% 
  ggplot(aes(x = year,y = rate_suicides, color = suicide_cause)) +
  geom_line() +
  geom_point(shape = 21, size = 3) +
  scale_x_continuous(breaks = seq(2006,2017,2)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = NULL
    ,y = " Rate"
  )




# ---- yearly-type-facets ----

# g <- d %>% 
#   filter(suicide_cause %in% major_causes) %>% 
#   ggplot(aes(x = year, y = rate_suicides, color = suicide_cause)) +
#   geom_line() +
#   geom_point(shape = 21) +
#   geom_smooth(method = "lm", se = FALSE) +
#   ggpmisc::stat_poly_eq(
#     formula = y ~ + x
#     ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
#     ,parse = TRUE
#     # , vjust = 7
#   ) 
# g


# g <- d %>% 
#   filter(suicide_cause %in% major_causes) %>% 
#   mutate(
#     suicide_cause = factor(
#       suicide_cause
#       ,levels = names(major_causes)
#       ,labels = major_causes
#     )
#   ) %>%
#   ggplot(aes(x = year, y = rate_suicides)) +
#   geom_line(alpha = 0.5) +
#   geom_point(shape = 21, size = 3, alpha = 0.8) +
#   geom_smooth(method = "lm", se = FALSE, color = "#1B9E77") +
#   scale_x_continuous(breaks = seq(2007,2017,3)) +
#   facet_wrap(~suicide_cause
#              # , scales = "free_y"
#   ) +
#   ggpmisc::stat_poly_eq(
#     formula = y ~ + x
#     ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
#     ,parse = TRUE
#     ,label.x = 0.05
#     ,label.y = 0.99
#   ) +
#   labs(
#     x        = NULL
#     ,y       = NULL
#     ,caption = "Non-Major counts all types other then Gun, Hanging, and Drug"
#     ,title = "Rates per 100,000"
#   )
# 
# g

#when ignoring race and ethnicity for age group 10-24 average increase of suicide
#mortality from gun (+0.1) per year is simaliar to average 
#increase from non-gun means (+0.105)

# g <- d %>% 
#   filter(suicide_cause %in% major_causes) %>% 
#   mutate(
#     suicide_cause = factor(
#       suicide_cause
#       ,levels = names(major_causes)
#       ,labels = major_causes
#     )
#   ) %>%
#   ggplot(aes(x = year, y = n_suicides)) +
#   geom_line(alpha = 0.5) +
#   geom_point(shape = 21, size = 3, alpha = 0.8) +
#   geom_smooth(method = "lm", se = FALSE, color = "#1B9E77") +
#   scale_x_continuous(breaks = seq(2007,2017,3)) +
#   facet_wrap(~suicide_cause
#              # , scales = "free_y"
#   ) +
#   ggpmisc::stat_poly_eq(
#     formula = y ~ + x
#     ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
#     ,parse = TRUE
#     ,label.x = 0.05
#     ,label.y = 0.99
#   ) +
#   labs(
#     x        = NULL
#     ,y       = NULL
#     ,caption = "Non-Major counts all types other then Gun, Hanging, and Drug"
#     ,title = "Counts of suicide events"
#   )
# 
# g
# ---- rate-cause-race ---------------------------------------------

d <- ds0 %>% 
  compute_rate(c("year","sex","race_ethnicity"))


d %>% 
  filter(suicide_cause %in% c("gun","hanging","non_gun_hang")) %>% 
  mutate(
    suicide_cause = factor(suicide_cause
                           ,levels = c("gun","hanging","non_gun_hang")
                           ,labels = c("Gun","Hang","Other"))
  ) %>% 
  ggplot(aes(x = year, y = rate_suicides, color = sex)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(2007,2017,5)) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(suicide_cause ~race_ethnicity
             # , scales = "free"
  ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    # , vjust = 7
  ) +
  labs(
    x      = NULL
    ,y     = NULL
    ,title = "Rate of Suicides by Race and Sex"
    ,color = "Sex"
  )

d %>% 
  filter(suicide_cause %in% c("gun","non_gun","hanging")) %>% 
  mutate(
    suicide_cause = factor(suicide_cause
                           ,levels = c("gun","haning","non_gun")
                           ,labels = c("Gun","Hang","Non-Gun"))
  ) %>% 
  ggplot(aes(x = year, y = rate_suicides, color = suicide_cause)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(2007,2017,5)) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(sex ~race_ethnicity
             # , scales = "free"
  ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    # , vjust = 7
  ) +
  labs(
    x      = NULL
    ,y     = NULL
    ,title = "Rate of Suicides by Race and Sex"
    ,color = "Suicide Cause"
  )
# ---- ----


# not being used in report right now

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
