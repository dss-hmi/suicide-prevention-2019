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
# source("./scripts/modeling/model-basic.R")  
# source("./scripts/common-functions.R")
# ---- declare-globals ---------------------------------------------------------
# you will need to replace this path to the location where you stored your data file
# path_file_input <- "data-unshared/derived/9-population-suicide.csv"
path_file_input <- "https://github.com/dss-hmi/suicide-prevention-2019/raw/b225979bad5e4728d7a594fd455ab929d3ccafb0/data-public/derived/9-population-suicide.csv"

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
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)

# ---- declare-functions -------------------------------
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

# ---- load-data ---------------------------------------------------------------
# data prepared by "./manipulation/9-aggregator.R" combining population estimates and suicide counts
ds_population_suicide <-   readr::read_csv(path_file_input)

d <- readr::read_csv(path_file_input)

# ----- load-data-map ----------------------
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
    year            = as.integer(year)
    ,sex            = factor(sex, levels = c("Male", "Female"))
    ,sex            = forcats::fct_recode(sex,
                                          c("Men" = "Male"),
                                          c("Women" = "Female")
                                          )
    ,race_ethnicity = factor(paste0(race, " + ", ethnicity))
    ,race           = factor(race)
    ,ethnicity      = factor(ethnicity)
    ,age_group      = factor(age_group, levels = names(lvl_age_groups), labels = lvl_age_groups)
    ,n_population   = as.integer(n_population)
    ,n_suicides     = as.integer(n_suicides)
  ) 
ds0 %>% dplyr::glimpse(70)

# ---- g11 ----------------------------
# What is the overall trajectory of suicides in FL between 2006 and 2017? 
d <- ds0 %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  compute_rate("year") %>% 
  dplyr::filter(suicide_cause == "suicide") %>% 
  tidyr::pivot_longer(
    cols       = c("n_suicides", "rate_suicides")
    ,names_to  = "metric"
    ,values_to = "value"
  ) %>% 
  dplyr::mutate(metric = car::recode(
    metric," 'n_suicides'='count';'rate_suicides'='rate_per100k'")) %>% 
  dplyr::mutate(
    metric = factor(
      metric
      ,levels = c("count","rate_per100k")
      ,labels = c("Count", "Rate per 100,000")
    )
  )
d %>% 
  ggplot(aes(x=year, y = value))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  facet_wrap(~metric, scales = "free")+
  ggpmisc::stat_poly_eq(formula = y ~ + x ,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        parse = TRUE, vjust = 7) +   
  geom_text(
    data = d %>% dplyr::filter(year %in% c(2006, 2017))
    , aes(label = round(value,2)), vjust =-0
  )+
  labs(
    title = "Trend in suicide mortality in Florida (ages 10+)", y = NULL
    
  )

# ---- g12 --------------
d12 <- ds0 %>% 
  compute_rate(c("year", "age_group")) %>% 
  dplyr::filter(suicide_cause == "suicide") %>% 
  tidyr::pivot_longer(cols = c("n_suicides", "rate_suicides"),names_to = "metric", values_to = "value") %>% 
  dplyr::mutate(metric = car::recode(metric," 'n_suicides'='count';'rate_suicides'='rate_per100k'")) %>% 
  dplyr::mutate(
    metric = factor(
      metric
      ,levels = c("count","rate_per100k")
      ,labels = c("Count", "Rate per 100,000")
    )
  )
d12 %>% 
  filter(metric == "Count") %>% 
  ggplot(aes(x=year, y = value))+
  geom_col(color = "black", fill = "salmon", alpha = .2)+
  # scale_y_continuous(labels = scales::comma_format(accuracy = 1))+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  facet_wrap(~ age_group, scale = "free")+
  labs(
    title = "Counts of suicide events in Florida from 2006 to 2017"
    ,y = "Number of deaths by suicide", x = NULL
  )
# ---- g13 --------------
d12 %>% 
  filter(metric == "Count") %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  ggplot(aes(x=year, y = value))+
  geom_col(color = "black", fill = "salmon", alpha = .2)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  facet_wrap(~ age_group)+
  labs(
    title = "Counts of suicide events in Florida from 2006 to 2017 (ages 10+)"
    ,y = "Number of deaths by suicide", x = NULL
  )

# ---- g21 ----------------
# demographic growth among the age groups
g21 <- d12 %>% 
  filter(metric == "Count") %>% # population counts are duplicats
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  ggplot(aes(x=year,y=n_population))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  # scale_color_viridis_d(option = "magma",begin = .7, end = .1)+
  # scale_fill_viridis_d(option = "magma",begin = .7, end = .1)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  # facet_wrap(~age_group, scales = "free")+
  facet_wrap(~age_group)+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, vjust = 7
  ) +
  labs(
    title = "Demographic growth in Florida within age groups"
    ,y = "Population count estimate", x = NULL
  )
g21

# ---- g21a ----------------
# demographic growth among the age groups
g21a <- d12 %>% 
  filter(metric == "Count") %>% # population counts are duplicats
  dplyr::filter(age_group %in% c("10-14","15-19","20-24")) %>%
  ggplot(aes(x=year,y=n_population))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  # scale_color_viridis_d(option = "magma",begin = .7, end = .1)+
  # scale_fill_viridis_d(option = "magma",begin = .7, end = .1)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  # facet_wrap(~age_group, scales = "free")+
  facet_wrap(~age_group)+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, vjust = 6
  ) +
  labs(
    title = "Demographic growth in Florida within age groups"
    ,y = "Population count estimate", x = NULL
  )
g21a


# ---- g22 ----------------
# suicide rate among the age groups
g22 <- d12 %>% 
  filter(metric == "Rate per 100,000") %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  ggplot(aes(x=year,y=value))+
  geom_smooth(method = "lm",se = F, color = "springgreen2")+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  # scale_color_viridis_d(option = "magma",begin = .7, end = .1)+
  # scale_fill_viridis_d(option = "magma",begin = .7, end = .1)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  # facet_wrap(~age_group, scales = "free")+
  facet_wrap(~age_group)+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, vjust = 1
  ) +
  geom_text(
    data = d12 %>% 
      filter(metric == "Rate per 100,000") %>% 
      dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>% 
      dplyr::filter(year %in% c(2006, 2017))
    ,aes( label = round(value,1 ) )
    # ,aes( label = round(value,1 ) )
    ,vjust =-0.8, size = 3, color = "grey30", 
  )+
  labs(
    title = "Trend of suicide rate in Florida across age groups (ages 10+)"
    ,y = "Rate per 100,000", x = NULL
    
  )
g22

# ---- g22a ----------------
# suicide rate among the age groups
g22a <- d12 %>% 
  filter(metric == "Rate per 100,000") %>% 
  dplyr::filter(age_group %in% c("10-14","15-19","20-24")) %>%
  # dplyr::mutate(age_group = base::droplevels(age_group)) %>% 
  ggplot(aes(x=year,y=value))+
  geom_smooth(method = "lm",se = F, color = "springgreen2")+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  # scale_color_viridis_d(option = "magma",begin = .7, end = .1)+
  # scale_fill_viridis_d(option = "magma",begin = .7, end = .1)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  # facet_wrap(~age_group, scales = "free")+
  facet_wrap(~age_group)+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, vjust = 3
  ) +
  geom_text(
    data = d12 %>% 
      filter(metric == "Rate per 100,000") %>% 
      dplyr::filter(age_group %in% c("10-14","15-19","20-24")) %>% 
      dplyr::filter(year %in% c(2006, 2017))
    ,aes( label = round(value,1 ) )
    # ,aes( label = round(value,1 ) )
    ,vjust =-0.8, size = 3, color = "grey30", 
  )+
  labs(
    title = "Trend of suicide rate in Florida across age groups (ages 10+)"
    ,y = "Rate per 100,000", x = NULL
    
  )
g22a


# ---- g31 ------------------
# how does trends vary by gender
d31 <- ds0 %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  compute_rate(c("year","sex") )%>% 
  dplyr::filter(suicide_cause == "suicide") %>% 
  tidyr::pivot_longer(cols = c("n_suicides", "rate_suicides"),names_to = "metric", values_to = "value") %>% 
  dplyr::mutate(metric = car::recode(metric," 'n_suicides'='count';'rate_suicides'='rate_per100k'")) %>% 
  dplyr::mutate(
    metric = factor(
      metric
      ,levels = c("count","rate_per100k")
      ,labels = c("Count", "Rate per 100,000")
    )
  )
g31 <- d31 %>%
  # ggplot(aes(x=year, y = value, group = sex, color = sex))+
  ggplot(aes(x=year, y = value, color = sex))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  facet_wrap(~metric, scales = "free")+
  scale_color_viridis_d(option = "magma",begin = .2, end = .65)+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x 
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    ,vjust = 7
  ) +
  geom_text(
    data = d31 %>% 
      dplyr::filter(year %in% c(2006, 2017)) 
    , aes(label = round(value,2))
    , vjust =-0.8, size = 3, color = "grey30" 
  )+
  labs(
    title = "Trend in suicide mortality in Florida (ages 10+)"
    ,y = NULL, x = NULL, color = NULL
  )+
  theme(legend.position = "top")
g31

# ---- g41 -------------------
# how does trends vary by gender and age
d41 <- ds0 %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  compute_rate(c("year","sex","age_group") )%>% 
  dplyr::filter(suicide_cause == "suicide") %>% # means
  tidyr::pivot_longer(cols = c("n_suicides", "rate_suicides"),names_to = "metric", values_to = "value") %>% 
  dplyr::mutate(metric = car::recode(metric," 'n_suicides'='count';'rate_suicides'='rate_per100k'")) %>% 
  dplyr::mutate(
    metric = factor(
      metric
      ,levels = c("count","rate_per100k")
      ,labels = c("Count", "Rate per 100,000")
    )
  )
g41 <- d41 %>% 
  dplyr::filter(metric == "Rate per 100,000") %>% # metric
  ggplot(aes(x=year, y = value, color = sex))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_color_viridis_d(option = "magma",begin = .2, end = .65)+
  # scale_fill_viridis_d(option = "magma",begin = .7, end = .1)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  facet_wrap(~age_group, scales = "free")+
  geom_text(
    data =  d41 %>%
      dplyr::filter(metric == "Rate per 100,000") %>% 
      dplyr::filter(year %in% c(2006, 2017))
    ,aes( label = round(value,1 ) )
    ,vjust =-0.8, size = 3, color = "grey30",
  )+
  labs(
    title = "Trend in suicide mortality in Florida (ages 10+)"
    ,y = "Rate per 100,000", x = NULL, color = NULL
  )+
  theme(legend.position = "top")
g41+
    ggpmisc::stat_poly_eq(
    formula = y ~ + x 
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    ,vjust = 7
  )

# ----- g42 ----------------------
g41 + 
  facet_wrap(~age_group)+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x 
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    ,vjust = 3
  ) 


# ---- g51 --------------------------

d51 <- ds0 %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  compute_rate(c("year","race_ethnicity") ) %>% 
  dplyr::filter(suicide_cause == "suicide") %>% 
  dplyr::mutate(n_population = n_population/1000000) %>% 
  tidyr::pivot_longer(
    cols = c("n_suicides", "rate_suicides","n_population")
    ,names_to = "metric"
    , values_to = "value"
  ) %>% 
  dplyr::mutate(
    metric = factor(
      metric
      ,levels = c("n_suicides","rate_suicides","n_population")
      ,labels = c("Suicide Count", "Suicide Rate (per 100k)", "Population Count (million)")
    )
  )
plot_51 <- function(d, metric_filter){
  g <- d %>% 
    filter(metric == metric_filter) %>% 
    ggplot(aes(x=year, y = value))+
    geom_smooth(method = "lm",se = F)+
    geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
    geom_line(alpha = .2)+
    scale_y_continuous(labels = scales::comma)+
    scale_x_continuous(breaks = seq(2007,2017,5))+
    geom_text(
      data =  d51 %>%
       filter(metric == metric_filter) %>% 
       dplyr::filter(year %in% c(2006, 2017))
      ,aes( label = scales::comma( round(value,1)  ) )
      ,vjust =-0.8, size = 3, color = "grey30"
    )+
    facet_wrap( ~ race_ethnicity, scales = "free",nrow = 1)+
    ggpmisc::stat_poly_eq(
      formula = y ~ + x
      ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
      ,parse = TRUE
      ,vjust = 3
    )+
    labs(
      # title = paste0("Trend in suicide mortality in Florida (ages 10+): ",race_filter)
      y = metric_filter, x = "", color = NULL
    )
}
g51 <- d51 %>% plot_51("Population Count (million)")
# g52 <- d51 %>% plot_51("Suicide Rate (per 100k)")
g53 <- d51 %>% plot_51("Suicide Count")

ggpubr::ggarrange(g51, g53 ,labels = c("Population","Count"), ncol =1, nrow = 2)

# ---- g51a --------------------------

metric_filter <- "Suicide Rate (per 100k)"
g52 <- d51 %>% 
  filter(metric == metric_filter) %>% 
  ggplot(aes(x=year, y = value))+
  geom_smooth(method = "lm",se = F,color = "springgreen2")+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  geom_text(
    data =  d51 %>%
      filter(metric == metric_filter) %>% 
      dplyr::filter(year %in% c(2006, 2017))
    ,aes( label = scales::comma( round(value,1)  ) )
    ,vjust =-0.8, size = 3, color = "grey30"
  )+
  facet_wrap( ~ race_ethnicity, nrow = 1)+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    ,vjust = 6
  )+
  labs(
    y = metric_filter, x = "", color = NULL
  )

g52a <- d51 %>% 
  filter(metric == metric_filter) %>% 
  ggplot(aes(x=year, y = value))+
  geom_smooth(method = "lm",se = F,color = "springgreen2")+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  geom_text(
    data =  d51 %>%
      filter(metric == metric_filter) %>% 
      dplyr::filter(year %in% c(2006, 2017))
    ,aes( label = scales::comma( round(value,1)  ) )
    ,vjust =-0.8, size = 3, color = "grey30"
  )+
  facet_wrap( ~ race_ethnicity, nrow = 1, scales = "free")+
  # ggpmisc::stat_poly_eq(
  #   formula = y ~ + x
  #   ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
  #   ,parse = TRUE
  #   ,vjust = 7
  # )+
  labs(
    y = metric_filter, x = "", color = NULL
  )

ggpubr::ggarrange(g52, g52a , ncol =1, nrow = 2)

# ---- g51c --------------------------

d51c <- ds0 %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  compute_rate(c("year","race_ethnicity","sex") ) %>% 
  dplyr::filter(suicide_cause == "suicide") %>% 
  dplyr::mutate(n_population = n_population/1000000) %>% 
  tidyr::pivot_longer(
    cols = c("n_suicides", "rate_suicides","n_population")
    ,names_to = "metric"
    , values_to = "value"
  ) %>% 
  dplyr::mutate(
    metric = factor(
      metric
      ,levels = c("n_suicides","rate_suicides","n_population")
      ,labels = c("Suicide Count", "Suicide Rate (per 100k)", "Population Count (million)")
    )
  )

metric_filter <- "Suicide Rate (per 100k)"
g51c <- d51c %>% 
  filter(metric == metric_filter) %>% 
  ggplot(aes(x=year, y = value, color = sex))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_color_viridis_d(option = "magma",begin = .2, end = .65)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  geom_text(
    data =  d51c %>%
      filter(metric == metric_filter) %>% 
      dplyr::filter(year %in% c(2006, 2017))
    ,aes( label = scales::comma( round(value,1)  ) )
    ,vjust =-0.8, size = 3, color = "grey30"
  )+
  facet_wrap( ~ race_ethnicity, nrow = 1)+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    ,vjust = 5
  )+
  theme(legend.position = "none")+
  labs(
    y = metric_filter, x = "", color = NULL
  )
g51c
g51ca <- d51c %>% 
  filter(metric == metric_filter) %>% 
  ggplot(aes(x=year, y = value, color = sex))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_color_viridis_d(option = "magma",begin = .2, end = .65)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  geom_text(
    data =  d51c %>%
      filter(metric == metric_filter) %>% 
      dplyr::filter(year %in% c(2006, 2017))
    ,aes( label = scales::comma( round(value,1)  ) )
    ,vjust =-0.8, size = 3, color = "grey30"
  )+
  facet_wrap( ~ race_ethnicity, nrow = 1, scales = "free")+
  # ggpmisc::stat_poly_eq(
  #   formula = y ~ + x
  #   ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
  #   ,parse = TRUE
  #   ,vjust = 7
  # )+
  theme(legend.position = "top")+
  labs(
    y = metric_filter, x = "", color = NULL
  )
g51ca
# ggpubr::ggarrange(g51c, g51ca , ncol =1, nrow = 2)

# ---- g52 ------------------------

d52 <- ds0 %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  compute_rate(c("year","sex","age_group","race_ethnicity") ) %>% 
  dplyr::filter(suicide_cause == "suicide") %>% # means
  dplyr::mutate(n_population = n_population/100000) %>% 
  tidyr::pivot_longer(
    cols = c("n_suicides", "rate_suicides","n_population")
    ,names_to = "metric"
    , values_to = "value"
  ) %>% 
  dplyr::mutate(
    metric = factor(
      metric
      ,levels = c("n_suicides","rate_suicides","n_population")
      ,labels = c("Suicide Count", "Suicide Rate (per 100k)", "Population Count (million)")
    ),
    metric = forcats::fct_rev(metric)
  )
plot_g52 <- function(d, race_filter){
  g52 <- d %>% 
    dplyr::filter(race_ethnicity == race_filter) %>% 
    ggplot(aes(x=year, y = value, color = sex))+
    geom_smooth(method = "lm",se = F)+
    geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
    geom_line(alpha = .2)+
    scale_color_viridis_d(option = "magma",begin = .2, end = .65)+
    # scale_fill_viridis_d(option = "magma",begin = .7, end = .1)+
    scale_y_continuous(labels = scales::comma)+
    scale_x_continuous(breaks = seq(2007,2017,5))+
    # facet_wrap(~age_group, scales = "free")+
    facet_grid(metric ~ age_group, scales = "free")+
    geom_text(
      data =  d52 %>%
        dplyr::filter(race_ethnicity == race_filter) %>% 
        dplyr::filter(year %in% c(2006, 2017))
      ,aes( label = round(value,1 ) )
      ,vjust =-0.8, size = 3, color = "grey30"
    )+
     ggpmisc::stat_poly_eq(
      formula = y ~ + x 
      ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
      ,parse = TRUE
      ,vjust = 3
    )+
    labs(
      title = paste0("Trend in suicide mortality in Florida (ages 10+): ",race_filter), 
      x = "", y = "", color = NULL
    )+
    theme(legend.position = "top")
}
g521 <- d52 %>% plot_g52("White + Non-Hispanic")
g522 <- d52 %>% plot_g52("White + Hispanic")
g523 <- d52 %>% plot_g52("Black & Other + Non-Hispanic")
g524 <- d52 %>% plot_g52("Black & Other + Hispanic")

cat("\n## White + Non-Hispanic\n");g521
cat("\n## White + Hispanic\n"); g522
cat("\n## Black & Other + Non-Hispanic\n"); g523
cat("\n## Black & Other + Hispanic\n"); g524



# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/blogposts/florida-suicide-1/fl-suicide-1.Rmd"
  ,output_format = c(
    "html_document"
    # ,"pdf_document"
    # ,"md_document"
    # "word_document"
  )
  ,clean=TRUE
)



