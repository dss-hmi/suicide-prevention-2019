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
    ,race_ethnicity = factor(paste0(race, " + ", ethnicity))
    ,race           = factor(race)
    ,ethnicity      = factor(ethnicity)
    ,age_group      = factor(age_group, levels = lvl_age_groups)
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
    title = "Trend in suicide mortality in Florida (ages 10+)"
    
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
  geom_col(color = "black", fill = "salmon", alpha = .3)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  facet_wrap(~ age_group, scale = "free")+
  labs(
    title = "Counts of suicide events in Florida from 2006 to 2017"
  )
# ---- g13 --------------
d12 %>% 
  filter(metric == "Count") %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  ggplot(aes(x=year, y = value))+
  geom_col(color = "black", fill = "salmon", alpha = .3)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  facet_wrap(~ age_group)+
  labs(
    title = "Counts of suicide events in Florida from 2006 to 2017 (ages 10+)"
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
  )
g21

# ---- g22 ----------------
# suicide rate among the age groups
g22 <- d12 %>% 
  filter(metric == "Rate per 100,000") %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  ggplot(aes(x=year,y=value))+
  geom_smooth(method = "lm",se = F, color = "salmon")+
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
    ,y = "Rate per 100,000", x = "Year"
    
  )
g22


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
    ,vjust = 5
  ) +
  geom_text(
    data = d31 %>% 
      dplyr::filter(year %in% c(2006, 2017)) 
    , aes(label = round(value,2))
    , vjust =-0.8, size = 3, color = "grey30" 
  )+
  labs(
    title = "Trend in suicide mortality in Florida (ages 10+)"

  )
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
  scale_color_viridis_d(option = "magma",begin = .7, end = .1)+
  # scale_fill_viridis_d(option = "magma",begin = .7, end = .1)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  facet_wrap(~age_group, scales = "free")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x 
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    ,vjust = 7
  ) +
  geom_text(
    data =  d41 %>%
      dplyr::filter(metric == "Rate per 100,000") %>% 
      dplyr::filter(year %in% c(2006, 2017))
    ,aes( label = round(value,1 ) )
    ,vjust =-0.8, size = 3, color = "grey30",
  )+
  labs(
    title = "Trend in suicide mortality in Florida (ages 10+)"
    ,y = "Rate per 100,000", x = "Year"
    
  )
g41

# ----- g42 ----------------------
g41 + facet_wrap(~age_group)



# ---- old --------------
# What is the trajectory of suicides in FL between 2006 and 2017? 
d <- ds0 %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:13]) %>%
  dplyr::group_by(year, sex, age_group) %>% 
  dplyr::summarize(
    count = sum(n_suicides, na.rm = T)
    # count = sum(n_population, na.rm = T)/1000
  ) %>% dplyr::ungroup()
d %>% 
  ggplot(aes(x=year, y = count, color = sex, fill= sex))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_color_viridis_d(option = "magma",begin = .7, end = .1)+
  scale_fill_viridis_d(option = "magma",begin = .7, end = .1)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  # facet_wrap(~age_group)+
  facet_wrap(~age_group, scales = "free")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, vjust = 7
  ) +   
  geom_text(
    data = d %>% dplyr::filter(year %in% c(2006, 2017)), aes(label = count), vjust =-0)+
  theme_bw()



# How does the overall trajectory of suicides counts differ by gender?
d <- ds0 %>% 
  dplyr::filter(age_group %in% age_groups_in_focus) %>%
  dplyr::group_by(year, sex) %>% 
  dplyr::summarize(
    n_population = sum(n_population, na.rm = T)
    ,n_suicides = sum(n_suicides, na.rm = T) 
  ) %>% dplyr::ungroup()
d %>% 
  ggplot(aes(x=year, y = n_suicides, color = sex))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  ggpmisc::stat_poly_eq(formula = y ~ + x ,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        parse = TRUE, vjust = 7) +   
  geom_text(
    data = d %>% dplyr::filter(year %in% c(2006, 2017)), aes(label = n_suicides), vjust =-0
  )+
  theme_bw()

# How does the overall trajectory of suicide RATE PER 100,000 differ by gender?
d <- ds0 %>% 
  dplyr::filter(age_group %in% age_groups_in_focus) %>%
  dplyr::group_by(year, sex) %>% 
  dplyr::summarize(
    n_population = sum(n_population, na.rm = T)
    ,n_suicides = sum(n_suicides, na.rm = T) 
  ) %>% dplyr::ungroup() %>% 
  dplyr::mutate(
    rate_per100k_suicide = n_suicides/ n_population * 100000
  )
d %>% 
  ggplot(aes(x=year, y = rate_per100k_suicide, color = sex))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  ggpmisc::stat_poly_eq(formula = y ~ + x ,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        parse = TRUE, vjust = 7) +   
  geom_text(
    data = d %>% dplyr::filter(year %in% c(2006, 2017)), aes(label = round(rate_per100k_suicide,2) ), vjust =-0
  )+
  theme_bw()

 # (1)How does the overall trajectory of suicide RATE PER 100,000 differ by gender, age, and ethicity within the age group of interest?
d <- ds0 %>% 
  # dplyr::filter(age_group %in% age_groups_in_focus) %>%
  dplyr::filter(age_group %in% c("10_14","15_19","20_24")) %>%
  dplyr::group_by(year, sex, age_group, race_ethnicity) %>% 
  dplyr::summarize(
    n_population = sum(n_population, na.rm = T)
    ,n_suicides = sum(n_suicides, na.rm = T) 
  ) %>% dplyr::ungroup() %>% 
  dplyr::mutate(
    rate_per100k_suicide = n_suicides/ n_population * 100000
  )
d %>% 
  ggplot(aes(x=year, y = rate_per100k_suicide, color = sex))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  ggpmisc::stat_poly_eq(formula = y ~  x ,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        parse = TRUE, vjust = 7) +   
  geom_text(
    data = d %>% dplyr::filter(year %in% c(2006, 2017))
    , aes(label = round(rate_per100k_suicide,2) )
    # , vjust =-0
  )+
  # facet_wrap(~race_ethnicity, scales = "free")+
  # facet_grid(age_group ~race_ethnicity, scales = "free")+
  facet_grid(race_ethnicity ~ age_group, scales = "free")+
  theme_bw() 

# How does the overall trajectory of suicide COUNTS differ by gender, age, and ethicity within the age group of interest? 
d <- ds0 %>% 
  # dplyr::filter(age_group %in% age_groups_in_focus) %>%
  dplyr::filter(age_group %in% c("10_14","15_19","20_24")) %>%
  dplyr::group_by(year, sex, age_group, race_ethnicity) %>% 
  dplyr::summarize(
    n_population = sum(n_population, na.rm = T)
    ,n_suicides = sum(n_suicides, na.rm = T) 
  ) %>% dplyr::ungroup() %>% 
  dplyr::mutate(
    rate_per100k_suicide = n_suicides/ n_population * 100000
  )
d %>% 
  ggplot(aes(x=year, y = n_population, color = sex))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  ggpmisc::stat_poly_eq(formula = y ~  x ,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        parse = TRUE, vjust = 7) +   
  geom_text(
    data = d %>% dplyr::filter(year %in% c(2006, 2017))
    , aes(label = round(n_population,2) )
    # , vjust =-0
  )+
  # facet_wrap(~race_ethnicity, scales = "free")+
  facet_grid(age_group ~race_ethnicity, scales = "free")+
  theme_bw() 

 
get_linear_model <- function(d, model_equation){
  # eq_formula <- as.formula("count ~ year")
  # eq_formula <- as.formula(model_equation)
  m <- stats::glm(
    formula = as.formula(model_equation)
    ,family = "gaussian"
    ,data = d
  )
  eq <- substitute(
      italic(y) == x0 + x1 %.% italic(x)*","~~italic(r)^2~"="~r2,
      list(
        # x0  = format(unname(coef(m)[1]), digits = 2),
        x0  = format(unname(coef(m)[1] + coef(m)[2]*x_at_intercept) , digits = 2,big.mark = ","),
        x1  = format(unname(coef(m)[2]), digits = 2),
        r2 = format((1 - (summary(m)$deviance/summary(m)$null.deviance)), digits = 3)
        # https://stats.stackexchange.com/questions/46345/how-to-calculate-goodness-of-fit-in-glm-r
      )
    )
  return(
    list("model" = m, "equation" = eq)
  )
}
# how to use
# m <- d %>% get_linear_model("count ~ year")
# m$equation



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



