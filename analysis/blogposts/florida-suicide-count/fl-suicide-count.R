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

# ---- g0 -----------------------------
# How did the total population of Florida changed over the years?
ds0 %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(
    count = sum(n_population, na.rm = T)
  ) %>% 
  ggplot(aes(x=year, y = count))+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()

# What was the trajectory of growth for each age group?
ds0 %>% 
  dplyr::group_by(year, age_group) %>% 
  dplyr::summarize(
    count = sum(n_population, na.rm = T)
  ) %>% 
  ggplot(aes(x=year, y = count, group = age_group))+
  geom_point()+
  geom_line()+
  facet_wrap(~age_group, scales = "free")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007, 2017,5))+
  theme_bw()

# What was the trajectory of growth for each age group by sex? Now only for 10-84
ds0 %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:12]) %>% 
  dplyr::group_by(year, age_group, sex) %>% 
  dplyr::summarize(
    count = sum(n_population, na.rm = T)
  ) %>% 
  ggplot(aes(x=year, y = count, group = interaction(age_group,sex), color = sex))+
  geom_point()+
  geom_line()+
  facet_wrap(~age_group, scales = "free")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007, 2017,5))+
  theme_bw()

# What was the trajectory of growth for each ethnic group? Now only for 10-84
ds0 %>% 
  dplyr::filter(age_group %in% lvl_age_groups[4:12]) %>% 
  dplyr::group_by(year, age_group, race_ethnicity) %>% 
  dplyr::summarize(
    count = sum(n_population, na.rm = T)
  ) %>% 
  ggplot(aes(x=year, y = count, group = interaction(age_group,race_ethnicity), color = race_ethnicity))+
  geom_point()+
  geom_line()+
  facet_wrap(~age_group, scales = "free")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007, 2017,5))+
  theme_bw()




# ----- compute-rate-function --------------------
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



# ---- g1 ----------------------------
# What is the trajectory of suicides in FL between 2006 and 2017? 
d <- ds0 %>% 
  dplyr::filter(age_group %in% age_groups_in_focus) %>%
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

# What is the overall trajectory of suicides in FL between 2006 and 2017? 
d <- ds0 %>% 
  dplyr::filter(age_group %in% age_groups_in_focus) %>%
  dplyr::group_by(year) %>% 
  dplyr::summarize(
    count = sum(n_suicides, na.rm = T)
  ) %>% dplyr::ungroup()
d %>% 
  ggplot(aes(x=year, y = count))+
  geom_smooth(method = "lm",se = F)+
  geom_point(shape = 21, size =3, alpha = .8, fill = NA)+
  geom_line(alpha = .2)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = seq(2007,2017,5))+
  ggpmisc::stat_poly_eq(formula = y ~ + x ,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        parse = TRUE, vjust = 7) +   
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
  input = "./analysis/blogposts/florida-demographic-growth/fl-demo-growth.Rmd"
  ,output_format = c(
    "html_document"
    # ,"pdf_document"
    # ,"md_document"
    # "word_document"
  )
  ,clean=TRUE
)



