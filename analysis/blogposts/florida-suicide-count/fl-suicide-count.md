---
title: "Florida Demographic Growth"
author: "Andriy Koval"
date: "March 27, 2020"
output:
  html_document:
    theme: simplex
    toc: true
    toc_depth: 3
    keep_md: true
    toc_float: true
    code_folding: show
---

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->

# Abstract



# Environment
<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 

```{.r .numberLines}
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # pipes %>% 
library(ggplot2)  # graphs
library(dplyr)    # data wrangling
requireNamespace("tidyr")  # data tidying
```

 
 ```{.r .numberLines}
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
 ```
 

```{.r .numberLines}
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
  return(d_long)
}

#how to use
# ls_compute_rate <- ds0 %>% compute_rate("year")

make_facet_graph <- function(
  d
  ,x_aes
  ,y_aes
  ,color_aes
  ,facet_expr = NULL
  ,smooth = FALSE
){
  # browser()
  # use of ensym, allows user to either provided quoted strings or unqouted strings
  g_out <- d %>% 
    ggplot(
      aes_string(
        x      = x_aes
        ,y     = y_aes
        ,color = color_aes
      )
    ) +
    geom_line() +
    geom_point(shape = 21) +
    scale_x_continuous(breaks = seq(2007,2017,5))
  
  if(smooth){
    g_out <- g_out +
      geom_smooth(method = "lm", se = FALSE)
  }
  
  if(!is.null(facet_expr)){
    
    facet_formula <- enexpr(facet_expr)
    
    g_out <- g_out +
      facet_grid(facet_formula)
  }
  
  return(g_out)
} 
```

# Data  

The data comes from Florida Health Charts and contains suicide mortality between 2006 and 2017, broken down by suicide means, county, sex, age, and race. The the dataset is [available for download here](). 

```{.r .numberLines}
# data prepared by "./manipulation/9-aggregator.R" combining population estimates and suicide counts
ds_population_suicide <-   readr::read_csv(path_file_input)
```


## Data Tweaks
This dataset already comes  well-gromed, so only some minor housekeeping tweaks are necessary to make graphing more convenient. 

```{.r .numberLines}
ds0 <- ds_population_suicide %>%
  dplyr::mutate(
    year            = as.integer(year)
    ,sex            = factor(sex)
    ,race_ethnicity = factor(paste0(race, " + ", ethnicity))
    ,race           = factor(race)
    ,ethnicity      = factor(ethnicity)
    ,age_group      = factor(age_group, levels = lvl_age_groups)
    ,n_population   = as.integer(n_population)
    ,n_suicides     = as.integer(n_suicides)
  ) 
ds0 %>% dplyr::glimpse(70)
```

```
Observations: 83,616
Variables: 16
$ county                                <chr> "Alachua", "Alachua...
$ year                                  <int> 2006, 2006, 2006, 2...
$ sex                                   <fct> Female, Female, Fem...
$ race                                  <fct> Black & Other, Blac...
$ ethnicity                             <fct> Hispanic, Hispanic,...
$ age_group                             <fct> 1_4, 10_14, 15_19, ...
$ n_population                          <int> 36, 50, 125, 250, 1...
$ n_suicides                            <int> NA, NA, NA, NA, NA,...
$ `Drugs & Biological Substances`       <dbl> NA, NA, NA, NA, NA,...
$ `Other Gases & Vapors`                <dbl> NA, NA, NA, NA, NA,...
$ `Hanging, Strangulation, Suffocation` <dbl> NA, NA, NA, NA, NA,...
$ `Firearms Discharge`                  <dbl> NA, NA, NA, NA, NA,...
$ `Jump From High Place`                <dbl> NA, NA, NA, NA, NA,...
$ `Other & Unspec & Sequelae`           <dbl> NA, NA, NA, NA, NA,...
$ `Other & Unspec Sol/Liq & Vapor`      <dbl> NA, NA, NA, NA, NA,...
$ race_ethnicity                        <fct> Black & Other + His...
```

The unit of analysis of this dataset is defined by the first 6 variables: `county`, `year`, `sex`, `race`, `ethnicity`, `age_group`. For each unit, there are two measures: the number of people (`n_population`) and the number of observed suicide events (`n_suicides`). The latter is broken down by 7 means of suicide. 


# Q1 
> What is the overall trajectory of suicides in FL between 2006 and 2017? 


```{.r .numberLines}
# What is the overall trajectory of suicides in FL between 2006 and 2017? 
d <- ds0 %>% 
  dplyr::filter(age_group %in% age_groups_in_focus) %>%
  compute_rate("year") %>% 
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
```

```
Warning: group_by_() is deprecated. 
Please use group_by() instead

The 'programming' vignette or the tidyeval book can help you
to program with group_by() : https://tidyeval.tidyverse.org
This warning is displayed once per session.
```

```
Observations: 12
Variables: 22
$ year                   <int> 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017
$ n_population           <int> 15585279, 15801908, 15869170, 15914067, 16245931, 16339483, 16493976, 16659012, 1688...
$ n_suicide              <int> 2310, 2467, 2620, 2755, 2605, 2633, 2757, 2729, 2797, 2959, 2928, 3001
$ n_gun                  <dbl> 1162, 1214, 1327, 1401, 1356, 1381, 1420, 1449, 1417, 1501, 1542, 1597
$ n_drug                 <dbl> 378, 372, 454, 455, 419, 377, 421, 402, 421, 413, 388, 371
$ n_hanging              <dbl> 465, 548, 524, 577, 539, 604, 607, 578, 651, 731, 632, 704
$ n_jump                 <dbl> 50, 41, 52, 43, 51, 56, 44, 60, 69, 60, 83, 58
$ n_other_seq            <dbl> 153, 155, 147, 186, 137, 133, 182, 170, 138, 156, 183, 182
$ n_other_liq            <dbl> 13, 19, 15, 20, 28, 14, 21, 13, 20, 15, 19, 9
$ n_other_gas            <dbl> 89, 118, 101, 73, 75, 68, 62, 57, 81, 83, 81, 80
$ n_non_gun              <dbl> 1148, 1253, 1293, 1354, 1249, 1252, 1337, 1280, 1380, 1458, 1386, 1404
$ n_non_gun_hang_drug    <dbl> 305, 333, 315, 322, 291, 271, 309, 300, 308, 314, 366, 329
$ rate_suicide           <dbl> 14.82168, 15.61204, 16.51000, 17.31173, 16.03478, 16.11434, 16.71519, 16.38152, 16.5...
$ rate_gun               <dbl> 7.455754, 7.682617, 8.362126, 8.803532, 8.346705, 8.451920, 8.609204, 8.697995, 8.39...
$ rate_drug              <dbl> 2.425366, 2.354146, 2.860893, 2.859106, 2.579107, 2.307295, 2.552447, 2.413108, 2.49...
$ rate_hanging           <dbl> 2.983585, 3.467936, 3.302000, 3.625723, 3.317754, 3.696567, 3.680131, 3.469594, 3.85...
$ rate_jump              <dbl> 0.3208156, 0.2594623, 0.3276794, 0.2702012, 0.3139248, 0.3427281, 0.2667641, 0.36016...
$ rate_other_seq         <dbl> 0.9816956, 0.9808942, 0.9263244, 1.1687773, 0.8432881, 0.8139792, 1.1034332, 1.02046...
$ rate_other_liq         <dbl> 0.08341205, 0.12023864, 0.09452290, 0.12567498, 0.17235085, 0.08568203, 0.12731921, ...
$ rate_other_gas         <dbl> 0.5710517, 0.7467453, 0.6364542, 0.4587137, 0.4616541, 0.4161698, 0.3758948, 0.34215...
$ rate_non_gun           <dbl> 7.365925, 7.929422, 8.147874, 8.508196, 7.688079, 7.662421, 8.105990, 7.683529, 8.17...
$ rate_non_gun_hang_drug <dbl> 1.956975, 2.107340, 1.984981, 2.023367, 1.791218, 1.658559, 1.873411, 1.800827, 1.82...
```

```
Warning: select_() is deprecated. 
Please use select() instead

The 'programming' vignette or the tidyeval book can help you
to program with select() : https://tidyeval.tidyverse.org
This warning is displayed once per session.
```

```{.r .numberLines}
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
    title = "Dynamics in suicide mortality in Florida among persons 10-84 years of age"
  
  )
```

<img src="figure_rmd/g1-1.png" width="900px" />

```{.r .numberLines}
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
```

<img src="figure_rmd/g1-2.png" width="900px" />

```{.r .numberLines}
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
```

<img src="figure_rmd/g1-3.png" width="900px" />

```{.r .numberLines}
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
```

<img src="figure_rmd/g1-4.png" width="900px" />

```{.r .numberLines}
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
```

<img src="figure_rmd/g1-5.png" width="900px" />

```{.r .numberLines}
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
```

<img src="figure_rmd/g1-6.png" width="900px" />

```{.r .numberLines}
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
```

session information
===========================================================================

For the sake of documentation and reproducibility, the current report was rendered in the following environment.  Click the line below to expand.

<details>
  <summary>Environment <span class="glyphicon glyphicon-plus-sign"></span></summary>

```
- Session info -------------------------------------------------------------------------------------------------------
 setting  value                       
 version  R version 3.6.2 (2019-12-12)
 os       Windows 10 x64              
 system   x86_64, mingw32             
 ui       RTerm                       
 language (EN)                        
 collate  English_United States.1252  
 ctype    English_United States.1252  
 tz       America/New_York            
 date     2020-04-26                  

- Packages -----------------------------------------------------------------------------------------------------------
 package     * version date       lib source        
 abind         1.4-5   2016-07-21 [1] CRAN (R 3.6.0)
 assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.2)
 backports     1.1.5   2019-10-02 [1] CRAN (R 3.6.1)
 callr         3.4.2   2020-02-12 [1] CRAN (R 3.6.2)
 car           3.0-7   2020-03-11 [1] CRAN (R 3.6.3)
 carData       3.0-3   2019-11-16 [1] CRAN (R 3.6.1)
 cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.6.2)
 cli           2.0.1   2020-01-08 [1] CRAN (R 3.6.2)
 codetools     0.2-16  2018-12-24 [2] CRAN (R 3.6.2)
 colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.6.1)
 crayon        1.3.4   2017-09-16 [1] CRAN (R 3.6.2)
 curl          4.3     2019-12-02 [1] CRAN (R 3.6.2)
 data.table    1.12.8  2019-12-09 [1] CRAN (R 3.6.2)
 desc          1.2.0   2018-05-01 [1] CRAN (R 3.6.2)
 devtools      2.2.2   2020-02-17 [1] CRAN (R 3.6.3)
 digest        0.6.24  2020-02-12 [1] CRAN (R 3.6.2)
 dplyr       * 0.8.4   2020-01-31 [1] CRAN (R 3.6.2)
 ellipsis      0.3.0   2019-09-20 [1] CRAN (R 3.6.2)
 evaluate      0.14    2019-05-28 [1] CRAN (R 3.6.2)
 fansi         0.4.1   2020-01-08 [1] CRAN (R 3.6.2)
 farver        2.0.3   2020-01-16 [1] CRAN (R 3.6.2)
 forcats       0.4.0   2019-02-17 [1] CRAN (R 3.6.2)
 foreign       0.8-72  2019-08-02 [2] CRAN (R 3.6.2)
 fs            1.3.1   2019-05-06 [1] CRAN (R 3.6.2)
 ggplot2     * 3.2.1   2019-08-10 [1] CRAN (R 3.6.2)
 ggpmisc       0.3.3   2019-12-01 [1] CRAN (R 3.6.3)
 glue          1.3.1   2019-03-12 [1] CRAN (R 3.6.2)
 gtable        0.3.0   2019-03-25 [1] CRAN (R 3.6.2)
 haven         2.2.0   2019-11-08 [1] CRAN (R 3.6.2)
 hms           0.5.3   2020-01-08 [1] CRAN (R 3.6.2)
 htmltools     0.4.0   2019-10-04 [1] CRAN (R 3.6.2)
 knitr       * 1.28    2020-02-06 [1] CRAN (R 3.6.2)
 labeling      0.3     2014-08-23 [1] CRAN (R 3.6.0)
 lazyeval      0.2.2   2019-03-15 [1] CRAN (R 3.6.2)
 lifecycle     0.1.0   2019-08-01 [1] CRAN (R 3.6.2)
 magrittr    * 1.5     2014-11-22 [1] CRAN (R 3.6.2)
 memoise       1.1.0   2017-04-21 [1] CRAN (R 3.6.2)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 3.6.2)
 openxlsx      4.1.4   2019-12-06 [1] CRAN (R 3.6.3)
 pillar        1.4.3   2019-12-20 [1] CRAN (R 3.6.2)
 pkgbuild      1.0.6   2019-10-09 [1] CRAN (R 3.6.2)
 pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 3.6.2)
 pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.6.2)
 plyr          1.8.6   2020-03-03 [1] CRAN (R 3.6.3)
 polynom       1.4-0   2019-03-22 [1] CRAN (R 3.6.2)
 prettyunits   1.1.1   2020-01-24 [1] CRAN (R 3.6.2)
 processx      3.4.2   2020-02-09 [1] CRAN (R 3.6.2)
 ps            1.3.2   2020-02-13 [1] CRAN (R 3.6.2)
 purrr         0.3.3   2019-10-18 [1] CRAN (R 3.6.2)
 R6            2.4.1   2019-11-12 [1] CRAN (R 3.6.2)
 Rcpp          1.0.3   2019-11-08 [1] CRAN (R 3.6.2)
 readr         1.3.1   2018-12-21 [1] CRAN (R 3.6.2)
 readxl        1.3.1   2019-03-13 [1] CRAN (R 3.6.2)
 remotes       2.1.1   2020-02-15 [1] CRAN (R 3.6.2)
 reshape2      1.4.3   2017-12-11 [1] CRAN (R 3.6.2)
 rio           0.5.16  2018-11-26 [1] CRAN (R 3.6.3)
 rlang         0.4.4   2020-01-28 [1] CRAN (R 3.6.2)
 rmarkdown     2.1     2020-01-20 [1] CRAN (R 3.6.2)
 rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.6.2)
 scales        1.1.0   2019-11-18 [1] CRAN (R 3.6.2)
 sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.6.2)
 stringi       1.4.5   2020-01-11 [1] CRAN (R 3.6.2)
 stringr       1.4.0   2019-02-10 [1] CRAN (R 3.6.2)
 testthat      2.3.1   2019-12-01 [1] CRAN (R 3.6.2)
 tibble        2.1.3   2019-06-06 [1] CRAN (R 3.6.2)
 tidyr         1.0.2   2020-01-24 [1] CRAN (R 3.6.2)
 tidyselect    1.0.0   2020-01-27 [1] CRAN (R 3.6.2)
 usethis       1.5.1   2019-07-04 [1] CRAN (R 3.6.2)
 utf8          1.1.4   2018-05-24 [1] CRAN (R 3.6.2)
 vctrs         0.2.2   2020-01-24 [1] CRAN (R 3.6.2)
 viridisLite   0.3.0   2018-02-01 [1] CRAN (R 3.6.2)
 withr         2.1.2   2018-03-15 [1] CRAN (R 3.6.2)
 xfun          0.12    2020-01-13 [1] CRAN (R 3.6.2)
 yaml          2.2.1   2020-02-01 [1] CRAN (R 3.6.2)
 zip           2.0.4   2019-09-01 [1] CRAN (R 3.6.3)

[1] C:/Users/an499583/Documents/R/win-library/3.6
[2] C:/Program Files/R/R-3.6.2/library
```
