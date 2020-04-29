---
title: "Florida Suicide (1)"
author: "Andriy Koval"
date: "April 26, 2020"
output:
  html_document:
    theme: simplex
    toc: true
    toc_depth: 3
    keep_md: true
    toc_float: true
    code_folding: hide
---

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->

# Abstract

This blogpost explores suicides trends in Florida between 2006 and 2017, examining the effects of age and gender. The post answers the following questions:  

> Q1 - What is the overall trajectory of suicides in FL between 2006 and 2017? 

> Q2 - How does change in suicide mortality differ by age? 

> Q3 - How do suicide trends differ by gender? 

> Q4 - How do suicide trends differ by gender and age group?

> Q5 - How do suicide trend differ by race?

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
 ```
 

```{.r .numberLines}
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
```

# Data  

The data comes from Florida Health Charts and contains suicide mortality between 2006 and 2017, broken down by suicide means, county, sex, age, and race. The the dataset is [available for download here](https://github.com/dss-hmi/suicide-prevention-2019/blob/b225979bad5e4728d7a594fd455ab929d3ccafb0/data-public/derived/9-population-suicide.csv).  


```{.r .numberLines}
# data prepared by "./manipulation/9-aggregator.R" combining population estimates and suicide counts
ds_population_suicide <-   readr::read_csv(path_file_input)

d <- readr::read_csv(path_file_input)
```


## Data Tweaks
This dataset already comes  well-gromed, so only some minor housekeeping tweaks are necessary to make graphing more convenient. 

```{.r .numberLines}
ds0 <- ds_population_suicide %>%
  dplyr::mutate(
    year            = as.integer(year)
    ,sex            = factor(sex, levels = c("Male", "Female"))
    ,race_ethnicity = factor(paste0(race, " + ", ethnicity))
    ,race           = factor(race)
    ,ethnicity      = factor(ethnicity)
    ,age_group      = factor(age_group, levels = names(lvl_age_groups), labels = lvl_age_groups)
    ,n_population   = as.integer(n_population)
    ,n_suicides     = as.integer(n_suicides)
  ) 
ds0 %>% dplyr::glimpse(70)
```

```
Rows: 83,616
Columns: 16
$ county                                <chr> "Alachua", "Alachua...
$ year                                  <int> 2006, 2006, 2006, 2...
$ sex                                   <fct> Female, Female, Fem...
$ race                                  <fct> Black & Other, Blac...
$ ethnicity                             <fct> Hispanic, Hispanic,...
$ age_group                             <fct> 1-4, 10-14, 15-19, ...
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

The unit of analysis of this dataset is defined by the first 6 variables: `county`, `year`, `sex`, `race`, `ethnicity`, `age_group`. For each unit, there are two measures: the number of people (`n_population`) and the number of observed suicide events (`n_suicides`). The latter is broken down by 7 means of suicide. For convenience, we create a combined variable `race_ethnicity`.


```{.r .numberLines}
ds0 %>% group_by(race, ethnicity, race_ethnicity) %>% count() %>% select(-n)
```

```
# A tibble: 4 x 3
# Groups:   race, ethnicity, race_ethnicity [4]
  race          ethnicity    race_ethnicity              
  <fct>         <fct>        <fct>                       
1 Black & Other Hispanic     Black & Other + Hispanic    
2 Black & Other Non-Hispanic Black & Other + Non-Hispanic
3 White         Hispanic     White + Hispanic            
4 White         Non-Hispanic White + Non-Hispanic        
```

# Q1 - Overall Trend

To begin, let us answer the most fundamental question: 

> What is the overall trajectory of suicides in FL between 2006 and 2017? 


```{.r .numberLines}
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
```

<img src="figure_rmd/g11-1.png" width="900px" />

We exclude persons younger than `10` from our analysis. For the ages `<10` suicide is extremely rare and often calls into question the classification of the underlying cause of death (e.g. can a 4-year-old really _intend_ to kill oneself?). Here is the frequency of suicides for each age group: 


```{.r .numberLines}
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
  )
```

<img src="figure_rmd/g12-1.png" width="900px" />
As we see, there are only `4` suicide events for this age group. We remove them and redraw the graph with the same scale across the facets for a better comparison among age groups.


```{.r .numberLines}
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
  )
```

<img src="figure_rmd/g13-1.png" width="900px" />


# Q2 - Age

> How does change in suicide mortality differ by age? 

Note that age groups do not have the same definition: ages younger than `25` are grouped into 5-year bins, while older ages are grouped into 10-year bins. This makes it difficult to compare absolute counts across age groups. Also, we must account for the fact that demographic growth may be uneven across the groups, which it is:


```{.r .numberLines}
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
```

<img src="figure_rmd/g21-1.png" width="900px" />
Notice that the age groups `10_14` and `15_19` have been ___declining___ by an average of `2,680` and `2,970` respectively, while `20_24` age group have been ___increasing___ by an average of `12,500` persons per year. 

To compare the change in suicide mortality among different age groups, we compute an age-adjusted rate measure:


```{.r .numberLines}
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
    ,parse = TRUE, vjust = 3
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
    ,y = "Rate per 100,000"
    
  )
g22
```

<img src="figure_rmd/g22-1.png" width="900px" />

# Q3 - Sex 

> How do suicide trends differ by gender? 

Existing research suggests that suicide prevalence will differ substantially between the genders and we do observe it in Florida data.


```{.r .numberLines}
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
```

<img src="figure_rmd/g31-1.png" width="900px" />

# Q4 - Sex and Age

> How do suicide trends differ by gender and age group?



```{.r .numberLines}
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
    ,y = "Rate per 100,000", x = "Year"
    
  )
g41+
    ggpmisc::stat_poly_eq(
    formula = y ~ + x 
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    ,vjust = 7
  )
```

<img src="figure_rmd/g41-1.png" width="900px" />

To help us compare the magnitute of suicide rate across age group, we redraw the graph with the same scale across the facets:


```{.r .numberLines}
g41 + 
  facet_wrap(~age_group)+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x 
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    ,vjust = 3
  ) 
```

<img src="figure_rmd/g42-1.png" width="900px" />

# Q5 - Race + Ethnicity

> How do suicide trend differ by race?

The data stored race and ethnicity into two separate variables, which we have combined to have well-populated groups

```{.r .numberLines}
ds0 %>% 
  dplyr::filter(year == 2017) %>% 
  group_by(race, ethnicity, race_ethnicity) %>%
  summarize(population_in_2017 = sum(n_population)) 
```

```
# A tibble: 4 x 4
# Groups:   race, ethnicity [4]
  race          ethnicity    race_ethnicity               population_in_2017
  <fct>         <fct>        <fct>                                     <int>
1 Black & Other Hispanic     Black & Other + Hispanic                 445241
2 Black & Other Non-Hispanic Black & Other + Non-Hispanic            4165780
3 White         Hispanic     White + Hispanic                        4690613
4 White         Non-Hispanic White + Non-Hispanic                   11254094
```
The differences in group size, combined with the rarity of suicide events make it difficult to compare the trends across groups on the same scale. 


```{.r .numberLines}
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
      y = metric_filter, x = ""
    )
}
g51 <- d51 %>% plot_51("Population Count (million)")
g52 <- d51 %>% plot_51("Suicide Rate (per 100k)")
g53 <- d51 %>% plot_51("Suicide Count")
ggpubr::ggarrange(g51, g52, g53, labels = c("Population","Rate","Count"), ncol =1, nrow = 3)
```

<img src="figure_rmd/g51-1.png" width="900px" />

When we start breaking down the groups with more categories (`age_group`, `sex`), this problem gets worse. Therefore the plots exploring the relationships between age, sex, and race are created for each of the race category individually, to avoid comparing across drastically different scales.  The graphs below are not ideal: while allowing to explore pattern within each race group they fail to provide a direct comparison among them. 

```{.r .numberLines}
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
      x = "", y = ""
    )+
    theme(legend.position = "bottom")
}
g521 <- d52 %>% plot_g52("White + Non-Hispanic")
g522 <- d52 %>% plot_g52("White + Hispanic")
g523 <- d52 %>% plot_g52("Black & Other + Non-Hispanic")
g524 <- d52 %>% plot_g52("Black & Other + Hispanic")

cat("\n## White + Non-Hispanic\n");g521
```


## White + Non-Hispanic
<img src="figure_rmd/g52-1.png" width="1200px" />

```{.r .numberLines}
cat("\n## White + Hispanic\n"); g522
```


## White + Hispanic
<img src="figure_rmd/g52-2.png" width="1200px" />

```{.r .numberLines}
cat("\n## Black & Other + Non-Hispanic\n"); g523
```


## Black & Other + Non-Hispanic
<img src="figure_rmd/g52-3.png" width="1200px" />

```{.r .numberLines}
cat("\n## Black & Other + Hispanic\n"); g524
```


## Black & Other + Hispanic
<img src="figure_rmd/g52-4.png" width="1200px" />
session information
===========================================================================

For the sake of documentation and reproducibility, the current report was rendered in the following environment.  Click the line below to expand.

<details>
  <summary>Environment <span class="glyphicon glyphicon-plus-sign"></span></summary>

```
- Session info -------------------------------------------------------------------------------------------------------
 setting  value                       
 version  R version 3.6.3 (2020-02-29)
 os       Windows 10 x64              
 system   x86_64, mingw32             
 ui       RTerm                       
 language (EN)                        
 collate  English_United States.1252  
 ctype    English_United States.1252  
 tz       America/New_York            
 date     2020-04-29                  

- Packages -----------------------------------------------------------------------------------------------------------
 package     * version date       lib source        
 abind         1.4-5   2016-07-21 [1] CRAN (R 3.6.0)
 assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.2)
 backports     1.1.5   2019-10-02 [1] CRAN (R 3.6.1)
 callr         3.4.3   2020-03-28 [1] CRAN (R 3.6.3)
 car           3.0-7   2020-03-11 [1] CRAN (R 3.6.3)
 carData       3.0-3   2019-11-16 [1] CRAN (R 3.6.1)
 cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.6.2)
 cli           2.0.2   2020-02-28 [1] CRAN (R 3.6.3)
 codetools     0.2-16  2018-12-24 [2] CRAN (R 3.6.3)
 colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.6.1)
 cowplot       1.0.0   2019-07-11 [1] CRAN (R 3.6.2)
 crayon        1.3.4   2017-09-16 [1] CRAN (R 3.6.2)
 curl          4.3     2019-12-02 [1] CRAN (R 3.6.2)
 data.table    1.12.8  2019-12-09 [1] CRAN (R 3.6.2)
 desc          1.2.0   2018-05-01 [1] CRAN (R 3.6.2)
 devtools      2.3.0   2020-04-10 [1] CRAN (R 3.6.3)
 digest        0.6.25  2020-02-23 [1] CRAN (R 3.6.3)
 dplyr       * 0.8.5   2020-03-07 [1] CRAN (R 3.6.3)
 ellipsis      0.3.0   2019-09-20 [1] CRAN (R 3.6.2)
 evaluate      0.14    2019-05-28 [1] CRAN (R 3.6.2)
 fansi         0.4.1   2020-01-08 [1] CRAN (R 3.6.2)
 farver        2.0.3   2020-01-16 [1] CRAN (R 3.6.2)
 forcats       0.4.0   2019-02-17 [1] CRAN (R 3.6.2)
 foreign       0.8-75  2020-01-20 [2] CRAN (R 3.6.3)
 fs            1.3.1   2019-05-06 [1] CRAN (R 3.6.2)
 ggplot2     * 3.2.1   2019-08-10 [1] CRAN (R 3.6.2)
 ggpmisc       0.3.3   2019-12-01 [1] CRAN (R 3.6.3)
 ggpubr        0.2.5   2020-02-13 [1] CRAN (R 3.6.2)
 ggsignif      0.6.0   2019-08-08 [1] CRAN (R 3.6.2)
 glue          1.4.0   2020-04-03 [1] CRAN (R 3.6.3)
 gtable        0.3.0   2019-03-25 [1] CRAN (R 3.6.2)
 haven         2.2.0   2019-11-08 [1] CRAN (R 3.6.2)
 hms           0.5.3   2020-01-08 [1] CRAN (R 3.6.2)
 htmltools     0.4.0   2019-10-04 [1] CRAN (R 3.6.2)
 knitr       * 1.28    2020-02-06 [1] CRAN (R 3.6.2)
 labeling      0.3     2014-08-23 [1] CRAN (R 3.6.0)
 lazyeval      0.2.2   2019-03-15 [1] CRAN (R 3.6.2)
 lifecycle     0.2.0   2020-03-06 [1] CRAN (R 3.6.3)
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
 purrr         0.3.4   2020-04-17 [1] CRAN (R 3.6.3)
 R6            2.4.1   2019-11-12 [1] CRAN (R 3.6.2)
 Rcpp          1.0.4.6 2020-04-09 [1] CRAN (R 3.6.3)
 readr         1.3.1   2018-12-21 [1] CRAN (R 3.6.2)
 readxl        1.3.1   2019-03-13 [1] CRAN (R 3.6.2)
 remotes       2.1.1   2020-02-15 [1] CRAN (R 3.6.2)
 reshape2      1.4.3   2017-12-11 [1] CRAN (R 3.6.2)
 rio           0.5.16  2018-11-26 [1] CRAN (R 3.6.3)
 rlang         0.4.5   2020-03-01 [1] CRAN (R 3.6.3)
 rmarkdown     2.1     2020-01-20 [1] CRAN (R 3.6.2)
 rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.6.2)
 scales        1.1.0   2019-11-18 [1] CRAN (R 3.6.2)
 sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.6.2)
 stringi       1.4.6   2020-02-17 [1] CRAN (R 3.6.2)
 stringr       1.4.0   2019-02-10 [1] CRAN (R 3.6.2)
 testthat      2.3.2   2020-03-02 [1] CRAN (R 3.6.3)
 tibble        3.0.1   2020-04-20 [1] CRAN (R 3.6.3)
 tidyr         1.0.2   2020-01-24 [1] CRAN (R 3.6.2)
 tidyselect    1.0.0   2020-01-27 [1] CRAN (R 3.6.2)
 usethis       1.6.0   2020-04-09 [1] CRAN (R 3.6.3)
 utf8          1.1.4   2018-05-24 [1] CRAN (R 3.6.2)
 vctrs         0.2.4   2020-03-10 [1] CRAN (R 3.6.3)
 viridisLite   0.3.0   2018-02-01 [1] CRAN (R 3.6.2)
 withr         2.1.2   2018-03-15 [1] CRAN (R 3.6.2)
 xfun          0.12    2020-01-13 [1] CRAN (R 3.6.2)
 yaml          2.2.1   2020-02-01 [1] CRAN (R 3.6.2)
 zip           2.0.4   2019-09-01 [1] CRAN (R 3.6.3)

[1] C:/Users/an499583/Documents/R/win-library/3.6
[2] C:/Users/an499583/Documents/R/R-3.6.3/library
```
