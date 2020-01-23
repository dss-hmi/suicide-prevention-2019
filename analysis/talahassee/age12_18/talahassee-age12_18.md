---
title: "Youth Suicide in Florida"
author: "Andriy Koval"
date: "2019-11-21"
output: 
  html_document:
    theme: simplex
    toc: true
    toc_depth: 3
    keep_md: true
    toc_float: true
---

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->


<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 

```
Warning: package 'ggplot2' was built under R version 3.5.3
```

```
Warning: package 'magrittr' was built under R version 3.5.3
```

```
Warning: package 'dplyr' was built under R version 3.5.3
```

```
Warning: package 'kableExtra' was built under R version 3.5.3
```

<!-- Load the sources.  Suppress the output when loading sources. --> 



# I. Exposition

## Glossary
Review object definitions to assist you in reading the report. 
<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 

```r
color_sex <- c(
  "Male" = "blue"
  ,"Female" = "pink"
  ,"Total"  = "black"
)

color_cause <- c(
  "Firearms" = "salmon"
  ,"Other"   = "green"
  ,"Total"   = "black"
  
)

color_race <- c(
  "black"    = "grey"
  ,"blother" = "lightblue"
  ,"latino" = "yellow"
  ,"white" = "purple"
)

# utility-functions -------------------------------------------------------

quick_save <- function(g,name,...){
  ggplot2::ggsave(
    filename = paste0(name,".png"), 
    plot     = g,
    device   = png,
    path     = "./analysis/talahassee/age12_18/prints/", # female marital educ poor_healt
    # width    = width,
    # height   = height,
    # units = "cm",
    # dpi      = 50,
    limitsize = FALSE,
    ...
  )
}
```

Review functions definitions to assist you in reading the report.


<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


## Load Data
<!-- Load the datasets.   -->

```r
ls_ds_wide <- readRDS("./data-unshared/derived/talahassee/12_18/ls_ds_wide.rds")
ls_ds_long <- readRDS("./data-unshared/derived/talahassee/12_18/ls_ds_long.rds")
ls_ds_long2 <- readRDS("./data-unshared/derived/talahassee/12_18/ls_ds_long2.rds")
ls_ds_long3 <- readRDS("./data-unshared/derived/talahassee/12_18/ls_ds_long3.rds")

ds_sex_cause      <- ls_ds_long3$cause_sex
ds_sex_cause_race <- ls_ds_long3$cause_sex_race
ds_cause_sex      <- ls_ds_long3$sex_cause
ds_cause_sex_race <- ls_ds_long3$sex_cause_race
```

<!-- Inspect the datasets.   -->


## Data Tweaks
<!-- Tweak the datasets.   -->




```r
ds_sex %>% glimpse()
```

```
Observations: 540
Variables: 6
$ race            <chr> "allrace", "allrace", "allrace", "allrace", "allrace", "allrace", "allrace", "allrace", "al...
$ mortality_cause <chr> "Firearms", "Firearms", "Firearms", "Firearms", "Firearms", "Firearms", "Firearms", "Firear...
$ sex             <chr> "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "...
$ year            <int> 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2...
$ measure         <fct> rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, r...
$ value           <dbl> 0.1, 0.5, 0.4, 0.1, 0.5, 0.5, 0.9, 0.2, 0.5, 0.5, 1.4, 0.5, 1.4, 0.7, 0.6, 1.6, 3.6, 3.2, 2...
```

```r
ds_sex_race %>% glimpse()
```

```
Observations: 2,160
Variables: 6
$ race            <chr> "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "...
$ mortality_cause <chr> "Firearms", "Firearms", "Firearms", "Firearms", "Firearms", "Firearms", "Firearms", "Firear...
$ sex             <chr> "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "...
$ year            <int> 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2...
$ measure         <fct> rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, rate, r...
$ value           <dbl> 0.0, 0.0, 0.0, 0.0, 0.6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.6, 0.0, 0.6, 0.0, 0.6, 0.0, 3.0, 2.4, 2...
```

```r
# g0 <- ds_sex %>%
#   ggplot(aes(x = year, y = value, color = sex) )+
#   geom_point()+
#   geom_line(aes(group=sex))+
#   # facet_grid(measure ~ ., scales = "free")+
#   facet_grid(measure ~ mortality_cause, scales = "free")+
#   theme_bw()
# 
# g0 %>% print()
# 
# g0 <- ls_ds_long4$sex_cause %>% 
#   ggplot(aes(x = year, y = value, color = mortality_cause) )+
#   geom_point()+
#   geom_line(aes(group=mortality_cause))+
#   facet_grid(measure ~ sex, scales = "free")+
#   theme_bw()
# 
# g0 %>% print()


# Sonata form report structure
```




```r
ds_sex_cause %>% glimpse()
```

```
Observations: 105
Variables: 9
$ order            <chr> "cause_sex", "cause_sex", "cause_sex", "cause_sex", "cause_sex", "cause_sex", "cause_sex",...
$ race             <chr> "allrace", "allrace", "allrace", "allrace", "allrace", "allrace", "allrace", "allrace", "a...
$ mortality_cause  <chr> "Firearms", "Firearms", "Firearms", "Firearms", "Firearms", "Firearms", "Firearms", "Firea...
$ sex              <chr> "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", ...
$ year             <int> 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, ...
$ count            <dbl> 1, 4, 3, 1, 4, 4, 7, 2, 4, 4, 11, 4, 11, 6, 5, 13, 30, 27, 18, 16, 29, 15, 21, 33, 25, 30,...
$ rate             <dbl> 0.1, 0.5, 0.4, 0.1, 0.5, 0.5, 0.9, 0.2, 0.5, 0.5, 1.4, 0.5, 1.4, 0.7, 0.6, 1.6, 3.6, 3.2, ...
$ pct_change_count <dbl> NA, NA, NA, NA, NA, NA, -28.57143, NA, NA, NA, -54.54545, NA, -54.54545, -16.66667, 0.0000...
$ pct_change_rate  <dbl> NA, NA, NA, NA, NA, NA, -33.333333, NA, NA, NA, -57.142857, NA, -57.142857, -14.285714, 0....
```

# II. Without Race

## A1 - Sex-Cause

```r
g1 <- ds_sex %>% 
  ggplot(aes(x = year, y = value, color = sex) )+
  geom_point()+
  geom_line(aes(group=sex))+
  facet_grid(measure ~ mortality_cause, scales = "free")+
  theme_bw()+
  labs(
    title = "Gender differences within each mortality cause"
  )
g1 %>% print()
```

```
Warning: Removed 20 rows containing missing values (geom_point).
```

<img src="figure_rmd/allrace-sex-cause-1.png" width="700px" />

## A2 - Cause-Sex

```r
g2 <- ds_sex %>% 
  ggplot(aes(x = year, y = value, color = mortality_cause) )+
  geom_point()+
  geom_line(aes(group=mortality_cause))+
  facet_grid(measure ~ sex, scales = "free")+
  theme_bw()+
  labs(
    title = "Mortality cause within each gender"
  )

g2%>% print()
```

```
Warning: Removed 20 rows containing missing values (geom_point).
```

<img src="figure_rmd/allrace-cause-sex-1.png" width="700px" />



# III. With Race

## B1 - Race-Sex-Cause

```r
for(i in names(color_race) ){
  g1 <- ds_sex_race %>%
    dplyr::filter(race == i) %>%
    ggplot(aes(x = year, y = value, color = sex) )+
    geom_point()+
    geom_line(aes(group=sex))+
    facet_grid(measure ~ mortality_cause, scales = "free")+
    theme_bw()+
    labs(
      title = paste0(toupper(i),": Gender differences within each mortality cause")
    )
  g1 %>% print()

}
```

```
Warning: Removed 180 rows containing missing values (geom_point).
```

```
Warning: Removed 2 rows containing missing values (geom_path).
```

<img src="figure_rmd/race-sex-cause-1.png" width="700px" />

```
Warning: Removed 136 rows containing missing values (geom_point).
```

<img src="figure_rmd/race-sex-cause-2.png" width="700px" />

```
Warning: Removed 148 rows containing missing values (geom_point).
```

<img src="figure_rmd/race-sex-cause-3.png" width="700px" />

```
Warning: Removed 26 rows containing missing values (geom_point).
```

<img src="figure_rmd/race-sex-cause-4.png" width="700px" />

## B2 - Race-Cause-Sex

```
Warning: Removed 180 rows containing missing values (geom_point).
```

<img src="figure_rmd/race-cause-sex-1.png" width="700px" />

```
Warning: Removed 136 rows containing missing values (geom_point).
```

<img src="figure_rmd/race-cause-sex-2.png" width="700px" />

```
Warning: Removed 148 rows containing missing values (geom_point).
```

<img src="figure_rmd/race-cause-sex-3.png" width="700px" />

```
Warning: Removed 26 rows containing missing values (geom_point).
```

<img src="figure_rmd/race-cause-sex-4.png" width="700px" />


## C1 - Sex-Cause-Race

```r
for(i in names(color_sex) ){
  g1 <- ds_sex_race %>% 
    dplyr::filter(sex == i) %>% 
    ggplot(aes(x = year, y = value, color = mortality_cause) )+
    geom_point()+
    geom_line(aes(group=mortality_cause))+
    facet_grid(measure ~ race, scales = "free")+
    theme_bw()+
    labs(
      title =  paste0("Sex = ",toupper(i),": Mortality causes for each race")
    )
  g1 %>% print()
}
```

```
Warning: Removed 130 rows containing missing values (geom_point).
```

<img src="figure_rmd/sex-cause-race-1.png" width="700px" />

```
Warning: Removed 274 rows containing missing values (geom_point).
```

```
Warning: Removed 2 rows containing missing values (geom_path).
```

<img src="figure_rmd/sex-cause-race-2.png" width="700px" />

```
Warning: Removed 86 rows containing missing values (geom_point).
```

<img src="figure_rmd/sex-cause-race-3.png" width="700px" />

## C2 - Sex-Race-Cause

```r
for(i in names(color_sex) ){
  g1 <- ds_sex_race %>% 
    dplyr::filter(sex == i) %>% 
    ggplot(aes(x = year, y = value, color = race) )+
    geom_point()+
    geom_line(aes(group=race))+
    facet_grid(measure ~ mortality_cause , scales = "free")+
    theme_bw()+
    labs(
      title = paste0("Sex = ",toupper(i),": Race differences for each mortality cause")
    )
  g1 %>% print()
}
```

```
Warning: Removed 130 rows containing missing values (geom_point).
```

<img src="figure_rmd/sex-race-cause-1.png" width="700px" />

```
Warning: Removed 274 rows containing missing values (geom_point).
```

```
Warning: Removed 2 rows containing missing values (geom_path).
```

<img src="figure_rmd/sex-race-cause-2.png" width="700px" />

```
Warning: Removed 86 rows containing missing values (geom_point).
```

<img src="figure_rmd/sex-race-cause-3.png" width="700px" />


## D1 - Cause-Sex-Race

```r
for( i in names(color_cause) ){
  g1 <- ds_sex_race %>% 
    dplyr::filter(mortality_cause == i) %>% 
    ggplot(aes(x = year, y = value, color = sex) )+
    geom_point()+
    geom_line(aes(group=sex))+
    facet_grid(measure ~ race, scales = "free")+
    theme_bw()+
    labs(
      title = paste0("Cause = ",toupper(i),": Gender differences in each race")
    )
  g1 %>% print()
}
```

```
Warning: Removed 234 rows containing missing values (geom_point).
```

```
Warning: Removed 2 rows containing missing values (geom_path).
```

<img src="figure_rmd/cause-sex-race-1.png" width="700px" />

```
Warning: Removed 158 rows containing missing values (geom_point).
```

<img src="figure_rmd/cause-sex-race-2.png" width="700px" />

```
Warning: Removed 98 rows containing missing values (geom_point).
```

<img src="figure_rmd/cause-sex-race-3.png" width="700px" />

## D2 - Cause-Race-Sex

```r
for( i in names(color_cause) ){
  g1 <- ds_sex_race %>% 
    dplyr::filter(mortality_cause == i) %>% 
    ggplot(aes(x = year, y = value, color = race) )+
    geom_point()+
    geom_line(aes(group=race))+
    facet_grid(measure ~ sex, scales = "free")+
    theme_bw()+
    labs(
      title =  paste0("Cause = ",toupper(i),": Race differences for each gender")
    )
  g1 %>% print()
}
```

```
Warning: Removed 234 rows containing missing values (geom_point).
```

<img src="figure_rmd/cause-race-sex-1.png" width="700px" />

```
Warning: Removed 158 rows containing missing values (geom_point).
```

<img src="figure_rmd/cause-race-sex-2.png" width="700px" />

```
Warning: Removed 98 rows containing missing values (geom_point).
```

<img src="figure_rmd/cause-race-sex-3.png" width="700px" />

# IV. Recap














```
Report rendered by an499583 at 2019-11-21, 10:14 -0500
```

```
R version 3.5.2 (2018-12-20)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17763)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] RColorBrewer_1.1-2 dichromat_2.0-0    kableExtra_1.1.0   dplyr_0.8.3        magrittr_1.5       ggplot2_3.1.1     
[7] knitr_1.26        

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.3        pillar_1.4.2      compiler_3.5.2    plyr_1.8.4        tools_3.5.2       zeallot_0.1.0    
 [7] digest_0.6.22     lifecycle_0.1.0   viridisLite_0.3.0 evaluate_0.14     tibble_2.1.3      gtable_0.3.0     
[13] pkgconfig_2.0.3   rlang_0.4.1       cli_1.1.0         rstudioapi_0.10   yaml_2.2.0        xfun_0.11        
[19] xml2_1.2.2        httr_1.4.1        withr_2.1.2       stringr_1.4.0     vctrs_0.2.0       hms_0.5.2        
[25] webshot_0.5.1     tidyselect_0.2.5  glue_1.3.1        R6_2.4.1          fansi_0.4.0       rmarkdown_1.17   
[31] reshape2_1.4.3    tidyr_1.0.0       purrr_0.3.3       readr_1.3.1       ellipsis_0.3.0    scales_1.0.0     
[37] backports_1.1.5   htmltools_0.4.0   rvest_0.3.5       assertthat_0.2.1  colorspace_1.4-1  labeling_0.3     
[43] utf8_1.1.4        stringi_1.4.3     lazyeval_0.2.2    munsell_0.5.0     crayon_1.3.4     
```
