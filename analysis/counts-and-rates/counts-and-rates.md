---
title: "Counts + Rates"
output:
  html_document:
    always_allow_html: yes
    df_print: kable
    highlight: tango
    keep_md: yes
    table: kable
    theme: spacelab
    toc: yes
    toc_float:
      collapsed: yes
      smooth_scroll: yes
  word_document:
    toc: yes
---

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->


This reports stores the drafts of the visualizations generated from the combined data set produced by `./manipulation/4-combiner.R` script.


# Set the scene 

Describe the working environment. 
<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 

```r
# Attach these packages so their functions don't need to be qualified
library(magrittr) # pipes
library(dplyr)    # disable when temp lines are removed
library(ggplot2)  # graphs
library(ggpubr)   # documents
```


<!-- Load the sources.  Suppress the output when loading sources. --> 

```r
# Call `base::source()` on any repo file that defines functions needed below. 
base::source("./scripts/common-functions.R")
```


<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 

```r
path_file_input <- "./data-unshared/derived/9-combined.rds"
html_flip <- FALSE
baseSize <- 10


age_group_order <- c(
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
```

<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 

```r
#Put code in here.  It doesn't call a chunk in the codebehind file.
```

<!-- Load the datasets.   -->

```r
dto      <- readRDS(path_file_input)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()
```

```
16.1 MB
```

```
[1] "list"
```

```
[1] "granularity_gls"        "granularity_population"
```

<!-- Inspect the datasets.   -->


<!-- Tweak the datasets.   -->

```r
# to collapse into a single data frame
ds <- dto[["granularity_population"]] %>% 
  Reduce(function(a , b) dplyr::left_join( a, b ), . )

ds %>% explore::describe_all()
```

<div class="kable-table">

variable           type        na   na_pct   unique   min      mean      max
-----------------  -----  -------  -------  -------  ----  --------  -------
county             chr          0      0.0       67    NA        NA       NA
year               chr          0      0.0       17    NA        NA       NA
sex                chr          0      0.0        2    NA        NA       NA
race               chr          0      0.0        2    NA        NA       NA
ethnicity          chr          0      0.0        3    NA        NA       NA
age_group          chr          0      0.0       13    NA        NA       NA
population_count   dou          0      0.0    14511     0   2926.53   144791
peer_group         dou       1612      1.5       25     1     32.85       81
resident_deaths    dou      97718     90.5       40     1      3.30       41
region             chr     101972     94.4        4    NA        NA       NA
community          dou     104156     96.4       35     8    491.19     3764
professionals      dou     102388     94.8       45     4     95.89     1161
county_gls         log     101972     94.4        2     1      1.00        1

</div>

```r
# to help us filter out those counties that had programming
counties_gls <- ds %>% 
  distinct(county,region) %>% # those who have region had programming
  na.omit() %>%
  dplyr::distinct(county) %>% 
  as.list() %>% unlist() %>% as.character()


# to view the total programming delivered (between 2015 and 2017)
ds %>% 
  dplyr::filter(county %in% counties_gls) %>% 
  dplyr::distinct(county, year, community, professionals ) %>% 
  # na.omit() %>% 
  dplyr::group_by(county) %>% 
  dplyr::summarize(
    community      = sum(community, na.rm= T)
    ,professionals = sum(professionals, na.rm= T)
  ) %>% 
  dplyr::arrange(desc(professionals))
```

<div class="kable-table">

county          community   professionals
-------------  ----------  --------------
Orange               8182            1605
Saint Lucie          1068             830
Volusia              2170             432
Seminole             2400             419
Lake                  206             290
Palm Beach            935             241
Duval                   0             235
Saint Johns           250             210
Brevard              1126             166
Hernando              115             131
Marion                325             119
Indian River          245             118
Osceola                35              84
Sumter                 85              81
Clay                    0              53
Martin                535              46
Citrus                  0              38
Flagler               397              29
Okeechobee              0              21
Putnam                  0              13
Polk                    0              11
Pasco                   0               6
Leon                  100               0

</div>

```r
ds <- ds %>% 
  dplyr::rename(
    "deaths_by_suicide" = "resident_deaths" # to remind what we count
  ) %>% 
  dplyr::mutate(
    # to have a standardized measure / put counties on the same scale
    suicide_rate_per100k = (deaths_by_suicide / population_count) *100000
    # to have a single variable describing racial background
    ,racethnicity = paste0(race," + ", ethnicity)
    # to aid in graph production ( note the spaces at the end of "NE  ")
    ,rgn = car::recode(
      region,
      "
      'central'  ='CN'
      ;'southeast'='SE'
      ;'northeast'='NE  '
      "
    )
  ) %>% 
  
  dplyr::select(county, year, sex, age_group, race, ethnicity, racethnicity, # context
                region, rgn, # support for graphing and grouping
                population_count, deaths_by_suicide, suicide_rate_per100k, #measures
                community, professionals # treatment
  )

ds %>% explore::describe_all()
```

<div class="kable-table">

variable               type        na   na_pct   unique    min      mean         max
---------------------  -----  -------  -------  -------  -----  --------  ----------
county                 chr          0      0.0       67     NA        NA          NA
year                   chr          0      0.0       17     NA        NA          NA
sex                    chr          0      0.0        2     NA        NA          NA
age_group              chr          0      0.0       13     NA        NA          NA
race                   chr          0      0.0        2     NA        NA          NA
ethnicity              chr          0      0.0        3     NA        NA          NA
racethnicity           chr          0      0.0        6     NA        NA          NA
region                 chr     101972     94.4        4     NA        NA          NA
rgn                    chr     101972     94.4        4     NA        NA          NA
population_count       dou          0      0.0    14511   0.00   2926.53   144791.00
deaths_by_suicide      dou      97718     90.5       40   1.00      3.30       41.00
suicide_rate_per100k   dou      97718     90.5     8395   0.83     63.70     7142.86
community              dou     104156     96.4       35   8.00    491.19     3764.00
professionals          dou     102388     94.8       45   4.00     95.89     1161.00

</div>

```r
# to remind out how to aggregate 
# the most granular level includes (6):
# county, year, sex, age_group, race, ethnicity
d1 <- ds %>% 
  # apply filters to better understand the structure of the data
  # dplyr::filter(county     %in% c("Orange") ) %>%
  # dplyr::filter(year       %in% c("2015")   ) %>%
  # dplyr::filter(sex        %in% c("Male")   ) %>%
  # dplyr::filter(age_group  %in% c("15_19")  ) %>%
  # dplyr::filter(race       %in% c("White")  ) %>%
  # dplyr::filter(ethnicity %in% c("Non-Hispanic") )%>%
  # dplyr::filter(ethnicity %in% c("Hispanic","Non-Hispanic") )%>%
  dplyr::group_by(county, year, sex, age_group, race, ethnicity) %>% # no aggregation
  # to exemplify useful aggregates:
  # dplyr::group_by(county, year                               ) %>%
  # dplyr::group_by(county, year, sex                          ) %>%
  # dplyr::group_by(county, year, sex, age_group               ) %>%
  # dplyr::group_by(county, year, sex, racethnicity                  ) %>%
  dplyr::summarize(
    population_count   = sum(population_count,   na.rm = T)
    ,deaths_by_suicide = sum(deaths_by_suicide,  na.rm = T)
    ,professionals     = sum(professionals,      na.rm = T)
    ,community         = sum(community,          na.rm = T)
  ) 
# use the code for preparing data for custom graphs
# d1 %>% glimpse(60)
# d1 %>% explore::describe()
```


# I Exposition 



# II. Development



## Population Counts

### p0
<img src="figure_rmd/population-0-1.png" width="960px" /><img src="figure_rmd/population-0-2.png" width="960px" /><img src="figure_rmd/population-0-3.png" width="960px" />

### p1
<img src="figure_rmd/population-1-1.png" width="960px" />




### p2
<img src="figure_rmd/population-2-1.png" width="960px" />

### p3  
<img src="figure_rmd/population-3-1.png" width="960px" />

## Suicides Counts

### p4  
<img src="figure_rmd/population-4-1.png" width="960px" />

### p5  
<img src="figure_rmd/population-5-1.png" width="960px" />


## Suicide Rates 

### p6  
<img src="figure_rmd/population-6-1.png" width="960px" />

### p7  
<img src="figure_rmd/population-7-1.png" width="960px" />

# III. Recapitulation




  


Session Information {#session-info}
===========================================================================

For the sake of documentation and reproducibility, the current report was rendered in the following environment.  Click the line below to expand.

<details>
  <summary>Environment <span class="glyphicon glyphicon-plus-sign"></span></summary>

```
- Session info -------------------------------------------------------------------------------------------------------
 setting  value                       
 version  R version 3.5.2 (2018-12-20)
 os       Windows 10 x64              
 system   x86_64, mingw32             
 ui       RTerm                       
 language (EN)                        
 collate  English_United States.1252  
 ctype    English_United States.1252  
 tz       America/Los_Angeles         
 date     2019-07-08                  

- Packages -----------------------------------------------------------------------------------------------------------
 package      * version  date       lib source        
 abind          1.4-5    2016-07-21 [1] CRAN (R 3.5.2)
 acepack        1.4.1    2016-10-29 [1] CRAN (R 3.5.3)
 assertthat     0.2.1    2019-03-21 [1] CRAN (R 3.5.3)
 backports      1.1.4    2019-04-10 [1] CRAN (R 3.5.3)
 base64enc      0.1-3    2015-07-28 [1] CRAN (R 3.5.2)
 callr          3.2.0    2019-03-15 [1] CRAN (R 3.5.3)
 car            3.0-3    2019-05-27 [1] CRAN (R 3.5.3)
 carData        3.0-2    2018-09-30 [1] CRAN (R 3.5.2)
 cellranger     1.1.0    2016-07-27 [1] CRAN (R 3.5.3)
 checkmate      1.9.3    2019-05-03 [1] CRAN (R 3.5.3)
 cli            1.1.0    2019-03-19 [1] CRAN (R 3.5.3)
 cluster        2.0.9    2019-05-01 [1] CRAN (R 3.5.3)
 codetools      0.2-16   2018-12-24 [1] CRAN (R 3.5.2)
 colorspace     1.4-1    2019-03-18 [1] CRAN (R 3.5.3)
 crayon         1.3.4    2017-09-16 [1] CRAN (R 3.5.3)
 curl           3.3      2019-01-10 [1] CRAN (R 3.5.3)
 data.table     1.12.2   2019-04-07 [1] CRAN (R 3.5.3)
 desc           1.2.0    2018-05-01 [1] CRAN (R 3.5.3)
 devtools       2.0.2    2019-04-08 [1] CRAN (R 3.5.3)
 digest         0.6.19   2019-05-20 [1] CRAN (R 3.5.3)
 dplyr        * 0.8.1    2019-05-14 [1] CRAN (R 3.5.3)
 DT             0.6      2019-05-09 [1] CRAN (R 3.5.3)
 evaluate       0.14     2019-05-28 [1] CRAN (R 3.5.2)
 explore        0.4.2    2019-05-22 [1] CRAN (R 3.5.3)
 forcats        0.4.0    2019-02-17 [1] CRAN (R 3.5.3)
 foreign        0.8-71   2018-07-20 [2] CRAN (R 3.5.2)
 Formula        1.2-3    2018-05-03 [1] CRAN (R 3.5.2)
 fs             1.3.1    2019-05-06 [1] CRAN (R 3.5.3)
 ggplot2      * 3.1.1    2019-04-07 [1] CRAN (R 3.5.3)
 ggpubr       * 0.2      2018-11-15 [1] CRAN (R 3.5.3)
 glue           1.3.1    2019-03-12 [1] CRAN (R 3.5.3)
 gridExtra      2.3      2017-09-09 [1] CRAN (R 3.5.3)
 gtable         0.3.0    2019-03-25 [1] CRAN (R 3.5.3)
 haven          2.1.0    2019-02-19 [1] CRAN (R 3.5.3)
 highr          0.8      2019-03-20 [1] CRAN (R 3.5.3)
 Hmisc          4.2-0    2019-01-26 [1] CRAN (R 3.5.3)
 hms            0.4.2    2018-03-10 [1] CRAN (R 3.5.3)
 htmlTable      1.13.1   2019-01-07 [1] CRAN (R 3.5.3)
 htmltools      0.3.6    2017-04-28 [1] CRAN (R 3.5.3)
 htmlwidgets    1.3      2018-09-30 [1] CRAN (R 3.5.3)
 httpuv         1.5.1    2019-04-05 [1] CRAN (R 3.5.3)
 knitr        * 1.23     2019-05-18 [1] CRAN (R 3.5.2)
 labeling       0.3      2014-08-23 [1] CRAN (R 3.5.2)
 later          0.8.0    2019-02-11 [1] CRAN (R 3.5.3)
 lattice        0.20-38  2018-11-04 [2] CRAN (R 3.5.2)
 latticeExtra   0.6-28   2016-02-09 [1] CRAN (R 3.5.3)
 lazyeval       0.2.2    2019-03-15 [1] CRAN (R 3.5.3)
 magrittr     * 1.5      2014-11-22 [1] CRAN (R 3.5.3)
 Matrix         1.2-17   2019-03-22 [1] CRAN (R 3.5.3)
 memoise        1.1.0    2017-04-21 [1] CRAN (R 3.5.3)
 mime           0.6      2018-10-05 [1] CRAN (R 3.5.2)
 munsell        0.5.0    2018-06-12 [1] CRAN (R 3.5.3)
 nnet           7.3-12   2016-02-02 [2] CRAN (R 3.5.2)
 openxlsx       4.1.0.1  2019-05-28 [1] CRAN (R 3.5.3)
 pillar         1.4.1    2019-05-28 [1] CRAN (R 3.5.2)
 pkgbuild       1.0.3    2019-03-20 [1] CRAN (R 3.5.3)
 pkgconfig      2.0.2    2018-08-16 [1] CRAN (R 3.5.3)
 pkgload        1.0.2    2018-10-29 [1] CRAN (R 3.5.3)
 plyr           1.8.4    2016-06-08 [1] CRAN (R 3.5.3)
 prettyunits    1.0.2    2015-07-13 [1] CRAN (R 3.5.3)
 processx       3.3.1    2019-05-08 [1] CRAN (R 3.5.2)
 promises       1.0.1    2018-04-13 [1] CRAN (R 3.5.3)
 pryr           0.1.4    2018-02-18 [1] CRAN (R 3.5.3)
 ps             1.3.0    2018-12-21 [1] CRAN (R 3.5.3)
 purrr          0.3.2    2019-03-15 [1] CRAN (R 3.5.3)
 R6             2.4.0    2019-02-14 [1] CRAN (R 3.5.3)
 RColorBrewer   1.1-2    2014-12-07 [1] CRAN (R 3.5.2)
 Rcpp           1.0.1    2019-03-17 [1] CRAN (R 3.5.3)
 readxl         1.3.1    2019-03-13 [1] CRAN (R 3.5.3)
 remotes        2.0.4    2019-04-10 [1] CRAN (R 3.5.3)
 reshape2       1.4.3    2017-12-11 [1] CRAN (R 3.5.3)
 rio            0.5.16   2018-11-26 [1] CRAN (R 3.5.3)
 rlang          0.3.4    2019-04-07 [1] CRAN (R 3.5.3)
 rmarkdown      1.13     2019-05-22 [1] CRAN (R 3.5.3)
 rpart          4.1-15   2019-04-12 [1] CRAN (R 3.5.3)
 rprojroot      1.3-2    2018-01-03 [1] CRAN (R 3.5.3)
 rstudioapi     0.10     2019-03-19 [1] CRAN (R 3.5.3)
 scales         1.0.0    2018-08-09 [1] CRAN (R 3.5.3)
 sessioninfo    1.1.1    2018-11-05 [1] CRAN (R 3.5.3)
 shiny          1.3.2    2019-04-22 [1] CRAN (R 3.5.2)
 stringi        1.4.3    2019-03-12 [1] CRAN (R 3.5.3)
 stringr        1.4.0    2019-02-10 [1] CRAN (R 3.5.3)
 survival       2.44-1.1 2019-04-01 [1] CRAN (R 3.5.3)
 tibble         2.1.3    2019-06-06 [1] CRAN (R 3.5.3)
 tidyselect     0.2.5    2018-10-11 [1] CRAN (R 3.5.3)
 usethis        1.5.0    2019-04-07 [1] CRAN (R 3.5.3)
 viridisLite    0.3.0    2018-02-01 [1] CRAN (R 3.5.3)
 withr          2.1.2    2018-03-15 [1] CRAN (R 3.5.3)
 xfun           0.7      2019-05-14 [1] CRAN (R 3.5.3)
 xtable         1.8-4    2019-04-21 [1] CRAN (R 3.5.3)
 yaml           2.2.0    2018-07-25 [1] CRAN (R 3.5.2)
 zip            2.0.2    2019-05-13 [1] CRAN (R 3.5.3)

[1] C:/Users/an499583/Documents/R/win-library/3.5
[2] C:/Program Files/R/R-3.5.2/library
```
</details>



Report rendered by an499583 at 2019-07-08, 13:06 -0700 in 64 seconds.


