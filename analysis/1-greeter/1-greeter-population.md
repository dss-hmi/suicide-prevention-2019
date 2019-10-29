---
title: "1-greeter-population"
output:
  html_document:
    always_allow_html: yes
    css: ../../libs/css/sidebar.css
    df_print: kable
    highlight: tango
    keep_md: yes
    theme: spacelab
    toc: yes
    table: kable
---

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->


This document is the annotation layer to [`./manipulation/1-greeter-populaton.R`][path_greeter] script, which connects to data sources used in the study, imports them into RStudio environment and prepares them for analytic enquiry.

[path_greeter]:https://github.com/dss-hmi/suicide-prevention-2019/blob/master/manipulation/1-greeter-population.R

# I. Environment

<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 

```r
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr) # disable when temp lines are removed
library(ggplot2)
library(ggpubr)
library(readxl)
```


<!-- Load the sources.  Suppress the output when loading sources. --> 

```r
# Call `base::source()` on any repo file that defines functions needed below.  
base::source("./scripts/common-functions.R")
```


<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 

```r
# path_file_input       <- "./data-unshared/raw/FloridaPopulation/FloridaPopulation-small.xlsx"
path_file_input       <- "./data-unshared/raw/FloridaPopulation/FloridaPopulation-full.xlsx"
```

<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


# II. Input
<!-- Load the datasets.   -->

```r
#
ds0 <-  readxl::read_excel(path_file_input, col_names = FALSE, skip = 3) #%>% dplyr::slice(1:1000)
```

<!-- Inspect the datasets.   -->


# III. Groom
<!-- Tweak the datasets.   -->

```r
# names(ds0) <- c("county","year","sex","race","ethnicity","10_14","15_19", "20_24","total") # small
ds1 <- ds0
names(ds1) <- c(
  "county","year","sex","race","ethnicity",
  "less_than_1", "1_4","5_9","10_14","15_19", "20_24","25_34",
  "35_44","45_54","55_64","65_74","75_84","85_plus","total"
)

# function to fill last seen for a given column
fill_last_seen <- function(
  column
){
  # first value is non-empty
  last_seen = column[[1]]
  count = 1L
  #fill rest of the cells with first non empty value
    for(cell in column){
    if(is.na(cell)){
      cell            = last_seen
      column[[count]] = cell
    }
    else{
      last_seen = cell
    }
    count = count +1
  }
  return(column)
}

ds1 %>% head(20)
```

<div class="kable-table">

county    year   sex      race            ethnicity    less_than_1    1_4     5_9   10_14   15_19   20_24   25_34   35_44   45_54   55_64   65_74   75_84   85_plus    total
--------  -----  -------  --------------  ----------  ------------  -----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  --------  -------
Alachua   2001   Female   White           Unknown              726   2912    3650    3990    8874   14430   11185   10780   10723    6820    4875    3855      1497    84317
NA        NA     NA       NA              Total                726   2912    3650    3990    8874   14430   11185   10780   10723    6820    4875    3855      1497    84317
NA        NA     NA       Black & Other   Unknown              439   1688    2260    2355    3326    3805    4315    4045    3155    1825    1150     720       312    29395
NA        NA     NA       NA              Total                439   1688    2260    2355    3326    3805    4315    4045    3155    1825    1150     720       312    29395
NA        NA     NA       Total           NA                  1165   4600    5910    6345   12200   18235   15500   14825   13878    8645    6025    4575      1809   113712
NA        NA     Male     White           Unknown              803   2972    3875    4155    8262   14445   12750   10335   10180    6270    4005    2580       612    81244
NA        NA     NA       NA              Total                803   2972    3875    4155    8262   14445   12750   10335   10180    6270    4005    2580       612    81244
NA        NA     NA       Black & Other   Unknown              450   1732    2410    2365    3109    3510    4055    3510    2615    1485     890     425       131    26687
NA        NA     NA       NA              Total                450   1732    2410    2365    3109    3510    4055    3510    2615    1485     890     425       131    26687
NA        NA     NA       Total           NA                  1253   4704    6285    6520   11371   17955   16805   13845   12795    7755    4895    3005       743   107931
NA        NA     Total    NA              NA                  2418   9304   12195   12865   23571   36190   32305   28670   26673   16400   10920    7580      2552   221643
NA        2002   Female   White           Unknown              695   2972    3660    3935    8769   14615   11580   10625   10760    7095    4920    3905      1590    85121
NA        NA     NA       NA              Total                695   2972    3660    3935    8769   14615   11580   10625   10760    7095    4920    3905      1590    85121
NA        NA     NA       Black & Other   Unknown              436   1724    2265    2355    3413    3905    4370    4100    3310    1905    1190     740       334    30047
NA        NA     NA       NA              Total                436   1724    2265    2355    3413    3905    4370    4100    3310    1905    1190     740       334    30047
NA        NA     NA       Total           NA                  1131   4696    5925    6290   12182   18520   15950   14725   14070    9000    6110    4645      1924   115168
NA        NA     Male     White           Unknown              744   3044    3855    4105    8328   14425   12991   10305   10095    6575    4060    2610       667    81804
NA        NA     NA       NA              Total                744   3044    3855    4105    8328   14425   12991   10305   10095    6575    4060    2610       667    81804
NA        NA     NA       Black & Other   Unknown              463   1776    2415    2430    3275    3660    4115    3600    2770    1570     930     435       145    27584
NA        NA     NA       NA              Total                463   1776    2415    2430    3275    3660    4115    3600    2770    1570     930     435       145    27584

</div>

```r
ds2 <- ds1 %>%
  dplyr::mutate_all(fill_last_seen)
names(ds2) <- names(ds1) # because mutate_all messes up column names
ds2 %>% dplyr::glimpse(100)
```

```
Observations: 16,349
Variables: 19
$ county      <chr> "Alachua", "Alachua", "Alachua", "Alachua", "Alachua", "Alachua", "Alachua"...
$ year        <chr> "2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001", "20...
$ sex         <chr> "Female", "Female", "Female", "Female", "Female", "Male", "Male", "Male", "...
$ race        <chr> "White", "White", "Black & Other", "Black & Other", "Total", "White", "Whit...
$ ethnicity   <chr> "Unknown", "Total", "Unknown", "Total", "Total", "Unknown", "Total", "Unkno...
$ less_than_1 <dbl> 726, 726, 439, 439, 1165, 803, 803, 450, 450, 1253, 2418, 695, 695, 436, 43...
$ `1_4`       <dbl> 2912, 2912, 1688, 1688, 4600, 2972, 2972, 1732, 1732, 4704, 9304, 2972, 297...
$ `5_9`       <dbl> 3650, 3650, 2260, 2260, 5910, 3875, 3875, 2410, 2410, 6285, 12195, 3660, 36...
$ `10_14`     <dbl> 3990, 3990, 2355, 2355, 6345, 4155, 4155, 2365, 2365, 6520, 12865, 3935, 39...
$ `15_19`     <dbl> 8874, 8874, 3326, 3326, 12200, 8262, 8262, 3109, 3109, 11371, 23571, 8769, ...
$ `20_24`     <dbl> 14430, 14430, 3805, 3805, 18235, 14445, 14445, 3510, 3510, 17955, 36190, 14...
$ `25_34`     <dbl> 11185, 11185, 4315, 4315, 15500, 12750, 12750, 4055, 4055, 16805, 32305, 11...
$ `35_44`     <dbl> 10780, 10780, 4045, 4045, 14825, 10335, 10335, 3510, 3510, 13845, 28670, 10...
$ `45_54`     <dbl> 10723, 10723, 3155, 3155, 13878, 10180, 10180, 2615, 2615, 12795, 26673, 10...
$ `55_64`     <dbl> 6820, 6820, 1825, 1825, 8645, 6270, 6270, 1485, 1485, 7755, 16400, 7095, 70...
$ `65_74`     <dbl> 4875, 4875, 1150, 1150, 6025, 4005, 4005, 890, 890, 4895, 10920, 4920, 4920...
$ `75_84`     <dbl> 3855, 3855, 720, 720, 4575, 2580, 2580, 425, 425, 3005, 7580, 3905, 3905, 7...
$ `85_plus`   <dbl> 1497, 1497, 312, 312, 1809, 612, 612, 131, 131, 743, 2552, 1590, 1590, 334,...
$ total       <dbl> 84317, 84317, 29395, 29395, 113712, 81244, 81244, 26687, 26687, 107931, 221...
```

```r
var_stem <- c("county","year","sex","race","ethnicity")
ds2 <- ds2 %>% 
  tidyr::gather(
    "age_group","count", setdiff(names(ds1), var_stem)
  )

ds2 %>% dplyr::glimpse(100)
```

```
Observations: 228,886
Variables: 7
$ county    <chr> "Alachua", "Alachua", "Alachua", "Alachua", "Alachua", "Alachua", "Alachua", ...
$ year      <chr> "2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001...
$ sex       <chr> "Female", "Female", "Female", "Female", "Female", "Male", "Male", "Male", "Ma...
$ race      <chr> "White", "White", "Black & Other", "Black & Other", "Total", "White", "White"...
$ ethnicity <chr> "Unknown", "Total", "Unknown", "Total", "Total", "Unknown", "Total", "Unknown...
$ age_group <chr> "less_than_1", "less_than_1", "less_than_1", "less_than_1", "less_than_1", "l...
$ count     <dbl> 726, 726, 439, 439, 1165, 803, 803, 450, 450, 1253, 2418, 695, 695, 436, 436,...
```

```r
# because it will be easier to compute on site + demonstration of summary logic
ds3 <- ds2 %>% 
  dplyr::filter(! county    == "Total") %>% 
  dplyr::filter(! year      == "Total") %>% 
  dplyr::filter(! sex       == "Total") %>% 
  dplyr::filter(! race      == "Total") %>% 
  dplyr::filter(! ethnicity == "Total") %>% 
  dplyr::filter(! age_group == "total") %>% 
  dplyr::arrange(county, year, sex, race, ethnicity, age_group) 
```










# IV. Inspect









<!-- Basic table view.   -->


<!-- Basic graph view.   -->




# V. Save to Disk

Let us create a `data transfer object` (dto), a list object that would store all three data components in this project. This object (dto) will be the point of departure for all subsequent analytic efforst. 



```r
ds3 %>% pryr::object_size()
```

```
6.06 MB
```

```r
ds3 %>%          saveRDS("./data-unshared/derived/1-greeted-population.rds")
ds3 %>% readr::write_csv("./data-unshared/derived/1-greeted-population.csv") # for read-only inspection
```



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
 date     2019-06-26                  

- Packages -----------------------------------------------------------------------------------------------------------
 package     * version date       lib source        
 assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.5.3)
 backports     1.1.4   2019-04-10 [1] CRAN (R 3.5.3)
 callr         3.2.0   2019-03-15 [1] CRAN (R 3.5.3)
 cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.5.3)
 cli           1.1.0   2019-03-19 [1] CRAN (R 3.5.3)
 codetools     0.2-16  2018-12-24 [1] CRAN (R 3.5.2)
 colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.5.3)
 crayon        1.3.4   2017-09-16 [1] CRAN (R 3.5.3)
 desc          1.2.0   2018-05-01 [1] CRAN (R 3.5.3)
 devtools      2.0.2   2019-04-08 [1] CRAN (R 3.5.3)
 digest        0.6.19  2019-05-20 [1] CRAN (R 3.5.3)
 dplyr       * 0.8.1   2019-05-14 [1] CRAN (R 3.5.3)
 evaluate      0.14    2019-05-28 [1] CRAN (R 3.5.2)
 fansi         0.4.0   2018-10-05 [1] CRAN (R 3.5.3)
 fs            1.3.1   2019-05-06 [1] CRAN (R 3.5.3)
 ggplot2     * 3.1.1   2019-04-07 [1] CRAN (R 3.5.3)
 ggpubr      * 0.2     2018-11-15 [1] CRAN (R 3.5.3)
 glue          1.3.1   2019-03-12 [1] CRAN (R 3.5.3)
 gtable        0.3.0   2019-03-25 [1] CRAN (R 3.5.3)
 highr         0.8     2019-03-20 [1] CRAN (R 3.5.3)
 hms           0.4.2   2018-03-10 [1] CRAN (R 3.5.3)
 htmltools     0.3.6   2017-04-28 [1] CRAN (R 3.5.3)
 knitr       * 1.23    2019-05-18 [1] CRAN (R 3.5.2)
 lazyeval      0.2.2   2019-03-15 [1] CRAN (R 3.5.3)
 magrittr    * 1.5     2014-11-22 [1] CRAN (R 3.5.3)
 memoise       1.1.0   2017-04-21 [1] CRAN (R 3.5.3)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 3.5.3)
 pillar        1.4.1   2019-05-28 [1] CRAN (R 3.5.2)
 pkgbuild      1.0.3   2019-03-20 [1] CRAN (R 3.5.3)
 pkgconfig     2.0.2   2018-08-16 [1] CRAN (R 3.5.3)
 pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.5.3)
 plyr          1.8.4   2016-06-08 [1] CRAN (R 3.5.3)
 prettyunits   1.0.2   2015-07-13 [1] CRAN (R 3.5.3)
 processx      3.3.1   2019-05-08 [1] CRAN (R 3.5.2)
 pryr          0.1.4   2018-02-18 [1] CRAN (R 3.5.3)
 ps            1.3.0   2018-12-21 [1] CRAN (R 3.5.3)
 purrr         0.3.2   2019-03-15 [1] CRAN (R 3.5.3)
 R6            2.4.0   2019-02-14 [1] CRAN (R 3.5.3)
 Rcpp          1.0.1   2019-03-17 [1] CRAN (R 3.5.3)
 readr         1.3.1   2018-12-21 [1] CRAN (R 3.5.3)
 readxl      * 1.3.1   2019-03-13 [1] CRAN (R 3.5.3)
 remotes       2.0.4   2019-04-10 [1] CRAN (R 3.5.3)
 rlang         0.3.4   2019-04-07 [1] CRAN (R 3.5.3)
 rmarkdown     1.13    2019-05-22 [1] CRAN (R 3.5.3)
 rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.5.3)
 scales        1.0.0   2018-08-09 [1] CRAN (R 3.5.3)
 sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.5.3)
 stringi       1.4.3   2019-03-12 [1] CRAN (R 3.5.3)
 stringr       1.4.0   2019-02-10 [1] CRAN (R 3.5.3)
 tibble        2.1.3   2019-06-06 [1] CRAN (R 3.5.3)
 tidyr         0.8.3   2019-03-01 [1] CRAN (R 3.5.3)
 tidyselect    0.2.5   2018-10-11 [1] CRAN (R 3.5.3)
 usethis       1.5.0   2019-04-07 [1] CRAN (R 3.5.3)
 utf8          1.1.4   2018-05-24 [1] CRAN (R 3.5.3)
 vctrs         0.1.0   2018-11-29 [1] CRAN (R 3.5.3)
 withr         2.1.2   2018-03-15 [1] CRAN (R 3.5.3)
 xfun          0.7     2019-05-14 [1] CRAN (R 3.5.3)
 yaml          2.2.0   2018-07-25 [1] CRAN (R 3.5.2)
 zeallot       0.1.0   2018-01-28 [1] CRAN (R 3.5.3)

[1] C:/Users/an499583/Documents/R/win-library/3.5
[2] C:/Program Files/R/R-3.5.2/library
```
</details>



Report rendered by an499583 at 2019-06-26, 10:45 -0700 in 7 seconds.


