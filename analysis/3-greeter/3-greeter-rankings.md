---
title: "3-greeter-rankings"
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


This document is the annotation layer to [`./manipulation/3-greeter-rankings.R`][path_greeter] script, which connects to data sources used in the study, imports them into RStudio environment and prepares them for analytic enquiry.

[path_greeter]:https://github.com/dss-hmi/suicide-prevention-2019/blob/master/manipulation/3-greeter-rankings.R

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
path_input       <- "./data-unshared/raw/FloridaRankings/"
```

<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


# II. Input
<!-- Load the datasets.   -->

```r
#
input_files <- list.files(path_input,pattern = ".xls", full.names = T)
# create list of relevent sheets per input file
# sheets chosen: Outcomes & Factors Rankings
ls_ds = list(
  "ds_2010" = readxl::read_excel(input_files[1],sheet = "Outcomes & Factors Rankings"),
  "ds_2011" = readxl::read_excel(input_files[2],sheet = "Outcomes & Factors Rankings"),
  "ds_2012" = readxl::read_excel(input_files[3],sheet = "Outcomes & Factors Rankings"),
  "ds_2013" = readxl::read_excel(input_files[4],sheet = "Outcomes & Factors Rankings"),
  "ds_2014" = readxl::read_excel(input_files[5],sheet = "Outcomes & Factors Rankings"),
  "ds_2015" = readxl::read_excel(input_files[6],sheet = "Outcomes & Factors Rankings"),
  "ds_2016" = readxl::read_excel(input_files[7],sheet = "Outcomes & Factors Rankings"),
  "ds_2017" = readxl::read_excel(input_files[8],sheet = "Outcomes & Factors Rankings"),
  "ds_2018" = readxl::read_excel(input_files[9],sheet = "Outcomes & Factors Rankings"),
  "ds_2019" = readxl::read_excel(input_files[10],sheet = "Outcomes & Factors Rankings")
)
```

# III. Groom
<!-- Tweak the datasets.   -->


<!-- Tweak the datasets.   -->

```r
# add year column for each file in list
library(tibble)
for(i in c(1:10)){
  ls_ds[[i]] <- ls_ds[[i]] %>% tibble::add_column(year = 2009+i)
}
library(data.table)
ds <- rbindlist(ls_ds)
#hard coding names for columns
names(ds) <- c("FIPS","State","County","Health-Outcomes-Z-score",
               "Health-Outcomes-Rank","Health-Factors-Z-score",
               "Health-Factors-Rank","year")
# remove na rows
ds <- na.omit(ds)
#remove non-sense header rows
ds <- ds %>% dplyr::filter(!County == "County")

head(ds)
```

<div class="kable-table">

FIPS    State     County     Health-Outcomes-Z-score   Health-Outcomes-Rank   Health-Factors-Z-score   Health-Factors-Rank    year
------  --------  ---------  ------------------------  ---------------------  -----------------------  --------------------  -----
12001   Florida   Alachua    -0.52505017762319484      18                     -0.60165485688806575     8                      2010
12003   Florida   Baker      0.77194404175301135       60                     0.48766003262232682      57                     2010
12005   Florida   Bay        -0.0023069614635685548    36                     -0.13827715463149559     26                     2010
12007   Florida   Bradford   0.50364377209061217       53                     0.15434078846842228      47                     2010
12009   Florida   Brevard    -0.34343741979015519      23                     -0.36284442505379155     14                     2010
12011   Florida   Broward    -0.65682024486037949      14                     -0.67551496700145675     2                      2010

</div>

# V. Save to Disk

Let us create a `data transfer object` (dto), a list object that would store all three data components in this project. This object (dto) will be the point of departure for all subsequent analytic efforst. 



```r
ds %>% pryr::object_size()
```

```
159 kB
```

```r
ds %>%          saveRDS("./data-unshared/derived/3-greeter-rankings.rds")
ds %>% readr::write_csv("./data-unshared/derived/3-greeted-rankings.csv") # for read-only inspection
```



Session Information {#session-info}
===========================================================================

For the sake of documentation and reproducibility, the current report was rendered in the following environment.  Click the line below to expand.

<details>
  <summary>Environment <span class="glyphicon glyphicon-plus-sign"></span></summary>

```
- Session info -------------------------------------------------------------------------------------------------------
 setting  value                       
 version  R version 3.5.1 (2018-07-02)
 os       Windows >= 8 x64            
 system   x86_64, mingw32             
 ui       RStudio                     
 language (EN)                        
 collate  English_United States.1252  
 ctype    English_United States.1252  
 tz       America/New_York            
 date     2019-07-05                  

- Packages -----------------------------------------------------------------------------------------------------------
 package     * version date       lib source        
 assertthat    0.2.0   2017-04-11 [1] CRAN (R 3.5.1)
 backports     1.1.2   2017-12-13 [1] CRAN (R 3.5.1)
 bindr         0.1.1   2018-03-13 [1] CRAN (R 3.5.1)
 bindrcpp    * 0.2.2   2018-03-29 [1] CRAN (R 3.5.1)
 callr         3.2.0   2019-03-15 [1] CRAN (R 3.5.3)
 cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.5.1)
 cli           1.1.0   2019-03-19 [1] CRAN (R 3.5.3)
 codetools     0.2-16  2018-12-24 [1] CRAN (R 3.5.2)
 colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.5.3)
 crayon        1.3.4   2017-09-16 [1] CRAN (R 3.5.1)
 data.table  * 1.12.2  2019-04-07 [1] CRAN (R 3.5.3)
 desc          1.2.0   2018-05-01 [1] CRAN (R 3.5.3)
 devtools      2.0.2   2019-04-08 [1] CRAN (R 3.5.3)
 digest        0.6.15  2018-01-28 [1] CRAN (R 3.5.1)
 dplyr       * 0.7.6   2018-06-29 [1] CRAN (R 3.5.1)
 evaluate      0.11    2018-07-17 [1] CRAN (R 3.5.1)
 fansi         0.2.3   2018-05-06 [1] CRAN (R 3.5.1)
 fs            1.3.1   2019-05-06 [1] CRAN (R 3.5.3)
 ggplot2     * 3.2.0   2019-06-16 [1] CRAN (R 3.5.3)
 ggpubr      * 0.2     2018-11-15 [1] CRAN (R 3.5.3)
 glue          1.3.0   2018-07-17 [1] CRAN (R 3.5.1)
 gtable        0.3.0   2019-03-25 [1] CRAN (R 3.5.3)
 highr         0.7     2018-06-09 [1] CRAN (R 3.5.1)
 hms           0.4.2   2018-03-10 [1] CRAN (R 3.5.1)
 htmltools     0.3.6   2017-04-28 [1] CRAN (R 3.5.1)
 knitr       * 1.20    2018-02-20 [1] CRAN (R 3.5.1)
 lazyeval      0.2.1   2017-10-29 [1] CRAN (R 3.5.1)
 magrittr    * 1.5     2014-11-22 [1] CRAN (R 3.5.1)
 memoise       1.1.0   2017-04-21 [1] CRAN (R 3.5.3)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 3.5.3)
 pillar        1.3.0   2018-07-14 [1] CRAN (R 3.5.1)
 pkgbuild      1.0.3   2019-03-20 [1] CRAN (R 3.5.3)
 pkgconfig     2.0.1   2017-03-21 [1] CRAN (R 3.5.1)
 pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.5.3)
 prettyunits   1.0.2   2015-07-13 [1] CRAN (R 3.5.1)
 processx      3.3.1   2019-05-08 [1] CRAN (R 3.5.3)
 pryr          0.1.4   2018-02-18 [1] CRAN (R 3.5.3)
 ps            1.3.0   2018-12-21 [1] CRAN (R 3.5.3)
 purrr         0.2.5   2018-05-29 [1] CRAN (R 3.5.1)
 R6            2.2.2   2017-06-17 [1] CRAN (R 3.5.1)
 Rcpp          0.12.18 2018-07-23 [1] CRAN (R 3.5.1)
 readr         1.1.1   2017-05-16 [1] CRAN (R 3.5.1)
 readxl      * 1.1.0   2018-04-20 [1] CRAN (R 3.5.1)
 remotes       2.0.4   2019-04-10 [1] CRAN (R 3.5.3)
 rlang         0.4.0   2019-06-25 [1] CRAN (R 3.5.3)
 rmarkdown     1.10    2018-06-11 [1] CRAN (R 3.5.1)
 rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.5.1)
 rstudioapi    0.7     2017-09-07 [1] CRAN (R 3.5.1)
 scales        1.0.0   2018-08-09 [1] CRAN (R 3.5.3)
 sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.5.3)
 stringi       1.2.4   2018-07-20 [1] CRAN (R 3.5.1)
 stringr       1.3.1   2018-05-10 [1] CRAN (R 3.5.1)
 tibble      * 1.4.2   2018-01-22 [1] CRAN (R 3.5.1)
 tidyselect    0.2.4   2018-02-26 [1] CRAN (R 3.5.1)
 usethis       1.5.0   2019-04-07 [1] CRAN (R 3.5.3)
 utf8          1.1.4   2018-05-24 [1] CRAN (R 3.5.1)
 withr         2.1.2   2018-03-15 [1] CRAN (R 3.5.1)
 yaml          2.2.0   2018-07-25 [1] CRAN (R 3.5.1)

[1] C:/Users/Jodh/Anaconda3/envs/rstudio/lib/R/library
```
</details>



Report rendered by Jodh at 2019-07-05, 13:39 -0400 in 2 seconds.


