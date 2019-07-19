---
title: 'GLS Activity: Maps'
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
    highlight: haddock
    toc: yes
  pdf_document:
    toc: yes
---

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->



# Set the scene 

Describe the working environment. 
<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 



<!-- Load the sources.  Suppress the output when loading sources. --> 



<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 


<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


<!-- Load the datasets.   -->

```{.numberLines .lineAnchors .chunkout}
14 MB
```

```{.numberLines .lineAnchors .chunkout}
[1] "list"
```

```{.numberLines .lineAnchors .chunkout}
[1] "granularity_gls"        "granularity_population"
```

<!-- Inspect the datasets.   -->


<!-- Tweak the datasets.   -->
<div class="kable-table">

county          community   professionals
-------------  ----------  --------------
Orange               8182            1605
Saint Lucie          1068             830
Volusia              2170             432
Seminole             2400             419
Lake                  206             290
Palm Beach            935             241
Brevard              1126             166
Indian River          110             118
Marion                325              87
Hernando              115              85
Sumter                 85              63
Osceola                35              59
Saint Johns           250              50
Martin                490              46
Flagler               397              29

</div><div class="kable-table">

variable        type    na   na_pct   unique   min     mean    max
--------------  -----  ---  -------  -------  ----  -------  -----
county          chr      0        0       23    NA       NA     NA
region          chr      0        0        3    NA       NA     NA
rgn             chr      0        0        3    NA       NA     NA
professionals   dou      0        0       23     0   225.13   1605
community       dou      0        0       17     0   790.17   8182

</div>


# I Summary view


## Prof


## Comm
















Session Information {#session-info}
===========================================================================

For the sake of documentation and reproducibility, the current report was rendered in the following environment.  Click the line below to expand.

<details>
  <summary>Environment <span class="glyphicon glyphicon-plus-sign"></span></summary>

```{.numberLines .lineAnchors .chunkout}
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
 date     2019-07-18                  

- Packages -----------------------------------------------------------------------------------------------------------
 package      * version  date       lib source        
 abind          1.4-5    2016-07-21 [1] CRAN (R 3.5.2)
 acepack        1.4.1    2016-10-29 [1] CRAN (R 3.5.3)
 assertthat     0.2.0    2017-04-11 [1] CRAN (R 3.5.1)
 backports      1.1.2    2017-12-13 [1] CRAN (R 3.5.1)
 base64enc      0.1-3    2015-07-28 [1] CRAN (R 3.5.1)
 bindr          0.1.1    2018-03-13 [1] CRAN (R 3.5.1)
 bindrcpp     * 0.2.2    2018-03-29 [1] CRAN (R 3.5.1)
 bitops         1.0-6    2013-08-17 [1] CRAN (R 3.5.1)
 callr          3.2.0    2019-03-15 [1] CRAN (R 3.5.3)
 car            3.0-3    2019-05-27 [1] CRAN (R 3.5.3)
 carData        3.0-2    2018-09-30 [1] CRAN (R 3.5.2)
 cellranger     1.1.0    2016-07-27 [1] CRAN (R 3.5.1)
 checkmate      1.9.3    2019-05-03 [1] CRAN (R 3.5.3)
 class          7.3-15   2019-01-01 [1] CRAN (R 3.5.3)
 classInt       0.3-3    2019-04-26 [1] CRAN (R 3.5.3)
 cli            1.1.0    2019-03-19 [1] CRAN (R 3.5.3)
 cluster        2.0.9    2019-05-01 [1] CRAN (R 3.5.3)
 codetools      0.2-16   2018-12-24 [1] CRAN (R 3.5.2)
 colorspace     1.4-1    2019-03-18 [1] CRAN (R 3.5.3)
 crayon         1.3.4    2017-09-16 [1] CRAN (R 3.5.1)
 crosstalk      1.0.0    2016-12-21 [1] CRAN (R 3.5.3)
 curl           3.2      2018-03-28 [1] CRAN (R 3.5.1)
 data.table     1.12.2   2019-04-07 [1] CRAN (R 3.5.3)
 DBI            1.0.0    2018-05-02 [1] CRAN (R 3.5.1)
 desc           1.2.0    2018-05-01 [1] CRAN (R 3.5.3)
 devtools       2.0.2    2019-04-08 [1] CRAN (R 3.5.3)
 digest         0.6.15   2018-01-28 [1] CRAN (R 3.5.1)
 dplyr        * 0.7.6    2018-06-29 [1] CRAN (R 3.5.1)
 DT             0.6      2019-05-09 [1] CRAN (R 3.5.3)
 e1071          1.7-2    2019-06-05 [1] CRAN (R 3.5.3)
 evaluate       0.11     2018-07-17 [1] CRAN (R 3.5.1)
 explore        0.4.3    2019-06-17 [1] CRAN (R 3.5.3)
 fansi          0.2.3    2018-05-06 [1] CRAN (R 3.5.1)
 forcats        0.3.0    2018-02-19 [1] CRAN (R 3.5.1)
 foreign        0.8-71   2018-07-20 [1] CRAN (R 3.5.2)
 Formula        1.2-3    2018-05-03 [1] CRAN (R 3.5.2)
 fs             1.3.1    2019-05-06 [1] CRAN (R 3.5.3)
 ggmap        * 3.0.0    2019-02-05 [1] CRAN (R 3.5.3)
 ggplot2      * 3.2.0    2019-06-16 [1] CRAN (R 3.5.3)
 ggpubr       * 0.2      2018-11-15 [1] CRAN (R 3.5.3)
 ggrepel      * 0.8.1    2019-05-07 [1] CRAN (R 3.5.3)
 glue           1.3.0    2018-07-17 [1] CRAN (R 3.5.1)
 gridExtra      2.3      2017-09-09 [1] CRAN (R 3.5.3)
 gtable         0.3.0    2019-03-25 [1] CRAN (R 3.5.3)
 haven          1.1.2    2018-06-27 [1] CRAN (R 3.5.1)
 highr          0.7      2018-06-09 [1] CRAN (R 3.5.1)
 Hmisc          4.2-0    2019-01-26 [1] CRAN (R 3.5.3)
 hms            0.4.2    2018-03-10 [1] CRAN (R 3.5.1)
 htmlTable      1.13.1   2019-01-07 [1] CRAN (R 3.5.3)
 htmltools      0.3.6    2017-04-28 [1] CRAN (R 3.5.1)
 htmlwidgets    1.3      2018-09-30 [1] CRAN (R 3.5.3)
 httpuv         1.4.5    2018-07-19 [1] CRAN (R 3.5.1)
 httr           1.3.1    2017-08-20 [1] CRAN (R 3.5.1)
 jpeg           0.1-8    2014-01-23 [1] CRAN (R 3.5.2)
 jsonlite       1.6      2018-12-07 [1] CRAN (R 3.5.3)
 KernSmooth     2.23-15  2015-06-29 [1] CRAN (R 3.5.3)
 knitr        * 1.20     2018-02-20 [1] CRAN (R 3.5.1)
 labeling       0.3      2014-08-23 [1] CRAN (R 3.5.2)
 later          0.7.3    2018-06-08 [1] CRAN (R 3.5.1)
 lattice        0.20-35  2017-03-25 [1] CRAN (R 3.5.1)
 latticeExtra   0.6-28   2016-02-09 [1] CRAN (R 3.5.3)
 lazyeval       0.2.1    2017-10-29 [1] CRAN (R 3.5.1)
 magrittr     * 1.5      2014-11-22 [1] CRAN (R 3.5.1)
 mapdata      * 2.3.0    2018-03-30 [1] CRAN (R 3.5.3)
 maps         * 3.3.0    2018-04-03 [1] CRAN (R 3.5.3)
 Matrix         1.2-17   2019-03-22 [1] CRAN (R 3.5.3)
 memoise        1.1.0    2017-04-21 [1] CRAN (R 3.5.3)
 mime           0.5      2016-07-07 [1] CRAN (R 3.5.1)
 munsell        0.5.0    2018-06-12 [1] CRAN (R 3.5.3)
 nnet           7.3-12   2016-02-02 [1] CRAN (R 3.5.3)
 openxlsx       4.1.0.1  2019-05-28 [1] CRAN (R 3.5.3)
 pillar         1.3.0    2018-07-14 [1] CRAN (R 3.5.1)
 pkgbuild       1.0.3    2019-03-20 [1] CRAN (R 3.5.3)
 pkgconfig      2.0.1    2017-03-21 [1] CRAN (R 3.5.1)
 pkgload        1.0.2    2018-10-29 [1] CRAN (R 3.5.3)
 plotly       * 4.9.0    2019-04-10 [1] CRAN (R 3.5.3)
 plyr           1.8.4    2016-06-08 [1] CRAN (R 3.5.1)
 png            0.1-7    2013-12-03 [1] CRAN (R 3.5.2)
 prettyunits    1.0.2    2015-07-13 [1] CRAN (R 3.5.1)
 processx       3.3.1    2019-05-08 [1] CRAN (R 3.5.3)
 promises       1.0.1    2018-04-13 [1] CRAN (R 3.5.1)
 pryr           0.1.4    2018-02-18 [1] CRAN (R 3.5.3)
 ps             1.3.0    2018-12-21 [1] CRAN (R 3.5.3)
 purrr          0.2.5    2018-05-29 [1] CRAN (R 3.5.1)
 R6             2.2.2    2017-06-17 [1] CRAN (R 3.5.1)
 RColorBrewer   1.1-2    2014-12-07 [1] CRAN (R 3.5.2)
 Rcpp           0.12.18  2018-07-23 [1] CRAN (R 3.5.1)
 readxl         1.1.0    2018-04-20 [1] CRAN (R 3.5.1)
 remotes        2.0.4    2019-04-10 [1] CRAN (R 3.5.3)
 RgoogleMaps    1.4.3    2018-11-07 [1] CRAN (R 3.5.3)
 rio            0.5.16   2018-11-26 [1] CRAN (R 3.5.3)
 rjson          0.2.20   2018-06-08 [1] CRAN (R 3.5.2)
 rlang          0.4.0    2019-06-25 [1] CRAN (R 3.5.3)
 rmarkdown      1.10     2018-06-11 [1] CRAN (R 3.5.1)
 rpart          4.1-15   2019-04-12 [1] CRAN (R 3.5.3)
 rprojroot      1.3-2    2018-01-03 [1] CRAN (R 3.5.1)
 rstudioapi     0.7      2017-09-07 [1] CRAN (R 3.5.1)
 scales         1.0.0    2018-08-09 [1] CRAN (R 3.5.3)
 sessioninfo    1.1.1    2018-11-05 [1] CRAN (R 3.5.3)
 shiny          1.1.0    2018-05-17 [1] CRAN (R 3.5.1)
 stringi        1.2.4    2018-07-20 [1] CRAN (R 3.5.1)
 stringr        1.3.1    2018-05-10 [1] CRAN (R 3.5.1)
 survival       2.44-1.1 2019-04-01 [1] CRAN (R 3.5.3)
 tibble         1.4.2    2018-01-22 [1] CRAN (R 3.5.1)
 tidyr          0.8.1    2018-05-18 [1] CRAN (R 3.5.1)
 tidyselect     0.2.4    2018-02-26 [1] CRAN (R 3.5.1)
 units          0.6-3    2019-05-03 [1] CRAN (R 3.5.3)
 usethis        1.5.0    2019-04-07 [1] CRAN (R 3.5.3)
 utf8           1.1.4    2018-05-24 [1] CRAN (R 3.5.1)
 viridisLite    0.3.0    2018-02-01 [1] CRAN (R 3.5.3)
 withr          2.1.2    2018-03-15 [1] CRAN (R 3.5.1)
 xtable         1.8-2    2016-02-05 [1] CRAN (R 3.5.1)
 yaml           2.2.0    2018-07-25 [1] CRAN (R 3.5.1)
 zip            2.0.2    2019-05-13 [1] CRAN (R 3.5.3)

[1] C:/Users/Jodh/Anaconda3/envs/rstudio/lib/R/library
```
</details>



Report rendered by Jodh at 2019-07-18, 20:59 -0400 in 6 seconds.


