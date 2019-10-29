---
title: "2-greeter-suicide"
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


This document is the annotation layer to [`./manipulation/2-greeter-suicide.R`][path_greeter] script, which connects to data sources used in the study, imports them into RStudio environment and prepares them for analytic enquiry.

[path_greeter]:https://github.com/dss-hmi/suicide-prevention-2019/blob/master/manipulation/2-greeter-suicide.R

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
path_input       <- "./data-unshared/raw/FloridaDeathsReport/"
```

<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


# II. Input
<!-- Load the datasets.   -->

```r
#
input_files <- list.files(path_input,pattern = ".xlsx$", full.names = T)
# when downloading the tables the interface couldn't handle all years at once
ls_ds <- list(
  "1" =  readxl::read_excel(input_files[1], col_names = FALSE, skip = 2)
  ,"2" = readxl::read_excel(input_files[2], col_names = FALSE, skip = 2)
  ,"3" = readxl::read_excel(input_files[3], col_names = FALSE, skip = 2)
)
# to bring all batches into the same dataframe:
ds0 <- ls_ds %>% dplyr::bind_rows()
ds0 %>% head(30) %>% neat()
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> ...1 </th>
   <th style="text-align:left;"> ...2 </th>
   <th style="text-align:left;"> ...3 </th>
   <th style="text-align:left;"> ...4 </th>
   <th style="text-align:left;"> ...5 </th>
   <th style="text-align:left;"> ...6 </th>
   <th style="text-align:left;"> ...7 </th>
   <th style="text-align:left;"> ...8 </th>
   <th style="text-align:right;"> ...9 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Alachua </td>
   <td style="text-align:left;"> 2006 </td>
   <td style="text-align:left;"> External Causes of Mortality </td>
   <td style="text-align:left;"> Suicide: Drugs &amp; Biological Substances (X60-X64) </td>
   <td style="text-align:left;"> 35-44 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Non-Hispanic </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 45-54 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Non-Hispanic </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Non-Hispanic </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Suicide: Other Gases &amp; Vapors (X67) </td>
   <td style="text-align:left;"> 65-74 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Non-Hispanic </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Suicide: Hanging, Strangulation, Suffocation (X70) </td>
   <td style="text-align:left;"> 20-24 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Non-Hispanic </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 25-34 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Non-Hispanic </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 35-44 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:left;"> Black &amp; Other </td>
   <td style="text-align:left;"> Non-Hispanic </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 45-54 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Non-Hispanic </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

<!-- Inspect the datasets.   -->


# III. Groom
<!-- Tweak the datasets.   -->

```r
# hardcoding, because cheaper and easy to check
names(ds0) <- c(
  "county","year","mortality_locus","mortality_cause","age_group","sex","race","ethnicity","resident_deaths"
)

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

ds1 <- ds0 %>% 
  dplyr::mutate_all(fill_last_seen)
ds1 %>% dplyr::glimpse(100)
```

```
Observations: 68,552
Variables: 9
$ county          <chr> "Alachua", "Alachua", "Alachua", "Alachua", "Alachua", "Alachua", "Alac...
$ year            <chr> "2006", "2006", "2006", "2006", "2006", "2006", "2006", "2006", "2006",...
$ mortality_locus <chr> "External Causes of Mortality", "External Causes of Mortality", "Extern...
$ mortality_cause <chr> "Suicide: Drugs & Biological Substances (X60-X64)", "Suicide: Drugs & B...
$ age_group       <chr> "35-44", "35-44", "35-44", "35-44", "45-54", "45-54", "45-54", "45-54",...
$ sex             <chr> "Female", "Female", "Female", "Total", "Female", "Female", "Female", "M...
$ race            <chr> "White", "White", "Total", "Total", "White", "White", "Total", "White",...
$ ethnicity       <chr> "Non-Hispanic", "Total", "Total", "Total", "Non-Hispanic", "Total", "To...
$ resident_deaths <dbl> 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 3, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2,...
```

```r
ds1 <- ds1 %>% 
  dplyr::mutate(
    age_group = tolower(age_group)
    ,age_group = gsub("-","_",age_group)
    ,age_group = gsub("\\+","_plus",age_group)
    ,age_group = gsub("'","",age_group)
  )
ds1 %>% 
  dplyr::distinct(age_group)
```

<div class="kable-table">

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> age_group </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 35_44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 45_54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> total </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 65_74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20_24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25_34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 55_64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 75_84 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 85_plus </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15_19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> unknown </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10_14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5_9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1_4 </td>
  </tr>
</tbody>
</table>

</div>

```r
# because it will be easier to compute on site + demonstration of summary logic
ds2 <- ds1 %>% 
  dplyr::filter(! county    == "Total") %>% 
  dplyr::filter(! year      == "Total") %>% 
  dplyr::filter(! mortality_locus      == "Total") %>% 
  dplyr::filter(! mortality_cause      == "Total") %>% 
  dplyr::filter(! sex       == "Total") %>% 
  dplyr::filter(! race      == "Total") %>% 
  dplyr::filter(! ethnicity == "Total") %>% 
  dplyr::filter(! age_group == "total")  
```










# IV. Inspect









<!-- Basic table view.   -->


<!-- Basic graph view.   -->




# V. Save to Disk

Let us create a `data transfer object` (dto), a list object that would store all three data components in this project. This object (dto) will be the point of departure for all subsequent analytic efforst. 



```r
ds2 %>% pryr::object_size()
```

```
1.36 MB
```

```r
ds2 %>%          saveRDS("./data-unshared/derived/2-greeted-suicide.rds")
ds2 %>% readr::write_csv("./data-unshared/derived/2-greeted-suicide.csv") # for read-only inspection
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
 httr          1.4.0   2018-12-11 [1] CRAN (R 3.5.3)
 kableExtra    1.1.0   2019-03-16 [1] CRAN (R 3.5.3)
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
 rstudioapi    0.10    2019-03-19 [1] CRAN (R 3.5.3)
 rvest         0.3.4   2019-05-15 [1] CRAN (R 3.5.3)
 scales        1.0.0   2018-08-09 [1] CRAN (R 3.5.3)
 sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.5.3)
 stringi       1.4.3   2019-03-12 [1] CRAN (R 3.5.3)
 stringr       1.4.0   2019-02-10 [1] CRAN (R 3.5.3)
 tibble        2.1.3   2019-06-06 [1] CRAN (R 3.5.3)
 tidyselect    0.2.5   2018-10-11 [1] CRAN (R 3.5.3)
 usethis       1.5.0   2019-04-07 [1] CRAN (R 3.5.3)
 utf8          1.1.4   2018-05-24 [1] CRAN (R 3.5.3)
 vctrs         0.1.0   2018-11-29 [1] CRAN (R 3.5.3)
 viridisLite   0.3.0   2018-02-01 [1] CRAN (R 3.5.3)
 webshot       0.5.1   2018-09-28 [1] CRAN (R 3.5.3)
 withr         2.1.2   2018-03-15 [1] CRAN (R 3.5.3)
 xfun          0.7     2019-05-14 [1] CRAN (R 3.5.3)
 xml2          1.2.0   2018-01-24 [1] CRAN (R 3.5.3)
 yaml          2.2.0   2018-07-25 [1] CRAN (R 3.5.2)
 zeallot       0.1.0   2018-01-28 [1] CRAN (R 3.5.3)

[1] C:/Users/an499583/Documents/R/win-library/3.5
[2] C:/Program Files/R/R-3.5.2/library
```
</details>



Report rendered by an499583 at 2019-06-26, 10:46 -0700 in 7 seconds.


