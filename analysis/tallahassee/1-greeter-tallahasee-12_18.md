



This report was automatically generated with the R package **knitr**
(version 1.28).


```r
# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(script = "./manipulation/tallahassee/1-greeter-tallahassee-12_18.R", output = "./analysis/tallahassee/1-greeter-tallahasee-12_18.md")
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio
```



```r
# Call `base::source()` on any repo file that defines functions needed below.  
base::source("./scripts/common-functions.R")
```

```r
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr) # disable when temp lines are removed
library(ggplot2)
library(ggpubr)
library(readxl)
```

```r
path_input       <- "./data-public/raw/tallahassee/12_18/"

# carry observations forward to fill cells missing due to Excel structure
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

# Note: this is an older solution which can be replaced with a more modern tidyr::fill
# however, tidyr::fill() requires to spell out the names of the columsn,
# wheares as fill_last_seen() could be used with dplyr::mutate_all()
# Example:
# tidyr::fill(race, ethnicity,sex,age_group, age)%>%
```

```r
# arrange into a list to use the structure later for data import
ls_input_files <- list(
  "cause_sex"       = list(
    "count_cause_sex_allrace" = paste0(path_input,"counts/counts-cause(113)-sex-year(2004-2018)-12_18.xlsx")
    ,"rate_cause_sex_allrace" = paste0(path_input,"rates/rates-cause(113)-sex-year(2004-2018)-12_18.xlsx")
  )
  ,"cause_sex_race" = list(
    "count_cause_sex_black"    = paste0(path_input,"counts/counts-cause(113)-sex-year(2004-2018)-12_18-black-non-hispanic.xlsx")
    ,"count_cause_sex_blother" = paste0(path_input,"counts/counts-cause(113)-sex-year(2004-2018)-12_18-black-other-non-hispanic.xlsx")
    ,"count_cause_sex_latino"  = paste0(path_input,"counts/counts-cause(113)-sex-year(2004-2018)-12_18-white-hispanic.xlsx")
    ,"count_cause_sex_white"   = paste0(path_input,"counts/counts-cause(113)-sex-year(2004-2018)-12_18-white-non-hispanic.xlsx")
    ,"rate_cause_sex_black"    = paste0(path_input,"rates/rates-cause(113)-sex-year(2004-2018)-12_18-black-non-hispanic.xlsx")
    ,"rate_cause_sex_blother"  = paste0(path_input,"rates/rates-cause(113)-sex-year(2004-2018)-12_18-black-other-non-hispanic.xlsx")
    ,"rate_cause_sex_latino"   = paste0(path_input,"rates/rates-cause(113)-sex-year(2004-2018)-12_18-white-hispanic.xlsx")
    ,"rate_cause_sex_white"    = paste0(path_input,"rates/rates-cause(113)-sex-year(2004-2018)-12_18-white-non-hispanic.xlsx")
  )
  ,"sex_cause"      = list(
    "count_sex_cause_allrace" = paste0(path_input,"counts/counts-sex-cause(113)-year(2004-2018)-12_18.xlsx")
    ,"rate_sex_cause_allrace" = paste0(path_input,"rates/rates-sex-cause(113)-year(2004-2018)-12_18.xlsx")
  )
  ,"sex_cause_race" = list(
    "count_sex_cause_black"    = paste0(path_input,"counts/counts-sex-cause(113)-year(2004-2018)-12_18-black-non-hispanic.xlsx")
    ,"count_sex_cause_blother" = paste0(path_input,"counts/counts-sex-cause(113)-year(2004-2018)-12_18-black-other-non-hispanic.xlsx")
    ,"count_sex_cause_latino"  = paste0(path_input,"counts/counts-sex-cause(113)-year(2004-2018)-12_18-white-hispanic.xlsx")
    ,"count_sex_cause_white"   = paste0(path_input,"counts/counts-sex-cause(113)-year(2004-2018)-12_18-white-non-hispanic.xlsx")
    ,"rate_sex_cause_black"    = paste0(path_input,"rates/rates-sex-cause(113)-year(2004-2018)-12_18-black-non-hispanic.xlsx")
    ,"rate_sex_cause_blother"  = paste0(path_input,"rates/rates-sex-cause(113)-year(2004-2018)-12_18-black-other-non-hispanic.xlsx")
    ,"rate_sex_cause_latino"   = paste0(path_input,"rates/rates-sex-cause(113)-year(2004-2018)-12_18-white-hispanic.xlsx")
    ,"rate_sex_cause_white"    = paste0(path_input,"rates/rates-sex-cause(113)-year(2004-2018)-12_18-white-non-hispanic.xlsx")
  )
)

ls_input <- ls_input_files # mimic the structure
for(i in seq_along(ls_input_files)){
  # i <- 1 # for testing and development
  # element_name <- ls_input_files[[i]] %>% names() # for examination
  for(j in seq_along(ls_input_files[[i]]) ){
    ls_input[[i]][[j]] <- readxl::read_excel(ls_input_files[[i]][[j]], col_names = FALSE, skip = 4)
  }
}
```

```
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ... and 15 more problems
```

```r
for(i in seq_along(ls_input)){
  # i <- ls_input[1] %>% names()
  # i <- 1; j <- 1
  # element_name <- ls_input[[i]] %>% names()
  for(j in seq_along(ls_input_files[[i]]) ){
    order_name <- names(ls_input)[[i]]
    if(order_name %in%  c("sex_cause", "sex_cause_race") ){
      names(ls_input[[i]][[j]]) <-  c("sex","external","injury","mortality_cause", c(2004:2018),'total')
  }else{
    names(ls_input[[i]][[j]]) <-  c("external","injury","mortality_cause", "sex",c(2004:2018),'total')
  }
  d_name <- names(ls_input[[i]])[[j]] 
  ls_input[[i]][[j]]<- ls_input[[i]][[j]] %>%
    dplyr::mutate_all(fill_last_seen) %>%
    dplyr::mutate(
      mortality_cause = ifelse(mortality_cause == "Suicide By Firearms Discharge (X72-X74)","Firearms", mortality_cause)
      ,mortality_cause = ifelse(mortality_cause == "Suicide By Other & Unspecified Means & Sequelae (X60-X71, X75-X84, Y87.0)","Other", mortality_cause)
    ) %>%
    dplyr::select(-external, -injury) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      measure = gsub("^(\\w+)_(\\w+)_(\\w+)_(\\w+)$", "\\1",d_name)
      ,order    = gsub("^(\\w+)_(\\w+)_(\\w+)_(\\w+)$", paste0("\\2","_","\\3"), d_name)
      ,race    = gsub("^(\\w+)_(\\w+)_(\\w+)_(\\w+)$", "\\4",d_name)
    ) %>%
    dplyr::select(order, measure, race, dplyr::everything()) %>%
    dplyr::select(-total)
  }
}
purrr::map(ls_input, names) 
```

```
## $cause_sex
## [1] "count_cause_sex_allrace" "rate_cause_sex_allrace" 
## 
## $cause_sex_race
## [1] "count_cause_sex_black"   "count_cause_sex_blother"
## [3] "count_cause_sex_latino"  "count_cause_sex_white"  
## [5] "rate_cause_sex_black"    "rate_cause_sex_blother" 
## [7] "rate_cause_sex_latino"   "rate_cause_sex_white"   
## 
## $sex_cause
## [1] "count_sex_cause_allrace" "rate_sex_cause_allrace" 
## 
## $sex_cause_race
## [1] "count_sex_cause_black"   "count_sex_cause_blother"
## [3] "count_sex_cause_latino"  "count_sex_cause_white"  
## [5] "rate_sex_cause_black"    "rate_sex_cause_blother" 
## [7] "rate_sex_cause_latino"   "rate_sex_cause_white"
```

```r
# because need to ensure the same source to be restructured during  for-loops
ls_ds_wide <- ls_input    # wide with respect to `year`
ls_ds_long <- ls_ds_wide  # single column `measure` for both `count` and `rate`
ls_ds_long2 <- ls_ds_wide # two column for `count` and `rate`

for(i in seq_along(ls_input)){
  # i <- 4
  ls_ds_wide[[i]] <- ls_input[[i]] %>% dplyr::bind_rows()
  file_name <- names(ls_input)[[i]]
  file_path <- paste0("./data-unshared/derived/tallahassee/12_18/wide/",file_name,".csv")
  ls_ds_wide[[i]] %>% readr::write_csv(file_path) 
  
  ls_ds_long[[i]] <- ls_ds_wide[[i]] %>% 
    tidyr::gather("year","value", 6:20) %>% 
    dplyr::mutate(
      year = as.integer(year)
    )
  file_name <- names(ls_ds_long)[[i]]
  file_path <- paste0("./data-unshared/derived/tallahassee/12_18/long/",file_name,".csv")
  ls_ds_long[[i]] %>% readr::write_csv(file_path) 
  
  file_path <- paste0("./data-unshared/derived/tallahassee/12_18/long2/",file_name,".csv")
  ls_ds_long2[[i]] <- ls_ds_long[[i]] %>%
    tidyr::spread(measure, value)
  ls_ds_long2[[i]] %>% readr::write_csv(file_path)
  
} 
```

```r
compute_change <- function(
  d
){
  d1 <- d %>% 
    dplyr::mutate(
      count_ref = filter(., year == 2018) %>% dplyr::select(count) %>% as.list() %>% unlist() %>% as.vector()
      ,rate_ref = filter(., year == 2018) %>% dplyr::select(rate) %>% as.list() %>% unlist() %>% as.vector()
    ) %>% 
    dplyr::mutate(
      pct_change_count = (count_ref - count)/count * 100
      ,pct_change_rate  = (rate_ref - rate)/rate * 100
    ) %>% 
    dplyr::select(-count_ref, -rate_ref)
  
  return(d1)
}
```

```r
ls_ds_long3 <- ls_ds_long2 # adding `pct_change` for both measures of `count` and `rate`
for(j in seq_along(ls_ds_long2) ){
  # j <- 2
  
  l_comb <- ls_ds_long2[[j]] %>% 
    dplyr::distinct(order, race, mortality_cause,sex) %>% 
    as.list()
  
  ls_temp <- list()
  for(i in seq_along( l_comb[[1]] ) ){
    # i <- 1
    ls_temp[[i]] <- ls_ds_long2[[j]] %>% 
      dplyr::filter(order == l_comb[["order"]][[i]] ) %>% 
      dplyr::filter(race == l_comb[["race"]][[i]] ) %>% 
      dplyr::filter(mortality_cause == l_comb[["mortality_cause"]][[i]] ) %>% 
      dplyr::filter(sex == l_comb[["sex"]][[i]] ) %>% 
      compute_change()
  }
  d_temp <- ls_temp %>% dplyr::bind_rows()
  ls_ds_long3[[j]] <- d_temp %>% 
    dplyr::mutate(
      pct_change_rate = ifelse(count < 5, NA, pct_change_rate)
      ,pct_change_count = ifelse(count < 5, NA, pct_change_count)
    )
  
  file_name <- names(ls_ds_long3)[[j]]
  file_path <- paste0("./data-unshared/derived/tallahassee/12_18/long3/",file_name,".csv")
  ls_ds_long3[[j]] %>% readr::write_csv(file_path) 
}



# save-to-disk ------------------------------------------------------------
ls_ds_wide %>% saveRDS("./data-unshared/derived/tallahassee/12_18/ls_ds_wide.rds") 
ls_ds_long %>% saveRDS("./data-unshared/derived/tallahassee/12_18/ls_ds_long.rds") 
ls_ds_long2 %>% saveRDS("./data-unshared/derived/tallahassee/12_18/ls_ds_long2.rds") 
ls_ds_long3 %>% saveRDS("./data-unshared/derived/tallahassee/12_18/ls_ds_long3.rds") 
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 3.6.2 (2019-12-12)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 18363)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] readxl_1.3.1  ggpubr_0.2.5  ggplot2_3.2.1 dplyr_0.8.4   magrittr_1.5 
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.3       pillar_1.4.3     compiler_3.6.2   cellranger_1.1.0
##  [5] tools_3.6.2      digest_0.6.24    evaluate_0.14    lifecycle_0.1.0 
##  [9] tibble_2.1.3     gtable_0.3.0     pkgconfig_2.0.3  rlang_0.4.4     
## [13] cli_2.0.1        rstudioapi_0.11  yaml_2.2.1       xfun_0.12       
## [17] stringr_1.4.0    withr_2.1.2      knitr_1.28       vctrs_0.2.2     
## [21] hms_0.5.3        grid_3.6.2       tidyselect_1.0.0 glue_1.3.1      
## [25] R6_2.4.1         fansi_0.4.1      rmarkdown_2.1    tidyr_1.0.2     
## [29] readr_1.3.1      purrr_0.3.3      scales_1.1.0     htmltools_0.4.0 
## [33] ellipsis_0.3.0   assertthat_0.2.1 colorspace_1.4-1 ggsignif_0.6.0  
## [37] utf8_1.1.4       stringi_1.4.5    lazyeval_0.2.2   munsell_0.5.0   
## [41] markdown_1.1     crayon_1.3.4
```

```r
Sys.time()
```

```
## [1] "2020-03-28 11:22:16 EDT"
```

