---
title: "Title"
output:
  html_document:
    keep_md: yes
    toc: yes
---

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->




<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 



<!-- Load the sources.  Suppress the output when loading sources. --> 



<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 


<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


# Load Data

<!-- Load the datasets.   -->

```
6.06 MB
```

```
[1] "tbl_df"     "tbl"        "data.frame"
```

```
[1] "county"    "year"      "sex"       "race"      "ethnicity" "age_group" "count"    
```

<!-- Inspect the datasets.   -->


# Tweak Data

<!-- Tweak the datasets.   -->

```
# A tibble: 4 x 2
  racethnicity                     n
  <chr>                        <int>
1 Black & Other + Hispanic     19162
2 Black & Other + Non-Hispanic 19162
3 White + Hispanic             19162
4 White + Non-Hispanic         19162
```



<!-- Basic table view.   -->


<!-- G1   -->

# Graph 1 


```r
# 
# ds1 %>% 
#   group_by(year,racethnicity,age_group) %>% 
#   summarise(
#     n_people = sum(count, na.rm = TRUE)
#   ) %>% 
#   ggplot(aes(x = year, y = n_people, group = racethnicity, color = racethnicity)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~age_group, scales = "free_y") 


g1 <- ds1 %>% 
  group_by(year,sex,racethnicity,age_group) %>% 
  summarise(
    n_people = sum(count, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = n_people, color = sex, group = interaction(racethnicity, sex))) +
  geom_line() +
  facet_grid(racethnicity ~ age_group, scales = "free_y")
g1
```

<img src="figure_rmd/g1-1.png" width="550px" />




```r
g1
```

<img src="figure_rmd/g1a-1.png" width="400px" />




```r
g1
```

<img src="figure_rmd/g1b-1.png" width="900px" />



