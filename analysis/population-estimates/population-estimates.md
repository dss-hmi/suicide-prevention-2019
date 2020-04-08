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
1 Black & Other + Hispanic     20904
2 Black & Other + Non-Hispanic 20904
3 White + Hispanic             20904
4 White + Non-Hispanic         20904
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
# g2 ----------------------------------------------------------------------

#User would choose age group to examine


g2 <- ds_grouped_totals %>% 
  filter(age_group == "20-24") %>%   #filter will be user determined
#could also have more then one group
  ggplot(aes(x = year, y = n_people, color = racethnicity, group = racethnicity)) +
  geom_line() +
  facet_grid(sex ~ .) 

g2
```

<img src="figure_rmd/g1-2.png" width="550px" />

```r
# g3 ----------------------------------------------------------------------

# bar graph by year, user can select year to disply

# ds_totals <- ds_grouped_totals %>% 
#   group_by(year, sex) %>% 
#   mutate(
#     t_people = sum(n_people)
#   ) %>% 
#   ungroup()

g3 <- ds_grouped_totals %>% 
  filter(year == 2017) %>% 
  ggplot(aes(x = age_group, y = n_people)) +
  geom_col(color = "red") +
  facet_grid(sex ~ racethnicity)
g3
```

<img src="figure_rmd/g1-3.png" width="550px" />


