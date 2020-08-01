rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr) # disable when temp lines are removed
library(ggplot2)

# ---- declare-globals ---------------------------------------------------------
path_file_input_0 <- "./data-unshared/derived/1-greeted-population-3-cdc.rds"
path_file_input_1 <- "./data-unshared/derived/2-greeted-suicide-2.rds"


# ---- load-data ---------------------------------------------------------------

ds_population <- readr::read_rds(path_file_input_0)
ds_suicide    <- readr::read_rds(path_file_input_1)

# ds_suicide %>% glimpse()
# ds_population %>% glimpse()
# ---- compare-data -----------------

ds_population %>% distinct(race,ethnicity) 
ds_suicide %>% distinct(race,ethnicity) 

# ---- tweak-data ------------------------------

ds_pop <- ds_population %>% 
  mutate_at(
    "county", ~stringr::str_replace_all(.
                                        ,c(
                                          "St. Johns"  = "Saint Johns"
                                          ,"St. Lucie" = "Saint Lucie"
                                          ,"DeSoto"    = "Desoto"
                                        ))
  ) %>% 
   mutate(
      race2 = forcats::fct_recode(race,
                                  "Black & Other"  =  "American Indian or Alaska Native"
                                  ,"Black & Other" =  "Asian or Pacific Islander"
                                  ,"Black & Other" =  "Black or African American")
      ,race3 = forcats::fct_recode(race,
                                  "Other"  =  "American Indian or Alaska Native"
                                  ,"Other" =  "Asian or Pacific Islander"
                                  ,"Black" =  "Black or African American")
      ,ethnicity = forcats::fct_recode(ethnicity,
                                      "Hispanic"      = "Hispanic or Latino"
                                      ,"Non-Hispanic" = "Not Hispanic or Latino")
    ) %>% 
  rename(race4 = race, sex = gender) %>% 
  mutate_at(c("race2","race3","ethnicity"), as.character)
ds_pop %>% distinct(race4, race3, race2, ethnicity)
ds_pop %>% glimpse()

ds_sui <- ds_suicide %>%
  mutate(  
    age    = if_else(age < 85,age,as.integer(86))
    ,cause = forcats::fct_recode(cause,
      "Firearms" = "Suicide By Firearms Discharge (X72-X74)",
      "Other" = "Suicide By Other & Unspecified Means & Sequelae (X60-X71, X75-X84, Y87.0)"
    )
    ,race3 = race
    ,race2 = forcats::fct_recode(race3,
                                 "Black & Other" = "Black",
                                 "Black & Other" = "Other"
                                 )
  )  %>% 
  mutate_at(c("race2"), as.character) %>% 
  rename(sex = gender) %>% 
  select(-race)
  
ds_sui %>% glimpse()
ds_sui %>% group_by(cause) %>% count()
ds_sui %>% distinct(race3, race2, ethnicity) 

# ---- prepare-for-joining -------------------
# `race` has several definitions. Therefore, before joining, we must aggregate
# both files using the compatible aggregation

ds_pop_agg <- ds_pop %>% 
  group_by(county, year, sex, age, race3, ethnicity) %>% 
  summarize(
    n_population = sum(population, na.rm = T)
  )

ds_pop_agg

ds_sui_agg <- ds_sui %>% 
  group_by(county, year, sex, age, race3, ethnicity, cause) %>% 
  summarize(
    n_suicides = sum(n_suicides, na.rm = T)
  ) %>% 
  tidyr::pivot_wider(names_from = "cause", values_from = "n_suicides") %>% 
  rename(
    n_firearms = Firearms, n_other = Other
  ) %>% 
  mutate(
    n_suicides = sum( n_firearms, n_other, na.rm=T)
  )
ds_sui_agg

# ---- join-data ---------------

ds0 <- ds_pop_agg %>% 
  left_join(ds_sui_agg, by = c("year"
                               ,"county"
                               ,"sex"
                               ,"age"
                               ,"race3"
                               ,"ethnicity")) %>% 
  mutate_at(c("n_firearms","n_other","n_suicides"), ~stringr::str_replace_na(.,0)) %>% 
  mutate_at(c("n_firearms","n_other","n_suicides"), as.integer) %>% 
  ungroup()
  
ds1 <- ds0 %>%   
  mutate_at(c("county", "sex","race3","ethnicity"), forcats::as_factor) %>% 
  select(county, year, sex, age, race3, ethnicity, n_population, n_suicides, 
         n_firearms, n_other)

ds1 %>% glimpse()

ds1 %>% readr::write_rds("./data-unshared/derived/9-population-suicide-2.rds", compress = "gz")


# ----- test-data ----------------
# boxplot with years as obs for each age bin
ds0 %>% glimpse()
d4 <- ds0 %>% 
  mutate(age = as.integer(age)) %>% 
  filter(age %in% c(10:85)) %>%
  filter(year %in% 2006:2018) %>% 
  group_by(year, age) %>% 
  summarize(
    n_suicide        = sum(n_suicides, na.rm = T)
  ) %>% 
  ungroup()
d4 %>% glimpse()



g4 <- d4 %>% 
  ggplot(aes(x = age, y = n_suicide))+
  geom_smooth(method = "lm", se= F, size = 1,color = "salmon")+
  geom_smooth(method = "loess", se= F, size = 1,color = "cyan3")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, color = "salmon"
    # , vjust = 7
  )+
  geom_boxplot(aes( group = age), fill = NA)+
  scale_x_continuous(breaks = seq(10,85,5))+
  # scale_y_continuous(breaks = seq(0,100,10))+
  geom_vline(xintercept = 24.5, size = 4, alpha = .1)+
  geom_vline(xintercept = 17.5, size = 1, linetype = "dashed", color = "grey80")+
  theme(
    panel.grid.minor = element_blank()
  )+
  labs(
    title = "Suicide events among person of the same age (2006-2018)"
    ,x = "Age in years", y = "Count of suicides (all causes)"
  )
g4

# ---- compute-rate-function ----

compute_rate <- function(
  d
  ,grouping_frame
  ,wide = FALSE
){
  #test variables
  # d <- ds0
  # grouping_frame = c("county", "year")
  #end test
  
  d_wide <- d %>% 
    group_by_at(c(grouping_frame, "cause")) %>% 
    summarise(
      n_population = sum(population, na.rm = TRUE)
      ,n_suicides  = sum(n_suicides, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(
      rate_suicide = ((n_suicides/n_population)*100000)
    )
  
  d_long <- d_wide %>% 
    tidyr::pivot_longer(c(n_suicides,rate_suicide)
                        ,names_to = "metric" 
                        ,values_to = "value" 
                        )
  
  if(wide){
    return(d_wide)
  }
  
  return(d_long)
  
}
  
#how to use
ds_example   <- ds0 %>% compute_rate(grouping_frame = c("county", "year"))
ds_example_w <- ds0 %>% compute_rate(grouping_frame = c("county", "year"), wide = TRUE)

# ---- save-to-disk ---------



# ---- store-data ----
# 
# ls_grouping_frame <- list(
#    c("county","year"                                )
#   ,c("county","year","gender"                       )
#   ,c("county","year"         ,"race_ethnicity"      )
#   ,c("county","year"                          ,"age")
#   ,c("county","year","gender","race_ethnicity"      )
#   ,c("county","year"         ,"race_ethnicity","age")
#   ,c("county","year","gender"                 ,"age")
#   ,c("county","year","gender","race_ethnicity","age")
#   ,c(         "year"                                )
#   ,c(         "year","gender"                       )
#   ,c(         "year"         ,"race_ethnicity"      )
#   ,c(         "year"                          ,"age")
#   ,c(         "year","gender","race_ethnicity"      )
#   ,c(         "year"         ,"race_ethnicity","age")
#   ,c(         "year","gender"                 ,"age")
#   ,c(         "year","gender","race_ethnicity","age")
# )
# 
# 
# 
# #loop through all combos of grouping frame to store data
# 
# for(i in seq_along(ls_grouping_frame)){
#   path_to_folder <- "./data-unshared/derived/rate/"
#   frame_i        <- ls_grouping_frame[[i]]
#   file_name      <- paste0(path_to_folder, paste0(frame_i, collapse = "-"),".rds")
#   d_computed     <- ds0 %>% compute_rate(grouping_frame = frame_i)
#   
#   d_computed %>% readr::write_rds(file_name, compress = "gz")
# }


# store youth data 


ds_youth <- ds0 %>% filter(age %in% 10:24)


for(i in seq_along(ls_grouping_frame)){
  path_to_folder <- "./data-unshared/derived/rate/youth/"
  frame_i        <- ls_grouping_frame[[i]]
  file_name      <- paste0(path_to_folder
                           ,paste0(frame_i, collapse = "-")
                           ,"-10-24"
                           ,".rds")
  d_computed     <- ds_youth %>% compute_rate(grouping_frame = frame_i)
  
  d_computed %>%readr::write_rds(file_name, compress = "gz")
}
