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


# ---- tweak-data ------------------------------

ds_population <- ds_population %>% 
  select(-race,-ethnicity) %>% 
  mutate_at(
    "county", ~stringr::str_replace_all(.
                                        ,c(
                                          "St. Johns"  = "Saint Johns"
                                          ,"St. Lucie" = "Saint Lucie"
                                          ,"DeSoto"    = "Desoto"
                                        ))
  )


ds_suicide <- ds_suicide %>%
  select(-race,-ethnicity) %>% 
  mutate(
    age    = if_else(age < 85,age,as.integer(86))
    ,cause = if_else(stringr::str_detect(cause,"Firearms"),"Firearms","Other")
  ) 


# --- join-data

ds0 <- ds_population %>% 
  left_join(ds_suicide, by = c("year"
                               ,"county"
                               ,"gender"
                               ,"age"
                               ,"race_f"
                               ,"ethnicity_f")) %>% 
  mutate_at("cause", ~stringr::str_replace_na(.,"None")) %>% 
  mutate_at("n_suicides", ~stringr::str_replace_na(.,0)) %>% 
  mutate_at("n_suicides", as.integer) %>% 
  mutate(
    race_ethnicity  = paste0(race_f, " + ", ethnicity_f)
    ,race_ethnicity = forcats::as_factor(race_ethnicity) 
    ,cause          = forcats::as_factor(cause)
    ,gender         = forcats::as_factor(gender)
  )

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


# ---- store-data ----

ls_grouping_frame <- list(
   c("county","year"                                )
  ,c("county","year","gender"                       )
  ,c("county","year"         ,"race_ethnicity"      )
  ,c("county","year"                          ,"age")
  ,c("county","year","gender","race_ethnicity"      )
  ,c("county","year"         ,"race_ethnicity","age")
  ,c("county","year","gender"                 ,"age")
  ,c("county","year","gender","race_ethnicity","age")
  ,c(         "year"                                )
  ,c(         "year","gender"                       )
  ,c(         "year"         ,"race_ethnicity"      )
  ,c(         "year"                          ,"age")
  ,c(         "year","gender","race_ethnicity"      )
  ,c(         "year"         ,"race_ethnicity","age")
  ,c(         "year","gender"                 ,"age")
  ,c(         "year","gender","race_ethnicity","age")
)



#loop through all combos of grouping frame to store data

for(i in seq_along(ls_grouping_frame)){
  path_to_folder <- "./data-unshared/derived/rate/"
  frame_i        <- ls_grouping_frame[[i]]
  file_name      <- paste0(path_to_folder, paste0(frame_i, collapse = "-"),".rds")
  d_computed     <- ds0 %>% compute_rate(grouping_frame = frame_i)
  
  d_computed %>% readr::write_rds(file_name, compress = "gz")
}


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
