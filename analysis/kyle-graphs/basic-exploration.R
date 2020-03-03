rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
cat("\f") # clear console when working in RStudio


# load-sources ------------------------------------------------------------


# load-packages -----------------------------------------------------------

library(magrittr) #Pipes
library(dplyr) # manipulation
library(ggplot2) #graphing
library(gghighlight)


# declare-globals ---------------------------------------------------------

path_to_input <- "./data-unshared/derived/cause113-age10-age99/cause113-age10-age99.rds"


# load-data ---------------------------------------------------------------

ds0 <- readRDS(path_to_input) 



# filter data --------------------------------------------------------------

ds_no_total <- ds0 %>% filter(!age == "Total", !race == "Total")

ds_only_total <- ds0 %>% filter(age == "Total")


# graph-data --------------------------------------------------------------

#static line
ages <- c(65,75,20,15)

g1 <- ds_no_total %>% 
  filter(race == "white", mortality_cause == "Firearms")%>% 
  ggplot(aes(x = year, y = count, group = age, color = age_group)) +
  geom_line() +
  gghighlight(age %in% ages , use_direct_label = FALSE) +
  facet_wrap(~ sex) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g1
