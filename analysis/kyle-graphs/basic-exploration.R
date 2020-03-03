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

#static line  all age groups
ages <- c(65,75,20,15)

g1 <- ds_no_total %>% 
  filter(race == "white", mortality_cause == "Firearms")%>% 
  group_by(age) %>% 
  ggplot(aes(x = year, y = count, group = age, color = age)) +
  geom_line(show.legend = FALSE, na.rm = TRUE) +
  gghighlight(age %in% ages , use_direct_label = FALSE) +
  facet_grid(sex ~ .) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g1

#static line, 1 age group, ploted by race
g2 <- ds_no_total %>% 
  filter(mortality_cause == "Other", age_group == "25-34") %>% 
  group_by(race, year) %>% 
  summarise_at(c("rate"),sum, na.rm = TRUE) %>% 
  ggplot(aes(x = year, y = rate, group = race, color = race)) +
  geom_line() +
  geom_point()
g2

test <- ds_no_total %>% group_by(race, year) %>%   summarise_at(c("count"),sum, na.rm = TRUE)

# static histogram
g3 <- ds_no_total %>% 
  filter(race == "black") %>%  # could filter for year as well
  ggplot(aes(x = age_group, y = count)) +
  geom_col(na.rm = TRUE) +
  facet_grid(sex ~ .) +
  gghighlight(mortality_cause == "Firearms")
g3


