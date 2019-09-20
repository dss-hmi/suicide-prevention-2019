# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
# These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console 


# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems

# ---- load-sources ------------------------------------------------------------

# ---- load-data -------------------------------------------------------------
ds <- readr::read_csv("data-unshared/derived/2-greeted-suicide.csv")

# ---- inspect-data -------------------------------------------------------------
print(ds,n = 20 )
dplyr::glimpse(ds)
# ---- tweak-data --------------------------------------------------------------
# let us count the number of times each value of the varialbe sex appears in the dataset 
ds %>% 
  dplyr::group_by(sex) %>% 
  dplyr::count()

# let us graph this table
ds %>% 
  dplyr::group_by(sex) %>% 
  dplyr::count() %>% 
  ggplot2::ggplot(aes(x = sex , y = n )) +
  geom_bar(stat = "identity")
# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------




# ---- publish ---------------------------------------
