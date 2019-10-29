# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md", figure)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./manipulation/stitched-output/0-greeter.md", )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below. 
base::source("./scripts/common-functions.R")

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
library(magrittr) # pipes
library(dplyr)    # disable when temp lines are removed
library(ggplot2)  # graphs
library(ggpubr)   # documents
  
# ---- declare-globals ---------------------------------------------------------
path_file_input <- "./data-unshared/derived/2-greeted-suicide.rds"
html_flip <- FALSE
baseSize <- 10
# ---- load-data ---------------------------------------------------------------
ds      <- readRDS(path_file_input)
ds %>% pryr::object_size(); dto %>% class(); dto %>% names()

# ----- custom-functions --------------------------------------

# ---- tweak-data ---------------------------------------------------------------
# to collapse into a single data frame
ds %>% explore::describe_all()

ds %>% 
  group_by(mortality_cause) %>% 
  count()


ds %>% 
  filter(year == 2017) %>%  
  filter(mortality_cause == "Suicide: Firearms Discharge (X72-X74)") %>% 
  group_by(age_group, sex,mortality_cause) %>% 
  count() %>% 
  ggplot(aes(x = age_group, y = n, fill = sex)) +
  geom_bar(stat="identity", position = "dodge", color = "black")+
  scale_fill_manual(values = c("Female" = "pink", "Male" = "skyblue"))+
  theme_minimal()
ds %>% 
  filter(year == 2017) %>%  
  filter(mortality_cause == "Suicide: Drugs & Biological Substances (X60-X64)") %>% 
  group_by(age_group, sex,mortality_cause) %>% 
  count() %>% 
  ggplot(aes(x = age_group, y = n, fill = sex)) +
  geom_bar(stat="identity", position = "dodge", color = "black")+
  scale_fill_manual(values = c("Female" = "pink", "Male" = "skyblue"))+
  theme_minimal()




# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------
rmarkdown::render(
  # input = "./analysis/gls-activity/gls-activity-1.Rmd"
  input = "./analysis/gls-activity/gls-activity-2-coverage.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



