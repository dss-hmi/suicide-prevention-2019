# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md")
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
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr) # disable when temp lines are removed
library(ggplot2)
library(ggpubr)
library(readxl)
# ---- declare-globals ---------------------------------------------------------
path_input       <- "./data-unshared/raw/FloridaRankings/"


file_names <- list.files(path_input)
file_years <-  gsub("^(\\d{4}) (.+)", "\\1", file_names)
file_paths <- list.files(path_input, full.names = T)

#list of file paths with index as years
names(file_paths) <- file_years


# get a list of all sheets in all files,returns a list
extract_sheets <- function(file_paths, file_years){
  l <- list()
  for (i in c(1:length(file_years))){
    l[[as.character(file_years[i])]] <- excel_sheets(file_paths[i])
  } 
  return (union_of_sheets)
}
all_sheets <- extract_sheets(file_paths,file_years)

#create union of all sheets
union_of_sheets <- Reduce(union,all_sheets)

#create matrix with x = years, y = total sheets
m1 <- matrix(nrow = length(file_years), ncol=length(union_of_sheets))
rownames(m1) <- file_years
colnames(m1) <- union_of_sheets

#fill cells
for (sheet_name in union_of_sheets){
  for(j in c(1:length(file_years))){
    if(sheet_name %in% all_sheets[[j]]){
      m1[j,sheet_name] = 1
    }
    else{
      m1[j,sheet_name] = 0
    }
  } 
}
m1





