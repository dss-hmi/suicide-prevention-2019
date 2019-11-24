rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html

# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R")        # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions

# ---- load-globals ------------------------------------------------------------

# ---- declare-globals --------------------------------------------------------
color_sex <- c(
  "Male" = "blue"
  ,"Female" = "pink"
  ,"Total"  = "black"
)

color_cause <- c(
  "Firearms" = "salmon"
  ,"Other"   = "green"
  ,"Total"   = "black"
  
)

color_race <- c(
  "black"    = "grey"
  ,"blother" = "lightblue"
  ,"latino" = "yellow"
  ,"white" = "purple"
)

# utility-functions -------------------------------------------------------

quick_save <- function(g,name,...){
  ggplot2::ggsave(
    filename = paste0(name,".png"), 
    plot     = g,
    device   = png,
    path     = "./analysis/talahassee/age12_18/prints/", # female marital educ poor_healt
    # width    = width,
    # height   = height,
    # units = "cm",
    # dpi      = 50,
    limitsize = FALSE,
    ...
  )
}

# ---- load-data -------------------------------------------------------------
ls_ds_wide <- readRDS("./data-unshared/derived/talahassee/12_18/ls_ds_wide.rds")
ls_ds_long <- readRDS("./data-unshared/derived/talahassee/12_18/ls_ds_long.rds")
ls_ds_long2 <- readRDS("./data-unshared/derived/talahassee/12_18/ls_ds_long2.rds")
ls_ds_long3 <- readRDS("./data-unshared/derived/talahassee/12_18/ls_ds_long3.rds")

ds_sex_cause      <- ls_ds_long3$cause_sex
ds_sex_cause_race <- ls_ds_long3$cause_sex_race
ds_cause_sex      <- ls_ds_long3$sex_cause
ds_cause_sex_race <- ls_ds_long3$sex_cause_race
# ---- inspect-data -------------------------------------------------------------
ds_sex_cause %>% glimpse()

# tweek-data --------------------------------------------------------------

ls_ds_long4 <- ls_ds_long3
for(i in seq_along(ls_ds_long3) ){
  
  ls_ds_long4[[i]] <- ls_ds_long3[[i]] %>% 
  # d <- ds_sex_cause %>% 
    tidyr::gather("measure","value", c("rate", "count", "pct_change_rate", "pct_change_count")) %>% 
    dplyr::mutate(
      measure = factor(
        measure
        ,levels = c("rate", "count", "pct_change_rate", "pct_change_count")
        )
    )
  
}
# ds_sex_cause      <- ls_ds_long4$cause_sex
# ds_sex_cause_race <- ls_ds_long4$cause_sex_race
# ds_cause_sex      <- ls_ds_long4$sex_cause
# ds_cause_sex_race <- ls_ds_long4$sex_cause_race

ds_sex      <- dplyr::bind_rows(
  ls_ds_long4$cause_sex
  , ls_ds_long4$sex_cause
) %>% 
  dplyr::select(-order) %>% 
  dplyr::distinct()


ds_sex_race <- dplyr::bind_rows(
  ls_ds_long4$cause_sex_race
  , ls_ds_long4$sex_cause_race
) %>% 
  dplyr::select(-order) %>% 
  dplyr::distinct()

ds_sex %>% readr::write_csv("./data-unshared/derived/talahassee/12_18/age12_18-sex.csv")
ds_sex_race %>% readr::write_csv("./data-unshared/derived/talahassee/12_18/age12_18-sex-race.csv")

# ---- basic-table --------------------------------------------------------------

ds_sex_race <- ds_sex_race %>% 
  dplyr::mutate(
  value  = ifelse(value == Inf | value == -Inf, NA, value) 
)


# ---- basic-graph --------------------------------------------------------------
ds_sex %>% glimpse()
ds_sex_race %>% glimpse()


# g0 <- ds_sex %>%
#   ggplot(aes(x = year, y = value, color = sex) )+
#   geom_point()+
#   geom_line(aes(group=sex))+
#   # facet_grid(measure ~ ., scales = "free")+
#   facet_grid(measure ~ mortality_cause, scales = "free")+
#   theme_bw()
# 
# g0 %>% print()
# 
# g0 <- ls_ds_long4$sex_cause %>% 
#   ggplot(aes(x = year, y = value, color = mortality_cause) )+
#   geom_point()+
#   geom_line(aes(group=mortality_cause))+
#   facet_grid(measure ~ sex, scales = "free")+
#   theme_bw()
# 
# g0 %>% print()


# Sonata form report structure



# ---- publish ---------------------------------------
path_report_1 <- "./reports/*/report_1.Rmd"
path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1,path_report_2)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      # "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

