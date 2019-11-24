rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
library(viridis)
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
    # device   = jpeg,
    path     = "./analysis/talahassee/age12_18/prints/", # female marital educ poor_healt
    # width    = 700,
    # height   = 700,
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

# ds_sex %>% readr::write_csv("./data-unshared/derived/talahassee/12_18/age12_18-sex.csv")
# ds_sex_race %>% readr::write_csv("./data-unshared/derived/talahassee/12_18/age12_18-sex-race.csv")

# ---- basic-table --------------------------------------------------------------

ds_sex_race <- ds_sex_race %>% 
  dplyr::mutate(
  value  = ifelse(value == Inf | value == -Inf, NA, value) 
)


# ---- manual-10-24 --------------------------
# manually edited file on 2019-11-22
rates_10_24 <- readr::read_csv("./data-unshared/raw/talahassee/10_24/population_counts-mortality_causes(all)-ages10_24.csv")
names(rates_10_24) <- c("race","ethnicity", c(2013:2018) )
ds1024 <- rates_10_24 %>% 
  tidyr::gather("year","rate", as.character(c(2013:2018))) %>% 
  dplyr::mutate(race_ethnicity = paste0(race, " + ", ethnicity)) %>%
  dplyr::select(-race, -ethnicity) %>%
  dplyr::select(race_ethnicity, dplyr::everything())

fig1 <- ds1024 %>% 
  ggplot(aes(x = year, y= rate, fill = race_ethnicity, color = race_ethnicity))+
  geom_line(aes(group = race_ethnicity), size = 1)+
  geom_point( size = 4, shape = 21, color = "black", alpha = .5)+
  # scale_fill_viridis(discrete  = TRUE, option = "D") +
  # scale_color_viridis(discrete = TRUE, option = "D")+
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Dark2" ))+
  scale_fill_manual(values = RColorBrewer::brewer.pal(4,  "Dark2" ))+
  scale_y_continuous(breaks = c(1:12))+
  theme_bw()+
  labs(
    title = "Rate of suicides per 100,000 among 10-24 year-old Floridians",
    y = "Rate per 100,000",
    x = "Year"
    ,fill = "Race category"
    ,color = "Race category"
  )+
  theme(
    panel.grid.minor.y = element_blank()
  )
fig1 %>% quick_save("fig1_rate_10_24_race",width = 550, height = 300, dpi = 400, scale = 1.2)
# fig1 %>% quick_save("fig1_rate_10_24_race",width = 600, height = 300, dpi = 400)

# ---- basic-graph --------------------------------------------------------------
ds_sex %>% glimpse()
ds_sex_race %>% glimpse()

fig2 <- ds_sex %>%
  dplyr::filter(year %in% c(2013:2018)) %>% 
  dplyr::filter(measure == "rate") %>% 
  dplyr::filter(sex == "Total") %>% 
  dplyr::filter(mortality_cause == "Total") %>% #View()
  ggplot(aes(x = year, y = value ))+
  geom_point( size = 4, shape = 21, fill = "salmon", alpha = .7)+
  geom_line()+
  geom_text(aes(label = value), nudge_y = .2)+
  theme_bw()+
  labs(
    title = "Rate of suicides among 12-18 year-old Floridians",
    y = "Rate per 100,000",
    x = "Year"
    
  )
fig2 %>% quick_save("fig2_rate_12_18",width = 500, height = 300, dpi = 400, scale = 1)


# Female suicides by means
fig3 <- ds_sex_race %>%
  dplyr::filter(year %in% c(2013:2018)) %>% 
  dplyr::filter(measure == "rate") %>% 
  dplyr::filter(sex == "Female") %>% 
  dplyr::filter(race != "blother") %>% 
  dplyr::filter(mortality_cause == "Total") %>% #View()
  ggplot(aes(x = year, y = value, color = race, fill = race))+
  geom_line(aes(group = race), size = 1)+
  geom_point( size = 4, shape = 21, color = "black", alpha = .5)+
  scale_y_continuous(breaks = c(1:6))+
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(n = 3, name = "Set1")), labels = c("Black + Other","Latino","White"))+
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(n = 3, name = "Set1")), labels = c("Black + Other","Latino","White"))+
  # scale_fill_discrete(labels = "Black + Other","Latino", "White")+
  # scale_co_discrete(labels = "Black + Other","Latino", "White")+
  theme_bw()+
  labs(
    title = "Rate of suicides in Florida among 12-18 year-old FEMALES",
    y = "Rate per 100,000",
    x = "Year",
    fill = "Race category", color = "Race category"
  )
fig3 %>% quick_save("fig3_rate_12_18_race_female", width = 500, height = 300, dpi = 400)


# baseSize = 28
fig4 <- ds_sex_race %>% 
  dplyr::filter(year %in% c(2013:2018)) %>% 
  dplyr::filter(measure == "rate") %>% 
  dplyr::filter(sex == "Total") %>% 
  dplyr::filter(race != "black") %>% 
  dplyr::filter(mortality_cause != "Total") %>% #View()
  # dplyr::recode(race, `blother`='Black + Other', `latino`='Latino', `white` = "White") %>% 
  dplyr::mutate(
    race =  car::recode(
      race,
      "
      'blother'  ='Black + Other'
      ;'latino'='Latino'
      ;'white'='White  '
      "
    )
  ) %>% 
  dplyr::mutate(race_cause = paste0(race, " by ", mortality_cause)) %>% 
  ggplot(aes(x = year, y = value, fill = race_cause))+
  geom_bar(stat="identity", position = "dodge", color = "black")+
  scale_y_continuous(breaks = c(1:5))+
  scale_x_continuous(breaks = c(2013:2018))+
  # geom_text(aes(label = value))+
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, "Paired"))+
  theme_bw()+
  labs(
    title = "Rate of suicide among 12-18 year-old Floridians by race category and lethal means",
    y = "Rate per 100,000",
    x = "Year",
    fill = "Race category by \n Lethal Means"
  )
fig4 %>% quick_save("fig4_rate_12_18_race_cause", width = 900, height = 450, dpi = 400)


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

