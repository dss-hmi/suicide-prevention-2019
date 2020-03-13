


# load libraries ----------------------------------------------------------
library(shiny)
library(magrittr) # pipes
library(dplyr)    # manipulation
library(ggplot2)  # graphs

# ---- declare-globals ---------------------------------------------------------
# Used for Runing App
path_input_population <- "../../data-unshared/derived/1-greeted-population.rds"
#Used for testing script
# path_input_population <- "./data-unshared/derived/1-greeted-population.rds"


# ---- load-data ---------------------------------------------------------------
ds_population  <- readRDS(path_input_population)


# ---- tweak-data ---------------------------------------------

lvl_age_group <- c(
    "less_than_1"         =   "<1"          
    ,"1_4"                =   "1-4"  
    ,"5_9"                =   "5-9"  
    ,"10_14"              =   "10-14"    
    ,"15_19"              =   "15-19"    
    ,"20_24"              =   "20-24"    
    ,"25_34"              =   "25-34"    
    ,"35_44"              =   "35-44"    
    ,"45_54"              =   "45-54"    
    ,"55_64"              =   "55-64"    
    ,"65_74"              =   "65-74"    
    ,"75_84"              =   "75-84"    
    ,"85_plus"            =   "85+"      
)

ds_population1 <-  ds_population %>% 
    filter(year >= 2006 ) %>% 
    mutate(
        racethnicity = paste0(race, " + ", ethnicity)
        ,age_group   = factor(
            age_group
            ,levels = names(lvl_age_group)
            ,labels = lvl_age_group
        )
        ,year        = factor(
            year
            ,levels = year
            ,labels = year
        )
    )


ds_population_totals <- ds_population1 %>% 
    group_by(year,sex,racethnicity,age_group) %>% 
    summarise(
        n_people = sum(count, na.rm = TRUE)
    ) %>% 
    ungroup()



# ui ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- 
    navbarPage(""  #Nav Bar Title
        #Begin Population Panel
        ,tabPanel("Population",  
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        "age_group"
                        ,label    = "Select Age Group"
                        ,choices  = ds_population_totals$age_group
                        ,selected = "less_than_1"
                    )
                    ,selectInput(
                        "year"
                        ,label    = "Select Year"
                        ,choices  = levels(ds_population_totals$year)
                        ,selected = levels(ds_population_totals$year[1])
                    )
                    
                )
                ,mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Count"
                            ,plotOutput("population_count")
                                )
                        ,tabPanel(
                            "Distribution"
                            ,plotOutput("age_distro")
                            )
                        
                    )
                )
            )
        ) 
        # End Population
        # Begin Suicide
        ,tabPanel("Suicide Counts and Rates")
    )

    



# server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$population_count <-  renderPlot({ 
        
        g2 <-  ds_population_totals %>% 
            filter(age_group == input$age_group) %>%   
            ggplot(
                aes(
                    x      = year
                    ,y     = n_people
                    ,color = racethnicity
                    ,group = racethnicity
                    )
                ) +
            geom_line() +
            geom_point(
                shape = 1
                ,size = 2) +
            theme_bw() +
            scale_y_continuous(labels = scales::comma) +
            facet_grid(sex ~ .) 
        
        g2
        
    })
    
    output$age_distro <- renderPlot({
        
        g3 <- ds_population_totals %>% 
            filter(year == input$year) %>% 
            ggplot(aes(x = age_group, y = n_people)) +
            geom_col() +
            facet_grid(sex ~ racethnicity)
        g3
    })
}

# run app -----------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
