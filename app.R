


# load libraries ----------------------------------------------------------
library(shiny)
library(magrittr) # pipes
library(dplyr)    # manipulation
library(ggplot2)# graphs
library(lubridate) # dates

# ---- declare-globals ---------------------------------------------------------
path_input_population <- "./data-unshared/derived/1-greeted-population.rds"


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

ui <- 
    navbarPage(""  #Nav Bar Title
        #Begin Population Panel
        ,tabPanel(
            "Population",
            tabsetPanel(
                type = "tabs"
                ,tabPanel(
                    "Panel 1"
                    ,sidebarLayout(
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
                            ,radioButtons(
                                "plot_choice"
                                ,label = "Choose Plot"
                                ,choices = c(
                                    "Total Population" = "total_population"
                                    ,"Age Breakdown"   = "age_breakdown"
                                )
                            )
                            ,downloadButton(
                                "downloadplot"
                                ,label = "Download Plot"
                            )
                        ) #close side bar panel 1
                        ,mainPanel(
                            plotOutput("population_count")
                            ,plotOutput("age_distro")
                        )
                    )
                )  #close Panel 1
                ,tabPanel(
                    "Panel 2"
                    ,verticalLayout(
                        titlePanel(
                            h1("Title", align = "center")
                        )
                        ,wellPanel(
                            fluidRow(
                                column(
                                    width = 6
                                    ,selectInput(
                                        "panel_2_sex"
                                        ,label    = "Choose Sex"
                                        ,choices  = ds_population_totals$sex
                                        ,multiple = TRUE
                                    )
                                )
                                ,column(
                                    width = 6
                                    ,selectInput(
                                        "panel_2_racethnicity"
                                        ,label    = "Choose Race + Ethnicity"
                                        ,choices  = ds_population_totals$racethnicity
                                        ,multiple = TRUE
                                    )
                                )
                            )
                        )
                        ,plotOutput(
                            NULL  #working on Plot
                        )
                    )
                )
            )# close Tabset for Population
        ) # Close Population Navbar
        # Begin Suicide Panel
        ,tabPanel("Suicide Counts and Rates")  # In Progress
    ) # close NavBar

    



# server ------------------------------------------------------------------

server <- function(input, output) {
    #define graphing functions to allow saving plots
    g2 <-function(){
            ds_population_totals %>% 
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
            # subtracting 2005 lines input up with factor levels
            geom_vline(
                xintercept = (as.numeric(input$year) - 2005)
                ,linetype  = "dashed"
                ,alpha     = 0.2
            ) +
            scale_y_continuous(labels = scales::comma) +
            facet_grid(sex ~ .) +
            labs(
                x      = NULL
                ,y     = "Total Persons"
                ,color = "Race + Ethnicity"
            )
        }

    g3 <- function(){
        ds_population_totals %>% 
        filter(year == input$year) %>% 
        ggplot(aes(x = age_group, y = n_people)) +
        geom_col(
            aes(fill = input$age_group == age_group)
            ,show.legend = FALSE
        ) +
        facet_grid(sex ~ racethnicity) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 270)) +
        scale_fill_brewer(palette = "Dark2") +
        labs(
            x  = NULL
            ,y = "Total Persons"
        )
    }
    

    output$population_count <-  renderPlot({ 
       g2()
    })
    
    output$age_distro <- renderPlot({
        g3()
    })
    
    plotInput <- reactive({
        switch(
            input$plot_choice
            ,"total_population" = g2()
            ,"age_breakdown"    = g3()
        )
    })
    
    
    output$downloadplot <- downloadHandler(
        filename = function() {
            paste(input$plot_choice, ".png", sep = "")
        }
        ,content = function(file) {
            ggsave(file, plot = plotInput() , device = "png")
        }
    )
    
}

# run app -----------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
