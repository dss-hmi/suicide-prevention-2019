#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)


# not sure where to put this so its here for now
facet_choices <- c(
    "None"              = "."
    ,"Race + Ethnicity" = "race_ethnicity"
    ,"Sex"              = "sex"
    ,"Suicide Cause"    = "suicide_cause"
    
)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Suicide Prevention"),

    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId   = "y_value_select"
                        ,label    = "Y Axis"
                        ,choices  = c("rate_suicides", "n_suicides")
                        ,selected = "n_suicides"
                        )
            ,selectInput(inputId  = "facet_row_select"
                         ,label   = "Facet Row"
                         ,choices = facet_choices)
            ,selectInput(inputId  = "facet_col_select"
                         ,label   = "Facet Column"
                         ,choices = facet_choices
                         )
            
        ),

        
        mainPanel(
            plotOutput("aPlot",width = 900, height = 500)
        )
    )
))
