#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Suicide Prevention"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId  = "y_value_select"
                        ,label   = "Y Axis"
                        ,choices = c("rate_suicides"
                                     ,"n_suicides")
                        ,selected = "n_suicides")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("aPlot",width = 900, height = 500)
        )
    )
))
