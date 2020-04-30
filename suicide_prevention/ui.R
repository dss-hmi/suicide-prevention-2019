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

color_choices <-  c(
    "Race + Ethnicity"  = "race_ethnicity"
    ,"Sex"              = "sex"
    ,"Suicide Cause"    = "suicide_cause"
    
)





suicide_type <- c(
"Total Suicide"          = "suicide"
,"Drug"                  = "drug"
,"Gun"                   = "gun"
,"Hanging"               = "hanging"
,"Jump"                  = "jump"
,"Other Sequelae"        = "other_seq" 
,"Other Sol/Liq & Vapor" = "other_liq" 
,"Other Gases & Vapor"   = "other_gas"
,"Non-Gun"               = "non_gun"
,"Non-Gun,Hanging,Drug"  = "non_gun_hang_drug"
)


age_groups <- c(
"<1"       = "less_than_1"             
,"1-4"     = "1_4"                
,"5-9"     = "5_9"                
,"10-14"   = "10_14"                  
,"15-19"   = "15_19"                  
,"20-24"   = "20_24"                  
,"25-34"   = "25_34"                  
,"35-44"   = "35_44"                  
,"45-54"   = "45_54"                  
,"55-64"   = "55_64"                  
,"65-74"   = "65_74"                  
,"75-84"   = "75_84"                  
,"85+"     = "85_plus"             
)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Suicide Prevention"),

    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId   = "y_value_select"
                        ,label    = "Y Axis"
                        ,choices  = c("Rate"    = "rate_suicides"
                                      ,"Count"   = "n_suicides"
                                    )
                        ,selected = "n_suicides"
                        )
            ,selectInput(inputId  = "facet_row_select"
                         ,label   = "Facet Row"
                         ,choices = facet_choices)
            ,selectInput(inputId  = "facet_col_select"
                         ,label   = "Facet Column"
                         ,choices = facet_choices
                         )
            ,selectInput(
                inputId   = "suicide_type_select"
                ,label    = "Suicide Type Filter"
                ,choices  = suicide_type
                ,multiple = TRUE
                ,selected = "suicide"
            )
            ,selectInput(
                inputId   = "age_range_select"
                ,label    = "Choose Age Groups"
                ,choices  = age_groups
                ,multiple = TRUE
                ,selected = c("10_14","15_19","20_24")
            )
            ,selectInput(
                inputId   = "color_select"
                ,label    = "Color Select"
                ,choices  = color_choices
            )
            ,checkboxInput(inputId = "smooth_checkbox"
                           ,label  = "Linear Regression Line"
                           ,value  = FALSE)
            
            
        ),

        
        mainPanel(
            plotOutput("aPlot",width = 900, height = 500)
        )
    )
))
