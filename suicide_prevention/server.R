#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


source("./data-prep.R")
source("./functions.R")
# source("./suicide_prevention/data-prep.R")
# source("./suicide_prevention/functions.R")



library(shiny)




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
#need to update color, and facets dynamically.


    
#change from grid to wrap
# offer the user choice of free scales or not

    

    output$aPlot <- renderPlot({
        x_value     <- "year"
        y_value     <- input$y_value_select
        facet_row   <- input$facet_row_select
        facet_col   <- input$facet_col_select
        color_value <- input$color_select
        age_group_filter <- input$age_range_select
        suicide_type_value <- input$suicide_type_select
        smooth_value <- input$smooth_checkbox
        scales_values <- input$scales_checkbox
        group_value <-  NULL
        
        
        facet_row_col <- paste0(facet_row,"~",facet_col)
        
        grouping_frame_values <- c(x_value)
        
        if(!facet_row %in% c(".","suicide_cause")){
            grouping_frame_values <- c(grouping_frame_values,facet_row)
        }
            
        if(!facet_col %in% c(".","suicide_cause")) {
            grouping_frame_values <- c(grouping_frame_values,facet_col)
        }
        
        if(!color_value == "suicide_cause" && (facet_row %in% c(".","suicide_cause") | facet_col %in% c(".","suicide_cause"))) {
            grouping_frame_values <- c(grouping_frame_values,color_value)
            group_value <- color_value 
        }
        
        
        
        d <- ds0 %>% 
            filter(age_group %in% age_group_filter) %>% 
            compute_rate(
                grouping_frame = grouping_frame_values
            )
        
        d <- d$long %>% 
            filter(suicide_cause %in% suicide_type_value) 
        
        g <- d %>%  
            make_facet_graph(
                x_aes       = x_value
                ,y_aes      = y_value
                ,color_aes  = color_value
                ,group_aes  = group_value
                ,facet_expr = facet_row_col
                ,smooth     = smooth_value
                ,scales     = scales_values)
        g
        
        
       
    })

})


