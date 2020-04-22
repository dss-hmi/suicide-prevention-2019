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
    
    
    
    
    

    output$aPlot <- renderPlot({
        x_value     <- "year"
        y_value     <- input$y_value_select
        facet_col   <- "race_ethnicity"
        facet_row   <- "sex"
        color_value <- "suicide_cause"
        age_group_filter <- c("10_14", "15_19", "20_24")
        
        facet_row_col <- paste0(facet_row,"~",facet_col)
        
        
        d <- ds0 %>% 
            filter(age_group %in% age_group_filter) %>% 
            compute_rate(
                grouping_frame = c(x_value,facet_row,facet_col)
            )
        
        d <- d$long %>% 
            filter(suicide_cause == "suicide") 
        
        g <- d %>%  
            make_facet_graph(
                x_aes       = x_value
                ,y_aes      = y_value
                ,color_aes  = color_value
                ,facet_expr = facet_row_col
                ,smooth     = TRUE)
        g
        
        
       
    })

})
