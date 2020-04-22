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
        d <- ds0 %>% 
            filter(age_group %in% age_groups_10_24) %>% 
            compute_rate(c("year","sex","race_ethnicity"))
        
        d <- d$long %>% 
            filter(suicide_cause == "suicide") 
        
        g <- d %>%  
            make_facet_graph(
                x_aes       = "year"
                ,y_aes      = "rate_suicides"
                ,color_aes  = "suicide_cause"
                ,facet_expr = "sex ~ race_ethnicity"
                ,smooth     = TRUE)
        g
        
        
       
    })

})
