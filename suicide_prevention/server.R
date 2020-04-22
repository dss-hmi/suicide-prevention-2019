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



library(shiny)





# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    
    
    

    output$aPlot <- renderPlot({
        major_causes <- c("gun","hanging","drug","non_gun","non_gun_hang_drug")
        d <- ds0 %>% 
            filter(age_group %in% age_groups_10_24) %>%
            # filter(year == 2017) %>% 
            compute_rate("year")
        d <- d$long
        g <- d %>%  
            filter(suicide_cause %in% major_causes) %>% 
            ggplot(aes(x = reorder(suicide_cause,-n_suicides), y = n_suicides)) +
            geom_col(alpha = 0.4) +
            geom_text(aes(label = n_suicides)) +
            coord_flip() +
            facet_wrap(~year)
        g
        
       
    })

})
