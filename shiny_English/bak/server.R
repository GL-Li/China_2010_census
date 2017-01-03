library(shiny)
library(ggplot2)

server <- function(input, output, session) {
    ##################### actually ui side ####################################
    # population ==============================================================
#     output$population <- renderUI({
#         source("./partials/population.R", local = TRUE)$value
#     })
#     
    
    
    ##################### real server side ####################################
    # overview ================================================================
    
    
    # population ==============================================================
    source("./controllers/population.R", local = TRUE)
    
    # education ============================================================
    source("./controllers/education.R", local = TRUE)
    
    # house ===================================================================
    source("./controllers/house.R", local = TRUE)
}