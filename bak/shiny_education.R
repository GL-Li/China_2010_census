library(shiny)
library(ggplot2)
load("china_education_age.RData")

# total population at each education level
total <- apply(education[4:11], 2, sum)
total_DF <- data.frame(edu = names(total), numb = total / 1e6)

# reorder factors for better bar plot
total_DF$edu <- factor(total_DF$edu, ordered = TRUE,
                       levels = names(education[4:11]))

# ui.R ========================================================================
ui <- fluidPage(
    fluidRow(
        column(width = 4, 
               plotOutput("bar", click = "clickBar")),
        column(width = 8,
               plotOutput("ageDistr"))
    )
)


# server.R ====================================================================
server <- function(input, output) {
    output$bar <- renderPlot(width = 300, height = 500, expr = {
        ggplot(total_DF, aes(edu, numb)) + 
            geom_bar(stat = "identity") +
            coord_flip() + 
            ggtitle("Population at education levels") + 
            xlab("highest  education") + 
            ylab("Population (million)")
    })
    observeEvent(input$clickBar, {
        output$ageDistr <- renderPlot(width = 600, height = 500, expr = {
            # use the percentage 
            colNumb <- round(input$clickBar$y) + 11
            colName <- colnames(education)[colNumb]
            ggplot(education, aes_string("age", colName)) + 
                geom_line(aes(color = sex)) + 
                geom_point(aes(color = sex)) + 
                ggtitle("Portion of the population at an education level as functions of age") + 
                xlab("Age") + 
                ylab(gsub("_", "  ", colName))
        })        
    })
}

shinyApp(ui, server)