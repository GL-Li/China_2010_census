# bar plot ====================================================================
output$eduBar <- renderPlot({
        ggplot(totalDf, aes(edu, numb)) + 
            geom_bar(stat = "identity", fill = "blue") +
            geom_text(aes(label = round(numb,2)), vjust = -1) +
            expand_limits(y = c(0, 600)) +
            ggtitle("population at each education level") + 
#             xlab("highest  education") + 
            ylab("Population (million)") +
            theme(axis.text.x = element_text(angle = 18, hjust = 1, 
                                             color = "black", size = 12),
                  axis.title.x = element_blank()) + 
            myThemeAxis
    })

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# level of education vs age after clicking bar plot ===========================
# start up plot
output$eduCompare <- renderPlot({
    ggplot(eduCombined, aes(age, university)) + 
        facet_wrap(~where) +
        ylim(c(0, 100)) +
        ggtitle("click on the bar plot to view details at each education level") +
        xlab("Age") + 
        ylab("Percent of Population (%)") +
        theme(legend.position = c(0.31, 1), 
              legend.justification = c(1, 1),
              legend.title = element_blank()) +
        theme(strip.text.x = element_text(size = 12)) +
        myThemeAxis
})
# after click
observeEvent(input$clickBar, {
    # make y non-reactive, anything inside renderXyz is reactive, that is, 
    # when zoom in and out, click value changes. should avoid
    x <- input$clickBar$x
    colNumb <- round(x) + 2
    eduCombined$temp <- 100 * eduCombined[[colNumb]] / eduCombined$population
    colName <- colnames(eduNation)[colNumb]
    output$eduCompare <- renderPlot(       
        expr = {
            # use the percentage 
            ggplot(eduCombined, aes(age, temp)) + 
                geom_line(aes(color = sex)) + 
                geom_point(aes(color = sex)) + 
                facet_wrap(~where) +
                ggtitle(gsub("_", "  ", colName)) + 
                xlab("Age") + 
                ylab("Percent of Population (%)") +
                theme(legend.position = c(0.31, 1), 
                      legend.justification = c(1, 1),
                      legend.title = element_blank()) +
                theme(strip.text.x = element_text(size = 12)) +
                myThemeAxis
        })        
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# percentage from city, town, and village at education level =================
output$eduWhere <- renderPlot({
    ggplot(eduWherePercent, aes(education, percent, group = where, color = where)) +
        geom_point() + geom_line() + 
        ggtitle("where are people at each education level?") + 
        #             xlab("highest  education") + 
        ylab("Percentage (%)") +
        theme(axis.text.x = element_text(angle = 18, hjust = 1, 
                                         color = "black", size = 12),
              axis.title.x = element_blank()) + 
        theme(legend.position = c(1, 0.7), 
              legend.justification = c(1, 1),
              legend.title = element_blank()) +
        myThemeAxis + 
        scale_color_manual(values = c("city" = "blue", "town" = "green",
                                      "village" = "red"))
        
})

# percent of male and female
output$eduGender <- renderPlot({
    ggplot(eduGenderPercent, aes(education, percent, 
                                 group = gender, color = gender)) +
        geom_point() + geom_line() + 
        ggtitle("male and female at each education level") + 
        #             xlab("highest  education") + 
        ylab("Percentage (%)") +
        theme(axis.text.x = element_text(angle = 18, hjust = 1, 
                                         color = "black", size = 12),
              axis.title.x = element_blank()) + 
        theme(legend.position = c(1, 1), 
              legend.justification = c(1, 1),
              legend.title = element_blank()) +
        myThemeAxis
})
