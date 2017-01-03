# bar plot ====================================================================
output$eduBar <- renderPlot({
        ggplot(totalDf, aes(edu, numb)) + 
            geom_bar(stat = "identity", fill = "blue") +
            coord_flip() + 
            ggtitle("Population at education levels") + 
            xlab("highest  education") + 
            ylab("Population (million)") +
            myThemeAxis
    })

# level of education vs age after clicking bar plot ===========================
observeEvent(input$clickBar, {
    # make y non-reactive, anything inside renderXyz is reactive, that is, 
    # when zoom in and out, click value changes. should avoid
    y <- input$clickBar$y
    colNumb <- round(y) + 2
    eduNation$temp <- 100 * eduNation[[colNumb]] / eduNation$population
    colName <- colnames(eduNation)[colNumb]
    output$ageDistr <- renderPlot(       
        expr = {
            # use the percentage 
            ggplot(eduNation, aes(age, temp)) + 
                geom_line(aes(color = sex)) + 
                geom_point(aes(color = sex)) + 
                ggtitle(gsub("_", "  ", colName)) + 
                xlab("Age") + 
                ylab("Percent of Population") +
                theme(legend.position = c(0.9, 0.85), 
                      legend.title = element_blank()) +
                myThemeAxis
        })        
})

# compare education in city, town, and villiage ===============================
output$eduCompare <- renderPlot({
    ggplot(eduCombined, aes(x = age, y = 100 * technical_college / population)) +
        geom_line(aes(color=factor(sex, levels = c("male", "female")))) + 
        geom_point(aes(color=factor(sex, levels = c("male", "female")))) +
        facet_wrap(~where) +
        xlab("Age (years)") + 
        ylab("Percent of Pupulation") + 
        theme(legend.position = c(0.9, 0.8),
              legend.title = element_blank()) +
        theme(strip.text.x = element_text(size = 12)) +
        myThemeAxis
})


