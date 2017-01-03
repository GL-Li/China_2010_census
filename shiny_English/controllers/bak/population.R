# total population vs year of birth ===========================================
output$totalPopul <- renderPlot({
        ggplot(data = popTotal, aes(x = birth_year, y = population / 1e6)) +
            geom_line(aes(color = sex)) + geom_point(aes(color = sex)) +
            scale_x_continuous(breaks = seq(1910, 2010, by = 20)) + 
            xlab("Year of Birth") + 
            ylab("Population (million)") + 
            geom_text(aes(label = text1), vjust = 2) +
            theme(legend.position = c(0.9, 0.15), 
                  legend.text=element_text(size=11),
                  legend.title=element_blank()) +
            myThemeAxis
    }
)
observeEvent(input$clickPopulTotal, {
    x <- input$clickPopulTotal$x
    y <- input$clickPopulTotal$y
    year <- round(x)
    
    # tripple the click range
    label1 <- paste0(popTotal$text1[popTotal$birth_year == year][2],
                     popTotal$text1[popTotal$birth_year == year-1][2],
                     popTotal$text1[popTotal$birth_year == year+1][2])
    label2 <- paste0(popTotal$description1[popTotal$birth_year == year][2],
                     popTotal$description1[popTotal$birth_year == year-1][2],
                     popTotal$description1[popTotal$birth_year == year+1][2])
    print(label1)
    if (sum(grepl(label1, LETTERS)) == 1) {
        output$clickPopulTotalText <- renderText({
            paste0("(", label1, ")   ", label2)
        })
    } else {
        output$clickPopulTotalText <- renderText({
            ""
        })
    }
    
    
})


# percent of total population vs year of birth ================================
output$totalPercent <- renderPlot({
        ggplot(data = popTotal, aes(x = birth_year, y = percent)) +
            geom_line(aes(color = sex)) + geom_point(aes(color = sex)) +
            scale_x_continuous(breaks = seq(1910, 2010, by = 20)) +
            xlab("Year of Birth") +
            ylab("Percentage (%)") +
            geom_text(aes(label = text2), vjust = 2) +
            theme(legend.position = "") +
            myThemeAxis
    }
)
observeEvent(input$clickPercentTotal, {
    x <- input$clickPercentTotal$x
    y <- input$clickPercentTotal$y
    year <- round(x)
    label1 <- paste0(popTotal$text2[popTotal$birth_year == year][2],
                     popTotal$text2[popTotal$birth_year == year-1][2],
                     popTotal$text2[popTotal$birth_year == year+1][2])
    label2 <- paste0(popTotal$description2[popTotal$birth_year == year][2],
                     popTotal$description2[popTotal$birth_year == year-1][2],
                     popTotal$description2[popTotal$birth_year == year+1][2])
    if (sum(grepl(label1, LETTERS)) == 1) {
        output$clickPopulTotalText <- renderText({
            paste0("(", label1, ")   ", label2)
        })
    } else {
        output$clickPopulTotalText <- renderText({
            ""
        })
    }
})


# compare population in city, town, and villiage ==============================
output$comparePopul <- renderPlot({
        ggplot(data = popCombined, aes(x = age, y = population / 1e6)) + 
            geom_line(aes(color = sex)) + 
            geom_point(aes(color = sex)) +
            facet_wrap(~ where) + 
            scale_x_reverse(breaks = seq(0, 100, by = 20)) +
            xlab("Age") + 
            ylab("Population (million)") +
            geom_text(aes(label = text), vjust = -3) +
            theme(legend.position = "") +
            theme(strip.text.x = element_text(size = 12)) +
            myThemeAxis
        
    }
)
observeEvent(input$clickComparePopul, {
    x <- input$clickComparePopul$x
    y <- input$clickComparePopul$y
    panel <- input$clickComparePopul$panelvar1
    print(panel)
    age <- round(x)
    label1 <- paste0(popCombined$text[popCombined$age == age &
                                          popCombined$where == panel][2],
                     popCombined$text[popCombined$age == age-1 &
                                          popCombined$where == panel][2],
                     popCombined$text[popCombined$age == age+1 &
                                          popCombined$where == panel][2])
    label2 <- paste0(popCombined$description[popCombined$age == age &
                                                 popCombined$where == panel][2],
                     popCombined$description[popCombined$age == age-1 &
                                                 popCombined$where == panel][2],
                     popCombined$description[popCombined$age == age+1 &
                                                 popCombined$where == panel][2])
    if (sum(grepl(label1, LETTERS)) == 1) {
        output$clickPopulTotalText <- renderText({
            paste0("(", label1, ")   ", label2)
        })
    } else {
        output$clickPopulTotalText <- renderText({
            ""
        })
    }
})
