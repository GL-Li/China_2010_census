# total population vs year of birth ===========================================
plotTotalPopul <- ggplot(data = popTotal, aes(x = birth_year, y = population / 1e6)) +
    geom_line(aes(color = sex)) + geom_point(aes(color = sex)) +
    scale_x_continuous(breaks = seq(1910, 2010, by = 20)) + 
    xlab("Year of Birth") + 
    ylab("Population (million)") + 
    geom_text(aes(label = text1), vjust = 2) +
    theme(legend.position = c(1, 0),
          legend.justification = c(1, 0),
          legend.title=element_blank()) +
    myThemeAxis
output$totalPopul <- renderPlot({
    plotTotalPopul
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
    label <- paste0("(", label1, ") ", label2)
    # break into short lines, code from http://goo.gl/y8zzOq
    label <- gsub('(.{1,25})(\\s|$)', '\\1\n', label)
    output$totalPopul <- renderPlot({
        if (sum(grepl(label1, LETTERS)) == 1) {
            plotTotalPopul + 
                geom_label(data = data.frame(), aes(x = 1910, y = 14.5), 
                           vjust = 1, hjust = 0, size = 4,
                           label = label, alpha = 0.6)
        } else {
            plotTotalPopul
        }
    })
})


# percent of total population vs year of birth ================================
plotTotalPercent <- ggplot(data = popTotal, aes(x = birth_year, y = percent)) +
    geom_line(aes(color = sex)) + geom_point(aes(color = sex)) +
    scale_x_continuous(breaks = seq(1910, 2010, by = 20)) +
    xlab("Year of Birth") +
    ylab("Percentage (%)") +
    geom_text(aes(label = text2), vjust = 2) +
    theme(legend.position = "") +
    myThemeAxis
output$totalPercent <- renderPlot({
    plotTotalPercent
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
    label <- paste0("(", label1, ") ", label2)
    # break into short lines, code from http://goo.gl/y8zzOq
    label <- gsub('(.{1,50})(\\s|$)', '\\1\n', label)
    
    output$totalPercent <- renderPlot({
        if (sum(grepl(label1, LETTERS)) == 1) {
            plotTotalPercent + 
                geom_label(data = data.frame(), aes(x = 1930, y = 70), 
                           vjust = 1, hjust = 0, size = 4,
                           label = label, alpha = 0.6)
        } else {
            plotTotalPercent
        }
    })
})


# compare population in city, town, and village ==============================
plotComparePopul <- ggplot(data = popCombined, aes(x = age, y = population / 1e6)) + 
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
# start up plot
output$comparePopul <- renderPlot({
    plotComparePopul
    }
)
# when clicked
observeEvent(input$clickComparePopul, {
    x <- input$clickComparePopul$x
    y <- input$clickComparePopul$y
    panel <- input$clickComparePopul$panelvar1
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
    label <- paste0("(", label1, ") ", label2)
    # break into short lines, code from http://goo.gl/y8zzOq
    label <- gsub('(.{1,27})(\\s|$)', '\\1\n', label)
    print(label)
    
    output$comparePopul <- renderPlot({
        if (sum(grepl(label1, LETTERS)) == 1) {
            plotComparePopul + 
                geom_label(data = data.frame(x = 100, y = 6.5, lab = label, where = panel), 
                          aes(x, y, label = lab),alpha = 0.6,
                          vjust = 1, hjust = 0, size = 4)
        } else {
            plotComparePopul
        }
    })
})
