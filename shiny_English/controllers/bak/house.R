# click on a map to get distribution ==========================================
# get reactive variables from the input
roomDistr <- reactive(get(paste0("houseDistr", input$chooseWhere))$roomDistr )
plotRoomDistr <- reactive(get(paste0("houseDistr", input$chooseWhere))$plotRoomDistr)
roomPopularMap <- reactive(get(paste0("houseDistr", input$chooseWhere))$roomPopularMap)
where <- reactive(tolower(input$chooseWhere))


# start up plot of popular room numbers on China map
output$map <- renderPlot(
    height = function() {
        0.8 * session$clientData$output_map_width
    },
    expr = {
    ggplot(data = roomPopularMap(), aes(x = long, y = lat)) +
        geom_polygon(aes(group = group, fill = factor(popular)), color = "black") +
        ggtitle(paste0("most popular: ", where())) +
        scale_fill_manual(values = c("1" = "#5858FA", "2" = "#58FAF4",
                                     "3" = "#58FA58", "4" = "#F4FA58")) +
        theme(legend.position = c(0.92, 0.2), 
              legend.text=element_text(size=11),
              legend.title=element_blank()) +
        myThemeAxis +
        ylim(c(15, 55))
})


# start up plot of distribution, cannot fix coor ratio as not Cartisan
output$distr <- renderPlot(
    height = function() {
        0.8 * session$clientData$output_distr_width
    },
    expr = {
    ggplot(plotRoomDistr(), aes(num_rooms, percent)) + 
        geom_line(aes(group = Chinese), alpha = 0.3) + 
        geom_point(aes(group = Chinese), alpha = 0.3) + 
        ggtitle(paste0("distribution: all provinces -- ", where())) + 
        xlab ("Number of Rooms") +
        ylab("Percent of Household (%)") +
        theme(axis.text.x = element_text(angle = 18, hjust = 1)) + 
        myThemeAxis
})

# update when clicked
observeEvent(input$clickMap, {
    # force x and y NOT reactive, otherwise related variables disappear when
    # unclicked, input$clickMap$x and input$clickMap$y are always reactive
    x <- input$clickMap$x
    y = input$clickMap$y
    clicked_prov <- which_province(map, x, y, "province")
    # these two are two close to seperate, but we only need one
    if (clicked_prov == "Hong Kong") clicked_prov <- "Guangdong"
    
    # back to room data, no data for Taiwan
    if (clicked_prov != "Taiwan") {
        room_prov <- as.numeric(roomDistr()[roomDistr()$province == clicked_prov, 2:12])
    } else {
        room_prov <- c(1,rep(0, 10))
        #           names(roomProv) <- names(rooms[3:12]) 
    }
    room_DF <- data.frame(num_room = factor(names(roomDistr()[3:12]), ordered = TRUE,
                                            levels = names(roomDistr()[3:12])),
                          household = room_prov[2:11] / room_prov[1] )
    # for geom_label( )
    if (clicked_prov != "") {
        labelDf <- data.frame(long = x, lat = y, text = clicked_prov)
    } else {
        labelDf <- data.frame(long = x, lat = y, text = NA)
    }
    
    
    output$map <- renderPlot(
        height = function() {
            0.8 * session$clientData$output_map_width
        },
        expr = {
            ggplot(data = roomPopularMap(), aes(x = long, y = lat)) +
                geom_polygon(aes(group = group, fill = factor(popular)), color = "black") +
                ggtitle(paste0("most popular: ", where())) +
                scale_fill_manual(values = c("1" = "#5858FA", "2" = "#58FAF4",
                                             "3" = "#58FA58", "4" = "#F4FA58")) +
                theme(legend.position = c(0.92, 0.2), 
                      legend.text=element_text(size=11),
                      legend.title=element_blank()) +
                myThemeAxis +
                ylim(c(15, 55)) +
#                 geom_polygon(data = map[map$province == clicked_prov,], 
#                          aes(long, lat, group = group), fill = "orange") +
                geom_path(data = map[map$province == clicked_prov,], 
                          aes(long, lat, group = group), color = "orange", size = 1) +
                geom_label(data = labelDf, aes(long, lat, label = text), 
                           alpha = 0.5, na.rm = TRUE, color = "red")
#                 annotate("text", x = x, y = y, label = clicked_prov, color = "red")
    })
    
    
    selected_prov <- plotRoomDistr()[plotRoomDistr()$province == clicked_prov,]
    output$distr <- renderPlot(
        height = function() {
            0.8 * session$clientData$output_distr_width
        },
        expr = {
        ggplot(plotRoomDistr(), aes(num_rooms, percent)) + 
            geom_line(aes(group = province), alpha = 0.3) + 
            geom_point(aes(group = province), alpha = 0.3) + 
            ggtitle(paste0("distribution: ", clicked_prov, " -- ", where())) + 
#             theme(plot.title = element_text(color = "red")) + 
            xlab ("Number of Rooms") +
            ylab("Percent of Household (%)") +
            geom_line(data = selected_prov, aes(num_rooms, percent, group = province), color = "red") + 
            geom_point(data = selected_prov, aes(num_rooms, percent, group = province), color = "red") +
            theme(axis.text.x = element_text(angle = 18, hjust = 1)) +
            myThemeAxis
    })
})

# average living area per head of all, city, town, and villiage ================
# map_title <- reactive(input$chooseWhere)
houseMapWhere <- reactive(get(paste0("houseMap", input$chooseWhere)))
output$avgLivingArea <- renderPlot(
    height = function() {
        0.8 * session$clientData$output_avgLivingArea_width
    },
    expr = {
    ggplot(houseMapWhere(), aes(long, lat)) + 
        geom_polygon(aes(group = group, fill = area_per_head), color = "black") +
        scale_fill_continuous(limit = c(18, 50), low = "white", high = "red") +
        ggtitle(where()) + 
        theme(legend.position = c(0.905, 0.25), 
              legend.text=element_text(size=11),
              legend.title=element_blank()) + 
        myThemeAxis +
        ylim(c(15, 55))
})

# mouse hover over
observeEvent(input$hoverAvgLivingArea, {
    # force x and y NOT reactive, otherwise related variables disappear when
    # unclicked, input$clickMap$x and input$clickMap$y are always reactive
    x <- input$hoverAvgLivingArea$x
    y = input$hoverAvgLivingArea$y
    hoveredProv <- which_province(map, x, y, "province")
    # these two are two close to seperate, but we only need one
    if (hoveredProv == "Hong Kong") hoveredProv <- "Guangdong"
    annotateNumber <- unique(houseMapWhere()$area_per_head[houseMapWhere()$province == hoveredProv])[1]
    
    
    if (hoveredProv != "") {
        annotateText <- paste0(gsub(" ", "~", hoveredProv), ":~", annotateNumber, "~m^2")
        if (hoveredProv == "Taiwan") {
            annotateText <- "Taiwan:~no~data"
        }
    } else {
        # prepare for na.rm = TRUE in geom_label()
        annotateText <- NA
    }
    print(annotateText)
    
    
    
    # create data frame for geom_label
    labelDf <- data.frame(long = x, lat = y, text = annotateText)
    
    output$avgLivingArea <- renderPlot(
        height = function() {
            0.8 * session$clientData$output_avgLivingArea_width
        },
        expr = {
            ggplot(houseMapWhere(), aes(long, lat)) + 
                geom_polygon(aes(group = group, fill = area_per_head), color = "black") +
                scale_fill_continuous(limit = c(18, 50), low = "white", high = "red") +
                ggtitle(where()) + 
                theme(legend.position = c(0.905, 0.25), 
                      legend.text=element_text(size=11),
                      legend.title=element_blank()) + 
                myThemeAxis +
                ylim(c(15, 55)) +
                geom_path(data = map[map$province == hoveredProv,], 
                             aes(long, lat, group = group), color = "yellow", size = 1) +
                geom_label(data = labelDf, aes(long, lat, label = text), 
                           alpha = 0.7, vjust = 2, na.rm = TRUE, parse = TRUE)
#                 annotate("text", x = x, y = y, label = annotateText, color = "black")
    })
    

})

# dependence of building area per person on education =========================
plotHouseEdu <- ggplot(houseEduCombined, aes(education, area_per_person)) +
    geom_line(aes(group = where, color = where), alpha = 0.6) + 
    geom_point(aes(size = number_of_household, color = where), alpha = 0.6) +
    guides(color = "legend", size = FALSE) +
    scale_size_area(max_size = 20) + 
    expand_limits(y = c(25.1, 43.2)) +
    # do not use limits = c(26, 43) below. click returns nothing out of the range
    scale_y_continuous(breaks = seq(26, 43, 2)) +
    scale_color_manual(values = c("city" = "blue", "town" = "green",
                                 "villiage" = "red")) +
    theme(legend.position = c(0.9, 0.15), 
          legend.text=element_text(size=11),
          legend.title=element_blank()) + 
    theme(axis.title = element_text(size = 16),
          axis.text.y = element_text(color = "black", size = 12),
          axis.text.x = element_text(angle = 18, hjust = 1, 
                                     color = "black", size = 12)) +
    xlab("Final Education") +
    ylab(expression(Area~per~Person~(m^2))) +
#     labs(list(x = "Final Education", y = expression("Area per Person (m^2)"))) + 
    ggtitle("dependence on education: city, town, and villiage")

#startup plot
output$areaPerPerson <- renderPlot(
    height = function() {
        0.8 * session$clientData$output_areaPerPerson_width
    },
    expr = plotHouseEdu
)

# when clicked
observeEvent(input$clickAreaPerPerson, {
    x <- round(input$clickAreaPerPerson$x)
    y <- input$clickAreaPerPerson$y
    # from x select the average area of city, town and villiage
    ctv <- houseEduCombined[c(x, x + 7, x + 14), 5]
    # find the one closest to click point
    idx <- which.min(abs(y - ctv))
    
    # ignore if too far from the closest one
    if (abs(y - ctv)[idx] > 0.5) {
        labelDf <- data.frame(long = x, lat = y, 
                              text = NA)
    } else {
        householdNumber <- round(houseEduCombined[x + 7 * (idx - 1), 2] / 1e6,2)
        where <- c("city ", "town ", "villiage ")[idx]
        labelDf <- data.frame(long = x, lat = y, 
                              text = paste0(where, "\n", householdNumber, " M"))
    }
    
    # make data frame for geom_label()
#     labelDf <- data.frame(long = x, lat = y, 
#                           text = paste0(where, "\n", householdNumber))
    output$areaPerPerson <- renderPlot(
        height = function() {
            0.8 * session$clientData$output_areaPerPerson_width
        },
        expr = {
        plotHouseEdu + 
                geom_label(data = labelDf, aes(long, lat, label = text), 
                           alpha = 0.5, vjust = 1, na.rm = TRUE)
#             annotate("text", x = x, y = y, 
#                      label = paste0(where, "\n", householdNumber))
    })
})
