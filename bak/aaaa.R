library(shiny)
library(ggplot2)

# global.R ====================================================================

# define a function to deternime which province a point (long, lat) is in
whichProvince <- function(mapData, long, lat, language = "省名") {
    # Args:
    #   mapData: The map data has a column "long" and a column "lat" to determine
    #       province borders. Other columns to specify province name as "省名", 
    #       "pinyinName", "provinceName", and "isoCode"
    #   long, lat: longitude and latitude of the point. long is in the 
    #       range of 70, 140, lat in the range of 5, 55, for China map
    #   language: specify the format of return, can be "省名", "pinyinName", 
    #       "provinceName", and "isoCode"
    #
    # Returns: 
    #   The province where the point is in.
    
    # calculate the difference in long and lat with respect to the point
    mapData$longDiff <- mapData$long - long
    mapData$latDiff <- mapData$lat - lat
    
    # keep difference to local, otherwise 内蒙古 is a problem
    # after several tries, longDiff < 7 and latDiff < 5 is the best range
    mapData <- mapData[abs(mapData$longDiff) < 7 & abs(mapData$latDiff) < 5, ]
    
    # define average distance of a province to the point
    # this distance cancels points in the opposite direction
    distLong <- tapply(mapData$longDiff, mapData[language], mean)
    distLat <- tapply(mapData$latDiff, mapData[language], mean)
    provDist <- sqrt(distLong^2 + distLat^2)
    
    return(names(sort(provDist))[1])
}

# read map data and add a row id for resotring order
map <- read.csv("ChinaProvinceMapData.csv")
map$rowID <- 1:nrow(map)

# read house data
library(readxl)
rooms <- read_excel("data/number_rooms_per_family_sect8_1.xls", 
                    skip = 4)
colnames(rooms) <- c("province", "total", "one", "two", "three", "four", "five", "six",
                     "seven", "eight", "nine", "ten_and_up")

# extract and remove national data from data frame
national <- as.numeric(rooms[1, 2:12])
names(national) <- colnames(rooms)[-1]
rooms <- rooms[-c(1,33),]   # last row is NA
rooms$province <- gsub(" ", "", rooms$province)  # clean space in chinese characters

# add column for average room numbers for each province
avg <- apply(rooms[, 3:11], 1, function(x) sum(as.numeric(x) * 1:10)) / rooms$total
rooms$avg <- round(avg, 2)

# add province name in english
chineseToEnglish <- unique(map$provinceName)
names(chineseToEnglish) <- unique(map$省名)
rooms$provinceName <- chineseToEnglish[gsub(" ", "", rooms$province)]

# add average room numbers to map data for fill
roomMerge <- rooms[c("province", "avg")]

names(roomMerge) <- c("省名", "avg")
mapRoom <- merge(map, roomMerge, by = "省名", sort = FALSE)
mapRoom <- mapRoom[order(mapRoom$rowID),]

# define a cosVec function to calculate the angle of two vectors
# cosVec <- function(x, y) {
#     return(sum(x * y) / sqrt(sum(x * x))/sqrt(sum(y * y)))
# }


# ui.R ========================================================================
ui <- fluidPage(fluidRow(
    column(width = 7,
           plotOutput("map", click = "clickMap", hover = "hoverMap")),
    column(width = 5,
           plotOutput("roomBar"))
))


# server.R ====================================================================
server <- function(input, output) {
    output$map <- renderPlot(width = 600, height = 500, expr = {
        ggplot(data = mapRoom, aes(x = long, y = lat)) +
            geom_polygon(aes(group = group, fill = avg), color = "black") # +
        #coord_map()  coordinate changes long and lat in map when clicked
    })
    
    observeEvent(input$clickMap, {
        # force x and y NOT reactive, otherwise related variables disappear when
        # unclicked, input$clickMap$x and input$clickMap$y are always reactive
        x <- input$clickMap$x
        y = input$clickMap$y
        clickedProv <- whichProvince(map, x, y)
        
        # back to room data
        roomProv <- as.numeric(rooms[rooms$province == clickedProv, 2:12])
        roomDF <- data.frame(numbRoom = factor(names(rooms[3:12]), ordered = TRUE,
                                               levels = names(rooms[3:12])),
                             houseHold = roomProv[2:11] / roomProv[1] )
        
        output$map <- renderPlot(width = 600, height = 500, expr = {
            ggplot(data = mapRoom, aes(x = long, y = lat)) +
                geom_polygon(aes(group = group, fill = avg), color = "black") + 
                geom_polygon(data = map[map$省名 == clickedProv,], 
                             aes(long, lat, group = group), fill = "yellow") +
                annotate("text", x = x, y = y, label = clickedProv)
        })
        
        print(clickedProv)
        
        output$roomBar <- renderPlot(width = 300, height = 500, expr = {
            ggplot(roomDF, aes(numbRoom, houseHold)) + 
                geom_bar(stat = "identity") + 
                coord_flip() + 
                ylim(c(0,0.5)) +               # need to install Chinese in shiny server
                ggtitle("新疆") +              # shiny server does not display Chinese
                ylab("household  fraction") + 
                xlab("Number  of  Rooms")
        })
    })
}

shinyApp(ui, server)