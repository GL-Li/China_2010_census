library(shiny)
library(ggplot2)

# global.R ====================================================================

# read map data and add a row id for resotring order
map <- read.csv("ChinaProvinceMapData.csv")
map$rowID <- 1:nrow(map)

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

# read house data
houseData <- function(xlsFile, skip = 4) {
    # This function is only to read house data in China 2010 census
    #
    # Args:
    #   xlsFile: path and filename, for example 
    #       "data/number_rooms_per_family_sect8_1.xls"
    #   skip: how many rows to skip in the xls file
    #
    # REturns: a list includes two elements, each a data frame
    #   $roomsData: used to plot distribution
    #   $roomMap: popular and average number of room are added to map
    #
    library(readxl)
    rooms <- read_excel(xlsFile, skip = 4)
    colnames(rooms) <- c("省名", "total", "one", "two", "three", "four", "five", "six",
                         "seven", "eight", "nine", "ten_and_up")
    
    # extract and remove national data from data frame
    national <- as.numeric(rooms[1, 2:12])
    names(national) <- colnames(rooms)[-1]
    rooms <- rooms[-c(1,33),]   # last row is NA
    rooms$省名 <- gsub(" ", "", rooms$省名)  # clear space in chinese characters
    
    # add province name in english
    chineseToEnglish <- unique(map$provinceName)
    names(chineseToEnglish) <- unique(map$省名)
    rooms$provinceName <- chineseToEnglish[gsub(" ", "", rooms$省名)]
    
    # reorganize the rooms data
    library(tidyr)
    numbRoomPerHouse <- gather(rooms, numbRooms, count, -c(省名, provinceName, total))
    numbRoomPerHouse$fraction <- numbRoomPerHouse$count / numbRoomPerHouse$total
    
    # add column for average room numbers for each province
    avg <- apply(rooms[, 3:12], 1, function(x) sum(as.numeric(x) * 1:10)) / rooms$total
    rooms$average <- round(avg, 2)
    
    # add the most popular room numbers
    pop <- apply(rooms[, 3:12], 1, function(x) which.max(as.numeric(x)))
    rooms$popular <- pop
    
    # add average and popular room numbers to map data for fill
    roomMerge <- rooms[c("省名", "average", "popular")]
    
    # names(roomMerge) <- c("省名", "avg")
    # keep all rows in map even no match from roomMerge
    # there is another merge() function in shinySignals, add base:: to avoid confusion
    mapRoom <- base::merge(map, roomMerge, by = "省名", sort = FALSE, all.x = TRUE)
    mapRoom <- mapRoom[order(mapRoom$rowID),]
    
    return(list(roomsData = numbRoomPerHouse, roomsMap = mapRoom))
}

houseAll <- houseData("data/number_rooms_per_family_sect8_1.xls")


rooms <- houseAll$rooms
numbRoomPerHouse <- houseAll$roomsData
mapRoom <- houseAll$roomsMap


# library(readxl)
# rooms <- read_excel("data/number_rooms_per_family_sect8_1.xls", 
#                     skip = 4)
# colnames(rooms) <- c("省名", "total", "one", "two", "three", "four", "five", "six",
#                      "seven", "eight", "nine", "ten_and_up")
# 
# # extract and remove national data from data frame
# national <- as.numeric(rooms[1, 2:12])
# names(national) <- colnames(rooms)[-1]
# rooms <- rooms[-c(1,33),]   # last row is NA
# rooms$省名 <- gsub(" ", "", rooms$省名)  # clear space in chinese characters
# 
# # add province name in english
# chineseToEnglish <- unique(map$provinceName)
# names(chineseToEnglish) <- unique(map$省名)
# rooms$provinceName <- chineseToEnglish[gsub(" ", "", rooms$省名)]
# 
# # reorganize the rooms data
# library(tidyr)
# numbRoomPerHouse <- gather(rooms, numbRooms, count, -c(省名, provinceName, total))
# numbRoomPerHouse$fraction <- numbRoomPerHouse$count / numbRoomPerHouse$total


# # add column for average room numbers for each province
# avg <- apply(rooms[, 3:12], 1, function(x) sum(as.numeric(x) * 1:10)) / rooms$total
# rooms$average <- round(avg, 2)
# 
# # add the most popular room numbers
# pop <- apply(rooms[, 3:12], 1, function(x) which.max(as.numeric(x)))
# rooms$popular <- pop
# 
# # add average and popular room numbers to map data for fill
# roomMerge <- rooms[c("省名", "average", "popular")]
# 
# # names(roomMerge) <- c("省名", "avg")
# # keep all rows in map even no match from roomMerge
# # there is another merge() function in shinySignals, add base:: to avoid confusion
# mapRoom <- base::merge(map, roomMerge, by = "省名", sort = FALSE, all.x = TRUE)
# mapRoom <- mapRoom[order(mapRoom$rowID),]

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
    output$map <- renderPlot(width = 580, height = 500, expr = {
        ggplot(data = mapRoom, aes(x = long, y = lat)) +
            geom_polygon(aes(group = group, fill = average), color = "black") +
            scale_fill_gradient(limit = c(1, 5)) + 
            ggtitle("Click on a province to view details")
            #coord_map()  coordinate changes long and lat in map when clicked
    })
    
    observeEvent(input$clickMap, {
        # force x and y NOT reactive, otherwise related variables disappear when
        # unclicked, input$clickMap$x and input$clickMap$y are always reactive
        x <- input$clickMap$x
        y = input$clickMap$y
        clickedProv <- whichProvince(map, x, y)
        # these two are two close to seperate, but we only need one
        if (clickedProv == "香港") clickedProv <- "广东"
        
        # back to room data, no data for Taiwan
        if (clickedProv != "台湾") {
          roomProv <- as.numeric(rooms[rooms$province == clickedProv, 2:12])
        } else {
          roomProv <- c(1,rep(0, 10))
#           names(roomProv) <- names(rooms[3:12]) 
        }
        roomDF <- data.frame(numbRoom = factor(names(rooms[3:12]), ordered = TRUE,
                                               levels = names(rooms[3:12])),
                             houseHold = roomProv[2:11] / roomProv[1] )
        
        output$map <- renderPlot(width = 580, height = 500, expr = {
            ggplot(data = mapRoom, aes(x = long, y = lat)) +
                geom_polygon(aes(group = group, fill = avg), color = "black") + 
                ggtitle("Click on a province to view details") +
                geom_polygon(data = map[map$省名 == clickedProv,], 
                             aes(long, lat, group = group), fill = "orange") +
                annotate("text", x = x, y = y, label = clickedProv, color = "red")
        })
        
        print(clickedProv)
        
        output$roomBar <- renderPlot(width = 300, height = 500, expr = {
            ggplot(roomDF, aes(numbRoom, houseHold)) + 
                geom_bar(stat = "identity") + 
                coord_flip() + 
                ylim(c(0, 0.55)) +
                ggtitle(clickedProv) + 
                ylab("household  fraction") + 
                xlab("Number  of  Rooms")
            # be careful with the ylim. the bar will not display if exceeds limit
        })
        
        
        
        
        output$roomLine <- renderPlot({
            
        })
        
        
        
    })
}
shinyApp(ui = ui, server = server)
