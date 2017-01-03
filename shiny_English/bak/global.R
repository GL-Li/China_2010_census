setwd("~/Dropbox/my_R_projects/China_2010_cencus/shiny_English")

library(shiny)
library(ggplot2); library(readxl); library(tidyr)

# set up theme for ggplot =====================================================
myThemeAxis <- theme(axis.title = element_text(size = 14),
                       axis.text = element_text(size = 11, color = "black"))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# prepare population data =====================================================
load("RData/population.RData")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# prepare education data ======================================================
load("RData/education.RData")

# nation - total population at each education level for bar plot
total <- apply(eduNation[3:9], 2, sum)
totalDf <- data.frame(edu = gsub("_", " ", names(total)), numb = total / 1e6)

# reorder factors for better bar plot
totalDf$edu <- factor(totalDf$edu, ordered = TRUE,
                       levels = gsub("_", " ", names(eduNation[3:9])))

# combine data for comparing city, town and villiage
eduCombined <- rbind(eduCity, eduTown, eduVilliage)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# prepare house data ==========================================================
load("RData/house.RData")
load("RData/China_province_map_data.RData")

# rename map data for easy coding
map <- China_province_map_data

# average living area, where == NA is for Taiwan
houseMapAll <- houseMap[houseMap$where %in% c("all", NA),]
houseMapCity <- houseMap[houseMap$where %in% c("city", NA),]
houseMapTown <- houseMap[houseMap$where %in% c("town", NA),]
houseMapVilliage <- houseMap[houseMap$where %in% c("villiage", NA),]


# define a function to deternime which province a point (long, lat) is in
which_province <- function(mapData, long, lat, language = "Chinese") {
    # Args:
    #   mapData: The map data has a column "long" and a column "lat" to determine
    #       province borders. Other columns to specify province name as "Chinese", 
    #       "pinyinName", "provinceName", and "isoCode"
    #   long, lat: longitude and latitude of the point. long is in the 
    #       range of 70, 140, lat in the range of 5, 55, for China map
    #   language: specify the format of return, can be "省名", "pinyinName", 
    #       "provinceName", and "isoCode"
    #
    # Returns: 
    #   The province where the point is in.
    
    # calculate the difference in long and lat with respect to the point
    mapData$long_diff <- mapData$long - long
    mapData$lat_diff <- mapData$lat - lat
    
    # keep difference to local, otherwise 内蒙古 is a problem
    # after several tries, long_diff < 7 and lat_diff < 5 is the best range
    mapData <- mapData[abs(mapData$long_diff) < 20 & abs(mapData$lat_diff) < 15, ]
    
    # calculate the angle between the vector from this clicked point to border and c(1, 0)
    vLong <- mapData$long_diff
    vLat <- mapData$lat_diff
    angleTemp <- acos(vLong / sqrt(vLong^2 + vLat^2))
    mapData$angle <- ifelse(vLat > 0, angleTemp, 2 * pi - angleTemp) 
    
    # calculate range of the angle and select the state with largest range
    rangeAngle <- tapply(mapData$angle, mapData[language], 
                         function(x) length(unique(round(x))))
    
    # return province, Hebei, Beijing, Tianjin is a mess
    sortProv <- names(sort(rangeAngle, decreasing = TRUE))
    if (sortProv[1] == "Hebei" & sortProv[2] %in% c("Beijing", "Tianjin")) {
        return(sortProv[2])
    } else {
        return(sortProv[1])
    }
}



