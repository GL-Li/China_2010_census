# setwd("~/Dropbox/my_R_projects/China_2010_cencus/shiny_English")

library(shiny)
library(ggplot2); library(readxl)  #; library(tidyr)

# set up theme for ggplot =====================================================
myThemeAxis <- theme(axis.title = element_text(size = 14),
                     axis.text = element_text(size = 12, color = "black"),
                     legend.text = element_text(size=11))
    
themeRemoveAxis <- theme(axis.title.x = element_blank(),
                         axis.text.x = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.title.y = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks.y = element_blank())


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# prepare population data =====================================================
load("RData/population.RData")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# prepare education data ======================================================
load("RData/education.RData")
load("process_for_global_R.RData")

#== nation - total population at each education level for bar plot
total <- apply(eduNation[3:9], 2, sum)
totalDf <- data.frame(edu = gsub("_", " ", names(total)), numb = total / 1e6)
# reorder factors for better bar plot
totalDf$edu <- factor(totalDf$edu, ordered = TRUE,
                       levels = gsub("_", " ", names(total)))

#== combine data for comparing city, town and village
eduCombined <- rbind(eduCity, eduTown, eduVillage)

#== max percent of prime age kids at school in city, town, and village
primePercent <- data.frame(
    apply(100 * eduCombined[, 3:9] / eduCombined$population, 2, 
          function(x) tapply(x, eduCombined$where, max, na.rm = TRUE))
    )
primePercent$where <- rownames(primePercent)


#== where are the population at each education level
eduTotalAll <- apply(eduCombined[, 3:9], 2, sum, na.rm = TRUE)
eduTotalCity <- apply(eduCity[,3:9], 2, sum, na.rm = TRUE)
eduTotalTown <- apply(eduTown[,3:9], 2, sum, na.rm = TRUE)
eduTotalVillage <- apply(eduVillage[,3:9], 2, sum, na.rm = TRUE)
eduTotal <- data.frame(all = eduTotalAll, city = eduTotalCity, 
                       town = eduTotalTown, village = eduTotalVillage)
# eduWherePercent <- 100 * data.frame(city = eduTotalCity / eduTotalAll,
#                                     town = eduTotalTown / eduTotalAll,
#                                     village = eduTotalVillage / eduTotalAll)
# eduWherePercent$education <- factor(gsub("_", " ", rownames(eduWherePercent)),
#                                     levels = gsub("_", " ", rownames(eduWherePercent)))
# library(tidyr)
# eduWherePercent <- gather(eduWherePercent, where, percent, -education)


#== male and female number at each education level
eduTotalMale <- apply(eduCombined[eduCombined$sex == "male", 3:9], 2, sum, na.rm = TRUE)
eduTotalFemale <- apply(eduCombined[eduCombined$sex == "female", 3:9], 2, sum, na.rm = TRUE)
# eduGenderPercent <- 100 * data.frame(male = eduTotalMale / eduTotalAll,
#                                female = eduTotalFemale / eduTotalAll)
# eduGenderPercent$education <- factor(gsub("_", " ", rownames(eduGenderPercent)),
#                                     levels = gsub("_", " ", rownames(eduGenderPercent)))
# eduGenderPercent <- gather(eduGenderPercent, gender, percent, -education)



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
houseMapVillage <- houseMap[houseMap$where %in% c("village", NA),]


# define a function to deternime which province a point (long, lat) is in
which_province <- function(mapData, long, lat, language = "province") {
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
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #### use rad is better than degree, small unit favors large provinces #####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    angleRange <- tapply(mapData$angle, mapData[language], 
                         function(x) length(unique(round(x))))
    
    print("======================================")
    print(angleRange)
    
    # clicked outside China, total angle range is less than 6
    if (max(angleRange) < 7) {
        return("")
    }
    
    # return province, Hebei, Beijing, Tianjin is a mess
    sortProv <- names(sort(angleRange, decreasing = TRUE))
    if (sortProv[1] == "Hebei" & sortProv[2] %in% c("Beijing", "Tianjin")) {
        return(sortProv[2])
    } else {
        return(sortProv[1])
    }
}

# delete data not used in shiny and clean memories
rm(list = c("China_province_map_data", "houseMap"))
gc()

