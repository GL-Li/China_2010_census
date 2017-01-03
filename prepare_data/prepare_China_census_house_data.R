library(ggplot2)

setwd("~/Dropbox/my_R_projects/China_2010_census/prepare_data/")
rm(list = ls())
source("global_functions.R")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# average number of rooms and building area ===================================
# in each province per family and per person. data from 1-14, 1-14a, 1-14b, 1-14c
library(readxl)
houseAll <- read_excel("xls_data/house_average_room_and_area_1_14.xls")
houseAll$where <- "all"
houseAll$地点 <- "全国"

houseCity <- read_excel("xls_data/house_average_room_and_area_city_1_14a.xls")
houseCity$where <- "city"
houseCity$地点 <- "城市"

houseTown <- read_excel("xls_data/house_average_room_and_area_town_1_14b.xls")
houseTown$where <- "town"
houseTown$地点 <- "乡镇"

houseVillage <- read_excel("xls_data/house_average_room_and_area_villiage_1_14c.xls")
houseVillage$where <- "village"
houseVillage$地点 <- "农村"

# put all, city, town, and village data together, which will be saved to RData
houseCombined <- rbind(houseAll, houseCity, houseTown, houseVillage)
houseCombined <- na.omit(houseCombined)
# the raw data has space between Chinese characters, so
houseCombined$省名 <- gsub(" ", "", houseCombined$省名)

# finalize column names in English
colnames(houseCombined) <- c("Chinese", "number_of_household", "population",
                              "number_of_rooms_per_household", "area_per_head",
                              "number_of_rooms_per_head", "where", 'location')

# merge combined avg data with map
houseMap <- merge_to_map(houseCombined, "China_province_map_data.csv")


# test plot, difficult to make nice plot with this data
g <- ggplot(houseCombined, aes(x = Chinese, y = number_of_rooms_per_head, fill = where))
g + geom_bar(stat = "identity", position = "dodge", width = 0.7)  

houseMapAll <- houseMap[houseMap$where == "all",]
ggplot(houseMapAll, aes(long, lat)) + 
    geom_polygon(aes(group = group, fill = area_per_head), color = "black")

houseMapCity <- houseMap[houseMap$where == "city",]
ggplot(houseMapCity, aes(long, lat)) + 
    geom_polygon(aes(group = group, fill = area_per_head), color = "black")

houseMapVillage <- houseMap[houseMap$where == "village",]
ggplot(houseMapVillage, aes(long, lat)) + 
    geom_polygon(aes(group = group, fill = area_per_head), color = "black") +
    scale_fill_continuous(limit = c(18, 50)) +
    theme(legend.position = c(0.92, 0.2), 
          legend.text=element_text(size=11),
          legend.title=element_blank()) + 
    ylim(c(15, 55))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# house and education =========================================================
# living area per head as function of education level
read_house_edu <- function(xls_file, where) {
    # Args
    #   xls_file: the xls file to be read. need to be cleaned in Excel
    #   where: select from "all", "city", "town", and "village"
    #
    # Returns: a data frame
    #
    houseEdu <- read_excel(xls_file, skip = 2)
    colnames(houseEdu) <- c("受教育程度", "number_of_household", "population",
                            "number_of_rooms_per_household", "area_per_person",
                            "number_of_rooms_per_person")
    edu <- c("no school", "elementary", "middle school", "high school", 
             "technical college",  "university", "graduate school")
    houseEdu$education <- factor(edu, levels = edu)
    houseEdu$where <- where
    return(houseEdu)
}

houseEduAll <- read_house_edu("xls_data/education_house_8_4.xls", "all")
houseEduCity <- read_house_edu("xls_data/education_house_city_8_4a.xls", "city")
houseEduTown <- read_house_edu("xls_data/education_house_town_8_4b.xls", "town")
houseEduVillage <- read_house_edu("xls_data/education_house_villiage_8_4c.xls", "village")

# combined data
houseEduCombined <- rbind(houseEduCity, houseEduTown, houseEduVillage)

levels <- c("未上过学", "小  学", "初  中",   "高  中",   "大学专科", "大学本科", "研究生")
houseEduCombined$受教育程度 <- factor(houseEduCombined$受教育程度, levels = levels)

houseEduCombined$where <- factor(houseEduCombined$where, 
                                 levels = c('city', 'town', 'village'))

where_to_location <- c("city" = "城市", "town" = "乡镇", "village" = "农村")
houseEduCombined$地点 <- factor(where_to_location[houseEduCombined$where],
                                    levels = c("城市", "乡镇", "农村"))



# houseEdu <- read_excel("xls_data/education_house_8_4.xls", skip = 2)
# colnames(houseEdu) <- c("受教育程度", "number_of_household", "population",
#                          "number_of_rooms_per_household", "area_per_person",
#                          "number_of_rooms_per_person")
# edu <- c("no school", "elementary", "middle school", "high school", 
#          "technical college",  "university", "graduate school")
# houseEdu$education <- factor(edu, levels = edu)
# ggplot(houseEduTown, aes(education, area_per_person)) +
#     geom_line(aes(group = 1)) + 
#     geom_point(aes(size = number_of_household), color = "red") + 
#     scale_size_area(max_size = 30) + 
#     scale_y_continuous(limits = c(25, 40), breaks = seq(28, 40, 2)) +
#     theme(legend.position = "") + 
#     theme(axis.title = element_text(size = 16),
#           axis.text.y = element_text(color = "black", size = 12),
#           axis.text.x = element_text(angle = 18, hjust = 1, 
#                                      color = "black", size = 12)) +
#     labs(list(x = "Final Education", y = "Area per Person"))
# 
# aa <- ggplot(houseEduCombined, aes(education, area_per_person)) +
#     geom_line(aes(group = where), alpha = 0.5) + 
#     geom_point(aes(size = number_of_household), alpha = 0.5) +
#     scale_size_area(max_size = 20) + 
#     scale_y_continuous(limits = c(26, 43), breaks = seq(26, 43, 4)) +
#     theme(legend.position = "") + 
#     theme(axis.title = element_text(size = 16),
#           axis.text.y = element_text(color = "black", size = 12),
#           axis.text.x = element_text(angle = 18, hjust = 1, 
#                                      color = "black", size = 12)) +
#     labs(list(x = "Final Education", y = "Area per Person"))

# library(plotly)
# ggplotly(aa)


################################################################################
# distribution of number of rooms in a house ==================================
read_house_distr <- function(xls_file, skip = 4) {
    # This function is only to read house data 8_1, 8_1a, 8_1b, and 8_1c in 
    # China 2010 census
    #
    # Args:
    #   xls_file: path and filename, for example 
    #       "xls_data/number_rooms_per_family_sect8_1.xls"
    #   skip: how many rows to skip in the xls file
    #
    # Returns: a list includes two elements, each a data frame
    #   $room_distr: used to plot distribution
    #   $rooms_popular: used to plot most popular number of rooms
    #
    library(readxl)
    roomDistr <- read_excel(xls_file, skip = 4)
    colnames(roomDistr) <- c("Chinese", "total", "one", "two", "three", "four", "five", "six",
                         "seven", "eight", "nine", "ten_and_up")
    
    # extract and remove national ("all") data from data frame
    national <- as.numeric(roomDistr[1, 2:12])
    names(national) <- colnames(roomDistr)[-1]
    roomDistr <- roomDistr[-c(1,33),]   # last row is NA
    roomDistr$Chinese <- gsub(" ", "", roomDistr$Chinese)  # clear space in chinese characters
    roomDistr$province <- Chinese_to_English[roomDistr$Chinese]
    
    # reorganize the rooms data
    library(tidyr)
    plotRoomDistr <- gather(roomDistr, num_rooms, count, -c(Chinese, province, total))
    plotRoomDistr$percent <- 100 * plotRoomDistr$count / plotRoomDistr$total
    plotRoomDistr$num_rooms <- factor(plotRoomDistr$num_rooms, 
                                      levels = unique(plotRoomDistr$num_rooms))
    
    # add the most popular room numbers
    numRoomPopular <- apply(roomDistr[, 3:12], 1, function(x) which.max(as.numeric(x)))
    roomDistr$popular <- numRoomPopular
    
    # add columns of popular room numbers to map data for fill
    colToMerge <- roomDistr[c("Chinese", "popular")]
    
    roomPopularMap <- merge_to_map(colToMerge, "China_province_map_data.csv")
    
    return(list(roomDistr = roomDistr, plotRoomDistr = plotRoomDistr, 
                roomPopularMap = roomPopularMap))
}

houseDistrAll <- read_house_distr("xls_data/number_rooms_per_family_sect8_1.xls")
houseDistrCity <- read_house_distr("xls_data/number_rooms_per_family_sect8_1a_city.xls")
houseDistrTown <- read_house_distr("xls_data/number_rooms_per_family_sect8_1b_town.xls")
houseDistrVillage <- read_house_distr("xls_data/number_rooms_per_family_sect8_1c_villiage.xls")





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# save data for shiny =========================================================
save(houseCombined, houseMap, houseEduCombined, houseDistrAll, houseDistrCity,
     houseDistrTown, houseDistrVillage, file = "RData/house.RData")
save(houseCombined, houseMap, houseEduCombined, houseDistrAll, houseDistrCity,
     houseDistrTown, houseDistrVillage, file = "../shiny_English/RData/house.RData")
save(houseCombined, houseMap, houseEduCombined, houseDistrAll, houseDistrCity,
     houseDistrTown, houseDistrVillage, file = "../blog_plot/RData/house.RData")