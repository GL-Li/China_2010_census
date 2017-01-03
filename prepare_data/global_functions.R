# global_functions.R to define functions that will be used in multiple files

# province name translations lookup table =====================================
longName <- c("黑龙江省", "内蒙古自治区", "新疆维吾尔自治区", "吉林省",
              "辽宁省", "甘肃省", "河北省", "北京市", "山西省",
              "天津市", "陕西省", "宁夏回族自治区", "青海省", "山东省",
              "西藏自治区", "河南省", "江苏省", "安徽省", "四川省",
              "湖北省", "重庆市", "上海市", "浙江省", "湖南省", "江西省",
              "云南省", "贵州省", "福建省", "广西壮族自治区", "台湾省",
              "广东省", "香港特别行政区", "海南省", "澳门特别行政区")

shortName <- c("黑龙江", "内蒙古", "新疆", "吉林",
               "辽宁", "甘肃", "河北", "北京", "山西",
               "天津", "陕西", "宁夏", "青海", "山东",
               "西藏", "河南", "江苏", "安徽", "四川",
               "湖北", "重庆", "上海", "浙江", "湖南", "江西",
               "云南", "贵州", "福建", "广西", "台湾",
               "广东", "香港", "海南", "澳门")

pinyinName <- c("Heilongjiang", "Neimenggu", "Xinjiang", "Jilin", 
                "Liaoning", "Gansu", "Hebei", "Beijing", "Shanxi",
                "Tianjin", "Shaanxi", "Ningxia", "Qinghai", "Shandong",
                "Xizang", "Henan", "Jiangsu", "Anhui", "Sichuan", 
                "Hubei", "Chongqing", "Shanghai", "Zhejiang", "Hunan", "Jiangxi",
                "Yunnan", "Guizhou", "Fujian", "Guangxi", "Taiwan", 
                "Guangdong", "Xianggang", "Hainan", "Aomen")

englishName <- c("Heilongjiang", "Inner Mongolia", "Xinjiang", "Jilin", 
                 "Liaoning", "Gansu", "Hebei", "Beijing", "Shanxi",
                 "Tianjin", "Shaanxi", "Ningxia", "Qinghai", "Shandong",
                 "Tibet", "Henan", "Jiangsu", "Anhui", "Sichuan", 
                 "Hubei", "Chongqing", "Shanghai", "Zhejiang", "Hunan", "Jiangxi",
                 "Yunnan", "Guizhou", "Fujian", "Guangxi", "Taiwan", 
                 "Guangdong", "Hong Kong", "Hainan", "Macau")

isoCode <- c("CN-23", "CN-15", "CN-65", "CN-22", "CN-21", "CN-62", "CN-13",
             "CN-11", "CN-14", "CN-12", "CN-61", "CN-64", "CN-63", "CN-37", 
             "CN-54", "CN-41", "CN-32", "CN-34", "CN-51", "CN-42", "CN-50",
             "CN-31", "CN-33", "CN-43", "CN-36", "CN-53", "CN-52", "CN-35",
             "CN-45", "CN-71", "CN-44", "CN-91", "CN-46", "CN-92")

# conversion tables
Chinese_to_English <- englishName
names(Chinese_to_English) <- shortName

Chinese_to_isoCode <- isoCode
names(Chinese_to_isoCode) <- shortName


# define a function that merges data of each province to china map data =======
merge_to_map <- function(provinceData, path_to_China_province_map_data.csv) {
    # Args: 
    #   provinceData: data that to be merged into China map. It has a column
    #       called "省名" and other columns associated to each province
    #   path_to_ChinaProvinceMapData.csv: the source csv data of china map,  
    #       "ChinaProvinceMapData.csv", which is to be read into a data frame
    #
    # Returns:
    #   a data frame including province data that is ready to be filled in the 
    #   map of China.
    
    # read the map data. This process takes less than 1 second. So keep in function
    map <- read.csv(path_to_China_province_map_data.csv)
    # add a rowID for reordering later on
    map$rowID <- 1:nrow(map)
    
    # keep all rows in mapData even there is no match from provinceData
    # there is another merge() function in shinySignals, add base:: to avoid confusion
    merged <- base::merge(map, provinceData, by = "Chinese", sort = FALSE, all.x = TRUE)
    merged <- merged[order(merged$rowID),]
}


# read xls data downloaded from ===============================================
# http://www.stats.gov.cn/tjsj/pcsj/rkpc/6rp/indexch.htm
read_cencus_age <- function(file, colNumber, skip, dir="./data/") {
    # This function is to read xls data in which the first column is age.
    #
    # Args:
    #     file: the xls file to be read
    #     colNumber: number of columns in the xls file. You have to check the 
    #                xls file to determine it.
    #     skip: number of rows to skip. You have to tune it until it works.
    #     dir: the directory to the file
    #
    # Returns:
    #     a working data frame converted from the xls file.
    #     The output data frame looks like:
    #
    #              合计        男        女     小计       男     ....  
    #     NA 1242546122 633278387 609267735 62136405 17503480     ....
    #     NA   56149412  30476514  25672898  2133288  1146435     ....
    #      6   14804470   8034452   6770018  1483284   802376     ....
    #      7   13429161   7292300   6136861   312968   165544     ....
    #      ..   .......   .......   .......   ......   ......     .... 
    #     Tune "skip" number if output looks different.
    
    df <- readxl::read_excel(paste0(dir, file), 
                            col_types=rep("numeric", colNumber), 
                            skip = skip)
}


# clean data after read_cencus_age()
clean_cencus <- function(data, col_male, col_female, col_names) {
    # clean the data so that it is ready for plotting
    #
    # Args:
    #     data: data to be cleaned
    #     col_male: vector of index of columns related to male
    #     col_female: vector of index of columns related to female
    #         You will have to eyeball the raw data to determing them.
    #     col_names: the vector of column names extracted from xls file, which 
    #         plus "sex", serves as column names of the returned data frame.
    #
    # Retruns:
    #     clean data frame with "sex" as a vaiable so that it is easy 
    # to compare between male and femal with ggplot
    
    # remove excessive rows where age is NA
    data <- data[!is.na(data[,1]),]
    
    # extract subset for male and female, ignore the total
    male <- data[, c(1, col_male)]
    names(male) <- col_names
    female <- data[, c(1, col_female)]
    names(female) <- col_names
    
    male$sex <- "male"
    female$sex <- "female"
    
    data <- rbind(male, female)
}



