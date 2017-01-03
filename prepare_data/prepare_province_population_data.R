setwd("~/Dropbox/my_R_projects/China_2010_cencus/prepare_data/")

library(readxl); library(ggplot2); library(tidyr)

read_2010_prov_popul <- function(xls_file, skip = 7) {
    popul <- read_excel(xls_file, skip = skip, col_names = FALSE)
    popul <- na.omit(popul)
    popul <- popul[-1, c(1, 5, 17)]
    names(popul) <- c("省市区", "人口2010", "户均人口2010")
    popul$省市区 <- gsub(" ", "", popul$省市区)
    return(popul)
}

read_2000_prov_popul <- function(xls_file, skip = 6) {
    popul <- read_excel(xls_file, skip = skip, col_names = FALSE)
    popul <- na.omit(popul)
    popul <- popul[-1, c(1, 5, 17)]
    names(popul) <- c("省市区", "人口2000", "户均人口2000")
    popul$省市区 <- c("北京",   "天津",   "河北",   "山西",   "内蒙古", "辽宁",
                   "吉林",   "黑龙江", "上海",   "江苏",  "浙江",   "安徽",
                   "福建",   "江西",   "山东",   "河南",   "湖北",   "湖南",
                   "广东",   "广西",  "海南",   "重庆",   "四川",   "贵州",
                   "云南", "西藏", "陕西", "甘肃", "青海", "宁夏",  "新疆" )
    return(popul)
}

popul_2010 <- read_2010_prov_popul("xls_data/province_population_1_1.xls")
popul_2000 <- read_2000_prov_popul("xls_data/Y2000/province_population_1_1_2000.xls")
prov_popul_2000_2010 <- cbind(popul_2010, popul_2000[c(2, 3)])


save(prov_popul_2000_2010, 
     file = "RData/province_population.RData")
save(prov_popul_2000_2010, 
     file = "../shiny_English/RData/province_population.RData")
save(prov_popul_2000_2010, 
     file = "../blog_plot/RData/province_population.RData")
