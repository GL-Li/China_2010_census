# naming all variables starting with edu

# data source: The 2010 population cencus of China
#   http://www.stats.gov.cn/tjsj/pcsj/rkpc/6rp/indexce.htm

setwd("~/Dropbox/my_R_projects/China_2010_cencus/prepare_data/")
# clearn environment
rm(list = ls())

# process data ================================================================
# define a function that reads education excel file into data frame
read_edu_data <- function(xlsFile) {
    # Args:
    #   xlsFile - the path to the excel file, for table 4-1, 4-1a, 4-1b, 4-1c
    #
    # Returns: a data frame shows at each age the populatio of male and female 
    #   whose final education is at from no school to graduate school
    
    require(readxl)
    education <- read_excel(xlsFile, 
                            col_types=rep("numeric", 25), skip = 4)
    # remove rows where the age is not a single number.
    # all NAs in age is removed. Note that the last one, which is
    # age above 85, is also removed. However, it will not affect the result much.
    education <- education[!is.na(education[,1]),]
    
    # extract male and female dataframe
    eduMale <- education[,c(1, 3, 6, 9, 12, 15, 18, 21, 24)]
    colnames(eduMale) <- c("age", "population", "no_school", "elementary_school", 
                            "middle_school", "high_school", "specialized_college", 
                            "university", "graduate_school")
    eduMale$sex <- "male"
    
    eduFemale <- education[,c(1, 4, 7, 10, 13, 16, 19, 22, 25)]
    colnames(eduFemale) <- c("age", "population", "no_school", "elementary_school", 
                              "middle_school", "high_school", "specialized_college", 
                              "university", "graduate_school")
    eduFemale$sex <- "female"
    
    eduAllSex <- rbind(eduMale, eduFemale)
    eduAllSex$sex <- factor(eduAllSex$sex, levels = c("male", "female"))
    eduAllSex$birth <- 2010 - eduAllSex$age
    
    return(eduAllSex)
}

# education data for the nation, in city, town, and village
eduNation <- read_edu_data("xls_data/education_sect4_1.xls")
eduNation$where <- "nation"

eduCity <- read_edu_data("xls_data/education_sect4_1a_city.xls")
eduCity$where <- "city"

eduTown <- read_edu_data("xls_data/education_sect4_1b_town.xls")
eduTown$where <- "town"

eduVillage <- read_edu_data("xls_data/education_sect4_1c_villiage.xls")
eduVillage$where <- "village"


# save RData for shiny ========================================================
rm("read_edu_data")

save(eduNation, eduCity, eduTown, eduVillage,
     file = "RData/education.RData")
save(eduNation, eduCity, eduTown, eduVillage,
     file = "../shiny_English/RData/education.RData")
save(eduNation, eduCity, eduTown, eduVillage,
     file = "../blog_plot/RData/education.RData")

# test plot ===================================================================
library(ggplot2)

# compare education in city, town, and village
edu_combined <- rbind(eduCity, eduTown, eduVillage)
g <- ggplot(edu_combined, aes(x = age, y = 100 * specialized_college / population))
g + geom_line(aes(color=factor(sex, levels = c("male", "female")))) + 
    geom_point(aes(color=factor(sex, levels = c("male", "female")))) +
    facet_wrap(~where) +
    ggtitle("Compare Education in City, Town, and Village") +
    xlab("Age (years)") + 
    ylab("Percent of Pupulation") + 
    theme(legend.position = c(0.93, 0.85)) +
    theme(strip.text.x = element_text(size = 12)) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 11, color = "black"))


