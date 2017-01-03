# Q: why this file?
# A: tidyr are too big in memory to be installed to AWS micro EC2, so have to 
# preprocess the data outside of shiny app

library(tidyr)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# prepare education data ======================================================
load("RData/education.RData")
eduCombined <- rbind(eduCity, eduTown, eduVillage)


#== where are the population at each education level
eduTotalAll <- apply(eduCombined[, 3:9], 2, sum, na.rm = TRUE)
eduTotalCity <- apply(eduCity[,3:9], 2, sum, na.rm = TRUE)
eduTotalTown <- apply(eduTown[,3:9], 2, sum, na.rm = TRUE)
eduTotalVillage <- apply(eduVillage[,3:9], 2, sum, na.rm = TRUE)
eduTotal <- data.frame(all = eduTotalAll, city = eduTotalCity, 
                       town = eduTotalTown, village = eduTotalVillage)
eduWherePercent <- 100 * data.frame(city = eduTotalCity / eduTotalAll,
                                    town = eduTotalTown / eduTotalAll,
                                    village = eduTotalVillage / eduTotalAll)
eduWherePercent$education <- factor(gsub("_", " ", rownames(eduWherePercent)),
                                    levels = gsub("_", " ", rownames(eduWherePercent)))
library(tidyr)
eduWherePercent <- gather(eduWherePercent, where, percent, -education)


#== male and female number at each education level
eduTotalMale <- apply(eduCombined[eduCombined$sex == "male", 3:9], 2, sum, na.rm = TRUE)
eduTotalFemale <- apply(eduCombined[eduCombined$sex == "female", 3:9], 2, sum, na.rm = TRUE)
eduGenderPercent <- 100 * data.frame(male = eduTotalMale / eduTotalAll,
                               female = eduTotalFemale / eduTotalAll)
eduGenderPercent$education <- factor(gsub("_", " ", rownames(eduGenderPercent)),
                                    levels = gsub("_", " ", rownames(eduGenderPercent)))
eduGenderPercent <- gather(eduGenderPercent, gender, percent, -education)

save(eduGenderPercent, eduWherePercent, file = "process_for_global_R.RData")
