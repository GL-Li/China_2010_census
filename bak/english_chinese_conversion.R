load("chinaCencusEducation.RData")
col0 <- colnames(education_new)

# more logical order
education <- education_new[c("age", "sex", "population", "noSchool", "elementarySchool",
                             "middleSchool", "highSchool", "twoYearCollege",
                             "fourYearCollege", "college", "graduate")]

# rename for easy processing
colnames(education) <- c("age", "sex", "population", "no_school", "elementary_school",
                         "middle_school", "high_school", "two_year_college",
                         "four_year_college", "all_college", "graduate")

教育_年龄 <- education
names(教育_年龄) <- c("年龄", "性别", "人口", "从未上学", "小学",
                  "初中", "高中", "大学专科",
                  "大学本科", "所有大学", "研究生")

# add new columns for percentage of population
for (i in 4:11) {
    tempName <- colnames(education[i])
    tempName <- paste0("fraction_", tempName)
    education[tempName] <- education[i] / education["population"]
}

for (i in 4:11) {
    tempName <- colnames(教育_年龄[i])
    tempName <- paste0(tempName, "比例")
    教育_年龄[tempName] <- 教育_年龄[i] / 教育_年龄["人口"]
}

save(education, 教育_年龄, file = "china_education_age.RData")
