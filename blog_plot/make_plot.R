##### plot for house ==========================================================
library(ggplot2)
load("house.RData")

### preprocess data
# order 受教育程度 for better plot
houseEduCombined$受教育程度 <- factor(houseEduCombined$受教育程度,
                                 ordered = TRUE,
                                 levels = houseEduCombined$受教育程度[1:7])
# add new column in Chinese
translateWhere <- c("城市", "乡镇", "农村")
names(translateWhere) <- c("city", "town", "village")
houseEduCombined$城乡 <- translateWhere[houseEduCombined$where]
houseEduCombined$城乡 <- factor(houseEduCombined$城乡, ordered = TRUE,
                              levels = c("城市", "乡镇", "农村"))
houseEduCombined$户数 <- houseEduCombined$number_of_household / 10000

# set up theme for ggplot
myThemeAxis <- theme(axis.title = element_text(size = 12),
                     axis.text = element_text(size = 11, color = "black"),
                     legend.text=element_text(size=11))

# make plot
ggplot(houseEduCombined, aes(受教育程度, area_per_person)) +
    geom_line(aes(group = where, color = 城乡), alpha = 0.6) + 
    geom_point(aes(size = 户数, color = 城乡), alpha = 0.6) +
    expand_limits(y = c(23.8, 44.2)) +
    scale_y_continuous(breaks = seq(24, 44, 4)) +
    scale_size("户数（万）", range = c(0.5, 15)) + 
    scale_color_manual("居住地", values = c("城市" = "blue", "乡镇" = "green",
                                  "农村" = "red")) +
    xlab("户主受教育程度") +
    ylab(expression(人均住房面积~(m^2))) +
    ggtitle("2010年第六次人口普查：住房和教育") +
    annotate("text", x = Inf, y = -Inf, label = "武达盟",
             hjust=1.1, vjust=-1.1, col="white", cex=6,
             fontface = "bold", alpha = 0.8)

ggsave(filename = "house_vs_edu.png", width = 6, height = 4)


### 各类教育程度人口在同龄人口中的比例 =========================================
load("eduCombined.RData")

# convert to long table
library(tidyr)
eduLong <- gather(eduCombined, 教育程度, 人口, -c(age, population, sex, birth, where))

# 新增列 - 同龄人口中的比例
eduLong$比例 <- 100 * eduLong$人口 /eduLong$population 

# 数值换成中文
sexToChinese <- c("男", "女")
names(sexToChinese) <- c("male", "female")
eduLong$性别 <- sexToChinese[eduLong$sex]
eduLong$性别 <- factor(eduLong$性别, ordered = TRUE, levels = c("男", "女"))

eduLevelToChinese <- c("未上过学", "小学", "初中", "高中", 
                       "大学专科", "大学本科", "研究生")
names(eduLevelToChinese) <- c("no_school", "elementary_school", "middle_school",
                              "high_school", "specialized_college",
                              "university", "graduate_school")
eduLong$教育程度 <- eduLevelToChinese[eduLong$教育程度]
eduLong$教育程度 <- factor(eduLong$教育程度, ordered = TRUE,
                       levels = c("未上过学", "小学", "初中", "高中", 
                                  "大学专科", "大学本科", "研究生"))

whereToChinese <- c("城市", "乡镇", "农村")
names(whereToChinese) <- c("city", "town", "village")
eduLong$居住地 <- whereToChinese[eduLong$where]
eduLong$居住地 <- factor(eduLong$居住地, ordered = TRUE,
                      levels = c("城市", "乡镇", "农村"))



ggplot(eduLong, aes(age, 比例)) + 
    geom_line(aes(color = 性别)) + 
    geom_point(aes(color = 性别), size = 0.3) +
    facet_grid(居住地 ~ 教育程度) +
    ggtitle("2010年第六次人口普查：最终学历的演化") + 
    xlab("年龄") + 
    ylab("同龄人口中的比例 (%)") +
    theme(legend.position = c(1, 1), 
          legend.justification = c(1, 1),
          legend.title = element_blank())

ggsave(filename = "education_population_age.png", width = 6, height = 4)
