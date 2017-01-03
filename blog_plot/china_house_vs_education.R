# house and education - data from China 2010 cencus
setwd("~/Dropbox/my_R_projects/China_2010_cencus/blog_plot")

library(ggplot2)
load("RData/house.RData")
myThemeAxis <- theme(axis.title = element_text(size = 14),
                     axis.text = element_text(size = 12, color = "black"),
                     legend.text = element_text(size=11))


# plot in English =============================================================
plotHouseEdu <- ggplot(houseEduCombined, aes(education, area_per_person)) +
    geom_line(aes(group = where, color = where), alpha = 0.6) + 
    geom_point(aes(size = number_of_household, color = where), alpha = 0.6) +
    guides(color = "legend", size = FALSE) +
    scale_size_area(max_size = 20) + 
    expand_limits(y = c(23.8, 43.2)) +
    # do not use limits = c(26, 43) below. click returns nothing out of the range
    scale_y_continuous(breaks = seq(24, 43, 2)) +
    scale_color_manual(values = c("city" = "blue", "town" = "green",
                                  "village" = "red")) +
    theme(legend.position = c(0, 1), 
          legend.justification = c(0, 1),
          legend.title=element_blank()) + 
    theme(axis.text.x = element_text(angle = 18, hjust = 1),
          axis.title.x = element_blank()) +
    #     xlab("Final Education") +
    ylab(expression(Area~per~Person~(m^2))) +
    #     labs(list(x = "Final Education", y = expression("Area per Person (m^2)"))) + 
    ggtitle("dependence on education")

plotHouseEdu

# plot in Chinese ==============================================================
plotHouseEdu <- ggplot(houseEduCombined, aes(受教育程度, area_per_person)) +
    geom_line(aes(group = where, color = 地点), alpha = 0.6) + 
    geom_point(aes(size = number_of_household, color = 地点), alpha = 0.6) +
    guides(color = "legend", size = FALSE) +
    scale_size_area(max_size = 25) + 
    expand_limits(y = c(23.8, 43.2)) +
    # do not use limits = c(26, 43) below. click returns nothing out of the range
    scale_y_continuous(breaks = seq(24, 43, 2)) +
    scale_color_manual(values = c("城市" = "blue", "乡镇" = "green",
                                  "农村" = "red")) +
    theme(legend.position = c(0, 1), 
          legend.justification = c(0, 1),
          legend.title=element_blank()) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.title.x = element_blank()) +
    # xlab("教育程度") +
    ylab(expression(人均面积(m^2))) +
    #     labs(list(x = "Final Education", y = expression("Area per Person (m^2)"))) + 
    ggtitle("人均居住面积和户主受教育程度关系（圆圈面积正比于户数）") +
    annotate("text", x = Inf, y = -Inf, label = "武达盟",
             hjust=1.1, vjust=-1.1, col="white", cex=6,
             fontface = "bold", alpha = 0.5) +
    myThemeAxis

plotHouseEdu
ggsave("~/Dropbox/blog_writing/cchere/house_edu.png", width = 6, height = 4)