setwd("~/Dropbox/my_R_projects/China_2010_cencus")

library(readxl); library(ggplot2); library(tidyr)

read_population <- function(xls_file) {
    popul <- read_excel(xls_file, col_types=rep("numeric", 4))
    colnames(popul) <- c("age", "total", "male", "female")
    
    popul[3, 1] <- 0
    popul <- popul[!is.na(popul$age),]
    
    popul <- gather(popul, sex, population, -c(age, total))
    popul$percent <- popul$population / popul$total * 100
    
    popul$birth_year <- 2010 - popul$age
    return(popul)
}

# total population ============================================================
popTotal <- read_population("xls_data/population_total_sect3_1.xls")

# add text column for labeling  the plot of population
popTotal$text1 <- rep("", nrow(popTotal))
popTotal$text1[popTotal$birth_year == 1961 & popTotal$sex == "female"] <- "A"
popTotal$text1[popTotal$birth_year == 1967 & popTotal$sex == "female"] <- "B"
popTotal$text1[popTotal$birth_year == 1980 & popTotal$sex == "female"] <- "C"
popTotal$text1[popTotal$birth_year == 1990 & popTotal$sex == "female"] <- "D"

popTotal$description1 <- rep("", nrow(popTotal))
popTotal$description1[popTotal$birth_year == 1961 & popTotal$sex == "female"] <- 
    paste("1958 - 1960: Great famine.", 
          "The number of baby born or survived dropped by over 30%.",
          "The birth number recovered quickly back to uptrend and even overshot",
          "after the nation exit the famine")
popTotal$description1[popTotal$birth_year == 1967 & popTotal$sex == "female"] <- 
    "1966: Culture Revolution started. "
popTotal$description1[popTotal$birth_year == 1980 & popTotal$sex == "female"] <- 
    paste("1974 - 1985: The decline of birth number in this period was caused", 
          "by three factors. The first is that the number of new mother came out of",
          "the great famine was low. The second factor is that the one-child-policy", 
          "was enforced in 1980. In the same year, the legal age for marrage was",
          "raised by two year to 20 for female, which further reduced the number of",
          "new borns.")
popTotal$description1[popTotal$birth_year == 1990 & popTotal$sex == "female"] <- 
    paste("1990: the post-famine baby boom pushed the number of new born",
          "to a new high before the number dropped sharply.",
          "Starting from 1990, the government enhanced the one-child-policy,",
          "plus it is the time of the second generation of those born in great",
          "famine. These two factors combined cause contineous decline of birth.")

# add text column for labeling  the plot of percentage
popTotal$text2 <- rep("", nrow(popTotal))
popTotal$text2[popTotal$birth_year == 1915 & popTotal$sex == "female"] <- "E"
popTotal$text2[popTotal$birth_year == 2005 & popTotal$sex == "female"] <- "F"

popTotal$description2 <- rep("", nrow(popTotal))
popTotal$description2[popTotal$birth_year == 1915 & popTotal$sex == "female"] <- 
    paste("Old population: The difference in the percentages of old males old ", 
          "females appears striking but it is perfectly normal as males are ", 
          "good at living a shorter life world wide. The more worrisome is ", 
          "the difference among childrens.")
popTotal$description2[popTotal$birth_year == 2005 & popTotal$sex == "female"] <- 
    paste("Children: since 1990, there are far more boys born than girls, ", 
          "a consequence of strictly enforced one-child-policy. ",
          "Traditionally, Chinese favors boys over girls. When they can only ",
          "have one child, they are more likely to choose a boy. This selection ",
          "became possible thanks to ultrosonic imaging techniques, ",
          "even though the practice of sex selection is banned in China.")



# compare population in city, town, and villiage ==============================
# city
popCity <- read_population("xls_data/population_city_sect3_1a.xls")
popCity$where <- "city"

popCity$text <- rep("", nrow(popCity))
popCity$text[popCity$age == 25 & popCity$sex == "female"] <- "G"
popCity$text[popCity$age == 5 & popCity$sex == "female"] <- "H"

popCity$description <- rep("", nrow(popCity))
popCity$description[popCity$age == 25 & popCity$sex == "female"] <- 
    paste("Immigrant workers: Men and women in villiage move to city for better ", 
          "income. They usually work in labor intensive factories and work for ", 
          "long hours. They have to be strong to survive the hard work.", 
          "")
popCity$description[popCity$age == 5 & popCity$sex == "female"] <- 
    paste("Children of immigrant workers: most of workers' children are left ", 
          "behind in the villiage. There are two reasons. The first is that  ",
          "these hard working worker have no time to take care of their children ",
          "in cities. But more importantly, cities descriminate them - their children ",
          "are not allowed to attend public schools where they are working, unless ",
          "paying a hefty fee they cannot afford.")

# town
popTown <- read_population("xls_data/population_town_sect3_1b.xls")
popTown$where <- "town"

popTown$text <- rep("", nrow(popTown))

popTown$description <- rep("", nrow(popTown))

# villiage
popVilliage <- read_population("xls_data/population_village_sect3_1c.xls")
popVilliage$where <- "villiage"

popVilliage$text <- rep("", nrow(popVilliage))

popVilliage$description <- rep("", nrow(popVilliage))

# combined
popCombined <- rbind(popCity, popTown, popVilliage)

# save data for shiny =========================================================
save(popTotal, popCity, popTown, popVilliage, popCombined, 
     file = "RData/population.RData")
save(popTotal, popCity, popTown, popVilliage, popCombined, 
     file = "shiny_English/RData/population.RData")


# make test plots =============================================================


gRatio <- ggplot(data = popTotal, aes(x = birth_year, y = percent))
gRatio + geom_line(aes(color = sex)) + geom_point(aes(color = sex)) +
    scale_x_continuous(breaks = seq(1910, 2010, by = 20)) +
    xlab("Birth Year") +
    ylab("Percentage (%)")

gPopul <- ggplot(data = popTotal, aes(x = birth_year, y = population / 1e6))
gPopul + geom_line(aes(color = sex)) + geom_point(aes(color = sex)) +
    scale_x_continuous(breaks = seq(1910, 2010, by = 20)) + 
    xlab("Year of Birth") + 
    ylab("Population (million)") + 
    geom_text(aes(label = text1), vjust = 2)

# aes(facet, size)
ggplot(data = popCombined, aes(x = age, y = population / 1e6)) + 
    geom_line(aes(color = sex, facet = where, size = where)) + 
    scale_size_manual(values = c(1.2, 0.7, 0.3)) + 
    scale_x_continuous(breaks = seq(0, 100, by = 20)) + 
    xlab("Age") + 
    ylab("Population (million)")

# aes(facet, shape), too busy
ggplot(data = popCombined, aes(x = age, y = population / 1e6)) + 
    geom_line(aes(color = sex, facet = where)) + 
    geom_point(aes(color = sex, facet = where, shape = where), size = 3) + 
    scale_x_continuous(breaks = seq(0, 100, by = 20)) + 
    xlab("Age") + 
    ylab("Population (million)")

# facet_wrap(), population
ggplot(data = popCombined, aes(x = age, y = population / 1e6)) + 
    geom_line(aes(color = sex), size = 0.2) + 
    geom_point(aes(color = sex), size = 1) +
    facet_wrap(~ where) + 
    scale_x_continuous(breaks = seq(0, 100, by = 20)) + 
    xlab("Age") + 
    ylab("Population (million)") + 
    theme(legend.text=element_text(size=10),
          legend.title=element_blank())

# facet_wrap(), percent
ggplot(data = popCombined, aes(x = age, y = percent)) + 
    geom_line(aes(color = sex), size = 0.2) + 
    geom_point(aes(color = sex), size = 1) +
    facet_wrap(~ where) + 
    scale_x_continuous(breaks = seq(0, 100, by = 20)) + 
    xlab("Age") + 
    ylab("Population (million)")