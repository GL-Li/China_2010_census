library(ggplot2)
source("global_functions.R")

# need to clean the excel file before reading
# remove unused rows and columns
# fill in missing cells, read_excel does not handel missing cells
population <- read_cencus_age("population_total_sect3_1.xls",
                              colNumber = 4, skip = 0)

# read_excel() does not read "0" into to numeric, quite annoying
population[3,1] <- 0

# make data ready for plot
population <- clean_cencus(population, 3, 4, c("age", "population"))
population$birthYear <- 2010 - population$age

# plot male and female side by side
g <- ggplot(data = population, aes(x = birthYear, y = population / 1e6))
g + geom_line(aes(color = sex)) + 
    geom_point(aes(color = sex)) +
    scale_x_continuous(breaks = seq(1900, 2020, by = 20) ) + 
    ggtitle("2010 pupulation") +
    xlab("Birth Year") +
    ylab("Population (million)")
