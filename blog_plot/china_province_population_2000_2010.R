setwd("~/Dropbox/my_R_projects/China_2010_cencus/blog_plot")

library(ggplot2)

# prepare population data =============================================================
load("RData/province_population.RData")
popul <- prov_popul_2000_2010
popul$Change_pct = round((popul$人口2010 - popul$人口2000) / popul$人口2000 * 100, 2)



# plot =========================================================================
# load("~/Dropbox/my_R_projects/000_template/plot_on_China_province_map.RData")
source("~/Dropbox/my_R_projects/all_my_functions/map_functions/china_map_functions.R")
df_popul <- popul[c(1, 6)]
fill_china_province_map(
    df_popul, "short_name", midpoint = 7.26,
    save_as = "figures/prov_population_change_2000_2010.png",
    title = "2000 - 2010 年各省市区常住人口变化百分数 （全国平均 7.26%)"
)


# df_family_size <- popul[c(1, 5)]
# fill_china_map(df_family_size, midpoint = 3.53,
#                   save_as = "figures/prov_family_size_2010_1.png",
#                   title = "2010 年各省市区户均人口",
#                   save_size = 8)
