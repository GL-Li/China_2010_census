# race, age and population
setwd("~/Dropbox/my_R_projects/China_2010_cencus/prepare_data")
library(readxl)
library(ggplot2)
library(tidyr)

# 2010 data ===================================================================
# clean data
data_raw <- read_excel("xls_data/race_2_1.xls", skip = 3)
df <- data_raw[c(1, seq(2, 178, 3))]
df <- df[-c(1, seq(3, 173, 6)),]
df <- df[!is.na(df$`年  龄`),]

col_names <- c("年龄", "合计", "汉族", "蒙古族", "回族", "藏族", "维吾尔族",
               "苗族", "彝族", "壮族", "布依族", "朝鲜族", "满族", "侗族",
               "瑶族", "白族", "土家族", "哈尼族", "哈萨克族", "傣族", "黎族",
               "傈僳族", "佤族", "畲族", "高山族", "拉祜族", "水族", "东乡族",
               "纳西族", "景颇族", "柯尔克孜族", "土族", "达斡尔族", "仫佬族",
               "羌族", "布朗族", "撒拉族", "毛南族", "仡佬族", "锡伯族", 
               "阿昌族", "普米族", "塔吉克族", "怒族", "乌孜别克族", "俄罗斯族",
               "鄂温克族", "德昂族", "保安族", "裕固族", "京族", "塔塔尔族", 
               "独龙族", "鄂伦春族", "赫哲族", "门巴族", "珞巴族", "基诺族", 
               "其他未识别的民族", "外国人加入中国籍")
names(df) <- col_names

# total population of each race
total <- as.numeric(df[1,])[-1]
names(total) <- names(df)[-1]
total_2010 <- total


# percentage distribution
data_pct <- data.frame(lapply(df[-1, -1], as.numeric))
data_pct <- data.frame(t(t(data_pct) / total))
data_pct["年龄"] <- 0:99

# for plot
df_pct <- data_pct[, 2:6]
df_pct["年龄"] <- 0:99

# long table
top_10 <- gather(df_pct, 民族,  比例 , -年龄)
top_10$民族 <- factor(top_10$民族, levels = unique(top_10$民族))

ggplot(top_10, aes(年龄, 比例 * 100, color = 民族)) + 
    geom_point() + geom_line() +
    xlim(c(0, 30)) +
    xlab("年龄") +
    ylab("百分比 （%）") +
    ggtitle("2010 年人口普查部分民族低龄人口比例") +
    scale_color_manual(values = c("red", "blue", "cyan", "orange", "green"))


# 2000 data ====================================================================
# clean data
data_raw <- read_excel("xls_data/Y2000/race_2_1_2000.xls", skip = 3)
df <- data_raw[c(1, seq(2, 178, 3))]
df <- df[!is.na(df$`年   龄   别` ),]
df <- df[-c(seq(2, 122, 6)),]
names(df) <- col_names

# total population of each race
total <- as.numeric(df[1,])[-1]
names(total) <- names(df)[-1]
total_2000 <- total

# percentage distribution
data_pct <- data.frame(lapply(df[-1, -1], as.numeric))
data_pct <- data.frame(t(t(data_pct) / total))
data_pct["年龄"] <- 0:99

# for plot
df_pct <- data_pct[, 2:6]
df_pct["年龄"] <- 0:99

# long table
top_10 <- gather(df_pct, 民族,  比例 , -年龄)
top_10$民族 <- factor(top_10$民族, levels = unique(top_10$民族))

ggplot(top_10, aes(年龄, 比例 * 100, color = 民族)) + 
    geom_point() + geom_line() +
    xlim(c(0, 20)) +
    xlab("年龄") +
    ylab("百分比 （%）") +
    ggtitle("2010 年人口普查部分民族低龄人口比例") +
    scale_color_manual(values = c("red", "blue", "cyan", "orange", "green"))


# growth 2000 - 2010 for each race =============================================
growth <- (total_2010 - total_2000) / total_2000
df_growth <- data.frame(growth = round(growth * 100, 2),
                        total_2000 = total_2000,
                        total_2010 = total_2010)
# ggplot(df_growth[-1,], aes(total_2000, growth)) +
#     geom_point(size = 4, color = "red", alpha = 0.5) + 
#     xlab("2000 年人口") + 
#     ylab("2000 - 2010 年人口变化 (%)") +
#     ggtitle("2000 - 2010 年各民族人口变化和 2000 年人口基数的关系") +
#     theme(plot.title = element_text(hjust = 0.5)) + 
#     scale_x_log10(breaks = c(1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9), 
#                   minor_breaks = c((2:9) *1e3, (2:9) *1e4, (2:9) *1e5, 
#                                    (2:9) *1e6, (2:9) *1e7, (2:9) *1e8), 
#                   labels = c("千",  "万", "十万", "百万", "千万", "亿", "十亿")) +
#     annotate("text", x = df_growth["外国人加入中国籍", "total_2000"] * 5,
#              y = df_growth["外国人加入中国籍", "growth"],
#              label = "外国人加入中国籍", size = 3) + 
#     annotate("text", x = df_growth["塔塔尔族", "total_2000"] * 2.5,
#              y = df_growth["塔塔尔族", "growth"],
#              label = "塔塔尔族", size = 3) +
#     annotate("text", x = df_growth["汉族", "total_2000"],
#              y = df_growth["汉族", "growth"] + 3.5,
#              label = "汉族", size = 3)  +
#     annotate("text", x = df_growth["维吾尔族", "total_2000"] * 2.5,
#              y = df_growth["维吾尔族", "growth"],
#              label = "维吾尔族", size = 3) + 
#     annotate("text", x = df_growth["满族", "total_2000"] * 1.8,
#              y = df_growth["满族", "growth"],
#              label = "满族", size = 3) + 
#     annotate("text", x = df_growth["壮族", "total_2000"] * 1.8,
#              y = df_growth["壮族", "growth"],
#              label = "壮族", size = 3) +
#     annotate("text", x = df_growth["其他未识别的民族", "total_2000"] * 5,
#              y = df_growth["其他未识别的民族", "growth"],
#              label = "其他未识别的民族", size = 3) +
#     annotate("text", x = df_growth["布朗族", "total_2000"] * 2.2,
#              y = df_growth["布朗族", "growth"],
#              label = "布朗族", size = 3) +
#     annotate("text", x = df_growth["藏族", "total_2000"] * 1.8,
#              y = df_growth["藏族", "growth"],
#              label = "藏族", size = 3) +
#     annotate("text", x = df_growth["彝族", "total_2000"] * 1.8,
#              y = df_growth["彝族", "growth"],
#              label = "彝族", size = 3) +
#     annotate("text", x = df_growth["回族", "total_2000"] * 1.8,
#              y = df_growth["回族", "growth"],
#              label = "回族", size = 3) +
#     annotate("text", x = df_growth["珞巴族", "total_2000"] * 2.2,
#              y = df_growth["珞巴族", "growth"],
#              label = "珞巴族", size = 3) +
#     annotate("text", x = df_growth["苗族", "total_2000"] * 0.35,
#              y = df_growth["苗族", "growth"] + 13,
#              label = "苗族", size = 3) +
#     annotate("text", x = df_growth["蒙古族", "total_2000"],
#              y = df_growth["蒙古族", "growth"] - 10,
#              label = "蒙古族", size = 3) +
#     annotate("text", x = df_growth["土家族", "total_2000"] * 0.15,
#              y = df_growth["土家族", "growth"] + 20,
#              label = "土家族", size = 3) +
#     annotate("segment", linetype = 3,
#              x = df_growth["蒙古族", "total_2000"],
#              xend = df_growth["蒙古族", "total_2000"],
#              y = df_growth["蒙古族", "growth"],
#              yend = df_growth["蒙古族", "growth"] - 8) +
#     annotate("segment", linetype = 3,
#              x = df_growth["土家族", "total_2000"],
#              xend = df_growth["土家族", "total_2000"] * 0.15,
#              y = df_growth["土家族", "growth"],
#              yend = df_growth["土家族", "growth"] + 18) +
#     annotate("segment", linetype = 3,
#              x = df_growth["苗族", "total_2000"],
#              xend = df_growth["苗族", "total_2000"] * 0.35,
#              y = df_growth["苗族", "growth"],
#              yend = df_growth["苗族", "growth"] + 11)
# 
# ggsave(file = "race_growth_rate_2000.png", width = 6, height = 4)

# 2010 base
ggplot(df_growth[-1,], aes(total_2010, growth)) +
    geom_point(size = 3, color = "red", alpha = 0.5) + 
    xlab("2010 年人口") + 
    ylab("2000 - 2010 年人口变化 (%)") +
    ggtitle("各民族 2000 - 2010 年人口变化和 2010 年人口") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_x_log10(breaks = c(1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9), 
                  minor_breaks = c((2:9) *1e3, (2:9) *1e4, (2:9) *1e5, 
                                   (2:9) *1e6, (2:9) *1e7, (2:9) *1e8), 
                  labels = c("千",  "万", "十万", "百万", "千万", "亿", "十亿")) +
    annotate("text", x = df_growth["外国人加入中国籍", "total_2010"] * 5,
             y = df_growth["外国人加入中国籍", "growth"],
             label = "外国人加入中国籍", size = 3) + 
    annotate("text", x = df_growth["塔塔尔族", "total_2010"] * 2.5,
             y = df_growth["塔塔尔族", "growth"],
             label = "塔塔尔族", size = 3) +
    annotate("text", x = df_growth["乌孜别克族", "total_2010"] * 3,
             y = df_growth["乌孜别克族", "growth"],
             label = "乌孜别克族", size = 3) +
    annotate("text", x = df_growth["汉族", "total_2010"],
             y = df_growth["汉族", "growth"] + 3.5,
             label = "汉族", size = 3)  +
    annotate("text", x = df_growth["维吾尔族", "total_2010"] * 2.5,
             y = df_growth["维吾尔族", "growth"],
             label = "维吾尔族", size = 3) + 
    annotate("text", x = df_growth["满族", "total_2010"] * 1.8,
             y = df_growth["满族", "growth"],
             label = "满族", size = 3) + 
    annotate("text", x = df_growth["壮族", "total_2010"] * 1.8,
             y = df_growth["壮族", "growth"],
             label = "壮族", size = 3) +
    annotate("text", x = df_growth["其他未识别的民族", "total_2010"] * 5,
             y = df_growth["其他未识别的民族", "growth"],
             label = "其他未识别的民族", size = 3) +
    annotate("text", x = df_growth["布朗族", "total_2010"] * 2.2,
             y = df_growth["布朗族", "growth"],
             label = "布朗族", size = 3) +
    annotate("text", x = df_growth["藏族", "total_2010"] * 1.8,
             y = df_growth["藏族", "growth"],
             label = "藏族", size = 3) +
    annotate("text", x = df_growth["彝族", "total_2010"] * 1.8,
             y = df_growth["彝族", "growth"],
             label = "彝族", size = 3) +
    annotate("text", x = df_growth["怒族", "total_2010"] * 1.8,
             y = df_growth["怒族", "growth"],
             label = "怒族", size = 3) +
    annotate("text", x = df_growth["回族", "total_2010"] * 1.8,
             y = df_growth["回族", "growth"],
             label = "回族", size = 3) +
    annotate("text", x = df_growth["珞巴族", "total_2010"] * 2.2,
             y = df_growth["珞巴族", "growth"],
             label = "珞巴族", size = 3) +
    annotate("text", x = df_growth["苗族", "total_2010"] * 0.35,
             y = df_growth["苗族", "growth"] + 13,
             label = "苗族", size = 3) +
    annotate("text", x = df_growth["蒙古族", "total_2010"],
             y = df_growth["蒙古族", "growth"] - 10,
             label = "蒙古族", size = 3) +
    annotate("text", x = df_growth["土家族", "total_2010"] * 0.15,
             y = df_growth["土家族", "growth"] + 20,
             label = "土家族", size = 3) +
    annotate("segment", linetype = 3,
             x = df_growth["蒙古族", "total_2010"],
             xend = df_growth["蒙古族", "total_2010"],
             y = df_growth["蒙古族", "growth"],
             yend = df_growth["蒙古族", "growth"] - 8) +
    annotate("segment", linetype = 3,
             x = df_growth["土家族", "total_2010"],
             xend = df_growth["土家族", "total_2010"] * 0.15,
             y = df_growth["土家族", "growth"],
             yend = df_growth["土家族", "growth"] + 18) +
    annotate("segment", linetype = 3,
             x = df_growth["苗族", "total_2010"],
             xend = df_growth["苗族", "total_2010"] * 0.35,
             y = df_growth["苗族", "growth"],
             yend = df_growth["苗族", "growth"] + 11) + 
    annotate("text", x = Inf, y = -Inf, 
             label = "数据来源：国家统计局网站 http://www.stats.gov.cn/", 
             cex = 2, hjust=1.1, vjust=-1.1) + 
    annotate("text", x = Inf, y = -Inf, label = "@武达盟",
             hjust=1.1, vjust=-1.1, col="white", cex=10,
             fontface = "bold", alpha = 0.5)

ggsave(file = "race_growth_rate_2010.png", width = 6, height = 4)

