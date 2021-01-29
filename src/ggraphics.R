setwd("/Volumes/SDXC/new data")
library(dplyr)
library(data.table)
library(ggplot2)

df1 <- fread("survdata_herd_info.csv", header = T)

summary(df1)
df1 <- df1[df1$year >= 2009,]
df1$year <- as.factor(df1$year)
df1$UBN <- as.character(df1$UBN)

df1[df1$numberlive >= 1000,] <- NA 



df2 <- tidyr::gather(data = df1, key = type, value = number, numberculled, numberlive, factor_key = T)
df2[, 3:18] <- NULL
df2 <- df2[complete.cases(df2),]
pd = position_dodge(width = 0.75)
numberplot <- ggplot(data = df2, aes(x = year, y = number, fill = type)) +
  stat_boxplot(geom = "errorbar", position = pd, width = 0.2) +
  geom_boxplot(outlier.shape = NA,position = pd) +
  scale_y_continuous(limits = quantile(df2$number, c(0.01, 0.99), na.rm = T)) +
  geom_text(x = "2009", y = 200, label = "outliers not shown")
numberplot

