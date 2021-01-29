setwd("/Volumes/SDXC/Summaries")
library(data.table)
library(ggplot2)

df1 <- fread("summary_parity.csv", header = T, sep = ",")

g <- ggplot(data = df1, mapping = aes(x = year, y = animals)) +
  geom_path(aes(color = parity)) + geom_point(aes(shape = parity, color = parity), size = 2) +
  scale_x_discrete(limits = seq(2009, 2019, 1)) + scale_y_continuous(breaks = seq(200000, 800000, 100000),labels = seq(200000, 800000, 100000))
g + theme_bw() + xlab("Year") + ylab("No. of Animals")
