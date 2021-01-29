library(data.table)
library(dplyr)

setwd("/Volumes/SDXC/sample")

df1 <- fread("sample10_survdata.csv", header = T)
df1$year <- substr(df1$TestDate, 1, 4)
df1 <- df1[df1$parity == "1st parity",]
df2 <- df1 %>% arrange(year) %>% group_by(year) %>% select(AnimalIdentifier, year, parity)
df21 <- df2[!duplicated(df2[,"AnimalIdentifier"]),]





df1_summary <- df21 %>%
  group_by(year) %>%
  summarize(new_animals = n_distinct(AnimalIdentifier))

df2 <- fread("sample10_survdata.csv", header = T)
df2$year <- substr(df2$TestDate, 1, 4)
df2_summary <- df2 %>% group_by(year) %>% suummarize(event_animals = sum(event == 1))

df2$perfactor <- as.factor(df2$perfactor)
df2$Insemfactor <- as.factor(df2$Insemfactor)

df2_summary <- df2 %>% group_by(perfactor) %>%
  summarize(animals = n_distinct(AnimalIdentifier), farms = n_distinct(HerdIdentifier), obs = n())
df_summary <- left_join(df1_summary, df2_summary, by = "year")

library(ggplot2)
df_summary$year <- as.numeric(df_summary$year)
a <- ggplot(data = df_summary, aes(x = year)) +
  geom_line(aes(y = new_animals), color = "blue") +
  geom_line(aes(y = event_animals), color = "red") +
  scale_x_continuous(breaks = seq(2009, 2019, 1))
a
