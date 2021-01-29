library(data.table)
library(dplyr)

df1 <- fread("survivaldata.csv", header = T)

df1$year <- substr(df1$TestDate, 1, 4)

df1 <- df1[df1$parity == "1st parity",]

df2 <- df1 %>% arrange(year) %>% group_by(year) %>% select(AnimalIdentifier, year, parity)
df21 <- df2[!duplicated(df2[,"AnimalIdentifier"]),]

df1_summary <- df21 %>%
  group_by(year) %>%
  summarize(new_animals = n_distinct(AnimalIdentifier))

df2 <- fread("survivaldata.csv", header = T)
df2$year <- substr(df2$TestDate, 1, 4)
df2_summary <- df2 %>% group_by(year) %>% summarize(event_animals = sum(event == 1))

fwrite(df1_summary, "influx_new_animals.csv", row.names = F)

df3 <- left_join(df1_summary, df2_summary, by = "year")

fwrite(df3, "influx_outflux_animals.csv", row.names = F)


