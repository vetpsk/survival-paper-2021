
library(data.table)
library(dplyr)

df1 <- fread("survivaldata.csv", header = T, nThread = 4)
df1$TestDate <- as.Date(df1$TestDate, format = "%Y-%m-%d")
df1$year <- year(df1$TestDate)

#df1_parity <- df1 %>% group_by(parity, year) %>% summarize(animals = n_distinct(AnimalIdentifier))
#fwrite(df1_parity, "summary_parity.csv", row.names = F)
df1_herdyear <- df1 %>% group_by(HerdIdentifier, year) %>% summarize(culled = sum(event == 1), total = n_distinct(AnimalIdentifier), tests = n_distinct(TestDate))
df1_herdyear$alive = df1_herdyear$total - df1_herdyear$culled

#average_test <- df1_herdyear %>% group_by(year) %>% summarize(tests = mean(tests))

#fwrite(average_test, "summary_averagetest.csv", row.names = F)
fwrite(df1_herdyear, "summary_herdyear.csv", row.names = F)
