library(dplyr)
library(data.table)


df1 <- fread("survivaldata.csv", header = T)
df1$BirthDate <- as.Date(df1$BirthDate, format = "%Y-%m-%d")
df1$TestDate <- as.Date(df1$TestDate, format = "%Y-%m-%d")

df1_age <- df1 %>% group_by(AnimalIdentifier) %>% summarize(age_at_cull = mean(max(TestDate) - BirthDate), year_of_culling = year(max(TestDate)))

fwrite(df1_age, "summary_age_cull.csv", row.names = F)
