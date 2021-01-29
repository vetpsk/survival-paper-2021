library(data.table)
library(dplyr)

pmdata <- fread(file = "edit42_pm005geb.csv", header = T,
                sep = ",", verbose = T)
died_naturally <- fread("animals_died_naturally.csv", header = T, verbose = T)
pmdata$TestDate <- as.Date(as.character(pmdata$TestDate), format = "%Y%m%d")

pmdata_step1 <- setDT(pmdata) %>% group_by(HerdIdentifier) %>% 
  mutate(., "max_test_date" = max(TestDate))

pmdata_step2 <- pmdata_step1 %>% ungroup() %>%
  group_by(AnimalIdentifier) %>%
  mutate(event = if_else(TestDate == max(TestDate) & TestDate < max_test_date, 1, 0))
print("before died_naturally")
table(pmdata_step2$event == 1, pmdata_step2$year)

pmdata_step2$event[pmdata_step2$AnimalIdentifier %in% died_naturally$AnimalIdentifier] <- 0
print("after")
table(pmdata_step2$event == 1, pmdata_step2$year)

fwrite(pmdata_step2, file = "event1_pm005geb.csv", row.names = F, sep = ",")

