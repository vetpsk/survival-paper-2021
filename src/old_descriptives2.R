library(data.table)
library(dplyr)

pmdata <- fread("edit42_pm005geb.csv", header = T, verbose = T)

pm_yearsummary <- pmdata %>%
group_by(HerdIdentifier, TestDate) %>%
summarize(., animals = n_distinct(AnimalIdentifier))

pm_yearsummary$year <-  substr(as.character(pm_yearsummary$TestDate), 1, 4)

pm_herdsize <- pm_yearsummary %>%
group_by(year, HerdIdentifier) %>%
summarize(mean_herdsize = mean(animals))

pm_yearsherdsize <- pm_herdsize %>% 
group_by(year) %>%
summarize(., animals = mean(mean_herdsize), max = max(mean_herdsize), min = min(mean_herdsize))


fwrite(pm_yearsherdsize, "pm_yearsherdsize.csv", row.names = F)
fwrite(pm_herdsize, "pm_herdsize.csv", row.names = F)


