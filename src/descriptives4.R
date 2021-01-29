library(data.table)
library(dplyr)

pmdata <- fread("event1_pm005geb.csv", header = T, verbose = T)

event_yearsummary <- pmdata %>%
group_by(HerdIdentifier, TestDate) %>%
summarize(., animals = n_distinct(AnimalIdentifier), events = sum(event == 1))

event_yearsummary$culling <- event_yearsummary$events/event_yearsummary$animals
 
event_yearsummary$year <-  substr(as.character(event_yearsummary$TestDate), 1, 4)

event_herdsize <- event_yearsummary %>%
group_by(year, HerdIdentifier) %>%
summarize(mean_culling = mean(culling), mean_herdsize = mean(animals))

event_yearsherdsize <- event_herdsize %>% 
group_by(year) %>%
summarize(., cullingrate = mean(mean_culling), animals = mean(mean_herdsize))

fwrite(event_yearsummary, "culling per herd.csv", row.names = F)
