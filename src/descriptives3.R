library(data.table)
library(dplyr)

pmdata <- fread("event1_pm005geb.csv", header = T, verbose = T)
#n <- 0
#pmsummary_event <- pmdata %>% group_by(year) %>%
#summarize(cullingevent = sum(event == 1))
#fwrite(pmsummary_event, "pmsummary_event.csv", row.names = F)
herdsum <- pmdata %>% group_by(HerdIdentifier, year) %>%
  summarise(herdsize = n_distinct(AnimalIdentifier), culling = sum(event == 1)) %>%
  mutate(culling_rate = culling/herdsize)
herdsum2 <- herdsum %>% ungroup() %>% group_by(year) %>% 
  summarize(mean_culling = mean(culling_rate), min_rate = min(culling_rate),
            max_rate = max(culling_rate))
fwrite(herdsum2, "cullingrate_year.csv", row.names = F)
