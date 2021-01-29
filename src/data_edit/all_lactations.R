library(dplyr)
library(data.table)

lactdata <- fread("edit1_lactations.csv", verbose = T, nThread = 4)
lactdata$Parity[lactdata$Parity >= 20] <- NA
table(is.na(lactdata$Parity))
lactdata$Parity <- as.numeric(lactdata$Parity)
table(lactdata$Parity)

lactdata$Parity1 <- seq(1, nrow(lactdata), 1)
lactdata$Parity1[lactdata$Parity == 1] <- 1
lactdata$Parity1[lactdata$Parity == 2] <- 2
lactdata$Parity1[lactdata$Parity %in% c(3,4)] <- 3
lactdata$Parity1[lactdata$Parity > 4] <- 4
lactdata$Parity1[is.na(lactdata$Parity) == TRUE] <- NA
table(is.na(lactdata$Parity1))
lactdata$Parity1 <- as.factor(lactdata$Parity1)

animID <- fread("event_pm005geb.csv", drop = 2:14, nThread= 4, verbose = T)
animID <- unique(animID)

lactdata1 <- lactdata[lactdata$AnimalIdentifier %in% animID$AnimalIdentifier,]

rm(animID)
rm(lactdata)

animdata <- fread("edit4_anim_info.csv", verbose = T)
sub_animdata <- animdata[,c(1,4)]

lactdata2 <- lactdata1 %>% group_by(AnimalIdentifier) %>%
  left_join(sub_animdata, by = "AnimalIdentifier")

fwrite(lactdata2, "edit4_lactations.csv")


lactdata22 <- lactdata2 %>% group_by(UBN) %>%
  mutate(max_year = max(as.numeric(substr(EndDateLact, 1, 4))))

lactdata22$EndDateLact <- as.Date(as.character(lactdata22$EndDateLact), format = "%Y%m%d")

lactdata222 <- lactdata22 %>% group_by(AnimalIdentifier) %>%
  arrange(Parity) %>%
  mutate(., event = ifelse(row_number() == n_distinct(Parity) & max_year != year(EndDateLact), 1, 0))

lactdata222$EndDateLact <- as.character(lactdata222$EndDateLact)

lactdata24 <- lactdata222[lactdata222$EndCode == 4,]
lactdata25 <- lactdata222[lactdata222$EndCode == 5,]
lactdata26 <- lactdata222[lactdata222$EndCode == 6,]

saveRDS(lactdata24, "HPC_lactdata4")
saveRDS(lactdata25, "HPC_lactdata5")
saveRDS(lactdata26, "HPC_lactdata6")
fwrite(lactdata222, "event_lactations.csv")

