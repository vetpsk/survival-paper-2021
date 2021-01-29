library(data.table)
library(dplyr)

pmdata <- fread("event1_pm005geb.csv", header = T, verbose = T)
animdata <- fread("tr_anim_info.csv",
                  header = F,
                  sep = ";",
                  drop = 5,
                  verbose = T,
                  col.names = c("AnimalIdentifier", "BirthDate", "EndDate", "EndCode"))

print("MPR")
length(unique(pmdata$AnimalIdentifier))
print("animal data")
length(unique(animdata$AnimalIdentifier))
print("intersect")
length(intersect(unique(pmdata$AnimalIdentifier), unique(animdata$AnimalIdentifier)))

pmdata$BirthDate <- as.Date(as.character(pmdata$BirthDate), format = "%Y%m%d")
animdata$BirthDate <- as.Date(as.character(animdata$BirthDate), format = "%Y%m%d")

print("attempt 1, with ID and BirthDate")
mergedf <- pmdata %>% group_by(AnimalIdentifier, BirthDate) %>% left_join(animdata, by = c("AnimalIdentifier", "BirthDate"))

str(mergedf)
length(unique(mergedf$AnimalIdentifier))


print("attempt 2, wih ID only")
mergedf <- pmdata %>% group_by(AnimalIdentifier) %>% left_join(animdata, by = ("AnimalIdentifier" = "AnimalIdentifier"))
str(mergedf)
length(unique(mergedf$AnimalIdentifier))

print("attempt 3, with different method : data.table, key = AnimalIDs")
pmdata <- setDT(pmdata, key  = "AnimalIdentifier")
animdata <- setDT(animdata, key = "AnimalIdentifier")
mergedf <- merge(pmdata, animdata, all = F)
str(mergedf)
length(unique(mergedf$AnimalIdentifier))

print("attempt 4, with data.table and 2 keys: IDs and Birth")
pmdata <- setDT(pmdata, key  = c("AnimalIdentifier", "BirthDate"))
animdata <- setDT(animdata, key = c("AnimalIdentifier", "BirthDate"))
mergedf <- merge(pmdata, animdata, all = F)
str(mergedf)
length(unique(mergedf$AnimalIdentifier)) 

print("attempt 5, subsetting animal info data with MPR IDs")
animdata1 <- animdata[animdata$AnimalIdentifier %in% unique(pmdata$AnimalIdentifier),]
length(animdata1$AnimalIdentifier)
str(animdata1)

rm(list = ls(all = TRUE)
 
