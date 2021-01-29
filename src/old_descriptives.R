library(data.table)
library(dplyr)


pmdata2 <- fread("edit4_pm005geb.csv", header = T, verbose = T)

pmdata2$HerdIdentifier <- as.factor(pmdata2$HerdIdentifier)
pmdata2$TestDate <- as.Date(as.character(pmdata2$TestDate), format = "%Y%m%d")
pmdata2$BirthDate <- as.Date(as.character(pmdata2$BirthDate), format = "%Y%m%d")
pmdata2$CalvDate <- as.Date(as.character(pmdata2$CalvDate), format = "%Y%m%d")

summary(pmdata2)
str(pmdata2)

print("animals pm24")
length(unique(pmdata2$AnimalIdentifier))

print("herds pm4")
length(levels(pmdata2$HerdIdentifier))
length(unique(pmdata2$HerdIdentifier))
print("rows pm4")
nrow(pmdata2)
ncol(pmdata2)



pmdata3 <- fread("edit41_pm005geb.csv", header = T, verbose = T)

pmdata3$HerdIdentifier <- as.factor(pmdata3$HerdIdentifier)
pmdata3$TestDate <- as.Date(as.character(pmdata3$TestDate), format = "%Y%m%d")
pmdata3$BirthDate <- as.Date(as.character(pmdata3$BirthDate), format = "%Y%m%d")
pmdata3$CalvDate <- as.Date(as.character(pmdata3$CalvDate), format = "%Y%m%d")

summary(pmdata3)
str(pmdata3)

print("animals pm41")
length(unique(pmdata3$AnimalIdentifier))

print("herds pm41")
length(levels(pmdata3$HerdIdentifier))
length(unique(pmdata3$HerdIdentifier))
print("rows pm41")
nrow(pmdata3)
ncol(pmdata3)


rm(list = ls())

