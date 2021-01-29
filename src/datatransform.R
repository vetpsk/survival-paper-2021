library(data.table)
library(dplyr)

pmdata <- fread("edit41_pm005geb.csv", header = T)

print("missing birthdate")
nrow(pmdata[pmdata$BirthDate <= 19500101,])
print(paste("animals = ", length(unique(pmdata$AnimalIdentifier[pmdata$BirthDate <= 19500101])))) 
print("missing CalvDate")
nrow(pmdata[pmdata$CalvDate <= 19500101,])
print(paste("animals = ", length(unique(pmdata$AnimalIdentifier[pmdata$CalvDate <= 19500101]))))
print("missing milk yield")
nrow(pmdata[pmdata$Kgmilk == 0,])
print(paste("animals = ", length(unique(pmdata$AnimalIdentifier[pmdata$Kgmilk == 0]))))
print("missing fat")
nrow(pmdata[pmdata$Fat == 0,])
print(paste("animals = ", length(unique(pmdata$AnimalIdentifier[pmdata$Fat == 0]))))
print("missing protein")
nrow(pmdata[pmdata$Protein == 0,])
print(paste("animals = ", length(unique(pmdata$AnimalIdentifier[pmdata$Protein == 0]))))
print("missing SCC")
nrow(pmdata[pmdata$SCC == 0,])
print(paste("animals = ", length(unique(pmdata$AnimalIdentifier[pmdata$SCC == 0]))))
print("LactValue negative")
nrow(pmdata[pmdata$LactValue < 0,])
print(paste("animals = ", length(unique(pmdata$AnimalIdentifier[pmdata$LactValue < 0]))))

pmdata$BirthDate[pmdata$BirthDate <= 19500101] <- NA
pmdata$CalvDate[pmdata$CalvDate <= 19500101] <- NA
pmdata$Kgmilk[pmdata$Kgmilk == 0] <- NA
pmdata$Fat[pmdata$Fat == 0] <- NA
pmdata$Protein[pmdata$Protein == 0] <- NA
pmdata$SCC[pmdata$SCC == 0] <- NA

fwrite(pmdata, "edit42_pm005geb.csv", row.names = F)
print("birth, calv, milk, dat, prot, scc")
table(is.na(pmdata$BirthDate) == TRUE)
table(is.na(pmdata$CalvDate) == TRUE)
table(is.na(pmdata$Kgmilk) == TRUE)
table(is.na(pmdata$Fat) == TRUE)
table(is.na(pmdata$Protein) == TRUE)
table(is.na(pmdata$SCC) == TRUE)

rm(list = ls())


