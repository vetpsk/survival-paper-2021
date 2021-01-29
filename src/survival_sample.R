library(data.table)
library(dplyr)

survdata <- fread("survivaldata.csv", header = T, verbose = T, nThread = 8)
str(survdata)

survdata$parity <- as.factor(survdata$parity)
survdata$LactValue1 <- as.factor(survdata$LactValue1)
survdata$fprfactor <- as.factor(survdata$fprfactor)
survdata$fprfactor2 <- as.factor(survdata$fprfactor2)
survdata$SCCfactor <- as.factor(survdata$SCCfactor)
survdata$perfactor <- as.factor(survdata$perfactor)
survdata$Insemfactor <- as.factor(survdata$Insemfactor)

survsample <- sample(survdata$HerdIdentifier, 50, replace = F)

survdata <- survdata[survdata$HerdIdentifier %in% survsample,]

library(survival)

fwrite(survdata, "sample50_survdata.csv", row.names = F)


