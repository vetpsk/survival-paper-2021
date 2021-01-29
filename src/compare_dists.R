library(data.table)
library(dplyr)
library(survival)

df2 <- fread("survivaldata.csv", header = T, verbose = T)
str(df2)
df2$HerdIdentifier <- as.factor(df2$HerdIdentifier)
df2$parity <- as.factor(df2$parity)
df2$event <- as.numeric(df2$event)
df2$SCCfactor <- as.factor(df2$SCCfactor)
df2$fprfactor <- as.factor(df2$fprfactor)
df2$fprfactor2 <- as.factor(df2$fprfactor2)
df2$LactValue1 <- as.factor(df2$LactValue1)
df2$perfactor <- as.factor(df2$perfactor)
df2$Insemfactor <- as.factor(df2$Insemfactor)
summary(df2)
df2$tstart[df2$tstart <= 0] <- NA
df2$tstop[df2$tstop <= 0] <- NA
summary(df2)

df2 <- distinct(df2)
df2 <- df2[complete.cases(df2),]


library(survival)
library(MASS)
modsurv10 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ perfactor + perfactor:LactValue1 + perfactor:SCCfactor +
                     perfactor:fprfactor + perfactor:parity + LactValue1 + parity + Insemfactor + SCCfactor + fprfactor + fprfactor2 + cluster(HerdIdentifier),
                   data = df2, dist = "weibull")
summary(modsurv10)
extractAIC(modsurv10)
rm(modsurv10)
modsurv10 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ perfactor + perfactor:LactValue1 + perfactor:SCCfactor +
                     perfactor:fprfactor + perfactor:parity + LactValue1 + parity + Insemfactor + SCCfactor + fprfactor + fprfactor2 + cluster(HerdIdentifier),
                   data = df2, dist = "loglogistic")
summary(modsurv10)
extractAIC(modsurv10)
rm(modsurv10)
modsurv10 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ perfactor + perfactor:LactValue1 + perfactor:SCCfactor +
                     perfactor:fprfactor + perfactor:parity + LactValue1 + parity + Insemfactor + SCCfactor + fprfactor + fprfactor2 + cluster(HerdIdentifier),
                   data = df2, dist = "logistic")
summary(modsurv10)
extractAIC(modsurv10)
rm(modsurv10)
modsurv10 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ perfactor + perfactor:LactValue1 + perfactor:SCCfactor +
                     perfactor:fprfactor + perfactor:parity + LactValue1 + parity + Insemfactor + SCCfactor + fprfactor + fprfactor2 + cluster(HerdIdentifier),
                   data = df2, dist = "exponential")
summary(modsurv10)
extractAIC(modsurv10)
rm(modsurv10)
modsurv10 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ perfactor + perfactor:LactValue1 + perfactor:SCCfactor +
                     perfactor:fprfactor + perfactor:parity + LactValue1 + parity + Insemfactor + SCCfactor + fprfactor + fprfactor2 + cluster(HerdIdentifier),
                   data = df2, dist = "lognormal")
summary(modsurv10)
extractAIC(modsurv10)
rm(modsurv10)
ls()

