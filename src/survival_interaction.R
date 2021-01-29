library(data.table)
library(dplyr)
library(survival)
df2 <- fread("survivaldata.csv", header = T)

df2$SCCfactor <- as.factor(df2$SCCfactor)
df2$perfactor <- as.factor(df2$perfactor)
df2$fprfactor <- as.factor(df2$fprfactor)
df2$fprfactor2 <- as.factor(df2$fprfactor2)
df2$parity <- as.factor(df2$parity)
df2$LactValue1 <- as.factor(df2$LactValue1)
df2$Insemfactor <- as.factor(df2$Insemfactor)


df2$BirthDate <- as.Date(df2$BirthDate, format = "%Y-%m-%d")
df2$TestDate <- as.Date(df2$TestDate, format = "%Y-%m-%d")
df2$max_date <- as.Date(df2$max_date, format = "%Y-%m-%d")
df2$min_date <- as.Date(df2$min_date, format = "%Y-%m-%d")
df2$year <- as.numeric(year(df2$TestDate))
df2$tstart[df2$tstart <= 0] <- NA
df2$tstop[df2$tstop <= 0] <- NA
summary(df2)

modsurvInsem <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~
                       LactValue1 + parity  +
                       SCCfactor + fprfactor + fprfactor2 + 
                         perfactor + perfactor:Insemfactor + 
                         perfactor:LactValue1 + perfactor:parity + perfactor:SCCfactor + 
                         perfactor:fprfactor + perfactor:fprfactor2 + 
                       cluster(HerdIdentifier),
                     data = df2 , dist = "lognormal", robust = T, model = T)

summary(modsurvInsem)
AIC(modsurvInsem)
save(modsurvInsem, file = "modelInsem.Rdata")
rm(modsurvInsem)

modsurvParity <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~
                          LactValue1 + Insemfactor  +
                          SCCfactor + fprfactor + fprfactor2 + 
                          perfactor + perfactor:Insemfactor + 
                          perfactor:LactValue1 + perfactor:parity + perfactor:SCCfactor + 
                          perfactor:fprfactor + perfactor:fprfactor2 + 
                          cluster(HerdIdentifier),
                        data = df2 , dist = "lognormal",
                        robust = TRUE, model = T)

summary(modsurvParity)
AIC(modsurvParity)
save(modsurvParity, file = "modelParity.Rdata")
rm(modsurvParity)

modsurvLactValue1 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~
                          Insemfactor + parity  +
                          SCCfactor + fprfactor + fprfactor2 + 
                          perfactor + perfactor:Insemfactor + 
                          perfactor:LactValue1 + perfactor:parity + perfactor:SCCfactor + 
                          perfactor:fprfactor + perfactor:fprfactor2 + 
                          cluster(HerdIdentifier),
                        data = df2 , dist = "lognormal",
                        robust = TRUE, model = T)

summary(modsurvLactValue1)
AIC(modsurvLactValue1)
save(modsurvLactValue1, file = "modelLactValue1.Rdata")
rm(modsurvLactValue1)

modsurvSCC <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~
                          LactValue1 + parity  +
                          Insemfactor + fprfactor + fprfactor2 + 
                          perfactor + perfactor:Insemfactor + 
                          perfactor:LactValue1 + perfactor:parity + perfactor:SCCfactor + 
                          perfactor:fprfactor + perfactor:fprfactor2 + 
                          cluster(HerdIdentifier),
                        data = df2 , dist = "lognormal",
                        robust = TRUE, model = T)

summary(modsurvSCC)
AIC(modsurvSCC)
save(modsurvSCC, file = "modelSCC.Rdata")
rm(modsurvSCC)

modsurvFpr1<- survreg(Surv(tstart, tstop, event = event, type = "interval") ~
                          LactValue1 + parity  +
                          SCCfactor + Insemfactor + fprfactor2 + 
                          perfactor + perfactor:Insemfactor + 
                          perfactor:LactValue1 + perfactor:parity + perfactor:SCCfactor + 
                          perfactor:fprfactor + perfactor:fprfactor2 + 
                          cluster(HerdIdentifier),
                        data = df2 , dist = "lognormal",
                        robust = TRUE, model = T)

summary(modsurvFpr1)
AIC(modsurvFpr1)
save(modsurvFpr1, file = "modelFpr1.Rdata")
rm(modsurvFpr1)

modsurvFpr2 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~
                          LactValue1 + parity  +
                          SCCfactor + fprfactor + Insemfactor + 
                          perfactor + perfactor:Insemfactor + 
                          perfactor:LactValue1 + perfactor:parity + perfactor:SCCfactor + 
                          perfactor:fprfactor + perfactor:fprfactor2 + 
                          cluster(HerdIdentifier),
                        data = df2 , dist = "lognormal",
                        robust = TRUE, model = T)

summary(modsurvFpr2)
AIC(modsurvFpr2)
save(modsurvFpr2, file = "modelFpr2.Rdata")
rm(modsurvFpr2)

ls()


