library(data.table)
library(survival)

df2 <- fread("survivaldata.csv", header = T, verbose = T)
df2$parity <- as.character(df2$parity)
df2$parity <- as.factor(df2$parity)
df2$LactValue1 <- as.factor(df2$LactValue1)
df2$fprfactor <- as.factor(df2$fprfactor)
df2$SCCfactor <- as.factor(df2$SCCfactor)
df2$perfactor <- as.factor(df2$perfactor)
df2$Insemfactor <- as.factor(df2$Insemfactor)
summary(df2)

load("model10.Rdata")
deviance_resid <- as.data.frame(residuals(modsurv10, type = "deviance"))
fwrite(deviance_resid, "deviance_modsurv10.csv", row.names = F)


