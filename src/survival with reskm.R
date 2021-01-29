library(data.table)
library(dplyr)
library(survival)
setwd("/Volumes/SDXC/sample")
df2 <- fread("sample50_survdata.csv", header = T, verbose = T)
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

par(mfrow = c(3,2), cex = 1, yaxt = "n", mar = c(2,2,1,1))
modsurv10 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ perfactor + perfactor:LactValue1 + perfactor:SCCfactor +
                       perfactor:fprfactor + perfactor:parity + LactValue1 + parity + Insemfactor + SCCfactor + fprfactor + fprfactor2 + cluster(HerdIdentifier),
                     data = df2, dist = "weibull")
summary(modsurv10)

fitted_modsurv10 <- modsurv10$linear.predictors
resids_modsurv10 <- (log(modsurv10$y[,1]) - fitted_modsurv10) / modsurv10$scale
resKm10 <- survfit(Surv(resids_modsurv10, event) ~ 1, data = df2[complete.cases(df2),])
plot(resKm10, mark.time = FALSE, 
     main = "A. Weibull")
xx <- seq(min(resids_modsurv10), max(resids_modsurv10), length.out = length(resids_modsurv10))
yy <- exp(-exp(xx))
lines(xx, yy, col = "gray", lwd = 3)
# legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       # "Survival function of Extreme Value distribution"), 
       # lty = c(1,2,1), col = c(1,1,2), bty = "n")


modsurv10 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ perfactor + perfactor:LactValue1 + perfactor:SCCfactor +
                       perfactor:fprfactor + perfactor:parity + LactValue1 + parity + Insemfactor + SCCfactor + fprfactor + fprfactor2 + cluster(HerdIdentifier),
                     data = df2, dist = "loglogistic")
summary(modsurv10)
fitted_modsurv10 <- modsurv10$linear.predictors
resids_modsurv10 <- (log(modsurv10$y[,1]) - fitted_modsurv10) / modsurv10$scale
resKm10 <- survfit(Surv(resids_modsurv10, event) ~ 1, data = df2[complete.cases(df2),])
plot(resKm10, mark.time = FALSE, 
     main = "B. Log-logistic")
xx <- seq(min(resids_modsurv10), max(resids_modsurv10), length.out = length(resids_modsurv10))
yy <- plogis(xx, lower.tail = FALSE)
lines(xx, yy, col = "gray", lwd = 3)
# legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
#                        "Survival function of Logistic distribution"), 
#        lty = c(1,2,1), col = c(1,1,2), bty = "n")

modsurv10 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ perfactor + perfactor:LactValue1 + perfactor:SCCfactor +
                       perfactor:fprfactor + perfactor:parity + LactValue1 + parity + Insemfactor + SCCfactor + fprfactor + fprfactor2 + cluster(HerdIdentifier),
                     data = df2, dist = "logistic")
summary(modsurv10)
fitted_modsurv10 <- modsurv10$linear.predictors
resids_modsurv10 <- (log(modsurv10$y[,1]) - fitted_modsurv10) / modsurv10$scale
resKm10 <- survfit(Surv(resids_modsurv10, event) ~ 1, data = df2[complete.cases(df2),])
plot(resKm10, mark.time = FALSE, main = "C. Logistic")
xx <- seq(min(resids_modsurv10), max(resids_modsurv10), length.out = length(resids_modsurv10))
yy <- plogis(xx, lower.tail = FALSE)
lines(xx, yy, col = "gray", lwd = 3)
# legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
#                        "Survival function of Logistic distribution"), 
#        lty = c(1,2,1), col = c(1,1,2), bty = "n")

modsurv10 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ perfactor + perfactor:LactValue1 + perfactor:SCCfactor +
                       perfactor:fprfactor + perfactor:parity + LactValue1 + parity + Insemfactor + SCCfactor + fprfactor + fprfactor2 + cluster(HerdIdentifier),
                     data = df2, dist = "exponential")
summary(modsurv10)
fitted_modsurv10 <- modsurv10$linear.predictors
resids_modsurv10 <- (log(modsurv10$y[,1]) - fitted_modsurv10) / modsurv10$scale
resKm10 <- survfit(Surv(resids_modsurv10, event) ~ 1, data = df2[complete.cases(df2),])
plot(resKm10, mark.time = FALSE, main = "D. Exponential")
xx <- seq(min(resids_modsurv10), max(resids_modsurv10), length.out = length(resids_modsurv10))
yy <- exp(-exp(xx))
lines(xx, yy, col = "gray", lwd = 3)
# legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
#                        "Survival function of Extreme Value distribution"), 
#        lty = c(1,2,1), col = c(1,1,2), bty = "n")

modsurv10 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ perfactor + perfactor:LactValue1 + perfactor:SCCfactor +
                       perfactor:fprfactor + perfactor:parity + LactValue1 + parity + Insemfactor + SCCfactor + fprfactor + fprfactor2 + cluster(HerdIdentifier),
                     data = df2, dist = "lognormal")
summary(modsurv10)
fitted_modsurv10 <- modsurv10$linear.predictors
resids_modsurv10 <- (log(modsurv10$y[,1]) - fitted_modsurv10) / modsurv10$scale
resKm10 <- survfit(Surv(resids_modsurv10, event) ~ 1, data = df2[complete.cases(df2),])
plot(resKm10, mark.time = FALSE, main = "E. Log-normal")
xx <- seq(min(resids_modsurv10), max(resids_modsurv10), length.out = length(resids_modsurv10))
yy <- pnorm(xx, lower.tail = FALSE)
lines(xx, yy, col = "gray", lwd = 3)
# legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
#                        "Survival function of Normal distribution"), 
#        lty = c(1,2,1), col = c(1,1,2), bty = "n")

plot.new()
legend("left", c("KM estimate", "95% CI KM estimate",
                       "Survival function of error distribution"),
       lty = c(1,2,1), lwd = c(1,1,3), col = c("black", "black", "gray"), bty = "n", xpd = TRUE, cex = 1)
