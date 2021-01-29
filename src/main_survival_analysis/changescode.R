library(dplyr)
library(data.table)

df2 <- fread("survivaldata.csv", header = T, sep = ",", nThread = 2, verbose = T)

##########################DESCRIPTION OF DATA##############################
##### factors: 
################## lastparity 4 levels: last parity at time of failure/ event
################## 1 = 1st parity
################## 2 = 2nd parity
################## 3 = parity 3 and 4
################## 4 = more than 4 parities
# 
################## fprfactor (fat protein ratio): proportion of times fpr >1.5 
##################         on test dates of MPR where days in milk <= 100 days
################## <=10% = #fpr > 1.5/ # tests of fpr
################## >10%<=50%
################## >50%
# 
################## SCCfactor 5 levels
################## less than 200 => SCC < 200,000
################## 200 to 400 => 200,000 < SCC <= 400,000
################## 400 to 600 => 400,000 < SCC <= 600,000
################## 200 to 400 => 600,000 < SCC <= 1 million
################## more than 1000 => SCC > 1 million
# 
################## perfactor: calendar time based perturbations factor for each year of event, 4 levels: 
##################          before 2013, between 2013 and 2016, beyond 2016
################## based on --> year @ TestDate
# 
################## LactValue1: 2 levels
################## less than 100 = Lactatiewaarde <= 100
################## more than 100 = Lactatiewaarde > 100
#
################## Insemfactor: 2 levels
################## less than 2 = rolling average inseminations per parity <= 2
################## more than 2 = rolling average inseminations per parity > 2

df2$SCCfactor <- as.factor(df2$SCCfactor)
df2$SCCfactor <- relevel(df2$SCCfactor, ref = "below 200")
df2$perfactor <- as.factor(df2$perfactor)
df2$perfactor <- relevel(df2$perfactor, ref = "before 2014")
df2$fprfactor <- as.factor(df2$fprfactor)
df2$fprfactor <- relevel(df2$fprfactor, ref = "below 10%")
df2$fprfactor2 <- as.factor(df2$fprfactor2)
df2$fprfactor2 <- relevel(df2$fprfactor2, ref = "below 10%")
df2$lastparity <- as.factor(df2$lastparity)
df2$lastparity <- relevel(df2$lastparity, ref = "1st parity")
df2$parity <- as.factor(df2$parity)
df2$parity <- relevel(df2$parity, ref = "1st parity")
df2$LactValue1 <- as.factor(df2$LactValue1)
df2$LactValue1 <- relevel(df2$LactValue1, ref = "less than 100")
df2$Insemfactor <- as.factor(df2$Insemfactor)
df2$Insemfactor <- relevel(df2$Insemfactor, ref = "below 2")

df2$BirthDate <- as.Date(df2$BirthDate, format = "%Y-%m-%d")
df2$TestDate <- as.Date(df2$TestDate, format = "%Y-%m-%d")
df2$max_date <- as.Date(df2$max_date, format = "%Y-%m-%d")
df2$min_date <- as.Date(df2$min_date, format = "%Y-%m-%d")

str(df2)
summary(df2)


library(survival)
library(survminer)
library(ggplot2) 
library(multcomp)

######### model 2: all main effects
modsurv2 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~
                      LactValue1 + lastparity + Insemfactor +
                      SCCfactor + fprfactor + perfactor + 
                      lastparity:LactValue1 + lastparity:Insemfactor + lastparity:SCCfactor + lastparity:fprfactor +
                      cluster(HerdIdentifier),
                    data = df2, dist = "lognormal",
                    robust = TRUE, model = T)
summary(modsurv2)


fitted_modsurv2 <- modsurv2$linear.predictors
resids_modsurv2 <- (log(modsurv2$y[,1]) - fitted_modsurv2) / modsurv2$scale
resKm2 <- survfit(Surv(resids_modsurv2, event) ~ 1, data = df2[complete.cases(df2),])
plot(resKm2, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability",
     main = "Testing residuals for lognormal regression model 2")
xx <- seq(min(resids_modsurv2), max(resids_modsurv2), length.out = length(resids_modsurv2))
yy <- pnorm(xx, lower.tail = FALSE)
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate",
                       "Survival function of Lognormal distribution"),
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

curve(1-plnorm(x, modsurv2$coefficients[1],modsurv2$scale),0,800, main = "baseline survival from model2", ylab = "survival probability", xlab = "time in weeks", add = F)

## all interactions model
modsurv3 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                      LactValue1 + strata(parity) + Insemfactor + 
                      SCCfactor + fprfactor + perfactor + 
                      cluster(HerdIdentifier), 
                    data = df2, dist = "lognormal",
                    robust = TRUE, model = T)
summary(modsurv3)


curve(1-plnorm(x, modsurv3$coefficients[1],modsurv3$scale[1]),0,1000, 
      main = "baseline survival from model2", ylab = "survival probability", 
      xlab = "time in weeks")
curve(1-plnorm(x, modsurv3$coefficients[1],modsurv3$scale[2]),0,1000, col = "red", add = T)
curve(1-plnorm(x, modsurv3$coefficients[1],modsurv3$scale[3]),0,1000, col = "blue", add = T)
curve(1-plnorm(x, modsurv3$coefficients[1],modsurv3$scale[4]),0,1000, col = "green", add = T)
legend(100,0.6, legend = c("1st parity", "2nd parity", "3-4 parities", "more than 4"), col = c("black", "red", "blue", "green"), cex = 0.65, lty = 1)

fitted_modsurv3 <- modsurv3$linear.predictors
resids_modsurv3 <- (log(modsurv3$y[,1]) - fitted_modsurv3) / modsurv3$scale

resKm3 <- survfit(Surv(resids_modsurv3, event) ~ 1, data = df2[complete.cases(df2),])
plot(resKm3, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability",
     main = "Testing residuals for Lognormal regression model 3")

xx3 <- seq(min(resids_modsurv3), max(resids_modsurv3), length.out = length(resids_modsurv3))
yy3 <- pnorm(xx3, lower.tail = F)
lines(xx3, yy3, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of normal distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

########## stepwise model selection
## AIC

extractAIC(modsurv2)
extractAIC(modsurv3)

anova(modsurv2, modsurv3)

stepAIC(modsurv3)

## Building new model based on AIC backwards selection

modsurv4 <- survreg(formula = Surv(tstart, tstop, event = event, type = "interval") ~ 
                      LactValue1 + lastparity + Insemfactor + 
                      SCCfactor + fprfactor + perfactor + 
                      lastparity:perfactor + Insemfactor:perfactor + SCCfactor:perfactor, 
                    data = df2, dist = "lognormal", model = T, robust = TRUE, 
                    cluster = HerdIdentifier)
summary(modsurv4)
fitted_modsurv4 <- modsurv4$linear.predictors
resids_modsurv4 <- (log(modsurv4$y[,1]) - fitted_modsurv4) / modsurv4$scale

resKm4 <- survfit(Surv(resids_modsurv4, event) ~ 1, data = df2[complete.cases(df2),])
plot(resKm4, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability",
     main = "Testing residuals for Lognormal regression model 4")

xx4 <- seq(min(resids_modsurv4), max(resids_modsurv4), length.out = length(resids_modsurv4))
yy4 <- pnorm(xx4, lower.tail = F)
lines(xx4, yy4, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of normal distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

## difference in main effects and full model
curve(1-plnorm(x, modsurv2$coefficients[1],modsurv2$scale),0,800, main = "baseline survival from model2", ylab = "survival probability", xlab = "time in weeks", add = F)
curve(1-plnorm(x, modsurv4$coefficients[1],modsurv4$scale),0,800, main = "baseline survival from model2 and model4", ylab = "survival probability", xlab = "time in weeks", col = "red", add = T)
text(100, 0.1, "model 2")
text(600, 0.8, "model 4\n (slight difference is seen)", col = "red")








ND <- with(df2, expand.grid(
  parity = levels(parity),
  LactValue1 = levels(LactValue1),
  fprfactor = levels(fprfactor),
  fprfactor2 = levels(fprfactor2),
  SCCfactor = levels(SCCfactor),
  perfactor = levels(perfactor),
  Insemfactor = levels(Insemfactor),
  HerdIdentifier = unique(HerdIdentifier)
))

prs <- predict(modsurv6, ND, se.fit = TRUE, type = "lp")
ND$pred <- prs[[1]]
ND$se <- prs[[2]]
ND$lo <- ND$pred - 1.96 * ND$se
ND$up <- ND$pred + 1.96 * ND$se

predictions = ND[order(-ND$pred),]
fwrite(predictions, "predictions.csv", row.names = F)
par(mfcol = c(2, 3), las = 0, cex = 0.5)
# plot(ND$pred ~ ND$parity,xlab = "Parity number", ylab = "log(Time) in weeks", boxwex = 0.3, horizontal  =T)
plot(ND$pred ~ ND$LactValue1, xlab = "Relative milk production level", ylab = "log(Time) in weeks", boxwex = 0.3, horizontal  =T, ylim = c(5.2,7.0))
plot(ND$pred ~ ND$fprfactor, xlab = "Count Fat-Prot ratio > 1.5", ylab = "log(Time) in weeks", boxwex = 0.3, horizontal  =T, ylim = c(5.2,7.0))
plot(ND$pred ~ ND$fprfactor2, xlab = "Count Fat-Prot ratio < 0.9", ylab = "log(Time) in weeks", boxwex = 0.3, horizontal  =T, ylim = c(5.2,7.0))
plot(ND$pred ~ ND$SCCfactor, xlab = "Somatic cell count X 1000", ylab = "log(Time) in weeks", boxwex = 0.3, horizontal  =T, ylim = c(5.2,7.0))
plot(ND$pred ~ ND$perfactor,xlab = "Policy Period", ylab = "log(Time) in weeks", boxwex = 0.3, horizontal  =T, ylim = c(5.2,7.0))
plot(ND$pred ~ ND$Insemfactor, xlab = "Rolling average of inseminations", ylab = "log(Time) in weeks", boxwex = 0.3, horizontal  =T, ylim = c(5.2,7.0))
par(las = 1)
mtext("Predicted log-Survival against associated factors", outer = TRUE, cex = 1.5, line = -2.5)

