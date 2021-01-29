library(data.table)
library(dplyr)

#Insemination merge
insemdata <- setDT(insemdata) %>% dplyr::select(AnimalIdentifier, BirthDate, Parity, InsemNumber, CalvingDate)
insemdata <- insemdata[!duplicated(insemdata)]
insemdata <- insemdata[insemdata$AnimalIdentifier %in% df1$AnimalIdentifier,]
insemdata <- insemdata[!duplicated(insemdata)]
setkey(insemdata, AnimalIdentifier, Parity)
insemdata[,rollInsem:= as.numeric(frollmean(InsemNumber,2)),by = AnimalIdentifier]
insemdata$rollInsem[is.na(insemdata$rollInsem) == T] <- insemdata$InsemNumber[is.na(insemdata$rollInsem) == T]


df1 <- df1 %>% left_join(insemdata, by = c("AnimalIdentifier" = "AnimalIdentifier", "CalvDate" = "CalvingDate"))
df1$BirthDate.y <- NULL

df1$Parity.y <- NULL
df1$BirthDate.x
df1 <- df1 %>% rename(BirthDate = BirthDate.x, Parity = Parity.x)
df1$Insemfactor[df1$rollInsem > 5.0] <- "more than 5" 
df1$Insemfactor[df1$rollInsem <= 5.0 & df1$rollInsem >= 2.0]<- "between 2 - 5" 
df1$Insemfactor[df1$rollInsem < 2.0] <- "less than 2" 

df1$Insemfactor <- as.factor(df1$Insemfactor)
rm(insemdata)
## as.Date

df1$BirthDate <- as.Date(as.character(df1$BirthDate), format = "%Y%m%d")
df1$TestDate <- as.Date(as.character(df1$TestDate), format = "%Y-%m-%d")
df1$CalvDate <- as.Date(as.character(df1$CalvDate), format = "%Y%m%d")


## fpr factor
df1 <- df1 %>% group_by(AnimalIdentifier) %>%
  mutate(max_date = max(TestDate))
df1 <- df1 %>% group_by(AnimalIdentifier) %>%
  mutate(min_date = min(TestDate))
df1$lactlength <- df1$TestDate-df1$CalvDate
df1$fpr <- ifelse(df1$lactlength <= 100, df1$Fat/df1$Protein, NA)

n <- 0
df1 <- df1 %>% group_by(AnimalIdentifier) %>%
  mutate(., fpr2 = ifelse(fpr > 1.5, n+1, n+0), fprtot = n_distinct(fpr)) %>%
  mutate(., fprn = sum(fpr2/fprtot, na.rm = T)) %>%
  dplyr::select(-c(fpr2,fprtot))

df1$fprfactor[df1$fprn <= 0.1] <- "<=10%"
df1$fprfactor[df1$fprn > 0.1 & df1$fprn <= 0.5] <- ">10% to <=50%"
df1$fprfactor[df1$fprn > 0.5] <- ">50%"

df1$fprfactor <- as.factor(df1$fprfactor)


## SCC factor

df1$SCCfactor <- rep(1, nrow(df1))
df1$SCCfactor[df1$SCC < 200] <- "less than 200"
df1$SCCfactor[df1$SCC >= 200 & df1$SCC <= 400] <- "between 200 and 400"
df1$SCCfactor[df1$SCC > 400 & df1$SCC <= 600] <- "between 400 and 600"
df1$SCCfactor[df1$SCC > 600 & df1$SCC <= 1000] <- "between 600 and 1000"
df1$SCCfactor[df1$SCC > 1000] <- "more than 1000"
df1$SCCfactor[is.na(df1$SCC) == TRUE] <- "missing"
df1$SCCfactor <- as.factor(df1$SCCfactor)



## parity factor

df1$parityfactor <- rep(0, times = nrow(df1))
df1 <- df1 %>% group_by(AnimalIdentifier) %>% mutate(., parityfactor = max(Parity))

df1$lastparity[df1$parityfactor == 1] <- 1
df1$lastparity[df1$parityfactor == 2] <- 2
df1$lastparity[df1$parityfactor %in% c(3,4)] <- 3
df1$lastparity[df1$parityfactor > 4] <- 4
df1$lastparity[df1$parityfactor == 0] <- NA
df1$lastparity <- as.factor(df1$lastparity)

df1$parityfactor <- NULL

## tstart tstop

df2 <- df1 %>% group_by(AnimalIdentifier) %>% 
  mutate(tstart  = as.numeric(TestDate - BirthDate)/7)
df2 <- df2 %>% group_by(AnimalIdentifier) %>%
  mutate(., tstop = ifelse(TestDate == max_date, tstart + mean(diff(tstart)),lead(tstart)))



df2 <- df2 %>% group_by(AnimalIdentifier) %>%
  mutate(., perturbations = ifelse(max(event) == 1, as.numeric(year(max(TestDate))), 9999))
df2$perfactor <- as.character(df2$perturbations)
# df2$perfactor <- relevel(df2$perfactor, ref = "censored")
## df2$perfactor <- relevel(df2$perfactor, ref = "2009")


## lactvalue
df2$LactValue1 <- rep(0, nrow(df2))
df2$LactValue1[df2$LactValue <= 100] <- "less than 100"
df2$LactValue1[df2$LactValue > 100] <- "more than 100"
df2$LactValue1 <- as.factor(df2$LactValue1)

## drop columns

df2 <- df2 %>% dplyr::select(., c(AnimalIdentifier, BirthDate, HerdIdentifier, TestDate,
                           event, max_date, min_date, SCCfactor, fprfactor, 
                           lastparity, tstart, tstop, LactValue, LactValue1, perfactor, rollInsem, Insemfactor))
df2$fprfactor <- relevel(df2$fprfactor, ref = "<=10%")
# levels(df2$SCCfactor) <- c("less than 400", "400 to 1000", "more than 1000")


df2 <- df2[complete.cases(df2),]
df2 <- distinct(df2)
## load libraries

library(survival)
library(survminer)
library(ggplot2) 
library(multcomp)

## surv models

modsurv <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                     LactValue1 +  SCCfactor + perfactor +
                     fprfactor + lastparity + Insemfactor +
                     cluster(HerdIdentifier), 
                   data = df2, dist = "weibull",
                   robust = TRUE, model = T)
summary(modsurv)
modsurv2 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                      LactValue1 + strata(perfactor) +
                      SCCfactor + fprfactor + cluster(HerdIdentifier), 
                    data = df2, dist = "loglogistic",
                    robust = TRUE, model = T)
summary(modsurv2)



## testing residuals
fitted_modsurv <- modsurv$linear.predictors
resids_modsurv <- (log(modsurv$y[,1]) - fitted_modsurv) / modsurv$scale

resKm <- survfit(Surv(resids_modsurv, event) ~ 1, data = df2[complete.cases(df2),])
plot(resKm, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability",
     main = "Testing residuals for Weibull regression")

xx <- seq(min(resids_modsurv), max(resids_modsurv), length.out = length(resids_modsurv))
yy <- exp(-exp(xx))
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of Extreme Value distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")


fitted_modsurv2 <- modsurv2$linear.predictors
resids_modsurv2 <- (log(modsurv2$y[,1]) - fitted_modsurv2) / modsurv2$scale
resKm2 <- survfit(Surv(resids_modsurv2, event) ~ 1, data = df2[complete.cases(df2),])
plot(resKm2, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability", 
     main = "Testing residuals for loglogistic regression")
xx <- seq(min(resids_modsurv2), max(resids_modsurv2), length.out = length(resids_modsurv2))
yy <- plogis(xx, lower.tail = FALSE)
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of Logistic distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

