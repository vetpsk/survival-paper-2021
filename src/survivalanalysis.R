library(dplyr)
library(data.table)

df1 <- fread("event1_pm005geb.csv", verbose = T, header = T, sep = ",", nThread = 2)
insemdata <- fread("edit4_inseminations.csv", header = T, verbose = T)

insemdata <- setDT(insemdata) %>% dplyr::select(AnimalIdentifier, BirthDate, Parity, InsemNumber, CalvingDate)
insemdata <- insemdata[!duplicated(insemdata)]
insemdata <- insemdata[insemdata$AnimalIdentifier %in% df1$AnimalIdentifier,]
insemdata <- insemdata[!duplicated(insemdata)]
setkey(insemdata, AnimalIdentifier, Parity)
insemdata[,rollInsem:= as.numeric(frollmean(InsemNumber,2)),by = AnimalIdentifier]
insemdata$rollInsem[is.na(insemdata$rollInsem) == T] <- insemdata$InsemNumber[is.na(insemdata$rollInsem) == T]


df1 <- df1 %>% left_join(insemdata, by = c("AnimalIdentifier" = "AnimalIdentifier", "CalvDate" = "CalvingDate"))
df1$BirthDate.y <- NULL
df1$Insemnumber <- NULL
df1$Parity.y <- NULL
df1$BirthDate.x
df1 <- df1 %>% rename(BirthDate = BirthDate.x, Parity = Parity.x)
df1$Insemfactor[df1$rollInsem < 2] <- "below 2"
df1$Insemfactor[df1$rollInsem >=2 & df1$rollInsem <5] <- "between 2 and 5"
df1$Insemfactor[df1$rollInsem >= 5] <- "beyond 5"
df1$Insemfactor <- as.factor(df1$Insemfactor)
df1 <- dplyr::distinct(df1)
rm(insemdata)

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
df1$fprfactor <- NULL
df1$fprfactor[df1$fprn <= 0.1] <- "below 10%"
df1$fprfactor[df1$fprn > 0.1 & df1$fprn <= 0.5] <- "between 10% and 50%"
df1$fprfactor[df1$fprn > 0.5] <- "more than 50%"
df1$fprfactor <- as.factor(df1$fprfactor)
df1$fprfactor <- relevel(df1$fprfactor, ref = "below 10%")

## SCC factor

df1$SCCfactor[df1$SCC <= 200] <- "below 200"
df1$SCCfactor[df1$SCC > 200 & df1$SCC <= 600] <- "between 200 and 600"
df1$SCCfactor[df1$SCC > 600 & df1$SCC <= 1000] <- "between 600 and 1000"
df1$SCCfactor[df1$SCC > 1000] <- "more than 1000"
df1$SCCfactor <- as.factor(df1$SCCfactor)
df1$SCCfactor <- relevel(df1$SCCfactor, ref = "below 200")

## parity factor

df1$parityfactor <- rep(0, times = nrow(df1))
df1 <- df1 %>% group_by(AnimalIdentifier) %>% mutate(., parityfactor = max(Parity))

df1$lastparity[df1$parityfactor == 1] <- "1st parity"
df1$lastparity[df1$parityfactor == 2] <- "2nd parity"
df1$lastparity[df1$parityfactor %in% c(3,4)] <- "3-4th parity"
df1$lastparity[df1$parityfactor > 4] <- "more than 4 parities"
df1$lastparity <- as.factor(df1$lastparity)

df1$parity[df1$Parity == 1] <- "1st parity"
df1$parity[df1$Parity == 2] <- "2nd parity"
df1$parity[df1$Parity > 2 & df1$Parity < 5] <- "3-4th parity"
df1$parity[df1$Parity > 4] <- "more than 4 parities"
df1$parity <- as.factor(df1$parity)
df1$parity <- relevel(df1$parity, ref = "1st parity")

## tstart tstop

df2 <- df1 %>% group_by(AnimalIdentifier) %>% 
  mutate(tstart  = as.numeric(TestDate - BirthDate)/7)
df2 <- df2 %>% group_by(AnimalIdentifier) %>%
  mutate(., tstop = ifelse(TestDate == max_date, tstart + mean(diff(tstart)),lead(tstart)))
head(df2)


df2$perturbations <- year(df2$TestDate)
df2$perfactor[df2$perturbations <= 2013] <- "before 2013"
df2$perfactor[df2$perturbations > 2013 & df2$perturbations <= 2016] <- "between 2014 and 2016"
df2$perfactor[df2$perturbations > 2016] <- "beyond 2016"
df2$perfactor <- as.factor(df2$perfactor)

df2$perfactor <- relevel(df2$perfactor, ref = "before 2013")

## lactvalue
df2$LactValue1 <- rep(NA, nrow(df2))
df2$LactValue1[df2$LactValue <= 100 & df2$LactValue >= 0] <- "less than 100"
df2$LactValue1[df2$LactValue > 100] <- "more than 100"
df2$LactValue1 <- as.factor(df2$LactValue1)

## drop columns

df2 <- df2 %>% dplyr::select(., c(AnimalIdentifier, BirthDate, HerdIdentifier, parity, TestDate, 
                                  event, max_date, min_date, SCCfactor, fprfactor, 
                                  lastparity, tstart, tstop, LactValue, LactValue1, perfactor, Insemfactor))
df2 <- distinct(df2)
df2 <- df2[complete.cases(df2),]
head(df2)

library(survival)
modsurv2 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~
                      LactValue1 + parity + Insemfactor +
                      SCCfactor + fprfactor + perfactor +
                      cluster(HerdIdentifier),
                    data = df2, dist = "lognormal",
                    robust = TRUE, model = T)
summary(modsurv2)
save(modsurv2, "model2.Rdata")

fitted_modsurv2 <- modsurv2$linear.predictors
resids_modsurv2 <- (log(modsurv2$y[,1]) - fitted_modsurv2) / modsurv2$scale
resKm2 <- survfit(Surv(resids_modsurv2, event) ~ 1, data = df2[complete.cases(df2),])
save(resKm2, "residual_model2.Rdata")

modsurv3 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~
                      LactValue1 + strata(parity) + Insemfactor +
                      SCCfactor + fprfactor + perfactor +
                      cluster(HerdIdentifier),
                    data = df2, dist = "lognormal",
                    robust = TRUE, model = T)
summary(modsurv3)
save(modsurv3, "model3.Rdata")

# fitted_modsurv31 <- modsurv3$linear.predictors
# resids_modsurv31 <- (log(modsurv3$y[,1]) - fitted_modsurv3) / modsurv3$scale[1]
# fitted_modsurv32 <- modsurv3$linear.predictors
# resids_modsurv32 <- (log(modsurv3$y[,1]) - fitted_modsurv3) / modsurv3$scale[2]
# fitted_modsurv33 <- modsurv3$linear.predictors
# resids_modsurv33 <- (log(modsurv3$y[,1]) - fitted_modsurv3) / modsurv3$scale[3]
# fitted_modsurv34 <- modsurv3$linear.predictors
# resids_modsurv34 <- (log(modsurv3$y[,1]) - fitted_modsurv3) / modsurv3$scale[4]
# resKm31 <- survfit(Surv(resids_modsurv31, event) ~ 1, data = df2[complete.cases(df2),])
# save(resKm31, "residual_model31.Rdata")
# resKm32 <- survfit(Surv(resids_modsurv32, event) ~ 1, data = df2[complete.cases(df2),])
# save(resKm32, "residual_model32.Rdata")
# resKm33 <- survfit(Surv(resids_modsurv33, event) ~ 1, data = df2[complete.cases(df2),])
# save(resKm33, "residual_model33.Rdata")
# resKm34 <- survfit(Surv(resids_modsurv34, event) ~ 1, data = df2[complete.cases(df2),])
# save(resKm34, "residual_model34.Rdata")
fwrite(df2, "survivaldata.csv", row.names = F)
rm(list = ls(all = T))

