library(dplyr)
library(data.table)
#setwd

## load data

pmdata <- fread("./Original data/event2_endcode_pm005geb.csv", nThread = 4, verbose = T)
# set.seed(990)
sam_herd <- sample(pmdata$HerdIdentifier , 50, replace = F)

df1 <- pmdata[pmdata$HerdIdentifier %in% sam_herd,]
sam_animal <- unique(df1$AnimalIdentifier)



## dates as dates 


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
  select(-c(fpr2,fprtot))

df1$fprfactor[df1$fprn <= 0.1] <- "<=10%"
df1$fprfactor[df1$fprn > 0.1 & df1$fprn <= 0.5] <- ">10% to <=50%"
df1$fprfactor[df1$fprn > 0.5] <- ">50%"

df1$fprfactor <- as.factor(df1$fprfactor)


## SCC factor

df1$SCCfactor <- ifelse(df1$SCC >= 400, ifelse(df1$SCC>= 1000, 2, 1), 0)
df1$SCCfactor <- as.factor(df1$SCCfactor)




## parity factor

df1$parityfactor <- rep(0, times = nrow(df1))
df1$parityfactor[df1$Parity == 1] <- 1
df1$parityfactor[df1$Parity == 2] <- 2
df1$parityfactor[df1$Parity %in% c(3,4)] <- 3
df1$parityfactor[df1$Parity > 4] <- 4
df1$parityfactor[df1$parityfactor == 0] <- NA
df1$parityfactor <- as.factor(df1$parityfactor)





## tstart tstop

df2 <- df1 %>% group_by(AnimalIdentifier) %>% 
  mutate(tstart  = as.numeric(TestDate - BirthDate)/7)
df2 <- df2 %>% group_by(AnimalIdentifier) %>%
  mutate(., tstop = ifelse(TestDate == max_date, tstart + mean(diff(tstart)),lead(tstart)))



df2 <- df2 %>% group_by(AnimalIdentifier) %>%
  mutate(., perturbations = ifelse(max(event) == 1, as.character(year(max(TestDate))), "censored"))
df2$perfactor <- as.factor(df2$perturbations)
# df2$perfactor <- relevel(df2$perfactor, ref = "censored")
## df2$perfactor <- relevel(df2$perfactor, ref = "2009")
df2$perfactor <- relevel(df2$perfactor, ref = "2019")

## lactvalue
df2$LactValue1 <- rep(0, nrow(df2))
df2$LactValue1[df2$LactValue <= 100] <- "less than 100"
df2$LactValue1[df2$LactValue > 100] <- "more than 100"
df2$LactValue1 <- as.factor(df2$LactValue1)

## drop columns

df2 <- df2 %>% select(., c(AnimalIdentifier, BirthDate, Parity, HerdIdentifier, TestDate, 
                           event, max_date, min_date, SCCfactor, fprfactor, 
                           parityfactor, tstart, tstop, LactValue, LactValue1, perfactor))
df2$fprfactor <- relevel(df2$fprfactor, ref = "<=10%")
levels(df2$SCCfactor) <- c("less than 400", "400 to 1000", "more than 1000")



## load libraries

library(survival)
library(survminer)
library(ggplot2) 
library(multcomp)


## cox ph models

modsurv <- coxph(Surv(tstart, tstop, event = event) ~ 
                   LactValue1 + strata(parityfactor) + perfactor +
                   SCCfactor + fprfactor, 
                 data = df2, id = df2$AnimalIdentifier, cluster  = df2$HerdIdentifier,
                 robust = TRUE)
modsurv2 <- coxph(Surv(tstart, tstop, event = event) ~ strata(parityfactor) + 
                    SCCfactor + fprfactor + LactValue1, data = df2, 
                  id = df2$AnimalIdentifier,cluster = df2$HerdIdentifier, 
                  robust = TRUE)

summary(modsurv)
summary(modsurv2)
summary(modsurv3)
preds <- as.data.frame(cbind("surv" = modsurv$y,
                             "predict1" = predict(modsurv, type = "risk"),
                             "predict2" = predict(modsurv2, type = "risk")))

df3 <- df2[,c(1,9:15)] %>% group_by(AnimalIdentifier) %>%
  left_join(preds, by = c("tstart" = "start", "tstop" = "stop"))
fwrite(df3, "./Original data/surv/predicted.csv", row.names = F, verbose = T)
## survfit models and survival curves --> univariate
modparity <- survfit(Surv(tstart, tstop, event = event) ~ parityfactor, 
                     data= df2, id = df2$AnimalIdentifier)
ggsurvplot(modparity)
modfpr <- survfit(Surv(tstart, tstop, event = event) ~ fprfactor, 
                  data= df2, id = df2$AnimalIdentifier)
ggsurvplot(modfpr, cumevents = T, surv.median.line = "hv")
modSCC <- survfit(Surv(tstart, tstop, event = event) ~ SCCfactor, 
                  data= df2, id = df2$AnimalIdentifier)
ggsurvplot(modSCC)
modLact <- survfit(Surv(tstart, tstop, event = event) ~ LactValue1 + strata(parityfactor), 
                    data= df2, id = df2$AnimalIdentifier)
ggsurvplot(modLact)

modperturb <- survfit(Surv(tstart, tstop, event = event) ~ perfactor, 
                      data= df2, id = df2$AnimalIdentifier)
ggsurvplot(modperturb, linetype = 2, conf.int = F)
df3 <- df2[df2$perfactor != "censored",]
df3 <- droplevels(df3)
modperturb2 <- glht(coxph(Surv(tstart, tstop, event = event) ~ 
                            Insemfactor + parityfactor + 
                            SCCfactor + fprfactor + perfactor, 
                          data = df3, id = df3$AnimalIdentifier,
                          cluster = df3$HerdIdentifier,
                          robust = TRUE), linfct=mcp(perfactor= "Tukey"))
summary(modperturb2)
par(mar=c(5,6,4,2)+1.5,mgp=c(5,1,0))
plot(modperturb2)
## gg hazard plots

ggforest(coxph(Surv(tstart, tstop, event = event) ~ 
                 LactValue1 + strata(parityfactor) + strata(perfactor) +
                 SCCfactor + fprfactor, 
               data = df2, id = df2$AnimalIdentifier, cluster  = df2$HerdIdentifier,
               robust = TRUE))
ggforest(coxph(Surv(tstart, tstop, event = event) ~ 
                 Insemfactor + parityfactor + 
                 SCCfactor + fprfactor + perfactor, 
               data = df2[df2$perfactor!= "censored",],
               cluster = df2$HerdIdentifier[df2$perfactor != "censored"], 
               robust = TRUE))
ggsurvplot(survfit(Surv(tstart, tstop, event = event) ~ 
                     perfactor, data = df2, id = df2$AnimalIdentifier),
           surv.median.line = "hv", cumevents = T)
# ggsurvplot(surv_fit(Surv(tstart, tstop, event = event) ~ 
#                     parityfactor + SCCfactor + fprfactor, 
#                 data = df2,cluster = df2$HerdIdentifier, robust = TRUE))



## cox diagnostics
par(mfrow = c(2,2))
a <-ggcoxzph(cox.zph(modsurv))
a
b <- cox.zph(modsurv)
b


## special: perturbations survival

df3 <- df2 %>% group_by(AnimalIdentifier) %>%
  filter(., max(event) == 1)
df3$perfactor <- droplevels(df3$perfactor)

df3$perfactor <- relevel(df3$perfactor, ref = "before 2012")

ggforest(coxph(Surv(tstart, tstop, event = event) ~ 
                 Insemfactor + parityfactor + 
                 SCCfactor + fprfactor + perfactor, 
               data = df3,cluster = df3$HerdIdentifier, 
               robust = TRUE))

ggsurvplot(survfit(Surv(tstart, tstop, event = event) ~ 
                     perfactor, data = df3, id = df3$AnimalIdentifier),
           surv.median.line = "hv", cumevents = T)


## manual graphs
a <- as.data.frame(cbind(modsurv$coefficients, exp(confint(modsurv))))
colnames(a) <- c("coeff", "lowerCI", "upperCI")
a$expCoeff <- exp(a$coeff)
a$var <- row.names(a)
b <- data.frame(var = c("perfactor2019", "SCCfactorless than 400", "fprfactor<=10%", "LactValue1less than 100"), 
                coeff = 0, lowerCI = 0, upperCI = 0, expCoeff = 1)
a <- rbind(a, b)
plot1 <- ggplot(data = a, mapping = aes(x = expCoeff, y = var)) +
  geom_point(shape = 15) + 
  geom_segment(mapping = aes(x = lowerCI, xend = upperCI, y= var, yend = var), 
               data = a, lineend = "butt")
plot1
fwrite(df2, "survivaldata.csv", row.names = F)
