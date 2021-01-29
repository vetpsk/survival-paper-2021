library(dplyr)
library(data.table)


## load data

pmdata <- fread("./Original data/event2_endcode_pm005geb.csv", nThread = 4, verbose = T)
# set.seed(990)
sam_herd <- sample(pmdata$HerdIdentifier , 50, replace = F)

df1 <- pmdata[pmdata$HerdIdentifier %in% sam_herd,]


## dates as dates

df1$BirthDate <- as.Date(as.character(df1$BirthDate), format = "%Y%m%d")
df1$TestDate <- as.Date(as.character(df1$TestDate), format = "%Y%m%d")
df1$CalvDate <- as.Date(as.character(df1$CalvDate), format = "%Y%m%d")
df_backup <- df1
df1 <- df1 %>% group_by(AnimalIdentifier) %>%
  mutate(max_date = max(TestDate))


## Calv factor

calv <- df1[,c(1,4)]
calv <- calv[!duplicated(calv),]
calv <- calv %>% group_by(AnimalIdentifier) %>%
  mutate(calv_int = CalvDate - lag(CalvDate))
calv$calv_int <- as.numeric(calv$calv_int)
calv$calvfactor <- rep(0, nrow(calv))
calv$calvfactor <- as.character(calv$calvfactor)
calv$calvfactor[calv$calv_int <= 400] <- "less than 400 days"
calv$calvfactor[calv$calv_int > 400] <- "more than 400 days"
calv$calvfactor[is.na(calv$calv_int) == TRUE] <- "first lactation"
df1 <- df1 %>% group_by(AnimalIdentifier) %>%
    left_join(calv[,c(1,2,4)], by = c("AnimalIdentifier", "CalvDate"))
rm(calv)
df1$calvfactor <- as.factor(df1$calvfactor)


## fpr factor

df1 <- df1 %>% group_by(AnimalIdentifier) %>%
  mutate(min_date = min(TestDate))
df1$lactlength <- df1$TestDate-df1$CalvDate
df1$fpr <- ifelse(df1$lactlength <= 100, df1$FatCent/df1$ProtCent, NA)

n <- 0
df1 <- df1 %>% group_by(AnimalIdentifier) %>%
  mutate(., fpr2 = ifelse(fpr > 1.5, n+1, n+0), fprtot = n_distinct(fpr)) %>%
  mutate(., fprn = sum(fpr2/fprtot, na.rm = T)) 
df1 <- df1 %>% dplyr::select(-c(fpr2,fprtot))

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



## remove other data

rm(pmdata, df_backup)
rm(sam_herd)

## perturbations factor

df2 <- df2 %>% group_by(AnimalIdentifier) %>%
  mutate(., perturbations = ifelse(max(event) == 1, as.character(year(max(TestDate))), "censored"))
df2$perfactor <- as.factor(df2$perturbations)




## drop columns

df2 <- df2 %>% dplyr::select(., c(AnimalIdentifier, BirthDate, Parity, HerdIdentifier, TestDate, 
                           event, EndCode, max_date, min_date, SCCfactor, fprfactor, 
                           parityfactor, tstart, tstop, calvfactor, perfactor))
df2$fprfactor <- relevel(df2$fprfactor, ref = "<=10%")



## load libraries

library(survival)
library(survminer)
library(ggplot2) 
library(multcomp)


## cox ph models

modsurv <- coxph(Surv(tstart, tstop, event = event) ~ 
                   calvfactor + parityfactor + 
                   SCCfactor + fprfactor, 
                 data = df2, id = df2$AnimalIdentifier,
                 cluster = df2$HerdIdentifier, 
                 robust = TRUE)

modsurv3 <- modsurv <- coxph(Surv(tstart, tstop, event = event) ~ 
                               calvfactor + parityfactor + 
                               SCCfactor + fprfactor + perfactor, 
                             data = df2,subset = perfactor != "censored", id = df2$AnimalIdentifier,
                             cluster = df2$HerdIdentifier, 
                             robust = TRUE)

summary(modsurv)
summary(modsurv3)


## survfit models and survival curves --> univariate

modcalv <- survfit(Surv(tstart, tstop, event = event) ~ calvfactor, 
                    data= df2,subset = calvfactor != "first lactation", id = df2$AnimalIdentifier)
ggsurvplot(modcalv, cumevents = T, test.for.trend = T)


## gg hazard plots
df2$calvfactor <- relevel(df2$calvfactor, ref = "less than 400 days")

ggforest(coxph(Surv(tstart, tstop, event = event) ~ calvfactor +parityfactor + 
                 SCCfactor + fprfactor, data = df2,
               cluster = df2$HerdIdentifier, robust = TRUE))


## special: perturbations survival



ggforest(coxph(Surv(tstart, tstop, event = event) ~ 
                 calvfactor + parityfactor + 
                 SCCfactor + fprfactor + perfactor, 
               data = df3,cluster = df3$HerdIdentifier, 
               robust = TRUE))

ggsurvplot(survfit(Surv(tstart, tstop, event = event) ~ 
                     perfactor, data = df3, id = df3$AnimalIdentifier),
           surv.median.line = "hv", cumevents = T)

#post-hoc

modcalv <- glht(coxph(Surv(tstart, tstop, event = event) ~ 
             calvfactor + parityfactor + 
             SCCfactor + fprfactor, 
           data = df2, id = df2$AnimalIdentifier,
           cluster = df2$HerdIdentifier,
           robust = TRUE), linfct=mcp(calvfactor= "Tukey"))
summary(modcalv)
par(mar=c(5,5,5,5))
plot(modcalv)
