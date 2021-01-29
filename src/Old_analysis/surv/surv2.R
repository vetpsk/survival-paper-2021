library(dplyr)
library(data.table)


## load data

pmdata <- fread("./Original data/event2_endcode_pm005geb.csv", nThread = 4, verbose = T)
# set.seed(990)
sam_herd <- sample(pmdata$HerdIdentifier , 50, replace = F)

df1 <- pmdata[pmdata$HerdIdentifier %in% sam_herd,]
sam_animal <- unique(df1$AnimalIdentifier)
insemdata <- fread("edit3_inseminations.csv", verbose = T)
insem_df <- insemdata[insemdata$AnimalIdentifier %in% sam_animal,]

insem_df <- insem_df[,c(3,5,6,7, 8,9,10)]
insem_df <- distinct(insem_df)

insem_df1 <- insem_df %>% group_by(AnimalIdentifier, Parity) %>%
  summarize(., Inseminations = max(InsemNumber), last_insem = max(InsemDate), 
            last_Calv = max(CalvingDate), first_Calv = min(CalvingDate))




## dates as dates 

df1 <- df1 %>% group_by(AnimalIdentifier, Parity) %>%
  left_join(insem_df1, by = c("AnimalIdentifier", "Parity"))
df1$BirthDate <- as.Date(as.character(df1$BirthDate), format = "%Y%m%d")
df1$TestDate <- as.Date(as.character(df1$TestDate), format = "%Y%m%d")
df1$CalvDate <- as.Date(as.character(df1$CalvDate), format = "%Y%m%d")
df1$last_insem <- as.Date(as.character(df1$last_insem), format = "%Y%m%d")
df1$last_Calv <- as.Date(as.character(df1$last_Calv), format = "%Y%m%d")
df1$first_Calv <- as.Date(as.character(df1$first_Calv), format = "%Y%m%d")
df1 <- df1 %>% group_by(AnimalIdentifier) %>%
  mutate(max_date = max(TestDate))


## fpr factor

df1 <- df1 %>% group_by(AnimalIdentifier) %>%
  mutate(min_date = min(TestDate))
df1$lactlength <- df1$TestDate-df1$CalvDate
df1$fpr <- ifelse(df1$lactlength <= 100, df1$FatCent/df1$ProtCent, NA)

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



## remove other data

rm(insem_df, insem_df1, sam_animal)
rm(pmdata, insemdata)
rm(sam_herd)



## inseminations factor
df2 <- df2 %>% group_by(AnimalIdentifier) %>%
  mutate(Insemfactor = ifelse(Parity == min(Parity) & is.na(Inseminations) == TRUE, "first calving", 
                              ifelse(Parity == max(Parity) & is.na(Inseminations) == TRUE, "last calving", NA)))

df2$Insemfactor[df2$Inseminations == 1] <- 1
df2$Insemfactor[df2$Inseminations %in% c(2,3)] <- 2
df2$Insemfactor[df2$Inseminations>3] <- 3
df2$Insemfactor <- as.factor(df2$Insemfactor)

df2 <- df2 %>% group_by(AnimalIdentifier) %>%
  mutate(., perturbations = ifelse(max(event) == 1, as.character(year(max(TestDate))), "censored"))
df2$perfactor <- as.factor(df2$perturbations)



## drop columns

df2 <- df2 %>% select(., c(AnimalIdentifier, BirthDate, Parity, HerdIdentifier, TestDate, 
                           event, EndCode, max_date, min_date, SCCfactor, fprfactor, 
                           parityfactor, tstart, tstop, Insemfactor, perfactor))
df2$fprfactor <- relevel(df2$fprfactor, ref = "<=10%")


## load libraries

library(survival)
library(survminer)
library(ggplot2) 
library(multcomp)


## cox ph models

modsurv <- coxph(Surv(tstart, tstop, event = event) ~ 
                   Insemfactor + parityfactor + 
                   SCCfactor + fprfactor, 
                 data = df2, id = df2$AnimalIdentifier,
                 cluster = df2$HerdIdentifier, 
                 robust = TRUE)
modsurv2 <- coxph(Surv(tstart, tstop, event = event) ~ parityfactor + 
                    SCCfactor + fprfactor, data = df2, 
                  id = df2$AnimalIdentifier,cluster = df2$HerdIdentifier, 
                  robust = TRUE)
modsurv3 <- modsurv <- coxph(Surv(tstart, tstop, event = event) ~ 
                               Insemfactor + parityfactor + 
                               SCCfactor + fprfactor + perfactor, 
                             data = df2,subset = perfactor != "censored", id = df2$AnimalIdentifier,
                             cluster = df2$HerdIdentifier, 
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
modinsem <- survfit(Surv(tstart, tstop, event = event) ~ Insemfactor, 
                    data= df2, id = df2$AnimalIdentifier)
ggsurvplot(modinsem)

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

ggforest(coxph(Surv(tstart, tstop, event = event) ~ parityfactor + 
                 SCCfactor + fprfactor, data = df2,
               cluster = df2$HerdIdentifier, robust = TRUE))
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
