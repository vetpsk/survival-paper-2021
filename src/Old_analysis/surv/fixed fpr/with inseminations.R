library(dplyr)
library(data.table)

pmdata <- fread("./Original data/event1_pm005geb.csv", nThread = 4, verbose = T)
# set.seed(990)
sam_herd <- sample(pmdata$HerdIdentifier , 50, replace = F)

df1 <- pmdata[pmdata$HerdIdentifier %in% sam_herd,]
sam_animal <- unique(df1$AnimalIdentifier)
insemdata <- fread("edit3_inseminations.csv", verbose = T)
insem_df <- insemdata[insemdata$AnimalIdentifier %in% sam_animal,]

insem_df <- insem_df[,c(3,5,6,7, 8,9,10)]
insem_df <- distinct(insem_df)

insem_df1 <- insem_df %>% group_by(AnimalIdentifier, Parity) %>%
  summarize(., Inseminations = max(InsemNumber), last_insem = max(InsemDate), last_Calv = max(CalvingDate), first_Calv = min(CalvingDate))

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

df1 <- df1 %>% group_by(AnimalIdentifier) %>%
  mutate(min_date = min(TestDate))
df1$lactlength <- df1$TestDate-df1$CalvDate

df1$fpr <- ifelse(df1$lactlength <= 100, df1$FatCent/df1$ProtCent, NA)
df1$SCCfactor <- ifelse(df1$SCC >= 400, ifelse(df1$SCC>= 1000, 2, 1), 0)

n <- 0

df1 <- df1 %>% group_by(AnimalIdentifier) %>%
  mutate(., fpr2 = ifelse(fpr > 1.5, n+1, n+0)) %>%
  mutate(., fprn = sum(fpr2, na.rm = T)) %>%
  select(-c(fpr2))
df1$fprfactor <- ifelse(df1$fprn == 1, "max once", ifelse(df1$fprn %in% c(2:5), "2 to 5", ifelse(
  df1$fprn > 5, "more than 5", "never"
)))

df1$fprfactor <- as.factor(df1$fprfactor)

df1$SCCfactor <- as.factor(df1$SCCfactor)
df1$parityfactor <- rep(0, times = nrow(df1))
df1$parityfactor[df1$Parity == 1] <- 1
df1$parityfactor[df1$Parity == 2] <- 2
df1$parityfactor[df1$Parity %in% c(3,4)] <- 3
df1$parityfactor[df1$Parity > 4] <- 4
df1$parityfactor[df1$parityfactor == 0] <- NA
df1$parityfactor <- as.factor(df1$parityfactor)
library(survival)

df2 <- df1 %>% group_by(AnimalIdentifier) %>% 
  mutate(tstart  = as.numeric(TestDate - BirthDate)/7)


df2 <- df2 %>% group_by(AnimalIdentifier) %>%
  mutate(., tstop = ifelse(TestDate == max_date, tstart + mean(diff(tstart)),lead(tstart)))

rm(insem_df, insem_df1, sam_animal)
rm(pmdata, insemdata)
rm(sam_herd)
df2$Insemfactor <- rep(NA, nrow(df2))
df2$Insemfactor[df2$Inseminations == 1] <- 1
df2$Insemfactor[df2$Inseminations %in% c(2,3)] <- 2
df2$Insemfactor[df2$Inseminations>3] <- 3
df2$Insemfactor <- as.factor(df2$Insemfactor)
df2$fprfactor <- relevel(df2$fprfactor, ref = "never")

df2 <- df2 %>% group_by(AnimalIdentifier) %>%
  mutate(., perturbations = ifelse(max(event) == 1, year(max(TestDate)), 9999))

df2$perfactor <- as.factor(ifelse(df2$perturbations <= 2012, "before 2012", ifelse(
  df2$perturbations %in% 2013:2015, "2013 to 2015", ifelse(
  df2$perturbations == 9999, "unculled", "after 2015"))))

df2 <- df2 %>% select(., c(AnimalIdentifier, BirthDate, Parity, HerdIdentifier, TestDate, 
                           event, max_date, min_date, SCCfactor, fprfactor, 
                           parityfactor, tstart, tstop, Insemfactor, perfactor))
library(survminer)
library(ggplot2) 
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
summary(modsurv)
summary(modsurv2)

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
ggforest(coxph(Surv(tstart, tstop, event = event) ~ parityfactor + 
                 SCCfactor + fprfactor, data = df2,
               cluster = df2$HerdIdentifier, robust = TRUE))
ggforest(coxph(Surv(tstart, tstop, event = event) ~ 
                 Insemfactor + parityfactor + 
                 SCCfactor + fprfactor, 
               data = df2,cluster = df2$HerdIdentifier, 
               robust = TRUE))
ggsurvplot(survfit(Surv(tstart, tstop, event = event) ~ 
                     perfactor, data = df2, id = df2$AnimalIdentifier),
           surv.median.line = "hv", cumevents = T)
# ggsurvplot(surv_fit(Surv(tstart, tstop, event = event) ~ parityfactor + SCCfactor + fprfactor, data = df2,cluster = df2$HerdIdentifier, robust = TRUE))
a <-ggcoxzph(cox.zph(modsurv))
a
b <- cox.zph(modsurv)
b



df3 <- df2 %>% group_by(AnimalIdentifier) %>%
  filter(., max(event) == 1)
df3$perfactor <- droplevels(df3$perfactor)
levels(df3$perfactor)

df3$perfactor <- relevel(df3$perfactor, ref = "before 2012")
ggforest(coxph(Surv(tstart, tstop, event = event) ~ 
                 Insemfactor + parityfactor + 
                 SCCfactor + fprfactor + perfactor, 
               data = df3,cluster = df3$HerdIdentifier, 
               robust = TRUE))
ggsurvplot(survfit(Surv(tstart, tstop, event = event) ~ 
                     perfactor, data = df3, id = df3$AnimalIdentifier),
           surv.median.line = "hv", cumevents = T)
