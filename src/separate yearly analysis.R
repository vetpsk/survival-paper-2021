
modsurv2009 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                      LactValue1 + Insemfactor +
                      SCCfactor + fprfactor + cluster(HerdIdentifier), 
                    data = df2[df2$perfactor == "2009",], dist = "weibull",
                    robust = TRUE, model = T)
summary(modsurv2009)
modsurv2010 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                         LactValue1 + Insemfactor +
                         SCCfactor + fprfactor + cluster(HerdIdentifier), 
                       data = df2[df2$perfactor == "2010",], dist = "weibull",
                       robust = TRUE, model = T)
summary(modsurv2010)
modsurv2011 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                         LactValue1 + Insemfactor +
                         SCCfactor + fprfactor + cluster(HerdIdentifier), 
                       data = df2[df2$perfactor == "2011",], dist = "weibull",
                       robust = TRUE, model = T)
summary(modsurv2011)
modsurv2012 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                         LactValue1 + Insemfactor +
                         SCCfactor + fprfactor + cluster(HerdIdentifier), 
                       data = df2[df2$perfactor == "2012",], dist = "weibull",
                       robust = TRUE, model = T)
summary(modsurv2012)

modsurv2013 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                         LactValue1 + Insemfactor +
                         SCCfactor + fprfactor + cluster(HerdIdentifier), 
                       data = df2[df2$perfactor == "2013",], dist = "weibull",
                       robust = TRUE, model = T)
summary(modsurv2013)
modsurv2014 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                         LactValue1 + Insemfactor +
                         SCCfactor + fprfactor + cluster(HerdIdentifier), 
                       data = df2[df2$perfactor == "2014",], dist = "weibull",
                       robust = TRUE, model = T)
summary(modsurv2014)
modsurv2015 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                         LactValue1 + Insemfactor +
                         SCCfactor + fprfactor + cluster(HerdIdentifier), 
                       data = df2[df2$perfactor == "2015",], dist = "weibull",
                       robust = TRUE, model = T)
summary(modsurv2015)
modsurv2016 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                         LactValue1 + Insemfactor +
                         SCCfactor + fprfactor + cluster(HerdIdentifier), 
                       data = df2[df2$perfactor == "2016",], dist = "weibull",
                       robust = TRUE, model = T)
summary(modsurv2016)
modsurv2017 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                         LactValue1 + Insemfactor +
                         SCCfactor + fprfactor + cluster(HerdIdentifier), 
                       data = df2[df2$perfactor == "2017",], dist = "weibull",
                       robust = TRUE, model = T)
summary(modsurv2017)
modsurv2018 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                         LactValue1 + Insemfactor +
                         SCCfactor + fprfactor + cluster(HerdIdentifier), 
                       data = df2[df2$perfactor == "2018",], dist = "weibull",
                       robust = TRUE, model = T)
summary(modsurv2018)
modsurv2019 <- survreg(Surv(tstart, tstop, event = event, type = "interval") ~ 
                         LactValue1 + Insemfactor +
                         SCCfactor + fprfactor + cluster(HerdIdentifier), 
                       data = df2[df2$perfactor == "2019",], dist = "weibull",
                       robust = TRUE, model = T)
summary(modsurv2019)
a <- data.frame(NULL)
listmodels <- mget(ls(pattern = "modsurv*"))

a <- lapply(listmodels, coef)
ab <- as.data.frame(a)

fwrite(ab, "coefficients by years of perturbation.csv")
