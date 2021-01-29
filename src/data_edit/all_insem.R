insemdata <- fread("tr_inseminations.csv")

length(unique(insemdata$AnimalIdentifier))
length(unique(insemdata$UBNcalving))
length(unique(insemdata$UBN))
summary(insemdata)


pmdata <- fread("event1_pm005geb.csv", drop = 2:14, nThread = 4, verbose = T)

insemdata2 <- insemdata[insemdata$AnimalIdentifier %in% unique(pmdata$AnimalIdentifier),]

length(unique(insemdata2$AnimalIdentifier))
length(unique(insemdata2$UBNcalving))
length(unique(insemdata2$UBN))
summary(insemdata2)

insemdata2$CalvingDate <- as.Date(as.character(insemdata2$CalvingDate), format = "%Y%m%d")

insemdata <- insemdata2 %>% filter(., year(CalvingDate) >= "2009")
length(unique(insemdata$AnimalIdentifier))
length(unique(insemdata$UBNcalving))
length(unique(insemdata$UBN))
summary(insemdata)

insemdata$CalvingDate <- as.character(insemdata$CalvingDate)
insemdata$CalvingDate <- gsub("-", "", insemdata$CalvingDate)

length(unique(insemdata$AnimalIdentifier[insemdata$BirthDate == 19500101]))
fwrite(insemdata, "edit4_inseminations.csv", verbose = T)
