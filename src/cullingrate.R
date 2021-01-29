library(data.table)
library(dplyr)

df2 <- fread("herds_in_surv.csv", header = T)

colnames(df2) <- "HerdIdentifier"

str(df2)

herdnames <- c("UBN",
               "year",
               "ageyearlive",
               "agedayslive",
               "numberlive",
               "numberproductiondayslive",
               "lifeprodn",
               "fat",
               "protein",
               "ageyearculled",
               "agedaysculled",
               "numberculled",
               "numberproductiondaysculled",
               "lifeprodnculled",
               "fatculled",
               "proteinculled",
               "numbercowscalvingint",
               "calvint",
               "numberheifers",
               "ageyearheifers",
               "agedaysheifers")
herddata <- fread("tr_herd_info.csv", drop = 22, sep = ";", header = F, verbose = T, col.names = herdnames)
herddata <- herddata[herddata$UBN %in% df2$HerdIdentifier,]

fwrite(herddata, "survdata_herd_info.csv", row.names = F)
