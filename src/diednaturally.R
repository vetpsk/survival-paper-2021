library(data.table)
library(dplyr)

animdata <- fread("tr_anim_info.csv",
                  header = F,
                  sep = ";",
                  drop = 5,
                  verbose = T,
                  col.names = c("AnimalIdentifier", "BirthDate", "EndDate", "EndCode"))

animals_died <- animdata[animdata$EndCode == 4,]

fwrite(animals_died, "animals_died_naturally.csv", row.names = F)


