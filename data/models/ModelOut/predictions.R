setwd("/Volumes/SDXC")

library(data.table)
library(dplyr)

df1 <- fread("predictions.csv")

df2 <- df1 %>% filter(., parity == "1st parity")

df_MQ <- df2 %>% select(-parity) %>% filter(., perfactor == "before 2014")
df_PMQ <- df2 %>% select(-parity) %>% filter(., perfactor == "between 2014 and 2016")
df_PH <- df2 %>% select(-parity) %>% filter(., perfactor == "beyond 2016")
plot(density(df_MQ$pred), main = "", xlab = "log-weeks", ylim = c(0, 5))

hist(df_MQ$pred, xlab = "Predicted log-survival times in weeks", main = "Milk Quota era", prob = T)
lines(density(df_MQ$pred), lwd = 2,lty = 3, col = "blue")
legend("topright", "Density", lty = 2, col = "blue", cex = 0.8, xpd = F, horiz = T)

hist(df_PMQ$pred, xlab = "Predicted log-survival times in weeks", main = "Post-milk Quota era", prob = T)
lines(density(df_PMQ$pred), lwd = 2,lty = 3, col = "blue")

legend("topright", "Density", lty = 2, col = "blue", cex = 0.8, xpd = F, horiz = T)
hist(df_PH$pred, xlab = "Predicted log-survival times in weeks", main = "PHosphate regulation", prob = T)
lines(density(df_PH$pred), lwd = 2,lty = 2, col = "red")

legend("topright", c("MQ", "PMQ", "PH"), lty = c(1,3,2),col = c("black", "blue", "red"), cex = 0.8, xpd = F, horiz = T)

fwrite(df_MQ, "pred_mq.csv", row.names = F)
fwrite(df_PMQ, "pred_PMQ.csv", row.names = F)

fwrite(df_PH, "pred_PH.csv", row.names = F)
