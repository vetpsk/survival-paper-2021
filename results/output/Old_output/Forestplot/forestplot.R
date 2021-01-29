library(dplyr)
library(data.table)
library(survival)
library(forestplot)
setwd("/Volumes/SDXC")
load("./ModelOut/model6.Rdata")
summary(modsurv6)


# resultfile <- fread("./Forestplot/data3.csv", header = T)
resultfile <- read_excel("Forestplot/data3.xlsx")
resultfile[resultfile == 0] <- NA

logic <- is.na(resultfile$coeff)
logic[1] <- TRUE
logic[22] <- TRUE
logic[c(3,6,11,15,19)] <- FALSE
intercept <- resultfile$coeff[1]
resultfile$survival <- resultfile$coeff + intercept
resultfile$survival[1] <- intercept
resultfile$inweeks <- exp(resultfile$survival)
resultfile$inweeks2 <- rbind("253 weeks and 2 days", NA, NA,
                             "286 weeks and 2 days", NA, NA,
                             "240 weeks and 4 days", "233 weeks and 4 days", "219 weeks and 1 day", NA, NA,
                             "267 weeks and 2 days", "268 weeks and 5 days", NA, NA,
                             "252 weeks 6 days", "245 weeks and 1 day", NA, NA,
                             "252 weeks and 6 days", "241 weeks and 5 days", "68 weeks and 3 days")

# coeff <- ifelse(is.na(resultfile$coeff) != TRUE, paste(round(resultfile$coeff, 4), "±", round(1.96*resultfile$se, 4)), NA)
# coeff[c(3, 6,11,15,20,24,28,32,37,43,49, 57)] <- "ref"
resultfile$inweeks2[c(3,6,11,15,19)] <- "ref level"
label = cbind(resultfile$variables, resultfile$inweeks2)
resultfile[1,2:8] <- NA
resultfile[22,2:8] <- NA
forestplot(labeltext = label, hrzl_lines = gpar(col="#444444"),
           mean = resultfile$coeff,
           lower = resultfile$lower, upper = resultfile$upper,
           col=fpColors(lines = "black"), boxsize = 0.1, ci.vertices = T, 
           zero = 0, ci.vertices.height = 0.1, align = "l", grid = c(-0.2, -0.1 ,0, 0.1, 0.2),
           is.summary = logic, line.margin = 0.3, xticks = seq(-0.2, 0.2, 0.1), lwd.ci = 1.5,
           lty.ci = 1, txt_gp = fpTxtGp(cex = 0.75, xlab = gpar(cex = 0.75), ticks = gpar(cex = 0.75)),
           xlab = "log(t)")

# SEE
# BELOW
#   ||
#   ||
#   \/















resultfile2 <- resultfile[c(1:4, 10:47,56:64),]
logic <- is.na(resultfile$coeff)
logic[1] <- TRUE
logic[64] <- TRUE
logic[c(3, 6,10,15,19,23,28,32,38,44)] <- FALSE
coeff <- ifelse(is.na(resultfile2$coeff) != TRUE, paste(round(resultfile2$coeff, 4), "±", round(1.96*resultfile2$se, 4)), NA)
coeff[c(3, 6,10,15,19,23,28,32,38,44)] <- "ref"
label = cbind(resultfile2$variables, coeff )
resultfile2[1,2:5] <- NA
resultfile2[51,2:5] <- NA
forestplot(labeltext = label, hrzl_lines = gpar(col="#444444"),
           mean = resultfile$coeff,
           lower = resultfile$lower, upper = resultfile$upper,
           col=fpColors(lines = "black"), boxsize = 0, ci.vertices = T, grid = T, 
           zero = 0, ci.vertices.height = 0.2, graph.pos = 3,
           is.summary = logic, line.margin = 0.3, xticks = c(-0.2, 0, 0.2), lwd.ci = 1.5,
           lty.ci = 1, txt_gp = fpTxtGp(cex = 0.65, xlab = gpar(cex = 0.75), ticks = gpar(cex = 0.75)),
           xlab = "log(t)")

