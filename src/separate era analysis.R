setwd("/Volumes/SDXC/ModelOut")

library(data.table)
library(dplyr)

load("model71.Rdata")
load("model72.Rdata")
load("model73.Rdata")

library(survival)
summary(modsurv71)
summary(modsurv72)
summary(modsurv73)

rm(list = ls())
