rm(list=ls(all=TRUE))

library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(packrat)
library(plyr)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)

source("R/functions.R")

##################
# Mineralisation #
##################
# process data
mine <- read.csv("Data/WTC_mineralisation.csv", colClasses=c("time" = "factor", "location" = "factor"))
mine <- mine[complete.cases(mine),]
mine <- droplevels(mine)
mine$chamber <- factor(ifelse(mine$chamber < 10, paste("0", mine$chamber, sep = ""), mine$chamber))
mine$insertion <- as.Date(dmy(mine$insertion))
mine$sampling <- as.Date(dmy(mine$sampling))
mine$date <- as.Date(ave(apply(cbind(mine$insertion, mine$sampling), 1, mean), mine$time), 
                     origin = origin) # same date for same date
save(mine, file = "Output//Data/WTC_Mineralisation.RData")

#################
# Summary table #
#################
source("R/SummaryTableExcel.R")

########
# Figs #
########
source("R/figs.R")

#########
# Stats #
#########
source("R/stats.R")
