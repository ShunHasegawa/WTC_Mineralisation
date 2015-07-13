rm(list=ls(all=TRUE))

source("R/packages.R")
source("R/functions.R")

##################
# Mineralisation #
##################
# process data
# mine <- read.csv("Data/WTC_mineralisation.csv", colClasses=c("time" = "factor", "location" = "factor"))
# mine <- mine[complete.cases(mine),]
# mine <- droplevels(mine)
# mine$chamber <- factor(ifelse(mine$chamber < 10, paste("0", mine$chamber, sep = ""), mine$chamber))
# mine$insertion <- as.Date(dmy(mine$insertion))
# mine$sampling <- as.Date(dmy(mine$sampling))
# mine$date <- as.Date(ave(apply(cbind(mine$insertion, mine$sampling), 1, mean), mine$time), 
#                      origin = origin) # same date for same date
# mine$id <- mine$chamber:mine$side
# save(mine, file = "Output//Data/WTC_Mineralisation.RData")
load("Output//Data/WTC_Mineralisation.RData")

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

save.image("Output/Data/AllObj.RData")
