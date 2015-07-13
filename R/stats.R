#############################
# Merge with soil variables #
#############################

# soil variables
load("Data/WTC_soilMoistTemp_Chamber_DailySummary.RData")
# restructure
names(soilChmSmry)[c(1, 2, 4)] <- c("date", "chamber", "probe")
SoilChMlt <- melt(soilChmSmry, id = c("date", "chamber", "temp", "probe"))
SoilCh <- cast(SoilChMlt, date + chamber + temp ~ probe + variable)

# chamber mean for mineralisation
par(mfrow = c(1, 3))
apply(mine[, c("nitrification", "n.min", "p.min")], 2, plot)

Mine_ChMean <- ddply(mine, .(time, insertion, sampling, date, chamber, temp), 
                     function(x) {
                       d1 <- colMeans(x[, c("nitrification", "n.min", "p.min")], na.rm = TRUE)
                       return(d1)})

# mean of soil vars during incubation period
SoilIncSampMean <- function(insertion, sampling, Chm, data = SoilCh){
  a <- subset(data, date >= insertion & date <= sampling & chamber == Chm)
  vars <- names(a)[which(!names(a) %in% c("date", "chamber", "temp"))]
  b <- ddply(a, .(chamber), function(x) colMeans(x[, vars], na.rm = TRUE))
  return(cbind(insertion, sampling, b))
}

Mine_DF <- ddply(Mine_ChMean, .(time, date, insertion, sampling, chamber, temp, nitrification, n.min, p.min),
                function(x) SoilIncSampMean(insertion= x$insertion, sampling= x$sampling, Chm = x$chamber))
Mine_DF$moist <- Mine_DF$SoilVW_5_25_Mean

p <- ggplot(SoilCh, aes(x = date, y = SoilVW_5_25_Mean))
p2 <- p + 
  geom_line() +
  geom_point(data = Mine_DF, aes(x = date, y = SoilVW_5_25_Mean), 
             col = "red", size = 2)+
  facet_wrap( ~ chamber)+
  geom_vline(xintercept = as.numeric(unique(Mine_DF$insertion)), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(unique(Mine_DF$sampling)), linetype = "dashed")
p2
# good


#################
# Nitrification #
#################
source("R/stats_Nitrification.R")

####################
# N mineralisation #
####################
source("R/stats_Nmineralisation.R")

###################
# P minerlisation #
###################
source("R/stats_Pmineralisation.R")

###########
# Summary #
###########

################
## Temp x Time #
################

# create stat summary table for LMM with Temp and time
TempTimeStatList <- list(nitrification = AnvF_nit, 
                         n.min = AnvF_nmin, 
                         p.min = AnvF_pmin) 

Stat_TempTime <- ldply(names(TempTimeStatList), 
                       function(x) StatTable(TempTimeStatList[[x]], variable = x))
save(Stat_TempTime, file = "output//data/TempTime_Stat.RData")
