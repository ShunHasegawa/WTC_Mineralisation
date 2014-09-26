######################
# summary data frame #
######################
ChMean <- ddply(mineMlt, .(time, date, insertion, sampling, temp, chamber, variable), Crt_SmryDF) 
TrtMean <- TrtMean <- ddply(ChMean, .(time, insertion, sampling, date, temp, variable), 
                            function(x) Crt_SmryDF(x, val = "Mean"))

palette(c("blue2", "goldenrod1", "firebrick2", "chartreuse4", "deepskyblue1", "darkorange1", 
          "darkorchid3", "darkgrey", "mediumpurple1", "orangered2", "chocolate", "burlywood3"))

theme_set(theme_bw()) # graphic backgroud is white

#################################
# plot each nutrient separately #
#################################
ChFg <- dlply(ChMean, .(variable), PltMean)
fls <- paste("Output/Figs/WTC_Mineralisation_Chamber_", c("Nitrification", "N_mineralisation", "P_mineralisation"), sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = ChFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltMean)
fls <- paste("Output/Figs/WTC_Mineralisation_Temp_", c("Nitrification", "N_mineralisation", "P_mineralisation"),sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

##################################
# plot all nutrient in one graph #
##################################
# labels for facet_grid
ylabs <- list(
  'nitrification' = "Nitrification rates",
  'n.min' = "N mineralisation rates",
  'p.min' = "P mineralisation rates")


ylab_label <- function(variable, value){
  return(ylabs[value])
}

pl <- PltMean(TrtMean) +
  facet_grid(variable~., scales= "free_y", labeller= ylab_label)
ggsavePP(filename = "Output//Figs/WTC_Mineralisation_Temp", plot = pl, width = 6, height = 6)


