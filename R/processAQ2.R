library(plyr)
library(lubridate)

source("R/functions.R")


# Correct based on ccv value for NO3_15
files <- dir(path = "Data/AQ2/NeedToBeCorrected/", pattern = "csv$")
l_ply(files, function(x) write.csv(Crrtct.ccv.df(x), file = paste("Data/AQ2/ReadyToProcess//", "Corrected_", x, sep = "")))

# read and combine files
files <- dir(path = "Data//AQ2//ReadyToProcess", pattern = "csv$")

write.csv(cmbn.fls(files), file = "Output//Data/processedAQ2.csv")
