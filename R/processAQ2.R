library(plyr)
library(lubridate)

files <- dir(path = "Data/AQ2/NeedToBeCorrected/", pattern = "csv$")

l_ply(files, function(x) write.csv(Crrtct.ccv.df(x), file = paste("Data/AQ2/ReadyToProcess//", "Corrected_", x, sep = "")))

