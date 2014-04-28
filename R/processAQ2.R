library(plyr)
library(lubridate)

files <- dir(path = "Data/AQ2/NeedToBeCorrected/", pattern = "csv$")

Jan14 <- Crrtct.ccv.df(files[1])

