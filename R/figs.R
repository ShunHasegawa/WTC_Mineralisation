# summary data frame
ChMean <- ddply(mineMlt, .(time, date, insertion, sampling, temp, chamber, variable), Crt_SmryDF) 
TrtMean <- TrtMean <- ddply(ChMean, .(Time, insertion, sampling, date, temp, variable), 
                            function(x) Crt_SmryDF(x, val = "Mean"))
