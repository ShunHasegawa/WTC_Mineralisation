mineMlt <- melt(mine, id = c("time", "date", "insertion", "sampling", "chamber", "location", "side", "temp", "id"), na.rm= TRUE)

# chamber summary table & mean
ChSmmryTbl <- dlply(mineMlt, .(variable), function(x) CreateTable(x, fac = "chamber"))
ChMean <- ddply(mineMlt, .(time, insertion, sampling, date, temp, chamber, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(ChMean, .(variable), function(x) CreateTable(x, fac = "temp"))

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rawdata
sheet <- createSheet(wb,sheetName="raw_data")
addDataFrame(mine, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for chamber summary
shnames <- paste("Chamber_mean.",c("Nitrification", "N_mineralisation","P_mineralisation", sep=""))
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = ChSmmryTbl[[x]]))

# worksheets for temp trt summary
shnames <- paste("Temp_mean.", c("Nitrification", "N_mineralisation","P_mineralisation"), sep = "")
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"Output/Table/WTC_Mineralisation.xlsx")
