# remove outlier
mine_RmOlr <- mine
mine_RmOlr$nitrification[mine_RmOlr$nitrification == min(mine_RmOlr$nitrification)] <- NA
mine_RmOlr$n.min[mine_RmOlr$n.min == min(mine_RmOlr$n.min)] <- NA
mineMlt <- melt(mine_RmOlr, id = c("time", "date", "insertion", "sampling", "chamber", "location", "side", "temp", "id"), na.rm= TRUE)

# chamber summary table & mean
ChSmmryTbl <- dlply(mineMlt, .(variable), function(x) CreateTable(x, fac = "chamber"))
ChMean <- ddply(mineMlt, .(time, insertion, sampling, date, temp, chamber, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(ChMean, .(variable), function(x) CreateTable(x, fac = "temp"))

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rowdata and rowdata without outlier
sheet <- createSheet(wb,sheetName="row_data")
addDataFrame(mine, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

sheet <- createSheet(wb,sheetName="row_data_withoutOutlier")
addDataFrame(mine_RmOlr, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for chamber summary
shnames <- paste("Chamber_mean.",c("Nitrification", "N_mineralisation","P_mineralisation", sep=""))
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = ChSmmryTbl[[x]]))

# worksheets for temp trt summary
shnames <- paste("Temp_mean.", c("Nitrification", "N_mineralisation","P_mineralisation"), sep = "")
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"Output/Table/WTC_Mineralisation.xlsx")
