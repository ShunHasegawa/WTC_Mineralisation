###################
# Drop and subset #
###################
subsetD <- function(...) droplevels(subset(...))

###################################
# Coorect AQ2 result based on CCV #
###################################

# subset dataset beween each ccv
Crrct.ccv <- function(x, data, ccval = 7.5){
  # subset dataset between given ccvs
  ccv.ps <- grep("V$", as.character(data$Sample.ID))
  
  ts <- data[c(ccv.ps[x] : ccv.ps[x + 1]),]
  trng <- range(ts$times)
  xv <- as.numeric(trng)
  yv <- ts$Result[ts$times == trng[1] | ts$times == trng[2]] 
  
  b <- ccval * (1 / yv[1] - 1 / yv[2]) / (xv[1] - xv[2])
  a <- ccval / yv[1] - b * xv[1]
  
  ts$Result <- ts$Result * (a + b * as.numeric(ts$times))
  ts$times <- NULL
  return(ts)
}

# applied the function above for each subsets of ccvs
Crrtct.ccv.df <- function(filename, ccval = 7.5){
  data <- read.csv(paste("Data/AQ2/NeedToBeCorrected/", filename, sep = ""), header = TRUE)
  
  # make time factor as numeric
  a <- sapply(as.character(data$Time), strsplit, " ")
  b <- ldply(a, function(x) paste(x[c(5, 2, 3, 4)], collapse = "/"))
  
  b$V1 <- ymd_hms(b$V1)
  
  names(b) <- c("Time", "times")
  
  # merge
  mrg.res <- merge(data, b, by = "Time")
  
  # reorder accoding to time
  mrg.res <- mrg.res[order(mrg.res$times), ]
  
  # add the latest ccv value at the end of data frame to make sure all measured values are placed between ccvs
  # if the last line is not ccv, vlues after last ccv will be turned into NA
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID))
  lstTime <- mrg.res$times[nrow(mrg.res)]
  mrg.res[nrow(mrg.res) + 1, ] <- mrg.res[max(ccv.ps), ] 
  mrg.res$times[nrow(mrg.res)] <- lstTime + 1 # make the last time latest by adding 1 to the actual latest time
  
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID)) # update ccv.position
  
  # re-caluculate the results
  a <- ldply(1:(length(ccv.ps)-1), function(x) Crrct.ccv(x, data = mrg.res, ccval))
  
  # if there are negative values, add minimum value to remove any negative values
  if(any(a$Result < 0)) a$Result <- a$Result - min(a$Result, na.rm = TRUE)
  
  return(a)
}

######################################################
# process and combine aq 2 data, then create a table #
######################################################
prcsAQ2 <- function(data){
  # remove ccv, ccb, standard
  res <- data[-grep("^C|^STANDARD", as.character(data$Sample.ID)),]
  
  # remove dup, top, middle
  res <- res[-grep("dup$|top|middle", as.character(res$Sample.Details)),]
  
  # sample labels
  a <- strsplit(as.character(res$Sample.Details), "[.]")
  
  # turn this into data frame
  a.df <- ldply(a)
  names(a.df)[c(1, 4:6)] <- c("Date", "Incubation", "Chamber", "Side")
  a.df$Date <- ymd(a.df$Date)
  res.df <- cbind(a.df, res)
  res.df <- res.df[c("Date", "Incubation","Chamber", "Side", "Result")]
  res.df <- res.df[order(res.df$Date, res.df$Incubation, as.numeric(res.df$Chamber), as.numeric(res.df$Side)),]
  return(res.df)
}

cmbn.fls <- function(file){
  # read files
  rd.fls <- lapply(file, function(x) read.csv(paste("Data/AQ2/ReadyToProcess/", x, sep = ""), header = TRUE))
  
  # process and make data frame for each test type
  pr.df <- ldply(rd.fls, function(x) ddply(x, .(Test.Name), prcsAQ2))
  
  # reshape
  names(pr.df)[grep("Result", names(pr.df))] <- "value"
  pr.cst$Chamber <- as.numeric(pr.cst$Chamber)
  pr.cst$Side  <- as.numeric(pr.cst$Side)
  pr.cst <- cast(pr.df, Date + Incubation + Chamber + Side ~ Test.Name)
  pr.cst <- pr.cst[order(pr.cst$Incubation, pr.cst$Date, as.numeric(pr.cst$Chamber), as.numeric(pr.cst$Side)),]
  return(pr.cst)
}

##########################
# Create a summary table #
##########################
CreateTable <- function(dataset, fac){
  a <- dataset[c("date", fac, "value")] #extract required columns
  colnames(a) <- c("date","variable","value") #change column names for cast
  means <- cast(a, date~variable, mean, na.rm = TRUE) 
  ses <- cast(a,date~variable,function(x) ci(x,na.rm=TRUE)[4])
  colnames(ses)[2:ncol(ses)] <- paste(colnames(ses)[2:ncol(ses)],"SE",sep=".")
  samples <- cast(a,date~variable,function(x) sum(!is.na(x))) #sample size
  colnames(samples)[2:ncol(samples)] <- paste(colnames(samples)[2:ncol(samples)],"N",sep=".")
  mer <- Reduce(function(...) merge(..., by = "date"), list(means, ses, samples)) #merge datasets
  mer <- mer[,c(1, order(names(mer)[-grep("date|N", names(mer))])+1, grep("N", names(mer)))] #re-order columns
  if(fac == "temp") mer$tempR <- with(mer, elev/amb - 1)
  mer$date <- as.character(mer$date) # date is turned into character for knitr output 
  return(mer)
}

#function which creates excel worksheets
crSheet <- function(sheetname, dataset){
  #create sheet
  sheet <- createSheet(wb, sheetName = sheetname)
  
  #add data to the sheet
  addDataFrame(dataset, sheet, showNA = TRUE, row.names = FALSE, startRow = 2)
  
  #title of the sheet
  addDataFrame(t(c(sheetname, "unit=mg DrySoil(kg)^(-1) day^(-1)")), sheet, startRow = 1, row.names = FALSE, col.names = FALSE)
}

############################
# make a summary dataframe #
############################
Crt_SmryDF <- function(data, val = "value"){
  x <- data[ ,val]
  Mean <- mean(x, na.rm = TRUE)
  SE <- ci(x, na.rm = TRUE)[[4]]
  N  <- sum(!is.na(x))
  data.frame(Mean, SE, N)
}

####################
# plot mean and se #
####################
PltMean <- function(data){
  unt <- substitute((mg~kg^-1~day^-1))
  
  ylabs <- c(expression(), 
             bquote(atop("Nitrification rates", paste(.(unt)))),
             bquote(atop("N mineralisation rates", paste(.(unt)))),
             bquote(atop("P mineralisation rates", paste(.(unt)))))
  
    ylab <- ifelse(length(unique(data$variable)) > 1, c(expression(), bquote(.(unt))),
                 ifelse(unique(data$variable) == "nitrification", ylabs[1], 
                        ifelse(unique(data$variable) == "n.min", ylabs[2],
                               ylabs[3])))
  
  colfactor <- ifelse(any(names(data) == "chamber"), "chamber", "temp")
  
  p <- ggplot(data, aes_string(x = "date", y = "Mean", col = colfactor))
  
  p2 <- p + geom_line(size = 1, position = position_dodge(10)) + 
    geom_errorbar(aes_string(ymin = "Mean - SE", ymax = "Mean + SE", col = colfactor), 
                  width = 15, 
                  position = position_dodge(10)) + 
    scale_x_date(breaks= date_breaks("1 month"), 
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2013-2-1", "2014-2-15"))) +
    geom_hline(yintercept = 0, col = "black", alpha = .8)+
    theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1)) +
    labs(x = "Month", y = ylab)
  
  # change colors, linetype and associated legend according plotting groups (chamber or treatment)
  if(colfactor == "temp") p2 +  scale_color_manual(values = c("blue", "red"), "Temp trt", labels = c("Ambient", "eTemp")) else
    p2 + scale_color_manual(values = palette(), "Chamber", labels = paste("Chamber", c(1:12), sep = "_")) +
    scale_linetype_manual(values = rep(c("solid", "dashed"), 6), "Chamber", labels = paste("Chamber", c(1:12), sep = "_")) +
    guides(color = guide_legend(keyheight = 0.7))
}

#######################
#model simplification #
#######################
MdlSmpl <- function(model){
  mod2 <- update(model, method = "ML") #change method from REML to ML
  stai <- stepAIC(mod2, trace = FALSE) #model simplification by AIC
  dr <- drop1(stai, test="Chisq") #test if removing a factor even more significantly lowers model
  model <- update(stai, method="REML")
  ifelse(all(dr[[4]] < 0.05, na.rm=TRUE), anr <- anova(model), anr<-NA) 
  #dr[[4]]<0.05-->unable to remove any more factors so finlize the results by changsing the method back to REML
  return(list(step.aic = stai$anova, drop1 = dr, anova.reml = anr, model.reml = model, model.ml = stai))
}

#############################################
# compare different auto-correlation models #
#############################################

atcr.cmpr <- function(model, rndmFac){
  if(rndmFac == "chamber/side"){
    model2 <- update(model,corr=corCompSymm(form=~1|chamber/side)) 
  } else {
    if(rndmFac == "chamber"){
      model2 <- update(model,corr=corCompSymm(form=~1|chamber))
    } else {
      model2 <- update(model,corr=corCompSymm(form=~1|id))
    }
  }
  
  model3 <- update(model,correlation=corARMA(q=2))
  model4 <- update(model,correlation=corAR1()) 
  model5 <- update(model,correlation=corARMA(q=1))
  a <- anova(model,model2,model3,model4,model5)
  rownames(a) <- c("NULL", "corCompSymm", "corARMA(q=2)", "corAR1()", "corARMA(q=1)")
  models <- list(model, model2, model3, model4, model5, 'models' = a)
  return(models)
}

###########################################
# produce box plots with transformed data #
###########################################
# log OR sqrt OR power(1/3) OR inverse OR box-cox
bxplts <- function(value, ofst = 0, data, ...){
  data$y <- data[[value]] + ofst #ofst is added to make y >0
  a <- boxcox(y ~ temp * time, data = data, plotit = FALSE)
  par(mfrow = c(2, 3))
  boxplot(y ~ temp*time, data, main = "raw")
  boxplot(log(y) ~ temp*time, main = "log", data)
  boxplot(sqrt(y) ~ temp*time, main = "sqrt", data)
  boxplot(y^(1/3) ~ temp*time, main = "power(1/3)", data)
  boxplot(1/y ~ temp*time, main = "inverse", data)
  BCmax <- a$x[a$y == max(a$y)]
  texcol <- ifelse(BCmax < 0, "red", "black") 
  boxplot(y^(BCmax) ~ temp*time, 
          main = "", sep = "=", 
          data = data)
  title(main = paste("Box Cox", round(BCmax, 4)), 
        col.main = texcol)
  par(mfrow = c(1,1))
}

# multiple box-cox power plot for different constant values
bxcxplts <- function(value, data, sval, fval){
  par.def <- par()
  data$yval <- data[[value]]
  ranges <- seq(sval, fval, (fval - sval)/9)
  
  # store parameters given from box-cox plot
  par(mfrow = c(5, 2))
  BCmax <- vector()
  for (i in 1:10){
    data$y <- data$yval + ranges[i]
    a <- boxcox(y ~ temp * time, data = data, plotit = FALSE)
    BCmax[i] <- a$x[a$y == max(a$y)]
  }
  
  # plot box plot with poer given from box-box for 
  # each contstant value
  par(mfrow = c(5, 2))
  par(omi = c(0, 0, 0, 0), mai = c(0.4, 0.4, 0.4, 0))
  sapply(1:10, function(x) {
    boxplot((yval + ranges[x]) ^ BCmax[x] ~ temp * time, 
            main = "", data = data)
    texcol <- ifelse(BCmax[x] < 0, "red", "black") 
    title(main = paste("constant=", round(ranges[x], 4), 
                       ", boxcox=", round(BCmax[x], 4)),
          col.main = texcol)
  })
  par(par.def) # set the graphic conditions back
}

##############################
# Save ggplot in PDF and PNG #
##############################
ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}

###########################
# step deletion with lmer #
###########################
stepLmer <- function(model, red.rndm = FALSE, ddf = "Kenward-Roger", ...){
  require(lmerTest)
  update(step(model, reduce.random = red.rndm, ddf = ddf,...)$model, 
         contrasts = NULL)
}
# use "Kenward-Roger" for approximation for denominator degrees of freedom. This
# is the same as the default DF given by Anova(model, test.statistic = "F). The
# default of step gives me a warning message for IEM-NO3 for some reasons (not
# sure why.. so changed it.)

#####################
# Squared R for LMM #
#####################
source("R/rsquaredglmm.R")

################################
# Return star based on P value #
################################
FormatPval <- function(Pval) {
  stars <- ifelse(Pval > .05, "",
                  ifelse(Pval > .01, "'*'",
                         ifelse(Pval > .001, "'**'", "'***'")))
  
  p <- as.character(ifelse(Pval > .1, round(Pval, 3),
                           ifelse(Pval < .001, "bold('<0.001')", 
                                  # shown with bold font. Note that inside of
                                  # bold needs to be in ''
                                  paste("bold(", round(Pval, 3), ")", sep = "'"))))
  return(data.frame(stars, p))
} 

########################################
# Create summary stat table from anova #
########################################
StatTable <- function(x, variable) { # x is anova result
  df <- data.frame(predictor = c(row.names(x)),
                   rbind(FormatPval(x$Pr)))
  
  # add a row for column name of the table in the fig 
  df <- rbind(df, data.frame(predictor = "", 
                             stars = "italic('P')", 
                             p = "italic('P')"))
  
  result <- merge(df, data.frame(predictor = c("temp", "time", "temp:time")), all = TRUE)
  
  # replace NA with ns
  result <- within(result, {
    p <- ifelse(is.na(p), "ns", as.character(p)) 
    # ifelse tries to return factor, so use as.character
    stars <- ifelse(is.na(stars), "ns", as.character(stars))
  })
  
  # relabel for plotting
  result$predictor <- factor(result$predictor, 
                             labels = c("", "Temp", "Time", "Temp~x~Time"), 
                             levels = c("", "temp", "time", "temp:time"))
  result$variable <- variable
  result <- result[order(result$predictor), ]
  return(result)
}

############################################
# Create df to add a stat table to figures #
############################################
StatPositionDF <- function(StatRes, variable, ytop, ylength, gap = .1){
  d <- data.frame(variable, ytop, gap = gap * ylength) 
  # ytop is y coordinate for the top (i.e. temp) of the table for each fig 
  # (variable), ylength is the difference of max and min value of the plot (i.e.
  # max(mean+SE) - min(mean-SE)). 0.1 * ylength is used to determine the gap between each row
  # of the table
  
  predictor <- levels(StatRes$predictor)
  
  # create df which contains variable, predictor and y coordinates for the other
  # predictors (i.e. Time, tempxTime) which is ylength*0.1 (= gap) lower than one above
  d2 <- ddply(d, .(variable),
              function(x){
                data.frame(predictor, 
                           ldply(1:length(predictor), function(z) x$ytop - z * x$gap))
              })
  names(d2)[3] <- "yval"
  
  # mege every thing
  d3 <- merge(d2, StatRes, by = c("variable", "predictor"))
  d3$temp <- "amb" # temp column is required for ggplot
  return(d3)
}

#######################
# Fig for publication #
#######################
# define graphic background
science_theme <- theme(panel.border = element_rect(color = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       axis.ticks.length = unit(-.2, "lines"),
                       axis.ticks.margin = unit(.5, "lines"),
                       legend.background = element_rect(fill = "transparent", colour = NA),
                       legend.position = c(.78, .94), 
                       legend.title = element_blank(),
                       legend.key.width = unit(2.5, "lines"),
                       legend.key = element_blank())

# white-black figure
WBFig <- function(data, ylab, facetLab = ylab_label, figTheme = science_theme, StatRes, StatY){
  # StatRes is stats tables to put on the figs; StatY is y coordinate for that tables
  
  # Blank data frame: defining the constant y range for N related mineralisation
  # N range
  Nrng <- with(subsetD(data, variable != "Net P mineralisation rate"), 
               c(min(Mean - SE, na.rm = TRUE), max(Mean + SE + 0.03, na.rm = TRUE)))
  
  blankDF <- data.frame(date = data$date[1], 
                        variable = unique(data$variable),
                        Mean = c(rep(Nrng[1], 2), 0,
                                 rep(Nrng[2], 2), 0),
                        temp = "amb")
  
  # P range
  Prng <- with(subsetD(data, variable == "Net P mineralisation rate"), 
               c(min(Mean - SE, na.rm = TRUE), max(Mean + SE, na.rm = TRUE)))
  
  # df for sub labels
  subLabDF <- with(data, 
                   data.frame(xv = as.Date("2013-2-2"),
                              variable = levels(variable),
                              yv = c(rep(Nrng[2], 2), Prng[2]),
                              labels = paste("(", letters[1:length(levels(variable))], ")",
                                             sep = ""),
                              temp = "amb"))
  
  # df for stat table
  
  ## create df
  statDF <- StatPositionDF(StatRes = StatRes, 
                           variable = levels(data$variable), 
                           ytop = StatY,
                           ylength = c(rep(diff(Nrng), 2), diff(Prng)))
  
  # create a plot
  p <- ggplot(data, aes(x = date, y = Mean, group = temp))
  
  p2 <- p + 
    geom_vline(xintercept = as.numeric(as.Date("2013-3-18")), 
               linetype = "dashed", col = "black") +
    geom_line(aes(linetype = temp), position = position_dodge(10)) + 
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                  width = 0, 
                  position = position_dodge(10)) + 
    geom_point(aes(fill = temp), 
               shape = 21, size = 4,   
               position = position_dodge(10)) +
    labs(x = "Month", y = ylab) +
    scale_x_date(breaks= date_breaks("3 month"),
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2013-2-1", "2014-2-15"))) +
    scale_fill_manual(values = c("black", "white"), 
                      labels = c("Ambient", "eTemp")) +
    scale_linetype_manual(values = c("solid", "dashed"), 
                          labels = c("Ambient", "eTemp")) +
    geom_text(aes(x = xv, y = yv, label = labels),
              fontface = "bold",
              hjust = 1,
              data = subLabDF) +
    facet_grid(variable~., scales= "free_y") +
    figTheme +
    geom_text(data = subset(statDF, predictor != ""), 
              aes(x = as.Date("2013-5-20"), y = yval, label = predictor),
              size = 3, hjust = 1, parse = TRUE) +
    # unless remove [" "] with predictor != "", labels will be messed up due to
    # this empty level
    geom_text(data = statDF, 
              aes(x = as.Date("2013-5-20") + 20, y = yval, label = p), 
              size = 3, parse = TRUE) +
    geom_blank(aes(x = date, y = Mean), data = blankDF)
  return(p2)
}

########################
# Summary ANCOVA table #
########################
AncvSmmryTbl <- function(AncvRes, predictor){
  ResAncv <- ldply(AncvRes, function(x) {
    x$predictor <- row.names(x)
    x <- merge(x[, -2], data.frame('predictor' = predictor), all = TRUE)
    names(x)[4] <- "Pr"
    x <- within(x, {
      Pr <- ifelse(Pr < 0.001, "<0.001", round(Pr, 3))
      Pr <- ifelse(is.na(Pr), "ns", Pr)
      F <- round(F, 2)
      Df.res <- round(Df.res, 0)
    })
    return(x)}, 
    .id = "response")
  
  ResAncv_mlt <- melt(ResAncv, id = c("response", "predictor"))
  ResAncv_cst <- dcast(response + variable ~ predictor, data = ResAncv_mlt)
  ResAncv_tbl <- ResAncv_cst[, c("response", "variable", predictor)]
  ResAncv_tbl[is.na(ResAncv_tbl)] <- "-"
  return(ResAncv_tbl)
}

######################
# Transformed visreg #
######################
# For some reasons, visreg doesn't work with trans argument
TransVirsreg <- function(VisObj, trans, ...){
  dd <- VisObj
  dd$fit$visregFit <- trans(dd$fit$visregFit)
  dd$res$visregRes <- trans(dd$res$visregRes)
  plot(dd, ...)
}
