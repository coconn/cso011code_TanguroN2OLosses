# GC-Rcode-fileloop.R
# processing GC data - converting raw GC data into mass per volume
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# this version of the GC-Rcode file loops through all the GC files to process them

# output products:
# vialDFfull.csv
# ambinfoDFfull.csv
# timezeroDFfull.csv
# warningsfull.csv

# outputs feed into FluxCalcs-Rcode.R


########################################################################
# WHAT FILES ARE WE LOOPING THROUGH

# set files to process and path to folder with standardized naming info
standardizedfn <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/standardizedfilenames.txt", stringsAsFactors=FALSE)
filestoprocess <- standardizedfn$standardizedfilenames
# where to get inputs
path = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-standardizedfilenames/"
# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rprocessed/"

# can use as test
#filestoprocess = c("20131104_AA","20131105_BB") # what is a good automated way to get this?  see below for attempts
# filestoprocess = "20140310_U" # used for testing

library(xlsx)
library(ggplot2)
library(gridExtra)
library(XLConnect)
library(data.table)

########################################################################
# BEGIN LOOP

## pre-start output df
vialDFfull <- data.frame()
ambinfoDFfull <- data.frame()
timezeroDFfull <- data.frame()


for (i in 1:length(filestoprocess)) {
  
  
  ########################################################################
  # BRING IN DATA, MAKE DATAFRAME
  
  ## pick a particular GC run
  ## bring in data
  
  # define files for this loop
  #   myfilerunsheet <- paste(path, "runsheets-standardizedfilenames/runsheet_", filestoprocess[i], ".txt", sep = "")
  myfilerunsheet <- paste(path, "runsheets-standardizedfilenames/runsheet_", filestoprocess[i], ".xlsx", sep = "")
  myfileecd <- paste(path, "ecd_", filestoprocess[i], ".txt", sep = "")
  myfilefid <- paste(path, "fid_", filestoprocess[i], ".txt", sep = "")
  myfiletcd <- paste(path, "tcd_", filestoprocess[i], ".txt", sep = "")
  
  # bring in GC data
  #   runsheet <- read.delim(myfilerunsheet)
  runsheet <- read.xlsx(myfilerunsheet,1)
  ecdN2O <- read.delim(myfileecd, header = FALSE)
  fidCH4 <- read.delim(myfilefid, header = FALSE)
  tcdCO2 <- read.delim(myfiletcd, header = FALSE)
  
  ## start vial info and conversions dataframe
  
  N2Oraw <- ecdN2O$V6
  CO2raw <- tcdCO2$V6
  CH4raw <- fidCH4$V6
  SampleName <- runsheet$SampleName
  Site <- runsheet$Site
  LUtype <- runsheet$LUtype
  Chamber <- runsheet$Chamber
  TimePt <- runsheet$TimePt
  TimePtSq <- runsheet$TimePtSq
  SampleDate <- runsheet$SampleDate
  Pressure <- runsheet$Pressure
  
  vialDF <- data.frame(N2Oraw, CO2raw, CH4raw, SampleName, Site, LUtype, Chamber, TimePt, TimePtSq, SampleDate, Pressure)
  
  vialDF
  
  
  
  
  ########################################################################
  # AMBIENT CORRECTION
  
  # ambients info for this run
  ambientsN <- subset(vialDF, vialDF$SampleName=='amb', select=c(N2Oraw))
  ambientsN
  ambientsNmean <- mean(ambientsN$N2Oraw)
  ambientsNmean
  ambientsNsd <- sd(ambientsN$N2Oraw)
  ambientsNsd  
  ambientsNcount <- dim(ambientsN)[1]
  ambientsNcount
  
  ambientsC <- subset(vialDF, vialDF$SampleName=='amb', select=c(CO2raw))
  ambientsC
  ambientsCmean <- mean(ambientsC$CO2raw)
  ambientsCmean
  ambientsCsd <- sd(ambientsC$CO2raw)
  ambientsCsd  
  ambientsCcount <- dim(ambientsC)[1]
  ambientsCcount
  
  ambientsCH <- subset(vialDF, vialDF$SampleName=='amb', select=c(CH4raw))
  ambientsCH
  ambientsCHmean <- mean(ambientsCH$CH4raw)
  ambientsCHmean
  ambientsCHsd <- sd(ambientsCH$CH4raw)
  ambientsCHsd  
  ambientsCHcount <- dim(ambientsCH)[1]
  ambientsCHcount
  
  # vial volume info
  airtmp <- 25 # degC
  volvial <- 9 # cc
  volsample <- 12 # cc
  voltotal <- volvial + volsample # cc
  volsampletmp <- volsample * (298.15/(273.15+airtmp))
  voltotaltmp <- volvial + volsampletmp
  
  # get volume correction columns, save to dataframe
  # venterea excel it's not temp controlled for standards, but yes temp controlled for others
  N2Oc <- ((voltotal * vialDF$N2Oraw[1:5]) - (volvial * ambientsNmean)) / volsample
  CO2c <- ((voltotal * vialDF$CO2raw[1:5]) - (volvial * ambientsCmean)) / volsample
  CH4c <- ((voltotal * vialDF$CH4raw[1:5]) - (volvial * ambientsCHmean)) / volsample
  last <- length(vialDF$N2Oraw)
  # tmp correction option (for vials that aren't standards)
  N2Oc[6:last] <- ((voltotaltmp * vialDF$N2Oraw[6:last]) - (volvial * ambientsNmean)) / volsampletmp
  CO2c[6:last] <- ((voltotaltmp * vialDF$CO2raw[6:last]) - (volvial * ambientsCmean)) / volsampletmp
  CH4c[6:last] <- ((voltotaltmp * vialDF$CH4raw[6:last]) - (volvial * ambientsCHmean)) / volsampletmp
  vialDF$N2Oc <- N2Oc
  vialDF$CO2c <- CO2c
  vialDF$CH4c <- CH4c
  
  # Q: how come you don't use the temp corrected ambient corrections for the standards?
  # because that's incorporated in the temperature correction standards (see ngN_cm3_correction variable)
  
  
  
  ########################################################################
  # STANDARDS CURVEs AND PPM CORRECTION
  
  # ppm values in the standards
  ppmNstds <- c(0.301,1.57,3) # Mix1, Mix2, 3N2O
  ppmCstds <- c(600,1000,3000) # Mix1, Mix2, 3KCO2
  ppmCHstds <- c(1,1.7,0) # Mix1, Mix2, (0,0) - see email "two quick questions: CH4 in standards? and using 10N2O"
    
  # regression dataframes
  
  areaN2Ostds <- subset(vialDF, vialDF$SampleName=='Mix1' | vialDF$SampleName=='Mix2' | vialDF$SampleName=='3N2O', select=c(N2Oc))
  stdtabN2O = data.frame(area=areaN2Ostds,ppm=ppmNstds)
  stdtabN2O
  
  # redefine using 10N2O standard if it's for the leak test
  # Mix1, Mix2, 3N2O, 10N2O 
  toMatch <- c("Leak") # c("Leak", "Jank") if you wanted to include 10N2O from other GC runs
  if (length(grep(paste(toMatch,collapse="|"), filestoprocess[i]))>0) {
        areaN2Ostds <- subset(vialDF, vialDF$SampleName=='Mix1' | vialDF$SampleName=='Mix2' | vialDF$SampleName=='3N2O' | vialDF$SampleName=='10N2O', select=c(N2Oc))
        ppmNstds <- c(0.301,1.57,3,10) 
        stdtabN2O = data.frame(area=areaN2Ostds,ppm=ppmNstds)
        stdtabN2O
  } 
  
  areaCO2stds <- subset(vialDF, vialDF$SampleName=='Mix1' | vialDF$SampleName=='Mix2' | vialDF$SampleName=='3KCO2', select=c(CO2c))
  stdtabCO2 = data.frame(area=areaCO2stds,ppm=ppmCstds)
  stdtabCO2
  # get rid of mix2 (don't trust it for CO2), use (0,0) instead
  stdtabCO2[2,1:2]<-0
  
  areaCH4stds <- subset(vialDF, vialDF$SampleName=='Mix1' | vialDF$SampleName=='Mix2' | vialDF$SampleName=='3KCO2', select=c(CH4c)) # third entry is a dummy right now
  stdtabCH4 = data.frame(area=areaCH4stds,ppm=ppmCHstds)
  stdtabCH4
  # make (0,0) one of the first two points
  stdtabCH4[3,1:2]<-0
  stdtabCH4 <- rbind(stdtabCH4[3,],stdtabCH4[1,],stdtabCH4[2,])
  
  
  # build standards curves
  
  # high level N2O calibration
  lmN2Ohigh <- lm(stdtabN2O$ppm ~ stdtabN2O$N2Oc)
  lmN2Ohigh_intercept <- coef(summary(lmN2Ohigh))["(Intercept)","Estimate"]
  lmN2Ohigh_slope <- coef(summary(lmN2Ohigh))["stdtabN2O$N2Oc","Estimate"]
  # square of correlation is pearson's R^2
  # http://en.wikipedia.org/wiki/Coefficient_of_determination
  lmN2Ohigh_cor <- cor(stdtabN2O$N2Oc, stdtabN2O$ppm)
  lmN2Ohigh_pearsonsR2 <- lmN2Ohigh_cor^2
  
  # low level N2O calibration
  lmN2Olow <- lm(stdtabN2O$ppm[1:2] ~ stdtabN2O$N2Oc[1:2])
  lmN2Olow_intercept <- coef(summary(lmN2Olow))["(Intercept)","Estimate"]
  lmN2Olow_slope <- coef(summary(lmN2Olow))["stdtabN2O$N2Oc[1:2]","Estimate"]
  # square of correlation is pearson's R^2
  # http://en.wikipedia.org/wiki/Coefficient_of_determination
  lmN2Olow_cor <- cor(stdtabN2O$N2Oc[1:2], stdtabN2O$ppm[1:2])
  lmN2Olow_pearsonsR2 <- lmN2Olow_cor^2
  
  # high level CO2 calibration
  lmCO2high <- lm(stdtabCO2$ppm ~ stdtabCO2$CO2c)
  lmCO2high_intercept <- coef(summary(lmCO2high))["(Intercept)","Estimate"]
  lmCO2high_slope <- coef(summary(lmCO2high))["stdtabCO2$CO2c","Estimate"]
  # square of correlation is pearson's R^2
  # http://en.wikipedia.org/wiki/Coefficient_of_determination
  lmCO2high_cor <- cor(stdtabCO2$CO2c, stdtabCO2$ppm)
  lmCO2high_pearsonsR2 <- lmCO2high_cor^2
  
  # low level CO2 calibration
  lmCO2low <- lm(stdtabCO2$ppm[1:2] ~ stdtabCO2$CO2c[1:2])
  lmCO2low_intercept <- coef(summary(lmCO2low))["(Intercept)","Estimate"]
  lmCO2low_slope <- coef(summary(lmCO2low))["stdtabCO2$CO2c[1:2]","Estimate"]
  # square of correlation is pearson's R^2
  # http://en.wikipedia.org/wiki/Coefficient_of_determination
  lmCO2low_cor <- cor(stdtabCO2$CO2c[1:2], stdtabCO2$ppm[1:2])
  lmCO2low_pearsonsR2 <- lmCO2low_cor^2
  
  # high level CH4 calibration
  lmCH4high <- lm(stdtabCH4$ppm ~ stdtabCH4$CH4c)
  lmCH4high_intercept <- coef(summary(lmCH4high))["(Intercept)","Estimate"]
  lmCH4high_slope <- coef(summary(lmCH4high))["stdtabCH4$CH4c","Estimate"]
  # square of correlation is pearson's R^2
  # http://en.wikipedia.org/wiki/Coefficient_of_determination
  lmCH4high_cor <- cor(stdtabCH4$CH4c, stdtabCH4$ppm)
  lmCH4high_pearsonsR2 <- lmCH4high_cor^2
  
  # low level CH4 calibration
  lmCH4low <- lm(stdtabCH4$ppm[1:2] ~ stdtabCH4$CH4c[1:2])
  lmCH4low_intercept <- coef(summary(lmCH4low))["(Intercept)","Estimate"]
  lmCH4low_slope <- coef(summary(lmCH4low))["stdtabCH4$CH4c[1:2]","Estimate"]
  # square of correlation is pearson's R^2
  # http://en.wikipedia.org/wiki/Coefficient_of_determination
  lmCH4low_cor <- cor(stdtabCH4$CH4c[1:2], stdtabCH4$ppm[1:2])
  lmCH4low_pearsonsR2 <- lmCH4low_cor^2
  
  # regression outcome data frames for saving later
  lmN2Otab1 <- data.table(lmN2Ohigh_intercept, lmN2Ohigh_slope, lmN2Ohigh_cor, lmN2Ohigh_pearsonsR2)
  lmN2Otab2 <- data.table(lmN2Olow_intercept, lmN2Olow_slope, lmN2Olow_cor, lmN2Olow_pearsonsR2)
  lmCO2tab1 <- data.table(lmCO2high_intercept, lmCO2high_slope, lmCO2high_cor, lmCO2high_pearsonsR2)
  lmCO2tab2 <- data.table(lmCO2low_intercept, lmCO2low_slope, lmCO2low_cor, lmCO2low_pearsonsR2)
  lmCH4tab1 <- data.table(lmCH4high_intercept, lmCH4high_slope, lmCH4high_cor, lmCH4high_pearsonsR2)
  lmCH4tab2 <- data.table(lmCH4low_intercept, lmCH4low_slope, lmCH4low_cor, lmCH4low_pearsonsR2)
  
  
  # get ppm correction columns, save to dataframe
  
  # ifelse test (determine whether to use high or low standards curve)
  N2Oppm <- ifelse(N2Oc>stdtabN2O$N2Oc[2], N2Oc*lmN2Ohigh_slope+lmN2Ohigh_intercept, N2Oc*lmN2Olow_slope+lmN2Olow_intercept)
  CO2ppm <- ifelse(CO2c>stdtabCO2$CO2c[1], CO2c*lmCO2high_slope+lmCO2high_intercept, CO2c*lmCO2low_slope+lmCO2low_intercept)
  CH4ppm <- ifelse(CH4c>stdtabCH4$CH4c[1], CH4c*lmCH4high_slope+lmCH4high_intercept, CH4c*lmCH4low_slope+lmCH4low_intercept)
  
  vialDF$N2Oppm <- N2Oppm
  vialDF$CO2ppm <- CO2ppm
  vialDF$CH4ppm <- CH4ppm
  
  
  ###### Q: how come we don't use 10N2O in the standards curves?
  ###### A: see email "two quick questions: CH4 in standards? and using 10N2O" - "Bottom line, do NOT include the 10 PPM std unless you actually had a significant number of samples showing up with Area Counts greater than the 3 PPM standard. This is very rare, and means you had very high fluxes. The N2O calibration becomes non-linear above 3PPM so this complicates things further. So you might want to let me know what you find out. -Rod"
  
  
  
  
  ########################################################################
  # SAVE STANDARD CURVE IMAGES
  
  # lm line info
  source("~/Documents/GITHUB/RPersonalFunctionsChristine/lm_eqn.r")
  
  # N2O standards graph
  p1 <- ggplot(stdtabN2O, aes(x=N2Oc, y=ppm)) + geom_point() 
  p1 <- p1 + geom_smooth(method=lm, data=stdtabN2O, se=FALSE, color="blue") + annotate(geom="text", x = -Inf, y = Inf, vjust=1, hjust=-0.05, label = lm_eqn(lmN2Ohigh), parse = TRUE, color="blue")
  p1 <- p1 + geom_smooth(method=lm, data=stdtabN2O[1:2,], se=FALSE, fullrange=TRUE, color="dark green", linetype="dotted", lwd=1) + annotate(geom="text", x = -Inf, y = Inf, vjust=2, hjust=-0.05, label = lm_eqn(lmN2Olow), parse = TRUE, color="dark green") 
  
  # CO2 standards graph
  p2 <- ggplot(stdtabCO2, aes(x=CO2c, y=ppm)) + geom_point() 
  p2 <- p2 + geom_smooth(method=lm, data=stdtabCO2, se=FALSE, color="blue") + annotate(geom="text", x = -Inf, y = Inf, vjust=1, hjust=-0.05, label = lm_eqn(lmCO2high), parse = TRUE, color="blue")
  p2 <- p2 + geom_smooth(method=lm, data=stdtabCO2[1:2,], se=FALSE, fullrange=TRUE, color="dark green", linetype="dotted", lwd=1) + annotate(geom="text", x = -Inf, y = Inf, vjust=2, hjust=-0.05, label = lm_eqn(lmCO2low), parse = TRUE, color="dark green") 
  
  # CH4 standards graph
  p3 <- ggplot(stdtabCH4, aes(x=CH4c, y=ppm)) + geom_point() 
  p3 <- p3 + geom_smooth(method=lm, data=stdtabCH4, se=FALSE, color="blue") + annotate(geom="text", x = -Inf, y = Inf, vjust=1, hjust=-0.05, label = lm_eqn(lmCH4high), parse = TRUE, color="blue")
  p3 <- p3 + geom_smooth(method=lm, data=stdtabCH4[1:2,], se=FALSE, fullrange=TRUE, color="dark green", linetype="dotted", lwd=1) + annotate(geom="text", x = -Inf, y = Inf, vjust=2, hjust=-0.05, label = lm_eqn(lmCH4low), parse = TRUE, color="dark green") 
  
  
  # where to save outputs
  pathsavefigs = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rprocessed/GC-Standard-Curves/"
  # reminder: GC run named filestoprocess[i]
  
  pdf(paste(pathsavefigs,"StandardCurves_", filestoprocess[i], ".pdf", sep=""), width=5, height=8)
  grid.arrange(p1, p2, p3, ncol=1, main=textGrob(paste("Standard Curves, GC Run:",filestoprocess[i]),just="top"))
  dev.off()
  
  png(paste(pathsavefigs,"StandardCurves_", filestoprocess[i], ".png", sep=""), width=450, height=900)
  grid.arrange(p1, p2, p3, ncol=1, main=textGrob(paste("Standard Curves, GC Run:",filestoprocess[i]),just="top"))
  dev.off()
  
  
  
  ########################################################################
  # CONVERT TO MASS PER VOLUME FROM PPM
  
  # tmp-volume-land use dataframe
  ngN_cm3_correction <- c(341.2262)
  ngC_cm3_correction <- c(0.1462398)
  ngCH_cm3_correction <- c(0.1462398) ##### What is the right number here?????  FIX THIS
  
  LU <- c("F","M","S")
  degC <- c(30,35,35)
  voltmpcorrN2O <- ngN_cm3_correction/(degC+273.15)
  voltmpcorrCO2 <- ngC_cm3_correction/(degC+273.15)
  voltmpcorrCH4 <- ngCH_cm3_correction/(degC+273.15)
  
  voltmptab = data.frame(LU,degC,voltmpcorrN2O,voltmpcorrCO2,voltmpcorrCH4)
  
  # solve for ngN_cm3 and ngC_cm3 and save to df
  ngN_cm3_N2O <- N2Oppm*voltmptab[1,3]
  ##### here is where I should have screened for land use - instead, I just used the 20 degC option for all land uses.
  ngC_cm3_CO2 <- CO2ppm*voltmptab[1,4]
  ngC_cm3_CH4 <- CH4ppm*voltmptab[1,5]
  
  vialDF$ngN_cm3_N2O <- ngN_cm3_N2O
  vialDF$ngC_cm3_CO2 <- ngC_cm3_CO2
  vialDF$ngC_cm3_CH4 <- ngC_cm3_CH4
  
  
  ###### Q: adjust tmp by land use (see ngN_cm3_N2O)
  
  ###### Q: what is the right number for line 256????? (ngCH_cm3_correction)
  
  
  
  
  ########################################################################
  # SUMMARY REPORT RE: THINGS TO WATCH
  
  # number of no pressure vials
  NoPressureCount <- length(which(Pressure=="N"))
  
  
  # summary for ambient vials
  
  ambNinfo <- ambientsN[1:4,1]
  ambNinfo[5] <- ambientsNmean
  ambNinfo[6] <- ambientsNsd
  ambNinfo[7] <- ambientsNcount
  ambNinfo[8] <- ambientsNsd/ambientsNmean
  ambCinfo <- ambientsC[1:4,1]
  ambCinfo[5] <- ambientsCmean
  ambCinfo[6] <- ambientsCsd
  ambCinfo[7] <- ambientsCcount
  ambCinfo[8] <- ambientsCsd/ambientsCmean
  ambCHinfo <- ambientsCH[1:4,1]
  ambCHinfo[5] <- ambientsCHmean
  ambCHinfo[6] <- ambientsCHsd
  ambCHinfo[7] <- ambientsCHcount
  ambCHinfo[8] <- ambientsCHsd/ambientsCHmean
  
  Labels <- c("Amb1","Amb2","Amb3","Amb4","Mean", "Std", "Count", "CV")
  
  ambinfoDF <- data.frame(Labels,ambNinfo,ambCinfo,ambCHinfo)
  ambinfoDF$GCrun <- runsheet$GCRun[1:8]
  
  
  
  # check on time 0 values inside chambers
  
  timezeroN <- subset(vialDF, vialDF$TimePt==0, select=c(N2Oppm))
  timezeroC <- subset(vialDF, vialDF$TimePt==0, select=c(CO2ppm))
  timezeroCH <- subset(vialDF, vialDF$TimePt==0, select=c(CH4ppm))
  timezeroLabels <- subset(vialDF, vialDF$TimePt==0, select=c(SampleName))
  
  timezeroNmean <- mean(timezeroN$N2Oppm)
  timezeroNsd <- sd(timezeroN$N2Oppm)
  timezeroNCV <- timezeroNsd/timezeroNmean
  timezeroNsdallow <- 2
  timezeroNupperlimit <- timezeroNmean+timezeroNsdallow*timezeroNsd
  timezeroNlowerlimit <- timezeroNmean-timezeroNsdallow*timezeroNsd
  
  timezeroCmean <- mean(timezeroC$CO2ppm)
  timezeroCsd <- sd(timezeroC$CO2ppm)
  timezeroCCV <- timezeroCsd/timezeroCmean
  timezeroCsdallow <- 2
  timezeroCupperlimit <- timezeroCmean+timezeroCsdallow*timezeroCsd
  timezeroClowerlimit <- timezeroCmean-timezeroCsdallow*timezeroCsd
  
  timezeroCHmean <- mean(timezeroCH$CH4ppm)
  timezeroCHsd <- sd(timezeroCH$CH4ppm)
  timezeroCHCV <- timezeroCHsd/timezeroCHmean
  timezeroCHsdallow <- 2
  timezeroCHupperlimit <- timezeroCHmean+timezeroCHsdallow*timezeroCHsd
  timezeroCHlowerlimit <- timezeroCHmean-timezeroCHsdallow*timezeroCHsd
  
  # add on summary info at the end
  tmplength <- dim(timezeroN)[1]
  
  timezeroNinfo <- timezeroN[1:tmplength,1]
  timezeroCinfo <- timezeroC[1:tmplength,1]
  timezeroCHinfo <- timezeroCH[1:tmplength,1]
  
  timezeroNinfo[(tmplength+1):(tmplength+6)] <- c(timezeroNmean,timezeroNsd,timezeroNCV,timezeroNsdallow,timezeroNupperlimit,timezeroNlowerlimit)
  timezeroCinfo[(tmplength+1):(tmplength+6)] <- c(timezeroCmean,timezeroCsd,timezeroCCV,timezeroCsdallow,timezeroCupperlimit,timezeroClowerlimit)
  timezeroCHinfo[(tmplength+1):(tmplength+6)] <- c(timezeroCHmean,timezeroCHsd,timezeroCHCV,timezeroCHsdallow,timezeroCHupperlimit,timezeroCHlowerlimit)
  
  timezeroLabs <- as.data.frame(as.matrix(timezeroLabels),stringsAsFactors=F)
  timezeroLabs <- timezeroLabs$SampleName
  
  timezeroLabs[(tmplength+1):(tmplength+6)]<-c("time zero mean", "time zero sd", "time zero CV", "time zero sd allow" ,"time zero upper limit", "time zero lower limit")
  
  timezeroDF <- data.frame(timezeroLabs,timezeroNinfo,timezeroCinfo,timezeroCHinfo)
  timezeroDF$GCrun <- runsheet$GCRun[1:(tmplength+6)]
  
  
  
  
  ########################################################################
  # PRINT WARNINGS
  
  # save to txt file
  mywarnings <- paste(pathsavefiles, "warnings_", filestoprocess[i], ".csv", sep = "")
  
  # are there many no pressure vials?
  
  cat(paste("VIAL PRESSURE INFO: there was/were ", NoPressureCount, " vial(s) with no pressure.  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n")
  
  # are the standards curves poor quality?
  
  cat(paste("STD CURVES INFO: the Pearson's R^2 for N2O HIGH was ", round(lmN2Ohigh_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n")
  cat(paste("STD CURVES INFO: the Pearson's R^2 for N2O LOW was ", round(lmN2Olow_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n",append=TRUE)
  cat(paste("STD CURVES INFO: the Pearson's R^2 for CO2 HIGH was ", round(lmCO2high_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n",append=TRUE)
  cat(paste("STD CURVES INFO: the Pearson's R^2 for CO2 LOW was ", round(lmCO2low_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n",append=TRUE)
  cat(paste("STD CURVES INFO: the Pearson's R^2 for CH4 HIGH was ", round(lmCH4high_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n",append=TRUE)
  cat(paste("STD CURVES INFO: the Pearson's R^2 for CH4 LOW was ", round(lmCH4low_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n",append=TRUE)
  
  # is there high variability in the ambient vials?
  
  indAMB = which(ambinfoDF$Labels=="CV")
  cat(paste("AMBIENT VIAL INFO: the CV for N2O was ", round(ambinfoDF$ambNinfo[indAMB], digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n",append=TRUE)
  cat(paste("AMBIENT VIAL INFO: the CV for CO2 was ", round(ambinfoDF$ambCinfo[indAMB], digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n",append=TRUE)
  cat(paste("AMBIENT VIAL INFO: the CV for CH4 was ", round(ambinfoDF$ambCHinfo[indAMB], digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n",append=TRUE)
  
  # is there high variability in the time zero vials?
  
  indTZ = which(timezeroDF$timezeroLabs=="time zero CV")
  
  cat(paste("TIME ZERO INFO: the CV for N2O was ", round(timezeroDF$timezeroNinfo[indTZ], digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n",append=TRUE)
  cat(paste("TIME ZERO INFO: the CV for CO2 was ", round(timezeroDF$timezeroCinfo[indTZ], digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n",append=TRUE)
  cat(paste("TIME ZERO INFO: the CV for CH4 was ", round(timezeroDF$timezeroCHinfo[indTZ], digits=4), ".  File = ", filestoprocess[i], sep = ""), file=mywarnings,sep="\n",append=TRUE)
  #cat(HERE, file=mywarnings,sep="\n",append=TRUE)
  
  # print these statements for the viewer to see
  
  print(paste("VIAL PRESSURE INFO: there was/were ", NoPressureCount, " vial(s) with no pressure.  File = ", filestoprocess[i], sep = ""))
  print(paste("STD CURVES INFO: the Pearson's R^2 for N2O HIGH was ", round(lmN2Ohigh_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("STD CURVES INFO: the Pearson's R^2 for N2O LOW was ", round(lmN2Olow_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""))  
  print(paste("STD CURVES INFO: the Pearson's R^2 for CO2 HIGH was ", round(lmCO2high_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("STD CURVES INFO: the Pearson's R^2 for CO2 LOW was ", round(lmCO2low_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("STD CURVES INFO: the Pearson's R^2 for CH4 HIGH was ", round(lmCH4high_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("STD CURVES INFO: the Pearson's R^2 for CH4 LOW was ", round(lmCH4low_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("AMBIENT VIAL INFO: the CV for N2O was ", round(ambinfoDF$ambNinfo[indAMB], digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("AMBIENT VIAL INFO: the CV for CO2 was ", round(ambinfoDF$ambCinfo[indAMB], digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("AMBIENT VIAL INFO: the CV for CH4 was ", round(ambinfoDF$ambCHinfo[indAMB], digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("TIME ZERO INFO: the CV for N2O was ", round(timezeroDF$timezeroNinfo[indTZ], digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("TIME ZERO INFO: the CV for CO2 was ", round(timezeroDF$timezeroCinfo[indTZ], digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("TIME ZERO INFO: the CV for CH4 was ", round(timezeroDF$timezeroCHinfo[indTZ], digits=4), ".  File = ", filestoprocess[i], sep = ""))
  
  
  # readline(prompt = "ready to move on?  press return.  ")
  
  
  
  ########################################################################
  # SAVE VENTEREA-STYLE EXCEL GC RUN SUMMARY SHEET
  
  # prep data into list
  NoPressureCounttab <- data.table(NoPressureCount)
  GCRun <- data.table(GCRun=filestoprocess[i])
  
  summarysheetdata = list(GCRun=GCRun, NoPressureCount=NoPressureCounttab,
           ambinfoDF=ambinfoDF, timezeroDF=timezeroDF,
           standardvialsN2O=stdtabN2O, standardvialsCO2=stdtabCO2, standardvialsCH4=stdtabCH4,
           StandardCurveInfo_lmN2Ohigh=lmN2Otab1, StandardCurveInfo_lmN2Olow=lmN2Otab2, 
           StandardCurveInfo_lmCO2high=lmCO2tab1, StandardCurveInfo_lmCO2low=lmCO2tab2, 
           StandardCurveInfo_lmCH4high=lmCH4tab1, StandardCurveInfo_lmCH4low=lmCH4tab2) 
  
  # where to save excel sheet
  pathsavesummary = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rprocessed/GC-Runs-Summary-Info/"
  # reminder: GC run named filestoprocess[i]
  
  # if excel file already exists, delete it
  if (file.exists(paste(pathsavesummary,"GCRunSummary_", filestoprocess[i], ".xlsx", sep=""))) {
        file.remove(paste(pathsavesummary,"GCRunSummary_", filestoprocess[i], ".xlsx", sep="")) 
  } 
  
  # XLConnect stuff
  wb = loadWorkbook(paste(pathsavesummary,"GCRunSummary_", filestoprocess[i], ".xlsx", sep=""), create = TRUE)
  # Create a new sheet
  createSheet(wb, name = "GCRunSummaryInfo")
  # cumulative length (rows) of matrices
  # +2 = 1 for list names, 1 for header row
  cumlen = cumsum(c(1, head(sapply(summarysheetdata, nrow), n = -1) + 3))
  # Write data rows (implicitly vectorized!)
  writeWorksheet(wb, data = summarysheetdata, sheet = "GCRunSummaryInfo", startRow = cumlen + 1, header = TRUE)
  # Write list names
  writeWorksheet(wb, data = as.list(names(summarysheetdata)), sheet = "GCRunSummaryInfo", startRow = cumlen, header = FALSE)
  # insert standard curve image into worksheet - create a named region called 'graphs'
  createName(wb, name = "graphs", formula = "GCRunSummaryInfo!$H$5", overwrite = TRUE)
  # Write image to the named region created above
  setwd(pathsavefigs)
  addImage(wb, filename = paste("StandardCurves_", filestoprocess[i], ".png", sep=""), name="graphs", originalSize = TRUE)
  saveWorkbook(wb)
  
  
  
  
  ########################################################################
  # SAVE CSV FILES, BIND OUTCOMES ONTO  RUNNING SUMMARY FILES
  
  # put GC run info into data frame
  vialDF$GCrun <- runsheet$GCRun
  
  # define files to save
  mycsvvialDF <- paste(pathsavefiles, "vialDF_", filestoprocess[i], ".csv", sep = "")
  mycsvambinfoDF <- paste(pathsavefiles, "ambinfoDF_", filestoprocess[i], ".csv", sep = "")
  mycsvtimezeroDF <- paste(pathsavefiles, "timezeroDF_", filestoprocess[i], ".csv", sep = "")
  
  # save vialDF, ambinfoDF, timezeroDF as csv
  write.csv(vialDF, file=mycsvvialDF, row.names=FALSE)
  write.csv(ambinfoDF, file=mycsvambinfoDF, row.names=FALSE)
  write.csv(timezeroDF, file=mycsvtimezeroDF, row.names=FALSE)  
  
  # bind vialDF, ambinfoDF, timezeroDF onto running dfs
  vialDFfull <- rbind(vialDFfull, vialDF)
  ambinfoDFfull <- rbind(ambinfoDFfull, ambinfoDF)
  timezeroDFfull <- rbind(timezeroDFfull, timezeroDF)
  
  
  
}


# write full DF files as csv files  
write.csv(vialDFfull, file=paste(pathsavefiles, "vialDFfull.csv", sep = ""), row.names=FALSE)  
write.csv(ambinfoDFfull, file=paste(pathsavefiles, "ambinfoDFfull.csv", sep = ""), row.names=FALSE)  
write.csv(timezeroDFfull, file=paste(pathsavefiles, "timezeroDFfull.csv", sep = ""), row.names=FALSE)  

# write all warnings into a single file
warningsfiles <- list.files(path=pathsavefiles, pattern = "warnings_*", full.names=TRUE, ignore.case = TRUE)
dataset <- do.call("rbind",lapply(warningsfiles, FUN=function(files){read.table(files, header=FALSE, sep="\t")}))
# why do the first rows print weird?  fix this eventually.
write.csv(dataset, file=paste(pathsavefiles, "warningsfull.csv", sep = ""), row.names=FALSE)  



########################################################################
# END LOOP



########################################################################
# NOTES AND TESTING


# testing update: for filestoprocess = "20140310_U" this matches the venterea excel exactly!
# thing that was keeping them different was the temperature correction.  above, the standards don't get a temperature correction, but everything else does

# no or low pressure vials get removed in FluxCalc-Rcode.R


########################################################################
# WAS ON TO DO LIST, NOW DONE

# instead of printing warning in a more readable way, just built in a "venterea style summary" page to get printed





