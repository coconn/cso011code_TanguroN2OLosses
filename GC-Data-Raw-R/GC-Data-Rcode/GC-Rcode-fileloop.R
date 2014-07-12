# processing GC data - fluxes
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# this version of the GC-Rcode file loops through all the GC files to process them


########################################################################
# WHAT FILES ARE WE LOOPING THROUGH

# set files to process and path to folder with standardized naming info
filestoprocess = c("20131104_AA","20131105_BB") # what is a good automated way to get this?  see below for attempts
# where to get inputs
path = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-standardizedfilenames/"
# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rprocessed/"
  
# OR
#filestoprocess <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/standardizedfilenames.txt") # how make this into a character string?
# OR
# filestoprocess = c("Even","Carbon","ClimateReg","HabQual") # list files here

stringAsFactors=FALSE
bob$phenotype <- as.character(bob$phenotype)

i <- sapply(bob, is.factor)
bob[i] <- lapply(bob[i],as.character)


########################################################################
# BEGIN LOOP


for (i in 1:length(filestoprocess)) {
  
  
  ########################################################################
  # BRING IN DATA, MAKE DATAFRAME
  
  ## pick a particular GC run
  ## bring in data
  
  # define files for this loop
  myfilerunsheet <- paste(path, "runsheets-standardizedfilenames/runsheet_", filestoprocess[i], ".txt", sep = "")
  myfileecd <- paste(path, "ecd_", filestoprocess[i], ".txt", sep = "")
  myfilefid <- paste(path, "fid_", filestoprocess[i], ".txt", sep = "")
  myfiletcd <- paste(path, "tcd_", filestoprocess[i], ".txt", sep = "")
  
  # bring in GC data
  runsheet <- read.delim(myfilerunsheet)
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
  
  vialDF <- data.frame(N2Oraw, CO2raw, CH4raw, SampleName, Site, LUtype, Chamber, TimePt, TimePtSq, SampleDate)
  
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
  airtmp <- 20 # degC
  volvial <- 9 # cc
  volsample <- 12 # cc
  voltotal <- volvial + volsample # cc
  volsampletmp <- volsample * (298.15/(273.15+airtmp))
  voltotaltmp <- volvial + volsampletmp
  
  # get volume correction columns, save to dataframe
  N2Oc <- ((voltotaltmp * vialDF$N2Oraw) - (volvial * ambientsNmean)) / volsampletmp
  CO2c <- ((voltotaltmp * vialDF$CO2raw) - (volvial * ambientsCmean)) / volsampletmp
  CH4c <- ((voltotaltmp * vialDF$CH4raw) - (volvial * ambientsCHmean)) / volsampletmp
  vialDF$N2Oc <- N2Oc
  vialDF$CO2c <- CO2c
  vialDF$CH4c <- CH4c
  
  ###### Q: how come you don't use the temp corrected ambient corrections for the standards?
  
  
  
  
  ########################################################################
  # STANDARDS CURVEs AND PPM CORRECTION
  
  # ppm values in the standards
  ppmNstds <- c(0.301,1.57,3) # Mix1, Mix2, 3N2O
  ppmCstds <- c(600,1000,3000) # Mix1, Mix2, 3KCO2
  
  # regression dataframes
  
  areaN2Ostds <- subset(vialDF, vialDF$SampleName=='Mix1' | vialDF$SampleName=='Mix2' | vialDF$SampleName=='3N2O', select=c(N2Oc))
  stdtabN2O = data.frame(area=areaN2Ostds,ppm=ppmNstds)
  stdtabN2O
  
  areaCO2stds <- subset(vialDF, vialDF$SampleName=='Mix1' | vialDF$SampleName=='Mix2' | vialDF$SampleName=='3KCO2', select=c(CO2c))
  stdtabCO2 = data.frame(area=areaCO2stds,ppm=ppmCstds)
  stdtabCO2
  # get rid of mix2 (don't trust it for CO2), use (0,0) instead
  stdtabCO2[2,1:2]<-0
  
  
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
  
  
  
  # get ppm correction columns, save to dataframe
  
  # ifelse test (determine whether to use high or low standards curve)
  N2Oppm <- ifelse(N2Oc>stdtabN2O$N2Oc[2], N2Oc*lmN2Ohigh_slope+lmN2Ohigh_intercept, N2Oc*lmN2Olow_slope+lmN2Olow_intercept)
  CO2ppm <- ifelse(CO2c>stdtabCO2$CO2c[1], CO2c*lmCO2high_slope+lmCO2high_intercept, CO2c*lmCO2low_slope+lmCO2low_intercept)
  
  vialDF$N2Oppm <- N2Oppm
  vialDF$CO2ppm <- CO2ppm
  # vialDF$CH4c <- CH4c # don't forget to do CH4 here once you have the standards info
  
  
  
  
  ###### Q: how come we don't use 10N2O in the standards curves?
  ###### Q: what are the CH4 values for Mix1 and Mix2 (and the other standards, if applicable)?
  ###### Q: don't forget to do the ppm and ng/cm3 corrections for methane (currently there are placeholders)
  
  
  
  
  ########################################################################
  # CONVERT TO MASS PER VOLUME FROM PPM
  
  # tmp-volume-land use dataframe
  ngN_cm3_correction <- c(341.2262)
  ngC_cm3_correction <- c(0.1462398)
  
  LU <- c("F","M","S")
  degC <- c(25,25,25)
  voltmpcorrN2O <- ngN_cm3_correction/(degC+273.15)
  voltmpcorrCO2 <- ngC_cm3_correction/(degC+273.15)
  
  voltmptab = data.frame(LU,degC,voltmpcorrN2O,voltmpcorrCO2)
  
  # solve for ngN_cm3 and ngC_cm3 and save to df
  ngN_cm3_N2O <- N2Oppm*voltmptab[1,3]
  ##### here is where I should have screened for land use - instead, I just used the 20 degC option for all land uses.
  ngC_cm3_CO2 <- CO2ppm*voltmptab[1,4]
  vialDF$ngN_cm3_N2O <- ngN_cm3_N2O
  vialDF$ngC_cm3_CO2 <- ngC_cm3_CO2
  
  
  ###### Q: adjust tmp by land use (see line 191)
  ###### Q: don't forget to go back and do CH4 for this step as well
  
  
  
  
  ########################################################################
  # SUMMARY REPORT RE: THINGS TO WATCH
  
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
  
  Labels <- c("Amb1","Amb2","Amb3","Amb4","Mean", "Std", "Count", "CV")
  
  ambinfoDF <- data.frame(Labels,ambNinfo,ambCinfo)
  ambinfoDF$GCrun <- runsheet$GCRun[1:8]
  
  
  
  # check on time 0 values inside chambers
  
  timezeroN <- subset(vialDF, vialDF$TimePt==0, select=c(N2Oppm))
  timezeroC <- subset(vialDF, vialDF$TimePt==0, select=c(CO2ppm))
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
  
  # add on summary info at the end
  tmplength <- dim(timezeroN)[1]
  
  timezeroNinfo <- timezeroN[1:tmplength,1]
  timezeroCinfo <- timezeroC[1:tmplength,1]
  
  timezeroNinfo[(tmplength+1):(tmplength+6)] <- c(timezeroNmean,timezeroNsd,timezeroNCV,timezeroNsdallow,timezeroNupperlimit,timezeroNlowerlimit)
  timezeroCinfo[(tmplength+1):(tmplength+6)] <- c(timezeroCmean,timezeroCsd,timezeroCCV,timezeroCsdallow,timezeroCupperlimit,timezeroClowerlimit)
  
  timezeroLabs <- as.data.frame(as.matrix(timezeroLabels),stringsAsFactors=F)
  timezeroLabs <- timezeroLabs$SampleName
  
  timezeroLabs[(tmplength+1):(tmplength+6)]<-c("time zero mean", "time zero sd", "time zero CV", "time zero sd allow" ,"time zero upper limit", "time zero lower limit")
  
  timezeroDF <- data.frame(timezeroLabs,timezeroNinfo,timezeroCinfo)
  timezeroDF$GCrun <- runsheet$GCRun[1:(tmplength+6)]
  
  
  
  
  ########################################################################
  # PRINT WARNINGS
  
  # are the standards curves poor quality?
  
  print(paste("STD CURVES INFO: the Pearson's R^2 for N2O HIGH was ", round(lmN2Ohigh_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("STD CURVES INFO: the Pearson's R^2 for N2O LOW was ", round(lmN2Olow_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""))  
  print(paste("STD CURVES INFO: the Pearson's R^2 for CO2 HIGH was ", round(lmCO2high_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("STD CURVES INFO: the Pearson's R^2 for CO2 LOW was ", round(lmCO2low_pearsonsR2, digits=4), ".  File = ", filestoprocess[i], sep = ""))
  
  # is there high variability in the ambient vials?
  
  ind = which(ambinfoDF$Labels=="CV")
  print(paste("AMBIENT VIAL INFO: the CV for N2O was ", round(ambinfoDF$ambNinfo[ind], digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("AMBIENT VIAL INFO: the CV for CO2 was ", round(ambinfoDF$ambCinfo[ind], digits=4), ".  File = ", filestoprocess[i], sep = ""))
  
  # is there high variability in the time zero vials?
  
  ind = which(timezeroDF$timezeroLabs=="time zero CV")
  print(paste("TIME ZERO INFO: the CV for N2O was ", round(timezeroDF$timezeroNinfo[ind], digits=4), ".  File = ", filestoprocess[i], sep = ""))
  print(paste("TIME ZERO INFO: the CV for CO2 was ", round(timezeroDF$timezeroCinfo[ind], digits=4), ".  File = ", filestoprocess[i], sep = ""))
  
  
  ###### Q: save these summary statements as text
  
  
  
  ########################################################################
  # SAVE THINGS
  
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
  

  
  ###### Q: save these summary statements as text
  
  
  
}
  
  
########################################################################
# END LOOP



fileconn <- file("output.txt")
writeLines(c("hello","world"),fileconn)
close(fileconn)

sink("output.txt")
cat("hello")
cat("/n")
cat("world")
sink()

# USE THIS
cat("hello", file="output2.txt",sep="\n")
cat("world", file="output2.txt",sep="\n",append=TRUE)
cat("third", file="output2.txt",append=TRUE)




########################################################################
# POSSIBLE TO DO

##### what about low or no pressure vials? remove these data points.  or do so when scrubbing for low R2 values on flux calcs





