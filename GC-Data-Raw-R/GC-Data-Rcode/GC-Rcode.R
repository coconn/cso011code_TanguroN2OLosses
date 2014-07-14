# GC-Rcode.R
# processing GC data - converting raw GC data into mass per volume
# disrupted N project
# CS O'Connell, UMN EEB/IonE



########################################################################
# BRING IN DATA, MAKE DATAFRAME

## pick a particular GC run
## bring in data

# bring in info about the GC run
runsheet <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/PRACTICE/n2o_runsheet_U.txt")

# bring in GC data
ecdN2O <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/PRACTICE/U/ecd.txt", header = FALSE)
fidCH4 <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/PRACTICE/U/fid.txt", header = FALSE)
tcdCO2 <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/PRACTICE/U/tcd.txt", header = FALSE)


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

# note that these are a bit off from the excel doc, because I'm here using standards that got the tmp correction when correcting for ambients
# from here, set new vectors that match the excel so you can continue to compare (delete this part later)

stdtabN2O$N2Oc <- c(15.2058625, 84.7365125, 140.5202125) # TMP!!!!
stdtabCO2$CO2c <- c(61.487075, 0, 259.881775) # TMP!!!!

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

# ifelse test (use high or low standards curve?)
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
ambCinfo <- ambientsC[1:4,1]
ambCinfo[5] <- ambientsCmean
ambCinfo[6] <- ambientsCsd
ambCinfo[7] <- ambientsCcount
Labels <- c("Amb1","Amb2","Amb3","Amb4","Mean", "Std", "Count")

ambinfoDF <- data.frame(Labels,ambNinfo,ambCinfo)
ambinfoDF$GCrun <- runsheet$GCRun[1:7]



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




# print warnings

# are the standards curves poor quality?


# is there high variability in the ambient vials?


# is there high variability in the time zero vials?




########################################################################
# SAVE THINGS

# put GC run info into data frame
vialDF$GCrun <- runsheet$GCRun
# vialDF <- subset(vialDF, select = -c(GCdate) ) # needed this because I changed my mind about my labels

# save vialDF, ambinfoDF, timezeroDF as csv
write.csv(vialDF, file="~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/PRACTICE/vialDF.csv", row.names=FALSE)

write.csv(ambinfoDF, file="~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/PRACTICE/ambinfoDF.csv", row.names=FALSE)

write.csv(timezeroDF, file="~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/PRACTICE/timezeroDF.csv", row.names=FALSE)


# save warnings as txt? see googled solutions for this if desired




###### Q: how to best loop this so the correct run sheet gets brought in and the vialDF gets saved correctly?  think of a good data storage option for this
###### Q: save these files individually but also build one huge vialDF that combines every run?  i could do that in a separete R script, too...

# save(list=paste("data", i, sep=""), file=paste("data", i, ".Rdata", sep="")) 
# solution suggested at http://r.789695.n4.nabble.com/assigning-and-saving-datasets-in-a-loop-with-names-changing-with-quot-i-quot-td843350.html
# or 
# for (i in 1:12){ 
# data <- my.fun(my.list[i]))   
# save(data,file = paste("data",i,".RData", sep="")) } 




########################################################################
# POSSIBLE TO DO

##### what about low or no pressure vials? remove these data points.  or do so when scrubbing for low R2 values on flux calcs





