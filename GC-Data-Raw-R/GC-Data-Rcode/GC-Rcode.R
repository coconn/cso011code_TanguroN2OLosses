# processing GC data - fluxes
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
# STANDARDS CURVE AND PPM CORRECTION

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

# this matches with the excel doc (since the ambient values are slightly off - see above for note about why temp correction isn't incorporated)
x<-c(61.487075,0,259.881775)
lm(stdtabCO2$ppm ~ x)
# using standards that got the tmp correction when correcting for ambients
lm(stdtabCO2$ppm ~ stdtabCO2$CO2c)

# correlation coefficient
corcoeff <- cor(x, stdtabCO2$ppm)
# using standards that got the tmp correction when correcting for ambients
cor(stdtabCO2$CO2c, stdtabCO2$ppm, method = "pearson")
cor(stdtabCO2$CO2c, stdtabCO2$ppm)
# square of that is the pearson's R^2
# http://en.wikipedia.org/wiki/Coefficient_of_determination
pearsonsR2 <- corcoeff^2





# N2O stds regression





###### Q: how come we don't use 10N2O in the standards curves?
###### Q: what are the CH4 values for Mix1 and Mix2 (and the other standards, if applicable)?












## solve for ppm corrected
## solve for ambient-corrected
## solve for ngN_cm3 and ngC_cm3












# build standards curve - N2O
# get the 3 N2O ppm values and peak area values

# build standards curve - CO2

# build standards curve - CH4





# check on time 0 values inside chambers



# issue report: is anything wrong?  and summary for ambient vials









